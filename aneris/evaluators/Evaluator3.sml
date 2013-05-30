(* Copyright 2011 Cornell University
 * Copyright 2012 Cornell University
 * Copyright 2013 Cornell University
 *
 *
 * This file is part of EventML - a tool aiming at specifying
 * distributed protocols in an ML like language.  It is an interface
 * to the logic of events and is compiled into Nuprl.  It is written
 * by the NUPRL group of Cornell University, Ithaca, NY.
 *
 * EventML is a free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * EventML is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with EventML.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Cornell University, NUPRL group
 *  o Date:        20 May 2011
 *  o File name:   Evaluator3.sml
 *  o Description: uses continuations.
 *)


structure Evaluator3 = struct

structure B   = Tools
structure T   = NuprlTerms
structure P   = Primitive
structure E   = EvalProperties
structure EV2 = Evaluator2

val NextStepEval3 = EV2.ClosNextStepEval2
val clos_refs     = EV2.clos_refs
val next2term     = EV2.next2term

fun unit x s cont = cont x s
fun bind (m, k) = fn s => fn cont => m s (fn v => fn s' => k v s' cont)
val >>= = bind
infix >>=

(*fun m_bind m k = fn cl => fn s => let val (v,s') = m s in k v cl s' end
val >>>= = m_bind
infix >>>=*)

type term  = T.nuprl_term
type env   = T.env
type rterm = T.nuprl_ref_term
type var   = T.nuprl_variable
datatype kind = EV  of term * env
	      | ALL of term * env
	      | LST of bool * (var list * rterm) list * env
datatype value = VAL  of term * env
	       | VLST of (term * env) list

fun bind_v (m, k) =
 fn s =>
 fn K =>
    m s (fn (VAL v)  => (fn s' => k v s' K)
	  | (VLST _) => raise Fail "bind_v")
val >>>= = bind_v
infix >>>=

fun bind_l (m, k) =
 fn s =>
 fn K =>
    m s (fn (VLST v) => (fn s' => k v s' K)
	  | (VAL _)  => raise Fail "bin_l")
val >>>>= = bind_l
infix >>>>=

fun km_num_principals opid term (state as (steps,_)) K =
    if steps < 0
       andalso opid = "apply"
       andalso T.is_nuprl_term "ycomb" (#1 (T.dest_apply term))
    then K 0 state
    else K (E.num_principals opid) state

fun m_num_principals opid term (state as (steps,_)) =
    if steps < 0
       andalso opid = "apply"
       andalso T.is_nuprl_term "ycomb" (#1 (T.dest_apply term))
    then 0
    else E.num_principals opid

fun printDebug term1 term2 env timer n m =
    if m < n andalso m mod 1000 = 0
    then let val time  = IntInf.toString (B.getMilliTime timer)
	     (*val size1 = T.size term1
	     val size2 = T.size term2
	     val size3 = T.size env*)
	     val tail  = "[time:"    ^ time ^
			 (*",size-t1:" ^ Int.toString size1 ^
			 ",size-t2:" ^ Int.toString size2 ^
			 ",size-e:"  ^ Int.toString size3 ^*)
			 "]"
	 in print (Int.toString m ^ tail ^ "\n")
	 end
    else ()

(* EVALUATOR3 - continuations *)
fun Evaluator3' term state =

    let val timer = B.startTimer ()

	fun EvalList cbva [] env = unit []
	  | EvalList cbva ((vars,t)::rest) env =
	    if List.null vars
	    then (Eval (T.rterm2term t,env))
		     >>=
		     (fn p => ((if cbva then EvalAll else unit) p)
				  >>=
				  (fn q => (EvalList cbva rest env)
					       >>=
					       (fn vs => unit (q :: vs))))
	    else raise Fail "term has bound variables"

	and Eval (t, env) =
	    let val (opr as (opid,params),subterms) = T.dest_term t
	    in (km_num_principals opid t)
		   >>=
		   (fn n =>
		       let val (p,np) = B.split n subterms
		       in (EvalList (E.is_eval_all opid) p env)
			      >>=
			      (fn c =>
			       fn (state as (n,(lib,cls))) =>
			       fn K =>
				  let val ((nt,e',ev),state' as (n',_)) = NextStepEval3 cls lib t c np env state
				      val t' = next2term nt
				      (*val _ = printDebug t t' e' timer n n'*)
				  in if ev then Eval (t',e') state' K else K (t',e') state'
				  end (*handle _ => K (T.mk_nuprl_term opr ((clos_refs c) @ np), env) state*))
		       end)
	    end

	and EvalAll (t, env) =
	    let val (opr as (opid,params),subterms) = T.dest_term t
		val (principals,nprincipals) = B.split (E.num_principal_all opid) subterms
		val env' = if List.null nprincipals then T.em_env else env
	    in (EvalList true principals env)
		   >>=
		   (fn c => unit (T.mk_nuprl_ref_term opr ((clos_refs c) @ nprincipals), env'))
	    end

	fun K p state = (p, state)

    in if T.is_ct term
       then Eval (T.dest_ct term) state K
       else Eval (term, T.em_env) state K
    end

fun Evaluator3'' term state =

    let val timer = B.startTimer ()

	fun EvalList cbva [] env state K = K ([],state)
	  | EvalList cbva ((vars,t)::rest) env state K =
	    if List.null vars
	    then let fun K2 (q,state) =
			 EvalList cbva rest env state (fn (vs,state) => K (q::vs,state))
		     fun K1 (p,state) =
			 if cbva
			 then EvalAll p state K2
			 else K2 (p,state)
		 in Eval (T.rterm2term t,env) state K1
		 end
	    else raise Fail "term has bound variables"

	and Eval (t, env) state K =
	    let val (opr as (opid,params),subterms) = T.dest_term t
		val n = m_num_principals opid t state
		val (p,np) = B.split n subterms
		fun K' (c,state as (n,(lib,cls))) =
		    let val ((nt,e',ev),state' as (n',_)) = NextStepEval3 cls lib t c np env state
			val t' = next2term nt
			(*val _ = printDebug t t' e' timer n n'*)
		    in if ev then Eval (t',e') state' K else K ((t',e'), state')
		    end (*handle _ => K ((T.mk_nuprl_term opr ((clos_lst c) @ np), env), state)*)
	    in EvalList (E.is_eval_all opid) p env state K'
	    end

	and EvalAll (t, env) state K =
	    let val (opr as (opid,params),subterms) = T.dest_term t
		val (principals,nprincipals) = B.split (E.num_principal_all opid) subterms
		val env' = if List.null nprincipals then T.em_env else env
		fun K' (c,state) = K ((T.mk_nuprl_ref_term opr ((clos_refs c) @ nprincipals), env'),state)
	    in EvalList true principals env state K'
	    end

	fun K x = x

    in if T.is_ct term
       then Eval (T.dest_ct term) state K
       else Eval (term, T.em_env) state K
    end

fun EvaluatorData3 term state =

    let val timer = B.startTimer ()

	fun Eval (EV (t, env)) =
	    let val (opr as (opid,params),subterms) = T.dest_term t
	    in (km_num_principals opid t)
		   >>=
		   (fn n =>
		       let val (p,np) = B.split n subterms
		       in (Eval (LST (E.is_eval_all opid, p, env)))
			      >>>>=
			      (fn c =>
			       fn (state as (n,(lib,cls))) =>
			       fn K =>
				  let val ((nt,e',ev),state' as (n',_)) = NextStepEval3 cls lib t c np env state
				      val t' = next2term nt
				      (*val _ = printDebug t t' e' timer n n'*)
				  in if ev then Eval (EV (t',e')) state' K else K (VAL (t',e')) state'
				  end (*handle _ => K (VAL (T.mk_nuprl_term opr ((clos_refs c) @ np), env)) state*))
		       end)
	    end
	  | Eval (ALL (t, env)) =
	    let val (opr as (opid,params),subterms) = T.dest_term t
		val (principals,nprincipals) = B.split (E.num_principal_all opid) subterms
		val env' = if List.null nprincipals then T.em_env else env
	    in (Eval (LST (true, principals, env)))
		   >>>>=
		   (fn c => unit (VAL (T.mk_nuprl_ref_term opr ((clos_refs c) @ nprincipals), env')))
	    end
	  | Eval (LST (cbva, [], env)) = unit (VLST [])
	  | Eval (LST (cbva, (vars,t)::rest, env)) =
	    if List.null vars
	    then (Eval (EV (T.rterm2term t,env)))
		     >>>=
		     (fn p => (if cbva then Eval (ALL p) else unit (VAL p))
				  >>>=
				  (fn q => (Eval (LST (cbva, rest, env)))
					       >>>>=
					       (fn vs => unit (VLST (q :: vs)))))
	    else raise Fail "term has bound variables"

	fun K p state = (p, state)

	val (x,s) =
	    if T.is_ct term
	    then Eval (EV (T.dest_ct term)) state K
	    else Eval (EV (term, T.em_env)) state K

    in case x of
	   VAL (t',e') => ((t',e'),s)
	 | VLST _ => raise Fail ""
    end

fun GenEvaluator3 ev cls lib steps term =
    let val ((t',e'),(n,s)) = ev term (steps, (lib,cls))
	val answer =
	    if cls andalso T.is_nuprl_pair_term t'
	    then let val (s,msgs) = T.dest_pair 8 t'
		     val msgs' = T.close msgs e'
		 in T.mk_pair_term (T.mk_rct (s, e')) msgs'
		 end
	    else T.close t' e'
    in (answer, steps - n)
    end

val Evaluator3  = GenEvaluator3 Evaluator3'
(*val Evaluator3  = GenEvaluator3 Evaluator3''*)
val EvaluatorD3 = GenEvaluator3 EvaluatorData3
end
