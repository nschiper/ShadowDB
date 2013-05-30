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
 *  o File name:   Evaluator4.sml
 *  o Description: defunctionalization.
 *)


structure Evaluator4 = struct

structure B   = Tools
structure T   = NuprlTerms
structure P   = Primitive
structure E   = EvalProperties
structure EV2 = Evaluator2
structure EV3 = Evaluator3

val NextStepEval4  = EV2.ClosNextStepEval2
val clos_refs      = EV2.clos_refs
val num_principals = EV3.m_num_principals
val next2term      = EV2.next2term

type term   = T.nuprl_term
type rterm  = T.nuprl_ref_term
type env    = T.env
type var    = T.nuprl_variable
type bterm  = var list * rterm
type bterms = bterm list

datatype cont_list = NEXT     of term * bterms * env * cont
		   | VAL_ALL  of term * bterms * env * cont
		   | SUB_LIST of term * env * cont_list

     and cont = EVAL_ALL_LIST of bterms * bool * env * cont_list
	      | EVAL_LIST     of bterms * bool * env * cont_list
	      | HALT

fun mk_next_term    t np env K = NEXT    (t, np, env, K)
fun mk_val_all_term t np env K = VAL_ALL (t, np, env, K)
fun mk_sub_list_term v env K = SUB_LIST (v, env, K)
fun mk_eval_list_term     tail cbva env K = EVAL_LIST     (tail, cbva, env, K)
fun mk_eval_all_list_term tail cbva env K = EVAL_ALL_LIST (tail, cbva, env, K)

(* EVALUATOR4 - defunctionalization *)
fun Evaluator4' term state =

    let fun ApplyListCont (NEXT (t,np,env,K')) lst (S as (n,(lib,cls))) =
	    let val opr = T.opr_of_term t
		fun recons () = T.mk_nuprl_ref_term opr ((clos_refs lst) @ np)
	    in let val ((nt,e',ev),S') = NextStepEval4 cls lib t lst np env S
		   val t' = next2term nt
	       in if ev
		  then Eval (t',e') K' S'
		  else ApplyCont (t',e') K' S'
	       end handle _ => ApplyCont (recons (), env) K' S
	    end
	  | ApplyListCont (VAL_ALL (t,np,env,K')) lst S =
	    let val opr = T.opr_of_term t
		val t'  = T.mk_nuprl_ref_term opr ((clos_refs lst) @ np)
	    in ApplyCont (t',env) K' S
	    end
	  | ApplyListCont (SUB_LIST (v,e,K')) lst S = ApplyListCont K' ((v,e)::lst) S

	and ApplyCont (v,e) (cterm as EVAL_ALL_LIST (tail,cbva,env,K')) S =
	    if cbva
	    then EvalAll (v,e) (mk_eval_list_term tail cbva env K') S
	    else EvalList cbva tail env (mk_sub_list_term v e K') S
	  | ApplyCont (v,e) (cterm as EVAL_LIST (tail,cbva,env,K')) S =
	    EvalList cbva tail env (mk_sub_list_term v e K') S
	  | ApplyCont (v,e) (cterm as HALT) S = ((v,e),S)

	and EvalList cbva [] env K S = ApplyListCont K [] S
	  | EvalList cbva ((vars,t)::rest) env K S =
	    if List.null vars
	    then Eval (T.rterm2term t,env) (mk_eval_all_list_term rest cbva env K) S
	    else raise Fail "term has bound variables"

	and Eval (t, env) K S =
	    let val (opr as (opid,params),subterms) = T.dest_term t
		val (p,np) = B.split (num_principals opid t S) subterms
		val b = E.is_eval_all opid
	    in EvalList b p env (mk_next_term t np env K) S
	    end

	and EvalAll (t, env) K S =
	    let val (opr as (opid,params),subterms) = T.dest_term t
		val (p,np) = B.split (E.num_principal_all opid) subterms
		val env' = if List.null np then T.em_env else env
	    in EvalList true p env (mk_val_all_term t np env' K) S
	    end

    in if T.is_ct term
       then Eval (T.dest_ct term) HALT state
       else Eval (term, T.em_env) HALT state
    end

fun Evaluator4 cls lib steps term =
    let val ((t',e'),(n,s)) = Evaluator4' term (steps, (lib,cls))
	val answer =
	    if cls andalso T.is_nuprl_pair_term t'
	    then let val (s,msgs) = T.dest_pair 8 t'
		     val msgs' = T.close msgs e'
		 in T.mk_pair_term (T.mk_rct (s, e')) msgs'
		 end
	    else T.close t' e'
    in (answer, steps - n)
    end

end
