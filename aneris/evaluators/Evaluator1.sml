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
 *  o File name:   Evaluator1.sml
 *  o Description: .
 *)


structure Evaluator1 = struct

structure B = Tools
structure T = NuprlTerms
structure P = Primitive
structure E = EvalProperties

fun member (element : string) list = List.exists (fn v => v = element) list

fun NextStepEval1 t steps0 principals non_principals steps lib =
    let val opid = T.opid_of_term t

    in if member opid ["add", "subtract", "multiply", "divide", "remainder"]
       then let val (v1,v2) = B.get2 principals
	    in (T.do_primitive_int_op opid v1 v2, B.decr_steps steps, false)
	    end

       else if opid = "minus"
       then let val v = B.get1 principals
	    in if T.is_nuprl_term "natural_number" v
	       then (T.mk_nuprl_simple_term "minus" [v], steps, false)
	       else (T.do_primitive_minus v, B.decr_steps steps, false)
	    end

       else if member opid ["less", "int_eq"]
       then let val (v1,v2) = B.get2 principals
		val (t3,t4) = B.get2_0bound non_principals
	    in if T.do_primitive_cmp opid v1 v2
	       then (T.rterm2term t3, B.decr_steps steps, true)
	       else (T.rterm2term t4, B.decr_steps steps, true)
	    end

       else if opid = "atom_eq"
       then let val n = (T.firstnat t) handle _ => 0
		val (v1,v2) = B.get2 principals
		val (t3,t4) = B.get2_0bound non_principals
	    in if T.compare_atomn n v1 v2
	       then (T.rterm2term t3, B.decr_steps steps, true)
	       else (T.rterm2term t4, B.decr_steps steps, true)
	    end

       else if opid = "eq_term"
       then let val (v1,v2) = B.get2 principals
	    in if P.is_complete_primitive_value v1
		  andalso
		  P.is_complete_primitive_value v2
	       then if T.alpha_equal_terms v1 v2
		    then (T.mk_inl_term T.mk_axiom_term, B.decr_steps steps, false)
		    else (T.mk_inr_term T.mk_axiom_term, B.decr_steps steps, false)
	       else raise Fail "eq_term"
	    end

       else if member opid ["isinr", "isinl", "ispair", "isint", "islambda", "isatom2", "isaxiom"]
       then let val v1 = B.get1 principals
		val (t2,t3) = B.get2_0bound non_principals
	    in if P.do_primitive_test opid v1
	       then (T.rterm2term t2, B.decr_steps steps, true)
	       else (T.rterm2term t3, B.decr_steps steps, true)
	    end

       else if opid = "spread"
       then let val v1 = B.get1 principals
		val (x,y,B) = B.get1_2bound non_principals
		val (a, b)  = T.dest_pair 5 v1
		    handle _ => raise Fail (T.toStringTerm t ^ "\n-+-+-+-\n" ^ T.toStringTerm v1 ^ "\n-+-+-+-\n" ^ Int.toString steps)
	    in (T.fo_subst [(x,a),(y,b)] (T.rterm2term B), B.decr_steps steps, true)
	    end

       else if opid = "decide"
       then let val v1 = B.get1 principals
		val (x,A,y,B) = B.get2_1bound non_principals
		val t1 = T.subtermn 1 v1
	    in if T.is_nuprl_inl_term v1
	       then (T.fo_subst [(x, t1)] (T.rterm2term A), B.decr_steps steps, true)
	       else if T.is_nuprl_inr_term v1
	       then (T.fo_subst [(y, t1)] (T.rterm2term B), B.decr_steps steps, true)
	       else raise Fail ("decide(" ^ T.opid_of_term v1 ^ ")")
	    end

       else if opid = "apply"
       then let val arg = B.get1_0bound non_principals
		val arg = T.rterm2term arg
	    in if steps < 0 andalso T.is_nuprl_term "ycomb" (T.subtermn 1 t)
	       then (T.mk_apply_term arg t, B.decr_steps steps0, true)
	       else let val f = B.get1 principals
			val (x,B) = T.dest_lambda 6 f
		    in (T.fo_subst [(x, arg)] B, B.decr_steps steps, true)
		    end
	    end

       else if opid = "fix"
       then let val f = B.get1_0bound non_principals
	    in (T.mk_apply_term (T.rterm2term f) t, B.decr_steps steps, true)
	    end

       else if opid = "!wait"
       then let val t = B.get1 principals
		val w = B.get1_0bound non_principals
	    in (T.do_primitive_wait t (T.rterm2term w), B.decr_steps steps, true)
	    end

       else if opid = "callbyvalue"
       then let val q = B.get1 principals
		val (x,B) = B.get1_1bound non_principals
	    in if P.is_primitive_value q
	       then (T.fo_subst [(x, q)] (T.rterm2term B), B.decr_steps steps, true)
	       else raise Fail "callbyvalue"
	    end

       else if opid = "callbyvalueall"
       then let val q = B.get1 principals
		val (x,B) = B.get1_1bound non_principals
	    in if P.is_complete_primitive_value q
	       then (T.fo_subst [(x, q)] (T.rterm2term B), B.decr_steps steps, true)
	       else ((*print (T.toStringTerm q);*)
		     raise Fail "callbyvalueall")
	    end

       else if opid = "ind"
       then let val q = B.get1 principals
		val (x,rd,downcase,basecase,y,ru,upcase) = B.get3_202bound non_principals
		val downcase = T.rterm2term downcase
		val basecase = T.rterm2term basecase
		val upcase   = T.rterm2term upcase
		val ord = T.is_zero q
		val t' = if ord = EQUAL then basecase
			 else let val (p,r,w,c) =
				      if ord = GREATER
				      then (T.dec_integer q,ru,y,upcase)
				      else (T.inc_integer q,rd,x,downcase)
				  val t2 = T.mk_ind_nterm p (x,rd,downcase) basecase (y,ru,upcase)
			      in (T.fo_subst [(w,q),(r,t2)] c)
			      end
	    in (t', B.decr_steps steps, true)
	    end

       else if opid = "variable"
       then (t, steps, false)

       else if opid = "!abstraction"
       then let val (v1,v2,v3) = B.get3 principals
	    in (T.mk_nuprl_iabstraction_term v2 v3, steps, false)
	    end

       else if E.is_termof_term lib t
       then (E.unfold_tof t, B.decr_steps steps, true)
       (*then (apply_conv (TagC (mk_tag_term 1)) t, user, B.decr_steps steps, true)*)

       else if E.is_abstraction_term lib t
       then (E.unfold_abs t, B.decr_steps steps, true)

       else (t, steps, false)

    end

(* EVALUATOR1 - recursive descent *)
fun Evaluator1 lib steps term =

    let fun EvalList (terms, steps, lib) eval =
	    foldl (fn ((vars,t), (steps,terms)) =>
		      if List.null vars
		      then let val (t1,steps1,l) = eval (T.rterm2term t,steps, lib)
			   in (steps1, terms @ [t1])
			   end
		      else raise Fail "EvalList")
		  (steps,[])
		  terms

	fun Eval (t, steps, lib) =
	    let val (opr,subterms) = T.dest_term t
		val (opid,params) = opr
		val b = E.is_eval_all opid
		val eval = if b then EvalAll o Eval else Eval
		(*val _ = print (Bool.toString b ^ "\n" ^ T.toStringTerm t ^ "\n")*)
		val (principals,non_principals) = B.split (E.num_principals opid) subterms
		val (steps1,terms) = EvalList (principals, steps, lib) eval
		fun abort () = (T.mk_nuprl_ref_term opr ((map (fn t => ([],T.mk_rterm t)) terms) @ non_principals),
				steps1,
				lib)
	    in let val (t',steps',ev) = NextStepEval1 t steps terms non_principals steps1 lib
	       in (if ev then Eval else fn x => x) (t', steps', lib)
	       end (*handle _ => abort ()*)
	    end

	and EvalAll (t, steps, lib) =
	    let val (opr,subterms) = T.dest_term t
		val (opid,params) = opr
		val (principals,non_principals) = B.split (E.num_principal_all opid) subterms
		val (steps1,terms) = EvalList (principals, steps, lib) (EvalAll o Eval)
	    in (T.mk_nuprl_ref_term opr ((map (fn t => ([],T.mk_rterm t)) terms) @ non_principals),
		steps1,
		lib)
	    end

	val (answer,num,_) = Eval (term, steps, lib)

    in (answer, steps - num)
    end

end
