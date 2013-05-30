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
 *  o File name:   Evaluator2.sml
 *  o Description: uses closures.
 *)


structure Evaluator2 = struct

structure B = Tools
structure T = NuprlTerms
structure P = Primitive
structure E = EvalProperties
structure M = Monad

val mk_rct = T.mk_rct
val em_env = T.em_env

fun get_steps (n,_) = n

fun upd_get_found_user x (n,(lib,cls)) = (x, (B.decr_steps n, (lib,cls)))

structure MAP = BinaryMapFn(type ord_key = string val compare = String.compare)
structure SET = BinarySetFn(type ord_key = string val compare = String.compare)

(*
fun apply_ren ren (var : T.variable) =
    case MAP.find (ren, var) of
	SOME v' => v'
      | NONE => raise Fail "apply_ren"

fun update_ren ren vars1 vars2 =
    B.accumulate2 (fn ren =>
		   fn u =>
		   fn v =>
		      MAP.insert (ren, T.dest_nuprl_var u, T.dest_nuprl_var v))
		  ren
		  vars1
		  vars2

fun ct_alpha_equal_closure_terms v1 e1 v2 e2 =
    let fun aux renamings (vs1,t1) env1 (vs2,t2) env2 =
	    let val renamings' = update_ren renamings vs1 vs2
		val op1 = T.opid_of_term t1
	    in if op1 = "!!closure"
	       then let val (t, e) = T.dest_ct t1
		    in aux renamings' ([],t) e ([],t2) env2
		    end
	       else let val op2 = T.opid_of_term t2
		    in if op2 = "!!closure"
		       then let val (t, e) = T.dest_ct t2
			    in aux renamings' ([],t1) env1 ([],t) e
			    end
		       else if op1 = "variable"
		       then let val v = T.dest_variable t1
			    in let val v' = apply_ren renamings' v
			       in op2 = "variable" andalso T.dest_variable t2 = v'
			       end handle _ => (case T.lookup env1 v of
						    SOME (t,e) => aux renamings' ([],t) e ([],t2) env2
						  | NONE => raise Fail "alpha_equal_closure_terms")
			    end
		       else if op2 = "variable"
		       then let val v = T.dest_variable t2
			    in (op1 = "variable"
				andalso
				apply_ren renamings' (T.dest_variable t1) = v)
			       handle _ => (case T.lookup env2 v of
						SOME (t,e) => aux renamings' ([],t1) env1 ([],t) e
					      | NONE => raise Fail "alpha_equal_closure_terms")
			    end
		       else op1 = op2
			    andalso B.all2 T.equal_parameters (T.parameters_of_term t1) (T.parameters_of_term t2)
			    andalso B.all2 (fn x => fn y => aux renamings' x env1 y env2) (T.brterms_of_term t1) (T.brterms_of_term t2)
		    end
	    end
    in aux MAP.empty ([],v1) e1 ([],v2) e2
    end handle _ => false
*)

val set_int_op  = SET.addList (SET.empty, ["add", "subtract", "multiply", "divide", "remainder"])
val set_comp_op = SET.addList (SET.empty, ["less", "int_eq"])
val set_is_op   = SET.addList (SET.empty, ["isinr", "isinl", "ispair", "isint", "islambda", "isatom2", "isaxiom"])

fun member_int_op  element = SET.member (set_int_op,  element)
fun member_comp_op element = SET.member (set_comp_op, element)
fun member_is_op   element = SET.member (set_is_op,   element)

val decr = M.decr

val unit = M.unit

fun get1 opid principals =
    (B.get1 principals) handle Fail str => raise Fail (str ^ ":" ^ opid)

datatype next = NEXT_R of T.nuprl_ref_term
	      | NEXT_V of T.variable * bool * (T.nuprl_term * bool) ref
	      | NEXT_T of T.nuprl_term

fun rdecr (rterm, e, b) = decr (NEXT_R rterm, e, b)
fun tdecr (term,  e, b) = decr (NEXT_T term,  e, b)

fun runit (rterm, e, b) = unit (NEXT_R rterm, e, b)
fun tunit (term,  e, b) = unit (NEXT_T term,  e, b)

fun vunit (v, a, termref, e, b) = unit (NEXT_V (v, a, termref), e, b)

fun tupd_get_found_user (term, e, b) = upd_get_found_user (NEXT_T term, e, b)

fun next2term (NEXT_R rterm)         = T.rterm2term rterm
  | next2term (NEXT_V (_,_,termref)) = #1 (!termref)
  | next2term (NEXT_T term)          = term

fun alpha_equal_complete_primitive_terms term1 env1 term2 env2 =
    if T.is_ct term1
    then let val (t,e) = T.dest_ct term1
	 in alpha_equal_complete_primitive_terms t e term2 env2
	 end
    else if T.is_ct term2
    then let val (t,e) = T.dest_ct term2
	 in alpha_equal_complete_primitive_terms term1 env1 t e
	 end
    else if T.is_nuprl_variable_term term1
    then let val v1 = T.dest_variable term1
	 in case T.lookup env1 v1 of
		SOME (t,e) => alpha_equal_complete_primitive_terms t e term2 env2
	      | NONE => raise Fail "alpha_equal_complete_primitive_terms:var1"
	 end
    else if T.is_nuprl_variable_term term2
    then let val v2 = T.dest_variable term2
	 in case T.lookup env2 v2 of
		SOME (t,e) => alpha_equal_complete_primitive_terms term1 env1 t e
	      | NONE => raise Fail "alpha_equal_complete_primitive_terms:var2"
	 end
    else T.opid_of_term term1 = T.opid_of_term term2
	 andalso B.all2
		     T.equal_parameters
		     (T.parameters_of_term term1)
		     (T.parameters_of_term term2)
	 andalso B.all2
		     (fn (vars1,t1) =>
		      fn (vars2,t2) =>
			 not (null vars1)
			 orelse not (null vars2)
			 orelse alpha_equal_complete_primitive_terms t1 env1 t2 env2)
		     (T.brterms_of_term term1)
		     (T.brterms_of_term term2)

fun is_complete_primitive_value env term =
    if T.is_ct term
    then let val (t,e) = T.dest_ct term
	 in is_complete_primitive_value e t
	 end
    else if T.is_nuprl_variable_term term
    then case T.lookup env (T.dest_variable term) of
	     SOME (t,e) => is_complete_primitive_value e t
	   | NONE => raise Fail "is_complete_primitive_value"
    else P.is_primitive_value term
	 andalso
	 let val (opr,bterms) = T.dest_term term
	 in List.all
		(fn (vars,rterm) =>
		    not (List.null vars)
		    orelse
		    is_complete_primitive_value env (T.rterm2term rterm))
		bterms
	 end

fun ClosNextStepEval2 cls lib t principals non_principals env =
    let val opid = T.opid_of_term t
	(*val _ = print (opid ^ "\n")*)

    in if member_int_op opid
       then let val ((v1,_),(v2,_)) = B.get2 principals
	    in tdecr (T.do_primitive_int_op opid v1 v2, em_env, false)
	    end

       else if opid = "minus"
       then let val (v,_) = get1 opid principals
	    in (*if T.is_nuprl_term "natural_number" v
	       then tunit (T.mk_minus_term v, em_env, false)
	       else*) tdecr (T.do_primitive_minus v, em_env, false)
	    end

       else if member_comp_op opid
       then let val ((v1,_),(v2,_)) = B.get2 principals
		val (t3,t4) = B.get2_0bound non_principals
	    in if T.do_primitive_cmp opid v1 v2
	       then rdecr (t3, env, true)
	       else rdecr (t4, env, true)
	    end

       else if opid = "atom_eq"
       then let val n = (T.firstnat t) handle _ => 0
		val ((v1,_),(v2,_)) = B.get2 principals
		val (t3,t4) = B.get2_0bound non_principals
	    in if T.compare_atomn n v1 v2
	       then rdecr (t3, env, true)
	       else rdecr (t4, env, true)
	    end

(*
       else if opid = "eq_term"
       then let val ((v1,e1),(v2,e2)) = B.get2 principals
	    in if P.is_complete_primitive_value v1
	       then if P.is_complete_primitive_value v2
		    then if ct_alpha_equal_closure_terms v1 e1 v2 e2
			 then tdecr (T.mk_inl_term T.mk_axiom_term, em_env, false)
			 else tdecr (T.mk_inr_term T.mk_axiom_term, em_env, false)
		    else raise Fail ("eq_term(2," ^ T.opid_of_term v2 ^ "," ^ T.toStringTerm v2 ^ ")")
	       else raise Fail ("eq_term(1," ^ T.opid_of_term v1 ^ "," ^ T.toStringTerm v1 ^ ")")
	    end
*)

(*
       else if opid = "eq_term"
       then let val ((v1,e1),(v2,e2)) = B.get2 principals
	    in if alpha_equal_complete_primitive_terms v1 e1 v2 e2
	       then tdecr (T.mk_inl_term T.mk_axiom_term, em_env, false)
	       else tdecr (T.mk_inr_term T.mk_axiom_term, em_env, false)
	    end
*)

       else if member_is_op opid
       then let val (v1,_) = get1 opid principals
		val (t2,t3) = B.get2_0bound non_principals
	    in if P.do_primitive_test opid v1
	       then rdecr (t2, env, true)
	       else rdecr (t3, env, true)
	    end

       else if opid = "spread"
       then let val (v1,e1) = get1 opid principals
		val (x,y,B) = B.get1_2bound non_principals
		val (a, b)  = (T.dest_rpair 5 v1)
		    handle _ => ((*print (T.toStringTerm t  ^ "\n---\n");
				 print (T.toStringTerm v1 ^ "\n---\n");
				 print (T.toStringEnv  e1 ^ "\n---\n");*)
				 raise Fail ("spread(" ^ T.opid_of_term v1 ^ ")"))
	    in rdecr (B, T.add2env env [(y,false,b,e1),(x,false,a,e1)], true)
	    end

       else if opid = "decide"
       then let val (v1,e1)   = get1 opid principals
		val (x,A,y,B) = B.get2_1bound non_principals
	    in if T.is_nuprl_inl_term v1
	       then rdecr (A, T.add2env env [(x,false,T.dest_rinl v1,e1)], true)
	       else if T.is_nuprl_inr_term v1
	       then rdecr (B, T.add2env env [(y,false,T.dest_rinr v1,e1)], true)
	       else raise Fail ("decide(" ^ T.toStringTerm v1 ^ ")")
	    end

       else if opid = "apply"
       then if List.null principals (* then the function is the Y combinator *)
	    then let val (yc,arg) = B.get2_0bound non_principals
		 in if T.opid_of_term (T.rterm2term yc) = "ycomb"
		    then tdecr (T.mk_apply_ref_term arg (T.mk_rterm t), env, true)
		    else raise Fail "apply"
		 end
	    else let val (f,fe) = get1 opid principals
		     val arg    = B.get1_0bound non_principals
		     val (x, B) = T.dest_ref_lambda 5 f
				  (*handle err =>
					 (print ("\n" ^ T.toStringTerm f ^ "\n");
					  raise err)*)
		 in rdecr (B, T.add2env fe [(x,false,arg,env)], true)
		 end

       else if opid = "fix"
       then let val f = B.get1_0bound non_principals
	    in (*let val (x,B) = T.dest_ref_lambda 6 (T.rterm2term f)
		     val t' = T.mk_rterm (T.mk_fix_ref_term f)
		 in rdecr (B, T.add2env env [(T.dest_nuprl_var x, t', env)], true)
		 end*)
		tdecr (T.mk_apply_ref_term f (T.mk_rterm t), env, true)
	    end

       else if opid = "!wait"
       then let val (t,_) = get1 opid principals
		val w     = B.get1_0bound non_principals
	    in rdecr (T.do_primitive_ref_wait t w, env, true)
	    end

       else if opid = "callbyvalue"
       then let val (q,e) = get1 opid principals
		val (x,B) = B.get1_1bound non_principals
	    in if P.is_primitive_value q
	       then rdecr (B, T.add2env env [(x,false,T.mk_rterm q,e)], true)
	       else raise Fail "callbyvalue"
	    end

       else if opid = "callbyvalueall"
       then let val (q,e) = get1 opid principals
		val (x,B) = B.get1_1bound non_principals
	    in if (*P.*)is_complete_primitive_value e q
	       then rdecr (B, T.add2env env [(x,true,T.mk_rterm q,e)], true)
	       else (*raise Fail ("callbyvalueall:(" ^ T.opid_of_term q ^ ")")*)
		   raise Fail ("callbyvalueall:(" ^ T.toStringTerm q ^ ")")
	    end

(*
       else if opid = "list_ind"
       then let val (q,e) = B.get1 principals
		val (nilcase,x,xs,r,conscase) = B.get2_03bound non_principals
		val opq = T.opid_of_term q
	    in if opq = "nil"
	       then decr (T.rterm2term nilcase, env, true)
	       else if opq = "cons"
	       then let val (qa,qb) = T.dest_cons q
			val qb' = mk_rct (qb,e)
			val t2  = T.mk_list_ind_ref_term (T.mk_rterm qb') nilcase (x,xs,r,conscase)
			val sub = [(x,qa,e),(xs,qb,e),(r,t2,env)]
		    in decr (T.rterm2term conscase, T.add2env env sub, true)
		    end
	       else raise Fail ("list_ind(" ^ T.toStringTerm t ^ ")")
	    end
*)

       else if opid = "ind"
       then let val (q,e) = get1 opid principals
		val (x,rd,downcase,basecase,y,ru,upcase) = B.get3_202bound non_principals
		val ord = T.is_zero q
		val (t',e') =
		    if ord = EQUAL then (basecase,env)
		    else let val (p,r,w,c) =
				 if ord = GREATER
				 then (T.dec_integer q,ru,y,upcase)
				 else (T.inc_integer q,rd,x,downcase)
			     val t2 = T.mk_ind_ref_nterm
					  (T.mk_rterm p)
					  (x,rd,downcase)
					  basecase
					  (y,ru,upcase)
			 in (c, T.add2env env [(w,false,T.mk_rterm q,e),(r,false,T.mk_rterm t2,env)])
			 end
	    in rdecr (t', e', true)
	    end

(*
       else if opid = "rec_ind"
       then let val (arg,f,x,B) = B.get2_02bound non_principals
		val B' = T.mk_nuprl_rec_ind_ref_term (T.mk_variable_term x) (f,x,B)
		val function = T.mk_lambda_term x B'
	    in rdecr (B, T.add2env env [(x,arg,env),(f,T.mk_rterm function,env)], true)
	    end
*)

       else if opid = "variable"
       then let val v = T.dest_variable t
	    in case T.lookup_clos env v of
		   (* the Boolean `b' indicates whether the term has already been
		    * fully evaluated. *)
		   SOME (_,b,termref) => vunit (v, b, termref, T.em_env, true)
		 | NONE => raise Fail ("variable " ^ T.toStringTerm t)
	    end

       else if opid = "!closure"
       then raise Fail "closure" (*let val (([],u),([],e)) = B.get2 non_principals
	    in unit (T.rterm2term u, T.rterm2env e, true)
	    end*)

       else if opid = "!abstraction"
       then let val ((v1,u1),(v2,u2),(v3,u3)) = B.get3 principals
	    in tunit (T.mk_nuprl_iabstraction_term v2 v3, env, false)
	    end

       else if E.is_termof_term lib t
       then tdecr (E.unfold_tof t, env, true)

       else if E.is_abstraction_term lib t
       then let val env = if List.null non_principals then em_env else env
		val ope = if cls then SOME env else NONE
	    in tupd_get_found_user (E.ct_unfold_abs ope t, env, true)
	    end

       else if List.null (T.bterms_of_term t)
       then tunit (t, em_env, false)

       else tunit (t, env, false)

    end

fun m_num_principals opid subterms (state as (steps,_)) =
    if steps < 0
       andalso opid = "apply"
       andalso T.is_nuprl_term "ycomb" (T.subterms_n 1 subterms)
    then (0, state)
    else (E.num_principals opid, state)

fun clos_refs lst = map (fn c => ([], T.mk_rterm (mk_rct c))) lst

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

fun get_opid (term, env) =
    let val opid = T.opid_of_term term
    in if T.is_ct term
       then get_opid (T.dest_ct term)
       else if opid = "variable"
       then case T.lookup_clos env (T.dest_variable term) of
		SOME (_,b,termref) => get_opid (#1 (!termref), T.em_env)
	      | NONE => raise Fail ("get_opid:variable:" ^ T.toStringTerm term)
       else opid
    end

val count = ref 0

(* EVALUATOR2 - closure conversion -- no monad *)
fun CEvaluator2' term state =

    let val timer = B.startTimer ()

	fun EvalList cbva [] env state = ([], state)
	  | EvalList cbva ((vars,rterm)::rest) env state =
	    let val term    = T.rterm2term rterm
		val (p,s1)  = Eval (term,env) state
		val (q,s2)  = if cbva then EvalAll p s1 else (p,s1)
		(*val _       = if T.is_nuprl_iclosure_term term
			      then let val clos = mct q
				       val _ = print ("[set:" ^ Int.toString (T.size term) ^ "->" ^ Int.toString (T.size clos) ^ "]\n")
				   in T.set_rterm rterm clos
				   end
			      else ()*)
		val (vs,s3) = EvalList cbva rest env s2
	    in (q::vs,s3)
	    end

	and Eval (t, env) state =
	    if T.is_ct t
	    then Eval (T.dest_ct t) state
	    else let val (opr as (opid,params),subterms) = T.dest_term t
		     val (n,s1) = m_num_principals opid subterms state
		     val (p,np) = B.split n subterms
				  handle Subscript =>
					 raise Fail ("Eval:split:Subscript:"
						     ^ opid
						     ^ "("
						     ^ Int.toString (List.length subterms)
						     ^ ")"
						     ^ ":"
						     ^ Int.toString n)
		     val (c,s2 as (_,(lib,cls))) = EvalList (E.is_eval_all opid) p env s1
		 (*val _ = print ("[----" ^ opid ^ "----]\n")*)
		 in let val ((nt,e',ev),s3) = ClosNextStepEval2 cls lib t c np env s2
			    (*val _ = print ("[size-1: "     ^ Int.toString (T.size t)       ^ "]\n")
			     val _ = print ("[size-env-1: " ^ Int.toString (T.size_env env) ^ "]\n")
			     val _ = print ("[size-2: "     ^ Int.toString (T.size t')      ^ "]\n")
			     val _ = print ("[size-env-2: " ^ Int.toString (T.size_env e')  ^ "]\n")*)
		    in if ev
		       then (*Eval (next2term nt,e') s3*)
			   case nt of
			       NEXT_R rterm => Eval (T.rterm2term rterm,e') s3
			     | NEXT_T term  => Eval (term,e') s3
			     | NEXT_V (v,a,termref) =>
			       let val (t0,b) = !termref
			       in if a orelse b
				  then (T.pull_out_envs (t0,e'), s3)
				  else let val ((t,e),s) = Eval (t0,e') s3
					   val (t1,e1) = T.pull_out_envs (t,e)
					   val e1' = (*T.filter_env t1*) e1
					   val _ =
					       (*if !count = 631 orelse !count = 632
					       then let val op1 = get_opid (t0,e')
							val op2 = get_opid (t1,e1')
							val n1  = get_steps s3
							val n2  = get_steps s
							(*val _ = termref := (T.mk_ct (T.mk_rterm t0, e'), false)*)
							(*val _ = termref := (T.mk_ct (T.mk_rterm t1, e1'), false)*)
							val _ =
							    if !count = 631
							    then termref := (T.refresh_term (T.mk_ct (T.mk_rterm t1, e1')), false)
							    else ()
						    in print ("\n------\nset"
							      ^ "(" ^ Int.toString (!count) ^ ")"
							      ^ v
							      ^ ":\n"
							      ^ op1 ^ "(" ^ Int.toString n1 ^ ")"
							      ^ "\n--\n"
							      ^ T.toStringTerm (T.CLO_TERM (T.mk_rterm t0, e'))
							      ^ "\n--\n"
							      ^ op2 ^ "(" ^ Int.toString n2 ^ ")"
							      ^ "\n--\n"
							      ^ T.toStringTerm (T.CLO_TERM (T.mk_rterm t1, e1'))
							      ^ "\n------\n")
						    end
					       else*) termref := (T.mk_ct (T.mk_rterm t1, e1'), true)
					   val _ = count := !count + 1
				       in ((t1,e1'),s)
				       end
			       end
		       else ((next2term nt,e'),s3)
		    end (*handle _ => ((T.mk_nuprl_ref_term opr ((clos_ref_lst c) @ np), env), s2)*)
		 end

	and EvalAll (t, env) state =
	    if T.is_ct t
	    then EvalAll (T.dest_ct t) state
	    else let val (opr as (opid,params),subterms) = T.dest_term t
		     val (principals,nprincipals) = B.split (E.num_principal_all opid) subterms
		     val env' = if List.null nprincipals then em_env else env
		     val (c,s) = EvalList true principals env state
		 in ((T.mk_nuprl_ref_term opr ((clos_refs c) @ nprincipals), env'),s)
		 end

    in Eval (term, em_env) state
    end

fun Evaluator2 cls lib steps term =
    let val _ = print "[starting evaluation]\n"
	val ((t',e'),(n,_)) = CEvaluator2' term (steps,(lib,cls))
	(*val _ = print ("***" ^ T.opid_of_term t')*)
	val answer = T.mk_rct (t', e')
	    (*if cls andalso T.is_nuprl_pair_term t'
	    then let val (s,msgs) = T.dest_pair 8 t'
		     val _ = print "[closing messages]\n"
		     (*val _ = print ("\n-------MSGS(1)------\n" ^ T.toStringTerm msgs ^ "\n------------\n")*)
		     val msgs' = T.close msgs e'
		 in T.mk_pair_term (T.mk_rct (s, e')) msgs'
		 end
	    else let val _ = print "[closing term]\n"
		 in T.close t' e'
		 end*)
	val steps' = steps - n
	val _ = print ("[evaluation done (" ^ Int.toString steps' ^ " steps)]\n")
    in (answer, steps')
    end

end
