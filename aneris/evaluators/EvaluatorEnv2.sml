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
 *  o File name:   EvaluatorEnv2.sml
 *  o Description: .
 *)


structure EvaluatorEnv2 = struct

structure B = Tools
structure T = NuprlTerms
structure P = Primitive

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

fun eval env term =
    case term of
	T.ADD_TERM (rterm1, rterm2) =>
	let val (v1,_) = eval_rterm env rterm1
	    val (v2,_) = eval_rterm env rterm2
	in (T.do_primitive_int_op "add" v1 v2, T.em_env)
	end
      | T.SUB_TERM (rterm1, rterm2) =>
	let val (v1,_) = eval_rterm env rterm1
	    val (v2,_) = eval_rterm env rterm2
	in (T.do_primitive_int_op "subtract" v1 v2, T.em_env)
	end
      | T.MUL_TERM (rterm1, rterm2) =>
	let val (v1,_) = eval_rterm env rterm1
	    val (v2,_) = eval_rterm env rterm2
	in (T.do_primitive_int_op "multiply" v1 v2, T.em_env)
	end
      | T.DIV_TERM (rterm1, rterm2) =>
	let val (v1,_) = eval_rterm env rterm1
	    val (v2,_) = eval_rterm env rterm2
	in (T.do_primitive_int_op "divide" v1 v2, T.em_env)
	end
      | T.REM_TERM (rterm1, rterm2) =>
	let val (v1,_) = eval_rterm env rterm1
	    val (v2,_) = eval_rterm env rterm2
	in (T.do_primitive_int_op "remainder" v1 v2, T.em_env)
	end
      | T.MIN_TERM rterm =>
	let val (v,_) = eval_rterm env rterm
	in (T.do_primitive_minus v, T.em_env)
	end
      | T.LES_TERM (rterm1, rterm2, rterm3, rterm4) =>
	let val (v1,_) = eval_rterm env rterm1
	    val (v2,_) = eval_rterm env rterm2
	in if T.do_primitive_cmp "less" v1 v2
	   then eval_rterm env rterm3
	   else eval_rterm env rterm4
	end
      | T.IEQ_TERM (rterm1, rterm2, rterm3, rterm4) =>
	let val (v1,_) = eval_rterm env rterm1
	    val (v2,_) = eval_rterm env rterm2
	in if T.do_primitive_cmp "int_eq" v1 v2
	   then eval_rterm env rterm3
	   else eval_rterm env rterm4
	end
      | T.AEQ_TERM (n, rterm1, rterm2, rterm3, rterm4) =>
	let val (v1,_) = eval_rterm env rterm1
	    val (v2,_) = eval_rterm env rterm2
	in if T.compare_atomn n v1 v2
	   then eval_rterm env rterm3
	   else eval_rterm env rterm4
	end
      | T.EQT_TERM (rterm1, rterm2) =>
	let val (v1,e1) = eval_then_all env rterm1
	    val (v2,e2) = eval_then_all env rterm2
	in if alpha_equal_complete_primitive_terms v1 e1 v2 e2
	   then (T.mk_inl_term T.mk_axiom_term, T.em_env)
	   else (T.mk_inr_term T.mk_axiom_term, T.em_env)
	end
      | T.IIR_TERM (rterm1, rterm2, rterm3) =>
	let val (v1,_) = eval_rterm env rterm1
	in if P.do_primitive_test "isinr" v1
	   then eval_rterm env rterm2
	   else eval_rterm env rterm3
	end
      | T.IIL_TERM (rterm1, rterm2, rterm3) =>
	let val (v1,_) = eval_rterm env rterm1
	in if P.do_primitive_test "isinl" v1
	   then eval_rterm env rterm2
	   else eval_rterm env rterm3
	end
      | T.IPA_TERM (rterm1, rterm2, rterm3) =>
	let val (v1,_) = eval_rterm env rterm1
	in if P.do_primitive_test "ispair" v1
	   then eval_rterm env rterm2
	   else eval_rterm env rterm3
	end
      | T.IIN_TERM (rterm1, rterm2, rterm3) =>
	let val (v1,_) = eval_rterm env rterm1
	in if P.do_primitive_test "isint" v1
	   then eval_rterm env rterm2
	   else eval_rterm env rterm3
	end
      | T.ILA_TERM (rterm1, rterm2, rterm3) =>
	let val (v1,_) = eval_rterm env rterm1
	in if P.do_primitive_test "islambda" v1
	   then eval_rterm env rterm2
	   else eval_rterm env rterm3
	end
      | T.IAT_TERM (rterm1, rterm2, rterm3) =>
	let val (v1,_) = eval_rterm env rterm1
	in if P.do_primitive_test "isatom2" v1
	   then eval_rterm env rterm2
	   else eval_rterm env rterm3
	end
      | T.IAX_TERM (rterm1, rterm2, rterm3) =>
	let val (v1,_) = eval_rterm env rterm1
	in if P.do_primitive_test "isaxiom" v1
	   then eval_rterm env rterm2
	   else eval_rterm env rterm3
	end
      | T.SPR_TERM (pair, var1, var2, rterm) =>
	let val (p,e) = eval_rterm env pair
	    val (a,b) = T.dest_rpair 5 p
	    val bindings = [(var2,false,b,e),(var1,false,a,e)]
	in eval_rterm (T.add2env env bindings) rterm
	end
      | T.DEC_TERM (dec, var1, rterm1, var2, rterm2) =>
	let val (v,e) = eval_rterm env dec
	in if T.is_nuprl_inl_term v
	   then let val bindings = [(var1,false,T.mk_rterm (T.dest_inl v),e)]
		in eval_rterm (T.add2env env bindings) rterm1
		end
	   else if T.is_nuprl_inr_term v
	   then let val bindings = [(var2,false,T.mk_rterm (T.dest_inr v),e)]
		in eval_rterm (T.add2env env bindings) rterm2
		end
	   else raise Fail ("decide(" ^ T.opid_of_term v ^ ")")
	end
      | T.APP_TERM (f, a) =>
	let val (v,e) = eval_rterm env f
	    val (x,b) = T.dest_ref_lambda 5 v
	    val bindings = [(x,false,a,env)]
	in eval_rterm (T.add2env e bindings) b
	end
      | T.FIX_TERM rterm =>
	eval env (T.mk_apply_ref_term rterm (T.mk_rterm term))
	(*let val (v,e) = eval_rterm env rterm
	in eval e (T.mk_apply_term v (T.FIX_TERM (T.mk_rterm v)))
	end*)
      | T.WAI_TERM (rterm1, rterm2) =>
	let val (t,_) = eval_rterm env rterm1
	in eval_rterm env (T.do_primitive_ref_wait t rterm2)
	end
      | T.CBV_TERM (rterm1, var, rterm2) =>
       let val (x,e) = eval_rterm env rterm1
       in if P.is_primitive_value x
	  then let val bindings = [(var,false,T.mk_rterm x,e)]
	       in eval_rterm (T.add2env env bindings) rterm2
	       end
	  else raise Fail "eval:callbyvalue"
       end
      | T.CBA_TERM (rterm1, var, rterm2) =>
       let val (x,e) = eval_then_all env rterm1
       in if is_complete_primitive_value e x
	  then let val bindings = [(var,true,T.mk_rterm x,e)]
	       in eval_rterm (T.add2env env bindings) rterm2
	       end
	  else raise Fail ("eval:callbyvalueall(" ^ T.toStringTerm (T.mk_rct (x, e)) ^ ")")
       end
      | T.IND_TERM (rterm, varx, vard, downcase, basecase, vary, varu, upcase) =>
	raise Fail "eval:IND_TERM"
	(*let val (x,e) = eval_rterm env rterm
	    val ord = T.is_zero x
	in if ord = EQUAL
	   then eval_rterm env basecase
	   else let fun mk_t2 f =
			T.mk_rterm (T.mk_ind_ref_nterm
					(T.mk_rterm (f x))
					(varx, vard, downcase)
					basecase
					(vary, varu, upcase))
		in if ord = GREATER
		   then let val t2 = mk_t2 T.dec_integer
			    val bindings = [(vary,T.mk_rterm x,e),(varu,t2,env)]
			in eval_rterm (T.add2env env bindings) upcase
			end
		   else let val t2 = mk_t2 T.inc_integer
			    val bindings = [(varx,T.mk_rterm x,e),(vard,t2,env)]
			in eval_rterm (T.add2env env bindings) downcase
			end
		end
	end*)
      | T.VAR_TERM var =>
	let val v = T.dest_nuprl_var var
	in case T.lookup_clos env v of
	       SOME (_,b,termref) =>
	       let val (t0,b) = !termref
		   val (t,e) = eval T.em_env t0
		   val _ = termref := (T.mk_rct (t, e), true)
	       in (t,e)
	       end
	     | NONE => raise Fail "eval:VAR_TERM:NONE"
	end
      | T.AXM_TERM   => (term, env)
      | T.BOT_TERM   => raise Fail "eval_all:BOT_TERM"
      | T.INT_TERM   => (term, env)
      | T.VOI_TERM   => (term, env)
      | T.DUM_TERM   => raise Fail "eval_all:DUM_TERM"
      | T.ATM_TERM _ => (term, env)
      | T.TOK_TERM _ => (term, env)
      | T.NAT_TERM _ => (term, env)
      | T.INL_TERM _ => (term, env)
      | T.INR_TERM _ => (term, env)
      | T.LAM_TERM _ => (term, env)
      | T.REC_TERM _ => (term, env)
      | T.PAI_TERM _ => (term, env)
      | T.EQU_TERM _ => (term, env)
      | T.FUN_TERM _ => (term, env)
      | T.PRD_TERM _ => (term, env)
      | T.TUN_TERM _ => (term, env)
      | T.SET_TERM _ => (term, env)
      | T.UNI_TERM _ => (term, env)
      | T.CLO_TERM (rterm, env) => eval_rterm env rterm
      | T.TERM _ => raise Fail ("eval:TERM(" ^ T.toStringTerm term ^ ")")

and eval_rterm env rterm = eval env (T.rterm2term rterm)

and eval_then_all env rterm =
    let val (t1,e1) = eval_rterm env rterm
	val (t2,e2) = eval_all e1 t1
    in (t2, e2)
    end

and eval_then_all_rterm env rterm =
    let val (t,e) = eval_then_all env rterm
    in (T.mk_rterm t, e)
    end

and eval_all env term =
    case term of
	(* set_all_1 *)
	T.INL_TERM rterm =>
	let val (rt,e) = eval_then_all_rterm env rterm
	in (T.INL_TERM rt, e)
	end
      | T.INR_TERM rterm =>
	let val (rt,e) = eval_then_all_rterm env rterm
	in (T.INR_TERM rt, e)
	end
      | T.FUN_TERM (rterm1, var, rterm2) =>
	let val (rt,e) = eval_then_all_rterm env rterm1
	    val (env',v) = T.new_evall_add2env env rt e
	    val rt' = T.mk_rterm (T.VAR_TERM v)
	in (T.FUN_TERM (rt', var, rterm2), env')
	end
      | T.PRD_TERM (rterm1, var, rterm2) =>
	let val (rt,e) = eval_then_all_rterm env rterm1
	    val (env',v) = T.new_evall_add2env env rt e
	    val rt' = T.mk_rterm (T.VAR_TERM v)
	in (T.PRD_TERM (rt', var, rterm2), env')
	end
      | T.TUN_TERM (rterm1, var, rterm2) =>
	let val (rt,e) = eval_then_all_rterm env rterm1
	    val (env',v) = T.new_evall_add2env env rt e
	    val rt' = T.mk_rterm (T.VAR_TERM v)
	in (T.TUN_TERM (rt', var, rterm2), env')
	end
      | T.SET_TERM (rterm1, var, rterm2) =>
	let val (rt,e) = eval_then_all_rterm env rterm1
	    val (env',v) = T.new_evall_add2env env rt e
	    val rt' = T.mk_rterm (T.VAR_TERM v)
	in (T.SET_TERM (rt', var, rterm2), env')
	end
      (* + isect, quotient *)
      (* set_all_2 *)
      | T.PAI_TERM (rterm1, rterm2) =>
	let val (rt1,e1) = eval_then_all_rterm env rterm1
	    val (rt2,e2) = eval_then_all_rterm env rterm2
	    val (env1,nvar1) = T.new_evall_add2env T.em_env rt1 e1
	    val (env2,nvar2) = T.new_evall_add2env env1 rt2 e2
	    val v1 = T.mk_rterm (T.VAR_TERM nvar1)
	    val v2 = T.mk_rterm (T.VAR_TERM nvar2)
	in (T.PAI_TERM (v1, v2), env2)
	end
      | T.UNI_TERM (rterm1, rterm2) =>
	let val (rt1,e1) = eval_then_all_rterm env rterm1
	    val (rt2,e2) = eval_then_all_rterm env rterm2
	    val (env1,nvar1) = T.new_evall_add2env T.em_env rt1 e1
	    val (env2,nvar2) = T.new_evall_add2env env1 rt2 e2
	    val v1 = T.mk_rterm (T.VAR_TERM nvar1)
	    val v2 = T.mk_rterm (T.VAR_TERM nvar2)
	in (T.UNI_TERM (v1, v2), env2)
	end
      (* + subtype_rel *)
      (* set_all_3 *)
      | T.EQU_TERM (rterm1, rterm2, rterm3) =>
	let val (rt1,e1) = eval_then_all_rterm env rterm1
	    val (rt2,e2) = eval_then_all_rterm env rterm2
	    val (rt3,e3) = eval_then_all_rterm env rterm3
	    val (env1,nvar1) = T.new_evall_add2env T.em_env rt1 e1
	    val (env2,nvar2) = T.new_evall_add2env env1 rt2 e2
	    val (env3,nvar3) = T.new_evall_add2env env2 rt3 e3
	    val v1 = T.mk_rterm (T.VAR_TERM nvar1)
	    val v2 = T.mk_rterm (T.VAR_TERM nvar2)
	    val v3 = T.mk_rterm (T.VAR_TERM nvar3)
	in (T.EQU_TERM (v1, v2, v3), env3)
	end
      (* rest *)
      | T.AXM_TERM   => (term, env)
      | T.INT_TERM   => (term, env)
      | T.VOI_TERM   => (term, env)
      | T.ATM_TERM _ => (term, env)
      | T.TOK_TERM _ => (term, env)
      | T.NAT_TERM _ => (term, env)
      | T.VAR_TERM _ => (term, env)
      | T.MIN_TERM _ => (term, env)
      | T.LAM_TERM _ => (term, env)
      | T.REC_TERM _ => (term, env)
      (* eval should have gotten rid of these *)
      | T.BOT_TERM   => raise Fail "eval_all:BOT_TERM"
      | T.DUM_TERM   => raise Fail "eval_all:DUM_TERM"
      | T.FIX_TERM _ => raise Fail "eval_all:FIX_TERM"
      | T.WAI_TERM _ => raise Fail "eval_all:WAI_TERM"
      | T.APP_TERM _ => raise Fail "eval_all:APP_TERM"
      | T.ADD_TERM _ => raise Fail "eval_all:ADD_TERM"
      | T.SUB_TERM _ => raise Fail "eval_all:SUB_TERM"
      | T.MUL_TERM _ => raise Fail "eval_all:MUL_TERM"
      | T.DIV_TERM _ => raise Fail "eval_all:DIV_TERM"
      | T.REM_TERM _ => raise Fail "eval_all:REM_TERM"
      | T.EQT_TERM _ => raise Fail "eval_all:EQT_TERM"
      | T.IAX_TERM _ => raise Fail "eval_all:IAX_TERM"
      | T.IPA_TERM _ => raise Fail "eval_all:IPA_TERM"
      | T.IIR_TERM _ => raise Fail "eval_all:IIR_TERM"
      | T.IIL_TERM _ => raise Fail "eval_all:IIL_TERM"
      | T.IIN_TERM _ => raise Fail "eval_all:IIN_TERM"
      | T.ILA_TERM _ => raise Fail "eval_all:ILA_TERM"
      | T.IAT_TERM _ => raise Fail "eval_all:IAT_TERM"
      | T.CBV_TERM _ => raise Fail "eval_all:CBV_TERM"
      | T.CBA_TERM _ => raise Fail "eval_all:CBA_TERM"
      | T.LES_TERM _ => raise Fail "eval_all:LES_TERM"
      | T.IEQ_TERM _ => raise Fail "eval_all:IEQ_TERM"
      | T.SPR_TERM _ => raise Fail "eval_all:SPR_TERM"
      | T.AEQ_TERM _ => raise Fail "eval_all:AEQ_TERM"
      | T.DEC_TERM _ => raise Fail "eval_all:DEC_TERM"
      | T.IND_TERM _ => raise Fail "eval_all:IND_TERM"
      | T.CLO_TERM _ => raise Fail "eval_all:CLO_TERM"
      | T.TERM _ => raise Fail "eval_all:TERM"

fun Evaluator2 cls lib steps term =
    let val _ = print "[starting evaluation]\n"
	val (t',e') = eval T.em_env term
	val answer =
	    if cls andalso T.is_nuprl_pair_term t'
	    then let val (s,msgs) = T.dest_pair 8 t'
		     val _ = print "[closing messages]\n"
		     val msgs' = T.close msgs e'
		 in T.mk_pair_term (T.mk_rct (s, e')) msgs'
		 end
	    else let val _ = print "[closing term]\n"
		 in T.close t' e'
		 end
	val _ = print ("[evaluation done (? steps)]\n")
    in (answer, 0)
    end

end
