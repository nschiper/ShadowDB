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
 *  o File name:   EvaluatorRef2.sml
 *  o Description: .
 *)


structure EvaluatorRef2 = struct

structure T = NuprlTerms
structure P = Primitive

fun eval term =
    case term of
	T.ADD_TERM (rterm1, rterm2) =>
	let val v1 = eval_rterm rterm1
	    val v2 = eval_rterm rterm2
	in T.do_primitive_int_op "add" v1 v2
	end
      | T.SUB_TERM (rterm1, rterm2) =>
	let val v1 = eval_rterm rterm1
	    val v2 = eval_rterm rterm2
	in T.do_primitive_int_op "subtract" v1 v2
	end
      | T.MUL_TERM (rterm1, rterm2) =>
	let val v1 = eval_rterm rterm1
	    val v2 = eval_rterm rterm2
	in T.do_primitive_int_op "multiply" v1 v2
	end
      | T.DIV_TERM (rterm1, rterm2) =>
	let val v1 = eval_rterm rterm1
	    val v2 = eval_rterm rterm2
	in T.do_primitive_int_op "divide" v1 v2
	end
      | T.REM_TERM (rterm1, rterm2) =>
	let val v1 = eval_rterm rterm1
	    val v2 = eval_rterm rterm2
	in T.do_primitive_int_op "remainder" v1 v2
	end
      | T.MIN_TERM rterm =>
	let val v = eval_rterm rterm
	in case v of
	       T.NAT_TERM _ => T.mk_minus_term v
	     | _ => T.do_primitive_minus v
	end
      | T.LES_TERM (rterm1, rterm2, rterm3, rterm4) =>
	let val v1 = eval_rterm rterm1
	    val v2 = eval_rterm rterm2
	in if T.do_primitive_cmp "less" v1 v2
	   then eval_rterm rterm3
	   else eval_rterm rterm4
	end
      | T.IEQ_TERM (rterm1, rterm2, rterm3, rterm4) =>
	let val v1 = eval_rterm rterm1
	    val v2 = eval_rterm rterm2
	in if T.do_primitive_cmp "int_eq" v1 v2
	   then eval_rterm rterm3
	   else eval_rterm rterm4
	end
      | T.AEQ_TERM (n, rterm1, rterm2, rterm3, rterm4) =>
	let val v1 = eval_rterm rterm1
	    val v2 = eval_rterm rterm2
	in if T.compare_atomn n v1 v2
	   then eval_rterm rterm3
	   else eval_rterm rterm4
	end
      | T.EQT_TERM (rterm1, rterm2) =>
	let val v1 = eval_all (eval_rterm rterm1)
	    val v2 = eval_all (eval_rterm rterm2)
	in if P.is_complete_primitive_value v1
	   then if P.is_complete_primitive_value v2
		then if T.alpha_equal_terms v1 v2
		     then T.mk_inl_term T.mk_axiom_term
		     else T.mk_inr_term T.mk_axiom_term
		else raise Fail ("eq_term(2,"
				 ^ T.opid_of_term v2
				 ^ ","
				 ^ T.toStringTerm v2
				 ^ ")")
	   else raise Fail ("eq_term(1,"
			    ^ T.opid_of_term v1
			    ^ ","
			    ^ T.toStringTerm v1
			    ^ ")")
	end
      | T.IIR_TERM (rterm1, rterm2, rterm3) =>
	let val v1 = eval_rterm rterm1
	in if P.do_primitive_test "isinr" v1
	   then eval_rterm rterm2
	   else eval_rterm rterm3
	end
      | T.IIL_TERM (rterm1, rterm2, rterm3) =>
	let val v1 = eval_rterm rterm1
	in if P.do_primitive_test "isinl" v1
	   then eval_rterm rterm2
	   else eval_rterm rterm3
	end
      | T.IPA_TERM (rterm1, rterm2, rterm3) =>
	let val v1 = eval_rterm rterm1
	in if P.do_primitive_test "ispair" v1
	   then eval_rterm rterm2
	   else eval_rterm rterm3
	end
      | T.IIN_TERM (rterm1, rterm2, rterm3) =>
	let val v1 = eval_rterm rterm1
	in if P.do_primitive_test "isint" v1
	   then eval_rterm rterm2
	   else eval_rterm rterm3
	end
      | T.ILA_TERM (rterm1, rterm2, rterm3) =>
	let val v1 = eval_rterm rterm1
	in if P.do_primitive_test "islambda" v1
	   then eval_rterm rterm2
	   else eval_rterm rterm3
	end
      | T.IAT_TERM (rterm1, rterm2, rterm3) =>
	let val v1 = eval_rterm rterm1
	in if P.do_primitive_test "isatom2" v1
	   then eval_rterm rterm2
	   else eval_rterm rterm3
	end
      | T.IAX_TERM (rterm1, rterm2, rterm3) =>
	let val v1 = eval_rterm rterm1
	in if P.do_primitive_test "isaxiom" v1
	   then eval_rterm rterm2
	   else eval_rterm rterm3
	end
      | T.SPR_TERM (pair, var1, var2, rterm) =>
	let val v1 = eval_rterm pair
	    val (a,b) = T.dest_rpair 5 v1
	    val _ = T.set_nuprl_rvar 1 var1 a
	    val _ = T.set_nuprl_rvar 2 var2 b
	in eval_rterm rterm
	end
      | T.DEC_TERM (dec, var1, rterm1, var2, rterm2) =>
	let val v = eval_rterm dec
	in if T.is_nuprl_inl_term v
	   then let val _ = T.set_nuprl_var 3 var1 (T.dest_inl v)
		in eval_rterm rterm1
		end
	   else if T.is_nuprl_inr_term v
	   then let val _ = T.set_nuprl_var 4 var2 (T.dest_inr v)
		in eval_rterm rterm2
		end
	   else raise Fail ("decide(" ^ T.opid_of_term v ^ ")")
	end
      | T.APP_TERM (f, a) =>
	let val v = eval_rterm f
	    val (x,b) = T.dest_lambda 5 v
	    val _ = T.set_nuprl_rvar 5 x a
	in eval b
	end
      | T.FIX_TERM rterm =>
	let val v = eval_rterm rterm
	    val (x,b) = T.dest_lambda 6 v
	    val _ = T.set_nuprl_var 6 x term
	in eval b
	end
      | T.WAI_TERM (rterm1, rterm2) =>
	let val t = eval_rterm rterm1
	in eval_rterm (T.do_primitive_ref_wait t rterm2)
	end
      | T.CBV_TERM (rterm1, var, rterm2) =>
       let val x = eval_rterm rterm1
       in if P.is_primitive_value x
	  then let val _ = T.set_nuprl_var 7 var x
	       in eval_rterm rterm2
	       end
	  else raise Fail "callbyvalue"
       end
      | T.CBA_TERM (rterm1, var, rterm2) =>
       let val x = eval_all (eval_rterm rterm1)
       in if P.is_complete_primitive_value x
	  then let val _ = T.set_nuprl_var 8 var x
	       in eval_rterm rterm2
	       end
	  else raise Fail "callbyvalueall"
       end
      | T.IND_TERM (rterm, varx, vard, downcase, basecase, vary, varu, upcase) =>
	let val x = eval_rterm rterm
	    val ord = T.is_zero x
	in if ord = EQUAL
	   then eval_rterm basecase
	   else if ord = GREATER
	   then let val t2 = T.mk_ind_ref_term
				 (T.mk_rterm (T.dec_integer x))
				 (T.dest_nuprl_var varx, T.dest_nuprl_var vard, downcase)
				 basecase
				 (T.dest_nuprl_var vary, T.dest_nuprl_var varu, upcase)
		    val _ = T.set_nuprl_var 9  vary x
		    val _ = T.set_nuprl_var 10 varu t2
		in eval_rterm upcase
		end
	   else let val t2 = T.mk_ind_ref_term
				 (T.mk_rterm (T.dec_integer x))
				 (T.dest_nuprl_var varx, T.dest_nuprl_var vard, downcase)
				 basecase
				 (T.dest_nuprl_var vary, T.dest_nuprl_var varu, upcase)
		    val _ = T.set_nuprl_var 11 varx x
		    val _ = T.set_nuprl_var 12 vard t2
		in eval_rterm downcase
		end
	end
      | T.VAR_TERM nuprl_var =>
	(case T.get_nuprl_var_ref nuprl_var of
	     NONE => raise Fail "eval:VAR_TERM:NONE"
	   | SOME r =>
	     let val t = eval (!r)
		 val _ = r := t
	     in t
	     end)
      | T.AXM_TERM => term
      | T.BOT_TERM => term
      | T.INT_TERM => term
      | T.DUM_TERM => term
      | T.ATM_TERM _ => term
      | T.TOK_TERM _ => term
      | T.NAT_TERM _ => term
      | T.INL_TERM _ => term
      | T.INR_TERM _ => term
      | T.LAM_TERM _ => term
      | T.REC_TERM _ => term
      | T.PAI_TERM _ => term
      | T.EQU_TERM _ => term
      | T.FUN_TERM _ => term
      | T.PRD_TERM _ => term
      | T.TUN_TERM _ => term
      | T.UNI_TERM _ => term
      | T.CLO_TERM _ => raise Fail "eval:CLO_TERM"
      | T.TERM _ => raise Fail ("eval:TERM(" ^ T.toStringTerm term ^ ")")

and eval_rterm rterm = eval (T.rterm2term rterm)

and eval_all term =
    case term of
	(* set_all_1 *)
	T.INL_TERM rterm => T.INL_TERM (eval_then_all_rterm rterm)
      | T.INR_TERM rterm => T.INR_TERM (eval_then_all_rterm rterm)
      | T.FUN_TERM (rterm1, var, rterm2) =>
	T.FUN_TERM (eval_then_all_rterm rterm1, var, rterm2)
      | T.PRD_TERM (rterm1, var, rterm2) =>
	T.PRD_TERM (eval_then_all_rterm rterm1, var, rterm2)
      | T.TUN_TERM (rterm1, var, rterm2) =>
	T.TUN_TERM (eval_then_all_rterm rterm1, var, rterm2)
      (* + isect, set, quotient *)
      (* set_all_2 *)
      | T.PAI_TERM (rterm1, rterm2) =>
	T.PAI_TERM (eval_then_all_rterm rterm1,
		    eval_then_all_rterm rterm2)
      | T.UNI_TERM (rterm1, rterm2) =>
	T.UNI_TERM (eval_then_all_rterm rterm1,
		    eval_then_all_rterm rterm2)
      (* + subtype_rel *)
      (* set_all_3 *)
      | T.EQU_TERM (rterm1, rterm2, rterm3) =>
	T.EQU_TERM (eval_then_all_rterm rterm1,
		    eval_then_all_rterm rterm2,
		    eval_then_all_rterm rterm3)
      | T.AXM_TERM => term
      | T.BOT_TERM => term
      | T.INT_TERM => term
      | T.DUM_TERM => term
      | T.ATM_TERM _ => term
      | T.TOK_TERM _ => term
      | T.NAT_TERM _ => term
      | T.VAR_TERM _ => term
      | T.FIX_TERM _ => term
      | T.MIN_TERM _ => term
      | T.LAM_TERM _ => term
      | T.REC_TERM _ => term
      | T.WAI_TERM _ => term
      | T.APP_TERM _ => term
      | T.ADD_TERM _ => term
      | T.SUB_TERM _ => term
      | T.MUL_TERM _ => term
      | T.DIV_TERM _ => term
      | T.REM_TERM _ => term
      | T.EQT_TERM _ => term
      | T.IAX_TERM _ => term
      | T.IPA_TERM _ => term
      | T.IIR_TERM _ => term
      | T.IIL_TERM _ => term
      | T.IIN_TERM _ => term
      | T.ILA_TERM _ => term
      | T.IAT_TERM _ => term
      | T.CBV_TERM _ => term
      | T.CBA_TERM _ => term
      | T.LES_TERM _ => term
      | T.IEQ_TERM _ => term
      | T.SPR_TERM _ => term
      | T.AEQ_TERM _ => term
      | T.DEC_TERM _ => term
      | T.IND_TERM _ => term
      | T.CLO_TERM _ => raise Fail "eval_all:CLO_TERM"
      | T.TERM _ => raise Fail "eval_all:TERM"

and eval_then_all_rterm rterm =
    T.mk_rterm (eval_all (eval_rterm rterm))

fun Evaluator2 cls lib steps term =
    let val _ = print "[starting evaluation]\n"
	val t' = eval term
	val _ = print ("[evaluation done (? steps)]\n")
    in (t', 0)
    end

end
