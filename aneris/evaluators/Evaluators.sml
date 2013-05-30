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
 *  o File name:   Evaluators.sml
 *  o Description: .
 *)


structure Evaluators :> EVALUATOR = struct

structure T    = NuprlTerms
structure P    = ParserNuprlAscii
structure EV1  = Evaluator1
structure EV2  = Evaluator2
structure EV3  = Evaluator3
structure EV4  = Evaluator4
structure EV5  = Evaluator5
structure EVE2 = EvaluatorEnv2
(*structure EVR2 = EvaluatorRef2*)

type eval_sig = int -> T.nuprl_term -> T.nuprl_term * int

val mapref = ref (T.emlib ())

val term1 =
    let val pair  = T.mk_pair_term (T.mk_nuprl_small_integer_term 1) (T.mk_nuprl_small_integer_term 2)
	val bterm = T.mk_variable_term "x"
	val name  = "term1: simple spread reducing to 1"
    in (name, T.mk_spread_term pair ("x", "y", bterm))
    end

val term2 =
    let val pair  = T.mk_pair_term (T.mk_nuprl_small_integer_term 1) (T.mk_nuprl_small_integer_term 2)
	val bterm = T.mk_variable_term "y"
	val name  = "term2: simple spread reducing to 2"
    in (name, T.mk_spread_term pair ("x", "y", bterm))
    end

val term3 =
    let val inr   = T.mk_inr_term (T.mk_nuprl_small_integer_term 2)
	val inl   = T.mk_inl_term (T.mk_nuprl_small_integer_term 3)
	val pair  = T.mk_pair_term inr inl
	val sprd  = T.mk_spread_term pair ("x", "y", T.mk_variable_term "y")
	val bt1   = ("x", T.mk_add_term (T.mk_variable_term "x") (T.mk_nuprl_small_integer_term 4))
	val bt2   = ("y", T.mk_add_term (T.mk_variable_term "y") (T.mk_nuprl_small_integer_term 5))
	val name  = "term3: simple decide reducing to 3 + 4 = 7"
    in (name, T.mk_decide_term sprd bt1 bt2)
    end

(*
(* This is the definition of a length of a list. *)
val term4 =
    let val L        = T.mk_variable_term "x"
	val nilcase  = T.mk_nuprl_small_integer_term 0
	val conscase = T.mk_nuprl_add_term (T.mk_nuprl_small_integer_term 1) (T.mk_variable_term "r")
	val recList  = T.mk_list_ind_term L nilcase ("x", "xs", "r", conscase)
	val name     = "term4: the length of a list function"
    in (name, T.mk_lambda_term "x" recList)
    end
*)

(*
val term5 =
    let val list1 = T.mk_cons_term (T.mk_nuprl_small_integer_term 1) (T.mk_nil_term ())
	val list2 = T.mk_cons_term (T.mk_nuprl_small_integer_term 6) list1
	val list3 = T.mk_cons_term (T.mk_nuprl_small_integer_term 3) list2
	val name  = "term5: length of [3, 6, 1]"
    in (name, T.mk_apply_term (#2 term4) list3)
    end
*)

(*
(* This is the example in the closure conversion file (Nuprl's implementation). *)
val term6 =
    let val L2        = T.mk_variable_term "xs"
	(* nilcase2 is supposed to be the size of xs,
	 * i.e., length should be an abstraction. *)
	val nilcase2  = T.mk_apply_term (T.mk_variable_term "length") (T.mk_variable_term "xs")
	val conscase2 = T.mk_add_term (T.mk_variable_term "x") (T.mk_variable_term "r")
	val L1        = T.mk_variable_term "x"
	val nilcase1  = T.mk_nuprl_small_integer_term 0
	val conscase1 = T.mk_list_ind_term L2 nilcase2 ("x", "xs", "r", conscase2)
	val recList   = T.mk_list_ind_term L1 nilcase1 ("x", "xs", "r", conscase1)
	val func      = T.mk_lambda_term "x" recList
	val list      = T.mk_cons_term (T.mk_nuprl_small_integer_term 1) (T.mk_cons_term (T.mk_nuprl_small_integer_term 2) (T.mk_nil_term ()))
	val name      = "term6: example from 'closure conversion', should reduce to 3"
    in (name, T.mk_apply_term (T.mk_lambda_term "length" (T.mk_apply_term func list)) (#2 term4))
    end
*)

val term7 =
    let val arg  = T.mk_pair_term (#2 term3) (T.mk_nuprl_small_integer_term 0)
	val name = "term7: simple example involving callbyvalue"
    in (name, T.mk_callbyvalue_term arg ("x", T.mk_variable_term "x"))
    end

val term8 =
    let val arg  = T.mk_pair_term (#2 term3) (T.mk_nuprl_small_integer_term 0)
	val name = "term8: simple example involving callbyvalueall"
    in (name, T.mk_callbyvalueall_term arg ("x", T.mk_variable_term "x"))
    end

val term9 =
    let val id   = T.mk_lambda_term "x" (T.mk_variable_term "x")
	val B    = T.mk_apply_term id (T.mk_variable_term "x")
	val arg  = T.mk_nuprl_small_integer_term 1
	val name = "term9: (x. (x. x) x) 1 -> 1"
    in (name, T.mk_apply_term (T.mk_lambda_term "x" B) arg)
    end

val term10 =
    let val idy  = T.mk_variable_term "y"
	val idr  = T.mk_variable_term "r"
	val sub  = T.mk_subtract_term idy (T.mk_nuprl_small_integer_term 1)
	val app  = T.mk_apply_term idr sub
	val add  = T.mk_add_term app (T.mk_nuprl_small_integer_term 3)
	val tst  = T.mk_nuprl_eq_int_term idy (T.mk_nuprl_small_integer_term 0)
	val ite  = T.mk_nuprl_ite_term tst (T.mk_nuprl_small_integer_term 0) add
	val lamy = T.mk_lambda_term "y" ite
	val lamr = T.mk_lambda_term "r" lamy
	val appY = T.mk_apply_term T.mk_nuprl_ycomb_term lamr
	val appA = T.mk_apply_term appY (T.mk_nuprl_small_integer_term 2)
	val cbva = T.mk_callbyvalueall_term appA ("x", T.mk_variable_term "x")
	val name = "term10: evallall (Y (\\r. \\y. if y = 0 then 0 else (r (y - 1)) + 3) 2)"
    in (name, cbva)
    end

val term11 =
    let val idy  = T.mk_variable_term "y"
	val idr  = T.mk_variable_term "r"
	val lamy = T.mk_lambda_term "y" idr
	val lamr = T.mk_lambda_term "r" lamy
	val app  = T.mk_apply_term lamr (T.mk_nuprl_small_integer_term 1)
	val cbva = T.mk_callbyvalueall_term app ("x", T.mk_variable_term "x")
	val name = "term11: evallall ((lam r. lam y. r) 1)"
    in (name, cbva)
    end

val term12 =
    let val idy  = T.mk_variable_term "y"
	val idr  = T.mk_variable_term "r"
	val lamy = T.mk_lambda_term "y" idr
	val lamr = T.mk_lambda_term "r" lamy
	val app  = T.mk_apply_term lamr (T.mk_nuprl_small_integer_term 1)
	val inj  = T.mk_inl_term app
	val cbva = T.mk_callbyvalueall_term inj ("x", T.mk_variable_term "x")
	val name = "term12: evallall (inl ((lam r. lam y. r) 1))"
    in (name, cbva)
    end

val term13 =
    let val idy  = T.mk_variable_term "y"
	val idr  = T.mk_variable_term "r"
	val lamy = T.mk_lambda_term "y" idr
	val lamr = T.mk_lambda_term "r" lamy
	val app  = T.mk_apply_term lamr (T.mk_nuprl_small_integer_term 1)
	val pair = T.mk_pair_term app (T.mk_nuprl_small_integer_term 0)
	val cbva = T.mk_callbyvalueall_term pair ("x", T.mk_variable_term "x")
	val name = "term13: evallall (((lam r. lam y. r) 1, 0))"
    in (name, cbva)
    end

val term14 =
    let val idr  = T.mk_variable_term "r"
	val idz  = T.mk_variable_term "z"
	val lamz = T.mk_lambda_term "z" idz
	val appz = T.mk_apply_term lamz (T.mk_nuprl_small_integer_term 0)
	val add  = T.mk_add_term idr appz
	val lamr = T.mk_lambda_term "r" add
	val app  = T.mk_apply_term lamr (T.mk_nuprl_small_integer_term 2)
	val inj  = T.mk_inl_term app
	val cbva = T.mk_callbyvalueall_term inj ("x", T.mk_variable_term "x")
	val name = "term14: evallall (inl ((\\ r. r + ((\\ z. z) 0)) 2))"
    in (name, cbva)
    end

val term15 =
    let val idr  = T.mk_variable_term "r"
	val idz  = T.mk_variable_term "z"
	val lamz = T.mk_lambda_term "z" idz
	val appz = T.mk_apply_term lamz (T.mk_nuprl_small_integer_term 0)
	val add  = T.mk_add_term idr appz
	val lamr = T.mk_lambda_term "r" add
	val app  = T.mk_apply_term lamr (T.mk_nuprl_small_integer_term 1)
	val pair = T.mk_pair_term app (T.mk_nuprl_small_integer_term 2)
	val cbva = T.mk_callbyvalueall_term pair ("x", T.mk_variable_term "x")
	val name = "term15: evallall (pair ((\\ r. r + ((\\ z. z) 0)) 1, 2))"
    in (name, cbva)
    end

val term16 =
    let val idr  = T.mk_variable_term "r"
	val lamr = T.mk_lambda_term "r" idr
	val appr = T.mk_apply_term lamr (T.mk_nuprl_small_integer_term 0)
	val pair = T.mk_pair_term appr (T.mk_nuprl_small_integer_term 2)
	val cbva = T.mk_callbyvalueall_term pair ("x", T.mk_variable_term "x")
	val name = "term16: evallall (pair ((\\ r. r) 0, 2))"
    in (name, cbva)
    end

val term17 =
    let val idr  = T.mk_variable_term "r"
	val lamr = T.mk_lambda_term "r" (T.mk_nuprl_small_integer_term 1)
	val appr = T.mk_apply_term lamr (T.mk_nuprl_small_integer_term 0)
	val pair = T.mk_pair_term appr (T.mk_nuprl_small_integer_term 2)
	val cbva = T.mk_callbyvalueall_term pair ("x", T.mk_variable_term "x")
	val name = "term17: evallall (pair ((\\ r. 1) 0, 2))"
    in (name, cbva)
    end

val term18 =
    let val idy  = T.mk_variable_term "y"
	val idr  = T.mk_variable_term "r"
	val lamy = T.mk_lambda_term "y" idr
	val lamr = T.mk_lambda_term "r" lamy
	val app1 = T.mk_apply_term lamr (T.mk_nuprl_small_integer_term 1)
	val app0 = T.mk_apply_term lamr (T.mk_nuprl_small_integer_term 0)
	val pair = T.mk_pair_term app1 app0
	val cbva = T.mk_callbyvalueall_term pair ("x", T.mk_variable_term "x")
	val name = "term18: evallall (((\\ r. \\ y. r) 1, (\\ r. \\ y. r) 0))"
    in (name, cbva)
    end

val term19 =
    let val idy  = T.mk_variable_term "y"
	val idr  = T.mk_variable_term "r"
	val lamy = T.mk_lambda_term "y" idr
	val appy = T.mk_apply_term lamy (T.mk_nuprl_small_integer_term 2)
	val lamr = T.mk_lambda_term "r" appy
	val appr = T.mk_apply_term lamr (T.mk_nuprl_small_integer_term 2)
	val cbva = T.mk_callbyvalueall_term appr ("x", T.mk_variable_term "x")
	val name = "term19: evallall ((\\ r. (\\ y. r) 2) 2)"
    in (name, cbva)
    end

val term20 =
    let val idy  = T.mk_variable_term "y"
	val idr  = T.mk_variable_term "r"
	val idw  = T.mk_variable_term "w"
	val lamw = T.mk_lambda_term "w" idr
	val lamy = T.mk_lambda_term "y" lamw
	val appy = T.mk_apply_term lamy (T.mk_nuprl_small_integer_term 2)
	val lamr = T.mk_lambda_term "r" appy
	val appr = T.mk_apply_term lamr (T.mk_nuprl_small_integer_term 2)
	val cbva = T.mk_callbyvalueall_term appr ("x", T.mk_variable_term "x")
	val name = "term20: evallall ((\\ r. (\\ y. \\ w. r) 2) 2)"
    in (name, cbva)
    end

val term21 =
    let val idy  = T.mk_variable_term "y"
	val idr  = T.mk_variable_term "r"
	val idw  = T.mk_variable_term "w"
	val lamw = T.mk_lambda_term "w" (T.mk_add_term idr idy)
	val lamy = T.mk_lambda_term "y" lamw
	val appy = T.mk_apply_term lamy (T.mk_nuprl_small_integer_term 2)
	val lamr = T.mk_lambda_term "r" appy
	val appr = T.mk_apply_term lamr (T.mk_nuprl_small_integer_term 2)
	val cbva = T.mk_callbyvalueall_term appr ("x", T.mk_variable_term "x")
	val name = "term21: evallall ((\\ r. (\\ y. \\ w. r + y) 2) 2)"
    in (name, cbva)
    end

val term22 =
    let val idy  = T.mk_variable_term "y"
	val lamy = T.mk_lambda_term "y" idy
	val appy = T.mk_apply_term lamy (T.mk_nuprl_small_integer_term 2)
	val name = "term22: (\\ y. y) 2"
    in (name, appy)
    end

val term23 =
    let val t2   = T.mk_nuprl_small_integer_term 2
	val t3   = T.mk_nuprl_small_integer_term 3
	val term = T.mk_add_term t2 t3
	val name = "term23: 2 + 3"
    in (name, term)
    end

val a_bunch_of_terms =
    [term1,  term2,  term3,
     (*term4,*)  (*term5,*) (*term6,*)
     term7,  term8,  term9,  term10,
     term11, term12, term13, term14, term15,
     term16, term17, term18, term19, term20,
     term21, term22, term23]

val Evaluator1 = EV1.Evaluator1 (T.emlib ())

val eval1 = EV1.Evaluator1
val eval2 = EV2.Evaluator2
(*val evalR = EVR2.Evaluator2*)
val evalE = EVE2.Evaluator2
val eval3 = EV3.Evaluator3
val eval4 = EV4.Evaluator4

fun ev1 () = eval1       (!mapref)
fun ev2 () = eval2 false (!mapref)
fun ev3 () = eval3 false (!mapref)
fun ev4 () = eval4 false (!mapref)


fun test n =
    let val evals =
	    case n of
		0 => [(1, ev1 ())(*,
	              (2, Evaluator2.Evaluator2),
		      (3, Evaluator3.Evaluator3),
		      (4, Evaluator4.Evaluator4)*)]
	      | 1 => [(1, ev1 ())]
	      | 2 => [(2, ev2 ())]
	      (*| 3 => [(3, Evaluator3.Evaluator3)]
	      | 4 => [(4, Evaluator4.Evaluator4)]*)
	      | _ => (print "No such evaluator"; [])
	val _ = print "\n"
    in app (fn (name, t1) =>
	   let val _ = print ("\"" ^ name ^ "\"\n")
	       val _ = print (T.toStringTerm t1 ^ "\n")
	       val _ = print (T.ppTerm       t1 ^ "\n\n")
	       val _ =
		   app (fn (x, eval) =>
			   let val _ = print ("[Evaluator " ^ Int.toString x ^ "]\n")
			       val (t2, _) = eval ~1 t1
			       val _ = print ("---->  " ^ T.toStringTerm t2 ^ "\n")
			       val _ = print ("---->  " ^ T.ppTerm       t2 ^ "\n")
			   in ()
			   end)
		       evals
	       val _ = print ("\n\n")
	   in ()
	   end)
	   a_bunch_of_terms
    end

fun run_ev1 prt (SOME lib_file) steps term =
    let val light = false
	val split = false
	val terms = P.parse prt light [] lib_file split
	val map   = T.terms2map 4 terms
    in EV1.Evaluator1 map steps term
    end
  | run_ev1 prt NONE steps term =
    EV1.Evaluator1 (T.emlib ()) steps term

fun run_ev2b prt (SOME lib_file) steps term =
    let val light = false
	val split = false
	val terms = P.parse prt light [] lib_file split
	val map   = T.terms2map 5 terms
    in eval2 true map steps term
    end
  | run_ev2b prt NONE steps term =
    eval2 true (T.emlib ()) steps term

(* evaluator -- recursive descent *)
fun run_ev1_map steps term = EV1.Evaluator1 (!mapref) steps term

(* evaluator with closures *)
fun run_ev2_map steps term = eval2 false (!mapref) steps term

(* evaluator with closures
 * + closures instead of subst when unfolding *)
fun run_ev2b_map steps term = eval2 true (!mapref) steps term

fun run_ev2e_map steps term = evalE true (!mapref) steps term

(* evaluator with closures
 * + closures instead of subst when unfolding
 * + CBV *)
fun run_ev2d_map steps term = eval2 true (!mapref) steps term

(* evaluator with continuations *)
fun run_ev3_map steps term = EV3.Evaluator3 false (!mapref) steps term

(* evaluator with continuations
 * + closures instead of subst when unfolding *)
fun run_ev3b_map steps term = EV3.Evaluator3 true (!mapref) steps term

(* evaluator with continuations
 * + datatype for the different sub-eval functions *)
fun run_ev3c_map steps term = EV3.EvaluatorD3 false (!mapref) steps term

(* evaluator with continuations
 * + closures instead of subst when unfolding
 * + datatype for the different sub-eval functions *)
fun run_ev3d_map steps term = EV3.EvaluatorD3 true (!mapref) steps term

(* evaluator with defunctionalization *)
fun run_ev4_map steps term = EV4.Evaluator4 false (!mapref) steps term

(* evaluator with defunctionalization
 * + closures instead of subst when unfolding *)
fun run_ev4b_map steps term = EV4.Evaluator4 true (!mapref) steps term

(* evaluator -- state machine *)
fun run_ev5_map steps term = EV5.Evaluator5 false (!mapref) steps term

(* evaluator -- state machine
 * + closures instead of subst when unfolding *)
fun run_ev5b_map steps term = EV5.Evaluator5 true (!mapref) steps term

fun start_session prt (SOME lib) =
    let val light = false
	val split = false
	val terms = P.parse prt light [] lib split
	val map   = T.terms2map 6 terms
	val _     = mapref := map
    in ()
    end
  | start_session prt NONE = ()

fun start_session_lib lib = mapref := lib

fun add_to_session terms =
    let val map =
	    foldl (fn (term, map) =>
		      if T.is_nuprl_iabstraction_term term
		      then let val (_, rlhs, rrhs) = T.dest_iabstraction term
			       val lhs  = T.rterm2term rlhs
			       val sign = T.getSignature lhs (*([], [])*)
			       val id   = T.opid_of_term lhs
			       val obid = ""
			       val opid = id
			       (*val _    = print ("[adding to library: " ^ id ^ "]\n")*)
			       val item = T.mk_item id sign obid rlhs rrhs []
			   in T.insert_abs map opid item
			   end
		      else map)
		  (!mapref)
		  terms
    in mapref := map
    end

fun get_lib () = !mapref

fun reset_lib () = mapref := T.emlib ()

val end_session = reset_lib

end
