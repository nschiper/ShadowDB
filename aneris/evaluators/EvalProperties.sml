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
 *  o Date:        29 July 2011
 *  o File name:   EvalProperties.sml
 *  o Description: Some properties of terms.
 *)


structure EvalProperties :> EVAL_PROPERTIES = struct

structure T = NuprlTerms

structure SET = BinarySetFn(type ord_key = string val compare = String.compare)

val all_set = SET.addList (SET.empty, ["callbyvalueall","eq_term"])

val set1 =
    SET.addList (SET.empty, ["limited_type_case","minus","isinr","isinl",
			     "ispair","isint","islambda","isatom2","isaxiom",
			     "spread","decide","apply",
			     "callbyvalue","callbyvalueall","ind","!wait"])
val set2 =
    SET.addList (SET.empty, ["add","subtract","multiply","divide","remainder",
			     "less","int_eq","atom_eq","eq_term"])
val set3 =
    SET.addList (SET.empty, ["!abstraction"])

val set_all1 =
    SET.addList (SET.empty, ["inl","inr","function","product",
			     "isect","tunion","set","quotient"])
val set_all2 =
    SET.addList (SET.empty, ["pair","union","subtype_rel"])

val set_all3 =
    SET.addList (SET.empty, ["equal"])

fun is_eval_all id = SET.member (all_set, id)

fun num_principals id =
    if SET.member (set3, id)
    then 3
    else if SET.member (set2, id)
    then 2
    else if SET.member (set1, id)
    then 1
    else 0

fun num_principal_all id =
    if SET.member (set_all3, id)
    then 3
    else if SET.member (set_all2, id)
    then 2
    else if SET.member (set_all1, id)
    then 1
    else 0

val dummy_opid = "dummy"
val dummy_term = T.mk_nuprl_simple_term dummy_opid []

val abs_ref = ref ("", dummy_term, dummy_term)

val tof_ref = ref dummy_term

fun search_in_nuprl (abs : T.abs_lib) opid term =
    let val {id, sign, obid, lhs, rhs, wfs} = T.find_sign abs term
    in abs_ref := (obid, T.rterm2term lhs, T.rterm2term rhs); true
    end handle _ => false

fun search_in_tof (tof : T.tof_lib) obid =
    case T.MAP.find (!tof, obid) of
	SOME rhs => (tof_ref := T.get rhs; true)
      | NONE => false

fun is_abstraction_term {abs, tof} term =
    let val opid = T.opid_of_term term
	val b = search_in_nuprl abs opid term
	(*val _ = print (opid ^ " " ^ Bool.toString b ^ "\n")*)
    in b
    end

fun is_termof_term {abs, tof} term =
    if T.is_nuprl_term "TERMOF" term
    then let val params = T.parameters_of_term term
	     val obidop = T.get_obid_parameters params
	 in case obidop of
		SOME obid => search_in_tof tof obid
	      | NONE => false
	 end
    else false

fun unfold_abs term =
    let val (obid, lhs, rhs) = !abs_ref
    in T.unfold_ab term lhs rhs
    end

fun ct_unfold_abs clos term =
    let val (obid, lhs, rhs) = !abs_ref
    in T.ct_unfold_ab clos term lhs rhs
    end

fun unfold_tof term = !tof_ref

end
