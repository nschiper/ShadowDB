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
 *  o File name:   Prelude.sml
 *  o Description: Small built-in EventML library
 *)


structure Prelude :> PRELUDE = struct

structure A = Ast
structure E = Env
structure D = Deps


(* ------ DATATYPE CONSTRUCTORS ------ *)

val env_true  = E.BINDVID (A.id_bool_true,  D.dummy_label, E.mk_new_scheme [] (E.mk_type_bool D.dummy_label))
val env_false = E.BINDVID (A.id_bool_false, D.dummy_label, E.mk_new_scheme [] (E.mk_type_bool D.dummy_label))
val env_nil   =
    let val tv  = E.nextItyvar ()
	val itv = E.mk_tyvar tv
	val ity = E.mk_type_list itv D.dummy_label
    in E.BINDVID (A.id_list_nil, D.dummy_label, E.mk_new_scheme [tv] ity)
    end
val env_inl   =
    let val tv1  = E.nextItyvar ()
	val tv2  = E.nextItyvar ()
	val ity1 = E.mk_type_disju (E.mk_tyvar tv1, E.mk_tyvar tv2) D.dummy_label
	val ity2 = E.mk_type_arrow (E.mk_tyvar tv1, ity1) D.dummy_label
    in E.BINDVID (A.id_disju_inl, D.dummy_label, E.mk_new_scheme [tv1, tv2] ity2)
    end
val env_inr   =
    let val tv1  = E.nextItyvar ()
	val tv2  = E.nextItyvar ()
	val ity1 = E.mk_type_disju (E.mk_tyvar tv1, E.mk_tyvar tv2) D.dummy_label
	val ity2 = E.mk_type_arrow (E.mk_tyvar tv2, ity1) D.dummy_label
    in E.BINDVID (A.id_disju_inr, D.dummy_label, E.mk_new_scheme [tv1, tv2] ity2)
    end
val env_fst   =
    let val tv1  = E.nextItyvar ()
	val tv2  = E.nextItyvar ()
	val ity1 = E.mk_type_tuple [E.mk_tyvar tv1, E.mk_tyvar tv2] D.dummy_label
	val ity2 = E.mk_type_arrow (ity1, E.mk_tyvar tv1) D.dummy_label
    in E.BINDVID (A.id_pair_fst, D.dummy_label, E.mk_new_scheme [tv1, tv2] ity2)
    end
val env_snd   =
    let val tv1  = E.nextItyvar ()
	val tv2  = E.nextItyvar ()
	val ity1 = E.mk_type_tuple [E.mk_tyvar tv1, E.mk_tyvar tv2] D.dummy_label
	val ity2 = E.mk_type_arrow (ity1, E.mk_tyvar tv2) D.dummy_label
    in E.BINDVID (A.id_pair_snd, D.dummy_label, E.mk_new_scheme [tv1, tv2] ity2)
    end

val init_constants = [env_nil,
		      env_true, env_false,
		      env_inl,  env_inr,
		      env_fst,  env_snd]


(* ------ TYPE CONSTRUCTORS ------ *)

fun mk_bindtyc0 name eq = E.BINDTYC (name, D.dummy_label, E.mk_typefun_gen 0 D.dummy_label name eq)
fun mk_bindtyc1 name eq = E.BINDTYC (name, D.dummy_label, E.mk_typefun_gen 1 D.dummy_label name eq)

val env_int    = mk_bindtyc0 E.tyconnameInt   E.eqtyconInt
val env_bool   = mk_bindtyc0 E.tyconnameBool  E.eqtyconBool
val env_real   = mk_bindtyc0 E.tyconnameReal  E.eqtyconReal
val env_atom   = mk_bindtyc0 E.tyconnameAtom  E.eqtyconAtom
val env_token  = mk_bindtyc0 E.tyconnameToken E.eqtyconToken
val env_top    = mk_bindtyc0 E.tyconnameTop   E.eqtyconTop
val env_type   = mk_bindtyc0 E.tyconnameType  E.eqtyconType
val env_prop   = mk_bindtyc0 E.tyconnameProp  E.eqtyconProp
val env_event  = mk_bindtyc0 E.tyconnameEvent E.eqtyconEvent
val env_unit   = mk_bindtyc0 E.tyconnameUnit  E.eqtyconUnit
val env_nat    = mk_bindtyc0 E.tyconnameNat   E.eqtyconNat
val env_msg    = mk_bindtyc0 E.tyconnameMsg   E.eqtyconMsg
val env_loc    = mk_bindtyc0 E.tyconnameLoc   E.eqtyconLoc
val env_instr  = mk_bindtyc0 E.tyconnameInstr E.eqtyconInstr

val env_list   = mk_bindtyc1 E.tyconnameList  E.eqtyconList
val env_class  = mk_bindtyc1 E.tyconnameClass E.eqtyconClass
val env_bag    = mk_bindtyc1 E.tyconnameBag   E.eqtyconBag
val env_deq    = mk_bindtyc1 E.tyconnameDeq   E.eqtyconDeq

val init_tycons = [env_int,  env_bool,  env_real,  env_atom, env_token,
		   env_top,
		   env_type, env_prop,  env_event,
		   env_unit, env_msg,   env_nat,
		   env_list, env_class, env_bag,   env_loc,  env_deq,
		   env_instr]


(* ------ OPERATORS ------ *)

fun env_op_int operator =
    let val iity = E.mk_type_int D.dummy_label
	val arr1 = E.mk_type_arrow (iity, iity) D.dummy_label
	val arr2 = E.mk_type_arrow (iity, arr1) D.dummy_label
    in E.BINDVID (operator, D.dummy_label, E.mk_new_scheme [] arr2)
    end

(*
fun env_op_eqdeq operator =
    let val tv    = E.nextItyvar ()
	val eqity = E.mk_eqtyvar tv D.dummy_label
	val deq   = E.mk_type_deq eqity D.dummy_label
    in E.BINDVID (operator, D.dummy_label, E.mk_new_scheme [tv] deq)
    end
*)

fun env_op_eqdeq operator =
    let val tv    = E.nextItyvar ()
	val eqity = E.mk_eqtyvar tv D.dummy_label
	val bool = E.mk_type_bool D.dummy_label
	val arr0 = E.mk_type_arrow (eqity, bool) D.dummy_label
	val arr  = E.mk_type_arrow (eqity, arr0) D.dummy_label
    in E.BINDVID (operator, D.dummy_label, E.mk_new_scheme [tv] arr)
    end

fun env_op_eqcomp operator =
    let val tv    = E.nextItyvar ()
	val eqity = E.mk_eqtyvar tv D.dummy_label
	val bity  = E.mk_type_bool D.dummy_label
	val arr1  = E.mk_type_arrow (eqity, bity) D.dummy_label
	val arr2  = E.mk_type_arrow (eqity, arr1) D.dummy_label
    in E.BINDVID (operator, D.dummy_label, E.mk_new_scheme [tv] arr2)
    end

fun env_op_intcomp operator =
    let val iity = E.mk_type_int  D.dummy_label
	val bity = E.mk_type_bool D.dummy_label
	val arr1 = E.mk_type_arrow (iity, bity) D.dummy_label
	val arr2 = E.mk_type_arrow (iity, arr1) D.dummy_label
    in E.BINDVID (operator, D.dummy_label, E.mk_new_scheme [] arr2)
    end

fun env_op_conslist operator =
    let val tv   = E.nextItyvar ()
	val itv  = E.mk_tyvar tv
	val lity = E.mk_type_list itv D.dummy_label
	val arr1 = E.mk_type_arrow (lity, lity) D.dummy_label
	val arr2 = E.mk_type_arrow (itv, arr1) D.dummy_label
    in E.BINDVID (operator, D.dummy_label, E.mk_new_scheme [tv] arr2)
    end

fun env_op_concatlist operator =
    let val tv   = E.nextItyvar ()
	val itv  = E.mk_tyvar tv
	val lity = E.mk_type_list itv D.dummy_label
	val arr1 = E.mk_type_arrow (lity, lity) D.dummy_label
	val arr2 = E.mk_type_arrow (lity, arr1) D.dummy_label
    in E.BINDVID (operator, D.dummy_label, E.mk_new_scheme [tv] arr2)
    end

fun env_op_unary_bool id =
    let val ity = E.mk_type_bool D.dummy_label
	val arr = E.mk_type_arrow (ity, ity) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [] arr)
    end

fun env_op_unary_prop id =
    let val ity = E.mk_type_prop D.dummy_label
	val arr = E.mk_type_arrow (ity, ity) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [] arr)
    end

fun env_op_disju_bool id =
    let val tv1  = E.nextItyvar ()
	val tv2  = E.nextItyvar ()
	val ity1 = E.mk_type_disju (E.mk_tyvar tv1, E.mk_tyvar tv2) D.dummy_label
	val ity2 = E.mk_type_arrow (ity1, E.mk_type_bool D.dummy_label) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [tv1, tv2] ity2)
    end

fun env_op_disju_left id =
    let val tv1  = E.nextItyvar ()
	val tv2  = E.nextItyvar ()
	val ity1 = E.mk_type_disju (E.mk_tyvar tv1, E.mk_tyvar tv2) D.dummy_label
	val ity2 = E.mk_type_arrow (ity1, E.mk_tyvar tv1) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [tv1, tv2] ity2)
    end

fun env_op_disju_right id =
    let val tv1  = E.nextItyvar ()
	val tv2  = E.nextItyvar ()
	val ity1 = E.mk_type_disju (E.mk_tyvar tv1, E.mk_tyvar tv2) D.dummy_label
	val ity2 = E.mk_type_arrow (ity1, E.mk_tyvar tv2) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [tv1, tv2] ity2)
    end

fun env_op_unary_deq id =
    let val tv   = E.nextItyvar ()
	val ity1 = E.mk_type_deq (E.mk_tyvar tv) D.dummy_label
	val bool = E.mk_type_bool D.dummy_label
	val arr0 = E.mk_type_arrow (E.mk_tyvar tv, bool) D.dummy_label
	val ity2 = E.mk_type_arrow (E.mk_tyvar tv, arr0) D.dummy_label
	val arr  = E.mk_type_arrow (ity1, ity2) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [tv] arr)
    end

fun env_op_binary_class id =
    let val tv   = E.nextItyvar ()
	val cls  = E.mk_type_class (E.mk_tyvar tv) D.dummy_label
	val arr1 = E.mk_type_arrow (cls, cls)  D.dummy_label
	val arr2 = E.mk_type_arrow (cls, arr1) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [tv] arr2)
    end

fun env_op_bind_class id =
    let val tv1  = E.nextItyvar ()
	val tv2  = E.nextItyvar ()
	val cls1 = E.mk_type_class (E.mk_tyvar tv1) D.dummy_label
	val cls2 = E.mk_type_class (E.mk_tyvar tv2) D.dummy_label
	val arr  = E.mk_type_arrow (E.mk_tyvar tv1, cls2) D.dummy_label
	val arr1 = E.mk_type_arrow (arr, cls2)  D.dummy_label
	val arr2 = E.mk_type_arrow (cls1, arr1) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [tv1, tv2] arr2)
    end

fun env_op_until_class id =
    let val tv1  = E.nextItyvar ()
	val tv2  = E.nextItyvar ()
	val cls1 = E.mk_type_class (E.mk_tyvar tv1) D.dummy_label
	val cls2 = E.mk_type_class (E.mk_tyvar tv2) D.dummy_label
	val arr1 = E.mk_type_arrow (cls2, cls1) D.dummy_label
	val arr2 = E.mk_type_arrow (cls1, arr1) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [tv1, tv2] arr2)
    end

fun env_op_class_at id =
    let val tv   = E.nextItyvar ()
	val cls  = E.mk_type_class (E.mk_tyvar tv) D.dummy_label
	val loc  = E.mk_type_loc D.dummy_label
	val bag  = E.mk_type_bag loc D.dummy_label
	val arr1 = E.mk_type_arrow (bag, cls)  D.dummy_label
	val arr2 = E.mk_type_arrow (cls, arr1) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [tv] arr2)
    end

fun env_op_class_opt id =
    let val tv   = E.nextItyvar ()
	val cls  = E.mk_type_class (E.mk_tyvar tv) D.dummy_label
	val bag  = E.mk_type_bag   (E.mk_tyvar tv) D.dummy_label
	val loc  = E.mk_type_loc D.dummy_label
	val fbag = E.mk_type_arrow (loc,  bag)  D.dummy_label
	val arr1 = E.mk_type_arrow (fbag, cls)  D.dummy_label
	val arr2 = E.mk_type_arrow (cls,  arr1) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [tv] arr2)
    end

fun env_op_class_opt_c id =
    let val tv   = E.nextItyvar ()
	val cls  = E.mk_type_class (E.mk_tyvar tv) D.dummy_label
	val arr1 = E.mk_type_arrow (cls, cls)  D.dummy_label
	val arr2 = E.mk_type_arrow (cls, arr1) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [tv] arr2)
    end

fun env_op_fun_fix id =
    let val tv1  = E.nextItyvar ()
	val tv2  = E.nextItyvar ()
	val arr1 = E.mk_type_arrow (E.mk_tyvar tv1, E.mk_tyvar tv2) D.dummy_label
	val arr2 = E.mk_type_arrow (arr1, E.mk_tyvar tv1) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [tv1, tv2] arr2)
    end

fun env_op_loc_un id =
    let val event = E.mk_type_event D.dummy_label
	val loc   = E.mk_type_loc   D.dummy_label
	val ity   = E.mk_type_arrow (event, loc) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [] ity)
    end

fun env_op_bin_events id =
    let val event = E.mk_type_event D.dummy_label
	val prop  = E.mk_type_prop  D.dummy_label
	val arr1  = E.mk_type_arrow (event, prop) D.dummy_label
	val arr2  = E.mk_type_arrow (event, arr1) D.dummy_label
    in E.BINDVID (id, D.dummy_label, E.mk_new_scheme [] arr2)
    end

val env_op_plus  = env_op_int          A.id_int_plus
val env_op_minus = env_op_int          A.id_int_minus
val env_op_mult  = env_op_int          A.id_int_mult
val env_op_div   = env_op_int          A.id_int_div
val env_op_eq    = env_op_eqdeq        A.id_eq
val env_op_eqeq  = env_op_eqcomp       A.id_eqeq
val env_op_diff  = env_op_eqcomp       A.id_diff
val env_op_leq   = env_op_intcomp      A.id_int_leq
val env_op_geq   = env_op_intcomp      A.id_int_geq
val env_op_lt    = env_op_intcomp      A.id_int_lt
val env_op_gt    = env_op_intcomp      A.id_int_gt
val env_op_lcons = env_op_conslist     A.id_list_cons
val env_op_lconc = env_op_concatlist   A.id_list_concat
val env_op_bnot  = env_op_unary_bool   A.id_bool_not
val env_op_not   = env_op_unary_prop   A.id_prop_not
val env_op_isl   = env_op_disju_bool   A.id_disju_isl
val env_op_eqof  = env_op_unary_deq    A.id_deq_eqof
val env_op_par   = env_op_binary_class A.id_class_par
val env_op_bind  = env_op_bind_class   A.id_class_bind
val env_op_until = env_op_until_class  A.id_class_until
val env_op_at    = env_op_class_at     A.id_class_at
val env_op_opt   = env_op_class_opt    A.id_class_opt
val env_op_opt_c = env_op_class_opt_c  A.id_class_opt_c
val env_op_fix   = env_op_fun_fix      A.id_fun_fix
val env_op_loc   = env_op_loc_un       A.id_es_loc
val env_op_causl = env_op_bin_events   A.id_es_causl
val env_op_locl  = env_op_bin_events   A.id_es_locl

val init_operators = [env_op_plus,  env_op_minus, env_op_mult, env_op_div,
		      env_op_eq,    env_op_eqeq,  env_op_diff,
		      env_op_leq,   env_op_geq,   env_op_lt,   env_op_gt,
		      env_op_lcons, env_op_lconc,
		      env_op_bnot,
		      env_op_not,
		      env_op_isl,
		      env_op_eqof,
		      env_op_par,   env_op_until, env_op_at, env_op_opt,
		      env_op_opt_c,
		      env_op_loc,   env_op_causl, env_op_locl]


(* ------ INITIAL ENVIRONMENT ------ *)

val init_env =
    let val inits = init_constants @ init_tycons @ init_operators
	val envs  = map (fn bind => E.mk_env_deppre bind D.dummy_label) inits
    in E.list2env envs
    end


end
