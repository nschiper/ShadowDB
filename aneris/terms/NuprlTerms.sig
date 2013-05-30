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
 *  o File name:   NuprlTerms.sig
 *  o Description: Datatype for Nuprl terms.
 *)


signature NUPRL_TERMS = sig

    structure MAP : ORD_MAP where type Key.ord_key = string

    type 'a toref

    type nuprl_nat

    type variable        = string
    type opid            = string
    type parameter_kind  = string
    type parameter_value = string

    type tag       = string ref
    type opid_tag  = opid * tag
    type parameter = parameter_value * parameter_kind
    type operator  = opid_tag * parameter list

    type nuprl_variable
    type nuprl_ref_term
    type env

    datatype nuprl_term       = TERM     of operator * nuprl_bound_term list
			      | AXM_TERM
			      | BOT_TERM
			      | INT_TERM
			      | VOI_TERM
			      | DUM_TERM
			      | ATM_TERM of int option
			      | TOK_TERM of parameter
			      | NAT_TERM of nuprl_nat
			      | VAR_TERM of nuprl_variable
			      | INL_TERM of nuprl_ref_term
			      | INR_TERM of nuprl_ref_term
			      | FIX_TERM of nuprl_ref_term
			      | MIN_TERM of nuprl_ref_term
			      | LAM_TERM of nuprl_variable * nuprl_ref_term
			      | REC_TERM of nuprl_variable * nuprl_ref_term
			      | WAI_TERM of nuprl_ref_term * nuprl_ref_term
			      | APP_TERM of nuprl_ref_term * nuprl_ref_term
			      | PAI_TERM of nuprl_ref_term * nuprl_ref_term
			      | ADD_TERM of nuprl_ref_term * nuprl_ref_term
			      | SUB_TERM of nuprl_ref_term * nuprl_ref_term
			      | MUL_TERM of nuprl_ref_term * nuprl_ref_term
			      | DIV_TERM of nuprl_ref_term * nuprl_ref_term
			      | REM_TERM of nuprl_ref_term * nuprl_ref_term
			      | EQT_TERM of nuprl_ref_term * nuprl_ref_term
			      | UNI_TERM of nuprl_ref_term * nuprl_ref_term
			      | EQU_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term
			      | IAX_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term
			      | IPA_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term
			      | IIR_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term
			      | IIL_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term
			      | IIN_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term
			      | ILA_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term
			      | IAT_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term
			      | CBV_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term
			      | CBA_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term
			      | FUN_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term
			      | PRD_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term
			      | TUN_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term
			      | SET_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term
			      | LES_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term * nuprl_ref_term
			      | IEQ_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term * nuprl_ref_term
			      | SPR_TERM of nuprl_ref_term * nuprl_variable * nuprl_variable * nuprl_ref_term
			      | AEQ_TERM of int * nuprl_ref_term * nuprl_ref_term * nuprl_ref_term * nuprl_ref_term
			      | DEC_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term * nuprl_variable * nuprl_ref_term
			      | IND_TERM of nuprl_ref_term * nuprl_variable * nuprl_variable * nuprl_ref_term * nuprl_ref_term * nuprl_variable * nuprl_variable * nuprl_ref_term
			      | CLO_TERM of nuprl_ref_term * env
	 and nuprl_bound_term = B_TERM   of nuprl_variable list * nuprl_ref_term

    (* The signature of a term:
     *   - the types of the parameters
     *   - the int for each subterms *)
    type sign = (parameter_value option * parameter_kind) list * int list

    type item     = {id   : string,
		     sign : sign,
		     obid : string,
		     lhs  : nuprl_ref_term,
		     rhs  : nuprl_ref_term,
		     wfs  : nuprl_ref_term list}
    type abs_lib  = item toref list MAP.map ref
    type tof_lib  = nuprl_term toref MAP.map ref
    type lib      = {abs : abs_lib, tof : tof_lib}

    val get : 'a toref -> 'a
    val mk  : 'a -> 'a toref

    val emlib      : unit -> lib
    val terms2map  : int -> nuprl_term list -> lib
    val terms2map' : int -> nuprl_term list -> lib
    val union_libs : lib -> lib -> lib
    val find_sign  : abs_lib -> nuprl_term -> item
    val insert_abs : lib -> string -> item -> lib
    val mk_item    : string
		     -> sign
		     -> string
		     -> nuprl_ref_term
		     -> nuprl_ref_term
		     -> nuprl_ref_term list
		     -> item
    val print_lib_stats   : lib -> unit
    val is_in_lib         : lib -> string -> bool
    val is_in_lib_tof_sub : lib -> string -> bool

    val stats_term_to_string : nuprl_term -> string

    val test_full_coverage : nuprl_term -> unit

    val mk_tag          : string -> tag
    val get_tag         : tag -> string
    val default_dtag    : string
    val dtag            : tag
    val reset_dtag      : unit -> unit
    val set_dummy_dtag  : unit -> unit

    val nuprl2eml_wf  : string -> nuprl_term -> string
    val nuprl2eml_abs : string -> nuprl_term -> string

    val nuprlTerm2eml       : nuprl_term     -> string
    val nuprl2haskell       : nuprl_term     -> string
    val nuprl2sml           : nuprl_term     -> string
    val tagless             : nuprl_term     -> string
    val toStringTerm        : nuprl_term     -> string
    val spToStringTerm      : nuprl_term     -> string
    val toStringRTerm       : nuprl_ref_term -> string
    val ppTerm              : nuprl_term     -> string
    val toStringEnv         : env            -> string
    val toStringTermStream  : nuprl_term     -> string -> unit

    val nuprl2lisp  : lib -> bool -> nuprl_term -> string
    val nuprl2lisp2 : lib -> bool -> nuprl_term -> string

    val opid_of_term  : nuprl_term -> opid
    val size          : nuprl_term -> int
    val env_size      : nuprl_term -> int
    val env_depth     : nuprl_term -> int
    val opr_of_term   : nuprl_term -> (opid * parameter list)
    val large_size    : nuprl_term -> IntInf.int

    val size_env      : env -> int

    val dest_nuprl_var    : nuprl_variable -> variable
    val mk_new_nuprl_var  : variable -> nuprl_variable
    val is_null_nuprl_var : nuprl_variable -> bool

    val mk_rterm      : nuprl_term -> nuprl_ref_term

    val rterm2term    : nuprl_ref_term -> nuprl_term

    val mk_ct         : (nuprl_ref_term * env) -> nuprl_term
    val mk_rct        : (nuprl_term     * env) -> nuprl_term

    val is_ct         : nuprl_term -> bool
    val dest_ct       : nuprl_term -> (nuprl_term * env)

    val pull_out_envs : (nuprl_term * env) -> (nuprl_term * env)

    val em_env            : env

    val lookup            : env -> variable -> (nuprl_term * env) option
    val lookup_clos       : env -> variable -> (variable * bool * (nuprl_term * bool) ref) option

    val add2env           : env -> (nuprl_variable * bool * nuprl_ref_term * env) list -> env
    val new_evall_add2env : env -> nuprl_ref_term -> env -> env * nuprl_variable

    val filter_env        : nuprl_term -> env -> env

    val close             : nuprl_term -> env -> nuprl_term

    val compute_to_inj    : nuprl_term -> nuprl_term

    val equal_nuprl_nats  : nuprl_nat -> nuprl_nat -> bool

    val mk_nuprl_obid_parameter        : parameter_value -> parameter
    val mk_nuprl_string_parameter      : parameter_value -> parameter
    val mk_nuprl_level_exp_parameter   : parameter_value -> parameter

    val mk_nuprl_ibool_term : bool -> nuprl_term

    val mk_nuprl_int_from_string  : string -> nuprl_term
    val mk_nuprl_real_from_string : string -> nuprl_term

    val mk_nuprl_small_integer_term         : int -> nuprl_term
    val mk_nuprl_small_natural_number_term  : int -> nuprl_term

    val mk_nuprl_ihost_term : string -> nuprl_term
    val mk_nuprl_itext_term : string -> nuprl_term

    val mk_nuprl_iport_term : int -> nuprl_term

    val mk_term           : opid -> tag -> parameter list -> nuprl_bound_term list -> nuprl_term
    val make_term         : opid -> tag -> parameter list -> nuprl_bound_term list -> nuprl_term
    val mk_nuprl_term     : (opid * parameter list) -> (nuprl_variable list * nuprl_term)     list -> nuprl_term
    val mk_nuprl_ref_term : (opid * parameter list) -> (nuprl_variable list * nuprl_ref_term) list -> nuprl_term

    val mk_set_cbva_term : nuprl_term -> nuprl_term

    val set_all_nuprl_var : nuprl_term -> nuprl_term

    val TERM2term : nuprl_term -> nuprl_term
    (*val term2TERM : nuprl_term -> nuprl_term*)

    val mk_nuprl_simple_term     : opid -> nuprl_term list     -> nuprl_term
    val mk_nuprl_ref_simple_term : opid -> nuprl_ref_term list -> nuprl_term

    val mk_variable_term       : parameter_value -> nuprl_term
    val mk_lambda_term         : variable -> nuprl_term -> nuprl_term
    val mk_rec_term            : variable -> nuprl_term -> nuprl_term
    val mk_wait_term           : nuprl_term -> nuprl_term -> nuprl_term
    val mk_apply_term          : nuprl_term -> nuprl_term -> nuprl_term
    val mk_decide_term         : nuprl_term -> (variable * nuprl_term) -> (variable * nuprl_term) -> nuprl_term
    val mk_inl_term            : nuprl_term -> nuprl_term
    val mk_inr_term            : nuprl_term -> nuprl_term
    val mk_minus_term          : nuprl_term -> nuprl_term
    val mk_spread_term         : nuprl_term -> (variable * variable * nuprl_term) -> nuprl_term
    val mk_pair_term           : nuprl_term -> nuprl_term -> nuprl_term
    val mk_axiom_term          : nuprl_term
    val mk_atom_term           : int option -> nuprl_term
    val mk_int_term            : nuprl_term
    val mk_ind_term            : nuprl_term -> (variable * variable * nuprl_term) -> nuprl_term -> (variable * variable * nuprl_term) -> nuprl_term
    val mk_ind_nterm           : nuprl_term -> (nuprl_variable * nuprl_variable * nuprl_term) -> nuprl_term -> (nuprl_variable * nuprl_variable * nuprl_term) -> nuprl_term
    val mk_divide_term         : nuprl_term -> nuprl_term -> nuprl_term
    val mk_multiply_term       : nuprl_term -> nuprl_term -> nuprl_term
    val mk_subtract_term       : nuprl_term -> nuprl_term -> nuprl_term
    val mk_add_term            : nuprl_term -> nuprl_term -> nuprl_term
    val mk_callbyvalue_term    : nuprl_term -> (variable * nuprl_term) -> nuprl_term
    val mk_callbyvalueall_term : nuprl_term -> (variable * nuprl_term) -> nuprl_term
    val mk_equal_term          : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_mkid_term           : string -> nuprl_term
    val mk_regular_token_term  : string -> nuprl_term

    val mk_prod_term : nuprl_term -> nuprl_term -> nuprl_term
    val mk_fun_term  : nuprl_term -> nuprl_term -> nuprl_term

    val mk_set_term  : nuprl_term -> (variable * nuprl_term) -> nuprl_term

    val mk_ind_ref_term   : nuprl_ref_term -> (variable * variable * nuprl_ref_term) -> nuprl_ref_term -> (variable * variable * nuprl_ref_term) -> nuprl_term
    val mk_ind_ref_nterm  : nuprl_ref_term -> (nuprl_variable * nuprl_variable * nuprl_ref_term) -> nuprl_ref_term -> (nuprl_variable * nuprl_variable * nuprl_ref_term) -> nuprl_term
    val mk_apply_ref_term : nuprl_ref_term -> nuprl_ref_term -> nuprl_term
    val mk_fix_ref_term   : nuprl_ref_term -> nuprl_term

    val mk_nuprl_icomment_term : string -> nuprl_term -> nuprl_term

    val mk_nuprl_type_term            : parameter_value -> nuprl_term
    val mk_nuprl_istring_term         : parameter_value -> nuprl_term
    val mk_nuprl_valuealltype_term    : parameter_value -> nuprl_term
    val mk_nuprl_prop_term            : parameter_value -> nuprl_term

    val mk_nuprl_event_orderingp_term : parameter_value -> nuprl_term -> nuprl_term
    val mk_nuprl_itheorem_term        : parameter_value -> nuprl_term -> nuprl_term
    val mk_nuprl_iupdate_term         : parameter_value -> nuprl_term -> nuprl_term
    val mk_nuprl_iproperty_term       : parameter_value -> nuprl_term -> nuprl_term
    val mk_nuprl_set_sig_term         : parameter_value -> nuprl_term -> nuprl_term

    val mk_nuprl_map_sig_term         : parameter_value -> nuprl_term -> nuprl_term -> nuprl_term

    val mk_nuprl_iinsert_object_term   : nuprl_term -> nuprl_term

    val mk_nuprl_iinsert_object_p_term : parameter_value -> nuprl_term -> nuprl_term

    val mk_nuprl_ioid_term : nuprl_term

    val mk_nuprl_iwf_lemmas_term : nuprl_term list -> nuprl_term
    val mk_nuprl_ilist_term      : nuprl_term list -> nuprl_term

    (* 0 argument *)
    val mk_nuprl_loc_term                : nuprl_term
    val mk_nuprl_name_term               : nuprl_term
    val mk_nuprl_top_term                : nuprl_term
    val mk_nuprl_real_term               : nuprl_term
    val mk_nuprl_unit_term               : nuprl_term
    val mk_nuprl_bool_term               : nuprl_term
    val mk_nuprl_loc_deq_term            : nuprl_term
    val mk_nuprl_bool_deq_term           : nuprl_term
    val mk_nuprl_atom_deq_term           : nuprl_term
    val mk_nuprl_nat_deq_term            : nuprl_term
    val mk_nuprl_unit_deq_term           : nuprl_term
    val mk_nuprl_int_deq_term            : nuprl_term
    val mk_nuprl_icons_nil_term          : nuprl_term
    val mk_nuprl_itext_nil_term          : nuprl_term
    val mk_nuprl_it_term                 : nuprl_term
    val mk_nuprl_nat_term                : nuprl_term
    val mk_nuprl_bfalse_term             : nuprl_term
    val mk_nuprl_btrue_term              : nuprl_term
    val mk_nuprl_empty_bag_term          : nuprl_term
    val mk_nuprl_ycomb_term              : nuprl_term
    val mk_nuprl_inewline_term           : nuprl_term
    val mk_nuprl_nil_term                : nuprl_term
    val mk_nuprl_null_class_program_term : nuprl_term
    val mk_nuprl_null_class_term         : nuprl_term

    (* 1 argument *)
    val mk_nuprl_list_term                         : nuprl_term -> nuprl_term
    val mk_nuprl_bag_term                          : nuprl_term -> nuprl_term
    val mk_nuprl_deq_term                          : nuprl_term -> nuprl_term
    val mk_nuprl_list_deq_term                     : nuprl_term -> nuprl_term
    val mk_nuprl_lifting0_term                     : nuprl_term -> nuprl_term
    val mk_nuprl_lifting1_term                     : nuprl_term -> nuprl_term
    val mk_nuprl_lifting2_term                     : nuprl_term -> nuprl_term
    val mk_nuprl_lifting3_term                     : nuprl_term -> nuprl_term
    val mk_nuprl_lifting4_term                     : nuprl_term -> nuprl_term
    val mk_nuprl_lifting_loc0_term                 : nuprl_term -> nuprl_term
    val mk_nuprl_lifting_loc1_term                 : nuprl_term -> nuprl_term
    val mk_nuprl_lifting_loc2_term                 : nuprl_term -> nuprl_term
    val mk_nuprl_lifting_loc3_term                 : nuprl_term -> nuprl_term
    val mk_nuprl_lifting_loc4_term                 : nuprl_term -> nuprl_term
    val mk_nuprl_concat_lifting0_term              : nuprl_term -> nuprl_term
    val mk_nuprl_concat_lifting1_term              : nuprl_term -> nuprl_term
    val mk_nuprl_concat_lifting2_term              : nuprl_term -> nuprl_term
    val mk_nuprl_concat_lifting3_term              : nuprl_term -> nuprl_term
    val mk_nuprl_concat_lifting4_term              : nuprl_term -> nuprl_term
    val mk_nuprl_concat_lifting_loc0_term          : nuprl_term -> nuprl_term
    val mk_nuprl_concat_lifting_loc1_term          : nuprl_term -> nuprl_term
    val mk_nuprl_concat_lifting_loc2_term          : nuprl_term -> nuprl_term
    val mk_nuprl_concat_lifting_loc3_term          : nuprl_term -> nuprl_term
    val mk_nuprl_concat_lifting_loc4_term          : nuprl_term -> nuprl_term
    val mk_nuprl_bag_null_term                     : nuprl_term -> nuprl_term
    val mk_nuprl_bag_only_term                     : nuprl_term -> nuprl_term
    val mk_nuprl_bag_size_term                     : nuprl_term -> nuprl_term
    val mk_nuprl_once_class_term                   : nuprl_term -> nuprl_term
    val mk_nuprl_bnot_term                         : nuprl_term -> nuprl_term
    val mk_nuprl_not_term                          : nuprl_term -> nuprl_term
    val mk_nuprl_pi1_term                          : nuprl_term -> nuprl_term
    val mk_nuprl_pi2_term                          : nuprl_term -> nuprl_term
    val mk_nuprl_send_once_class_term              : nuprl_term -> nuprl_term
    val mk_nuprl_send_once_loc_class_term          : nuprl_term -> nuprl_term
    val mk_nuprl_on_loc_class_term                 : nuprl_term -> nuprl_term
    val mk_nuprl_but_first_class_term              : nuprl_term -> nuprl_term
    val mk_nuprl_skip_first_class_term             : nuprl_term -> nuprl_term
    val mk_nuprl_primed_class_term                 : nuprl_term -> nuprl_term
    val mk_nuprl_single_bag_term                   : nuprl_term -> nuprl_term
    val mk_nuprl_msg_header_term                   : nuprl_term -> nuprl_term
    val mk_nuprl_msg_type_term                     : nuprl_term -> nuprl_term
    val mk_nuprl_msg_body_term                     : nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined0_class_term          : nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined0_loc_class_term      : nuprl_term -> nuprl_term
    val mk_nuprl_combined0_class_term              : nuprl_term -> nuprl_term
    val mk_nuprl_combined0_loc_class_term          : nuprl_term -> nuprl_term
    val mk_nuprl_isl_term                          : nuprl_term -> nuprl_term
    val mk_nuprl_isr_term                          : nuprl_term -> nuprl_term
    val mk_nuprl_outl_term                         : nuprl_term -> nuprl_term
    val mk_nuprl_outr_term                         : nuprl_term -> nuprl_term
    val mk_nuprl_evalall_term                      : nuprl_term -> nuprl_term
    val mk_nuprl_df_program_meaning_term           : nuprl_term -> nuprl_term
    val mk_nuprl_esE_term                          : nuprl_term -> nuprl_term
    val mk_nuprl_assert_term                       : nuprl_term -> nuprl_term
    val mk_nuprl_eqof_term                         : nuprl_term -> nuprl_term
    val mk_nuprl_es_eq_term                        : nuprl_term -> nuprl_term
    val mk_nuprl_once_class_program_term           : nuprl_term -> nuprl_term
    val mk_nuprl_return_loc_bag_class_program_term : nuprl_term -> nuprl_term
    val mk_nuprl_return_loc_bag_class_term         : nuprl_term -> nuprl_term
    val mk_nuprl_on_loc_class_program_term         : nuprl_term -> nuprl_term
    val mk_nuprl_msg_term                          : nuprl_term -> nuprl_term
    val mk_nuprl_interface_term                    : nuprl_term -> nuprl_term
    val mk_nuprl_event_ordering_p_term             : nuprl_term -> nuprl_term
    val mk_nuprl_base_headers_msg_val_term         : nuprl_term -> nuprl_term
    val mk_nuprl_base_locs_headers_term            : nuprl_term -> nuprl_term
    val mk_nuprl_base_class_program_term           : nuprl_term -> nuprl_term
    val mk_nuprl_set_sig_set_term                  : nuprl_term -> nuprl_term
    val mk_nuprl_set_sig_member_term               : nuprl_term -> nuprl_term
    val mk_nuprl_set_sig_empty_term                : nuprl_term -> nuprl_term
    val mk_nuprl_set_sig_isEmpty_term              : nuprl_term -> nuprl_term
    val mk_nuprl_set_sig_singleton_term            : nuprl_term -> nuprl_term
    val mk_nuprl_set_sig_add_term                  : nuprl_term -> nuprl_term
    val mk_nuprl_set_sig_union_term                : nuprl_term -> nuprl_term
    val mk_nuprl_set_sig_remove_term               : nuprl_term -> nuprl_term
    val mk_nuprl_map_sig_map_term                  : nuprl_term -> nuprl_term
    val mk_nuprl_map_sig_eqKey_term                : nuprl_term -> nuprl_term
    val mk_nuprl_map_sig_find_term                 : nuprl_term -> nuprl_term
    val mk_nuprl_map_sig_inDom_term                : nuprl_term -> nuprl_term
    val mk_nuprl_map_sig_empty_term                : nuprl_term -> nuprl_term
    val mk_nuprl_map_sig_isEmpty_term              : nuprl_term -> nuprl_term
    val mk_nuprl_map_sig_update_term               : nuprl_term -> nuprl_term
    val mk_nuprl_map_sig_add_term                  : nuprl_term -> nuprl_term
    val mk_nuprl_map_sig_remove_term               : nuprl_term -> nuprl_term

    (* 2 arguments *)
    val mk_nuprl_eq_atom_term                      : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_eq_bool_term                      : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_eq_int_term                       : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_eq_id_term                        : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_eq_loc_term                       : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_union_term                        : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_bor_term                          : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_band_term                         : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_or_term                           : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_and_term                          : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_proddeq_term                      : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_sumdeq_term                       : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_combined_class_term               : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined_class_term           : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_combined_loc_class_term           : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined_loc_class_term       : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_icons_cons_term                   : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_itext_cons_term                   : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_member_term                       : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_primed_class_opt_term             : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_until_class_term                  : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_parallel_class_term               : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_messages_delivered_term           : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_msgs_interface_delivered_term     : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_lt_int_term                       : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_le_int_term                       : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_concat_term                       : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_name_eq_term                      : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_iabstraction_term                 : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_iff_term                          : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_uiff_term                         : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_implies_term                      : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_uimplies_term                     : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_select_term                       : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_general_base_class_term           : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_cons_bag_term                     : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_class_opt_term                    : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_class_opt_class_term              : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_lifting_gen_term                  : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_bag_map_term                      : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined1_class_term          : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_combined1_class_term              : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_combined1_loc_class_term          : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined0_class_opt_term      : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined1_loc_class_term      : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined0_loc_class_opt_term  : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_iinclude_properties_term          : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_lifting_loc_gen_term              : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_concat_lifting_loc_gen_term       : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_concat_lifting_gen_term           : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_class_at_term                     : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_msg_has_type_term                 : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_base_prc_term                     : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_once_prc_term                     : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_send_once_loc_prc_term            : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_on_loc_prc_term                   : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_but_first_prc_term                : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_skip_first_prc_term               : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_prior_prc_term                    : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_less_than_term                    : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_le_term                           : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_es_pred_term                      : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_bind_class_term               : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_class_at_program_term             : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_parallel_class_program_term       : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_bind_class_program_term           : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_until_class_program_term          : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_eclass0_term                      : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_eclass0_program_term              : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_eclass1_term                      : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_eclass1_program_term              : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_eclass2_term                      : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_eclass2_program_term              : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_eclass3_term                      : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_eclass3_program_term              : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_cons_term                         : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_mk_msg_interface_term             : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_class_term                        : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_make_msg_term                     : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_no_repeats_term                   : nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_record_select_term                : nuprl_term -> nuprl_term -> nuprl_term

    (* 3 arguments *)
    val mk_nuprl_eq_bag_term                       : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_ite_term                          : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_comb_term                     : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_std_ma_term                       : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_message_constraint_term           : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_msg_interface_constraint_term     : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined2_class_term          : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_combined2_class_term              : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_combined2_loc_class_term          : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined1_class_opt_term      : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined2_loc_class_term      : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined1_loc_class_opt_term  : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_base_headers_msg_val_loc_term     : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_base_at_prc_term                  : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_until_prc_term                    : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_at_prc_term                       : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_parallel_prc_term                 : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_prior_init_prc_term               : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_es_locl_term                      : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_es_le_term                        : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_single_valued_classrel_term       : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_es_eq_E_term                      : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_es_causl_term                     : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_es_functional_class_term          : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_classfun_term                     : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_state_class1_term                 : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_state_class1_program_term         : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory_class1_term                : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory_class1_program_term        : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_state1_term                       : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory1_term                      : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_make_msg_interface_term           : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_local_class_msg_term              : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_programmable_term                 : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_nlp_term                          : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term

    (* 4 arguments *)
    val mk_nuprl_union_deq_term                    : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_product_deq_term                  : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined3_class_term          : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_combined3_class_term              : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_combined3_loc_class_term          : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined2_class_opt_term      : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined3_loc_class_term      : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined2_loc_class_opt_term  : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_bind_prc_term                     : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_loc_comb1_prc_term                : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined_loc_class1_prc_term  : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_state1_prc_term                   : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory1_prc_term                  : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term

    (* 5 arguments *)
    val mk_nuprl_rec_combined3_class_opt_term      : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_combined3_loc_class_opt_term  : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_classrel_term                     : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_rec_bind_prc_term                 : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_state_class2_term                 : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_state_class2_program_term         : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory_class2_term                : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory_class2_program_term        : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_state2_term                       : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory2_term                      : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term

    (* 6 arguments *)
    val mk_nuprl_rec_comb_prc_term                 : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term

    (* 7 arguments *)
    val mk_nuprl_state_class3_term                 : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_state_class3_program_term         : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory_class3_term                : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory_class3_program_term        : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_state3_term                       : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory3_term                      : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term

    (* 8 arguments *)
    val mk_nuprl_state2_prc_term                   : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory2_prc_term                  : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term

    (* 9 arguments *)
    val mk_nuprl_state_class4_term                 : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_state_class4_program_term         : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory_class4_term                : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory_class4_program_term        : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_state4_term                       : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory4_term                      : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term

    (* 11 arguments *)
    val mk_nuprl_state3_prc_term                   : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory3_prc_term                  : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term

    (* 14 arguments *)
    val mk_nuprl_state4_prc_term                   : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term
    val mk_nuprl_memory4_prc_term                  : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term

    val mk_nuprl_list_ind_term : nuprl_term -> nuprl_term -> (variable * variable * variable * nuprl_term) -> nuprl_term

    val mk_nuprl_itext_list_term : nuprl_term list -> nuprl_term

    val mk_nuprl_spreadn_term : nuprl_term -> (variable list * nuprl_term) -> nuprl_term

    val mk_nuprl_bind_class_term     : nuprl_term -> (variable * nuprl_term) -> nuprl_term
    val mk_nuprl_all_term            : nuprl_term -> (variable * nuprl_term) -> nuprl_term
    val mk_nuprl_uall_term           : nuprl_term -> (variable * nuprl_term) -> nuprl_term
    val mk_nuprl_exists_term         : nuprl_term -> (variable * nuprl_term) -> nuprl_term
    val mk_nuprl_isect_term          : nuprl_term -> (variable * nuprl_term) -> nuprl_term
    val mk_nuprl_lall_term           : nuprl_term -> (variable * nuprl_term) -> nuprl_term

    val mk_nuprl_rec_ind_term : nuprl_term -> (nuprl_variable * nuprl_variable * nuprl_term) -> nuprl_term

    val mk_nuprl_rec_ind_ref_term : nuprl_term -> (nuprl_variable * nuprl_variable * nuprl_ref_term) -> nuprl_term

    val mk_nuprl_applies_term : nuprl_term -> nuprl_term list -> nuprl_term

    val mk_nuprl_let_term      : variable -> nuprl_term -> nuprl_term -> nuprl_term

    val mk_nuprl_genrec_term : variable * variable * nuprl_term -> nuprl_term

    val mk_nuprl_lambdas_term : variable list -> nuprl_term -> nuprl_term

    val mk_nuprl_finite_list_term : nuprl_term list -> nuprl_term

    val is_nuprl_term : opid -> nuprl_term -> bool

    val is_nuprl_variable_term              : nuprl_term -> bool
    val is_nuprl_lambda_term                : nuprl_term -> bool
    val is_nuprl_wait_term                  : nuprl_term -> bool
    val is_nuprl_type_term                  : nuprl_term -> bool
    val is_nuprl_integer_term               : nuprl_term -> bool
    val is_nuprl_minus_natural_number_term  : nuprl_term -> bool
    val is_nuprl_iwf_lemmas_term            : nuprl_term -> bool
    val is_nuprl_iwftheorem_term            : nuprl_term -> bool
    val is_nuprl_iabstraction_term          : nuprl_term -> bool
    val is_nuprl_iinclude_properties_term   : nuprl_term -> bool
    val is_nuprl_pair_term                  : nuprl_term -> bool
    val is_nuprl_axiom_term                 : nuprl_term -> bool
    val is_nuprl_select_term                : nuprl_term -> bool
    val is_nuprl_function_term              : nuprl_term -> bool
    val is_nuprl_product_term               : nuprl_term -> bool
    val is_nuprl_eclass_term                : nuprl_term -> bool
    val is_nuprl_bag_term                   : nuprl_term -> bool
    val is_nuprl_eqof_term                  : nuprl_term -> bool
    val is_nuprl_bool_term                  : nuprl_term -> bool
    val is_nuprl_apply_term                 : nuprl_term -> bool
    val is_nuprl_eq_int_term                : nuprl_term -> bool
    val is_nuprl_eq_id_term                 : nuprl_term -> bool
    val is_nuprl_inl_term                   : nuprl_term -> bool
    val is_nuprl_inr_term                   : nuprl_term -> bool

    val is_nuprl_event_orderingp_term       : parameter_value -> nuprl_term -> bool
    val is_nuprl_prop_term                  : parameter_value -> nuprl_term -> bool

    val dest_iport : nuprl_term -> int
    val dest_ihost : nuprl_term -> string

    val dest_id    : int -> nuprl_term -> string

    val full_dest_term    : nuprl_term -> (opid * parameter list) * (nuprl_variable list * nuprl_term) list
    val dest_term         : nuprl_term -> (opid * parameter list) * (nuprl_variable list * nuprl_ref_term) list
    val dest_simple_term  : nuprl_term -> opid * nuprl_ref_term list

    val dest_ref_lambda   : int -> nuprl_term -> nuprl_variable * nuprl_ref_term
    val dest_lambda       : int -> nuprl_term -> nuprl_variable * nuprl_term

    val dest_ref_spread   : int -> nuprl_term -> nuprl_ref_term * nuprl_variable * nuprl_variable * nuprl_ref_term
    val dest_spread       : int -> nuprl_term -> nuprl_term * nuprl_variable * nuprl_variable * nuprl_term

    val dest_let          : nuprl_term ->  nuprl_term * nuprl_variable * nuprl_term

    val dest_rec_comb     : nuprl_term -> nuprl_term * nuprl_term * nuprl_term

    val dest_iabstraction : nuprl_term -> nuprl_ref_term * nuprl_ref_term * nuprl_ref_term

    val dest_iinclude_properties : nuprl_term -> nuprl_ref_term * nuprl_ref_term

    val dest_variable      : nuprl_term -> variable

    val dest_small_integer : nuprl_term -> int

    val dest_lambdas       : int -> nuprl_term -> nuprl_variable list * nuprl_term

    val dest_alls_ualls    : nuprl_term -> (variable * nuprl_term * bool) list * nuprl_term

    val dest_primed_class : nuprl_term -> nuprl_term
    val dest_iwftheorem   : nuprl_term -> nuprl_term
    val dest_bag          : nuprl_term -> nuprl_term
    val dest_bnot         : nuprl_term -> nuprl_term
    val dest_eqof         : nuprl_term -> nuprl_term
    val dest_es_eq        : nuprl_term -> nuprl_term
    val dest_inl          : nuprl_term -> nuprl_term
    val dest_inr          : nuprl_term -> nuprl_term
    val dest_minus        : nuprl_term -> nuprl_term

    val dest_rinl         : nuprl_term -> nuprl_ref_term
    val dest_rinr         : nuprl_term -> nuprl_ref_term

    val dest_simple_product   : nuprl_term -> nuprl_term * nuprl_term
    val dest_simple_function  : nuprl_term -> nuprl_term * nuprl_term
    val dest_simple_functions : nuprl_term -> nuprl_term list * nuprl_term

    val dest_select       : nuprl_term -> nuprl_term * nuprl_term
    val dest_prior_prc    : nuprl_term -> nuprl_term * nuprl_term
    val dest_apply        : nuprl_term -> nuprl_term * nuprl_term
    val dest_member       : nuprl_term -> nuprl_term * nuprl_term
    val dest_lt_int       : nuprl_term -> nuprl_term * nuprl_term
    val dest_le_int       : nuprl_term -> nuprl_term * nuprl_term
    val dest_eq_int       : nuprl_term -> nuprl_term * nuprl_term
    val dest_eq_id        : nuprl_term -> nuprl_term * nuprl_term
    val dest_band         : nuprl_term -> nuprl_term * nuprl_term

    val dest_applies      : nuprl_term -> nuprl_term * nuprl_term list

    val dest_pair         : int -> nuprl_term -> nuprl_term * nuprl_term

    val dest_rpair        : int -> nuprl_term -> nuprl_ref_term * nuprl_ref_term

    val dest_list         : nuprl_term -> nuprl_term list

    val dest_simple_null_term : nuprl_term -> opid * nuprl_ref_term list

    val dest_eclass : nuprl_term -> nuprl_term * nuprl_variable * nuprl_variable * nuprl_term

    val fo_subst : (nuprl_variable * nuprl_term) list -> nuprl_term -> nuprl_term

    val replace_terms : (opid * nuprl_term) list -> nuprl_term -> nuprl_term

    val refresh_term : nuprl_term -> nuprl_term

    val unfold_ab : nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term

    val ct_unfold_ab : env option -> nuprl_term -> nuprl_term -> nuprl_term -> nuprl_term

    val unfold_all : lib -> nuprl_term -> nuprl_term

    val trim_lib_terms : lib -> nuprl_term list -> unit

    val partial_ev_opt : nuprl_term -> nuprl_term

    val toDeq : nuprl_term -> nuprl_term

    val subtermn : int -> nuprl_term -> nuprl_term

    val subterms : nuprl_term -> nuprl_ref_term list

    val subterms_n : int -> ('a * nuprl_ref_term) list -> nuprl_term

    val bterms_of_term  : nuprl_term -> (nuprl_variable list * nuprl_ref_term) list
    val brterms_of_term : nuprl_term -> (nuprl_variable list * nuprl_term)     list

    val type_of_parameter  : parameter -> parameter_kind
    val value_of_parameter : parameter -> parameter_value

    val parameters_of_term : nuprl_term -> parameter list

    val equal_parameters : parameter -> parameter -> bool

    val alpha_equal_terms : nuprl_term -> nuprl_term -> bool
    val alpha_eq_terms    : nuprl_term -> nuprl_term -> unit

    val firstnat : nuprl_term -> int

    val do_primitive_int_op   : opid -> nuprl_term -> nuprl_term -> nuprl_term
    val do_primitive_cmp      : opid -> nuprl_term -> nuprl_term -> bool
    val do_primitive_minus    : nuprl_term -> nuprl_term
    val do_primitive_wait     : nuprl_term -> nuprl_term -> nuprl_term
    val do_primitive_ref_wait : nuprl_term -> nuprl_ref_term -> nuprl_ref_term

    val compare_atomn : int -> nuprl_term -> nuprl_term -> bool

    val is_zero     : nuprl_term -> order

    val inc_integer : nuprl_term -> nuprl_term
    val dec_integer : nuprl_term -> nuprl_term

    val get_obid_parameters : parameter list -> parameter_value option

    val getSignature : nuprl_term -> sign
    val eq_signs : sign -> sign -> bool

    val to_filter_out : string list

end
