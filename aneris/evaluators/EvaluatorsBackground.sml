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
 *  o File name:   EvaluatorsBackground.sml
 *  o Description: .
 *)


structure EvaluatorsBackground = struct

structure EH = ErrorHandler
structure B  = Tools

type variable = string

type opid = string

type parameter_kind  = string (* such as: "token", "nat", "level-expression" *)
type parameter_value = string
type parameter = parameter_value * parameter_kind

type operator = opid * parameter list

datatype term       = TERM   of operator * bound_term list
     and bound_term = B_TERM of variable list * term

exception ERROR_FORMAT_TERM (* when a term does not have the adequat format *)
exception FAIL
exception FAIL_WITH of string
exception ERROR (* any other error *)
exception ERROR_LISP of string

val primitive_opid_parm_types_and_arities =
    [("axiom", [], []),
     ("natural_number", ["natural"], []),
     ("int",   [], []),
     ("token", ["token"], [])]

(**** TO STRING ****)

fun toStringParameter (value, kind) = "(" ^ value ^ "," ^ kind ^ ")"

fun toStringParameters [] = ""
  | toStringParameters [x] = toStringParameter x
  | toStringParameters (x :: xs) = toStringParameter x ^ "," ^ toStringParameters xs

fun toStringOperator (opid, parameters) =
    opid ^ "(" ^ toStringParameters parameters ^ ")"

fun toStringVars [] = ""
  | toStringVars [x] = x
  | toStringVars (x :: xs) = x ^ "," ^ toStringVars xs

fun toStringTerm (TERM (operator, bterms)) =
    toStringOperator operator ^ "{" ^ toStringBTerms bterms ^ "}"

and toStringBTerm (B_TERM (vars, term)) =
    "([" ^ toStringVars vars ^ "]," ^ toStringTerm term ^ ")"

and toStringBTerms [] = ""
  | toStringBTerms [x] = toStringBTerm x
  | toStringBTerms (x :: xs) = toStringBTerm x ^ "," ^ toStringBTerms xs

val toString = toStringTerm

(**** TODO ****)

(* NOTE: Defined in /usr/fdl/sys/library/fml
 * Its commented type seems to be wrong.
 * Is the definition just (param1 = param2)? *)
fun equal_parameters param1 param2 = raise EH.TODO

fun limited_type_case1 _ = raise EH.TODO
fun limited_type_case2 _ = raise EH.TODO
fun limited_type_case_no_redex1 _ = raise EH.TODO
fun limited_type_case_no_redex2 _ = raise EH.TODO
fun apply_conv  _ = raise EH.TODO (*?*)
fun TagC        _ = raise EH.TODO (*?*)
fun firstnat term = raise EH.TODO (*?*)
fun mapfilter _ = raise EH.TODO (* NOTE: What's the difference with map? *)
fun occurs_free _ = raise EH.TODO (* NOTE: Defined in /usr/fdl/sys/library/fml/mlt-defs.lsp *)
fun maybe_new_var _ = raise EH.TODO (*?*)

fun make_extended_parameter_value _ = raise EH.TODO
fun type_id_to_type _ = raise EH.TODO
fun message_emit _ = raise EH.TODO
fun warn_message _ = raise EH.TODO

fun instantiate_bound_term _ = raise EH.TODO
fun orb_eval_by_description _ = raise EH.TODO
fun iexpression_term _ = raise EH.TODO
fun listp _ = raise EH.TODO
fun map_list_to_ilist _ = raise EH.TODO
fun interpret_result _ = raise EH.TODO
fun lisp_op _ = raise EH.TODO

fun is_abstraction_term term = raise EH.TODO
    (*let val abs = abstraction_of_term term
    in lisp_and abs
		(lisp_and (abstraction_visible abs)
			  (expansion_of_abstraction abs))
    end*)


(**** USEFUL FUNCTIONS ****)

fun string_to_variable st = st

fun lisp_or  a b = a handle _ => b
fun lisp_and a b = (a; b)

(**** ACCESSORS ****)

fun opid_of_term (TERM ((opid, _), _)) = opid

val id_of_term = opid_of_term (* NOTE: Are they really the same? *)

fun bterms_of_term (TERM (_, bterms)) = map (fn (B_TERM x) => x) bterms

val bound_terms_of_term = bterms_of_term

fun subterms term = map (fn (_, term) => term) (bterms_of_term term)

fun two_subterms term =
    let val (TERM (_, [B_TERM (_, t1), B_TERM (_, t2)])) = term
    in (t1, t2)
    end handle _ => raise FAIL_WITH "two_subterms"

fun opid_of_operator (value, _) = value

fun type_id_of_parameter (_, kind) = kind
(* NOTE: Is that the correct definition? *)

(**** SIMPLE CHECKING FUNCTIONS ****)

fun is_term token term = opid_of_term term = token

val mk_primitive_op_instance_recognizer = is_term (* NOTE: Are they really the same? *)

(**** CONSTRUCTORS ****)

fun mk_variable_parm tok = (tok, "token")
fun mk_natural_parm tag = (Int.toString tag, "nat")
fun make_token_parameter token = (token, "token")
fun make_string_parameter string = (string, "string")

val make_natural_parameter = mk_natural_parm

val natural_parameter = mk_natural_parm

fun make_parameter (param : parameter) = param

fun mk_term operator b_terms = TERM (operator, map (fn x => B_TERM x) b_terms)

val make_term = mk_term

fun mk_simple_op_term operator terms =
    TERM (operator, map (fn x => B_TERM ([], x)) terms)

fun mk_simple_term opid term_list =
    TERM ((opid, []), map (fn term => B_TERM ([], term)) term_list)

fun mk_natural_number_term nat =
    TERM (("natural_number", [(Int.toString nat, "nat")]), [])

fun mk_minus_term term = mk_simple_term "minus" [term]

fun mk_integer_term int =
    if int < 0
    then mk_minus_term (mk_natural_number_term (~int))
    else mk_natural_number_term int

fun mk_apply_term function argument = mk_simple_term "apply" [function, argument]

fun mk_pair_term left right = mk_simple_term "pair" [left, right]

fun mk_variable_term tok = mk_term ("variable", [mk_variable_parm tok]) []

fun mk_lambda_term var term = mk_term ("lambda", []) [([var], term)]

fun mk_spread_term pair (v1, v2, bterm) =
    mk_term ("spread", []) [([], pair), ([v1, v2], bterm)]

val mk_it_term = mk_simple_term "it" []

fun mk_inl_term term = mk_simple_term "inl" [term]
fun mk_inr_term term = mk_simple_term "inr" [term]

fun mk_decide_term dec (var1, bterm1) (var2, bterm2) =
    mk_term ("decide", []) [([], dec), ([var1], bterm1), ([var2], bterm2)]

fun mk_ite_term test term1 term2 = mk_decide_term test ("", term1) ("", term2)

fun mk_sub_term term1 term2 = mk_simple_term "subtract" [term1, term2]
fun mk_add_term term1 term2 = mk_simple_term "add"      [term1, term2]

fun mk_int_eq_term term1 term2 term3 term4 = mk_simple_term "int_eq" [term1, term2, term3, term4]

fun mk_eq_int_term term1 term2 =
    mk_int_eq_term term1
		   term2
		   (mk_inl_term mk_it_term)
		   (mk_inr_term mk_it_term)

fun mk_rec_ind_term arg (f,x,B) = mk_term ("rec_ind",[]) [([],arg),([f,x],B)]

fun mk_list_ind_term L nilcase (x, xs, r, conscase) =
    mk_term ("list_ind", []) [([], L), ([], nilcase), ([x, xs, r], conscase)]

fun mk_cons_term x xs = mk_simple_term "cons" [x, xs]

val mk_nil_term = mk_simple_term "nil" []

fun mk_callbyvalue_term arg (x, B) =
    mk_term ("callbyvalue", []) [([], arg), ([x], B)]

fun mk_callbyvalueall_term arg (x, B) =
    mk_term ("callbyvalueall", []) [([], arg), ([x], B)]

fun mk_tag_term tag term = mk_term ("tag", [mk_natural_parm tag]) [([], term)]

fun itoken_term   t = make_term ("!token",   [make_token_parameter   t]) []
fun inatural_term t = make_term ("!natural", [make_natural_parameter t]) []
fun itext_term    s = make_term ("!text",    [make_string_parameter  s]) []

fun build_primitive_value term params =
    let val opid = opid_of_term term
    in if B.member opid ["inl", "inr", "pair", "cons"]
       then mk_simple_term opid params
       else term
    end

(**** DESTRUCTORS ****)

fun dest_simple_bound_term (B_TERM ([], term)) = term
  | dest_simple_bound_term _ = raise ERROR_FORMAT_TERM

fun dest_simple_bound_term_list b_term_list =
    map (fn b_term => dest_simple_bound_term b_term) b_term_list

fun dest_simple_term (TERM ((opid, []), bound_term_list)) =
    (opid, dest_simple_bound_term_list bound_term_list)
  | dest_simple_term _ = raise ERROR_FORMAT_TERM

fun dest_lambda term =
    if is_lambda_term term
    then let val [([x], B)] = bterms_of_term term
	 in (x, B)
	 end
    else raise FAIL_WITH "dest_lambda"

fun dest_primitive_value term =
    let val opid = opid_of_term term
    in if B.member opid ["pair", "cons"]
       then [subtermn 1 term, subtermn 2 term]
       else if B.member opid ["inl", "inr"]
       then [subtermn 1 term]
       else []
    end

fun dest_variable_parm param =
    if type_id_of_parameter param = "token" (* NOTE: Is that the correct thing to do? *)
    then value_of_parameter param
    else raise FAIL_WITH "dest_variable_parm"

fun dest_variable term =
    if is_variable_term term
    then dest_variable_parm (hd (parameters_of_term term))
    else raise FAIL_WITH "dest_var"

(**** OTHER CHECKING FUNCTIONS ****)

 (* NOTE: What does that do?
  * It should just check that the values are equal tokens *)

(**** OTHER FUNCTIONS ****)

fun instantiate_parameter value kind =
    (* NOTE: the original code has a condition on value:
     * (when (basic-message-p value) (break "ip")). *)
    make_parameter (value, kind)

fun dummy_variable_id_p id = (id = "dummy-var")

fun raise_error msg =
    msg
(* NOTE: this is not what it does, see /usr/fdl/sys/library/basic/bsc-lisp.lsp. *)

fun error_message tags =
    tags
(* NOTE: this is not what it does, see /usr/fdl/sys/library/basic/bsc-lisp.lsp. *)

fun make_extended_parameter_value kind args =
    kind
(* NOTE: this is not what it does, see /usr/fdl/sys/library/term/trm-defs.lsp. *)

fun error_parameter_value sexpr msg =
    make_extended_parameter_value "error" [sexpr, msg]

val allow_dummy_variable_operator = false

val variable = "variable"
val variable_type = "variable"

val dummy_variable_error_operator =
    (variable, [instantiate_parameter (error_parameter_value "" (error_message "parameter dummy variable"))
				      variable_type])

val dummy_variable_error_operator_r =
    let val special (*declare*) = allow_dummy_variable_operator
    in (if allow_dummy_variable_operator
	then message_emit (warn_message "operator variable_dummy")
	else raise_error (error_message "operator variable dummy");
	dummy_variable_error_operator)
    end

val dummy_token_error_operator =
    ("token", [natural_parameter 1,
	       instantiate_parameter (error_parameter_value "" (error_message "parameter dummy token"))
				     "token"])

fun instantiate_operator id parameters =
    if id = "variable"
       andalso
       ((dummy_variable_id_p (value_of_parameter (hd parameters))) handle _ => false)
    then dummy_variable_error_operator_r
    else if "token_opid" = id
	    andalso
	    (("||" = value_of_parameter (hd (tl parameters))) handle _ => false)
    then dummy_token_error_operator
    else (id, parameters)

fun instantiate_term opr bound_terms =
    make_term opr bound_terms

(* NOTE: this is not how it's defined. *)
val inil_term = []

fun iml_term parse_p result_p term data =
    let val bool = type_id_to_type "bool"
    in instantiate_term (instantiate_operator "iml"
					      [instantiate_parameter parse_p bool,
					       instantiate_parameter result_p bool])
			((instantiate_bound_term term) :: (map instantiate_bound_term data))
    end

fun orb_eval_args_to_term' tok term (t, tl) props =
    let val readonlyp = tok = "readonly"
	val fan       = if readonlyp then "any" else tok
	val rsps      = orb_eval_by_description term
						(iexpression_term (iml_term "false" "true" t tl)
								  false
								  readonlyp)
						fan
						props
    in if listp rsps
       then map_list_to_ilist (map (fn rsp => lisp_or (interpret_result rsp)
						      (raise_error (error_message "result term not list")))
				   rsps)
			      (inil_term)
       else lisp_op (interpret_result rsps)
		    (raise_error (error_message "result term not"))
    end

fun orb_eval_args_to_term tok term (t, tl) =
    orb_eval_args_to_term' tok term (t, tl) "main"
(* NOTE: Defined in /usr/fdl/sys/library/orb/orb-ml.lsp *)

val iml_cons_op = ("!ml_text_cons", [])
fun iml_cons_term a b = make_term iml_cons_op [([], a), ([], b)]

val icons_op = ("!cons", [])
fun icons_term a b = make_term icons_op [([], a), ([], b)]

val null_ap_itext = itext_term "Null_ap "
val term_ap_itext = itext_term "Term_ap "
val lparen_itext  = itext_term "("
val rparen_itext  = itext_term ")"

fun wrap_parens t =
    iml_cons_term lparen_itext (iml_cons_term t rparen_itext)

fun term_ap m t =
    (wrap_parens (iml_cons_term term_ap_itext (B.fst m)), (t :: (B.snd m)))

val nil_term_list : term list = []

fun null_ap term =
    (wrap_parens (iml_cons_term null_ap_itext (wrap_parens term)), nil_term_list)

val nuprl5_library_description_term =
    make_term ("description", [make_token_parameter "NUPRL"])
	      [([], icons_term (inatural_term 5) (inatural_term 0)),
	       ([], itoken_term "LIBRARY")]

fun lib_eval_to_term_aux e = orb_eval_args_to_term "ONE" nuprl5_library_description_term e

val lib_eval_to_term = lib_eval_to_term_aux

val lib_unfold_ap = null_ap (itext_term "lib_unfold ")

fun lib_unfold term = lib_eval_to_term (term_ap lib_unfold_ap term)

fun unfold_ab term = lib_unfold term
(* orb_eval_args_to_term "ONE"
			 nuprl5_library_description_term
			 (let val a = itext_term "Term_ap "
			      val b = wrap_parens (iml_cons_term (itext_term "Null_ap ") (wrap_parens (itext_term "lib_unfold ")))
			  in (wrap_parens (iml_cons_term a b), [term])
			  end)
 *)

(*fun is_free_var var = is_free_var_val (variable_value var)

fun is_bound_var var = not (is_free_var var)

(* NOTE: Defined in /usr/fdl/sys/library/fml/mlt-term.lsp *)
fun second_order_free_variables term =
    let val result = []
	fun vist term =
	    let val _ = if is_variable_term term
			then let val arity = length (bound_terms_of_term term)
			     in if (arity = 0
				    andalso
				    bound_var (id_of_variable_term term))
				   orelse
				   (markv (id_of_variable_term term) arity)
				then ()
				else (push (id_of_variable_term term, arity) result;
				      markv (id_of_variable_term term) arity)
			     end
			else ()
	    in map (fn bterm =>
		       let val _ = enter_bindings (bindings_of_bound_term_n bterm)
			   val _ = visit (term_of_bound_term bterm)
		       in exit_bindings (bindings_of_bound_term_n bterm)
		       end)
		   (bound_terms_of_term term)
	    end
	val _ = visit term
    in result
    end

val free_term_vars_with_arities = second_order_free_variables

fun free_vars term =
    map fst (free_term_vars_with_arities term)*)

val null_var = string_to_variable ""

val mvt = mk_variable_term

fun lookup_binding env v =
    let val (opr, [([w], thunk), ([], e)]) = dest_term env
    in if w = v
       then dest_pair thunk
       else lookup_binding e v
    end

fun closure2term (term, env) =
    let val vars = free_vars term
	val sub = mapfilter (fn v => (v, closure2term (lookup_binding env v))) vars
    in fo_subst sub term
    end

fun add_env_binding env v a e =
    if v = null_var
    then env
    else let val bind =
		 if is_variable_term a
		 then let val (b, e') = lookup_binding e (dest_variable a)
		      in mk_pair_term b e'
		      end handle _ => mk_pair_term a e
		 else mk_pair_term a e
	 in (*let val _ = lookup_binding env v
	    in env
	    end handle _ =>*) mk_term ("env", []) [([v], bind), ([], env)]
	 end

fun fresh_variable term =
    let val frees = free_vars term
	fun getNewFreeVar var =
	    if isin_var_list var frees
	    then getNewFreeVar (var ^ "'")
	    else var
    in getNewFreeVar "w"
    end



end
