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
 *  o File name:   NuprlTerms.sml
 *  o Description: Datatype for nuprl terms
 *)


signature TOREF = sig
    type 'a t
    val get : 'a t -> 'a
    val mk  : 'a -> 'a t
    val set : 'a t -> 'a -> unit
end

structure ToRefRef :> TOREF = struct
type 'a t = 'a ref
fun get x = !x
fun mk x = ref x
fun set x y = (x := y)
end

structure ToRefId :> TOREF = struct
type 'a t = 'a
fun get x = x
fun mk x = x
fun set x y = ()
end

structure ToRefFun :> TOREF = struct
type 'a t = unit -> 'a
fun get x = x ()
fun mk x = fn () => x
fun set x y = ()
end

structure NuprlTerms :> NUPRL_TERMS = struct

structure T  = ListFormat
structure EH = LibBase
structure II = IntInf
structure TR = ToRefId

structure SKEY = struct type ord_key = string val compare = String.compare end
structure IKEY = struct type ord_key = int val compare = Int.compare end
structure SET  = BinarySetFn(SKEY)
structure VARS = SET
(*structure VARS = ListSetFn(KEY)*)
(*structure MAP  = SplayMapFn(KEY)*)
structure MAP  = BinaryMapFn(SKEY)
(*structure MAP  = ListMapFn(KEY)*)
structure SUB  = MAP
structure IMAP = BinaryMapFn(IKEY)

type 'a toref             = 'a TR.t

type nuprl_nat            = II.int

type variable             = string

type opid                 = string

type parameter_kind       = string (* such as: "token", "nat", "level-expression" *)
type parameter_value      = string

type tag                  = string ref

type opid_tag             = opid * tag
type parameter            = parameter_value * parameter_kind

type operator             = opid_tag * parameter list

(*type nuprl_variable   = variable*)

type nuprl_variable = variable * int

datatype nuprl_term       = TERM     of operator * nuprl_bound_term list (* generic term    *)
			  | AXM_TERM                                     (* axiom           *)
			  | BOT_TERM                                     (* axiom           *)
			  | INT_TERM                                     (* int type        *)
			  | VOI_TERM                                     (* void type       *)
			  | DUM_TERM                                     (* dummy term      *)
			  | ATM_TERM of int option                       (* atom type       *)
			  | TOK_TERM of parameter                        (* token           *)
			  | NAT_TERM of nuprl_nat                        (* natural number  *)
			  | VAR_TERM of nuprl_variable                   (* variable        *)
			  | INL_TERM of nuprl_ref_term                   (* left injection  *)
			  | INR_TERM of nuprl_ref_term                   (* right injection *)
			  | MIN_TERM of nuprl_ref_term                   (* minus           *)
			  | FIX_TERM of nuprl_ref_term                   (* fixpoint        *)
			  | CLO_TERM of nuprl_ref_term * env             (* closure         *)
			  | LAM_TERM of nuprl_variable * nuprl_ref_term  (* lambda          *)
			  | REC_TERM of nuprl_variable * nuprl_ref_term  (* rec type        *)
			  | WAI_TERM of nuprl_ref_term * nuprl_ref_term  (* wait            *)
			  | APP_TERM of nuprl_ref_term * nuprl_ref_term  (* application     *)
			  | PAI_TERM of nuprl_ref_term * nuprl_ref_term  (* pair            *)
			  | ADD_TERM of nuprl_ref_term * nuprl_ref_term  (* add             *)
			  | SUB_TERM of nuprl_ref_term * nuprl_ref_term  (* subtract        *)
			  | MUL_TERM of nuprl_ref_term * nuprl_ref_term  (* multiply        *)
			  | DIV_TERM of nuprl_ref_term * nuprl_ref_term  (* divide          *)
			  | REM_TERM of nuprl_ref_term * nuprl_ref_term  (* remainder       *)
			  | EQT_TERM of nuprl_ref_term * nuprl_ref_term  (* eq_term         *)
			  | UNI_TERM of nuprl_ref_term * nuprl_ref_term  (* union           *)
			  | EQU_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term  (* equal          *)
			  | IAX_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term  (* isaxiom        *)
			  | IPA_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term  (* ispair         *)
			  | IIR_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term  (* isinr          *)
			  | IIL_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term  (* isinl          *)
			  | IIN_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term  (* isint          *)
			  | ILA_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term  (* islambda       *)
			  | IAT_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term  (* isatom2        *)
			  | CBV_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term  (* callbyvalue    *)
			  | CBA_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term  (* callbyvalueall *)
			  | FUN_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term  (* function type  *)
			  | PRD_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term  (* product type   *)
			  | TUN_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term  (* tunion type    *)
			  | SET_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term  (* set type       *)
			  | LES_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term * nuprl_ref_term  (* less    *)
			  | IEQ_TERM of nuprl_ref_term * nuprl_ref_term * nuprl_ref_term * nuprl_ref_term  (* int_eq  *)
			  | SPR_TERM of nuprl_ref_term * nuprl_variable * nuprl_variable * nuprl_ref_term  (* spread  *)
			  | AEQ_TERM of int * nuprl_ref_term * nuprl_ref_term * nuprl_ref_term * nuprl_ref_term  (* atom_eq *)
			  | DEC_TERM of nuprl_ref_term * nuprl_variable * nuprl_ref_term * nuprl_variable * nuprl_ref_term (* decide *)
			  | IND_TERM of nuprl_ref_term * nuprl_variable * nuprl_variable * nuprl_ref_term * nuprl_ref_term * nuprl_variable * nuprl_variable * nuprl_ref_term (* integer induction *)
     and nuprl_bound_term = B_TERM   of nuprl_variable list * nuprl_ref_term
     and nuprl_ref_term   = R_TERM   of nuprl_term toref
     and env              = ENV      of (variable * bool * (nuprl_term * bool) ref) MAP.map
(* In env, the first boolean indicates whether the term has already been fully evaluated
 * the second boolean indicates whether the term has been evaluated *)

type sign = (parameter_value option * parameter_kind) list * int list

(* An item is as follows:
 *   - string:   name of the abstraction (and not of the term)
 *   - sign:     signature of the term (lhs) (e.g., foo(0;1;0) -> [0;1;0] + parameters)
 *   - string:   object identifier
 *   - term:     left-hand-side of abstration
 *   - term:     right-hand-side of abstration
 *)
type item     = {id   : string,
		 sign : sign,
		 obid : string,
		 lhs  : nuprl_ref_term,
		 rhs  : nuprl_ref_term,
		 wfs  : nuprl_ref_term list}
type abs_lib  = item toref list MAP.map ref
type tof_lib  = nuprl_term toref MAP.map ref
type lib      = {abs : abs_lib, (* abstractions *)
		 tof : tof_lib} (* TERMOFs      *)

fun get_item_sign (item : item) = #sign item

val get = TR.get
val mk  = TR.mk


(* ------ REF TERMS ------ *)

fun rterm2term (R_TERM rt) = TR.get rt

fun mk_rterm term = R_TERM (TR.mk term)


(* ------ TAGS ------ *)

fun mk_tag  x = ref x
fun get_tag x = !x
fun set_tag x y = (x := y)

val default_dtag = "OPID"
val dummy_dtag   = ""
val dtag         = mk_tag default_dtag

fun get_dtag () = dtag
fun set_dtag v  = set_tag dtag v

fun set_dummy_dtag () = set_dtag dummy_dtag
fun reset_dtag     () = set_dtag default_dtag


(* ------ OPID ------ *)

fun opid_of_term (TERM (((opid, _), _), _)) = opid
  | opid_of_term AXM_TERM     = "axiom"
  | opid_of_term BOT_TERM     = "bottom"
  | opid_of_term INT_TERM     = "int"
  | opid_of_term VOI_TERM     = "void"
  | opid_of_term DUM_TERM     = raise Fail "opid_of_term:DUM_TERM"
  | opid_of_term (ATM_TERM _) = "atom"
  | opid_of_term (TOK_TERM _) = "token"
  | opid_of_term (NAT_TERM _) = "natural_number"
  | opid_of_term (VAR_TERM _) = "variable"
  | opid_of_term (INL_TERM _) = "inl"
  | opid_of_term (INR_TERM _) = "inr"
  | opid_of_term (FIX_TERM _) = "fix"
  | opid_of_term (MIN_TERM _) = "minus"
  | opid_of_term (LAM_TERM _) = "lambda"
  | opid_of_term (REC_TERM _) = "rec"
  | opid_of_term (WAI_TERM _) = "!wait"
  | opid_of_term (APP_TERM _) = "apply"
  | opid_of_term (PAI_TERM _) = "pair"
  | opid_of_term (ADD_TERM _) = "add"
  | opid_of_term (SUB_TERM _) = "subtract"
  | opid_of_term (MUL_TERM _) = "multiply"
  | opid_of_term (DIV_TERM _) = "divide"
  | opid_of_term (REM_TERM _) = "remainder"
  | opid_of_term (EQT_TERM _) = "eq_term"
  | opid_of_term (UNI_TERM _) = "union"
  | opid_of_term (EQU_TERM _) = "equal"
  | opid_of_term (IAX_TERM _) = "isaxiom"
  | opid_of_term (IPA_TERM _) = "ispair"
  | opid_of_term (IIR_TERM _) = "isinr"
  | opid_of_term (IIL_TERM _) = "isinl"
  | opid_of_term (IIN_TERM _) = "isint"
  | opid_of_term (ILA_TERM _) = "islambda"
  | opid_of_term (IAT_TERM _) = "isatom2"
  | opid_of_term (CBV_TERM _) = "callbyvalue"
  | opid_of_term (CBA_TERM _) = "callbyvalueall"
  | opid_of_term (FUN_TERM _) = "function"
  | opid_of_term (PRD_TERM _) = "product"
  | opid_of_term (TUN_TERM _) = "tunion"
  | opid_of_term (SET_TERM _) = "set"
  | opid_of_term (LES_TERM _) = "less"
  | opid_of_term (IEQ_TERM _) = "int_eq"
  | opid_of_term (SPR_TERM _) = "spread"
  | opid_of_term (AEQ_TERM _) = "atom_eq"
  | opid_of_term (DEC_TERM _) = "decide"
  | opid_of_term (IND_TERM _) = "ind"
  | opid_of_term (CLO_TERM _) = "!!closure"


(* ------ REFERENCE VARIABLE ------ *)

fun set_null_nuprl_var (v, n) = ("", n)

fun is_null_nuprl_var ("", _) = true
  | is_null_nuprl_var _ = false

fun mk_new_nuprl_var var = (var, 0)

fun upd_nuprl_var (_, n) v = (v, n)

fun dest_nuprl_var (var, _) = var

fun nvars2set set nvars =
    foldr
	(fn (nvar,set) => VARS.add (set, dest_nuprl_var nvar))
	set
	nvars

fun max_ints m =
    MAP.foldl
	(fn (m,n) => if m > n then m + 1 else n)
	0
	m

(* sets the references of nuprl variables *)
fun set_all_nuprl_var term m =
    case term of
	TERM _ => raise Fail ("set_all_nuprl_var:TERM(" ^ opid_of_term term ^ ")")
      | AXM_TERM => term
      | BOT_TERM => term
      | INT_TERM => term
      | VOI_TERM => term
      | DUM_TERM => term
      | ATM_TERM nop => term
      | TOK_TERM par => term
      | NAT_TERM nat => term
      | VAR_TERM (v,_) =>
	(case MAP.find (m, v) of
	     NONE => raise Fail "set_all_nuprl_var:VAR_TERM"
	   | SOME i => VAR_TERM (v, i))
      | INL_TERM rterm => INL_TERM (set_all_nuprl_rvar rterm m)
      | INR_TERM rterm => INR_TERM (set_all_nuprl_rvar rterm m)
      | FIX_TERM rterm => FIX_TERM (set_all_nuprl_rvar rterm m)
      | MIN_TERM rterm => MIN_TERM (set_all_nuprl_rvar rterm m)
      | LAM_TERM ((v,_), rterm) =>
	let val n = max_ints m
	    val rterm' = set_all_nuprl_rvar rterm (MAP.insert (m, v, n))
	in LAM_TERM ((v,n), rterm')
	end
      | REC_TERM ((v,_), rterm) =>
	let val n = max_ints m
	    val rterm' = set_all_nuprl_rvar rterm (MAP.insert (m, v, n))
	in REC_TERM ((v,n), rterm')
	end
      | WAI_TERM (rterm1, rterm2) => WAI_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m)
      | APP_TERM (rterm1, rterm2) => APP_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m)
      | PAI_TERM (rterm1, rterm2) => PAI_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m)
      | ADD_TERM (rterm1, rterm2) => ADD_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m)
      | SUB_TERM (rterm1, rterm2) => SUB_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m)
      | MUL_TERM (rterm1, rterm2) => MUL_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m)
      | DIV_TERM (rterm1, rterm2) => DIV_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m)
      | REM_TERM (rterm1, rterm2) => REM_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m)
      | EQT_TERM (rterm1, rterm2) => EQT_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m)
      | UNI_TERM (rterm1, rterm2) => UNI_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m)
      | EQU_TERM (rterm1, rterm2, rterm3) => EQU_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m, set_all_nuprl_rvar rterm3 m)
      | IAX_TERM (rterm1, rterm2, rterm3) => IAX_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m, set_all_nuprl_rvar rterm3 m)
      | IPA_TERM (rterm1, rterm2, rterm3) => IPA_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m, set_all_nuprl_rvar rterm3 m)
      | IIR_TERM (rterm1, rterm2, rterm3) => IIR_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m, set_all_nuprl_rvar rterm3 m)
      | IIL_TERM (rterm1, rterm2, rterm3) => IIL_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m, set_all_nuprl_rvar rterm3 m)
      | IIN_TERM (rterm1, rterm2, rterm3) => IIN_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m, set_all_nuprl_rvar rterm3 m)
      | ILA_TERM (rterm1, rterm2, rterm3) => ILA_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m, set_all_nuprl_rvar rterm3 m)
      | IAT_TERM (rterm1, rterm2, rterm3) => IAT_TERM (set_all_nuprl_rvar rterm1 m, set_all_nuprl_rvar rterm2 m, set_all_nuprl_rvar rterm3 m)
      | CBV_TERM (a, (v, _), f) =>
	let val n  = max_ints m
	    val a' = set_all_nuprl_rvar a m
	    val f' = set_all_nuprl_rvar f (MAP.insert (m, v, n))
	in CBV_TERM (a', (v, n), f')
	end
      | CBA_TERM (a, (v, _), f) =>
	let val n  = max_ints m
	    val a' = set_all_nuprl_rvar a m
	    val f' = set_all_nuprl_rvar f (MAP.insert (m, v, n))
	in CBA_TERM (a', (v, n), f')
	end
      | FUN_TERM (a, (v, _), f) =>
	let val n  = max_ints m
	    val a' = set_all_nuprl_rvar a m
	    val f' = set_all_nuprl_rvar f (MAP.insert (m, v, n))
	in FUN_TERM (a', (v, n), f')
	end
      | PRD_TERM (a, (v, _), f) =>
	let val n  = max_ints m
	    val a' = set_all_nuprl_rvar a m
	    val f' = set_all_nuprl_rvar f (MAP.insert (m, v, n))
	in PRD_TERM (a', (v, n), f')
	end
      | TUN_TERM (a, (v, _), f) =>
	let val n  = max_ints m
	    val a' = set_all_nuprl_rvar a m
	    val f' = set_all_nuprl_rvar f (MAP.insert (m, v, n))
	in TUN_TERM (a', (v, n), f')
	end
      | SET_TERM (a, (v, _), f) =>
	let val n  = max_ints m
	    val a' = set_all_nuprl_rvar a m
	    val f' = set_all_nuprl_rvar f (MAP.insert (m, v, n))
	in SET_TERM (a', (v, n), f')
	end
      | LES_TERM (a, b, rterm1, rterm2) =>
	LES_TERM (set_all_nuprl_rvar a m,
		  set_all_nuprl_rvar b m,
		  set_all_nuprl_rvar rterm1 m,
		  set_all_nuprl_rvar rterm2 m)
      | IEQ_TERM (a, b, rterm1, rterm2) =>
	IEQ_TERM (set_all_nuprl_rvar a m,
		  set_all_nuprl_rvar b m,
		  set_all_nuprl_rvar rterm1 m,
		  set_all_nuprl_rvar rterm2 m)
      | SPR_TERM (p, (v1, _), (v2, _), rterm) =>
	let val n1 = max_ints m
	    val n2 = n1 + 1
	    val p' = set_all_nuprl_rvar p m
	    val m' = MAP.insert (MAP.insert (m, v1, n1), v2, n2)
	    val rterm' = set_all_nuprl_rvar rterm m'
	in SPR_TERM (p', (v1, n1), (v2, n2), rterm')
	end
      | AEQ_TERM (n, a, b, rterm1, rterm2) =>
	AEQ_TERM (n,
		  set_all_nuprl_rvar a m,
		  set_all_nuprl_rvar b m,
		  set_all_nuprl_rvar rterm1 m,
		  set_all_nuprl_rvar rterm2 m)
      | DEC_TERM (dec, (v1, _), rterm1, (v2, _), rterm2) =>
	let val n1 = max_ints m
	    val n2 = n1
	    val dec' = set_all_nuprl_rvar dec m
	    val rterm1' = set_all_nuprl_rvar rterm1 (MAP.insert (m, v1, n1))
	    val rterm2' = set_all_nuprl_rvar rterm2 (MAP.insert (m, v2, n2))
	in DEC_TERM (dec', (v1, n1), rterm1', (v2, n2), rterm2')
	end
      | IND_TERM (i, (x, _), (vd, _), downcase, basecase, (y, _), (vu, _), upcase) =>
	let val nx  = max_ints m
	    val nd  = nx + 1
	    val ny  = nx
	    val nu  = nd
	    val md  = MAP.insert (MAP.insert (m, x, nx), vd, nd)
	    val mu  = MAP.insert (MAP.insert (m, y, ny), vu, nu)
	    val i'        = set_all_nuprl_rvar i m
	    val downcase' = set_all_nuprl_rvar downcase md
	    val basecase' = set_all_nuprl_rvar basecase m
	    val upcase'   = set_all_nuprl_rvar upcase mu
	in IND_TERM (i',
		     (x,  nx), (vd, nd), downcase',
		     basecase',
		     (y,  ny), (vu, nu), upcase')
	end
      | CLO_TERM _ => raise Fail "set_all_nuprl_var:CLO_TERM"

and set_all_nuprl_rvar rterm m = mk_rterm (set_all_nuprl_var (rterm2term rterm) m)

val set_all_nuprl_var = fn term => set_all_nuprl_var term MAP.empty

(* -- *)
(*
fun mk_new_nuprl_var var = var

fun upd_nuprl_var _ v = v

fun set_nuprl_var v term = ()

fun set_nuprl_rvar v rterm = ()

fun dest_nuprl_var var = var

fun get_nuprl_var_ref v = NONE
*)


(* ------ BASIC CONSTRUCTORS ------ *)

fun mk_nuprl_variable_parameter  tok    = (tok,              "v")
fun mk_nuprl_nat_parameter       tag    = (Int.toString tag, "n")
fun mk_nuprl_natural_parameter   tag    = (II.toString tag,  "n")
fun mk_nuprl_token_parameter     token  = (token,            "t")
fun mk_nuprl_string_parameter    string = (string,           "s")
fun mk_nuprl_level_exp_parameter level  = (level,            "l")
fun mk_nuprl_obid_parameter      obid   = (obid,             "o")
fun mk_nuprl_ut2_parameter       id     = (id,               "ut2")
fun mk_nuprl_bool_parameter      bool   = (if bool then "T" else "F", "b")

fun mk_nuprl_parameter (param : parameter) = param

fun mk_term opid tag params brterms =
    case (opid, params, brterms) of
	("axiom",  params, []) => AXM_TERM
      | ("bottom", params, []) => BOT_TERM
      | ("int",    params, []) => INT_TERM
      | ("void",   params, []) => VOI_TERM
      | ("natural_number", (n,kind) :: rest, []) =>
	(case II.fromString n of
	     NONE => TERM (((opid,tag),params),[])
	   | SOME x => NAT_TERM x)
      | ("atom", [], []) => ATM_TERM NONE
      | ("atom", [(i,kind)], []) =>
	(case Int.fromString i of
	     NONE => TERM (((opid,tag),params),[])
	   | SOME n => ATM_TERM (SOME n))
      (* for atoms, we sometimes generate a TERM because the library defines atom{n}
       * and not only atom{} and atom{2} *)
      | ("token", [(i,kind)], []) => TOK_TERM (i,kind)
      | ("variable", (var,kind) :: params, []) => VAR_TERM (mk_new_nuprl_var var)
      | ("inl",   _, [B_TERM ([], term)]) => INL_TERM term
      | ("inr",   _, [B_TERM ([], term)]) => INR_TERM term
      | ("fix",   _, [B_TERM ([], term)]) => FIX_TERM term
      | ("minus", _, [B_TERM ([], term)]) => MIN_TERM term
      | ("lambda", _, [B_TERM ([var], body)]) => LAM_TERM (var, body)
      | ("rec",    _, [B_TERM ([var], body)]) => REC_TERM (var, body)
      | ("!wait",     _, [B_TERM ([], t1), B_TERM ([], t2)]) => WAI_TERM (t1, t2)
      | ("apply",     _, [B_TERM ([], t1), B_TERM ([], t2)]) => APP_TERM (t1, t2)
      | ("pair",      _, [B_TERM ([], t1), B_TERM ([], t2)]) => PAI_TERM (t1, t2)
      | ("add",       _, [B_TERM ([], t1), B_TERM ([], t2)]) => ADD_TERM (t1, t2)
      | ("subtract",  _, [B_TERM ([], t1), B_TERM ([], t2)]) => SUB_TERM (t1, t2)
      | ("multiply",  _, [B_TERM ([], t1), B_TERM ([], t2)]) => MUL_TERM (t1, t2)
      | ("divide",    _, [B_TERM ([], t1), B_TERM ([], t2)]) => DIV_TERM (t1, t2)
      | ("remainder", _, [B_TERM ([], t1), B_TERM ([], t2)]) => REM_TERM (t1, t2)
      | ("eq_term",   _, [B_TERM ([], t1), B_TERM ([], t2)]) => EQT_TERM (t1, t2)
      | ("union",     _, [B_TERM ([], t1), B_TERM ([], t2)]) => UNI_TERM (t1, t2)
      | ("equal",    _, [B_TERM ([], a), B_TERM ([], term1), B_TERM ([], term2)]) => EQU_TERM (a, term1, term2)
      | ("isaxiom",  _, [B_TERM ([], a), B_TERM ([], term1), B_TERM ([], term2)]) => IAX_TERM (a, term1, term2)
      | ("ispair",   _, [B_TERM ([], a), B_TERM ([], term1), B_TERM ([], term2)]) => IPA_TERM (a, term1, term2)
      | ("isinr",    _, [B_TERM ([], a), B_TERM ([], term1), B_TERM ([], term2)]) => IIR_TERM (a, term1, term2)
      | ("isinl",    _, [B_TERM ([], a), B_TERM ([], term1), B_TERM ([], term2)]) => IIL_TERM (a, term1, term2)
      | ("isint",    _, [B_TERM ([], a), B_TERM ([], term1), B_TERM ([], term2)]) => IIN_TERM (a, term1, term2)
      | ("islambda", _, [B_TERM ([], a), B_TERM ([], term1), B_TERM ([], term2)]) => ILA_TERM (a, term1, term2)
      | ("isatom2",  _, [B_TERM ([], a), B_TERM ([], term1), B_TERM ([], term2)]) => IAT_TERM (a, term1, term2)
      | ("callbyvalue",    _, [B_TERM ([], a), B_TERM ([x], f)]) => CBV_TERM (a, x, f)
      | ("callbyvalueall", _, [B_TERM ([], a), B_TERM ([x], f)]) => CBA_TERM (a, x, f)
      | ("function",       _, [B_TERM ([], a), B_TERM ([x], f)]) => FUN_TERM (a, x, f)
      | ("product",        _, [B_TERM ([], a), B_TERM ([x], f)]) => PRD_TERM (a, x, f)
      | ("tunion",         _, [B_TERM ([], a), B_TERM ([x], f)]) => TUN_TERM (a, x, f)
      | ("set",            _, [B_TERM ([], a), B_TERM ([x], f)]) => SET_TERM (a, x, f)
      | ("int_eq", _, [B_TERM ([], a), B_TERM ([], b), B_TERM ([], t1), B_TERM ([], t2)]) => IEQ_TERM (a, b, t1, t2)
      | ("less",   _, [B_TERM ([], a), B_TERM ([], b), B_TERM ([], t1), B_TERM ([], t2)]) => LES_TERM (a, b, t1, t2)
      | ("spread", _, [B_TERM ([], pair), B_TERM ([v1, v2], term)]) => SPR_TERM (pair, v1, v2, term)
      | ("atom_eq", [], [B_TERM ([], a), B_TERM ([], b), B_TERM ([], t1), B_TERM ([], t2)]) => AEQ_TERM (0, a, b, t1, t2)
      | ("atom_eq", (n,kind) :: rest, [B_TERM ([], a), B_TERM ([], b), B_TERM ([], t1), B_TERM ([], t2)]) =>
	(case Int.fromString n of
	     NONE => TERM (((opid,tag),params),brterms)
	   | SOME n => AEQ_TERM (n, a, b, t1, t2))
      | ("decide", _, [B_TERM ([], dec), B_TERM ([v1], term1), B_TERM ([v2], term2)]) =>
	DEC_TERM (dec, v1, term1, v2, term2)
      | ("ind", _, [B_TERM ([], i), B_TERM ([x, rd], downcase), B_TERM ([], basecase), B_TERM ([y, ru], upcase)]) =>
	IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)
      | _ => TERM (((opid, tag), params), brterms)

fun make_term opid tag params brterms = TERM (((opid, tag), params), brterms)


(* +TEMP++++++++++++++++++++++++++++ *)
(*val mk_term = make_term*)
(* +++++++++++++++++++++++++++++ *)


fun mk_nuprl_ref_term (opid, params) b_rterms =
    let val subs = map (fn (vars, rterm) => B_TERM (vars, rterm)) b_rterms
    in mk_term opid dtag params subs
    end

fun mk_nuprl_ref_sb_term (opid, params) rterms =
    let val subs = map (fn rterm => B_TERM ([], rterm)) rterms
    in mk_term opid dtag params subs
    end

fun mk_nuprl_term (opid, params) b_terms =
    let val subs = map (fn (vars, term) => B_TERM (vars, mk_rterm term)) b_terms
    in mk_term opid dtag params subs
    end

fun mk_nuprl_sb_term (opid, params) terms =
    let val subs = map (fn term => B_TERM ([], mk_rterm term)) terms
    in mk_term opid dtag params subs
    end

fun mk_nuprl_ref_simple_term opid ref_term_list =
    let val subs = map (fn term => B_TERM ([], term)) ref_term_list
    in mk_term opid dtag [] subs
    end

fun mk_nuprl_simple_term opid term_list =
    let val subs = map (fn term => B_TERM ([], mk_rterm term)) term_list
    in mk_term opid dtag [] subs
    end

fun make_nuprl_ref_term (opid, params) b_rterms =
    let val subs = map (fn (vars, rterm) => B_TERM (vars, rterm)) b_rterms
    in make_term opid dtag params subs
    end

fun make_nuprl_ref_sb_term (opid, params) rterms =
    let val subs = map (fn rterm => B_TERM ([], rterm)) rterms
    in make_term opid dtag params subs
    end

fun make_nuprl_term (opid, params) b_terms =
    let val subs = map (fn (vars, term) => B_TERM (vars, mk_rterm term)) b_terms
    in make_term opid dtag params subs
    end

fun make_nuprl_ref_simple_term opid ref_term_list =
    let val subs = map (fn term => B_TERM ([], term)) ref_term_list
    in make_term opid dtag [] subs
    end

fun make_nuprl_simple_term opid term_list =
    let val subs = map (fn term => B_TERM ([], mk_rterm term)) term_list
    in make_term opid dtag [] subs
    end


(* ------ A FEW USEFUL FUNCTIONS ------ *)

(* -- tests -- *)
fun is_nuprl_variable_term (TERM ((("variable", _), _), _)) = true
  | is_nuprl_variable_term (VAR_TERM _) = true
  | is_nuprl_variable_term _ = false

fun is_nuprl_lambda_term (TERM ((("lambda", _), _), _)) = true
  | is_nuprl_lambda_term (LAM_TERM _) = true
  | is_nuprl_lambda_term _ = false

fun is_nuprl_rec_term (TERM ((("rec", _), _), _)) = true
  | is_nuprl_rec_term (REC_TERM _) = true
  | is_nuprl_rec_term _ = false

fun is_nuprl_function_term (TERM ((("function", _), _), _)) = true
  | is_nuprl_function_term (FUN_TERM _) = true
  | is_nuprl_function_term _ = false

fun is_nuprl_product_term (TERM ((("product", _), _), _)) = true
  | is_nuprl_product_term (PRD_TERM _) = true
  | is_nuprl_product_term _ = false

fun is_nuprl_tunion_term (TERM ((("tunion", _), _), _)) = true
  | is_nuprl_tunion_term (TUN_TERM _) = true
  | is_nuprl_tunion_term _ = false

fun is_nuprl_set_term (TERM ((("set", _), _), _)) = true
  | is_nuprl_set_term (SET_TERM _) = true
  | is_nuprl_set_term _ = false

fun is_nuprl_minus_term (TERM ((("minus", _), _), _)) = true
  | is_nuprl_minus_term (MIN_TERM _) = true
  | is_nuprl_minus_term _ = false

fun is_nuprl_wait_term (TERM ((("!wait", _), _), _)) = true
  | is_nuprl_wait_term (WAI_TERM _) = true
  | is_nuprl_wait_term _ = false

fun is_nuprl_apply_term (TERM ((("apply", _), _), _)) = true
  | is_nuprl_apply_term (APP_TERM _) = true
  | is_nuprl_apply_term _ = false

fun is_nuprl_natural_number_term (TERM ((("natural_number", _), _), _)) = true
  | is_nuprl_natural_number_term (NAT_TERM _) = true
  | is_nuprl_natural_number_term _ = false

(*
fun is_nuprl_cons_term (TERM ((("cons", _), _), _)) = true
  | is_nuprl_cons_term (CON_TERM _) = true
  | is_nuprl_cons_term _ = false
*)

(*
fun is_nuprl_nil_term (TERM ((("nil", _), _), _)) = true
  | is_nuprl_nil_term NIL_TERM = true
  | is_nuprl_nil_term _ = false
*)

fun is_nuprl_axiom_term (TERM ((("axiom", _), _), _)) = true
  | is_nuprl_axiom_term AXM_TERM = true
  | is_nuprl_axiom_term _ = false

fun is_nuprl_bottom_term (TERM ((("bottom", _), _), _)) = true
  | is_nuprl_bottom_term BOT_TERM = true
  | is_nuprl_bottom_term _ = false

fun is_nuprl_int_term (TERM ((("int", _), _), _)) = true
  | is_nuprl_int_term INT_TERM = true
  | is_nuprl_int_term _ = false

fun is_nuprl_void_term (TERM ((("void", _), _), _)) = true
  | is_nuprl_void_term VOI_TERM = true
  | is_nuprl_void_term _ = false

fun is_nuprl_atom_term (TERM ((("atom", _), _), _)) = true
  | is_nuprl_atom_term (ATM_TERM _) = true
  | is_nuprl_atom_term _ = false

fun is_nuprl_token_term (TERM ((("token", _), _), _)) = true
  | is_nuprl_token_term (TOK_TERM _) = true
  | is_nuprl_token_term _ = false

fun is_nuprl_pair_term (TERM ((("pair", _), _), _)) = true
  | is_nuprl_pair_term (PAI_TERM _) = true
  | is_nuprl_pair_term _ = false

fun is_nuprl_inl_term (TERM ((("inl", _), _), _)) = true
  | is_nuprl_inl_term (INL_TERM _) = true
  | is_nuprl_inl_term _ = false

fun is_nuprl_inr_term (TERM ((("inr", _), _), _)) = true
  | is_nuprl_inr_term (INR_TERM _) = true
  | is_nuprl_inr_term _ = false

fun is_nuprl_fix_term (TERM ((("fix", _), _), _)) = true
  | is_nuprl_fix_term (FIX_TERM _) = true
  | is_nuprl_fix_term _ = false

fun is_canonical_term term =
    is_nuprl_lambda_term term
    orelse is_nuprl_pair_term term
    orelse is_nuprl_inl_term term
    orelse is_nuprl_inr_term term
    orelse is_nuprl_axiom_term term
    orelse is_nuprl_natural_number_term term
    orelse opid_of_term term = "token"

fun equal_nuprl_nats n1 n2 = (II.compare (n1, n2) = EQUAL)

(* -- destructors -- *)
fun dest_variable (TERM ((("variable", _), [(var, "v")]), _)) = var
  | dest_variable (VAR_TERM var) = dest_nuprl_var var
  | dest_variable term = raise Fail "dest_variable"

fun dest_ref_lambda n (TERM ((("lambda", _), _), [B_TERM ([var], body)])) = (var, body)
  | dest_ref_lambda n (LAM_TERM (var, body)) = (var, body)
  | dest_ref_lambda n term = raise Fail ("dest_ref_lambda(" ^ Int.toString n ^ "):" ^ opid_of_term term)

fun dest_lambda n term =
    let val (v,rterm) = dest_ref_lambda n term
    in (v, rterm2term rterm)
    end

fun dest_lambdas n term =
    let val (v,term1) = dest_lambda n term
	val (vars,term2) = dest_lambdas n term1
    in (v::vars,term2)
    end handle _ => ([],term)

fun dest_ref_spread n (TERM ((("spread", _), _), [B_TERM ([], pair), B_TERM ([v1, v2], body)])) = (pair, v1, v2, body)
  | dest_ref_spread n (SPR_TERM (pair, v1, v2, body)) = (pair, v1, v2, body)
  | dest_ref_spread n term = raise Fail ("dest_ref_spread(" ^ Int.toString n ^ "):" ^ opid_of_term term)

fun dest_spread n term =
    let val (pair, v1, v2, body) = dest_ref_spread n term
    in (rterm2term pair, v1, v2, rterm2term body)
    end

fun dest_ref_function (TERM ((("function", _), _), [B_TERM ([], term1), B_TERM ([var], term2)])) =
    (term1, var, term2)
  | dest_ref_function (FUN_TERM (rterm1, var, rterm2)) = (rterm1, var, rterm2)
  | dest_ref_function _ = raise EH.Impossible "dest_function"

fun dest_function term =
    let val (rterm1, var, rterm2) = dest_ref_function term
    in (rterm2term rterm1, var, rterm2term rterm2)
    end

fun dest_simple_function term =
    let val (term1, var, term2) = dest_function term
    in case dest_nuprl_var var of
	   "" => (term1, term2)
	 | _ => raise Fail "dest_simple_function"
    end

fun dest_ref_product (TERM ((("product", _), _), [B_TERM ([], term1), B_TERM ([var], term2)])) =
    (term1, var, term2)
  | dest_ref_product (PRD_TERM (rterm1, var, rterm2)) = (rterm1, var, rterm2)
  | dest_ref_product _ = raise EH.Impossible "dest_product"

fun dest_product term =
    let val (rterm1, var, rterm2) = dest_ref_product term
    in (rterm2term rterm1, var, rterm2term rterm2)
    end

fun dest_simple_product term =
    let val (term1, var, term2) = dest_product term
    in case dest_nuprl_var var of
	   "" => (term1, term2)
	 | _ => raise Fail "dest_simple_product"
    end

fun dest_wait (TERM ((("!wait", _), _), [B_TERM ([], time), B_TERM ([], exp)])) = (rterm2term time, rterm2term exp)
  | dest_wait (WAI_TERM (time, exp)) = (rterm2term time, rterm2term exp)
  | dest_wait term = raise Fail ("dest_wait")

fun dest_apply (TERM ((("apply", _), _), [B_TERM ([], rterm1), B_TERM ([], rterm2)])) = (rterm2term rterm1, rterm2term rterm2)
  | dest_apply (APP_TERM (rterm1, rterm2)) = (rterm2term rterm1, rterm2term rterm2)
  | dest_apply term = raise Fail ("dest_apply:" ^ opid_of_term term)

fun dest_pair n (TERM ((("pair", _), _), [B_TERM ([], rt1), B_TERM ([], rt2)])) = (rterm2term rt1, rterm2term rt2)
  | dest_pair n (PAI_TERM (rterm1, rterm2)) = (rterm2term rterm1, rterm2term rterm2)
  | dest_pair n term = raise Fail ("dest_pair(" ^ opid_of_term term ^ "," ^ Int.toString n ^ ")")

fun dest_rpair n (TERM ((("pair", _), _), [B_TERM ([], rt1), B_TERM ([], rt2)])) = (rt1, rt2)
  | dest_rpair n (PAI_TERM (rterm1, rterm2)) = (rterm1, rterm2)
  | dest_rpair n term = raise Fail ("dest_rpair(" ^ opid_of_term term ^ "," ^ Int.toString n ^ ")")

fun dest_natural_number (TERM ((("natural_number", tag), ((n, kind) :: params)), [])) =
    (case II.fromString n of
	 NONE => raise Fail ("dest_natural_number:no_nat_in_string(" ^ n ^ ")")
       | SOME x => x)
  | dest_natural_number (NAT_TERM n) = n
  | dest_natural_number _ = raise EH.Impossible "dest_natural_number"

fun dest_token n (TERM ((("token", _), [(t,k)]), [])) = (t,k)
  | dest_token n (TOK_TERM (t,k)) = (t,k)
  | dest_token n term = raise Fail ("dest_token(" ^ Int.toString n ^ "):" ^ opid_of_term term)

fun dest_id n term =
    case dest_token n term of
	(t, "ut2") => t
      | _ => raise Fail ("dest_id(" ^ Int.toString n ^ ")")

fun dest_minus (TERM ((("minus", _), _), [B_TERM ([], rterm)])) = rterm2term rterm
  | dest_minus (MIN_TERM rterm) = rterm2term rterm
  | dest_minus term = raise Fail ("dest_minus")

fun dest_inl (TERM ((("inl", _), _), [B_TERM ([], rterm)])) = rterm2term rterm
  | dest_inl (INL_TERM rterm) = rterm2term rterm
  | dest_inl term = raise Fail ("dest_inl")

fun dest_rinl (TERM ((("inl", _), _), [B_TERM ([], rterm)])) = rterm
  | dest_rinl (INL_TERM rterm) = rterm
  | dest_rinl term = raise Fail ("dest_rinl")

fun dest_inr (TERM ((("inr", _), _), [B_TERM ([], rterm)])) = rterm2term rterm
  | dest_inr (INR_TERM rterm) = rterm2term rterm
  | dest_inr term = raise Fail ("dest_inr")

fun dest_rinr (TERM ((("inr", _), _), [B_TERM ([], rterm)])) = rterm
  | dest_rinr (INR_TERM rterm) = rterm
  | dest_rinr term = raise Fail ("dest_rinr")

fun dest_fix (TERM ((("fix", _), _), [B_TERM ([], rterm)])) = rterm2term rterm
  | dest_fix (FIX_TERM rterm) = rterm2term rterm
  | dest_fix term = raise Fail ("dest_fix")


(* variable *)
fun mk_variable_term var = VAR_TERM (mk_new_nuprl_var var)
fun make_variable_term var =
    TERM ((("variable", dtag), [mk_nuprl_variable_parameter var]), [])

(* apply *)
fun mk_apply_ref_term func arg = APP_TERM (func, arg)
fun mk_apply_term func arg = APP_TERM (mk_rterm func, mk_rterm arg)
fun make_apply_term func arg =
    TERM ((("apply", dtag), []), [B_TERM ([], func), B_TERM ([], arg)])

(* lambda *)
fun mk_lambda_ref_term var rterm = LAM_TERM (mk_new_nuprl_var var, rterm)
fun mk_lambda_term var term = LAM_TERM (mk_new_nuprl_var var, mk_rterm term)
fun make_lambda_term var rterm =
    TERM ((("lambda", dtag), []), [B_TERM ([var], rterm)])

(* rec *)
fun mk_rec_ref_term var rterm = REC_TERM (mk_new_nuprl_var var, rterm)
fun mk_rec_term var term = REC_TERM (mk_new_nuprl_var var, mk_rterm term)
fun make_rec_term var rterm =
    TERM ((("rec", dtag), []), [B_TERM ([var], rterm)])

(* wait *)
fun mk_wait_ref_term time exp = WAI_TERM (time, exp)
fun mk_wait_term time exp = WAI_TERM (mk_rterm time, mk_rterm exp)
fun make_wait_term time exp =
    TERM ((("!wait", dtag), []), [B_TERM ([], time), B_TERM ([], exp)])

(* pair *)
fun mk_pair_ref_term t1 t2 = PAI_TERM (t1, t2)
fun mk_pair_term t1 t2 = PAI_TERM (mk_rterm t1, mk_rterm t2)
fun make_pair_term t1 t2 =
    TERM ((("pair", dtag), []), [B_TERM ([], t1), B_TERM ([], t2)])

(* add *)
fun mk_add_ref_term t1 t2 = ADD_TERM (t1, t2)
fun mk_add_term t1 t2 = ADD_TERM (mk_rterm t1, mk_rterm t2)
fun make_add_term t1 t2 =
    TERM ((("add", dtag), []), [B_TERM ([], t1), B_TERM ([], t2)])

(* subtract *)
fun mk_subtract_ref_term t1 t2 = SUB_TERM (t1, t2)
fun mk_subtract_term t1 t2 = SUB_TERM (mk_rterm t1, mk_rterm t2)
fun make_subtract_term t1 t2 =
    TERM ((("subtract", dtag), []), [B_TERM ([], t1), B_TERM ([], t2)])

(* multiply *)
fun mk_multiply_ref_term t1 t2 = MUL_TERM (t1, t2)
fun mk_multiply_term t1 t2 = MUL_TERM (mk_rterm t1, mk_rterm t2)
fun make_multiply_term t1 t2 =
    TERM ((("multiply", dtag), []), [B_TERM ([], t1), B_TERM ([], t2)])

(* divide *)
fun mk_divide_ref_term t1 t2 = DIV_TERM (t1, t2)
fun mk_divide_term t1 t2 = DIV_TERM (mk_rterm t1, mk_rterm t2)
fun make_divide_term t1 t2 =
    TERM ((("divide", dtag), []), [B_TERM ([], t1), B_TERM ([], t2)])

(* remainder *)
fun mk_remainder_ref_term t1 t2 = REM_TERM (t1, t2)
fun mk_remainder_term t1 t2 = REM_TERM (mk_rterm t1, mk_rterm t2)
fun make_remainder_term t1 t2 =
    TERM ((("remainder", dtag), []), [B_TERM ([], t1), B_TERM ([], t2)])

(* eq_term *)
fun mk_eq_term_ref_term t1 t2 = EQT_TERM (t1, t2)
fun mk_eq_term_term t1 t2 = EQT_TERM (mk_rterm t1, mk_rterm t2)
fun make_eq_term_term t1 t2 =
    TERM ((("eq_term", dtag), []), [B_TERM ([], t1), B_TERM ([], t2)])

(* union *)
fun mk_union_ref_term t1 t2 = UNI_TERM (t1, t2)
fun mk_union_term t1 t2 = UNI_TERM (mk_rterm t1, mk_rterm t2)
fun make_union_term t1 t2 =
    TERM ((("union", dtag), []), [B_TERM ([], t1), B_TERM ([], t2)])

(* axiom *)
val mk_axiom_term = AXM_TERM
val make_axiom_term = TERM ((("axiom", dtag), []), [])

(* dummy *)
val mk_dummy_term = DUM_TERM
val make_dummy_term = TERM ((("dummy", dtag), []), [])

(* bottom *)
val mk_bottom_term = BOT_TERM
val make_bottom_term = TERM ((("bottom", dtag), []), [])

(* int *)
val mk_int_term = INT_TERM
val make_int_term = TERM ((("int", dtag), []), [])

(* void *)
val mk_void_term = VOI_TERM
val make_void_term = TERM ((("void", dtag), []), [])

(* atom *)
fun mk_atom_term nop = ATM_TERM nop
fun make_atom_term NONE = TERM ((("atom", dtag), []), [])
  | make_atom_term (SOME n) = TERM ((("atom", dtag), [mk_nuprl_nat_parameter n]), [])

(* token *)
fun mk_token_term (t, k) = TOK_TERM (t,k)
fun make_token_term (t, k) = TERM ((("token", dtag), [(t,k)]), [])
fun mk_regular_token_term tok = mk_token_term (mk_nuprl_token_parameter tok)
fun mk_mkid_term id = mk_token_term (mk_nuprl_ut2_parameter id)

(* natural number *)
fun mk_natural_number_term nat = NAT_TERM nat
fun make_natural_number_term nat = TERM ((("natural_number", dtag), [mk_nuprl_natural_parameter nat]), [])

(* inl *)
fun mk_inl_ref_term rterm = INL_TERM rterm
fun mk_inl_term term = INL_TERM (mk_rterm term)
fun make_inl_term rterm = TERM ((("inl", dtag), []), [B_TERM ([], rterm)])

(* inr *)
fun mk_inr_ref_term rterm = INR_TERM rterm
fun mk_inr_term term = INR_TERM (mk_rterm term)
fun make_inr_term rterm = TERM ((("inr", dtag), []), [B_TERM ([], rterm)])

(* fix *)
fun mk_fix_ref_term rterm = FIX_TERM rterm
fun mk_fix_term term = FIX_TERM (mk_rterm term)
fun make_fix_term rterm = TERM ((("fix", dtag), []), [B_TERM ([], rterm)])

(* minus *)
fun mk_minus_ref_term rterm = MIN_TERM rterm
fun mk_minus_term term = MIN_TERM (mk_rterm term)
fun make_minus_term rterm = TERM ((("minus", dtag), []), [B_TERM ([], rterm)])

(* induction *)
fun mk_ind_nterm i (x, rd, downcase) basecase (y, ru, upcase) =
    IND_TERM (mk_rterm i,
	      x, rd, mk_rterm downcase,
	      mk_rterm basecase,
	      y, rd, mk_rterm upcase)

fun mk_ind_term i (x, rd, downcase) basecase (y, ru, upcase) =
    IND_TERM (mk_rterm i,
	      mk_new_nuprl_var x,
	      mk_new_nuprl_var rd,
	      mk_rterm downcase,
	      mk_rterm basecase,
	      mk_new_nuprl_var y,
	      mk_new_nuprl_var rd,
	      mk_rterm upcase)

fun mk_ind_ref_term i (x, rd, downcase) basecase (y, ru, upcase) =
    IND_TERM (i,
	      mk_new_nuprl_var x,
	      mk_new_nuprl_var rd,
	      downcase,
	      basecase,
	      mk_new_nuprl_var y,
	      mk_new_nuprl_var rd,
	      upcase)

fun mk_ind_ref_nterm i (x, rd, downcase) basecase (y, ru, upcase) =
    IND_TERM (i,
	      x, rd, downcase,
	      basecase,
	      y, rd, upcase)

fun make_ind_term i (x, rd, downcase) basecase (y, ru, upcase) =
    TERM ((("ind", dtag), []),
	  [B_TERM ([], i),
	   B_TERM ([x, rd], downcase),
	   B_TERM ([], basecase),
	   B_TERM ([y, ru], upcase)])


(* spread *)
fun mk_spread_ref_term pair (var1, var2, term) =
    SPR_TERM (pair,
	      mk_new_nuprl_var var1,
	      mk_new_nuprl_var var2,
	      term)

fun mk_spread_term pair (var1, var2, term) =
    SPR_TERM (mk_rterm pair,
	      mk_new_nuprl_var var1,
	      mk_new_nuprl_var var2,
	      mk_rterm term)

fun mk_spread_nterm pair (var1, var2, term) =
    SPR_TERM (mk_rterm pair,
	      var1,
	      var2,
	      mk_rterm term)

fun make_spread_term pair (var1, var2, rterm) =
    TERM ((("spread", dtag), []),
	  [B_TERM ([], pair),
	   B_TERM ([var1, var2], rterm)])

(* decide *)
fun mk_decide_ref_term dec (var1, term1) (var2, term2) =
    DEC_TERM (dec,
	      mk_new_nuprl_var var1,
	      term1,
	      mk_new_nuprl_var var2,
	      term2)

fun mk_decide_term dec (var1, term1) (var2, term2) =
    DEC_TERM (mk_rterm dec,
	      mk_new_nuprl_var var1,
	      mk_rterm term1,
	      mk_new_nuprl_var var2,
	      mk_rterm term2)

fun make_decide_term dec (var1, rterm1) (var2, rterm2) =
    TERM ((("decide", dtag), []),
	  [B_TERM ([], dec),
	   B_TERM ([var1], rterm1),
	   B_TERM ([var2], rterm2)])

(* less *)
fun mk_less_ref_term a b rterm1 rterm2 = LES_TERM (a, b, rterm1, rterm2)
fun mk_less_term a b term1 term2 = LES_TERM (mk_rterm a, mk_rterm b, mk_rterm term1, mk_rterm term2)
fun make_less_term a b rterm1 rterm2 =
    TERM ((("less", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([], b),
	   B_TERM ([], rterm1),
	   B_TERM ([], rterm2)])

(* int_eq *)
fun mk_int_eq_ref_term a b rterm1 rterm2 = IEQ_TERM (a, b, rterm1, rterm2)
fun mk_int_eq_term a b term1 term2 = IEQ_TERM (mk_rterm a, mk_rterm b, mk_rterm term1, mk_rterm term2)
fun make_int_eq_term a b rterm1 rterm2 =
    TERM ((("int_eq", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([], b),
	   B_TERM ([], rterm1),
	   B_TERM ([], rterm2)])

(* atom_eq *)
fun mk_atom_eq_ref_term n a b rterm1 rterm2 =
    AEQ_TERM (n, a, b, rterm1, rterm2)
fun mk_atom_eq_term n a b term1 term2 =
    AEQ_TERM (n, mk_rterm a, mk_rterm b, mk_rterm term1, mk_rterm term2)
fun make_atom_eq_term n a b rterm1 rterm2 =
    TERM ((("atom_eq", dtag), if n = 0 then [] else [mk_nuprl_nat_parameter n]),
	  [B_TERM ([], a),
	   B_TERM ([], b),
	   B_TERM ([], rterm1),
	   B_TERM ([], rterm2)])

(* callbyvalue *)
fun mk_callbyvalue_ref_term a (x, f) =
    CBV_TERM (a, mk_new_nuprl_var x, f)
fun mk_callbyvalue_term a (x, f) =
    CBV_TERM (mk_rterm a, mk_new_nuprl_var x, mk_rterm f)
fun make_callbyvalue_term a (x, f) =
    TERM ((("callbyvalue", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([x], f)])

(* callbyvalueall *)
fun mk_callbyvalueall_ref_term a (x, f) =
    CBA_TERM (a, mk_new_nuprl_var x, f)
fun mk_callbyvalueall_term a (x, f) =
    CBA_TERM (mk_rterm a, mk_new_nuprl_var x, mk_rterm f)
fun make_callbyvalueall_term a (x, f) =
    TERM ((("callbyvalueall", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([x], f)])
fun mk_set_cbva_term term =
    let val nv = mk_new_nuprl_var "x"
    in CBA_TERM (mk_rterm term, nv, mk_rterm (VAR_TERM nv))
    end


(* function *)
fun mk_function_ref_term a (x, f) = FUN_TERM (a, mk_new_nuprl_var x, f)
fun mk_function_term a (x, f) = FUN_TERM (mk_rterm a, mk_new_nuprl_var x, mk_rterm f)
fun make_function_term a (x, f) =
    TERM ((("function", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([x], f)])
fun mk_fun_term term1 term2 = mk_function_term term1 ("", term2)

(* product *)
fun mk_product_ref_term a (x, f) = PRD_TERM (a, mk_new_nuprl_var x, f)
fun mk_product_term a (x, f) = PRD_TERM (mk_rterm a, mk_new_nuprl_var x, mk_rterm f)
fun make_product_term a (x, f) =
    TERM ((("product", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([x], f)])
fun mk_prod_term term1 term2 = mk_product_term term1 ("", term2)

(* tunion *)
fun mk_tunion_ref_term a (x, f) =
    TUN_TERM (a, mk_new_nuprl_var x, f)
fun mk_tunion_term a (x, f) =
    TUN_TERM (mk_rterm a, mk_new_nuprl_var x, mk_rterm f)
fun make_tunion_term a (x, f) =
    TERM ((("tunion", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([x], f)])

(* set *)
fun mk_set_ref_term a (x, f) =
    SET_TERM (a, mk_new_nuprl_var x, f)
fun mk_set_term a (x, f) =
    SET_TERM (mk_rterm a, mk_new_nuprl_var x, mk_rterm f)
fun make_set_term a (x, f) =
    TERM ((("set", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([x], f)])

(* isaxiom *)
fun mk_isaxiom_ref_term a rterm1 rterm2 = IAX_TERM (a, rterm1, rterm2)
fun mk_isaxiom_term a term1 term2 = IAX_TERM (mk_rterm a, mk_rterm term1, mk_rterm term2)
fun make_isaxiom_term a rterm1 rterm2 =
    TERM ((("isaxiom", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([], rterm1),
	   B_TERM ([], rterm2)])

(* ispair *)
fun mk_ispair_ref_term a rterm1 rterm2 = IPA_TERM (a, rterm1, rterm2)
fun mk_ispair_term a term1 term2 = IPA_TERM (mk_rterm a, mk_rterm term1, mk_rterm term2)
fun make_ispair_term a rterm1 rterm2 =
    TERM ((("ispair", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([], rterm1),
	   B_TERM ([], rterm2)])

(* isinl *)
fun mk_isinl_ref_term a rterm1 rterm2 = IIL_TERM (a, rterm1, rterm2)
fun mk_isinl_term a term1 term2 = IIR_TERM (mk_rterm a, mk_rterm term1, mk_rterm term2)
fun make_isinl_term a rterm1 rterm2 =
    TERM ((("isinl", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([], rterm1),
	   B_TERM ([], rterm2)])

(* isinr *)
fun mk_isinr_ref_term a rterm1 rterm2 = IIR_TERM (a, rterm1, rterm2)
fun mk_isinr_term a term1 term2 = IIR_TERM (mk_rterm a, mk_rterm term1, mk_rterm term2)
fun make_isinr_term a rterm1 rterm2 =
    TERM ((("isinr", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([], rterm1),
	   B_TERM ([], rterm2)])

(* isint *)
fun mk_isint_ref_term a rterm1 rterm2 = IIN_TERM (a, rterm1, rterm2)
fun mk_isint_term a term1 term2 = IIN_TERM (mk_rterm a, mk_rterm term1, mk_rterm term2)
fun make_isint_term a rterm1 rterm2 =
    TERM ((("isint", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([], rterm1),
	   B_TERM ([], rterm2)])

(* islambda *)
fun mk_islambda_ref_term a rterm1 rterm2 = ILA_TERM (a, rterm1, rterm2)
fun mk_islambda_term a term1 term2 = ILA_TERM (mk_rterm a, mk_rterm term1, mk_rterm term2)
fun make_islambda_term a rterm1 rterm2 =
    TERM ((("islambda", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([], rterm1),
	   B_TERM ([], rterm2)])

(* isatom2 *)
fun mk_isatom2_ref_term a rterm1 rterm2 = IAT_TERM (a, rterm1, rterm2)
fun mk_isatom2_term a term1 term2 = IAT_TERM (mk_rterm a, mk_rterm term1, mk_rterm term2)
fun make_isatom2_term a rterm1 rterm2 =
    TERM ((("isatom2", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([], rterm1),
	   B_TERM ([], rterm2)])

(* equal *)
fun mk_equal_ref_term a rterm1 rterm2 = EQU_TERM (a, rterm1, rterm2)
fun mk_equal_term a term1 term2 = EQU_TERM (mk_rterm a, mk_rterm term1, mk_rterm term2)
fun make_equal_term a rterm1 rterm2 =
    TERM ((("equal", dtag), []),
	  [B_TERM ([], a),
	   B_TERM ([], rterm1),
	   B_TERM ([], rterm2)])

(* transform to TERM form -- shallow *)
fun term2TERM term =
    case term of
	TERM _ => term
      | AXM_TERM => make_axiom_term
      | BOT_TERM => make_bottom_term
      | INT_TERM => make_int_term
      | VOI_TERM => make_void_term
      | DUM_TERM => make_dummy_term
      | ATM_TERM nop => make_atom_term nop
      | TOK_TERM par => make_token_term par
      | NAT_TERM nat => make_natural_number_term nat
      | VAR_TERM var => make_variable_term (dest_nuprl_var var)
      | INL_TERM rterm => make_inl_term   rterm
      | INR_TERM rterm => make_inr_term   rterm
      | FIX_TERM rterm => make_fix_term   rterm
      | MIN_TERM rterm => make_minus_term rterm
      | LAM_TERM (var, rterm) => make_lambda_term var rterm
      | REC_TERM (var, rterm) => make_rec_term    var rterm
      | WAI_TERM (rterm1, rterm2) => make_wait_term      rterm1 rterm2
      | APP_TERM (rterm1, rterm2) => make_apply_term     rterm1 rterm2
      | PAI_TERM (rterm1, rterm2) => make_pair_term      rterm1 rterm2
      | ADD_TERM (rterm1, rterm2) => make_add_term       rterm1 rterm2
      | SUB_TERM (rterm1, rterm2) => make_subtract_term  rterm1 rterm2
      | MUL_TERM (rterm1, rterm2) => make_multiply_term  rterm1 rterm2
      | DIV_TERM (rterm1, rterm2) => make_divide_term    rterm1 rterm2
      | REM_TERM (rterm1, rterm2) => make_remainder_term rterm1 rterm2
      | EQT_TERM (rterm1, rterm2) => make_eq_term_term   rterm1 rterm2
      | UNI_TERM (rterm1, rterm2) => make_union_term     rterm1 rterm2
      | EQU_TERM (rterm1, rterm2, rterm3) => make_equal_term    rterm1 rterm2 rterm3
      | IAX_TERM (rterm1, rterm2, rterm3) => make_isaxiom_term  rterm1 rterm2 rterm3
      | IPA_TERM (rterm1, rterm2, rterm3) => make_ispair_term   rterm1 rterm2 rterm3
      | IIR_TERM (rterm1, rterm2, rterm3) => make_isinr_term    rterm1 rterm2 rterm3
      | IIL_TERM (rterm1, rterm2, rterm3) => make_isinl_term    rterm1 rterm2 rterm3
      | IIN_TERM (rterm1, rterm2, rterm3) => make_isint_term    rterm1 rterm2 rterm3
      | ILA_TERM (rterm1, rterm2, rterm3) => make_islambda_term rterm1 rterm2 rterm3
      | IAT_TERM (rterm1, rterm2, rterm3) => make_isatom2_term  rterm1 rterm2 rterm3
      | CBV_TERM (a, x, f) => make_callbyvalue_term    a (x, f)
      | CBA_TERM (a, x, f) => make_callbyvalueall_term a (x, f)
      | FUN_TERM (a, x, f) => make_function_term       a (x, f)
      | PRD_TERM (a, x, f) => make_product_term        a (x, f)
      | TUN_TERM (a, x, f) => make_tunion_term         a (x, f)
      | SET_TERM (a, x, f) => make_set_term            a (x, f)
      | LES_TERM (a, b, rterm1, rterm2) => make_less_term   a b rterm1 rterm2
      | IEQ_TERM (a, b, rterm1, rterm2) => make_int_eq_term a b rterm1 rterm2
      | SPR_TERM (p, var1, var2, rterm) => make_spread_term p (var1, var2, rterm)
      | AEQ_TERM (n, a, b, rterm1, rterm2) => make_atom_eq_term n a b rterm1 rterm2
      | DEC_TERM (dec, var1, rterm1, var2, rterm2) => make_decide_term dec (var1, rterm1) (var2, rterm2)
      | IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase) => make_ind_term i (x, rd, downcase) basecase (y, ru, upcase)
      | CLO_TERM _ => raise Fail "term2TERM:CLO_TERM"

fun TERM2term term =
    case term of
	TERM (((opid, tag), params), brterms) =>
	mk_term opid tag params
		(map (fn (B_TERM (vars, rterm)) =>
			 B_TERM (vars, mk_rterm (TERM2term (rterm2term rterm))))
		     brterms)
      | AXM_TERM => term
      | BOT_TERM => term
      | INT_TERM => term
      | VOI_TERM => term
      | DUM_TERM => term
      | ATM_TERM nop => term
      | TOK_TERM par => term
      | NAT_TERM nat => term
      | VAR_TERM var => term
      | INL_TERM rterm => INL_TERM (TERM2rterm rterm)
      | INR_TERM rterm => INR_TERM (TERM2rterm rterm)
      | FIX_TERM rterm => FIX_TERM (TERM2rterm rterm)
      | MIN_TERM rterm => MIN_TERM (TERM2rterm rterm)
      | LAM_TERM (var, rterm) => LAM_TERM (var, TERM2rterm rterm)
      | REC_TERM (var, rterm) => REC_TERM (var, TERM2rterm rterm)
      | WAI_TERM (rterm1, rterm2) => WAI_TERM (TERM2rterm rterm1, TERM2rterm rterm2)
      | APP_TERM (rterm1, rterm2) => APP_TERM (TERM2rterm rterm1, TERM2rterm rterm2)
      | PAI_TERM (rterm1, rterm2) => PAI_TERM (TERM2rterm rterm1, TERM2rterm rterm2)
      | ADD_TERM (rterm1, rterm2) => ADD_TERM (TERM2rterm rterm1, TERM2rterm rterm2)
      | SUB_TERM (rterm1, rterm2) => SUB_TERM (TERM2rterm rterm1, TERM2rterm rterm2)
      | MUL_TERM (rterm1, rterm2) => MUL_TERM (TERM2rterm rterm1, TERM2rterm rterm2)
      | DIV_TERM (rterm1, rterm2) => DIV_TERM (TERM2rterm rterm1, TERM2rterm rterm2)
      | REM_TERM (rterm1, rterm2) => REM_TERM (TERM2rterm rterm1, TERM2rterm rterm2)
      | EQT_TERM (rterm1, rterm2) => EQT_TERM (TERM2rterm rterm1, TERM2rterm rterm2)
      | UNI_TERM (rterm1, rterm2) => UNI_TERM (TERM2rterm rterm1, TERM2rterm rterm2)
      | EQU_TERM (rterm1, rterm2, rterm3) => EQU_TERM (TERM2rterm rterm1, TERM2rterm rterm2, TERM2rterm rterm3)
      | IAX_TERM (rterm1, rterm2, rterm3) => IAX_TERM (TERM2rterm rterm1, TERM2rterm rterm2, TERM2rterm rterm3)
      | IPA_TERM (rterm1, rterm2, rterm3) => IPA_TERM (TERM2rterm rterm1, TERM2rterm rterm2, TERM2rterm rterm3)
      | IIR_TERM (rterm1, rterm2, rterm3) => IIR_TERM (TERM2rterm rterm1, TERM2rterm rterm2, TERM2rterm rterm3)
      | IIL_TERM (rterm1, rterm2, rterm3) => IIL_TERM (TERM2rterm rterm1, TERM2rterm rterm2, TERM2rterm rterm3)
      | IIN_TERM (rterm1, rterm2, rterm3) => IIN_TERM (TERM2rterm rterm1, TERM2rterm rterm2, TERM2rterm rterm3)
      | ILA_TERM (rterm1, rterm2, rterm3) => ILA_TERM (TERM2rterm rterm1, TERM2rterm rterm2, TERM2rterm rterm3)
      | IAT_TERM (rterm1, rterm2, rterm3) => IAT_TERM (TERM2rterm rterm1, TERM2rterm rterm2, TERM2rterm rterm3)
      | CBV_TERM (a, x, f) => CBV_TERM (TERM2rterm a, x, TERM2rterm f)
      | CBA_TERM (a, x, f) => CBA_TERM (TERM2rterm a, x, TERM2rterm f)
      | FUN_TERM (a, x, f) => FUN_TERM (TERM2rterm a, x, TERM2rterm f)
      | PRD_TERM (a, x, f) => PRD_TERM (TERM2rterm a, x, TERM2rterm f)
      | TUN_TERM (a, x, f) => TUN_TERM (TERM2rterm a, x, TERM2rterm f)
      | SET_TERM (a, x, f) => SET_TERM (TERM2rterm a, x, TERM2rterm f)
      | LES_TERM (a, b, rterm1, rterm2) => LES_TERM (TERM2rterm a, TERM2rterm b, TERM2rterm rterm1, TERM2rterm rterm2)
      | IEQ_TERM (a, b, rterm1, rterm2) => IEQ_TERM (TERM2rterm a, TERM2rterm b, TERM2rterm rterm1, TERM2rterm rterm2)
      | SPR_TERM (p, var1, var2, rterm) => SPR_TERM (TERM2rterm p, var1, var2, TERM2rterm rterm)
      | AEQ_TERM (n, a, b, rterm1, rterm2) => AEQ_TERM (n, TERM2rterm a, TERM2rterm b, TERM2rterm rterm1, TERM2rterm rterm2)
      | DEC_TERM (dec, var1, rterm1, var2, rterm2) => DEC_TERM (TERM2rterm dec, var1, TERM2rterm rterm1, var2, TERM2rterm rterm2)
      | IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase) => IND_TERM (TERM2rterm i, x, rd, TERM2rterm downcase, TERM2rterm basecase, y, ru, TERM2rterm upcase)
      | CLO_TERM _ => raise Fail "term2TERM_deep:CLO_TERM"

and TERM2rterm rterm = mk_rterm (TERM2term (rterm2term rterm))


(* +TEMP++++++++++++++++++++++++++++ *)
(*fun TERM2term term = term*)
(* +++++++++++++++++++++++++++++ *)

(* transform to TERM form -- deep *)
fun term2TERM_deep term =
    case term of
	TERM _ => term
      | AXM_TERM => make_axiom_term
      | BOT_TERM => make_bottom_term
      | INT_TERM => make_int_term
      | VOI_TERM => make_void_term
      | DUM_TERM => make_dummy_term
      | ATM_TERM nop => make_atom_term nop
      | TOK_TERM par => make_token_term par
      | NAT_TERM nat => make_natural_number_term nat
      | VAR_TERM var => make_variable_term (dest_nuprl_var var)
      | INL_TERM rterm => make_inl_term   (rterm2TERM_deep rterm)
      | INR_TERM rterm => make_inr_term   (rterm2TERM_deep rterm)
      | FIX_TERM rterm => make_fix_term   (rterm2TERM_deep rterm)
      | MIN_TERM rterm => make_minus_term (rterm2TERM_deep rterm)
      | LAM_TERM (var, rterm) => make_lambda_term var (rterm2TERM_deep rterm)
      | REC_TERM (var, rterm) => make_rec_term    var (rterm2TERM_deep rterm)
      | WAI_TERM (rterm1, rterm2) => make_wait_term      (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2)
      | APP_TERM (rterm1, rterm2) => make_apply_term     (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2)
      | PAI_TERM (rterm1, rterm2) => make_pair_term      (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2)
      | ADD_TERM (rterm1, rterm2) => make_add_term       (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2)
      | SUB_TERM (rterm1, rterm2) => make_subtract_term  (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2)
      | MUL_TERM (rterm1, rterm2) => make_multiply_term  (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2)
      | DIV_TERM (rterm1, rterm2) => make_divide_term    (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2)
      | REM_TERM (rterm1, rterm2) => make_remainder_term (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2)
      | EQT_TERM (rterm1, rterm2) => make_eq_term_term   (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2)
      | UNI_TERM (rterm1, rterm2) => make_union_term     (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2)
      | EQU_TERM (rterm1, rterm2, rterm3) => make_equal_term    (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2) (rterm2TERM_deep rterm3)
      | IAX_TERM (rterm1, rterm2, rterm3) => make_isaxiom_term  (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2) (rterm2TERM_deep rterm3)
      | IPA_TERM (rterm1, rterm2, rterm3) => make_ispair_term   (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2) (rterm2TERM_deep rterm3)
      | IIR_TERM (rterm1, rterm2, rterm3) => make_isinr_term    (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2) (rterm2TERM_deep rterm3)
      | IIL_TERM (rterm1, rterm2, rterm3) => make_isinl_term    (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2) (rterm2TERM_deep rterm3)
      | IIN_TERM (rterm1, rterm2, rterm3) => make_isint_term    (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2) (rterm2TERM_deep rterm3)
      | ILA_TERM (rterm1, rterm2, rterm3) => make_islambda_term (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2) (rterm2TERM_deep rterm3)
      | IAT_TERM (rterm1, rterm2, rterm3) => make_isatom2_term  (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2) (rterm2TERM_deep rterm3)
      | CBV_TERM (a, x, f) => make_callbyvalue_term    (rterm2TERM_deep a) (x, rterm2TERM_deep f)
      | CBA_TERM (a, x, f) => make_callbyvalueall_term (rterm2TERM_deep a) (x, rterm2TERM_deep f)
      | FUN_TERM (a, x, f) => make_function_term       (rterm2TERM_deep a) (x, rterm2TERM_deep f)
      | PRD_TERM (a, x, f) => make_product_term        (rterm2TERM_deep a) (x, rterm2TERM_deep f)
      | TUN_TERM (a, x, f) => make_tunion_term         (rterm2TERM_deep a) (x, rterm2TERM_deep f)
      | SET_TERM (a, x, f) => make_set_term            (rterm2TERM_deep a) (x, rterm2TERM_deep f)
      | LES_TERM (a, b, rterm1, rterm2) => make_less_term   (rterm2TERM_deep a) (rterm2TERM_deep b) (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2)
      | IEQ_TERM (a, b, rterm1, rterm2) => make_int_eq_term (rterm2TERM_deep a) (rterm2TERM_deep b) (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2)
      | SPR_TERM (p, var1, var2, rterm) => make_spread_term (rterm2TERM_deep p) (var1, var2, rterm2TERM_deep rterm)
      | AEQ_TERM (n, a, b, rterm1, rterm2) => make_atom_eq_term n (rterm2TERM_deep a) (rterm2TERM_deep b) (rterm2TERM_deep rterm1) (rterm2TERM_deep rterm2)
      | DEC_TERM (dec, var1, rterm1, var2, rterm2) => make_decide_term (rterm2TERM_deep dec) (var1, rterm2TERM_deep rterm1) (var2, rterm2TERM_deep rterm2)
      | IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase) => make_ind_term (rterm2TERM_deep i) (x, rd, rterm2TERM_deep downcase) (rterm2TERM_deep basecase) (y, ru, rterm2TERM_deep upcase)
      | CLO_TERM _ => raise Fail "term2TERM_deep:CLO_TERM"

and rterm2TERM_deep rterm = mk_rterm (term2TERM_deep (rterm2term rterm))


(* ------ CLOSURES ------ *)

val em_env = ENV MAP.empty

fun is_em_env (ENV env) = MAP.isEmpty env

fun is_ct (CLO_TERM _) = true
  | is_ct _ = false

fun get_ct (CLO_TERM clos) = clos
  | get_ct _ = raise Fail "get_ct"

fun dest_clos (rterm, env) = (rterm2term rterm, env)

fun dest_ct (CLO_TERM clos) = dest_clos clos
  | dest_ct _ = raise Fail "dest_ct"

fun dest_rclos (rterm, env) = (rterm, env)

fun dest_rct (CLO_TERM clos) = dest_rclos clos
  | dest_rct _ = raise Fail "dest_rct"

fun lookup_clos (ENV env) var = MAP.find (env, var)

fun lookup env var =
    case lookup_clos env var of
	SOME (_,_,termref) =>
	(case #1 (!termref) of
	     CLO_TERM (rterm, e) => SOME (rterm2term rterm, e)
	   | term => SOME (term, em_env))
      | NONE => NONE

fun pull_out_envs (term, env) =
    if is_ct term
    then pull_out_envs (dest_ct term)
    else (term, env)

(* clos -> term *)
fun mk_rct (term as CLO_TERM clos, env) = mk_rct (dest_clos clos)
  | mk_rct (term, env) =
    if is_em_env env
    then term
    else CLO_TERM (mk_rterm term, env)

(* rclos -> term *)
fun mk_ct (rterm, env) =
    if is_em_env env
    then rterm2term rterm
    else CLO_TERM (rterm, env)

(* rclos -> rterm *)
fun mk_rctr (rterm, env) =
    if is_em_env env
    then rterm
    else mk_rterm (CLO_TERM (rterm, env))

fun compute_to_inj_env term env =
    case term of
	INL_TERM rterm => INL_TERM (mk_rctr (rterm, env))
      | INR_TERM rterm => INR_TERM (mk_rctr (rterm, env))
      | CLO_TERM (rterm, env) =>
	let val t = rterm2term rterm
	in compute_to_inj_env t env
	end
      | VAR_TERM var =>
	(case lookup env (dest_nuprl_var var) of
	     SOME (t, e) => compute_to_inj_env t e
	   | NONE => raise Fail "compute_to_inj_env:free_variable")
      | _ => raise Fail "compute_to_inj_env:not_an_injection"

fun compute_to_inj term = compute_to_inj_env term em_env

fun get_bterms (TERM (_, bterms)) = bterms
  | get_bterms AXM_TERM = []
  | get_bterms BOT_TERM = []
  | get_bterms INT_TERM = []
  | get_bterms VOI_TERM = []
  | get_bterms DUM_TERM = []
  | get_bterms (ATM_TERM _) = []
  | get_bterms (TOK_TERM _) = []
  | get_bterms (NAT_TERM _) = []
  | get_bterms (VAR_TERM _) = []
  | get_bterms (INL_TERM rterm) = [B_TERM ([], rterm)]
  | get_bterms (INR_TERM rterm) = [B_TERM ([], rterm)]
  | get_bterms (FIX_TERM rterm) = [B_TERM ([], rterm)]
  | get_bterms (MIN_TERM rterm) = [B_TERM ([], rterm)]
  | get_bterms (LAM_TERM (var, rterm)) = [B_TERM ([var], rterm)]
  | get_bterms (REC_TERM (var, rterm)) = [B_TERM ([var], rterm)]
  | get_bterms (WAI_TERM (rterm1, rterm2)) = [B_TERM ([], rterm1), B_TERM ([], rterm2)]
  | get_bterms (APP_TERM (rterm1, rterm2)) = [B_TERM ([], rterm1), B_TERM ([], rterm2)]
  | get_bterms (PAI_TERM (rterm1, rterm2)) = [B_TERM ([], rterm1), B_TERM ([], rterm2)]
  | get_bterms (ADD_TERM (rterm1, rterm2)) = [B_TERM ([], rterm1), B_TERM ([], rterm2)]
  | get_bterms (SUB_TERM (rterm1, rterm2)) = [B_TERM ([], rterm1), B_TERM ([], rterm2)]
  | get_bterms (MUL_TERM (rterm1, rterm2)) = [B_TERM ([], rterm1), B_TERM ([], rterm2)]
  | get_bterms (DIV_TERM (rterm1, rterm2)) = [B_TERM ([], rterm1), B_TERM ([], rterm2)]
  | get_bterms (REM_TERM (rterm1, rterm2)) = [B_TERM ([], rterm1), B_TERM ([], rterm2)]
  | get_bterms (EQT_TERM (rterm1, rterm2)) = [B_TERM ([], rterm1), B_TERM ([], rterm2)]
  | get_bterms (UNI_TERM (rterm1, rterm2)) = [B_TERM ([], rterm1), B_TERM ([], rterm2)]
  | get_bterms (EQU_TERM (rterm1, rterm2, rterm3)) = [B_TERM ([], rterm1), B_TERM ([], rterm2), B_TERM ([], rterm3)]
  | get_bterms (IAX_TERM (rterm1, rterm2, rterm3)) = [B_TERM ([], rterm1), B_TERM ([], rterm2), B_TERM ([], rterm3)]
  | get_bterms (IPA_TERM (rterm1, rterm2, rterm3)) = [B_TERM ([], rterm1), B_TERM ([], rterm2), B_TERM ([], rterm3)]
  | get_bterms (IIR_TERM (rterm1, rterm2, rterm3)) = [B_TERM ([], rterm1), B_TERM ([], rterm2), B_TERM ([], rterm3)]
  | get_bterms (IIL_TERM (rterm1, rterm2, rterm3)) = [B_TERM ([], rterm1), B_TERM ([], rterm2), B_TERM ([], rterm3)]
  | get_bterms (IIN_TERM (rterm1, rterm2, rterm3)) = [B_TERM ([], rterm1), B_TERM ([], rterm2), B_TERM ([], rterm3)]
  | get_bterms (ILA_TERM (rterm1, rterm2, rterm3)) = [B_TERM ([], rterm1), B_TERM ([], rterm2), B_TERM ([], rterm3)]
  | get_bterms (IAT_TERM (rterm1, rterm2, rterm3)) = [B_TERM ([], rterm1), B_TERM ([], rterm2), B_TERM ([], rterm3)]
  | get_bterms (CBV_TERM (a, x, f)) = [B_TERM ([], a), B_TERM ([x], f)]
  | get_bterms (CBA_TERM (a, x, f)) = [B_TERM ([], a), B_TERM ([x], f)]
  | get_bterms (FUN_TERM (a, x, f)) = [B_TERM ([], a), B_TERM ([x], f)]
  | get_bterms (PRD_TERM (a, x, f)) = [B_TERM ([], a), B_TERM ([x], f)]
  | get_bterms (TUN_TERM (a, x, f)) = [B_TERM ([], a), B_TERM ([x], f)]
  | get_bterms (SET_TERM (a, x, f)) = [B_TERM ([], a), B_TERM ([x], f)]
  | get_bterms (LES_TERM (a, b, rterm1, rterm2)) = [B_TERM ([], a), B_TERM ([], b), B_TERM ([], rterm1), B_TERM ([], rterm2)]
  | get_bterms (IEQ_TERM (a, b, rterm1, rterm2)) = [B_TERM ([], a), B_TERM ([], b), B_TERM ([], rterm1), B_TERM ([], rterm2)]
  | get_bterms (SPR_TERM (pair, var1, var2, rterm)) = [B_TERM ([], pair), B_TERM ([var1, var2], rterm)]
  | get_bterms (AEQ_TERM (n, a, b, rterm1, rterm2)) = [B_TERM ([], a), B_TERM ([], b), B_TERM ([], rterm1), B_TERM ([], rterm2)]
  | get_bterms (DEC_TERM (dec, var1, rterm1, var2, rterm2)) = [B_TERM ([], dec), B_TERM ([var1], rterm1), B_TERM ([var2], rterm2)]
  | get_bterms (IND_TERM (i, x, vd, downcase, basecase, y, vu, upcase)) = [B_TERM ([], i), B_TERM ([x, vd], downcase), B_TERM ([], basecase), B_TERM ([y, vu], upcase)]
  | get_bterms (CLO_TERM clos) = raise Fail "get_brterms"

(* -- free variables -- *)
fun domain (ENV m) =
    MAP.foldri (fn (i, _, set) => VARS.add (set, i))
	       VARS.empty
	       m

val empty_vars = VARS.empty

fun fo_free_vars bounds (term as TERM (operator, bterms)) =
    if is_nuprl_variable_term term
    then let val v = dest_variable term
	 in if VARS.member (bounds, v)
	    then VARS.empty
	    else VARS.singleton v
	 end
    else List.foldr (fn (bterm, vars) =>
			VARS.union (vars, fo_free_vars_bterm bounds bterm))
		    empty_vars
		    bterms
  | fo_free_vars bounds (term as AXM_TERM) = VARS.empty
  | fo_free_vars bounds (term as BOT_TERM) = VARS.empty
  | fo_free_vars bounds (term as INT_TERM) = VARS.empty
  | fo_free_vars bounds (term as VOI_TERM) = VARS.empty
  | fo_free_vars bounds (term as DUM_TERM) = VARS.empty
  | fo_free_vars bounds (term as ATM_TERM _) = VARS.empty
  | fo_free_vars bounds (term as TOK_TERM _) = VARS.empty
  | fo_free_vars bounds (term as NAT_TERM _) = VARS.empty
  | fo_free_vars bounds (term as VAR_TERM var) =
    let val v = dest_nuprl_var var
    in if VARS.member (bounds, v)
       then VARS.empty
       else VARS.singleton v
    end
  | fo_free_vars bounds (term as INL_TERM rterm) = fo_free_vars_rterm bounds rterm
  | fo_free_vars bounds (term as INR_TERM rterm) = fo_free_vars_rterm bounds rterm
  | fo_free_vars bounds (term as FIX_TERM rterm) = fo_free_vars_rterm bounds rterm
  | fo_free_vars bounds (term as MIN_TERM rterm) = fo_free_vars_rterm bounds rterm
  | fo_free_vars bounds (term as LAM_TERM (var, rterm)) = fo_free_vars_rterm (VARS.add (bounds, dest_nuprl_var var)) rterm
  | fo_free_vars bounds (term as REC_TERM (var, rterm)) = fo_free_vars_rterm (VARS.add (bounds, dest_nuprl_var var)) rterm
  | fo_free_vars bounds (term as WAI_TERM (rterm1, rterm2)) = VARS.union (fo_free_vars_rterm bounds rterm1, fo_free_vars_rterm bounds rterm2)
  | fo_free_vars bounds (term as APP_TERM (rterm1, rterm2)) = VARS.union (fo_free_vars_rterm bounds rterm1, fo_free_vars_rterm bounds rterm2)
  | fo_free_vars bounds (term as PAI_TERM (rterm1, rterm2)) = VARS.union (fo_free_vars_rterm bounds rterm1, fo_free_vars_rterm bounds rterm2)
  | fo_free_vars bounds (term as ADD_TERM (rterm1, rterm2)) = VARS.union (fo_free_vars_rterm bounds rterm1, fo_free_vars_rterm bounds rterm2)
  | fo_free_vars bounds (term as SUB_TERM (rterm1, rterm2)) = VARS.union (fo_free_vars_rterm bounds rterm1, fo_free_vars_rterm bounds rterm2)
  | fo_free_vars bounds (term as MUL_TERM (rterm1, rterm2)) = VARS.union (fo_free_vars_rterm bounds rterm1, fo_free_vars_rterm bounds rterm2)
  | fo_free_vars bounds (term as DIV_TERM (rterm1, rterm2)) = VARS.union (fo_free_vars_rterm bounds rterm1, fo_free_vars_rterm bounds rterm2)
  | fo_free_vars bounds (term as REM_TERM (rterm1, rterm2)) = VARS.union (fo_free_vars_rterm bounds rterm1, fo_free_vars_rterm bounds rterm2)
  | fo_free_vars bounds (term as EQT_TERM (rterm1, rterm2)) = VARS.union (fo_free_vars_rterm bounds rterm1, fo_free_vars_rterm bounds rterm2)
  | fo_free_vars bounds (term as UNI_TERM (rterm1, rterm2)) = VARS.union (fo_free_vars_rterm bounds rterm1, fo_free_vars_rterm bounds rterm2)
  | fo_free_vars bounds (term as EQU_TERM (rterm1, rterm2, rterm3)) =
    VARS.union (fo_free_vars_rterm bounds rterm1,
		VARS.union (fo_free_vars_rterm bounds rterm2,
			    fo_free_vars_rterm bounds rterm3))
  | fo_free_vars bounds (term as IAX_TERM (rterm1, rterm2, rterm3)) =
    VARS.union (fo_free_vars_rterm bounds rterm1,
		VARS.union (fo_free_vars_rterm bounds rterm2,
			    fo_free_vars_rterm bounds rterm3))
  | fo_free_vars bounds (term as IPA_TERM (rterm1, rterm2, rterm3)) =
    VARS.union (fo_free_vars_rterm bounds rterm1,
		VARS.union (fo_free_vars_rterm bounds rterm2,
			    fo_free_vars_rterm bounds rterm3))
  | fo_free_vars bounds (term as IIR_TERM (rterm1, rterm2, rterm3)) =
    VARS.union (fo_free_vars_rterm bounds rterm1,
		VARS.union (fo_free_vars_rterm bounds rterm2,
			    fo_free_vars_rterm bounds rterm3))
  | fo_free_vars bounds (term as IIL_TERM (rterm1, rterm2, rterm3)) =
    VARS.union (fo_free_vars_rterm bounds rterm1,
		VARS.union (fo_free_vars_rterm bounds rterm2,
			    fo_free_vars_rterm bounds rterm3))
  | fo_free_vars bounds (term as IIN_TERM (rterm1, rterm2, rterm3)) =
    VARS.union (fo_free_vars_rterm bounds rterm1,
		VARS.union (fo_free_vars_rterm bounds rterm2,
			    fo_free_vars_rterm bounds rterm3))
  | fo_free_vars bounds (term as ILA_TERM (rterm1, rterm2, rterm3)) =
    VARS.union (fo_free_vars_rterm bounds rterm1,
		VARS.union (fo_free_vars_rterm bounds rterm2,
			    fo_free_vars_rterm bounds rterm3))
  | fo_free_vars bounds (term as IAT_TERM (rterm1, rterm2, rterm3)) =
    VARS.union (fo_free_vars_rterm bounds rterm1,
		VARS.union (fo_free_vars_rterm bounds rterm2,
			    fo_free_vars_rterm bounds rterm3))
  | fo_free_vars bounds (term as CBV_TERM (a, x, f)) =
    VARS.union (fo_free_vars_rterm bounds a,
		fo_free_vars_rterm (VARS.add (bounds, dest_nuprl_var x)) f)
  | fo_free_vars bounds (term as CBA_TERM (a, x, f)) =
    VARS.union (fo_free_vars_rterm bounds a,
		fo_free_vars_rterm (VARS.add (bounds, dest_nuprl_var x)) f)
  | fo_free_vars bounds (term as FUN_TERM (a, x, f)) =
    VARS.union (fo_free_vars_rterm bounds a,
		fo_free_vars_rterm (VARS.add (bounds, dest_nuprl_var x)) f)
  | fo_free_vars bounds (term as PRD_TERM (a, x, f)) =
    VARS.union (fo_free_vars_rterm bounds a,
		fo_free_vars_rterm (VARS.add (bounds, dest_nuprl_var x)) f)
  | fo_free_vars bounds (term as TUN_TERM (a, x, f)) =
    VARS.union (fo_free_vars_rterm bounds a,
		fo_free_vars_rterm (VARS.add (bounds, dest_nuprl_var x)) f)
  | fo_free_vars bounds (term as SET_TERM (a, x, f)) =
    VARS.union (fo_free_vars_rterm bounds a,
		fo_free_vars_rterm (VARS.add (bounds, dest_nuprl_var x)) f)
  | fo_free_vars bounds (term as IEQ_TERM (a, b, rterm1, rterm2)) =
    VARS.union (fo_free_vars_rterm bounds a,
		VARS.union (fo_free_vars_rterm bounds b,
			    VARS.union (fo_free_vars_rterm bounds rterm1,
					fo_free_vars_rterm bounds rterm2)))
  | fo_free_vars bounds (term as LES_TERM (a, b, rterm1, rterm2)) =
    VARS.union (fo_free_vars_rterm bounds a,
		VARS.union (fo_free_vars_rterm bounds b,
			    VARS.union (fo_free_vars_rterm bounds rterm1,
					fo_free_vars_rterm bounds rterm2)))
  | fo_free_vars bounds (term as SPR_TERM (pair, var1, var2, rterm)) =
    VARS.union (fo_free_vars_rterm bounds pair,
		fo_free_vars_rterm
		    (VARS.add
			 (VARS.add (bounds, dest_nuprl_var var1),
			  dest_nuprl_var var2))
		    rterm)
  | fo_free_vars bounds (term as AEQ_TERM (n, a, b, rterm1, rterm2)) =
    VARS.union (fo_free_vars_rterm bounds a,
		VARS.union (fo_free_vars_rterm bounds b,
			    VARS.union (fo_free_vars_rterm bounds rterm1,
					fo_free_vars_rterm bounds rterm2)))
  | fo_free_vars bounds (term as DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    VARS.union (fo_free_vars_rterm bounds dec,
		VARS.union (fo_free_vars_rterm
				(VARS.add (bounds, dest_nuprl_var var1))
				rterm1,
			    fo_free_vars_rterm
				(VARS.add (bounds, dest_nuprl_var var2))
				rterm2))
  | fo_free_vars bounds (term as IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) =
    VARS.union (fo_free_vars_rterm bounds i,
		VARS.union (fo_free_vars_rterm
				(VARS.add (VARS.add (bounds, dest_nuprl_var x), dest_nuprl_var rd))
				downcase,
			    VARS.union
				(fo_free_vars_rterm bounds basecase,
				 fo_free_vars_rterm
				     (VARS.add (VARS.add (bounds, dest_nuprl_var y), dest_nuprl_var ru))
				     upcase)))
  | fo_free_vars bounds (term as CLO_TERM clos) = fo_free_vars_clos bounds clos

and fo_free_vars_clos bounds (rterm, env) =
    VARS.union (fo_free_vars_rterm (VARS.union (bounds, domain env)) rterm,
		fo_free_vars_env bounds env)

and fo_free_vars_env bounds (ENV env) =
    MAP.foldr
	(fn ((_,_,termref), vars) =>
	    VARS.union (vars, fo_free_vars bounds (#1 (!termref))))
	empty_vars
	env

and fo_free_vars_rterm bounds rterm = fo_free_vars bounds (rterm2term rterm)

and fo_free_vars_bterm bounds (B_TERM (vars, rterm)) =
    let val bounds' = nvars2set bounds vars
    in fo_free_vars_rterm bounds' rterm
    end

val free_vars = fo_free_vars VARS.empty

(* -- free variables - mapping from variables to occurence-- *)
val empty_vars_map = MAP.empty

fun free_vars_union_map m1 m2 = MAP.unionWith (fn (n, m) => n + m) (m1, m2)

fun fo_free_vars_map bounds (term as TERM (operator, bterms)) =
    if is_nuprl_variable_term term
    then let val v = dest_variable term
	 in if VARS.member (bounds, v)
	    then empty_vars_map
	    else MAP.singleton (v, 1)
	 end
    else List.foldr (fn (bterm, vars) =>
			free_vars_union_map
			    vars
			    (fo_free_vars_bterm_map bounds bterm))
		    empty_vars_map
		    bterms
  | fo_free_vars_map bounds (term as AXM_TERM) = empty_vars_map
  | fo_free_vars_map bounds (term as BOT_TERM) = empty_vars_map
  | fo_free_vars_map bounds (term as INT_TERM) = empty_vars_map
  | fo_free_vars_map bounds (term as VOI_TERM) = empty_vars_map
  | fo_free_vars_map bounds (term as DUM_TERM) = empty_vars_map
  | fo_free_vars_map bounds (term as ATM_TERM _) = empty_vars_map
  | fo_free_vars_map bounds (term as TOK_TERM _) = empty_vars_map
  | fo_free_vars_map bounds (term as NAT_TERM _) = empty_vars_map
  | fo_free_vars_map bounds (term as VAR_TERM var) =
    let val v = dest_nuprl_var var
    in if VARS.member (bounds, v)
       then empty_vars_map
       else MAP.singleton (v, 1)
    end
  | fo_free_vars_map bounds (term as INL_TERM rterm) = fo_free_vars_rterm_map bounds rterm
  | fo_free_vars_map bounds (term as INR_TERM rterm) = fo_free_vars_rterm_map bounds rterm
  | fo_free_vars_map bounds (term as FIX_TERM rterm) = fo_free_vars_rterm_map bounds rterm
  | fo_free_vars_map bounds (term as MIN_TERM rterm) = fo_free_vars_rterm_map bounds rterm
  | fo_free_vars_map bounds (term as LAM_TERM (var, rterm)) = fo_free_vars_rterm_map (VARS.add (bounds, dest_nuprl_var var)) rterm
  | fo_free_vars_map bounds (term as REC_TERM (var, rterm)) = fo_free_vars_rterm_map (VARS.add (bounds, dest_nuprl_var var)) rterm
  | fo_free_vars_map bounds (term as WAI_TERM (rterm1, rterm2)) = free_vars_union_map (fo_free_vars_rterm_map bounds rterm1) (fo_free_vars_rterm_map bounds rterm2)
  | fo_free_vars_map bounds (term as APP_TERM (rterm1, rterm2)) = free_vars_union_map (fo_free_vars_rterm_map bounds rterm1) (fo_free_vars_rterm_map bounds rterm2)
  | fo_free_vars_map bounds (term as PAI_TERM (rterm1, rterm2)) = free_vars_union_map (fo_free_vars_rterm_map bounds rterm1) (fo_free_vars_rterm_map bounds rterm2)
  | fo_free_vars_map bounds (term as ADD_TERM (rterm1, rterm2)) = free_vars_union_map (fo_free_vars_rterm_map bounds rterm1) (fo_free_vars_rterm_map bounds rterm2)
  | fo_free_vars_map bounds (term as SUB_TERM (rterm1, rterm2)) = free_vars_union_map (fo_free_vars_rterm_map bounds rterm1) (fo_free_vars_rterm_map bounds rterm2)
  | fo_free_vars_map bounds (term as MUL_TERM (rterm1, rterm2)) = free_vars_union_map (fo_free_vars_rterm_map bounds rterm1) (fo_free_vars_rterm_map bounds rterm2)
  | fo_free_vars_map bounds (term as DIV_TERM (rterm1, rterm2)) = free_vars_union_map (fo_free_vars_rterm_map bounds rterm1) (fo_free_vars_rterm_map bounds rterm2)
  | fo_free_vars_map bounds (term as REM_TERM (rterm1, rterm2)) = free_vars_union_map (fo_free_vars_rterm_map bounds rterm1) (fo_free_vars_rterm_map bounds rterm2)
  | fo_free_vars_map bounds (term as EQT_TERM (rterm1, rterm2)) = free_vars_union_map (fo_free_vars_rterm_map bounds rterm1) (fo_free_vars_rterm_map bounds rterm2)
  | fo_free_vars_map bounds (term as UNI_TERM (rterm1, rterm2)) = free_vars_union_map (fo_free_vars_rterm_map bounds rterm1) (fo_free_vars_rterm_map bounds rterm2)
  | fo_free_vars_map bounds (term as EQU_TERM (rterm1, rterm2, rterm3)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds rterm1)
	(free_vars_union_map
	     (fo_free_vars_rterm_map bounds rterm2)
	     (fo_free_vars_rterm_map bounds rterm3))
  | fo_free_vars_map bounds (term as IAX_TERM (rterm1, rterm2, rterm3)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds rterm1)
	(free_vars_union_map
	     (fo_free_vars_rterm_map bounds rterm2)
	     (fo_free_vars_rterm_map bounds rterm3))
  | fo_free_vars_map bounds (term as IPA_TERM (rterm1, rterm2, rterm3)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds rterm1)
	(free_vars_union_map
	     (fo_free_vars_rterm_map bounds rterm2)
	     (fo_free_vars_rterm_map bounds rterm3))
  | fo_free_vars_map bounds (term as IIR_TERM (rterm1, rterm2, rterm3)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds rterm1)
	(free_vars_union_map
	     (fo_free_vars_rterm_map bounds rterm2)
	     (fo_free_vars_rterm_map bounds rterm3))
  | fo_free_vars_map bounds (term as IIL_TERM (rterm1, rterm2, rterm3)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds rterm1)
	(free_vars_union_map
	     (fo_free_vars_rterm_map bounds rterm2)
	     (fo_free_vars_rterm_map bounds rterm3))
  | fo_free_vars_map bounds (term as IIN_TERM (rterm1, rterm2, rterm3)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds rterm1)
	(free_vars_union_map
	     (fo_free_vars_rterm_map bounds rterm2)
	     (fo_free_vars_rterm_map bounds rterm3))
  | fo_free_vars_map bounds (term as ILA_TERM (rterm1, rterm2, rterm3)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds rterm1)
	(free_vars_union_map
	     (fo_free_vars_rterm_map bounds rterm2)
	     (fo_free_vars_rterm_map bounds rterm3))
  | fo_free_vars_map bounds (term as IAT_TERM (rterm1, rterm2, rterm3)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds rterm1)
	(free_vars_union_map
	     (fo_free_vars_rterm_map bounds rterm2)
	     (fo_free_vars_rterm_map bounds rterm3))
  | fo_free_vars_map bounds (term as CBV_TERM (a, x, f)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds a)
	(fo_free_vars_rterm_map (VARS.add (bounds, dest_nuprl_var x)) f)
  | fo_free_vars_map bounds (term as CBA_TERM (a, x, f)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds a)
	(fo_free_vars_rterm_map (VARS.add (bounds, dest_nuprl_var x)) f)
  | fo_free_vars_map bounds (term as FUN_TERM (a, x, f)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds a)
	(fo_free_vars_rterm_map (VARS.add (bounds, dest_nuprl_var x)) f)
  | fo_free_vars_map bounds (term as PRD_TERM (a, x, f)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds a)
	(fo_free_vars_rterm_map (VARS.add (bounds, dest_nuprl_var x)) f)
  | fo_free_vars_map bounds (term as TUN_TERM (a, x, f)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds a)
	(fo_free_vars_rterm_map (VARS.add (bounds, dest_nuprl_var x)) f)
  | fo_free_vars_map bounds (term as SET_TERM (a, x, f)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds a)
	(fo_free_vars_rterm_map (VARS.add (bounds, dest_nuprl_var x)) f)
  | fo_free_vars_map bounds (term as IEQ_TERM (a, b, rterm1, rterm2)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds a)
	(free_vars_union_map
	     (fo_free_vars_rterm_map bounds b)
	     (free_vars_union_map
		  (fo_free_vars_rterm_map bounds rterm1)
		  (fo_free_vars_rterm_map bounds rterm2)))
  | fo_free_vars_map bounds (term as LES_TERM (a, b, rterm1, rterm2)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds a)
	(free_vars_union_map
	     (fo_free_vars_rterm_map bounds b)
	     (free_vars_union_map
		  (fo_free_vars_rterm_map bounds rterm1)
		  (fo_free_vars_rterm_map bounds rterm2)))
  | fo_free_vars_map bounds (term as SPR_TERM (pair, var1, var2, rterm)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds pair)
	(fo_free_vars_rterm_map
	     (VARS.add
		  (VARS.add (bounds, dest_nuprl_var var1),
		   dest_nuprl_var var2))
	     rterm)
  | fo_free_vars_map bounds (term as AEQ_TERM (n, a, b, rterm1, rterm2)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds a)
	(free_vars_union_map
	     (fo_free_vars_rterm_map bounds b)
	     (free_vars_union_map
		  (fo_free_vars_rterm_map bounds rterm1)
		  (fo_free_vars_rterm_map bounds rterm2)))
  | fo_free_vars_map bounds (term as DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds dec)
	(free_vars_union_map
	     (fo_free_vars_rterm_map
		  (VARS.add (bounds, dest_nuprl_var var1))
		  rterm1)
	     (fo_free_vars_rterm_map
		  (VARS.add (bounds, dest_nuprl_var var2))
		  rterm2))
  | fo_free_vars_map bounds (term as IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) =
    free_vars_union_map
	(fo_free_vars_rterm_map bounds i)
	(free_vars_union_map
	     (fo_free_vars_rterm_map
		  (VARS.add (VARS.add (bounds, dest_nuprl_var x), dest_nuprl_var rd))
		  downcase)
	     (free_vars_union_map
		  (fo_free_vars_rterm_map bounds basecase)
		  (fo_free_vars_rterm_map
		       (VARS.add (VARS.add (bounds, dest_nuprl_var y), dest_nuprl_var ru))
		       upcase)))
  | fo_free_vars_map bounds (term as CLO_TERM clos) = fo_free_vars_clos_map bounds clos

and fo_free_vars_clos_map bounds (rterm, env) =
    free_vars_union_map
	(fo_free_vars_rterm_map (VARS.union (bounds, domain env)) rterm)
	(fo_free_vars_env_map bounds env)

and fo_free_vars_env_map bounds (ENV env) =
    MAP.foldr
	(fn ((_,_,termref), vars) =>
	    free_vars_union_map
		vars
		(fo_free_vars_map bounds (#1 (!termref))))
	empty_vars_map
	env

and fo_free_vars_rterm_map bounds rterm = fo_free_vars_map bounds (rterm2term rterm)

and fo_free_vars_bterm_map bounds (B_TERM (vars, rterm)) =
    let val bounds' = nvars2set bounds vars
    in fo_free_vars_rterm_map bounds' rterm
    end

val free_vars_map = fo_free_vars_map VARS.empty

fun find_free_vars_map m x =
    case MAP.find (m, x) of
	SOME x => x
      | NONE => 0

(* -- simple closure: (v, [v -> t]) *)
fun is_simple_closure (rterm, env) =
    let val term = rterm2term rterm
    in is_nuprl_variable_term term
       andalso
       Option.isSome (lookup_clos env (dest_variable term))
    end

(* shallow term: op(v1,..,vn) *)
fun is_shallow_term (TERM (_, bterms)) =
    foldr (fn (B_TERM ([], rterm), SOME vars) =>
	      let val t = rterm2term rterm
	      in if is_nuprl_variable_term t
		 then SOME (VARS.add (vars, dest_variable t))
		 else NONE
	      end
	    | _ => NONE)
	  (SOME VARS.empty)
	  bterms
  | is_shallow_term _ = NONE

fun prune_clos (term, ENV env) =
    let (*val env' =
	    case is_shallow_term term of
		SOME vars =>
		MAP.filteri (fn (v, _) => VARS.member (vars, v)) env
	      | NONE => env*)
	val frees = free_vars term
	val env' = MAP.filteri (fn (v, _) => VARS.member (frees, v)) env
    in mk_rct (term, ENV env')
    end

(*val prune_clos = mk_rclos*)

(*
fun remove_from_env (env as ENV m) var =
    ENV (#1 (MAP.remove (m, var))) handle _ => env

fun sp_add2env  (env as ENV m) var term =
    ENV (MAP.insert (m, var, mk_rterm term))
*)

(* This is call _evall_ because we know that rt has already been fully evaluated,
 * which is why we insert the term tagged with `true` in the environment. *)
fun new_evall_add2env (env as ENV m) rt e =
    let val dom = domain env
	fun aux n =
	    if VARS.member (dom, "x" ^ Int.toString n)
	    then aux (n + 1)
	    else n
	val var     = "x" ^ Int.toString (aux 0)
	val nvar    = mk_new_nuprl_var var
	val termref = ref (mk_ct (rt, e), false)
	val m'      = MAP.insert (m, var, (var, true, termref))
    in (ENV m', nvar)
    end

fun filter_env term (ENV m) =
    let val frees = free_vars term
    in ENV (MAP.filteri (fn (v, _) => VARS.member (frees, v)) m)
    end

fun add2env_one (ENV env) (v,b,t,e as ENV m) =
    if v = ""
    then ENV env
    else let val term = rterm2term t
	 in if is_nuprl_variable_term term
	    then let val tv = dest_variable term
		 in case lookup_clos e tv of
			SOME c => (*if is_simple_closure c
				    then raise Fail "simple_closure"
				    else*) ENV (MAP.insert (env, v, c))
		      | NONE => raise Fail ("add2env_one(" ^ tv ^ ")") (*mk_rclos (t, e)*)
		 end
	    else if is_ct term
	    then let val (t',e') = dest_rct term
		 in add2env_one (ENV env) (v,b,t',e')
		 end
	    else if null (get_bterms term)
	    then ENV (MAP.insert (env, v, (v,b,ref (term, false))))
	    else let val termref = ref (mk_ct (t, filter_env term e), false)
		 in ENV (MAP.insert (env, v, (v,b,termref)))
		 end
	 end

fun add2env env [] = env
  | add2env env ((nv,b,t,e) :: xs) =
    add2env (add2env_one env (dest_nuprl_var nv, b, t, e)) xs

fun close term env =
    let fun aux term bounds env =
	    case term of
		CLO_TERM clos =>
		let val (t,e) = dest_clos clos
		    val bounds' = VARS.difference (bounds, domain e)
		in aux t bounds' e
		end
	      | AXM_TERM   => term
	      | BOT_TERM   => term
	      | INT_TERM   => term
	      | VOI_TERM   => term
	      | DUM_TERM   => term
	      | ATM_TERM _ => term
	      | TOK_TERM _ => term
	      | NAT_TERM _ => term
	      | VAR_TERM var =>
		let val v = dest_nuprl_var var
		in if VARS.member (bounds, v)
		   then term
		   else case lookup env v of
			    SOME (t,e) => aux t (VARS.difference (bounds, domain e)) e
			  | NONE => term
		end
	      | INL_TERM rterm =>
		let val t = aux (rterm2term rterm) bounds env
		in INL_TERM (mk_rterm t)
		end
	      | INR_TERM rterm =>
		let val t = aux (rterm2term rterm) bounds env
		in INR_TERM (mk_rterm t)
		end
	      | FIX_TERM rterm =>
		let val t = aux (rterm2term rterm) bounds env
		in FIX_TERM (mk_rterm t)
		end
	      | MIN_TERM rterm =>
		let val t = aux (rterm2term rterm) bounds env
		in MIN_TERM (mk_rterm t)
		end
	      | LAM_TERM (var, rterm) =>
		let val bs = VARS.add (bounds, dest_nuprl_var var)
		    val t  = aux (rterm2term rterm) bs env
		in LAM_TERM (var, mk_rterm t)
		end
	      | REC_TERM (var, rterm) =>
		let val bs = VARS.add (bounds, dest_nuprl_var var)
		    val t  = aux (rterm2term rterm) bs env
		in REC_TERM (var, mk_rterm t)
		end
	      | WAI_TERM (rterm1, rterm2) =>
		let val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in WAI_TERM (mk_rterm t1, mk_rterm t2)
		end
	      | APP_TERM (rterm1, rterm2) =>
		let val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in APP_TERM (mk_rterm t1, mk_rterm t2)
		end
	      | PAI_TERM (rterm1, rterm2) =>
		let val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in PAI_TERM (mk_rterm t1, mk_rterm t2)
		end
	      | ADD_TERM (rterm1, rterm2) =>
		let val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in ADD_TERM (mk_rterm t1, mk_rterm t2)
		end
	      | SUB_TERM (rterm1, rterm2) =>
		let val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in SUB_TERM (mk_rterm t1, mk_rterm t2)
		end
	      | MUL_TERM (rterm1, rterm2) =>
		let val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in MUL_TERM (mk_rterm t1, mk_rterm t2)
		end
	      | DIV_TERM (rterm1, rterm2) =>
		let val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in DIV_TERM (mk_rterm t1, mk_rterm t2)
		end
	      | REM_TERM (rterm1, rterm2) =>
		let val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in REM_TERM (mk_rterm t1, mk_rterm t2)
		end
	      | EQT_TERM (rterm1, rterm2) =>
		let val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in EQT_TERM (mk_rterm t1, mk_rterm t2)
		end
	      | UNI_TERM (rterm1, rterm2) =>
		let val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in UNI_TERM (mk_rterm t1, mk_rterm t2)
		end
	      | EQU_TERM (a, rterm1, rterm2) =>
		let val a  = aux (rterm2term a)      bounds env
		    val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in EQU_TERM (mk_rterm a, mk_rterm t1, mk_rterm t2)
		end
	      | IAX_TERM (a, rterm1, rterm2) =>
		let val a  = aux (rterm2term a)      bounds env
		    val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in IAX_TERM (mk_rterm a, mk_rterm t1, mk_rterm t2)
		end
	      | IPA_TERM (a, rterm1, rterm2) =>
		let val a  = aux (rterm2term a)      bounds env
		    val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in IPA_TERM (mk_rterm a, mk_rterm t1, mk_rterm t2)
		end
	      | IIR_TERM (a, rterm1, rterm2) =>
		let val a  = aux (rterm2term a)      bounds env
		    val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in IIR_TERM (mk_rterm a, mk_rterm t1, mk_rterm t2)
		end
	      | IIL_TERM (a, rterm1, rterm2) =>
		let val a  = aux (rterm2term a)      bounds env
		    val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in IIL_TERM (mk_rterm a, mk_rterm t1, mk_rterm t2)
		end
	      | IIN_TERM (a, rterm1, rterm2) =>
		let val a  = aux (rterm2term a)      bounds env
		    val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in IIN_TERM (mk_rterm a, mk_rterm t1, mk_rterm t2)
		end
	      | ILA_TERM (a, rterm1, rterm2) =>
		let val a  = aux (rterm2term a)      bounds env
		    val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in ILA_TERM (mk_rterm a, mk_rterm t1, mk_rterm t2)
		end
	      | IAT_TERM (a, rterm1, rterm2) =>
		let val a  = aux (rterm2term a)      bounds env
		    val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in IAT_TERM (mk_rterm a, mk_rterm t1, mk_rterm t2)
		end
	      | CBV_TERM (a, x, f) =>
		let val a = aux (rterm2term a) bounds env
		    val f = aux (rterm2term f) (VARS.add (bounds, dest_nuprl_var x)) env
		in CBV_TERM (mk_rterm a, x, mk_rterm f)
		end
	      | CBA_TERM (a, x, f) =>
		let val a = aux (rterm2term a) bounds env
		    val f = aux (rterm2term f) (VARS.add (bounds, dest_nuprl_var x)) env
		in CBA_TERM (mk_rterm a, x, mk_rterm f)
		end
	      | FUN_TERM (a, x, f) =>
		let val a = aux (rterm2term a) bounds env
		    val f = aux (rterm2term f) (VARS.add (bounds, dest_nuprl_var x)) env
		in FUN_TERM (mk_rterm a, x, mk_rterm f)
		end
	      | PRD_TERM (a, x, f) =>
		let val a = aux (rterm2term a) bounds env
		    val f = aux (rterm2term f) (VARS.add (bounds, dest_nuprl_var x)) env
		in PRD_TERM (mk_rterm a, x, mk_rterm f)
		end
	      | TUN_TERM (a, x, f) =>
		let val a = aux (rterm2term a) bounds env
		    val f = aux (rterm2term f) (VARS.add (bounds, dest_nuprl_var x)) env
		in TUN_TERM (mk_rterm a, x, mk_rterm f)
		end
	      | SET_TERM (a, x, f) =>
		let val a = aux (rterm2term a) bounds env
		    val f = aux (rterm2term f) (VARS.add (bounds, dest_nuprl_var x)) env
		in SET_TERM (mk_rterm a, x, mk_rterm f)
		end
	      | LES_TERM (a, b, rterm1, rterm2) =>
		let val a  = aux (rterm2term a)      bounds env
		    val b  = aux (rterm2term b)      bounds env
		    val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in LES_TERM (mk_rterm a, mk_rterm b, mk_rterm t1, mk_rterm t2)
		end
	      | IEQ_TERM (a, b, rterm1, rterm2) =>
		let val a  = aux (rterm2term a)      bounds env
		    val b  = aux (rterm2term b)      bounds env
		    val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in IEQ_TERM (mk_rterm a, mk_rterm b, mk_rterm t1, mk_rterm t2)
		end
	      | SPR_TERM (pair, var1, var2, rterm) =>
		let val p  = aux (rterm2term pair) bounds env
		    val bs = VARS.addList (bounds, [dest_nuprl_var var1, dest_nuprl_var var2])
		    val t  = aux (rterm2term rterm) bs env
		in SPR_TERM (mk_rterm p, var1, var2, mk_rterm t)
		end
	      | AEQ_TERM (n, a, b, rterm1, rterm2) =>
		let val a  = aux (rterm2term a)      bounds env
		    val b  = aux (rterm2term b)      bounds env
		    val t1 = aux (rterm2term rterm1) bounds env
		    val t2 = aux (rterm2term rterm2) bounds env
		in AEQ_TERM (n, mk_rterm a, mk_rterm b, mk_rterm t1, mk_rterm t2)
		end
	      | DEC_TERM (dec, var1, rterm1, var2, rterm2) =>
		let val d  = aux (rterm2term dec) bounds env
		    val b1 = VARS.add (bounds, dest_nuprl_var var1)
		    val t1 = aux (rterm2term rterm1) b1 env
		    val b2 = VARS.add (bounds, dest_nuprl_var var2)
		    val t2 = aux (rterm2term rterm2) b2 env
		in DEC_TERM (mk_rterm d, var1, mk_rterm t1, var2, mk_rterm t2)
		end
	      | IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase) =>
		let val i  = aux (rterm2term i) bounds env
		    val b1 = VARS.add (VARS.add (bounds, dest_nuprl_var x), dest_nuprl_var rd)
		    val t1 = aux (rterm2term downcase) b1 env
		    val t  = aux (rterm2term basecase) bounds env
		    val b2 = VARS.add (VARS.add (bounds, dest_nuprl_var y), dest_nuprl_var ru)
		    val t2 = aux (rterm2term upcase) b2 env
		in IND_TERM (mk_rterm i, x, rd, mk_rterm t1, mk_rterm t, y, ru, mk_rterm t2)
		end
	      | TERM (opr as ((opid, tag), params), bterms) =>
		if opid = "variable"
		then let val v = dest_variable term
		     in if VARS.member (bounds, v)
			then term
			else case lookup env v of
				 SOME (t,e) => aux t (VARS.difference (bounds, domain e)) e
			       | NONE => term
		     end
		else let val bterms' =
			     map (fn B_TERM (vs, rt) =>
				     let val t1 = rterm2term rt
					 val bs = nvars2set bounds vs
					 val t2 = aux t1 bs env
				     in B_TERM (vs, mk_rterm t2)
				     end)
				 bterms
		     in TERM (opr, bterms')
		     end
    (*val _ = print ("[closing]\n")*)
    in aux term VARS.empty env
    end


(*
(* ------ SETS UNUSED VARIABLES TO DUMMY ------ *)

fun to ind sub term =
    case term of
	AXM_TERM => "nil"
      | BOT_TERM => "(error \"bottom\")"
      | INT_TERM => nuprl2lisp_type term
      | DUM_TERM => raise Fail "nuprl2lisp_term:DUM_TERM"
      | ATM_TERM NONE => nuprl2lisp_type term
      | ATM_TERM (SOME n) => nuprl2lisp_type term
      | TOK_TERM (t,k) =>  "\"" ^ t ^ "\""
      | NAT_TERM n => II.toString n
      | VAR_TERM var =>
	let val v = dest_nuprl_var var
	in case SUB.find (sub, v) of
	       SOME v => v
	     | NONE => nuprl2lisp_var var
	end
      | INL_TERM t =>
	"(cons \"inl\" (cons " ^ nuprl2lisp_ref_term ind sub t ^ " nil))"
      | INR_TERM t =>
	"(cons \"inr\" (cons " ^ nuprl2lisp_ref_term ind sub t ^ " nil))"
      | FIX_TERM t => raise Fail "nuprl2lisp:FIX_TERM:not_applied"
      | MIN_TERM t => raise Fail "nuprl2lisp:MIN_TERM"
      | LAM_TERM (x, f) =>
	let val vars = free_vars (rterm2term f)
	    val v = dest_nuprl_var x
	in "(lambda ("
	   ^ nuprl2lisp_var x ^ ")"
	   ^ (if VARS.member (vars, v)
	      then ""
	      else "\n(declare (ignore " ^ nuprl2lisp_var x ^ "))")
	   ^ "\n"
	   ^ nuprl2lisp_ref_term ind sub f
	   ^ ")"
	end
      | REC_TERM (t, e) => nuprl2lisp_type term
      | WAI_TERM (t, e) => raise Fail "nuprl2lisp_term:WAI_TERM"
      | APP_TERM (f, a) =>
	(case rterm2term f of
	     FIX_TERM t =>
	     let val f = rterm2term t
	     in if is_nuprl_lambda_term f
		then let val (r,g) = dest_lambda 0 f
		     in if is_nuprl_lambda_term g
			then let val (x,b) = dest_lambda 0 g
				 val r' = "#'" ^ nuprl2lisp_var r
				 val sub' = SUB.insert (sub, dest_nuprl_var r, r')
			     in "(labels (("
				^ nuprl2lisp_var r
				^ " ("
				^ nuprl2lisp_var x
				^ ")"
				^ "\n"
				^ nuprl2lisp_term ind sub' b
				^ "))\n"
				^ "(funcall #'" ^ nuprl2lisp_var r
				^ "\n"
				^ nuprl2lisp_ref_term ind sub a
				^ "))"
			     end
			else raise Fail "nuprl2lisp_term:FIX_TERM(2)"
		     end
		else raise Fail "nuprl2lisp_term:FIX_TERM(1)"
	     end
	   | _ =>
	     "(funcall "
	     ^ nuprl2lisp_ref_term ind sub f
	     ^ "\n"
	     ^ nuprl2lisp_ref_term ind sub a
	     ^ ")")
      | PAI_TERM (x, y) =>
	"(cons "
	^ nuprl2lisp_ref_term ind sub x
	^ " (cons "
	^ nuprl2lisp_ref_term ind sub y
	^ " nil))"
      | ADD_TERM (x, y) =>
	"(+ "
	^ nuprl2lisp_ref_term ind sub x
	^ " "
	^ nuprl2lisp_ref_term ind sub y
	^ ")"
      | SUB_TERM (x, y) => raise Fail "nuprl2lisp_term:SUB_TERM"
      | MUL_TERM (x, y) => raise Fail "nuprl2lisp_term:MUL_TERM"
      | DIV_TERM (x, y) =>
	"(/ "
	^ nuprl2lisp_ref_term ind sub x
	^ " "
	^ nuprl2lisp_ref_term ind sub y
	^ ")"
      | REM_TERM (x, y) => raise Fail "nuprl2lisp_term:REM_TERM"
      | EQT_TERM (x, y) =>
	"(if (string= "
	^ nuprl2lisp_ref_term ind sub x
	^ " "
	^ nuprl2lisp_ref_term ind sub y
	^ ")"
	^ "\n"
	^ "(cons \"inl\" (cons nil nil))"
	^ "\n"
	^ "(cons \"inr\" (cons nil nil)))"
      | UNI_TERM (x, y) => nuprl2lisp_type term
      | EQU_TERM (a, rterm1, rterm2) => nuprl2lisp_type term
      | IAX_TERM (a, rterm1, rterm2) =>
	"(if (null "
	^ nuprl2lisp_ref_term ind sub a
	^ ")"
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm1
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm2
	^ ")"
      | IPA_TERM (a, rterm1, rterm2) =>
	"(if (= 2 (length "
	^ nuprl2lisp_ref_term ind sub a
	^ "))"
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm1
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm2
	^ ")"
      | IIR_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp_term:IIR_TERM"
      | IIL_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp_term:IIL_TERM"
      | IIN_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp_term:IIN_TERM"
      | ILA_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp_term:ILA_TERM"
      | IAT_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp_term:IAT_TERM"
      | CBV_TERM (arg, x, B) =>
	"(funcall (lambda ("
	^ nuprl2lisp_var x
	^ ")\n"
	^ nuprl2lisp_ref_term ind sub B
	^ ")"
	^ "\n"
	^ nuprl2lisp_ref_term ind sub arg
	^ ")"
      | CBA_TERM (arg, x, B) =>
	"(funcall (lambda ("
	^ nuprl2lisp_var x
	^ ")\n"
	^ nuprl2lisp_ref_term ind sub B
	^ ")"
	^ "\n"
	^ nuprl2lisp_ref_term ind sub arg
	^ ")"
      | FUN_TERM (arg, x, B) => nuprl2lisp_type term
      | PRD_TERM (arg, x, B) => nuprl2lisp_type term
      | TUN_TERM (arg, x, B) => nuprl2lisp_type term
      | LES_TERM (a, b, rterm1, rterm2) =>
	"(if (< "
	^ nuprl2lisp_ref_term ind sub a
	^ " "
	^ nuprl2lisp_ref_term ind sub b
	^ ")"
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm1
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm2
	^ ")"
      | IEQ_TERM (a, b, rterm1, rterm2) =>
	"(if (= "
	^ nuprl2lisp_ref_term ind sub a
	^ " "
	^ nuprl2lisp_ref_term ind sub b
	^ ")"
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm1
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm2
	^ ")"
      | SPR_TERM (pair, var1, var2, rterm) =>
	let val vars = free_vars (rterm2term rterm)
	    val b1   = VARS.member (vars, dest_nuprl_var var1)
	    val b2   = VARS.member (vars, dest_nuprl_var var2)
	in if b1 orelse b2
	   then "(let ((pair " (* first check that 'pair' is not already bound *)
		^ nuprl2lisp_ref_term ind sub pair
		^ "))"
		^ "\n"
		^ "(if (= 2 (length pair))"
		^ "\n"
		^ "(let ("
		^ (if b1
		   then "("
			^ nuprl2lisp_var var1
			^ " (car pair))"
		   else "")
		^ (if b2
		   then (if b1 then "\n" else "")
			^ "("
			^ nuprl2lisp_var var2
			^ " (car (cdr pair)))"
		   else "")
		^ ")"
		^ "\n"
		^ nuprl2lisp_ref_term ind sub rterm
		^ ")\n"
		^ "(error \"spread\")))"
	   else nuprl2lisp_ref_term ind sub rterm
	end
      | AEQ_TERM (n, a, b, rterm1, rterm2) =>
	"(if (string= "
	^ nuprl2lisp_ref_term ind sub a
	^ " "
	^ nuprl2lisp_ref_term ind sub b
	^ ")"
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm1
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm2
	^ ")"
      | DEC_TERM (dec, var1, rterm1, var2, rterm2) =>
	let val vars1 = free_vars (rterm2term rterm1)
	    val vars2 = free_vars (rterm2term rterm2)
	    val b1    = VARS.member (vars1, dest_nuprl_var var1)
	    val b2    = VARS.member (vars2, dest_nuprl_var var2)
	in "(let ((dec " (* first check that 'dec' is not already bound *)
	   ^ nuprl2lisp_ref_term ind sub dec
	   ^ "))"
	   ^ "\n"
	   ^ "(if (= 2 (length dec))"
	   ^ "\n"
	   ^ "(let ((tag (car dec)))"
	   ^ "\n"
	   ^ "(if (string= tag \"inl\")"
	   ^ "\n"
	   ^ (if b1
	      then "(let (("
		   ^ nuprl2lisp_var var1
		   ^ " (car (cdr dec))))"
		   ^ "\n"
		   ^ nuprl2lisp_ref_term ind sub rterm1
		   ^ ")"
	      else nuprl2lisp_ref_term ind sub rterm1)
	   ^ "\n"
	   ^ "(if (string= tag \"inr\")"
	   ^ "\n"
	   ^ (if b2
	      then "(let (("
		   ^ nuprl2lisp_var var2
		   ^ " (car (cdr dec))))"
		   ^ "\n"
		   ^ nuprl2lisp_ref_term ind sub rterm2
		   ^ ")"
	      else nuprl2lisp_ref_term ind sub rterm2)
	   ^ "\n"
	   ^ "(error \"decide\"))))"
	   ^ "\n"
	   ^ "(error \"decide\")))"
	end
      | IND_TERM _ => raise Fail ("nuprl2lisp:IND_TERM")
      | CLO_TERM _ => raise Fail ("nuprl2lisp:CLO_TERM")
      | TERM ((("subtract", _), []), [B_TERM ([], x), B_TERM ([], y)]) =>
	"(- "
	^ nuprl2lisp_ref_term ind sub x
	^ " "
	^ nuprl2lisp_ref_term ind sub y
	^ ")"
      | TERM ((("add", _), []), [B_TERM ([], x), B_TERM ([], y)]) =>
	"(+ "
	^ nuprl2lisp_ref_term ind sub x
	^ " "
	^ nuprl2lisp_ref_term ind sub y
	^ ")"
      | TERM ((("divide", _), []), [B_TERM ([], x), B_TERM ([], y)]) =>
	"(/ "
	^ nuprl2lisp_ref_term ind sub x
	^ " "
	^ nuprl2lisp_ref_term ind sub y
	^ ")"
      | TERM ((("callbyvalueall", _), _), [B_TERM ([], arg), B_TERM ([x], B)]) =>
	"(funcall (lambda ("
	^ nuprl2lisp_var x
	^ ")\n"
	^ nuprl2lisp_ref_term ind sub B
	^ ")"
	^ "\n"
	^ nuprl2lisp_ref_term ind sub arg
	^ ")"
      | TERM ((("callbyvalue", _), _), [B_TERM ([], arg), B_TERM ([x], B)]) =>
	"(funcall (lambda ("
	^ nuprl2lisp_var x
	^ ")\n"
	^ nuprl2lisp_ref_term ind sub B
	^ ")"
	^ "\n"
	^ nuprl2lisp_ref_term ind sub arg
	^ ")"
      | TERM ((("atom_eq", _), _), [B_TERM ([], a), B_TERM ([], b), B_TERM ([], rterm1), B_TERM ([], rterm2)]) =>
	"(if (string= "
	^ nuprl2lisp_ref_term ind sub a
	^ " "
	^ nuprl2lisp_ref_term ind sub b
	^ ")"
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm1
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm2
	^ ")"
      | TERM ((("less", _), _), [B_TERM ([], a), B_TERM ([], b), B_TERM ([], rterm1), B_TERM ([], rterm2)]) =>
	"(if (< "
	^ nuprl2lisp_ref_term ind sub a
	^ " "
	^ nuprl2lisp_ref_term ind sub b
	^ ")"
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm1
	^ "\n"
	^ nuprl2lisp_ref_term ind sub rterm2
	^ ")"
      | TERM ((("eq_term", _), _), [B_TERM ([], a), B_TERM ([], b)]) =>
	"(if (string= "
	^ nuprl2lisp_ref_term ind sub a
	^ " "
	^ nuprl2lisp_ref_term ind sub b
	^ ")"
	^ "\n"
	^ "(cons \"inl\" (cons nil nil))"
	^ "\n"
	^ "(cons \"inr\" (cons nil nil)))"
      | (term as TERM ((("product", _), _), [B_TERM ([], a), B_TERM ([v], b)])) =>
	if is_null_nuprl_var v
	then "\"" ^ ppTerm term ^ "\""
	else raise Fail "nuprl2lisp_term:product"
      | (term as TERM ((("union", _), _), [B_TERM ([], a), B_TERM ([], b)])) =>
	"\"" ^ ppTerm term ^ "\""
      | TERM ((("token", _), [(t,tkind)]), []) => "\"" ^ t ^ "\""
      | TERM (((opid, tag), parameters), subterms) =>
	raise Fail ("nuprl2lisp:"
		    ^ opid
		    ^ "("
		    ^ Int.toString (List.length parameters)
		    ^ "-"
		    ^ Int.toString (List.length subterms)
		    ^ ")")
*)


(* ------ TO STRING ------ *)

fun toStringOpid  opid  = opid
fun toStringTag   tag   = get_tag tag
fun toStringValue value = value
fun toStringKind  kind  = kind

fun toStringParameter (value, kind) = toStringValue value ^ ":" ^ toStringKind kind

fun toStringParameters params =
    T.fmt {init  = "",
	   final = "",
	   sep   = ",",
	   fmt   = toStringParameter}
	  params

fun toStringOpidTag (opid, tag) = toStringOpid opid ^ ":" ^ toStringTag tag

fun toStringOperator (opid_tag, []) = toStringOpidTag opid_tag
  | toStringOperator (opid_tag, parameters) =
    toStringOpidTag opid_tag ^ "," ^ toStringParameters parameters

fun toStringVars vars =
    T.fmt {init  = "",
	   final = "",
	   sep   = ",",
	   fmt   = fn v => v ^ ":v"}
	  vars

fun toStringNVars nvars =
    T.fmt {init  = "",
	   final = "",
	   sep   = ",",
	   fmt   = fn v => dest_nuprl_var v ^ ":v"}
	  nvars

fun toStringTerm (TERM (operator, [])) ind space newline =
    "{" ^ toStringOperator operator ^ "}()"
  | toStringTerm (TERM (operator, bterms)) ind space newline =
    let val ind' = ind ^ space
    in "{" ^ toStringOperator operator ^ "}" ^ newline ^ ind ^
       "(" ^ toStringBTerms   bterms ind' space newline ^ ")"
    end
  | toStringTerm AXM_TERM                                      ind space newline = toStringTerm make_axiom_term ind space newline
  | toStringTerm BOT_TERM                                      ind space newline = toStringTerm make_bottom_term ind space newline
  | toStringTerm INT_TERM                                      ind space newline = toStringTerm make_int_term ind space newline
  | toStringTerm VOI_TERM                                      ind space newline = toStringTerm make_void_term ind space newline
  | toStringTerm DUM_TERM                                      ind space newline = raise Fail "toStringTerm:DUM_TERM"
  | toStringTerm (ATM_TERM x)                                  ind space newline = toStringTerm (make_atom_term x) ind space newline
  | toStringTerm (TOK_TERM x)                                  ind space newline = toStringTerm (make_token_term x) ind space newline
  | toStringTerm (NAT_TERM nat)                                ind space newline = toStringTerm (make_natural_number_term nat) ind space newline
  | toStringTerm (VAR_TERM var)                                ind space newline = toStringTerm (make_variable_term (dest_nuprl_var var)) ind space newline
  | toStringTerm (INL_TERM rterm)                              ind space newline = toStringTerm (make_inl_term rterm) ind space newline
  | toStringTerm (INR_TERM rterm)                              ind space newline = toStringTerm (make_inr_term rterm) ind space newline
  | toStringTerm (FIX_TERM rterm)                              ind space newline = toStringTerm (make_fix_term rterm) ind space newline
  | toStringTerm (MIN_TERM rterm)                              ind space newline = toStringTerm (make_minus_term rterm) ind space newline
  | toStringTerm (LAM_TERM (var, rterm))                       ind space newline = toStringTerm (make_lambda_term var rterm) ind space newline
  | toStringTerm (REC_TERM (var, rterm))                       ind space newline = toStringTerm (make_rec_term    var rterm) ind space newline
  | toStringTerm (WAI_TERM (rterm1, rterm2))                   ind space newline = toStringTerm (make_wait_term      rterm1 rterm2) ind space newline
  | toStringTerm (APP_TERM (rterm1, rterm2))                   ind space newline = toStringTerm (make_apply_term     rterm1 rterm2) ind space newline
  | toStringTerm (PAI_TERM (rterm1, rterm2))                   ind space newline = toStringTerm (make_pair_term      rterm1 rterm2) ind space newline
  | toStringTerm (ADD_TERM (rterm1, rterm2))                   ind space newline = toStringTerm (make_add_term       rterm1 rterm2) ind space newline
  | toStringTerm (SUB_TERM (rterm1, rterm2))                   ind space newline = toStringTerm (make_subtract_term  rterm1 rterm2) ind space newline
  | toStringTerm (MUL_TERM (rterm1, rterm2))                   ind space newline = toStringTerm (make_multiply_term  rterm1 rterm2) ind space newline
  | toStringTerm (DIV_TERM (rterm1, rterm2))                   ind space newline = toStringTerm (make_divide_term    rterm1 rterm2) ind space newline
  | toStringTerm (REM_TERM (rterm1, rterm2))                   ind space newline = toStringTerm (make_remainder_term rterm1 rterm2) ind space newline
  | toStringTerm (EQT_TERM (rterm1, rterm2))                   ind space newline = toStringTerm (make_eq_term_term   rterm1 rterm2) ind space newline
  | toStringTerm (UNI_TERM (rterm1, rterm2))                   ind space newline = toStringTerm (make_union_term     rterm1 rterm2) ind space newline
  | toStringTerm (EQU_TERM (a, rterm1, rterm2))                ind space newline = toStringTerm (make_equal_term    a rterm1 rterm2) ind space newline
  | toStringTerm (IAX_TERM (a, rterm1, rterm2))                ind space newline = toStringTerm (make_isaxiom_term  a rterm1 rterm2) ind space newline
  | toStringTerm (IPA_TERM (a, rterm1, rterm2))                ind space newline = toStringTerm (make_ispair_term   a rterm1 rterm2) ind space newline
  | toStringTerm (IIR_TERM (a, rterm1, rterm2))                ind space newline = toStringTerm (make_isinr_term    a rterm1 rterm2) ind space newline
  | toStringTerm (IIL_TERM (a, rterm1, rterm2))                ind space newline = toStringTerm (make_isinl_term    a rterm1 rterm2) ind space newline
  | toStringTerm (IIN_TERM (a, rterm1, rterm2))                ind space newline = toStringTerm (make_isint_term    a rterm1 rterm2) ind space newline
  | toStringTerm (ILA_TERM (a, rterm1, rterm2))                ind space newline = toStringTerm (make_islambda_term a rterm1 rterm2) ind space newline
  | toStringTerm (IAT_TERM (a, rterm1, rterm2))                ind space newline = toStringTerm (make_isatom2_term  a rterm1 rterm2) ind space newline
  | toStringTerm (CBV_TERM (a, x, f))                          ind space newline = toStringTerm (make_callbyvalue_term    a (x, f)) ind space newline
  | toStringTerm (CBA_TERM (a, x, f))                          ind space newline = toStringTerm (make_callbyvalueall_term a (x, f)) ind space newline
  | toStringTerm (FUN_TERM (a, x, f))                          ind space newline = toStringTerm (make_function_term       a (x, f)) ind space newline
  | toStringTerm (PRD_TERM (a, x, f))                          ind space newline = toStringTerm (make_product_term        a (x, f)) ind space newline
  | toStringTerm (TUN_TERM (a, x, f))                          ind space newline = toStringTerm (make_tunion_term         a (x, f)) ind space newline
  | toStringTerm (SET_TERM (a, x, f))                          ind space newline = toStringTerm (make_set_term            a (x, f)) ind space newline
  | toStringTerm (LES_TERM (a, b, rterm1, rterm2))             ind space newline = toStringTerm (make_less_term   a b rterm1 rterm2) ind space newline
  | toStringTerm (IEQ_TERM (a, b, rterm1, rterm2))             ind space newline = toStringTerm (make_int_eq_term a b rterm1 rterm2) ind space newline
  | toStringTerm (SPR_TERM (pair, var1, var2, rterm))          ind space newline = toStringTerm (make_spread_term pair (var1, var2, rterm)) ind space newline
  | toStringTerm (AEQ_TERM (n, a, b, rterm1, rterm2))          ind space newline = toStringTerm (make_atom_eq_term n a b rterm1 rterm2) ind space newline
  | toStringTerm (DEC_TERM (dec, var1, rterm1, var2, rterm2))  ind space newline = toStringTerm (make_decide_term dec (var1, rterm1) (var2, rterm2)) ind space newline
  | toStringTerm (IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) ind space newline = toStringTerm (make_ind_term i (x, rd, downcase) basecase (y, ru, upcase)) ind space newline
  | toStringTerm (CLO_TERM clos) ind space newline = toStringClos clos ind space newline

and toStringClos (rterm, env) ind space newline =
    let val ind' = ind ^ space
    in "{!closure:OPID}\n" ^ ind ^
       "(" ^ toStringRTerm rterm ind' space newline ^ ";" ^ newline
       ^ toStringEnv env ind' space newline ^ ")"
    end

and toStringEnv (ENV m) ind space newline =
    (MAP.foldri (fn (var, (_,_,termref), fstr) =>
		 fn ind =>
		    let val ind1 = ind  ^ space
			val ind2 = ind1 ^ space
		    in ind
		       ^ "{env:OPID}" ^ newline
		       ^ ind
		       ^ "({bound_id:OPID," ^ toStringVars [var] ^ "}" ^ newline
		       ^ ind1
		       ^ "("
		       ^ toStringTerm (#1 (!termref)) ind2 space newline ^ ");" ^ newline
		       ^ fstr ind1 ^ ")"
		    end)
		(fn ind => ind ^ "{env:OPID}()")
		m)
	ind

and toStringRTerm ref_term = toStringTerm (rterm2term ref_term)

and toStringBTerm (B_TERM ([],   rterm)) ind space newline =
    toStringRTerm rterm ind space newline
  | toStringBTerm (B_TERM (vars, rterm)) ind space newline =
    let val str = toStringNVars vars
    in "{bound_id:OPID," ^ str ^ "}" ^ newline ^ ind ^
       "(" ^ toStringRTerm rterm (ind ^ space) space newline ^ ")"
    end

and toStringBTerms bterms ind space newline =
    T.fmt {init  = "",
	   final = "",
	   sep   = ";" ^ newline ^ ind,
	   fmt   = fn bterm => toStringBTerm bterm ind space newline}
	  bterms

and toStringTerms terms ind space newline =
    T.fmt {init  = "",
	   final = "",
	   sep   = ";" ^ newline ^ ind,
	   fmt   = fn term => toStringTerm term ind space newline}
	  terms

val to_string_space   = " "
val to_string_newline = "\n"

val toStringTerm   = fn term  => toStringTerm  term  "" to_string_space to_string_newline
and toStringRTerm  = fn rterm => toStringRTerm rterm "" to_string_space to_string_newline
and toStringEnv    = fn env   => toStringEnv   env   "" to_string_space to_string_newline
and spToStringTerm = fn term  => toStringTerm  term  "" "" ""

(* -- write to file while traversing the tree *)

fun toStringTerm_stream out (TERM (operator, [])) ind =
    TextIO.output (out, "{" ^ toStringOperator operator   ^ "}()")
  | toStringTerm_stream out (TERM (operator, bterms)) ind =
    let val ind' = ind ^ " "
	val opr  = toStringOperator operator
    in TextIO.output (out, "{" ^ opr ^ "}\n" ^ ind ^ "(");
       toStringBTerms_stream out bterms ind';
       TextIO.output (out, ")")
    end
  | toStringTerm_stream out AXM_TERM                    ind = toStringTerm_stream out make_axiom_term  ind
  | toStringTerm_stream out BOT_TERM                    ind = toStringTerm_stream out make_bottom_term ind
  | toStringTerm_stream out INT_TERM                    ind = toStringTerm_stream out make_int_term    ind
  | toStringTerm_stream out VOI_TERM                    ind = toStringTerm_stream out make_void_term   ind
  | toStringTerm_stream out DUM_TERM                    ind = raise Fail "toStringTerm_stream:DUM_TERM"
  | toStringTerm_stream out (ATM_TERM x)                ind = toStringTerm_stream out (make_atom_term x) ind
  | toStringTerm_stream out (TOK_TERM x)                ind = toStringTerm_stream out (make_token_term x) ind
  | toStringTerm_stream out (NAT_TERM nat)              ind = toStringTerm_stream out (make_natural_number_term nat) ind
  | toStringTerm_stream out (VAR_TERM var)              ind = toStringTerm_stream out (make_variable_term (dest_nuprl_var var)) ind
  | toStringTerm_stream out (INL_TERM rterm)            ind = toStringTerm_stream out (make_inl_term   rterm) ind
  | toStringTerm_stream out (INR_TERM rterm)            ind = toStringTerm_stream out (make_inr_term   rterm) ind
  | toStringTerm_stream out (FIX_TERM rterm)            ind = toStringTerm_stream out (make_fix_term   rterm) ind
  | toStringTerm_stream out (MIN_TERM rterm)            ind = toStringTerm_stream out (make_minus_term rterm) ind
  | toStringTerm_stream out (LAM_TERM (var, rterm))     ind = toStringTerm_stream out (make_lambda_term var rterm) ind
  | toStringTerm_stream out (REC_TERM (var, rterm))     ind = toStringTerm_stream out (make_rec_term    var rterm) ind
  | toStringTerm_stream out (WAI_TERM (rterm1, rterm2)) ind = toStringTerm_stream out (make_wait_term      rterm1 rterm2) ind
  | toStringTerm_stream out (APP_TERM (rterm1, rterm2)) ind = toStringTerm_stream out (make_apply_term     rterm1 rterm2) ind
  | toStringTerm_stream out (PAI_TERM (rterm1, rterm2)) ind = toStringTerm_stream out (make_pair_term      rterm1 rterm2) ind
  | toStringTerm_stream out (ADD_TERM (rterm1, rterm2)) ind = toStringTerm_stream out (make_add_term       rterm1 rterm2) ind
  | toStringTerm_stream out (SUB_TERM (rterm1, rterm2)) ind = toStringTerm_stream out (make_subtract_term  rterm1 rterm2) ind
  | toStringTerm_stream out (MUL_TERM (rterm1, rterm2)) ind = toStringTerm_stream out (make_multiply_term  rterm1 rterm2) ind
  | toStringTerm_stream out (DIV_TERM (rterm1, rterm2)) ind = toStringTerm_stream out (make_divide_term    rterm1 rterm2) ind
  | toStringTerm_stream out (REM_TERM (rterm1, rterm2)) ind = toStringTerm_stream out (make_remainder_term rterm1 rterm2) ind
  | toStringTerm_stream out (EQT_TERM (rterm1, rterm2)) ind = toStringTerm_stream out (make_eq_term_term   rterm1 rterm2) ind
  | toStringTerm_stream out (UNI_TERM (rterm1, rterm2)) ind = toStringTerm_stream out (make_union_term     rterm1 rterm2) ind
  | toStringTerm_stream out (EQU_TERM (a, rterm1, rterm2)) ind = toStringTerm_stream out (make_equal_term    a rterm1 rterm2) ind
  | toStringTerm_stream out (IAX_TERM (a, rterm1, rterm2)) ind = toStringTerm_stream out (make_isaxiom_term  a rterm1 rterm2) ind
  | toStringTerm_stream out (IPA_TERM (a, rterm1, rterm2)) ind = toStringTerm_stream out (make_ispair_term   a rterm1 rterm2) ind
  | toStringTerm_stream out (IIR_TERM (a, rterm1, rterm2)) ind = toStringTerm_stream out (make_isinr_term    a rterm1 rterm2) ind
  | toStringTerm_stream out (IIL_TERM (a, rterm1, rterm2)) ind = toStringTerm_stream out (make_isinl_term    a rterm1 rterm2) ind
  | toStringTerm_stream out (IIN_TERM (a, rterm1, rterm2)) ind = toStringTerm_stream out (make_isint_term    a rterm1 rterm2) ind
  | toStringTerm_stream out (ILA_TERM (a, rterm1, rterm2)) ind = toStringTerm_stream out (make_islambda_term a rterm1 rterm2) ind
  | toStringTerm_stream out (IAT_TERM (a, rterm1, rterm2)) ind = toStringTerm_stream out (make_isatom2_term  a rterm1 rterm2) ind
  | toStringTerm_stream out (CBV_TERM (a, x, f)) ind = toStringTerm_stream out (make_callbyvalue_term    a (x, f)) ind
  | toStringTerm_stream out (CBA_TERM (a, x, f)) ind = toStringTerm_stream out (make_callbyvalueall_term a (x, f)) ind
  | toStringTerm_stream out (FUN_TERM (a, x, f)) ind = toStringTerm_stream out (make_function_term       a (x, f)) ind
  | toStringTerm_stream out (PRD_TERM (a, x, f)) ind = toStringTerm_stream out (make_product_term        a (x, f)) ind
  | toStringTerm_stream out (TUN_TERM (a, x, f)) ind = toStringTerm_stream out (make_tunion_term         a (x, f)) ind
  | toStringTerm_stream out (SET_TERM (a, x, f)) ind = toStringTerm_stream out (make_set_term            a (x, f)) ind
  | toStringTerm_stream out (LES_TERM (a, b, rterm1, rterm2)) ind = toStringTerm_stream out (make_less_term    a b rterm1 rterm2) ind
  | toStringTerm_stream out (IEQ_TERM (a, b, rterm1, rterm2)) ind = toStringTerm_stream out (make_int_eq_term  a b rterm1 rterm2) ind
  | toStringTerm_stream out (SPR_TERM (pair, var1, var2, rterm))          ind = toStringTerm_stream out (make_spread_term pair (var1, var2, rterm)) ind
  | toStringTerm_stream out (AEQ_TERM (n, a, b, rterm1, rterm2))          ind = toStringTerm_stream out (make_atom_eq_term n a b rterm1 rterm2) ind
  | toStringTerm_stream out (DEC_TERM (dec, var1, rterm1, var2, rterm2))  ind = toStringTerm_stream out (make_decide_term dec (var1, rterm1) (var2, rterm2)) ind
  | toStringTerm_stream out (IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) ind = toStringTerm_stream out (make_ind_term i (x, rd, downcase) basecase (y, ru, upcase)) ind
  | toStringTerm_stream out (CLO_TERM clos) ind = toStringClos_stream out clos ind

and toStringClos_stream out (rterm, env) ind =
    let val ind' = ind ^ " "
    in TextIO.output (out, "{!closure:OPID}\n" ^ ind ^ "(");
       toStringRTerm_stream out rterm ind';
       TextIO.output (out, ";\n" ^ ind');
       toStringEnv_stream out env ind';
       TextIO.output (out, ")")
    end

and toStringEnv_stream out (ENV m) ind =
    (MAP.foldli (fn (var, (_,_,termref), ind) =>
		    let val ind1 = ind  ^ " "
		    in (TextIO.output (out, "{env:OPID}\n" ^ ind);
			TextIO.output (out, "({bound_id:OPID," ^ toStringVars [var] ^ "}\n" ^ ind1);
			toStringTerm_stream out (#1 (!termref)) ind1;
			TextIO.output (out, "\n" ^ ind1);
			ind1)
		    end)
		ind
		m;
     ())

and toStringRTerm_stream out ref_term =
    toStringTerm_stream out (rterm2term ref_term)

and toStringBTerm_stream out (B_TERM ([],   rterm)) ind =
    toStringRTerm_stream out rterm ind
  | toStringBTerm_stream out (B_TERM (vars, rterm)) ind =
    let val str = toStringNVars vars
    in TextIO.output (out, "{bound_id:OPID," ^ str ^ "}\n" ^ ind ^ "(");
       toStringRTerm_stream out rterm (ind ^ " ");
       TextIO.output (out, ")")
    end

and toStringBTerms_stream out [] ind = ()
  | toStringBTerms_stream out [x] ind = toStringBTerm_stream out x ind
  | toStringBTerms_stream out (x :: xs) ind =
    (toStringBTerm_stream out x ind;
     TextIO.output (out, ";\n" ^ ind);
     toStringBTerms_stream out xs ind)

fun toStringTermStream term file =
    let val out = TextIO.openOut file
	val _   = toStringTerm_stream out term ""
	val _   = TextIO.closeOut out
    in ()
    end


(* -- pretty printer -- *)

fun ppTerm (VAR_TERM var) = dest_nuprl_var var
  | ppTerm (LAM_TERM (var, rterm)) =
    "(\\" ^ dest_nuprl_var var ^ ". " ^ ppRTerm rterm ^ ")"
  | ppTerm (REC_TERM (var, rterm)) =
    "rec(" ^ dest_nuprl_var var ^ ". " ^ ppRTerm rterm ^ ")"
  | ppTerm (WAI_TERM (rterm1, rterm2)) =
    "wait(" ^ ppRTerm rterm1 ^ "," ^ ppRTerm rterm2 ^ ")"
  | ppTerm (APP_TERM (rterm1, rterm2)) =
    "((" ^ ppRTerm rterm1 ^ ")(" ^ ppRTerm rterm2 ^ "))"
  | ppTerm (INL_TERM rterm) = "inl(" ^ ppRTerm rterm ^ ")"
  | ppTerm (INR_TERM rterm) = "inr(" ^ ppRTerm rterm ^ ")"
  | ppTerm (FIX_TERM rterm) = "fix(" ^ ppRTerm rterm ^ ")"
  | ppTerm (PAI_TERM (rterm1, rterm2)) =
    "(" ^ ppRTerm rterm1 ^ "," ^ ppRTerm rterm2 ^ ")"
  | ppTerm AXM_TERM = "axiom"
  | ppTerm BOT_TERM = "bottom"
  | ppTerm INT_TERM = "int"
  | ppTerm VOI_TERM = "void"
  | ppTerm DUM_TERM = raise Fail "ppTerm:DUM_TERM"
  | ppTerm (ATM_TERM NONE) = "atom"
  | ppTerm (ATM_TERM (SOME 2)) = "Loc"
  | ppTerm (TOK_TERM (t,k)) = t
  | ppTerm (NAT_TERM n) = II.toString n
  | ppTerm (IAX_TERM (rterm1, rterm2, rterm3)) =
    "isaxiom(" ^ ppRTerm rterm1 ^ "," ^ ppRTerm rterm2 ^ "," ^ ppRTerm rterm3 ^ ")"
  | ppTerm (IPA_TERM (rterm1, rterm2, rterm3)) =
    "ispair(" ^ ppRTerm rterm1 ^ "," ^ ppRTerm rterm2 ^ "," ^ ppRTerm rterm3 ^ ")"
  | ppTerm (SPR_TERM (pair, v1, v2, rterm)) =
    "(let " ^ dest_nuprl_var v1 ^ "," ^ dest_nuprl_var v2 ^ " = " ^ ppRTerm pair ^ " in " ^ ppRTerm rterm ^ ")"
  | ppTerm (DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    "(case " ^ ppRTerm dec ^ " of inl(" ^ dest_nuprl_var var1 ^ ") => " ^ ppRTerm rterm1 ^ " | inr(" ^ dest_nuprl_var var2 ^ ") => " ^ ppRTerm rterm2 ^ ")"
  | ppTerm (IEQ_TERM (rterm1, rterm2, rterm3, rterm4)) =
    "int_eq(" ^ ppRTerm rterm1 ^ "," ^ ppRTerm rterm2 ^ "," ^ ppRTerm rterm3 ^ "," ^ ppRTerm rterm4 ^ ")"
  | ppTerm (AEQ_TERM (n, rterm1, rterm2, rterm3, rterm4)) =
    "(if " ^ ppRTerm rterm1 ^ " =" ^ Int.toString n ^ " " ^ ppRTerm rterm2 ^ " then " ^ ppRTerm rterm3 ^ " else " ^ ppRTerm rterm4 ^ ")"
  | ppTerm (CBA_TERM (arg, x, B)) =
    "(let " ^ dest_nuprl_var x ^ " <- " ^ ppRTerm arg ^ " in " ^ ppRTerm B ^ ")"
  | ppTerm (TERM ((("apply", _), []), [B_TERM ([], f), B_TERM ([], a)])) =
    "((" ^ ppRTerm f ^ ")(" ^ ppRTerm a ^ "))"
  | ppTerm (TERM ((("int_eq", _), []), [B_TERM ([], x), B_TERM ([], y), B_TERM ([], w), B_TERM ([], z)])) =
    "(if " ^ ppRTerm x ^ " = " ^ ppRTerm y ^ " then " ^ ppRTerm w ^ " else " ^ ppRTerm z ^ ")"
  | ppTerm (TERM ((("subtract", _), []), [B_TERM ([], x), B_TERM ([], y)])) =
    ppRTerm x ^ "-" ^ ppRTerm y
  | ppTerm (TERM ((("add", _), []), [B_TERM ([], x), B_TERM ([], y)])) =
    ppRTerm x ^ "+" ^ ppRTerm y
  | ppTerm (TERM ((("pair", _), []), [B_TERM ([], x), B_TERM ([], y)])) =
    "(" ^ ppRTerm x ^ "," ^ ppRTerm y ^ ")"
  | ppTerm (TERM ((("variable", _),       [(v,vkind)]), [])) = v
  | ppTerm (TERM ((("natural_number", _), [(n,nkind)]), [])) = n
  | ppTerm (TERM ((("ycomb", _), []), [])) = "Y"
  | ppTerm (TERM ((("lambda", _), []), [B_TERM ([x], f)])) =
    "\\" ^ dest_nuprl_var x ^ ". " ^ ppRTerm f
  | ppTerm (TERM ((("axiom", _), []), [])) = "axiom"
  | ppTerm (TERM ((("nil", _), []), [])) = "[]"
  | ppTerm (TERM ((("inl", _), []), [B_TERM ([], t)])) = "inl(" ^ ppRTerm t ^ ")"
  | ppTerm (TERM ((("inr", _), []), [B_TERM ([], t)])) = "inr(" ^ ppRTerm t ^ ")"
  | ppTerm (TERM ((("fix", _), []), [B_TERM ([], t)])) = "fix(" ^ ppRTerm t ^ ")"
  | ppTerm (TERM ((("ifthenelse", _), []), [B_TERM ([], b), B_TERM ([], t1), B_TERM ([], t2)])) =
    "if " ^ ppRTerm b ^ "\nthen " ^ ppRTerm t1 ^ "\nelse " ^ ppRTerm t2
  | ppTerm (TERM ((("decide", _), []), [B_TERM ([], dec), B_TERM ([v1], t1), B_TERM ([v2], t2)])) =
    "case " ^ ppRTerm dec ^ " of inl(" ^ dest_nuprl_var v1 ^ ") => " ^ ppRTerm t1 ^ " | inr(" ^ dest_nuprl_var v2 ^ ") => " ^ ppRTerm t2
  | ppTerm (TERM ((("spread", _), []), [B_TERM ([], pair), B_TERM ([x1,x2], t)])) =
    "let " ^ dest_nuprl_var x1 ^ "," ^ dest_nuprl_var x2 ^ " = " ^ ppRTerm pair ^ " in " ^ ppRTerm t
  | ppTerm (TERM ((("list_ind", _), []), [B_TERM ([], L), B_TERM ([], base), B_TERM ([x,xs], reccase)])) =
    "rec-case " ^ ppRTerm L ^ " of [] => " ^ ppRTerm base ^ " | " ^ dest_nuprl_var x ^ "." ^ dest_nuprl_var xs ^ " => " ^ ppRTerm reccase
  | ppTerm (TERM ((("callbyvalue", _), []), [B_TERM ([], arg), B_TERM ([x], B)])) =
    "let " ^ dest_nuprl_var x ^ " := " ^ ppRTerm arg ^ "\nin " ^ ppRTerm B
  | ppTerm (TERM ((("callbyvalueall", _), []), [B_TERM ([], arg), B_TERM ([x], B)])) =
    "let " ^ dest_nuprl_var x ^ " <- " ^ ppRTerm arg ^ "\nin " ^ ppRTerm B
  | ppTerm (TERM ((("product", _), []), [B_TERM ([], t1), B_TERM ([v], t2)])) =
    if is_null_nuprl_var v
    then "(" ^ ppRTerm t1 ^ " * " ^ ppRTerm t2 ^ ")"
    else raise Fail "ppTerm:product"
  | ppTerm (TERM ((("union", _), []), [B_TERM ([], t1), B_TERM ([], t2)])) =
    "(" ^ ppRTerm t1 ^ " + " ^ ppRTerm t2 ^ ")"
  | ppTerm (TERM ((("function", _), []), [B_TERM ([], t1), B_TERM ([v], t2)])) =
    if is_null_nuprl_var v
    then "(" ^ ppRTerm t1 ^ " -> " ^ ppRTerm t2 ^ ")"
    else raise Fail "ppTerm:function"
  | ppTerm (TERM ((("equal", _), []), [B_TERM ([], t), B_TERM ([], t1), B_TERM ([], t2)])) =
    "(" ^ ppRTerm t1 ^ " = " ^ ppRTerm t2 ^ " in " ^ ppRTerm t ^ ")"
  | ppTerm (TERM (((opid, _), params), bterms)) = opid ^ "(" ^ ppBTerms bterms ^ ")"
  | ppTerm term = toStringTerm term

and ppBTerms bterms =
    T.fmt {init  = "",
	   final = "",
	   sep   = ",",
	   fmt   = ppBTerm}
	  bterms

and ppBTerm (B_TERM (vars, rterm)) =
    T.fmt {init  = "",
	   final = "",
	   sep   = ".",
	   fmt   = fn x => dest_nuprl_var x} vars
    ^ ppRTerm rterm

and ppRTerm ref_term = ppTerm (rterm2term ref_term)


(* ------ tests all subterms of a term are covered by our constructors ------ *)

fun test_full_coverage term =
    case term of
	TERM _ =>
	let val opid = opid_of_term term
	    val _ = print ("test_full_coverage: " ^ toStringTerm term ^ " not covered\n")
	in ()
	end
      | AXM_TERM => ()
      | BOT_TERM => ()
      | INT_TERM => ()
      | VOI_TERM => ()
      | DUM_TERM => ()
      | ATM_TERM nop => ()
      | TOK_TERM par => ()
      | NAT_TERM nat => ()
      | VAR_TERM var => ()
      | INL_TERM rterm => test_full_coverage (rterm2term rterm)
      | INR_TERM rterm => test_full_coverage (rterm2term rterm)
      | FIX_TERM rterm => test_full_coverage (rterm2term rterm)
      | MIN_TERM rterm => test_full_coverage (rterm2term rterm)
      | LAM_TERM (var, rterm) => test_full_coverage (rterm2term rterm)
      | REC_TERM (var, rterm) => test_full_coverage (rterm2term rterm)
      | WAI_TERM (rterm1, rterm2) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | APP_TERM (rterm1, rterm2) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | PAI_TERM (rterm1, rterm2) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | ADD_TERM (rterm1, rterm2) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | SUB_TERM (rterm1, rterm2) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | MUL_TERM (rterm1, rterm2) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | DIV_TERM (rterm1, rterm2) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | REM_TERM (rterm1, rterm2) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | EQT_TERM (rterm1, rterm2) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | UNI_TERM (rterm1, rterm2) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | EQU_TERM (rterm1, rterm2, rterm3) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2); test_full_coverage (rterm2term rterm3))
      | IAX_TERM (rterm1, rterm2, rterm3) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2); test_full_coverage (rterm2term rterm3))
      | IPA_TERM (rterm1, rterm2, rterm3) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2); test_full_coverage (rterm2term rterm3))
      | IIR_TERM (rterm1, rterm2, rterm3) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2); test_full_coverage (rterm2term rterm3))
      | IIL_TERM (rterm1, rterm2, rterm3) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2); test_full_coverage (rterm2term rterm3))
      | IIN_TERM (rterm1, rterm2, rterm3) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2); test_full_coverage (rterm2term rterm3))
      | ILA_TERM (rterm1, rterm2, rterm3) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2); test_full_coverage (rterm2term rterm3))
      | IAT_TERM (rterm1, rterm2, rterm3) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2); test_full_coverage (rterm2term rterm3))
      | CBV_TERM (a, x, f) => (test_full_coverage (rterm2term a); test_full_coverage (rterm2term f))
      | CBA_TERM (a, x, f) => (test_full_coverage (rterm2term a); test_full_coverage (rterm2term f))
      | FUN_TERM (a, x, f) => (test_full_coverage (rterm2term a); test_full_coverage (rterm2term f))
      | PRD_TERM (a, x, f) => (test_full_coverage (rterm2term a); test_full_coverage (rterm2term f))
      | TUN_TERM (a, x, f) => (test_full_coverage (rterm2term a); test_full_coverage (rterm2term f))
      | SET_TERM (a, x, f) => (test_full_coverage (rterm2term a); test_full_coverage (rterm2term f))
      | LES_TERM (a, b, rterm1, rterm2) => (test_full_coverage (rterm2term a); test_full_coverage (rterm2term b); test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | IEQ_TERM (a, b, rterm1, rterm2) => (test_full_coverage (rterm2term a); test_full_coverage (rterm2term b); test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | SPR_TERM (p, var1, var2, rterm) => (test_full_coverage (rterm2term p); test_full_coverage (rterm2term rterm))
      | AEQ_TERM (n, a, b, rterm1, rterm2) => (test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | DEC_TERM (dec, var1, rterm1, var2, rterm2) => (test_full_coverage (rterm2term dec); test_full_coverage (rterm2term rterm1); test_full_coverage (rterm2term rterm2))
      | IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase) => (test_full_coverage (rterm2term i); test_full_coverage (rterm2term downcase); test_full_coverage (rterm2term basecase); test_full_coverage (rterm2term upcase))
      | CLO_TERM _ => raise Fail "test_full_coverage:CLO_TERM"


(* ------ ACCESSORS ------ *)

fun opr_of_term (TERM (((opid, _), params), _)) = (opid, params)
  | opr_of_term AXM_TERM     = ("axiom",          [])
  | opr_of_term BOT_TERM     = ("bottom",         [])
  | opr_of_term INT_TERM     = ("int",            [])
  | opr_of_term VOI_TERM     = ("void",           [])
  | opr_of_term DUM_TERM     = raise Fail "opr_of_term:DUM_TERM"
  | opr_of_term (ATM_TERM x) = ("atom",           case x of NONE => [] | SOME n => [(Int.toString n, "n")])
  | opr_of_term (TOK_TERM x) = ("token",          [x])
  | opr_of_term (NAT_TERM n) = ("natural_number", [(II.toString n, "n")])
  | opr_of_term (VAR_TERM v) = ("variable",       [(dest_nuprl_var v, "v")])
  | opr_of_term (INL_TERM _) = ("inl",            [])
  | opr_of_term (INR_TERM _) = ("inr",            [])
  | opr_of_term (FIX_TERM _) = ("fix",            [])
  | opr_of_term (MIN_TERM _) = ("minus",          [])
  | opr_of_term (LAM_TERM _) = ("lambda",         [])
  | opr_of_term (REC_TERM _) = ("rec",            [])
  | opr_of_term (WAI_TERM _) = ("!wait",          [])
  | opr_of_term (APP_TERM _) = ("apply",          [])
  | opr_of_term (PAI_TERM _) = ("pair",           [])
  | opr_of_term (ADD_TERM _) = ("add",            [])
  | opr_of_term (SUB_TERM _) = ("subtract",       [])
  | opr_of_term (MUL_TERM _) = ("multiply",       [])
  | opr_of_term (DIV_TERM _) = ("divide",         [])
  | opr_of_term (REM_TERM _) = ("remainder",      [])
  | opr_of_term (EQT_TERM _) = ("eq_term",        [])
  | opr_of_term (UNI_TERM _) = ("union",          [])
  | opr_of_term (EQU_TERM _) = ("equal",          [])
  | opr_of_term (LES_TERM _) = ("less",           [])
  | opr_of_term (IEQ_TERM _) = ("int_eq",         [])
  | opr_of_term (IAX_TERM _) = ("isaxiom",        [])
  | opr_of_term (IPA_TERM _) = ("ispair",         [])
  | opr_of_term (IIR_TERM _) = ("isinr",          [])
  | opr_of_term (IIL_TERM _) = ("isinl",          [])
  | opr_of_term (IIN_TERM _) = ("isint",          [])
  | opr_of_term (ILA_TERM _) = ("islambda",       [])
  | opr_of_term (IAT_TERM _) = ("isatom2",        [])
  | opr_of_term (CBV_TERM _) = ("callbyvalue",    [])
  | opr_of_term (CBA_TERM _) = ("callbyvalueall", [])
  | opr_of_term (FUN_TERM _) = ("function",       [])
  | opr_of_term (PRD_TERM _) = ("product",        [])
  | opr_of_term (TUN_TERM _) = ("tunion",         [])
  | opr_of_term (SET_TERM _) = ("set",            [])
  | opr_of_term (SPR_TERM _) = ("spread",         [])
  | opr_of_term (AEQ_TERM (n,_,_,_,_)) = ("atom_eq", [(Int.toString n, "n")])
  | opr_of_term (DEC_TERM _) = ("decide",         [])
  | opr_of_term (IND_TERM _) = ("ind",            [])
  | opr_of_term (CLO_TERM clos) = raise Fail "opr_of_term"

fun parameters_of_term (TERM (((_, _), params), _)) = params
  | parameters_of_term AXM_TERM     = []
  | parameters_of_term BOT_TERM     = []
  | parameters_of_term INT_TERM     = []
  | parameters_of_term VOI_TERM     = []
  | parameters_of_term DUM_TERM     = raise Fail "parameters_of_term:DUM_TERM"
  | parameters_of_term (ATM_TERM x) = (case x of NONE => [] | SOME n => [(Int.toString n,"n")])
  | parameters_of_term (TOK_TERM x) = [x]
  | parameters_of_term (NAT_TERM n) = [(II.toString n,"n")]
  | parameters_of_term (VAR_TERM v) = [(dest_nuprl_var v,"v")]
  | parameters_of_term (INL_TERM _) = []
  | parameters_of_term (INR_TERM _) = []
  | parameters_of_term (FIX_TERM _) = []
  | parameters_of_term (MIN_TERM _) = []
  | parameters_of_term (LAM_TERM _) = []
  | parameters_of_term (REC_TERM _) = []
  | parameters_of_term (WAI_TERM _) = []
  | parameters_of_term (APP_TERM _) = []
  | parameters_of_term (PAI_TERM _) = []
  | parameters_of_term (ADD_TERM _) = []
  | parameters_of_term (SUB_TERM _) = []
  | parameters_of_term (MUL_TERM _) = []
  | parameters_of_term (DIV_TERM _) = []
  | parameters_of_term (REM_TERM _) = []
  | parameters_of_term (EQT_TERM _) = []
  | parameters_of_term (UNI_TERM _) = []
  | parameters_of_term (EQU_TERM _) = []
  | parameters_of_term (IPA_TERM _) = []
  | parameters_of_term (IAX_TERM _) = []
  | parameters_of_term (IIR_TERM _) = []
  | parameters_of_term (IIL_TERM _) = []
  | parameters_of_term (IIN_TERM _) = []
  | parameters_of_term (ILA_TERM _) = []
  | parameters_of_term (IAT_TERM _) = []
  | parameters_of_term (CBV_TERM _) = []
  | parameters_of_term (CBA_TERM _) = []
  | parameters_of_term (FUN_TERM _) = []
  | parameters_of_term (PRD_TERM _) = []
  | parameters_of_term (TUN_TERM _) = []
  | parameters_of_term (SET_TERM _) = []
  | parameters_of_term (LES_TERM _) = []
  | parameters_of_term (IEQ_TERM _) = []
  | parameters_of_term (SPR_TERM _) = []
  | parameters_of_term (AEQ_TERM (n,_,_,_,_)) = [(Int.toString n, "n")]
  | parameters_of_term (DEC_TERM _) = []
  | parameters_of_term (IND_TERM _) = []
  | parameters_of_term (CLO_TERM clos) = raise Fail "parameters_of_term"

fun bterms_of_term (TERM (_, bterms))          = map (fn (B_TERM (vars, rterm)) => (vars, rterm)) bterms
  | bterms_of_term AXM_TERM                    = []
  | bterms_of_term BOT_TERM                    = []
  | bterms_of_term INT_TERM                    = []
  | bterms_of_term VOI_TERM                    = []
  | bterms_of_term DUM_TERM                    = raise Fail "bterms_of_term:DUM_TERM"
  | bterms_of_term (ATM_TERM _)                = []
  | bterms_of_term (TOK_TERM _)                = []
  | bterms_of_term (NAT_TERM _)                = []
  | bterms_of_term (VAR_TERM _)                = []
  | bterms_of_term (INL_TERM rterm)            = [([],rterm)]
  | bterms_of_term (INR_TERM rterm)            = [([],rterm)]
  | bterms_of_term (FIX_TERM rterm)            = [([],rterm)]
  | bterms_of_term (MIN_TERM rterm)            = [([],rterm)]
  | bterms_of_term (LAM_TERM (var, rterm))     = [([var],rterm)]
  | bterms_of_term (REC_TERM (var, rterm))     = [([var],rterm)]
  | bterms_of_term (WAI_TERM (rterm1, rterm2)) = [([],rterm1), ([],rterm2)]
  | bterms_of_term (APP_TERM (rterm1, rterm2)) = [([],rterm1), ([],rterm2)]
  | bterms_of_term (PAI_TERM (rterm1, rterm2)) = [([],rterm1), ([],rterm2)]
  | bterms_of_term (ADD_TERM (rterm1, rterm2)) = [([],rterm1), ([],rterm2)]
  | bterms_of_term (SUB_TERM (rterm1, rterm2)) = [([],rterm1), ([],rterm2)]
  | bterms_of_term (MUL_TERM (rterm1, rterm2)) = [([],rterm1), ([],rterm2)]
  | bterms_of_term (DIV_TERM (rterm1, rterm2)) = [([],rterm1), ([],rterm2)]
  | bterms_of_term (REM_TERM (rterm1, rterm2)) = [([],rterm1), ([],rterm2)]
  | bterms_of_term (EQT_TERM (rterm1, rterm2)) = [([],rterm1), ([],rterm2)]
  | bterms_of_term (UNI_TERM (rterm1, rterm2)) = [([],rterm1), ([],rterm2)]
  | bterms_of_term (EQU_TERM (a, rterm1, rterm2)) = [([],a), ([],rterm1), ([],rterm2)]
  | bterms_of_term (IAX_TERM (a, rterm1, rterm2)) = [([],a), ([],rterm1), ([],rterm2)]
  | bterms_of_term (IPA_TERM (a, rterm1, rterm2)) = [([],a), ([],rterm1), ([],rterm2)]
  | bterms_of_term (IIR_TERM (a, rterm1, rterm2)) = [([],a), ([],rterm1), ([],rterm2)]
  | bterms_of_term (IIL_TERM (a, rterm1, rterm2)) = [([],a), ([],rterm1), ([],rterm2)]
  | bterms_of_term (IIN_TERM (a, rterm1, rterm2)) = [([],a), ([],rterm1), ([],rterm2)]
  | bterms_of_term (ILA_TERM (a, rterm1, rterm2)) = [([],a), ([],rterm1), ([],rterm2)]
  | bterms_of_term (IAT_TERM (a, rterm1, rterm2)) = [([],a), ([],rterm1), ([],rterm2)]
  | bterms_of_term (CBV_TERM (a, x, f)) = [([],a), ([x],f)]
  | bterms_of_term (CBA_TERM (a, x, f)) = [([],a), ([x],f)]
  | bterms_of_term (FUN_TERM (a, x, f)) = [([],a), ([x],f)]
  | bterms_of_term (PRD_TERM (a, x, f)) = [([],a), ([x],f)]
  | bterms_of_term (TUN_TERM (a, x, f)) = [([],a), ([x],f)]
  | bterms_of_term (SET_TERM (a, x, f)) = [([],a), ([x],f)]
  | bterms_of_term (LES_TERM (a, b, rterm1, rterm2)) = [([],a), ([],b), ([],rterm1), ([],rterm2)]
  | bterms_of_term (IEQ_TERM (a, b, rterm1, rterm2)) = [([],a), ([],b), ([],rterm1), ([],rterm2)]
  | bterms_of_term (SPR_TERM (pair, var1, var2, rterm)) = [([],pair), ([var1,var2],rterm)]
  | bterms_of_term (AEQ_TERM (n, a, b, rterm1, rterm2)) = [([],a), ([],b), ([],rterm1), ([],rterm2)]
  | bterms_of_term (DEC_TERM (dec, var1, rterm1, var2, rterm2)) = [([],dec), ([var1],rterm1), ([var2],rterm2)]
  | bterms_of_term (IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) = [([],i), ([x,rd],downcase), ([],basecase), ([x,ru],upcase)]
  | bterms_of_term (CLO_TERM clos) = raise Fail "bterms_of_term"

fun brterms_of_term (TERM (_, bterms))          = map (fn (B_TERM (vars, rterm)) => (vars, rterm2term rterm)) bterms
  | brterms_of_term AXM_TERM                    = []
  | brterms_of_term BOT_TERM                    = []
  | brterms_of_term INT_TERM                    = []
  | brterms_of_term VOI_TERM                    = []
  | brterms_of_term DUM_TERM                    = raise Fail "brterms_of_term:DUM_TERM"
  | brterms_of_term (ATM_TERM _)                = []
  | brterms_of_term (TOK_TERM _)                = []
  | brterms_of_term (NAT_TERM _)                = []
  | brterms_of_term (VAR_TERM _)                = []
  | brterms_of_term (INL_TERM rterm)            = [([],rterm2term rterm)]
  | brterms_of_term (INR_TERM rterm)            = [([],rterm2term rterm)]
  | brterms_of_term (FIX_TERM rterm)            = [([],rterm2term rterm)]
  | brterms_of_term (MIN_TERM rterm)            = [([],rterm2term rterm)]
  | brterms_of_term (LAM_TERM (var, rterm))     = [([var],rterm2term rterm)]
  | brterms_of_term (REC_TERM (var, rterm))     = [([var],rterm2term rterm)]
  | brterms_of_term (WAI_TERM (rterm1, rterm2)) = [([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (APP_TERM (rterm1, rterm2)) = [([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (PAI_TERM (rterm1, rterm2)) = [([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (ADD_TERM (rterm1, rterm2)) = [([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (SUB_TERM (rterm1, rterm2)) = [([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (MUL_TERM (rterm1, rterm2)) = [([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (DIV_TERM (rterm1, rterm2)) = [([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (REM_TERM (rterm1, rterm2)) = [([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (EQT_TERM (rterm1, rterm2)) = [([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (UNI_TERM (rterm1, rterm2)) = [([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (EQU_TERM (a, rterm1, rterm2)) = [([],rterm2term a), ([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (IAX_TERM (a, rterm1, rterm2)) = [([],rterm2term a), ([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (IPA_TERM (a, rterm1, rterm2)) = [([],rterm2term a), ([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (IIR_TERM (a, rterm1, rterm2)) = [([],rterm2term a), ([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (IIL_TERM (a, rterm1, rterm2)) = [([],rterm2term a), ([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (IIN_TERM (a, rterm1, rterm2)) = [([],rterm2term a), ([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (ILA_TERM (a, rterm1, rterm2)) = [([],rterm2term a), ([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (IAT_TERM (a, rterm1, rterm2)) = [([],rterm2term a), ([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (CBV_TERM (a, x, f)) = [([],rterm2term a), ([x],rterm2term f)]
  | brterms_of_term (CBA_TERM (a, x, f)) = [([],rterm2term a), ([x],rterm2term f)]
  | brterms_of_term (FUN_TERM (a, x, f)) = [([],rterm2term a), ([x],rterm2term f)]
  | brterms_of_term (PRD_TERM (a, x, f)) = [([],rterm2term a), ([x],rterm2term f)]
  | brterms_of_term (TUN_TERM (a, x, f)) = [([],rterm2term a), ([x],rterm2term f)]
  | brterms_of_term (SET_TERM (a, x, f)) = [([],rterm2term a), ([x],rterm2term f)]
  | brterms_of_term (LES_TERM (a, b, rterm1, rterm2)) = [([],rterm2term a), ([],rterm2term b), ([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (IEQ_TERM (a, b, rterm1, rterm2)) = [([],rterm2term a), ([],rterm2term b), ([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (SPR_TERM (pair, var1, var2, rterm)) = [([],rterm2term pair), ([var1,var2],rterm2term rterm)]
  | brterms_of_term (AEQ_TERM (n, a, b, rterm1, rterm2)) = [([],rterm2term a), ([],rterm2term b), ([],rterm2term rterm1), ([],rterm2term rterm2)]
  | brterms_of_term (DEC_TERM (dec, var1, rterm1, var2, rterm2)) = [([],rterm2term dec), ([var1],rterm2term rterm1), ([var2],rterm2term rterm2)]
  | brterms_of_term (IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) = [([],rterm2term i), ([x,rd],rterm2term downcase), ([],rterm2term basecase), ([y,ru],rterm2term upcase)]
  | brterms_of_term (CLO_TERM clos) = raise Fail "brterms_of_term"

fun subterms (TERM (_, bterms))          = map (fn (B_TERM (_, rterm)) => rterm) bterms
  | subterms AXM_TERM                    = []
  | subterms BOT_TERM                    = []
  | subterms INT_TERM                    = []
  | subterms VOI_TERM                    = []
  | subterms DUM_TERM                    = raise Fail "subterms:DUM_TERM"
  | subterms (ATM_TERM _)                = []
  | subterms (TOK_TERM _)                = []
  | subterms (NAT_TERM _)                = []
  | subterms (VAR_TERM _)                = []
  | subterms (INL_TERM rterm)            = [rterm]
  | subterms (INR_TERM rterm)            = [rterm]
  | subterms (FIX_TERM rterm)            = [rterm]
  | subterms (MIN_TERM rterm)            = [rterm]
  | subterms (LAM_TERM (_, rterm))       = [rterm]
  | subterms (REC_TERM (_, rterm))       = [rterm]
  | subterms (WAI_TERM (rterm1, rterm2)) = [rterm1, rterm2]
  | subterms (APP_TERM (rterm1, rterm2)) = [rterm1, rterm2]
  | subterms (PAI_TERM (rterm1, rterm2)) = [rterm1, rterm2]
  | subterms (ADD_TERM (rterm1, rterm2)) = [rterm1, rterm2]
  | subterms (SUB_TERM (rterm1, rterm2)) = [rterm1, rterm2]
  | subterms (MUL_TERM (rterm1, rterm2)) = [rterm1, rterm2]
  | subterms (DIV_TERM (rterm1, rterm2)) = [rterm1, rterm2]
  | subterms (REM_TERM (rterm1, rterm2)) = [rterm1, rterm2]
  | subterms (EQT_TERM (rterm1, rterm2)) = [rterm1, rterm2]
  | subterms (UNI_TERM (rterm1, rterm2)) = [rterm1, rterm2]
  | subterms (EQU_TERM (a, rterm1, rterm2)) = [a, rterm1, rterm2]
  | subterms (IAX_TERM (a, rterm1, rterm2)) = [a, rterm1, rterm2]
  | subterms (IPA_TERM (a, rterm1, rterm2)) = [a, rterm1, rterm2]
  | subterms (IIR_TERM (a, rterm1, rterm2)) = [a, rterm1, rterm2]
  | subterms (IIL_TERM (a, rterm1, rterm2)) = [a, rterm1, rterm2]
  | subterms (IIN_TERM (a, rterm1, rterm2)) = [a, rterm1, rterm2]
  | subterms (ILA_TERM (a, rterm1, rterm2)) = [a, rterm1, rterm2]
  | subterms (IAT_TERM (a, rterm1, rterm2)) = [a, rterm1, rterm2]
  | subterms (CBV_TERM (a, x, f)) = [a, f]
  | subterms (CBA_TERM (a, x, f)) = [a, f]
  | subterms (FUN_TERM (a, x, f)) = [a, f]
  | subterms (PRD_TERM (a, x, f)) = [a, f]
  | subterms (TUN_TERM (a, x, f)) = [a, f]
  | subterms (SET_TERM (a, x, f)) = [a, f]
  | subterms (LES_TERM (a, b, rterm1, rterm2)) = [a, b, rterm1, rterm2]
  | subterms (IEQ_TERM (a, b, rterm1, rterm2)) = [a, b, rterm1, rterm2]
  | subterms (SPR_TERM (pair, var1, var2, rterm)) = [pair, rterm]
  | subterms (AEQ_TERM (n, a, b, rterm1, rterm2)) = [a, b, rterm1, rterm2]
  | subterms (DEC_TERM (dec, var1, rterm1, var2, rterm2)) = [dec, rterm1, rterm2]
  | subterms (IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) = [i, downcase, basecase, upcase]
  | subterms (CLO_TERM clos) = raise Fail "subterms"

fun subtermn n (TERM (_, bterm_list)) =
    (case List.nth (bterm_list, n - 1) of
	 B_TERM ([], t) => rterm2term t
       | _ => raise Fail "subtermn")
  | subtermn n AXM_TERM = raise Fail "subtermn:AXM_TERM"
  | subtermn n BOT_TERM = raise Fail "subtermn:BOT_TERM"
  | subtermn n INT_TERM = raise Fail "subtermn:INT_TERM"
  | subtermn n VOI_TERM = raise Fail "subtermn:VOI_TERM"
  | subtermn n DUM_TERM = raise Fail "subtermn:DUM_TERM"
  | subtermn n (ATM_TERM _) = raise Fail "subtermn:ATM_TERM"
  | subtermn n (TOK_TERM _) = raise Fail "subtermn:TOK_TERM"
  | subtermn n (NAT_TERM _) = raise Fail "subtermn:NAT_TERM"
  | subtermn n (VAR_TERM _) = raise Fail "subtermn:VAR_TERM"
  | subtermn n (INL_TERM rterm) =
    if n = 1
    then rterm2term rterm
    else raise Fail "subtermn:INL_TERM"
  | subtermn n (INR_TERM rterm) =
    if n = 1
    then rterm2term rterm
    else raise Fail "subtermn:INR_TERM"
  | subtermn n (FIX_TERM rterm) =
    if n = 1
    then rterm2term rterm
    else raise Fail "subtermn:FIX_TERM"
  | subtermn n (MIN_TERM rterm) =
    if n = 1
    then rterm2term rterm
    else raise Fail "subtermn:MIN_TERM"
  | subtermn n (LAM_TERM _) = raise Fail "subtermn:LAM_TERM"
  | subtermn n (REC_TERM _) = raise Fail "subtermn:REC_TERM"
  | subtermn n (WAI_TERM (rterm1, rterm2)) =
    if n = 1
    then rterm2term rterm1
    else if n = 2
    then rterm2term rterm2
    else raise Fail "subtermn:WAI_TERM"
  | subtermn n (APP_TERM (rterm1, rterm2)) =
    if n = 1
    then rterm2term rterm1
    else if n = 2
    then rterm2term rterm2
    else raise Fail "subtermn:APP_TERM"
  | subtermn n (PAI_TERM (rterm1, rterm2)) =
    if n = 1
    then rterm2term rterm1
    else if n = 2
    then rterm2term rterm2
    else raise Fail "subtermn:PAI_TERM"
  | subtermn n (ADD_TERM (rterm1, rterm2)) =
    if n = 1
    then rterm2term rterm1
    else if n = 2
    then rterm2term rterm2
    else raise Fail "subtermn:ADD_TERM"
  | subtermn n (SUB_TERM (rterm1, rterm2)) =
    if n = 1
    then rterm2term rterm1
    else if n = 2
    then rterm2term rterm2
    else raise Fail "subtermn:SUB_TERM"
  | subtermn n (MUL_TERM (rterm1, rterm2)) =
    if n = 1
    then rterm2term rterm1
    else if n = 2
    then rterm2term rterm2
    else raise Fail "subtermn:MUL_TERM"
  | subtermn n (DIV_TERM (rterm1, rterm2)) =
    if n = 1
    then rterm2term rterm1
    else if n = 2
    then rterm2term rterm2
    else raise Fail "subtermn:DIV_TERM"
  | subtermn n (REM_TERM (rterm1, rterm2)) =
    if n = 1
    then rterm2term rterm1
    else if n = 2
    then rterm2term rterm2
    else raise Fail "subtermn:REM_TERM"
  | subtermn n (EQT_TERM (rterm1, rterm2)) =
    if n = 1
    then rterm2term rterm1
    else if n = 2
    then rterm2term rterm2
    else raise Fail "subtermn:EQT_TERM"
  | subtermn n (UNI_TERM (rterm1, rterm2)) =
    if n = 1
    then rterm2term rterm1
    else if n = 2
    then rterm2term rterm2
    else raise Fail "subtermn:UNI_TERM"
  | subtermn n (EQU_TERM (a, rterm1, rterm2)) =
    if n = 1
    then rterm2term a
    else if n = 2
    then rterm2term rterm1
    else if n = 3
    then rterm2term rterm2
    else raise Fail "subtermn:EQU_TERM"
  | subtermn n (IAX_TERM (a, rterm1, rterm2)) =
    if n = 1
    then rterm2term a
    else if n = 2
    then rterm2term rterm1
    else if n = 3
    then rterm2term rterm2
    else raise Fail "subtermn:IAX_TERM"
  | subtermn n (IPA_TERM (a, rterm1, rterm2)) =
    if n = 1
    then rterm2term a
    else if n = 2
    then rterm2term rterm1
    else if n = 3
    then rterm2term rterm2
    else raise Fail "subtermn:IPA_TERM"
  | subtermn n (IIR_TERM (a, rterm1, rterm2)) =
    if n = 1
    then rterm2term a
    else if n = 2
    then rterm2term rterm1
    else if n = 3
    then rterm2term rterm2
    else raise Fail "subtermn:IIR_TERM"
  | subtermn n (IIL_TERM (a, rterm1, rterm2)) =
    if n = 1
    then rterm2term a
    else if n = 2
    then rterm2term rterm1
    else if n = 3
    then rterm2term rterm2
    else raise Fail "subtermn:IIL_TERM"
  | subtermn n (IIN_TERM (a, rterm1, rterm2)) =
    if n = 1
    then rterm2term a
    else if n = 2
    then rterm2term rterm1
    else if n = 3
    then rterm2term rterm2
    else raise Fail "subtermn:IIN_TERM"
  | subtermn n (ILA_TERM (a, rterm1, rterm2)) =
    if n = 1
    then rterm2term a
    else if n = 2
    then rterm2term rterm1
    else if n = 3
    then rterm2term rterm2
    else raise Fail "subtermn:ILA_TERM"
  | subtermn n (IAT_TERM (a, rterm1, rterm2)) =
    if n = 1
    then rterm2term a
    else if n = 2
    then rterm2term rterm1
    else if n = 3
    then rterm2term rterm2
    else raise Fail "subtermn:IAT_TERM"
  | subtermn n (CBV_TERM (a, x, f)) =
    if n = 1
    then rterm2term a
    else raise Fail "subtermn:CBV_TERM"
  | subtermn n (CBA_TERM (a, x, f)) =
    if n = 1
    then rterm2term a
    else raise Fail "subtermn:CBA_TERM"
  | subtermn n (FUN_TERM (a, x, f)) =
    if n = 1
    then rterm2term a
    else raise Fail "subtermn:FUN_TERM"
  | subtermn n (PRD_TERM (a, x, f)) =
    if n = 1
    then rterm2term a
    else raise Fail "subtermn:PRD_TERM"
  | subtermn n (TUN_TERM (a, x, f)) =
    if n = 1
    then rterm2term a
    else raise Fail "subtermn:TUN_TERM"
  | subtermn n (SET_TERM (a, x, f)) =
    if n = 1
    then rterm2term a
    else raise Fail "subtermn:SET_TERM"
  | subtermn n (LES_TERM (a, b, rterm1, rterm2)) =
    if n = 1
    then rterm2term a
    else if n = 2
    then rterm2term b
    else if n = 3
    then rterm2term rterm1
    else if n = 4
    then rterm2term rterm2
    else raise Fail "subtermn:LES_TERM"
  | subtermn n (IEQ_TERM (a, b, rterm1, rterm2)) =
    if n = 1
    then rterm2term a
    else if n = 2
    then rterm2term b
    else if n = 3
    then rterm2term rterm1
    else if n = 4
    then rterm2term rterm2
    else raise Fail "subtermn:IEQ_TERM"
  | subtermn n (SPR_TERM (pair, var1, var2, rterm)) =
    if n = 1
    then rterm2term pair
    else raise Fail "subtermn:SPR_TERM"
  | subtermn n (AEQ_TERM (m, a, b, rterm1, rterm2)) =
    if n = 1
    then rterm2term a
    else if n = 2
    then rterm2term b
    else if n = 3
    then rterm2term rterm1
    else if n = 4
    then rterm2term rterm2
    else raise Fail "subtermn:AEQ_TERM"
  | subtermn n (DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    if n = 1
    then rterm2term dec
    else raise Fail "subtermn:DEC_TERM"
  | subtermn n (IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) =
    if n = 1
    then rterm2term i
    else if n = 3
    then rterm2term basecase
    else raise Fail "subtermn:IND_TERM"
  | subtermn n (CLO_TERM clos) = raise Fail "subtermn:CLO_TERM"

fun subterms_n n bterms =
    let val (_, rt) = List.nth (bterms, n)
    in rterm2term rt
    end

fun type_of_parameter  (_, kind)  = kind
fun value_of_parameter (value, _) = value

fun destruct_natural_parameter (n,"n") = Option.valOf (Int.fromString n)
  | destruct_natural_parameter _ = raise Fail "destruct_natural_parameter"

fun firstnat term = destruct_natural_parameter (hd (parameters_of_term term))

fun get_obid_parameters [] = NONE
  | get_obid_parameters ((p,k) :: xs) =
    if k = "o"
    then SOME p
    else get_obid_parameters xs

fun sign_to_string (lst1, lst2) =
    "("
    ^ (T.fmt {init  = "[",
	      final = "]",
	      sep   = ",",
	      fmt   = fn (SOME v, k) => toStringValue v ^ ":" ^ toStringKind k
		       | (NONE, k) => "-:" ^ toStringKind k}
	     lst1)
    ^ ","
    ^ (T.fmt {init  = "[",
	      final = "]",
	      sep   = ",",
	      fmt   = Int.toString}
	     lst2)
    ^ ")"

fun is_abstract_metavar v = String.isPrefix "%a" v

fun getSignature (term as TERM (_, lst)) =
    let val params = parameters_of_term term
	val types  = map (fn (v, k) =>
			     if is_abstract_metavar v
			     then (NONE, k)
			     else (SOME v, k))
			 params
	val subs   = map (fn (B_TERM (vs, _)) => List.length vs) lst
    in (types, subs)
    end
  | getSignature (term as AXM_TERM)   = raise Fail "getSignature:AXM_TERM"
  | getSignature (term as BOT_TERM)   = raise Fail "getSignature:BOT_TERM"
  | getSignature (term as INT_TERM)   = raise Fail "getSignature:INT_TERM"
  | getSignature (term as VOI_TERM)   = raise Fail "getSignature:VOI_TERM"
  | getSignature (term as DUM_TERM)   = raise Fail "getSignature:DUM_TERM"
  | getSignature (term as ATM_TERM _) = raise Fail "getSignature:ATM_TERM"
  | getSignature (term as TOK_TERM _) = raise Fail "getSignature:TOK_TERM"
  | getSignature (term as NAT_TERM _) = raise Fail "getSignature:NAT_TERM"
  | getSignature (term as VAR_TERM _) = raise Fail "getSignature:VAR_TERM"
  | getSignature (term as INL_TERM _) = raise Fail "getSignature:INL_TERM"
  | getSignature (term as INR_TERM _) = raise Fail "getSignature:INR_TERM"
  | getSignature (term as FIX_TERM _) = raise Fail "getSignature:FIX_TERM"
  | getSignature (term as MIN_TERM _) = raise Fail "getSignature:MIN_TERM"
  | getSignature (term as LAM_TERM _) = raise Fail "getSignature:LAM_TERM"
  | getSignature (term as REC_TERM _) = raise Fail "getSignature:REC_TERM"
  | getSignature (term as WAI_TERM _) = raise Fail "getSignature:WAI_TERM"
  | getSignature (term as APP_TERM _) = raise Fail "getSignature:APP_TERM"
  | getSignature (term as PAI_TERM _) = raise Fail "getSignature:PAI_TERM"
  | getSignature (term as ADD_TERM _) = raise Fail "getSignature:ADD_TERM"
  | getSignature (term as SUB_TERM _) = raise Fail "getSignature:SUB_TERM"
  | getSignature (term as MUL_TERM _) = raise Fail "getSignature:MUL_TERM"
  | getSignature (term as DIV_TERM _) = raise Fail "getSignature:DIV_TERM"
  | getSignature (term as REM_TERM _) = raise Fail "getSignature:REM_TERM"
  | getSignature (term as EQT_TERM _) = raise Fail "getSignature:EQT_TERM"
  | getSignature (term as UNI_TERM _) = raise Fail "getSignature:UNI_TERM"
  | getSignature (term as EQU_TERM _) = raise Fail "getSignature:EQU_TERM"
  | getSignature (term as IAX_TERM _) = raise Fail "getSignature:IAX_TERM"
  | getSignature (term as IPA_TERM _) = raise Fail "getSignature:IPA_TERM"
  | getSignature (term as IIR_TERM _) = raise Fail "getSignature:IIR_TERM"
  | getSignature (term as IIL_TERM _) = raise Fail "getSignature:IIL_TERM"
  | getSignature (term as IIN_TERM _) = raise Fail "getSignature:IIN_TERM"
  | getSignature (term as ILA_TERM _) = raise Fail "getSignature:ILA_TERM"
  | getSignature (term as IAT_TERM _) = raise Fail "getSignature:IAT_TERM"
  | getSignature (term as CBV_TERM _) = raise Fail "getSignature:CBV_TERM"
  | getSignature (term as CBA_TERM _) = raise Fail "getSignature:CBA_TERM"
  | getSignature (term as FUN_TERM _) = raise Fail "getSignature:FUN_TERM"
  | getSignature (term as PRD_TERM _) = raise Fail "getSignature:PRD_TERM"
  | getSignature (term as TUN_TERM _) = raise Fail "getSignature:TUN_TERM"
  | getSignature (term as SET_TERM _) = raise Fail "getSignature:SET_TERM"
  | getSignature (term as LES_TERM _) = raise Fail "getSignature:LES_TERM"
  | getSignature (term as IEQ_TERM _) = raise Fail "getSignature:IEQ_TERM"
  | getSignature (term as SPR_TERM _) = raise Fail "getSignature:SPR_TERM"
  | getSignature (term as AEQ_TERM _) = raise Fail "getSignature:AEQ_TERM"
  | getSignature (term as DEC_TERM _) = raise Fail "getSignature:DEC_TERM"
  | getSignature (term as IND_TERM _) = raise Fail "getSignature:IND_TERM"
  | getSignature (term as CLO_TERM _) = raise Fail "getSignature:CLO_TERM"

fun eq_kinds (k1, k2) =
    (k1 = "t" andalso k2 = "token")
    orelse
    (k2 = "t" andalso k1 = "token")
    orelse
    k1 = k2

fun eq_sign_kinds ((v1, k1), (v2, k2)) =
    (case (v1, v2) of
	 (SOME a, SOME b) => (a : parameter_value) = b
       | _ => true)
    andalso
    eq_kinds (k1, k2)

fun eq_signs ((kinds1, subs1) : sign)
	     ((kinds2, subs2) : sign) =
    (ListPair.allEq eq_sign_kinds (kinds1, kinds2)
     andalso
     subs1 = subs2)
    handle _ => false

fun simpleSignature (paramtypes, subs) =
    List.all (fn (_,"o") => true | _ => false) paramtypes
    andalso
    List.all (fn n => n = 0) subs

fun is_sml_primitive (TERM _) = false
  | is_sml_primitive _ = true


(* size of a term -- number of nodes *)
fun size (TERM (opr, bterms)) =
    foldr (fn (bterm, n) =>
	      let (*val _ = print ("[partial size: " ^ Int.toString n ^ "]\n")*)
	      in n + size_bterm bterm
	      end)
	  1
	  bterms
  | size AXM_TERM = 1
  | size BOT_TERM = 1
  | size INT_TERM = 1
  | size VOI_TERM = 1
  | size DUM_TERM = 1
  | size (ATM_TERM _) = 1
  | size (TOK_TERM _) = 1
  | size (NAT_TERM _) = 1
  | size (VAR_TERM _) = 1
  | size (INL_TERM rterm) = 1 + size_rterm rterm
  | size (INR_TERM rterm) = 1 + size_rterm rterm
  | size (FIX_TERM rterm) = 1 + size_rterm rterm
  | size (MIN_TERM rterm) = 1 + size_rterm rterm
  | size (LAM_TERM (_, rterm)) = 1 + size_rterm rterm
  | size (REC_TERM (_, rterm)) = 1 + size_rterm rterm
  | size (WAI_TERM (rterm1, rterm2)) = 1 + size_rterm rterm1 + size_rterm rterm2
  | size (APP_TERM (rterm1, rterm2)) = 1 + size_rterm rterm1 + size_rterm rterm2
  | size (PAI_TERM (rterm1, rterm2)) = 1 + size_rterm rterm1 + size_rterm rterm2
  | size (ADD_TERM (rterm1, rterm2)) = 1 + size_rterm rterm1 + size_rterm rterm2
  | size (SUB_TERM (rterm1, rterm2)) = 1 + size_rterm rterm1 + size_rterm rterm2
  | size (MUL_TERM (rterm1, rterm2)) = 1 + size_rterm rterm1 + size_rterm rterm2
  | size (DIV_TERM (rterm1, rterm2)) = 1 + size_rterm rterm1 + size_rterm rterm2
  | size (REM_TERM (rterm1, rterm2)) = 1 + size_rterm rterm1 + size_rterm rterm2
  | size (EQT_TERM (rterm1, rterm2)) = 1 + size_rterm rterm1 + size_rterm rterm2
  | size (UNI_TERM (rterm1, rterm2)) = 1 + size_rterm rterm1 + size_rterm rterm2
  | size (EQU_TERM (a, rterm1, rterm2)) = 1 + size_rterm a + size_rterm rterm1 + size_rterm rterm2
  | size (IAX_TERM (a, rterm1, rterm2)) = 1 + size_rterm a + size_rterm rterm1 + size_rterm rterm2
  | size (IPA_TERM (a, rterm1, rterm2)) = 1 + size_rterm a + size_rterm rterm1 + size_rterm rterm2
  | size (IIR_TERM (a, rterm1, rterm2)) = 1 + size_rterm a + size_rterm rterm1 + size_rterm rterm2
  | size (IIL_TERM (a, rterm1, rterm2)) = 1 + size_rterm a + size_rterm rterm1 + size_rterm rterm2
  | size (IIN_TERM (a, rterm1, rterm2)) = 1 + size_rterm a + size_rterm rterm1 + size_rterm rterm2
  | size (ILA_TERM (a, rterm1, rterm2)) = 1 + size_rterm a + size_rterm rterm1 + size_rterm rterm2
  | size (IAT_TERM (a, rterm1, rterm2)) = 1 + size_rterm a + size_rterm rterm1 + size_rterm rterm2
  | size (CBV_TERM (a, x, f)) = 1 + size_rterm a + size_rterm f
  | size (CBA_TERM (a, x, f)) = 1 + size_rterm a + size_rterm f
  | size (FUN_TERM (a, x, f)) = 1 + size_rterm a + size_rterm f
  | size (PRD_TERM (a, x, f)) = 1 + size_rterm a + size_rterm f
  | size (TUN_TERM (a, x, f)) = 1 + size_rterm a + size_rterm f
  | size (SET_TERM (a, x, f)) = 1 + size_rterm a + size_rterm f
  | size (LES_TERM (a, b, rterm1, rterm2)) = 1 + size_rterm a + size_rterm b + size_rterm rterm1 + size_rterm rterm2
  | size (IEQ_TERM (a, b, rterm1, rterm2)) = 1 + size_rterm a + size_rterm b + size_rterm rterm1 + size_rterm rterm2
  | size (SPR_TERM (pair, var1, var2, rterm)) = 1 + size_rterm pair + size_rterm rterm
  | size (AEQ_TERM (n, a, b, rterm1, rterm2)) = 1 + size_rterm a + size_rterm b + size_rterm rterm1 + size_rterm rterm2
  | size (DEC_TERM (dec, var1, rterm1, var2, rterm2)) = 1 + size_rterm dec + size_rterm rterm1 + size_rterm rterm2
  | size (IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) = 1 + size_rterm i + size_rterm downcase + size_rterm basecase + size_rterm upcase
  | size (CLO_TERM clos) = size_clos clos

and size_rterm rterm = size (rterm2term rterm)

and size_bterm (B_TERM (vars, rterm)) = size_rterm rterm

and size_clos (rterm, env) = size_rterm rterm + size_env env

and size_env (ENV env) =
    MAP.foldr
	(fn ((_,_,termref), n) => size (#1 (!termref)) + n)
	0
	env


(* size of a term -- number of nodes -- uses IntInf *)
fun large_size (TERM (opr, bterms)) =
    foldr (fn (bterm, n) => IntInf.+ (n, large_size_bterm bterm))
	  1
	  bterms
  | large_size AXM_TERM = 1
  | large_size BOT_TERM = 1
  | large_size INT_TERM = 1
  | large_size VOI_TERM = 1
  | large_size DUM_TERM = 1
  | large_size (ATM_TERM _) = 1
  | large_size (TOK_TERM _) = 1
  | large_size (NAT_TERM _) = 1
  | large_size (VAR_TERM _) = 1
  | large_size (INL_TERM rterm) = 1 + large_size_rterm rterm
  | large_size (INR_TERM rterm) = 1 + large_size_rterm rterm
  | large_size (FIX_TERM rterm) = 1 + large_size_rterm rterm
  | large_size (MIN_TERM rterm) = 1 + large_size_rterm rterm
  | large_size (LAM_TERM (_, rterm)) = 1 + large_size_rterm rterm
  | large_size (REC_TERM (_, rterm)) = 1 + large_size_rterm rterm
  | large_size (WAI_TERM (rterm1, rterm2)) = 1 + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (APP_TERM (rterm1, rterm2)) = 1 + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (PAI_TERM (rterm1, rterm2)) = 1 + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (ADD_TERM (rterm1, rterm2)) = 1 + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (SUB_TERM (rterm1, rterm2)) = 1 + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (MUL_TERM (rterm1, rterm2)) = 1 + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (DIV_TERM (rterm1, rterm2)) = 1 + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (REM_TERM (rterm1, rterm2)) = 1 + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (EQT_TERM (rterm1, rterm2)) = 1 + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (UNI_TERM (rterm1, rterm2)) = 1 + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (EQU_TERM (a, rterm1, rterm2)) = 1 + large_size_rterm a + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (IAX_TERM (a, rterm1, rterm2)) = 1 + large_size_rterm a + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (IPA_TERM (a, rterm1, rterm2)) = 1 + large_size_rterm a + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (IIR_TERM (a, rterm1, rterm2)) = 1 + large_size_rterm a + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (IIL_TERM (a, rterm1, rterm2)) = 1 + large_size_rterm a + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (IIN_TERM (a, rterm1, rterm2)) = 1 + large_size_rterm a + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (ILA_TERM (a, rterm1, rterm2)) = 1 + large_size_rterm a + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (IAT_TERM (a, rterm1, rterm2)) = 1 + large_size_rterm a + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (CBV_TERM (a, x, f)) = 1 + large_size_rterm a + large_size_rterm f
  | large_size (CBA_TERM (a, x, f)) = 1 + large_size_rterm a + large_size_rterm f
  | large_size (FUN_TERM (a, x, f)) = 1 + large_size_rterm a + large_size_rterm f
  | large_size (PRD_TERM (a, x, f)) = 1 + large_size_rterm a + large_size_rterm f
  | large_size (TUN_TERM (a, x, f)) = 1 + large_size_rterm a + large_size_rterm f
  | large_size (SET_TERM (a, x, f)) = 1 + large_size_rterm a + large_size_rterm f
  | large_size (LES_TERM (a, b, rterm1, rterm2)) = 1 + large_size_rterm a + large_size_rterm b + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (IEQ_TERM (a, b, rterm1, rterm2)) = 1 + large_size_rterm a + large_size_rterm b + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (SPR_TERM (pair, var1, var2, rterm)) = 1 + large_size_rterm pair + large_size_rterm rterm
  | large_size (AEQ_TERM (n, a, b, rterm1, rterm2)) = 1 + large_size_rterm a + large_size_rterm b + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (DEC_TERM (dec, var1, rterm1, var2, rterm2)) = 1 + large_size_rterm dec + large_size_rterm rterm1 + large_size_rterm rterm2
  | large_size (IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) = 1 + large_size_rterm i + large_size_rterm downcase + large_size_rterm basecase + large_size_rterm upcase
  | large_size (CLO_TERM clos) = large_size_clos clos

and large_size_rterm rterm = large_size (rterm2term rterm)

and large_size_bterm (B_TERM (vars, rterm)) = large_size_rterm rterm

and large_size_clos (rterm, env) = IntInf.+ (large_size_rterm rterm, large_size_env env)

and large_size_env (ENV env) =
    MAP.foldr
	(fn ((_,_,termref), n) => IntInf.+ (large_size (#1 (!termref)), n))
	0
	env


(* size of the environment of a term term *)
fun env_size (TERM (opr, bterms)) = foldr (fn (bterm, n) => n + env_size_bterm bterm) 0 bterms
  | env_size AXM_TERM = 0
  | env_size BOT_TERM = 0
  | env_size INT_TERM = 0
  | env_size VOI_TERM = 0
  | env_size DUM_TERM = 0
  | env_size (ATM_TERM _) = 0
  | env_size (TOK_TERM _) = 0
  | env_size (NAT_TERM _) = 0
  | env_size (VAR_TERM _) = 0
  | env_size (INL_TERM rterm) = env_size_rterm rterm
  | env_size (INR_TERM rterm) = env_size_rterm rterm
  | env_size (FIX_TERM rterm) = env_size_rterm rterm
  | env_size (MIN_TERM rterm) = env_size_rterm rterm
  | env_size (LAM_TERM (_, rterm)) = env_size_rterm rterm
  | env_size (REC_TERM (_, rterm)) = env_size_rterm rterm
  | env_size (WAI_TERM (rterm1, rterm2)) = env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (APP_TERM (rterm1, rterm2)) = env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (PAI_TERM (rterm1, rterm2)) = env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (ADD_TERM (rterm1, rterm2)) = env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (SUB_TERM (rterm1, rterm2)) = env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (MUL_TERM (rterm1, rterm2)) = env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (DIV_TERM (rterm1, rterm2)) = env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (REM_TERM (rterm1, rterm2)) = env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (EQT_TERM (rterm1, rterm2)) = env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (UNI_TERM (rterm1, rterm2)) = env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (EQU_TERM (a, rterm1, rterm2)) = env_size_rterm a + env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (IAX_TERM (a, rterm1, rterm2)) = env_size_rterm a + env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (IPA_TERM (a, rterm1, rterm2)) = env_size_rterm a + env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (IIR_TERM (a, rterm1, rterm2)) = env_size_rterm a + env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (IIL_TERM (a, rterm1, rterm2)) = env_size_rterm a + env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (IIN_TERM (a, rterm1, rterm2)) = env_size_rterm a + env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (ILA_TERM (a, rterm1, rterm2)) = env_size_rterm a + env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (IAT_TERM (a, rterm1, rterm2)) = env_size_rterm a + env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (CBV_TERM (a, x, f)) = env_size_rterm a + env_size_rterm f
  | env_size (CBA_TERM (a, x, f)) = env_size_rterm a + env_size_rterm f
  | env_size (FUN_TERM (a, x, f)) = env_size_rterm a + env_size_rterm f
  | env_size (PRD_TERM (a, x, f)) = env_size_rterm a + env_size_rterm f
  | env_size (TUN_TERM (a, x, f)) = env_size_rterm a + env_size_rterm f
  | env_size (SET_TERM (a, x, f)) = env_size_rterm a + env_size_rterm f
  | env_size (LES_TERM (a, b, rterm1, rterm2)) = env_size_rterm a + env_size_rterm b + env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (IEQ_TERM (a, b, rterm1, rterm2)) = env_size_rterm a + env_size_rterm b + env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (SPR_TERM (pair, var1, var2, rterm)) = env_size_rterm pair + env_size_rterm rterm
  | env_size (AEQ_TERM (n, a, b, rterm1, rterm2)) = env_size_rterm a + env_size_rterm b + env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (DEC_TERM (dec, var1, rterm1, var2, rterm2)) = env_size_rterm dec + env_size_rterm rterm1 + env_size_rterm rterm2
  | env_size (IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) = env_size_rterm i + env_size_rterm downcase + env_size_rterm basecase + env_size_rterm upcase
  | env_size (CLO_TERM clos) = env_size_clos clos

and env_size_rterm rterm = env_size (rterm2term rterm)

and env_size_bterm (B_TERM (vars, rterm)) = env_size_rterm rterm

and env_size_clos (rterm, env) = env_size_rterm rterm + size_env env


(* depth of a term *)
fun env_depth (TERM (opr, bterms)) =
    foldr (fn (bterm, n) => Int.max (n, env_depth_bterm bterm)) 0 bterms
  | env_depth AXM_TERM = 0
  | env_depth BOT_TERM = 0
  | env_depth INT_TERM = 0
  | env_depth VOI_TERM = 0
  | env_depth DUM_TERM = 0
  | env_depth (ATM_TERM _) = 0
  | env_depth (TOK_TERM _) = 0
  | env_depth (NAT_TERM _) = 0
  | env_depth (VAR_TERM _) = 0
  | env_depth (INL_TERM rterm) = env_depth_rterm rterm
  | env_depth (INR_TERM rterm) = env_depth_rterm rterm
  | env_depth (FIX_TERM rterm) = env_depth_rterm rterm
  | env_depth (MIN_TERM rterm) = env_depth_rterm rterm
  | env_depth (LAM_TERM (_, rterm)) = env_depth_rterm rterm
  | env_depth (REC_TERM (_, rterm)) = env_depth_rterm rterm
  | env_depth (WAI_TERM (rterm1, rterm2)) = env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (APP_TERM (rterm1, rterm2)) = env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (PAI_TERM (rterm1, rterm2)) = env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (ADD_TERM (rterm1, rterm2)) = env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (SUB_TERM (rterm1, rterm2)) = env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (MUL_TERM (rterm1, rterm2)) = env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (DIV_TERM (rterm1, rterm2)) = env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (REM_TERM (rterm1, rterm2)) = env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (EQT_TERM (rterm1, rterm2)) = env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (UNI_TERM (rterm1, rterm2)) = env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (EQU_TERM (a, rterm1, rterm2)) = env_depth_rterm a + env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (IAX_TERM (a, rterm1, rterm2)) = env_depth_rterm a + env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (IPA_TERM (a, rterm1, rterm2)) = env_depth_rterm a + env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (IIR_TERM (a, rterm1, rterm2)) = env_depth_rterm a + env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (IIL_TERM (a, rterm1, rterm2)) = env_depth_rterm a + env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (IIN_TERM (a, rterm1, rterm2)) = env_depth_rterm a + env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (ILA_TERM (a, rterm1, rterm2)) = env_depth_rterm a + env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (IAT_TERM (a, rterm1, rterm2)) = env_depth_rterm a + env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (CBV_TERM (a, x, f)) = env_depth_rterm a + env_depth_rterm f
  | env_depth (CBA_TERM (a, x, f)) = env_depth_rterm a + env_depth_rterm f
  | env_depth (FUN_TERM (a, x, f)) = env_depth_rterm a + env_depth_rterm f
  | env_depth (PRD_TERM (a, x, f)) = env_depth_rterm a + env_depth_rterm f
  | env_depth (TUN_TERM (a, x, f)) = env_depth_rterm a + env_depth_rterm f
  | env_depth (SET_TERM (a, x, f)) = env_depth_rterm a + env_depth_rterm f
  | env_depth (LES_TERM (a, b, rterm1, rterm2)) = env_depth_rterm a + env_depth_rterm b + env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (IEQ_TERM (a, b, rterm1, rterm2)) = env_depth_rterm a + env_depth_rterm b + env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (SPR_TERM (pair, var1, var2, rterm)) = env_depth_rterm pair + env_depth_rterm rterm
  | env_depth (AEQ_TERM (n, a, b, rterm1, rterm2)) = env_depth_rterm a + env_depth_rterm b + env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (DEC_TERM (dec, var1, rterm1, var2, rterm2)) = env_depth_rterm dec + env_depth_rterm rterm1 + env_depth_rterm rterm2
  | env_depth (IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) = env_depth_rterm i + env_depth_rterm downcase + env_depth_rterm basecase + env_depth_rterm upcase
  | env_depth (CLO_TERM clos) = env_depth_clos clos

and env_depth_rterm rterm = env_depth (rterm2term rterm)

and env_depth_bterm (B_TERM (vars, rterm)) = env_depth_rterm rterm

and env_depth_clos (rterm, env) =
    Int.max (env_depth_rterm rterm, env_depth_env env + 1)

and env_depth_env (ENV env) =
    MAP.foldr
	(fn ((_,_,termref), n) => Int.max (env_depth (#1 (!termref)), n))
	0
	env


(* ------ CHECKERS ------ *)

fun is_nuprl_term token term = (opid_of_term term = token)
fun is_nuprl_ref_term token rterm = is_nuprl_term token (rterm2term rterm)

val is_nuprl_type_term                = is_nuprl_term "universe"
val is_nuprl_iwf_lemmas_term          = is_nuprl_term "!wf"
val is_nuprl_iabstraction_term        = is_nuprl_term "!abstraction"
val is_nuprl_loc_term                 = is_nuprl_term "Id"
val is_nuprl_list_term                = is_nuprl_term "list"
val is_nuprl_bool_term                = is_nuprl_term "bool"
val is_nuprl_unit_term                = is_nuprl_term "unit"
val is_nuprl_select_term              = is_nuprl_term "select"
val is_nuprl_eclass_term              = is_nuprl_term "eclass"
val is_nuprl_iabstraction_term        = is_nuprl_term "!abstraction"
val is_nuprl_all_term                 = is_nuprl_term "all"
val is_nuprl_uall_term                = is_nuprl_term "uall"
val is_nuprl_bag_term                 = is_nuprl_term "bag"
val is_nuprl_eqof_term                = is_nuprl_term "eqof"
val is_nuprl_eq_int_term              = is_nuprl_term "eq_int"
val is_nuprl_eq_id_term               = is_nuprl_term "eq_id"
val is_nuprl_iinclude_properties_term = is_nuprl_term "!include_properties"
val is_nuprl_cons_term                = is_nuprl_term "cons"
val is_nuprl_nil_term                 = is_nuprl_term "nil"
val is_nuprl_it_term                  = is_nuprl_term "it"

fun is_nuprl_iwftheorem_term (TERM ((("!theorem", _), [(name, "t")]), [B_TERM ([], theorem)])) =
    String.isSuffix "_wf" name
  | is_nuprl_iwftheorem_term _ = false

fun equal_parameters param1 param2 = (param1 : parameter) = param2


(* ------ LIBRARY HANDLING ------ *)

val to_keep =
    ["NI_Auto",
     "isect_1",
     "core_2",
     "well_fnd",
     "int_1",
     "bool_1",
     "union",
     "subtype_0",
     "sqequal_1",
     "fun_1",
     "rfunction_1",
     "rel_1",
     "quot_1",
     "int_2",
     "list_1",
     "prog_1",
     "subtype_1",
     "num_thy_1",
     "basic",
     "tree_1",
     "nat_extra",
     "list+",
     "sets_1",
     "list_2",
     "list_3",
     "call by value",
     "general",
     "atoms",
     "decidable-equality",
     "event-ordering",
     "process-model",
     "event-logic-applications",
     "event-structures2",
     "event-system"]

val to_filter_out =
    ["test",
     "DivA",
     "ppcc",
     "gen_algebra_1",
     "groups_1",
     "rings_1",
     "algebras_1",
     "perms_1",
     "perms_2",
     "factor_1",
     "mset",
     "basic-tactics",
     "tactics",
     "polynom_1",
     "polynom_2",
     "polynom_3",
     "polynom_4",
     "rationals",
     "reals",
     "better\\ polynomials",
     "polynomials",
     "randomness",
     "realizability",
     "FullyIntensional",
     "concrete-message-automata",
     "dependent\\ intersection",
     "message-automata"]

fun isin_str str lst = List.exists (fn s : string => s = str) lst

fun filter_library lst [] = []
  | filter_library lst (term as (TERM (((opid, tag), []), b_terms)) :: terms) =
    if get_tag tag = "t"
    then if isin_str opid lst
	 then filter_library lst terms
	 else term :: filter_library lst terms
    else raise Fail "filter_library"
  | filter_library _ _ = raise Fail "filter_library"

fun emlib () : lib = {abs = ref MAP.empty, tof = ref MAP.empty}

fun union_libs ({abs = abs1, tof = tof1} : lib)
	       ({abs = abs2, tof = tof2} : lib) =
    {abs = ref (MAP.unionWithi (fn (id, x, y) =>
				   let (*val _ =
					   if id = "Memory1-prc"
					   then print ("[-----------------------------------------" ^ Int.toString (length x) ^ "-" ^ Int.toString (length y) ^ "]\n")
					   else ()*)
				   in x @ y
				   end)
			       (!abs1, !abs2)),
     tof = ref (MAP.unionWithi (fn (id, x, y) => y)
			       (!tof1, !tof2))}

fun mk_item id sign obid lhs rhs wfs =
    {id = id, sign = sign, obid = obid, lhs = lhs, rhs = rhs, wfs = wfs}

fun insert_abs (lib as {abs, tof}) id info =
    let val a = !abs
	(*val _ = if id = "Memory1-prc"
		then print ("[---------------------------------------------------]\n")
		else ()*)
	val _ = case MAP.find (a, id) of
		    SOME lst => abs := MAP.insert (a, id, (TR.mk info) :: lst)
		  | NONE     => abs := MAP.insert (a, id, [TR.mk info])
    in lib
    end

fun insert_tof (lib as {abs, tof}) obid rhs =
    (tof := MAP.insert (!tof, obid, TR.mk rhs); lib)

datatype lib_kind =
	 ABS of string * item
       | TOF of string * nuprl_term

fun term2map' n (TERM (((id, tag), [(obid,"o")]),
		       [B_TERM ([], lhs),
			B_TERM ([], rhs),
			B_TERM ([], wfs)])) =
    if get_tag tag = "t"
    then let val trhs = rterm2term rhs
	 in if is_nuprl_term "!primitive" trhs
	    then NONE
	    else let val tlhs = rterm2term lhs
		     val twfs = rterm2term wfs
		     val opid = opid_of_term tlhs
		     val sign = getSignature tlhs
		     val subs = subterms twfs
		     (*val obid =
			 case params of
			     [] => ""
			   | [(obid,"o")] => obid
			   | _ => raise Fail "term2map:params"*)
		     val nlhs = mk_rterm (TERM2term tlhs)
		     val nrhs = mk_rterm (TERM2term trhs)
		     val item = mk_item id sign obid nlhs nrhs subs
		 in SOME (ABS (opid, item))
		 end
	 end
    else raise Fail "term2map:wrong_format:abs"
  | term2map' n (TERM (((id, tag), [(obid,"o")]),
		       [B_TERM ([], termof),
			B_TERM ([], extract)])) =
    if get_tag tag = "t"
    then let val ttermof = rterm2term termof
	     (*val obid =
		 case params of
		     [] => ""
		   | [(obid,"o")] => obid
		   | _ => raise Fail "term2map:params"*)
	 in if is_nuprl_term "TERMOF" ttermof
	    then case get_obid_parameters (parameters_of_term ttermof) of
		     SOME oid =>
		     if oid = obid
		     then SOME (TOF (oid, rterm2term extract))
		     else raise Fail "wrong object identifier"
		   | NONE => NONE
	    else NONE
	 end
    else raise Fail "term2map:wrong_format:termof"
  | term2map' n (term as TERM (opr,bterms)) =
    raise Fail ("term2map:wrong_format:TERM("
		^ Int.toString n
		^ "):"
		^ opid_of_term term
		^ "("
		^ Int.toString (List.length bterms)
		^ ")")
  | term2map' n term =
    raise Fail ("wrong format:term2map("
		^ Int.toString n
		^ "):not_a_TERM:"
		^ opid_of_term term)

fun term2map n term = term2map' n (term2TERM term)

fun b_terms2map n [] = emlib ()
  | b_terms2map n ((B_TERM ([], term)) :: b_terms) =
    (case term2map n (rterm2term term) of
	 SOME (ABS (opid, item)) => insert_abs (b_terms2map n b_terms) opid item
       | SOME (TOF (oid, rhs)) => insert_tof (b_terms2map n b_terms) oid rhs
       | NONE => b_terms2map n b_terms)
  | b_terms2map n _ = raise Fail "b_terms2map:wrong_format"

fun terms2map n [] = emlib ()
  | terms2map n ((TERM (((opid, tag), []), b_terms)) :: terms) =
    if get_tag tag = "t"
    then let (*val _    = print ("[loading theory: " ^ opid ^ "]\n")*)
	     val map1 = b_terms2map n b_terms
	     val map2 = terms2map n terms
	 in union_libs map1 map2
	 end
    else raise Fail ("wrong format:terms2map:lib" ^ get_tag tag ^ "-")
  | terms2map n _ =  raise Fail "wrong format:terms2map"

fun terms2map' n [] = emlib ()
  | terms2map' n ((TERM (((opid, tag), []), b_terms)) :: terms) =
    if get_tag tag = "t"
    then if isin_str opid to_filter_out
	 then ((*print ("[not loading " ^ opid ^ " theory]\n");*) terms2map' n terms)
	 else let val map1 = b_terms2map n b_terms
		  val map2 = terms2map' n terms
	      in union_libs map1 map2
	      end
    else raise Fail "wrong format:terms2map':lib"
  | terms2map' n _ = raise Fail "wrong format:terms2map'"

fun find_sign abs term =
    let val sign = getSignature term
	val opid = opid_of_term term
	fun aux ritem =
	    let val {id, sign = sign', obid, lhs, rhs, wfs} = TR.get ritem
	    in not (is_nuprl_ref_term "!primitive" rhs)
	       andalso
	       (eq_signs sign sign')
	    end
    in case MAP.find (!abs, opid) of
	   SOME lst =>
	   (case List.find aux lst of
		  SOME ritem => TR.get ritem
		| NONE => raise Fail ("find_sign:wrong-signature-for-"
				      ^ opid
				      ^ "-"
				      ^ sign_to_string sign
				      ^ "-"
				      ^ T.fmt {init  = "[",
					       final = "]",
					       sep   = ",",
					       fmt   = fn i => sign_to_string (get_item_sign (TR.get i))}
					      lst))
	 | NONE => raise Fail ("find_sign:not_in_lib(" ^ opid ^ ")")
    end

fun get_opids term = SET.add (get_opids_bterms (get_bterms term), opid_of_term term)
and get_opids_bterms bterms = foldr (fn (bterm, set) => SET.union (set, get_opids_bterm bterm)) SET.empty bterms
and get_opids_bterm (B_TERM (vars, rterm)) = get_opids (rterm2term rterm)

fun closure_term_wrt_lib set (lib as {abs, tof}) =
    let val m = !abs
    in SET.foldr (fn (opid, set) =>
		     case MAP.find (m, opid) of
			 SOME lst =>
			 foldr (fn ({id, sign, obid, lhs, rhs, wfs}, set) =>
				   let val set1 = get_opids rhs
				       val set2 = closure_term_wrt_lib set1 lib
				   in SET.union (set, set2)
				   end)
			       set
			       lst
		       | NONE => set)
		 set
		 set
    end


(* ------ DESTRUCTORS ------ *)

fun dest_simple_term term =
    let val opid  = opid_of_term term
	val terms =
	    map (fn ([], term) => term
		  | _ => (print (toStringTerm term); raise EH.Impossible "dest_simple_term"))
		(bterms_of_term term)
    in (opid, terms)
    end

fun dest_simple_null_term term =
    let val opid  = opid_of_term term
	val terms =
	    map (fn ([],  term) => term
		  | ([v], term) =>
		    if is_null_nuprl_var v
		    then term
		    else (print (toStringRTerm term); raise EH.Impossible "dest_simple_null_term")
		  | _ => (print (toStringTerm term); raise EH.Impossible "dest_simple_null_term"))
		(bterms_of_term term)
    in (opid, terms)
    end

fun dest_term term =
    ((opid_of_term term, parameters_of_term term), bterms_of_term term)

fun full_dest_term (TERM (((opid, _), params), bterms)) =
    ((opid, params), map (fn (B_TERM (vars, rterm)) => (vars, rterm2term rterm)) bterms)
  | full_dest_term AXM_TERM                    = raise Fail "full_dest_term:AXM_TERM"
  | full_dest_term BOT_TERM                    = raise Fail "full_dest_term:BOT_TERM"
  | full_dest_term INT_TERM                    = raise Fail "full_dest_term:INT_TERM"
  | full_dest_term VOI_TERM                    = raise Fail "full_dest_term:VOI_TERM"
  | full_dest_term DUM_TERM                    = raise Fail "full_dest_term:DUM_TERM"
  | full_dest_term (ATM_TERM x)                = raise Fail "full_dest_term:ATM_TERM"
  | full_dest_term (TOK_TERM x)                = raise Fail "full_dest_term:TOK_TERM"
  | full_dest_term (NAT_TERM n)                = raise Fail "full_dest_term:NAT_TERM"
  | full_dest_term (VAR_TERM _)                = raise Fail "full_dest_term:VAR_TERM"
  | full_dest_term (INL_TERM rterm)            = raise Fail "full_dest_term:INL_TERM"
  | full_dest_term (INR_TERM rterm)            = raise Fail "full_dest_term:INR_TERM"
  | full_dest_term (FIX_TERM rterm)            = raise Fail "full_dest_term:FIX_TERM"
  | full_dest_term (MIN_TERM rterm)            = raise Fail "full_dest_term:MIN_TERM"
  | full_dest_term (LAM_TERM (var, rterm))     = raise Fail "full_dest_term:LAM_TERM"
  | full_dest_term (REC_TERM (var, rterm))     = raise Fail "full_dest_term:REC_TERM"
  | full_dest_term (WAI_TERM (rterm1, rterm2)) = raise Fail "full_dest_term:WAI_TERM"
  | full_dest_term (APP_TERM (rterm1, rterm2)) = raise Fail "full_dest_term:APP_TERM"
  | full_dest_term (PAI_TERM (rterm1, rterm2)) = raise Fail "full_dest_term:PAI_TERM"
  | full_dest_term (ADD_TERM (rterm1, rterm2)) = raise Fail "full_dest_term:ADD_TERM"
  | full_dest_term (SUB_TERM (rterm1, rterm2)) = raise Fail "full_dest_term:SUB_TERM"
  | full_dest_term (MUL_TERM (rterm1, rterm2)) = raise Fail "full_dest_term:MUL_TERM"
  | full_dest_term (DIV_TERM (rterm1, rterm2)) = raise Fail "full_dest_term:DIV_TERM"
  | full_dest_term (REM_TERM (rterm1, rterm2)) = raise Fail "full_dest_term:REM_TERM"
  | full_dest_term (EQT_TERM (rterm1, rterm2)) = raise Fail "full_dest_term:EQT_TERM"
  | full_dest_term (UNI_TERM (rterm1, rterm2)) = raise Fail "full_dest_term:UNI_TERM"
  | full_dest_term (EQU_TERM (a, rterm1, rterm2)) = raise Fail "full_dest_term:EQU_TERM"
  | full_dest_term (IAX_TERM (a, rterm1, rterm2)) = raise Fail "full_dest_term:IAX_TERM"
  | full_dest_term (IPA_TERM (a, rterm1, rterm2)) = raise Fail "full_dest_term:IPA_TERM"
  | full_dest_term (IIR_TERM (a, rterm1, rterm2)) = raise Fail "full_dest_term:IIR_TERM"
  | full_dest_term (IIL_TERM (a, rterm1, rterm2)) = raise Fail "full_dest_term:IIL_TERM"
  | full_dest_term (IIN_TERM (a, rterm1, rterm2)) = raise Fail "full_dest_term:IIN_TERM"
  | full_dest_term (ILA_TERM (a, rterm1, rterm2)) = raise Fail "full_dest_term:ILA_TERM"
  | full_dest_term (IAT_TERM (a, rterm1, rterm2)) = raise Fail "full_dest_term:IAT_TERM"
  | full_dest_term (CBV_TERM (a, x, f)) = raise Fail "full_dest_term:CBV_TERM"
  | full_dest_term (CBA_TERM (a, x, f)) = raise Fail "full_dest_term:CBA_TERM"
  | full_dest_term (FUN_TERM (a, x, f)) = raise Fail "full_dest_term:FUN_TERM"
  | full_dest_term (PRD_TERM (a, x, f)) = raise Fail "full_dest_term:PRD_TERM"
  | full_dest_term (TUN_TERM (a, x, f)) = raise Fail "full_dest_term:TUN_TERM"
  | full_dest_term (SET_TERM (a, x, f)) = raise Fail "full_dest_term:SET_TERM"
  | full_dest_term (LES_TERM (a, b, rterm1, rterm2)) = raise Fail "full_dest_term:LES_TERM"
  | full_dest_term (IEQ_TERM (a, b, rterm1, rterm2)) = raise Fail "full_dest_term:IEQ_TERM"
  | full_dest_term (SPR_TERM (pair, var1, var2, rterm)) = raise Fail "full_dest_term:SPR_TERM"
  | full_dest_term (AEQ_TERM (n, a, b, rterm1, rterm2)) = raise Fail "full_dest_term:AEQ_TERM"
  | full_dest_term (DEC_TERM (dec, var1, rterm1, var2, rterm2)) = raise Fail "full_dest_term:DEC_TERM"
  | full_dest_term (IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) = raise Fail "full_dest_term:IND_TERM"
  | full_dest_term (CLO_TERM clos) = raise Fail "full_dest_term:CLO_TERM"

fun dest_alls_ualls term =
    let val b = is_nuprl_all_term term
    in if b orelse is_nuprl_uall_term term
       then case bterms_of_term term of
		[([], typ), ([x], B)] =>
		let val (bounds, body) = dest_alls_ualls (rterm2term B)
		in ((dest_nuprl_var x, rterm2term typ, b) :: bounds, body)
		end
	      | _ => raise Fail "dest_alls_ualls"
       else ([], term)
    end

fun dest_so_variable (TERM ((("variable", _), [(var, "v")]), bterms)) =
    (var, map (fn B_TERM ([], rt) => rterm2term rt
		| _ => raise Fail "dest_so_variable")
	      bterms)
  | dest_so_variable _ = raise Fail "dest_so_variable"

fun dest_eclass (TERM ((("eclass", tag), params),
		       [B_TERM ([],      info),
			B_TERM ([es, e], X)])) =
    (rterm2term info, es, e, rterm2term X)
  | dest_eclass _ = raise EH.Impossible "dest_eclass"

fun dest_let (TERM ((("let", tag), params),
		    [B_TERM ([],  exp1),
		     B_TERM ([v], exp2)])) =
    (rterm2term exp1, v, rterm2term exp2)
  | dest_let _ = raise EH.Impossible "dest_let"

fun dest_simple_functions term =
    if is_nuprl_function_term term
    then let val (t1, t2) = dest_simple_function term
	     val (lst, T) = dest_simple_functions t2
	 in (t1 :: lst, T)
	 end
    else ([], term)

fun dest_iabstraction (TERM ((("!abstraction", tag), []),
			     [B_TERM ([], t1),
			      B_TERM ([], t2),
			      B_TERM ([], t3)])) =
    (t1, t2, t3)
  | dest_iabstraction _ = raise EH.Impossible "dest_iabstraction"

fun dest_iinclude_properties (TERM ((("!include_properties", tag), []),
				    [B_TERM ([], t1),
				     B_TERM ([], t2)])) =
    (t1, t2)
  | dest_iinclude_properties _ = raise EH.Impossible "dest_iinclude_properties"

fun dest_iwftheorem (TERM ((("!theorem", _), [(name, "t")]),
			   [B_TERM ([], theorem)])) = rterm2term theorem
  | dest_iwftheorem _ = raise EH.Impossible "dest_iwftheorem"

fun gen_dest_single (TERM (((opid, tag), parms), [B_TERM ([], term)])) str msg =
    if opid = str
    then rterm2term term
    else raise EH.Impossible msg
  | gen_dest_single term opid msg = raise Fail ("gen_dest_single:" ^ msg)

fun dest_bnot         term = gen_dest_single term "bnot"         "dest_bnot"
fun dest_bag          term = gen_dest_single term "bag"          "dest_bag"
fun dest_type_list    term = gen_dest_single term "list"         "dest_type_list"
fun dest_eqof         term = gen_dest_single term "eqof"         "dest_eqof"
fun dest_primed_class term = gen_dest_single term "primed-class" "dest_primed_class"
fun dest_es_eq        term = gen_dest_single term "es-eq"        "dest_es_eq"

fun dest_integer m term =
    (if is_nuprl_natural_number_term term
     then dest_natural_number term
     else if is_nuprl_minus_term term
     then II.~ (dest_natural_number (dest_minus term))
     else raise Fail "")
    handle Fail str => raise Fail ("dest_integer:not-int-"
				   ^ Int.toString m
				   ^ "("
				   ^ toStringTerm term
				   ^ ")"
				   ^ str)

fun dest_small_integer term = II.toInt (dest_integer 1 term)

fun dest_iport (TERM ((("!port", tag), [(str, "n")]), [])) =
    (case II.fromString str of
	 NONE => raise Fail ("dest_integer:not-int-in-string(" ^ str ^ ")")
       | SOME x => II.toInt x)
  | dest_iport _ = raise Fail "dest_iport"

fun dest_ihost (TERM ((("!host", tag), [(x, "s")]), [])) = x
  | dest_ihost _ = raise Fail "dest_ihost"

fun dest_rec_comb (TERM ((("rec-comb", tag), []),
			 [B_TERM ([], Xs),
			  B_TERM ([], F),
			  B_TERM ([], init)])) =
    (rterm2term Xs, rterm2term F, rterm2term init)
  | dest_rec_comb _ =  raise EH.Impossible "dest_rec_comb"

fun gen_dest_pair (TERM (((opid, tag), params),
			 [B_TERM ([], term1),
			  B_TERM ([], term2)])) str msg =
    if opid = str
    then (rterm2term term1, rterm2term term2)
    else raise EH.Impossible msg
  | gen_dest_pair term opid msg =  raise EH.Impossible msg

fun dest_select    term = gen_dest_pair term "select"    "dest_select"
fun dest_prior_prc term = gen_dest_pair term "prior-prc" "dest_prior_prc"
fun dest_lt_int    term = gen_dest_pair term "lt_int"    "dest_lt_int"
fun dest_le_int    term = gen_dest_pair term "le_int"    "dest_le_int"
fun dest_eq_int    term = gen_dest_pair term "eq_int"    "dest_eq_int"
fun dest_eq_id     term = gen_dest_pair term "eq_id"     "dest_eq_id"
fun dest_band      term = gen_dest_pair term "band"      "dest_band"
fun dest_member    term = gen_dest_pair term "member"    "dest_member"
fun dest_cons      term = gen_dest_pair term "cons"      "dest_cons"

fun dest_list term =
    if is_nuprl_cons_term term
    then let val (h, t) = dest_cons term
	 in h :: (dest_list t)
	 end
    else if is_nuprl_pair_term term
    then let val (h, t) = dest_pair 21 term
	 in h :: (dest_list t)
	 end
    else if is_nuprl_nil_term term
	    orelse
	    is_nuprl_it_term term
	    orelse
	    is_nuprl_axiom_term term
    then []
    else raise Fail ("dest_list:" ^ opid_of_term term)

fun dest_applies (TERM ((("apply", tag), []), [B_TERM ([], f), B_TERM ([], arg)])) =
    let val (x, xs) = dest_applies (rterm2term f)
    in (x, xs @ [rterm2term arg])
    end
  | dest_applies (APP_TERM (rterm1, rterm2)) =
    let val (x, xs) = dest_applies (rterm2term rterm1)
    in (x, xs @ [rterm2term rterm2])
    end
  | dest_applies term = (term, [])

fun is_nuprl_minus_natural_number_term term =
    is_nuprl_minus_term term
    andalso
    is_nuprl_natural_number_term (dest_minus term)

fun is_nuprl_integer_term term =
    is_nuprl_natural_number_term term
    orelse
    is_nuprl_minus_natural_number_term term

fun is_nuprl_event_orderingp_term lvl (TERM ((("event-ordering+", tag), [(l,_)]), [B_TERM ([], info)])) = (l = lvl)
  | is_nuprl_event_orderingp_term _ _ = false

fun is_nuprl_prop_term lvl (TERM ((("prop", tag), [(l,_)]), [])) = (l = lvl)
  | is_nuprl_prop_term _ _ = false


(* ------ CONSTRUCTORS ------ *)

fun mk_nuprl_small_natural_number_term int = mk_natural_number_term (II.fromInt int)

fun mk_nuprl_df_program_meaning_term term =
    mk_nuprl_simple_term "df-program-meaning" [term]

fun mk_nuprl_ihost_term host =
    TERM ((("!host", dtag), [mk_nuprl_string_parameter host]), [])

fun mk_nuprl_iport_term port =
    TERM ((("!port", dtag), [mk_nuprl_natural_parameter (II.fromInt port)]), [])

fun mk_nuprl_integer_term i =
    if II.< (i, 0)
    then (*mk_minus_term (mk_natural_number_term (II.~ i))*)
	mk_minus_term (mk_natural_number_term (II.~ i))
    else mk_natural_number_term i

fun mk_nuprl_int_from_string str =
    case II.fromString str of
	NONE => raise Fail "mk_nuprl_int_from_string"
      | SOME n => mk_nuprl_integer_term n

(* This is a crude hack!! *)
fun mk_nuprl_real_from_string str =
    case Real.fromString str of
	NONE   => raise Fail "mk_nuprl_real_from_string"
      | SOME r => mk_nuprl_integer_term (Real.toLargeInt (IEEEReal.getRoundingMode ()) r)

fun mk_nuprl_small_integer_term int = mk_nuprl_integer_term (II.fromInt int)

fun mk_nuprl_applies_term func args =
    foldl (fn (arg, f) => mk_apply_term f arg)
	  func
	  args

fun mk_nuprl_let_term var pat body =
    mk_nuprl_term ("let", []) [([], pat), ([mk_new_nuprl_var var], body)]

fun mk_nuprl_lambdas_term vars term =
    foldr (fn (var, term) => mk_lambda_term var term)
	  term
	  vars

fun mk_nuprl_spreadn_term pair ([],       bterm) = raise EH.Impossible ""
  | mk_nuprl_spreadn_term pair ([_],      bterm) = raise EH.Impossible ""
  | mk_nuprl_spreadn_term pair ([v1, v2], bterm) = mk_spread_term pair (v1, v2, bterm)
  | mk_nuprl_spreadn_term pair (vars,     bterm) =
    mk_nuprl_term ("spreadn", []) [([], pair), (map mk_new_nuprl_var vars, bterm)]

fun mk_nuprl_isl_term  term = mk_nuprl_simple_term "isl"  [term]
fun mk_nuprl_isr_term  term = mk_nuprl_simple_term "isr"  [term]
fun mk_nuprl_outl_term term = mk_nuprl_simple_term "outl" [term]
fun mk_nuprl_outr_term term = mk_nuprl_simple_term "outr" [term]

val mk_nuprl_ycomb_term = mk_nuprl_simple_term "ycomb" []

fun mk_nuprl_genrec_term (n, r, B) =
    mk_nuprl_term ("genrec", []) [([mk_new_nuprl_var n, mk_new_nuprl_var r], B)]

fun mk_nuprl_bind_class_term f (x, g) =
    mk_nuprl_term ("bind-class", []) [([], f), ([mk_new_nuprl_var x], g)]

fun mk_nuprl_combined_class_term         f lst = mk_nuprl_simple_term "simple-comb"            [f, lst]
fun mk_nuprl_rec_combined_class_term     f lst = mk_nuprl_simple_term "rec-combined-class"     [lst, f]
fun mk_nuprl_combined_loc_class_term     f lst = mk_nuprl_simple_term "simple-loc-comb"        [f, lst]
fun mk_nuprl_rec_combined_loc_class_term f lst = mk_nuprl_simple_term "rec-combined-loc-class" [lst, f]

fun mk_nuprl_rec_comb_term f classes init = mk_nuprl_simple_term "rec-comb" [classes, f, init]

fun mk_nuprl_so_apply1_term f x     = mk_nuprl_simple_term "so_apply" [f, x]
fun mk_nuprl_so_apply2_term f x y   = mk_nuprl_simple_term "so_apply" [f, x, y]
fun mk_nuprl_so_apply3_term f x y z = mk_nuprl_simple_term "so_apply" [f, x, y, z]

fun mk_nuprl_combined0_class_term bag =
    mk_nuprl_simple_term "simple-comb-0" [bag]
fun mk_nuprl_combined1_class_term f class =
    mk_nuprl_simple_term "simple-comb-1" [f, class]
fun mk_nuprl_combined2_class_term f class1 class2 =
    mk_nuprl_simple_term "simple-comb-2" [f, class1, class2]
fun mk_nuprl_combined3_class_term f class1 class2 class3 =
    mk_nuprl_simple_term "simple-comb-3" [f, class1, class2, class3]

fun mk_nuprl_combined0_loc_class_term bag =
    mk_nuprl_simple_term "simple-loc-comb-0" [bag]
fun mk_nuprl_combined1_loc_class_term f class =
    mk_nuprl_simple_term "simple-loc-comb-1" [f, class]
fun mk_nuprl_combined2_loc_class_term f class1 class2 =
    mk_nuprl_simple_term "simple-loc-comb-2" [f, class1, class2]
fun mk_nuprl_combined3_loc_class_term f class1 class2 class3 =
    mk_nuprl_simple_term "simple-loc-comb-3" [f, class1, class2, class3]

fun mk_nuprl_rec_combined0_class_term bag =
    mk_nuprl_simple_term "rec-combined-class-0" [bag]
fun mk_nuprl_rec_combined1_class_term f class =
    mk_nuprl_simple_term "rec-combined-class-1" [f, class]
fun mk_nuprl_rec_combined2_class_term f class1 class2 =
    mk_nuprl_simple_term "rec-combined-class-2" [f, class1, class2]
fun mk_nuprl_rec_combined3_class_term f class1 class2 class3 =
    mk_nuprl_simple_term "rec-combined-class-3" [f, class1, class2, class3]

fun mk_nuprl_rec_combined0_class_opt_term opt bag =
    mk_nuprl_simple_term "rec-combined-class-opt-0" [bag, opt]
fun mk_nuprl_rec_combined1_class_opt_term opt f class =
    mk_nuprl_simple_term "rec-combined-class-opt-1" [f, opt, class]
fun mk_nuprl_rec_combined2_class_opt_term opt f class1 class2 =
    mk_nuprl_simple_term "rec-combined-class-opt-2" [f, opt, class1, class2]
fun mk_nuprl_rec_combined3_class_opt_term opt f class1 class2 class3 =
    mk_nuprl_simple_term "rec-combined-class-opt-3" [f, opt, class1, class2, class3]

fun mk_nuprl_rec_combined0_loc_class_term bag =
    mk_nuprl_simple_term "rec-combined-loc-class-0" [bag]
fun mk_nuprl_rec_combined1_loc_class_term f class =
    mk_nuprl_simple_term "rec-combined-loc-class-1" [f, class]
fun mk_nuprl_rec_combined2_loc_class_term f class1 class2 =
    mk_nuprl_simple_term "rec-combined-loc-class-2" [f, class1, class2]
fun mk_nuprl_rec_combined3_loc_class_term f class1 class2 class3 =
    mk_nuprl_simple_term "rec-combined-loc-class-3" [f, class1, class2, class3]

fun mk_nuprl_rec_combined0_loc_class_opt_term opt bag =
    mk_nuprl_simple_term "rec-combined-loc-class-opt-0" [bag, opt]
fun mk_nuprl_rec_combined1_loc_class_opt_term opt f class =
    mk_nuprl_simple_term "rec-combined-loc-class-opt-1" [f, opt, class]
fun mk_nuprl_rec_combined2_loc_class_opt_term opt f class1 class2 =
    mk_nuprl_simple_term "rec-combined-loc-class-opt-2" [f, opt, class1, class2]
fun mk_nuprl_rec_combined3_loc_class_opt_term opt f class1 class2 class3 =
    mk_nuprl_simple_term "rec-combined-loc-class-opt-3" [f, opt, class1, class2, class3]

fun mk_nuprl_lifting0_term f = mk_nuprl_simple_term "lifting-0" [f]
fun mk_nuprl_lifting1_term f = mk_nuprl_simple_term "lifting-1" [f]
fun mk_nuprl_lifting2_term f = mk_nuprl_simple_term "lifting-2" [f]
fun mk_nuprl_lifting3_term f = mk_nuprl_simple_term "lifting-3" [f]
fun mk_nuprl_lifting4_term f = mk_nuprl_simple_term "lifting-4" [f]

fun mk_nuprl_lifting_gen_term n f = mk_nuprl_simple_term "lifting-gen" [n, f]

fun mk_nuprl_lifting_loc0_term f = mk_nuprl_simple_term "lifting-loc-0" [f]
fun mk_nuprl_lifting_loc1_term f = mk_nuprl_simple_term "lifting-loc-1" [f]
fun mk_nuprl_lifting_loc2_term f = mk_nuprl_simple_term "lifting-loc-2" [f]
fun mk_nuprl_lifting_loc3_term f = mk_nuprl_simple_term "lifting-loc-3" [f]
fun mk_nuprl_lifting_loc4_term f = mk_nuprl_simple_term "lifting-loc-4" [f]

fun mk_nuprl_lifting_loc_gen_term n f = mk_nuprl_simple_term "lifting-loc-gen" [n, f]

fun mk_nuprl_concat_lifting0_term f = mk_nuprl_simple_term "concat-lifting-0" [f]
fun mk_nuprl_concat_lifting1_term f = mk_nuprl_simple_term "concat-lifting-1" [f]
fun mk_nuprl_concat_lifting2_term f = mk_nuprl_simple_term "concat-lifting-2" [f]
fun mk_nuprl_concat_lifting3_term f = mk_nuprl_simple_term "concat-lifting-3" [f]
fun mk_nuprl_concat_lifting4_term f = mk_nuprl_simple_term "concat-lifting-4" [f]

fun mk_nuprl_concat_lifting_gen_term n f = mk_nuprl_simple_term "concat-lifting-gen" [n, f]

fun mk_nuprl_concat_lifting_loc0_term f = mk_nuprl_simple_term "concat-lifting-loc-0" [f]
fun mk_nuprl_concat_lifting_loc1_term f = mk_nuprl_simple_term "concat-lifting-loc-1" [f]
fun mk_nuprl_concat_lifting_loc2_term f = mk_nuprl_simple_term "concat-lifting-loc-2" [f]
fun mk_nuprl_concat_lifting_loc3_term f = mk_nuprl_simple_term "concat-lifting-loc-3" [f]
fun mk_nuprl_concat_lifting_loc4_term f = mk_nuprl_simple_term "concat-lifting-loc-4" [f]

fun mk_nuprl_concat_lifting_loc_gen_term n f = mk_nuprl_simple_term "concat-lifting-loc-gen" [n, f]

fun mk_nuprl_type_term i = mk_nuprl_term ("universe", [mk_nuprl_level_exp_parameter i]) []

fun mk_nuprl_valuealltype_term i = mk_nuprl_term ("vatype", [mk_nuprl_level_exp_parameter i]) []

fun mk_nuprl_single_valued_classrel_term es class typ =
    mk_nuprl_term ("single-valued-classrel", [])
		  [([], es),
		   ([], class),
		   ([], typ)]

fun mk_nuprl_std_ma_term eo class headers =
    mk_nuprl_term ("std-ma", [mk_nuprl_level_exp_parameter "i"])
		  [([], eo),
		   ([], class),
		   ([], headers)]

fun mk_nuprl_message_constraint_term eo class headers =
    mk_nuprl_term ("message-constraint", [mk_nuprl_level_exp_parameter "i"])
		  [([], eo),
		   ([], class),
		   ([], headers)]

fun mk_nuprl_msg_interface_constraint_term eo class headers =
    mk_nuprl_term ("msg-interface-constraint", [mk_nuprl_level_exp_parameter "i"])
		  [([], eo),
		   ([], class),
		   ([], headers)]

fun mk_nuprl_messages_delivered_term eo class =
    mk_nuprl_term ("messages-delivered", [mk_nuprl_level_exp_parameter "i"])
		  [([], eo),
		   ([], class)]

fun mk_nuprl_msgs_interface_delivered_term eo class =
    mk_nuprl_term ("msgs-interface-delivered", [mk_nuprl_level_exp_parameter "i"])
		  [([], eo),
		   ([], class)]

fun mk_nuprl_prop_term i =
    mk_nuprl_term ("prop", [mk_nuprl_level_exp_parameter i]) []

fun mk_nuprl_set_sig_term i item =
    mk_nuprl_term ("set-sig", [mk_nuprl_level_exp_parameter i])
		  [([], item)]

fun mk_nuprl_map_sig_term i key value =
    mk_nuprl_term ("map-sig", [mk_nuprl_level_exp_parameter i])
		  [([], key), ([], value)]

(* 0 argument *)
val mk_nuprl_real_term               = mk_nuprl_simple_term "real"               []
val mk_nuprl_bool_term               = mk_nuprl_simple_term "bool"               []
val mk_nuprl_unit_term               = mk_nuprl_simple_term "unit"               []
val mk_nuprl_top_term                = mk_nuprl_simple_term "top"                []
val mk_nuprl_nat_term                = mk_nuprl_simple_term "nat"                []
val mk_nuprl_loc_term                = mk_nuprl_simple_term "Id"                 []
val mk_nuprl_name_term               = mk_nuprl_simple_term "name"               [] (* Atom List *)
val mk_nuprl_inewline_term           = mk_nuprl_simple_term "!newline"           []
val mk_nuprl_empty_bag_term          = mk_nuprl_simple_term "empty-bag"          []
val mk_nuprl_icons_nil_term          = mk_nuprl_simple_term "!cons"              []
val mk_nuprl_itext_nil_term          = mk_nuprl_simple_term "!text_cons"         []
val mk_nuprl_bool_deq_term           = mk_nuprl_simple_term "bool-deq"           []
val mk_nuprl_int_deq_term            = mk_nuprl_simple_term "int-deq"            []
val mk_nuprl_atom_deq_term           = mk_nuprl_simple_term "atom-deq"           []
val mk_nuprl_nat_deq_term            = mk_nuprl_simple_term "nat-deq"            []
val mk_nuprl_loc_deq_term            = mk_nuprl_simple_term "id-deq"             []
val mk_nuprl_unit_deq_term           = mk_nuprl_simple_term "unit-deq"           []
val mk_nuprl_btrue_term              = mk_nuprl_simple_term "btrue"              []
val mk_nuprl_bfalse_term             = mk_nuprl_simple_term "bfalse"             []
val mk_nuprl_condition_cons_term     = mk_nuprl_simple_term "!condition_cons"    []
val mk_nuprl_it_term                 = mk_nuprl_simple_term "it"                 []
val mk_nuprl_ioid_term               = mk_nuprl_simple_term "!oid"               []
val mk_nuprl_nil_term                = mk_nuprl_simple_term "nil"                []
val mk_nuprl_null_class_program_term = mk_nuprl_simple_term "null-class-program" []
val mk_nuprl_null_class_term         = mk_nuprl_simple_term "null-class"         []

(* 1 argument *)
fun mk_nuprl_once_class_term                   class = mk_nuprl_simple_term "once-class"                    [class]
fun mk_nuprl_send_once_class_term              class = mk_nuprl_simple_term "send-once-class"               [class]
fun mk_nuprl_send_once_loc_class_term          class = mk_nuprl_simple_term "send-once-loc-class"           [class]
fun mk_nuprl_on_loc_class_term                 class = mk_nuprl_simple_term "on-loc-class"                  [class]
fun mk_nuprl_but_first_class_term              class = mk_nuprl_simple_term "but-first-class"               [class]
fun mk_nuprl_skip_first_class_term             class = mk_nuprl_simple_term "skip-first-class"              [class]
fun mk_nuprl_primed_class_term                 class = mk_nuprl_simple_term "primed-class"                  [class]
fun mk_nuprl_single_bag_term                   elt   = mk_nuprl_simple_term "single-bag"                    [elt]
fun mk_nuprl_bnot_term                         term  = mk_nuprl_simple_term "bnot"                          [term]
fun mk_nuprl_not_term                          term  = mk_nuprl_simple_term "not"                           [term]
fun mk_nuprl_pi1_term                          term  = mk_nuprl_simple_term "pi1"                           [term]
fun mk_nuprl_pi2_term                          term  = mk_nuprl_simple_term "pi2"                           [term]
fun mk_nuprl_hd_term                           term  = mk_nuprl_simple_term "hd"                            [term]
fun mk_nuprl_tl_term                           term  = mk_nuprl_simple_term "tl"                            [term]
fun mk_nuprl_es_eq_term                        es    = mk_nuprl_simple_term "es-eq"                         [es]
fun mk_nuprl_list_deq_term                     term  = mk_nuprl_simple_term "list-deq"                      [term]
fun mk_nuprl_eqof_term                         term  = mk_nuprl_simple_term "eqof"                          [term]
fun mk_nuprl_valueall_type_term                typ   = mk_nuprl_simple_term "valueall-type"                 [typ]
fun mk_nuprl_list_term                         term  = mk_nuprl_simple_term "list"                          [term]
fun mk_nuprl_bag_term                          term  = mk_nuprl_simple_term "bag"                           [term]
fun mk_nuprl_deq_term                          term  = mk_nuprl_simple_term "deq"                           [term]
fun mk_nuprl_esE_term                          es    = mk_nuprl_simple_term "es-E"                          [es]
fun mk_nuprl_assert_term                       b     = mk_nuprl_simple_term "assert"                        [b]
fun mk_nuprl_msg_header_term                   term  = mk_nuprl_simple_term "msg-header"                    [term]
fun mk_nuprl_msg_type_term                     term  = mk_nuprl_simple_term "msg-type"                      [term]
fun mk_nuprl_msg_body_term                     term  = mk_nuprl_simple_term "msg-body"                      [term]
fun mk_nuprl_bag_null_term                     bag   = mk_nuprl_simple_term "bag-null"                      [bag]
fun mk_nuprl_bag_only_term                     bag   = mk_nuprl_simple_term "bag-only"                      [bag]
fun mk_nuprl_bag_size_term                     bag   = mk_nuprl_simple_term "bag-size"                      [bag]
fun mk_nuprl_evalall_term                      term  = mk_nuprl_simple_term "evalall"                       [term]
fun mk_nuprl_once_class_program_term           term  = mk_nuprl_simple_term "once-class-program"            [term]
fun mk_nuprl_return_loc_bag_class_program_term term  = mk_nuprl_simple_term "return-loc-bag-class-program"  [term]
fun mk_nuprl_return_loc_bag_class_term         term  = mk_nuprl_simple_term "return-loc-bag-class"          [term]
fun mk_nuprl_on_loc_class_program_term         term  = mk_nuprl_simple_term "on-loc-class-program"          [term]
fun mk_nuprl_msg_term                          term  = mk_nuprl_simple_term "Message"                       [term]
fun mk_nuprl_interface_term                    term  = mk_nuprl_simple_term "msg-interface"                 [term]
fun mk_nuprl_base_headers_msg_val_term         term  = mk_nuprl_simple_term "base-headers-msg-val"          [term]
fun mk_nuprl_base_locs_headers_term            term  = mk_nuprl_simple_term "base-locs-headers"             [term]
fun mk_nuprl_base_class_program_term           term  = mk_nuprl_simple_term "base-class-program"            [term]
fun mk_nuprl_set_sig_set_term                  term  = mk_nuprl_simple_term "set-sig-set"                   [term]
fun mk_nuprl_set_sig_member_term               term  = mk_nuprl_simple_term "set-sig-member"                [term]
fun mk_nuprl_set_sig_empty_term                term  = mk_nuprl_simple_term "set-sig-empty"                 [term]
fun mk_nuprl_set_sig_isEmpty_term              term  = mk_nuprl_simple_term "set-sig-isEmpty"               [term]
fun mk_nuprl_set_sig_singleton_term            term  = mk_nuprl_simple_term "set-sig-singleton"             [term]
fun mk_nuprl_set_sig_add_term                  term  = mk_nuprl_simple_term "set-sig-add"                   [term]
fun mk_nuprl_set_sig_union_term                term  = mk_nuprl_simple_term "set-sig-union"                 [term]
fun mk_nuprl_set_sig_remove_term               term  = mk_nuprl_simple_term "set-sig-remove"                [term]
fun mk_nuprl_map_sig_map_term                  term  = mk_nuprl_simple_term "map-sig-map"                   [term]
fun mk_nuprl_map_sig_eqKey_term                term  = mk_nuprl_simple_term "map-sig-eqKey"                 [term]
fun mk_nuprl_map_sig_find_term                 term  = mk_nuprl_simple_term "map-sig-find"                  [term]
fun mk_nuprl_map_sig_inDom_term                term  = mk_nuprl_simple_term "map-sig-inDom"                 [term]
fun mk_nuprl_map_sig_empty_term                term  = mk_nuprl_simple_term "map-sig-empty"                 [term]
fun mk_nuprl_map_sig_isEmpty_term              term  = mk_nuprl_simple_term "map-sig-isEmpty"               [term]
fun mk_nuprl_map_sig_update_term               term  = mk_nuprl_simple_term "map-sig-update"                [term]
fun mk_nuprl_map_sig_add_term                  term  = mk_nuprl_simple_term "map-sig-add"                   [term]
fun mk_nuprl_map_sig_remove_term               term  = mk_nuprl_simple_term "map-sig-remove"                [term]

(* 2 arguments *)
fun mk_nuprl_class_at_term               class  locs   = mk_nuprl_simple_term "class-at"               [class,  locs]
fun mk_nuprl_general_base_class_term     term1  term2  = mk_nuprl_simple_term "general-base-class"     [term1,  term2]
fun mk_nuprl_concat_term                 list1  list2  = mk_nuprl_simple_term "append"                 [list1,  list2]
fun mk_nuprl_select_term                 ind    list   = mk_nuprl_simple_term "select"                 [ind,    list]
fun mk_nuprl_parallel_class_term         class1 class2 = mk_nuprl_simple_term "parallel-class"         [class1, class2]
fun mk_nuprl_until_class_term            class1 class2 = mk_nuprl_simple_term "until-class"            [class1, class2]
fun mk_nuprl_primed_class_opt_term       class  bag    = mk_nuprl_simple_term "primed-class-opt"       [class,  bag]
fun mk_nuprl_class_opt_term              class  bag    = mk_nuprl_simple_term "class-opt"              [class,  bag]
fun mk_nuprl_class_opt_class_term        class1 class2 = mk_nuprl_simple_term "class-opt-class"        [class1, class2]
fun mk_nuprl_base_prc_term               name   typ    = mk_nuprl_simple_term "base-prc"               [name,   typ]
fun mk_nuprl_once_prc_term               typ    class  = mk_nuprl_simple_term "once-prc"               [typ,    class]
fun mk_nuprl_send_once_loc_prc_term      typ    bag    = mk_nuprl_simple_term "send-once-loc-prc"      [typ,    bag]
fun mk_nuprl_on_loc_prc_term             typ    fX     = mk_nuprl_simple_term "on-loc-prc"             [typ,    fX]
fun mk_nuprl_but_first_prc_term          typ    class  = mk_nuprl_simple_term "but-first-prc"          [typ,    class]
fun mk_nuprl_skip_first_prc_term         typ    class  = mk_nuprl_simple_term "skip-first-prc"         [typ,    class]
fun mk_nuprl_prior_prc_term              typ    class  = mk_nuprl_simple_term "prior-prc"              [typ,    class]
fun mk_nuprl_or_term                     term1  term2  = mk_nuprl_simple_term "or"                     [term1,  term2]
fun mk_nuprl_and_term                    term1  term2  = mk_nuprl_simple_term "and"                    [term1,  term2]
fun mk_nuprl_rec_bind_class_term         X      Y      = mk_nuprl_simple_term "rec-bind-class"         [X,      Y]
fun mk_nuprl_member_term                 term1  term2  = mk_nuprl_simple_term "member"                 [term1,  term2]
fun mk_nuprl_eq_atom_term                nt1    nt2    = mk_nuprl_simple_term "eq_atom"                [nt1,    nt2]
fun mk_nuprl_eq_bool_term                nt1    nt2    = mk_nuprl_simple_term "eq_bool"                [nt1,    nt2]
fun mk_nuprl_eq_int_term                 nt1    nt2    = mk_nuprl_simple_term "eq_int"                 [nt1,    nt2]
fun mk_nuprl_eq_id_term                  nt1    nt2    = mk_nuprl_simple_term "eq_id"                  [nt1,    nt2]
fun mk_nuprl_eq_loc_term                 nt1    nt2    = mk_nuprl_simple_term "eq_id"                  [nt1,    nt2]
fun mk_nuprl_bor_term                    term1  term2  = mk_nuprl_simple_term "bor"                    [term1,  term2]
fun mk_nuprl_band_term                   term1  term2  = mk_nuprl_simple_term "band"                   [term1,  term2]
fun mk_nuprl_iff_term                    term1  term2  = mk_nuprl_simple_term "iff"                    [term1,  term2]
fun mk_nuprl_uiff_term                   term1  term2  = mk_nuprl_simple_term "uiff"                   [term1,  term2]
fun mk_nuprl_implies_term                term1  term2  = mk_nuprl_simple_term "implies"                [term1,  term2]
fun mk_nuprl_uimplies_term               term1  term2  = mk_nuprl_simple_term "uimplies"               [term1,  term2]
fun mk_nuprl_proddeq_term                term1  term2  = mk_nuprl_simple_term "proddeq"                [term1,  term2]
fun mk_nuprl_sumdeq_term                 term1  term2  = mk_nuprl_simple_term "sumdeq"                 [term1,  term2]
fun mk_nuprl_lt_int_term                 term1  term2  = mk_nuprl_simple_term "lt_int"                 [term1,  term2]
fun mk_nuprl_le_int_term                 term1  term2  = mk_nuprl_simple_term "le_int"                 [term1,  term2]
fun mk_nuprl_less_than_term              term1  term2  = mk_nuprl_simple_term "less_than"              [term1,  term2]
fun mk_nuprl_le_term                     term1  term2  = mk_nuprl_simple_term "le"                     [term1,  term2]
fun mk_nuprl_es_pred_term                es     e      = mk_nuprl_simple_term "es-pred"                [es,     e]
fun mk_nuprl_union_term                  term1  term2  = mk_nuprl_simple_term "union"                  [term1,  term2]
fun mk_nuprl_msg_has_type_term           term1  term2  = mk_nuprl_simple_term "msg-has-type"           [term1,  term2]
fun mk_nuprl_name_eq_term                term1  term2  = mk_nuprl_simple_term "name_eq"                [term1,  term2]
fun mk_nuprl_icons_cons_term             term1  term2  = mk_nuprl_simple_term "!cons"                  [term1,  term2]
fun mk_nuprl_itext_cons_term             term1  term2  = mk_nuprl_simple_term "!text_cons"             [term1,  term2]
fun mk_nuprl_iinclude_properties_term    prop   term   = mk_nuprl_simple_term "!include_properties"    [prop,   term]
fun mk_nuprl_cons_bag_term               head   tail   = mk_nuprl_simple_term "cons-bag"               [head,   tail]
fun mk_nuprl_bag_map_term                f      bag    = mk_nuprl_simple_term "bag-map"                [f,      bag]
fun mk_nuprl_eq_term_term                term1  term2  = mk_nuprl_simple_term "eq_term"                [term1,  term2]
fun mk_nuprl_class_at_program_term       proc   locs   = mk_nuprl_simple_term "class-at-program"       [proc,   locs]
fun mk_nuprl_parallel_class_program_term term1  term2  = mk_nuprl_simple_term "parallel-class-program" [term1,  term2]
fun mk_nuprl_bind_class_program_term     term1  term2  = mk_nuprl_simple_term "bind-class-program"     [term1,  term2]
fun mk_nuprl_until_class_program_term    term1  term2  = mk_nuprl_simple_term "until-class-program"    [term1,  term2]
fun mk_nuprl_eclass0_term                term1  term2  = mk_nuprl_simple_term "eclass0"                [term1,  term2]
fun mk_nuprl_eclass0_program_term        term1  term2  = mk_nuprl_simple_term "eclass0-program"        [term1,  term2]
fun mk_nuprl_eclass1_term                term1  term2  = mk_nuprl_simple_term "eclass1"                [term1,  term2]
fun mk_nuprl_eclass1_program_term        term1  term2  = mk_nuprl_simple_term "eclass1-program"        [term1,  term2]
fun mk_nuprl_eclass2_term                term1  term2  = mk_nuprl_simple_term "eclass2"                [term1,  term2]
fun mk_nuprl_eclass2_program_term        term1  term2  = mk_nuprl_simple_term "eclass2-program"        [term1,  term2]
fun mk_nuprl_eclass3_term                term1  term2  = mk_nuprl_simple_term "eclass3"                [term1,  term2]
fun mk_nuprl_eclass3_program_term        term1  term2  = mk_nuprl_simple_term "eclass3-program"        [term1,  term2]
fun mk_nuprl_cons_term                   term1  term2  = mk_nuprl_simple_term "cons"                   [term1,  term2]
fun mk_nuprl_mk_msg_interface_term       loc    msg    = mk_nuprl_simple_term "mk-msg-interface"       [loc,    msg]
fun mk_nuprl_make_msg_term               hdr    value  = mk_nuprl_simple_term "make-Msg"               [hdr,    value]
fun mk_nuprl_no_repeats_term             typ    list   = mk_nuprl_simple_term "no_repeats"             [typ,    list]
fun mk_nuprl_record_select_term          record sel    = mk_nuprl_simple_term "record-select"          [record, sel]

(* 3 arguments *)
fun mk_nuprl_base_headers_msg_val_loc_term term1 term2 term3 = mk_nuprl_simple_term "base-headers-msg-val-loc" [term1, term2, term3]
fun mk_nuprl_base_at_prc_term              name  typ   locs  = mk_nuprl_simple_term "base-at-prc"              [name,  typ,   locs]
fun mk_nuprl_until_prc_term                typ   X1    X2    = mk_nuprl_simple_term "until-prc"                [typ,   X1,    X2]
fun mk_nuprl_at_prc_term                   typ   X     locs  = mk_nuprl_simple_term "at-prc"                   [typ,   X,     locs]
fun mk_nuprl_parallel_prc_term             typ   X     Y     = mk_nuprl_simple_term "parallel-prc"             [typ,   X,     Y]
fun mk_nuprl_prior_init_prc_term           typ   X     init  = mk_nuprl_simple_term "prior-init-prc"           [typ,   X,     init]
fun mk_nuprl_ite_term                      term1 term2 term3 = mk_nuprl_simple_term "ifthenelse"               [term1, term2, term3]
fun mk_nuprl_reduce_term                   term1 term2 term3 = mk_nuprl_simple_term "reduce"                   [term1, term2, term3]
fun mk_nuprl_es_eq_E_term                  es    term1 term2 = mk_nuprl_simple_term "es-eq-E"                  [es,    term1, term2]
fun mk_nuprl_es_causl_term                 es    term1 term2 = mk_nuprl_simple_term "es-causl"                 [es,    term1, term2]
fun mk_nuprl_es_functional_class_term      es    typ   cls   = mk_nuprl_simple_term "es-functional-class"      [es,    typ,   cls]
fun mk_nuprl_classfun_term                 es    cls   e     = mk_nuprl_simple_term "classfun"                 [es,    cls,   e]
fun mk_nuprl_eq_bag_term                   deq   term1 term2 = mk_nuprl_simple_term "bag-eq"                   [deq,   term1, term2]
fun mk_nuprl_state_class1_term             init  tr    term  = mk_nuprl_simple_term "state-class1"             [init,  tr,    term]
fun mk_nuprl_state_class1_program_term     init  tr    term  = mk_nuprl_simple_term "state-class1-program"     [init,  tr,    term]
fun mk_nuprl_memory_class1_term            init  tr    term  = mk_nuprl_simple_term "memory-class1"            [init,  tr,    term]
fun mk_nuprl_memory_class1_program_term    init  tr    term  = mk_nuprl_simple_term "memory-class1-program"    [init,  tr,    term]
fun mk_nuprl_es_locl_term                  es    e1    e2    = mk_nuprl_simple_term "es-locl"                  [es,    e1,    e2]
fun mk_nuprl_es_le_term                    es    e1    e2    = mk_nuprl_simple_term "es-le"                    [es,    e1,    e2]
fun mk_nuprl_state1_term                   init  tr    term  = mk_nuprl_simple_term "State1"                   [init,  tr,    term]
fun mk_nuprl_memory1_term                  init  tr    term  = mk_nuprl_simple_term "Memory1"                  [init,  tr,    term]
fun mk_nuprl_make_msg_interface_term       i     loc   msg   = mk_nuprl_simple_term "make-msg-interface"       [i,     loc,   msg]

(* 4 arguments *)
fun mk_nuprl_product_deq_term                 typ1 typ2 deq1 deq2 = mk_nuprl_simple_term "product-deq"                 [typ1, typ2, deq1, deq2]
fun mk_nuprl_union_deq_term                   typ1 typ2 deq1 deq2 = mk_nuprl_simple_term "union-deq"                   [typ1, typ2, deq1, deq2]
fun mk_nuprl_bind_prc_term                    typA typB X    Y    = mk_nuprl_simple_term "bind-prc"                    [typA, typB, X,    Y]
fun mk_nuprl_loc_comb1_prc_term               typ  n    Xprs F    = mk_nuprl_simple_term "loc-comb1-prc"               [typ,  n,    Xprs, F]
fun mk_nuprl_rec_combined_loc_class1_prc_term typ  n    Xprs F    = mk_nuprl_simple_term "rec-combined-loc-class1-prc" [typ,  n,    Xprs, F]
fun mk_nuprl_state1_prc_term                  typ  init tr   term = mk_nuprl_simple_term "State1-prc"                  [typ,  init, tr,   term]
fun mk_nuprl_memory1_prc_term                 typ  init tr   term = mk_nuprl_simple_term "Memory1-prc"                 [typ,  init, tr,   term]

(* 5 arguments *)
fun mk_nuprl_classrel_term              es   T   X     e   v     = mk_nuprl_simple_term "classrel"              [es,   T,   X,     e,   v]
fun mk_nuprl_rec_bind_prc_term          A    B   X     Y   arg   = mk_nuprl_simple_term "rec-bind-prc"          [A,    B,   X,     Y,   arg]
fun mk_nuprl_state_class2_term          init tr1 term1 tr2 term2 = mk_nuprl_simple_term "state-class2"          [init, tr1, term1, tr2, term2]
fun mk_nuprl_state_class2_program_term  init tr1 term1 tr2 term2 = mk_nuprl_simple_term "state-class2-program"  [init, tr1, term1, tr2, term2]
fun mk_nuprl_memory_class2_term         init tr1 term1 tr2 term2 = mk_nuprl_simple_term "memory-class2"         [init, tr1, term1, tr2, term2]
fun mk_nuprl_memory_class2_program_term init tr1 term1 tr2 term2 = mk_nuprl_simple_term "memory-class2-program" [init, tr1, term1, tr2, term2]
fun mk_nuprl_state2_term                init tr1 term1 tr2 term2 = mk_nuprl_simple_term "State2"                [init, tr1, term1, tr2, term2]
fun mk_nuprl_memory2_term               init tr1 term1 tr2 term2 = mk_nuprl_simple_term "Memory2"               [init, tr1, term1, tr2, term2]

(* 6 arguments *)
fun mk_nuprl_rec_comb_prc_term typ n Xprs init F strict = mk_nuprl_simple_term "rec-comb-prc" [typ, n, Xprs, init, F, strict]

(* 7 arguments *)
fun mk_nuprl_state_class3_term          init tr1 term1 tr2 term2 tr3 term3 = mk_nuprl_simple_term "state-class3"          [init, tr1, term1, tr2, term2, tr3, term3]
fun mk_nuprl_state_class3_program_term  init tr1 term1 tr2 term2 tr3 term3 = mk_nuprl_simple_term "state-class3-program"  [init, tr1, term1, tr2, term2, tr3, term3]
fun mk_nuprl_memory_class3_term         init tr1 term1 tr2 term2 tr3 term3 = mk_nuprl_simple_term "memory-class3"         [init, tr1, term1, tr2, term2, tr3, term3]
fun mk_nuprl_memory_class3_program_term init tr1 term1 tr2 term2 tr3 term3 = mk_nuprl_simple_term "memory-class3-program" [init, tr1, term1, tr2, term2, tr3, term3]
fun mk_nuprl_state3_term                init tr1 term1 tr2 term2 tr3 term3 = mk_nuprl_simple_term "State3"                [init, tr1, term1, tr2, term2, tr3, term3]
fun mk_nuprl_memory3_term               init tr1 term1 tr2 term2 tr3 term3 = mk_nuprl_simple_term "Memory3"               [init, tr1, term1, tr2, term2, tr3, term3]

(* 8 arguments *)
fun mk_nuprl_state2_prc_term  typ1 typ2 typ init tr1 term1 tr2 term2 = mk_nuprl_simple_term "State2-prc"  [typ1, typ2, typ, init, tr1, term1, tr2, term2]
fun mk_nuprl_memory2_prc_term typ1 typ2 typ init tr1 term1 tr2 term2 = mk_nuprl_simple_term "Memory2-prc" [typ1, typ2, typ, init, tr1, term1, tr2, term2]

(* 9 arguments *)
fun mk_nuprl_state_class4_term          init tr1 term1 tr2 term2 tr3 term3 tr4 term4 = mk_nuprl_simple_term "state-class4"          [init, tr1, term1, tr2, term2, tr3, term3, tr4, term4]
fun mk_nuprl_state_class4_program_term  init tr1 term1 tr2 term2 tr3 term3 tr4 term4 = mk_nuprl_simple_term "state-class4-program"  [init, tr1, term1, tr2, term2, tr3, term3, tr4, term4]
fun mk_nuprl_memory_class4_term         init tr1 term1 tr2 term2 tr3 term3 tr4 term4 = mk_nuprl_simple_term "memory-class4"         [init, tr1, term1, tr2, term2, tr3, term3, tr4, term4]
fun mk_nuprl_memory_class4_program_term init tr1 term1 tr2 term2 tr3 term3 tr4 term4 = mk_nuprl_simple_term "memory-class4-program" [init, tr1, term1, tr2, term2, tr3, term3, tr4, term4]
fun mk_nuprl_state4_term                init tr1 term1 tr2 term2 tr3 term3 tr4 term4 = mk_nuprl_simple_term "State4"                [init, tr1, term1, tr2, term2, tr3, term3, tr4, term4]
fun mk_nuprl_memory4_term               init tr1 term1 tr2 term2 tr3 term3 tr4 term4 = mk_nuprl_simple_term "Memory4"               [init, tr1, term1, tr2, term2, tr3, term3, tr4, term4]

(* 11 arguments *)
fun mk_nuprl_state3_prc_term  typ1 typ2 typ3 typ init tr1 term1 tr2 term2 tr3 term3 = mk_nuprl_simple_term "State3-prc"  [typ1, typ2, typ3, typ, init, tr1, term1, tr2, term2, tr3, term3]
fun mk_nuprl_memory3_prc_term typ1 typ2 typ3 typ init tr1 term1 tr2 term2 tr3 term3 = mk_nuprl_simple_term "Memory3-prc" [typ1, typ2, typ3, typ, init, tr1, term1, tr2, term2, tr3, term3]

(* 14 arguments *)
fun mk_nuprl_state4_prc_term  typ1 typ2 typ3 typ4 typ init tr1 term1 tr2 term2 tr3 term3 tr4 term4 = mk_nuprl_simple_term "State4-prc"  [typ1, typ2, typ3, typ4, typ, init, tr1, term1, tr2, term2, tr3, term3, tr4, term4]
fun mk_nuprl_memory4_prc_term typ1 typ2 typ3 typ4 typ init tr1 term1 tr2 term2 tr3 term3 tr4 term4 = mk_nuprl_simple_term "Memory4-prc" [typ1, typ2, typ3, typ4, typ, init, tr1, term1, tr2, term2, tr3, term3, tr4, term4]

fun mk_nuprl_eq_prod_term deq1 deq2 nt1 nt2 =
    let	val bdeq = mk_nuprl_proddeq_term deq1 deq2
	val app1 = mk_apply_term bdeq nt1
	val app2 = mk_apply_term app1 nt2
    in app2
    end

fun mk_nuprl_eq_union_term deq1 deq2 nt1 nt2 =
    let	val bdeq = mk_nuprl_sumdeq_term deq1 deq2
	val app1 = mk_apply_term bdeq nt1
	val app2 = mk_apply_term app1 nt2
    in app2
    end

fun mk_nuprl_eq_list_term deq nt1 nt2 =
    let	val bdeq = mk_nuprl_list_deq_term deq
	val app1 = mk_apply_term bdeq nt1
	val app2 = mk_apply_term app1 nt2
    in app2
    end

fun mk_nuprl_list_ind_term lst nilcase (x, xs, r, conscase) =
    mk_nuprl_term
	("list_ind", [])
	[([], lst),
	 ([], nilcase),
	 ([mk_new_nuprl_var x, mk_new_nuprl_var xs, mk_new_nuprl_var r], conscase)]

fun mk_nuprl_itext_list_term [] = mk_nuprl_itext_nil_term
  | mk_nuprl_itext_list_term (x :: xs) =
    mk_nuprl_itext_cons_term x (mk_nuprl_itext_list_term xs)

fun mk_nuprl_all_term    term1 (var, term2) = mk_nuprl_term ("all",    []) [([], term1), ([mk_new_nuprl_var var], term2)]
fun mk_nuprl_uall_term   term1 (var, term2) = mk_nuprl_term ("uall",   []) [([], term1), ([mk_new_nuprl_var var], term2)]
fun mk_nuprl_exists_term term1 (var, term2) = mk_nuprl_term ("exists", []) [([], term1), ([mk_new_nuprl_var var], term2)]
fun mk_nuprl_isect_term  term1 (var, term2) = mk_nuprl_term ("isect",  []) [([], term1), ([mk_new_nuprl_var var], term2)]
fun mk_nuprl_lall_term   term1 (var, term2) = mk_nuprl_term ("l_all",  []) [([], term1), ([mk_new_nuprl_var var], term2)]

fun mk_nuprl_rec_ind_term arg (f, x, B) =
    mk_nuprl_term ("rec_ind", []) [([], arg), ([f, x], B)]

fun mk_nuprl_rec_ind_ref_term arg (f, x, B) =
    mk_nuprl_ref_term ("rec_ind", []) [([], mk_rterm arg), ([f, x], B)]

fun mk_nuprl_finite_list_term list =
    foldr (fn (elt, list) => mk_nuprl_cons_term elt list)
	  mk_nuprl_nil_term
	  list

fun mk_nuprl_event_orderingp_term i f =
    mk_nuprl_term ("event-ordering+", [mk_nuprl_level_exp_parameter i])
		  [([], mk_nuprl_msg_term f)]

fun mk_nuprl_event_ordering_p_term f = mk_nuprl_event_orderingp_term "i" f

fun mk_nuprl_class_term f term =
    mk_nuprl_term ("eclass", [mk_nuprl_level_exp_parameter "i"])
		  [([], mk_nuprl_msg_term f),
		   ([mk_new_nuprl_var "es", mk_new_nuprl_var "e"], term)]

fun mk_nuprl_normal_locally_programmable_term f typ class =
    mk_nuprl_term ("normal-locally-programmable", [mk_nuprl_level_exp_parameter "i"])
		  [([], mk_nuprl_msg_term f),
		   ([], typ),
		   ([], class)]

val mk_nuprl_nlp_term = mk_nuprl_normal_locally_programmable_term

fun mk_nuprl_local_class_msg_term f typ class =
    mk_nuprl_term ("local-class", [mk_nuprl_level_exp_parameter "i"])
		  [([], mk_nuprl_msg_term f),
		   ([], typ),
		   ([], class)]

fun mk_nuprl_programmable_term f typ class =
    mk_nuprl_term ("programmable", [mk_nuprl_level_exp_parameter "i"])
		  [([], mk_nuprl_msg_term f),
		   ([], typ),
		   ([], class)]

fun mk_nuprl_iabstraction_term t1 t2 =
    mk_nuprl_simple_term "!abstraction" [mk_nuprl_condition_cons_term, t1, t2]

fun mk_nuprl_itheorem_term name term =
    mk_nuprl_term ("!theorem", [mk_nuprl_token_parameter name]) [([], term)]

fun mk_nuprl_iupdate_term name term =
    mk_nuprl_term ("!update", [mk_nuprl_token_parameter name]) [([], term)]

fun mk_nuprl_iinsert_object_term term =
    mk_nuprl_simple_term "!insert_object_id_in_operator" [term]

fun mk_nuprl_iinsert_object_p_term name term =
    mk_nuprl_term ("!insert_object_id_in_operator", [mk_nuprl_token_parameter name, mk_nuprl_natural_parameter 0]) [([], term)]

fun mk_nuprl_iproperty_term name value =
    mk_nuprl_term ("!property", [mk_nuprl_token_parameter name]) [([], value)]

fun mk_nuprl_istring_term string =
    mk_nuprl_term ("!string", [mk_nuprl_string_parameter string]) []

fun mk_nuprl_ibool_term bool =
    mk_nuprl_term ("!bool", [mk_nuprl_bool_parameter bool]) []

fun mk_nuprl_ilist_term [] = mk_nuprl_icons_nil_term
  | mk_nuprl_ilist_term (term :: terms) = mk_nuprl_icons_cons_term term (mk_nuprl_ilist_term terms)

fun mk_nuprl_itext_term str =
    mk_nuprl_term  ("!text", [mk_nuprl_string_parameter str]) []

fun mk_nuprl_iwf_lemmas_term wf_lemmas = mk_nuprl_simple_term "!wf" wf_lemmas

fun mk_nuprl_icomment_term name comment =
    mk_nuprl_term ("!comment", [mk_nuprl_string_parameter name]) [([], comment)]

(*fun mk_nuprl_mlnk_term term = mk_nuprl_simple_term "mlnk" [term]
fun mk_nuprl_mtag_term term = mk_nuprl_simple_term "mtag" [term]
fun mk_nuprl_mval_term term = mk_nuprl_simple_term "mval" [term]*)

(*
fun build_primitive_value term params =
    let val opid = opid_of_term term
    in if List.exists (fn x => x = opid) ["inl", "inr", "pair", "cons"]
       then mk_nuprl_simple_term opid params
       else term
    end
*)

fun toDeq term =
    if is_nuprl_int_term term
    then mk_nuprl_int_deq_term
    else if is_nuprl_bool_term term
    then mk_nuprl_bool_deq_term
    else if is_nuprl_unit_term term
    then mk_nuprl_unit_deq_term
    else if is_nuprl_loc_term term
    then mk_nuprl_loc_deq_term
    else if is_nuprl_atom_term term
    then mk_nuprl_atom_deq_term
    else if is_nuprl_list_term term
    then mk_nuprl_list_deq_term (toDeq (dest_type_list term))
    else if is_nuprl_product_term term
    then let val (t1, t2) = dest_simple_product term
	 in mk_nuprl_proddeq_term (toDeq t1) (toDeq t2)
	 end
    else raise Fail "toDeq"


fun do_primitive_int_op opid value1 value2 =
    let (*val _  = print ("[doing primitive operator on int]\n")
	val _  = print ("[" ^ opid_of_term value1 ^ "]\n")
	val _  = print ("[" ^ opid_of_term value2 ^ "]\n")
	val _  = print (toStringTerm value1 ^ "\n" ^ toStringTerm value2 ^ "\n")*)
	val n1 = dest_integer 2 value1
	val n2 = dest_integer 3 value2
	val n  = case opid of
		     "add"       => II.+   (n1, n2)
		   | "subtract"  => II.-   (n1, n2)
		   | "multiply"  => II.*   (n1, n2)
		   | "divide"    => II.div (n1, n2)
		   | "remainder" => II.rem (n1, n2)
		   | _           => raise Fail "wrong term"
    in mk_nuprl_integer_term n
    end

fun do_primitive_wait value exp =
    let val n = dest_integer 4 value
	val _ = print ("[---------sleeping for " ^ II.toString n ^ "s---------]\n")
	val _ = OS.Process.sleep (Time.fromSeconds (II.toLarge n))
    in exp
    end

fun do_primitive_ref_wait value rterm =
    let val n = dest_integer 4 value
	val _ = print ("[---------sleeping for " ^ II.toString n ^ "s---------]\n")
	val _ = OS.Process.sleep (Time.fromSeconds (II.toLarge n))
    in rterm
    end

fun do_primitive_minus value =
    let val n = dest_integer 4 value
    in mk_nuprl_integer_term (~n)
    end

fun do_primitive_cmp cmp value1 value2 =
    if cmp = "int_eq" orelse cmp = "less"
    then let val n1 = dest_integer 5 value1
	     val n2 = dest_integer 6 value2
	 in if cmp = "less"
	    then II.< (n1, n2)
	    else n1 = n2
	 end
    else raise Fail "unknown primitive comparison"

fun is_zero term = II.compare (dest_integer 7 term, 0)

fun inc_integer term = mk_nuprl_integer_term (II.+ (dest_integer 8 term, 1))
fun dec_integer term = mk_nuprl_integer_term (II.- (dest_integer 9 term, 1))

fun compare_atomn n value1 value2 =
    let val op_id1 = opid_of_term value1
	val op_id2 = opid_of_term value2
    in case (parameters_of_term value1,
	     parameters_of_term value2) of
	   ([param1], [param2]) =>
	   (* NOTE: both values should only have one parameter *)
	   let val ptype = type_of_parameter param1
	   in if op_id1 = "token"
		 andalso op_id1 = op_id2
		 (* NOTE: both values have to be tokens *)
		 andalso ptype = type_of_parameter param2
		 (* NOTE: both value have to have the the same kind of parameters *)
		 andalso (case n of (* NOTE: what are these: *)
			      0 => (ptype = "token" orelse ptype = "t")
			    | 1 => ptype = "ut1"
			    | 2 => ptype = "ut2"
			    | _ => false)
	      then equal_parameters param1 param2
	      else raise Fail ("compare_atomn - "
			       ^ op_id1 ^ " "
			       ^ opid_of_term value2 ^ " "
			       ^ ptype ^ " "
			       ^ type_of_parameter param2 ^ " "
			       ^ Int.toString n)
	   end
	 | (ps1, ps2) =>
	   raise Fail ("compare_atomn("
		       ^ op_id1 ^ ":"
		       ^ Int.toString (List.length ps1) ^ ","
		       ^ op_id2 ^ ":"
		       ^ Int.toString (List.length ps2) ^ ","
		       ^ Int.toString n
		       ^ ")")
    end

(* ------ FREE VARS and SUBSTITUTIONS ------ *)

fun remove_list vars lst =
    foldr (fn (var, vars) => VARS.delete (vars, var) handle _ => vars)
	  vars
	  lst

fun new_free_var frees var =
    let val var' = var ^ "'"
	(*val _ = print ("[new var: " ^ var' ^ "]\n")*)
    in if VARS.member (frees, var')
       then new_free_var frees var'
       else var'
    end

datatype ran_sub = FO of nuprl_term
		 | SO of variable list * nuprl_term

fun filter_sub vars = SUB.filteri (fn (v, _) => not (VARS.member (vars, v)))

fun filter_ren vars = SUB.filteri (fn (v, _) => not (VARS.member (vars, v)))

fun insert_sub sub (v, t) = SUB.insert (sub, v, FO t)

fun insert_ren ren (v, t) = SUB.insert (ren, v, t)

fun insert_list_sub sub list = foldr (fn (x, sub) => insert_sub sub x) sub list

fun insert_list_ren ren list = foldr (fn (x, ren) => insert_ren ren x) ren list

val empty_sub = SUB.empty : ran_sub SUB.map

val empty_ren = SUB.empty : variable SUB.map

val gen_sub = insert_list_sub empty_sub

val gen_ren = insert_list_ren empty_ren

fun apply_ren ren v =
    case SUB.find (ren, v) of
	NONE => v
      | SOME u => u

fun rename_parameter ren (v,k) =
    case SUB.find (ren, v) of
	SOME v' => (v', k)
      | NONE    => (v,  k)

fun rename_operator (opid_tag, params) ren =
    let val params' = map (rename_parameter ren) params
    in (opid_tag, params')
    end

fun fo_subst_aux (sub, ren) (term as TERM (operator, bterms)) =
    if is_nuprl_variable_term term
    then let val (v, ts) = dest_so_variable term
	 in apply_sub (sub, ren) (v, ts) term
	 end
    else let val operator' = rename_operator operator ren
	     val bterms'   = map (fo_subst_bterm (sub, ren)) bterms
	 in TERM (operator', bterms')
	 end
  | fo_subst_aux (sub, ren) (term as AXM_TERM) = term
  | fo_subst_aux (sub, ren) (term as BOT_TERM) = term
  | fo_subst_aux (sub, ren) (term as INT_TERM) = term
  | fo_subst_aux (sub, ren) (term as VOI_TERM) = term
  | fo_subst_aux (sub, ren) (term as DUM_TERM) = term
  | fo_subst_aux (sub, ren) (term as ATM_TERM _) = term
  | fo_subst_aux (sub, ren) (term as TOK_TERM (t,k)) =
    TOK_TERM (rename_parameter ren (t,k))
  | fo_subst_aux (sub, ren) (term as NAT_TERM _) = term
  | fo_subst_aux (sub, ren) (term as VAR_TERM var) =
    apply_sub (sub, ren) (dest_nuprl_var var, []) term
  | fo_subst_aux (sub, ren) (term as INL_TERM rterm) = INL_TERM (fo_subst_rterm (sub, ren) rterm)
  | fo_subst_aux (sub, ren) (term as INR_TERM rterm) = INR_TERM (fo_subst_rterm (sub, ren) rterm)
  | fo_subst_aux (sub, ren) (term as FIX_TERM rterm) = FIX_TERM (fo_subst_rterm (sub, ren) rterm)
  | fo_subst_aux (sub, ren) (term as MIN_TERM rterm) = MIN_TERM (fo_subst_rterm (sub, ren) rterm)
  | fo_subst_aux (sub, ren) (term as LAM_TERM (var, rterm)) =
    let val (vars, t) = fo_rename (sub, ren) ([var], rterm2term rterm)
    in case vars of
	   [v] => LAM_TERM (v, mk_rterm t)
	 | _   => raise Fail "fo_subst_aux:LAM_TERM"
    end
  | fo_subst_aux (sub, ren) (term as REC_TERM (var, rterm)) =
    let val (vars, t) = fo_rename (sub, ren) ([var], rterm2term rterm)
    in case vars of
	   [v] => REC_TERM (v, mk_rterm t)
	 | _   => raise Fail "fo_subst_aux:REC_TERM"
    end
  | fo_subst_aux (sub, ren) (term as WAI_TERM (rterm1, rterm2)) =
    WAI_TERM (fo_subst_rterm (sub, ren) rterm1,
	      fo_subst_rterm (sub, ren) rterm2)
  | fo_subst_aux (sub, ren) (term as APP_TERM (rterm1, rterm2)) =
    APP_TERM (fo_subst_rterm (sub, ren) rterm1,
	      fo_subst_rterm (sub, ren) rterm2)
  | fo_subst_aux (sub, ren) (term as PAI_TERM (rterm1, rterm2)) =
    PAI_TERM (fo_subst_rterm (sub, ren) rterm1,
	      fo_subst_rterm (sub, ren) rterm2)
  | fo_subst_aux (sub, ren) (term as ADD_TERM (rterm1, rterm2)) =
    ADD_TERM (fo_subst_rterm (sub, ren) rterm1,
	      fo_subst_rterm (sub, ren) rterm2)
  | fo_subst_aux (sub, ren) (term as SUB_TERM (rterm1, rterm2)) =
    SUB_TERM (fo_subst_rterm (sub, ren) rterm1,
	      fo_subst_rterm (sub, ren) rterm2)
  | fo_subst_aux (sub, ren) (term as MUL_TERM (rterm1, rterm2)) =
    MUL_TERM (fo_subst_rterm (sub, ren) rterm1,
	      fo_subst_rterm (sub, ren) rterm2)
  | fo_subst_aux (sub, ren) (term as DIV_TERM (rterm1, rterm2)) =
    DIV_TERM (fo_subst_rterm (sub, ren) rterm1,
	      fo_subst_rterm (sub, ren) rterm2)
  | fo_subst_aux (sub, ren) (term as REM_TERM (rterm1, rterm2)) =
    REM_TERM (fo_subst_rterm (sub, ren) rterm1,
	      fo_subst_rterm (sub, ren) rterm2)
  | fo_subst_aux (sub, ren) (term as EQT_TERM (rterm1, rterm2)) =
    EQT_TERM (fo_subst_rterm (sub, ren) rterm1,
	      fo_subst_rterm (sub, ren) rterm2)
  | fo_subst_aux (sub, ren) (term as UNI_TERM (rterm1, rterm2)) =
    UNI_TERM (fo_subst_rterm (sub, ren) rterm1,
	      fo_subst_rterm (sub, ren) rterm2)
  | fo_subst_aux (sub, ren) (term as EQU_TERM (a, rterm1, rterm2)) =
    let val a  = fo_subst_rterm (sub, ren) a
	val t1 = fo_subst_rterm (sub, ren) rterm1
	val t2 = fo_subst_rterm (sub, ren) rterm2
    in EQU_TERM (a, t1, t2)
    end
  | fo_subst_aux (sub, ren) (term as IAX_TERM (a, rterm1, rterm2)) =
    let val a  = fo_subst_rterm (sub, ren) a
	val t1 = fo_subst_rterm (sub, ren) rterm1
	val t2 = fo_subst_rterm (sub, ren) rterm2
    in IAX_TERM (a, t1, t2)
    end
  | fo_subst_aux (sub, ren) (term as IPA_TERM (a, rterm1, rterm2)) =
    let val a  = fo_subst_rterm (sub, ren) a
	val t1 = fo_subst_rterm (sub, ren) rterm1
	val t2 = fo_subst_rterm (sub, ren) rterm2
    in IPA_TERM (a, t1, t2)
    end
  | fo_subst_aux (sub, ren) (term as IIR_TERM (a, rterm1, rterm2)) =
    let val a  = fo_subst_rterm (sub, ren) a
	val t1 = fo_subst_rterm (sub, ren) rterm1
	val t2 = fo_subst_rterm (sub, ren) rterm2
    in IIR_TERM (a, t1, t2)
    end
  | fo_subst_aux (sub, ren) (term as IIL_TERM (a, rterm1, rterm2)) =
    let val a  = fo_subst_rterm (sub, ren) a
	val t1 = fo_subst_rterm (sub, ren) rterm1
	val t2 = fo_subst_rterm (sub, ren) rterm2
    in IIL_TERM (a, t1, t2)
    end
  | fo_subst_aux (sub, ren) (term as IIN_TERM (a, rterm1, rterm2)) =
    let val a  = fo_subst_rterm (sub, ren) a
	val t1 = fo_subst_rterm (sub, ren) rterm1
	val t2 = fo_subst_rterm (sub, ren) rterm2
    in IIN_TERM (a, t1, t2)
    end
  | fo_subst_aux (sub, ren) (term as ILA_TERM (a, rterm1, rterm2)) =
    let val a  = fo_subst_rterm (sub, ren) a
	val t1 = fo_subst_rterm (sub, ren) rterm1
	val t2 = fo_subst_rterm (sub, ren) rterm2
    in ILA_TERM (a, t1, t2)
    end
  | fo_subst_aux (sub, ren) (term as IAT_TERM (a, rterm1, rterm2)) =
    let val a  = fo_subst_rterm (sub, ren) a
	val t1 = fo_subst_rterm (sub, ren) rterm1
	val t2 = fo_subst_rterm (sub, ren) rterm2
    in IAT_TERM (a, t1, t2)
    end
  | fo_subst_aux (sub, ren) (term as CBV_TERM (a, var, f)) =
    let val a = fo_subst_rterm (sub, ren) a
	val (vars,f) = fo_rename (sub, ren) ([var], rterm2term f)
    in case vars of
	   [x] => CBV_TERM (a, x, mk_rterm f)
	 | _ => raise Fail "fo_subst_aux:CBV_TERM"
    end
  | fo_subst_aux (sub, ren) (term as CBA_TERM (a, var, f)) =
    let val a = fo_subst_rterm (sub, ren) a
	val (vars,f) = fo_rename (sub, ren) ([var], rterm2term f)
    in case vars of
	   [x] => CBA_TERM (a, x, mk_rterm f)
	 | _ => raise Fail "fo_subst_aux:CBA_TERM"
    end
  | fo_subst_aux (sub, ren) (term as FUN_TERM (a, var, f)) =
    let val a = fo_subst_rterm (sub, ren) a
	val (vars,f) = fo_rename (sub, ren) ([var], rterm2term f)
    in case vars of
	   [x] => FUN_TERM (a, x, mk_rterm f)
	 | _ => raise Fail "fo_subst_aux:FUN_TERM"
    end
  | fo_subst_aux (sub, ren) (term as PRD_TERM (a, var, f)) =
    let val a = fo_subst_rterm (sub, ren) a
	val (vars,f) = fo_rename (sub, ren) ([var], rterm2term f)
    in case vars of
	   [x] => PRD_TERM (a, x, mk_rterm f)
	 | _ => raise Fail "fo_subst_aux:PRD_TERM"
    end
  | fo_subst_aux (sub, ren) (term as TUN_TERM (a, var, f)) =
    let val a = fo_subst_rterm (sub, ren) a
	val (vars,f) = fo_rename (sub, ren) ([var], rterm2term f)
    in case vars of
	   [x] => TUN_TERM (a, x, mk_rterm f)
	 | _ => raise Fail "fo_subst_aux:TUN_TERM"
    end
  | fo_subst_aux (sub, ren) (term as SET_TERM (a, var, f)) =
    let val a = fo_subst_rterm (sub, ren) a
	val (vars,f) = fo_rename (sub, ren) ([var], rterm2term f)
    in case vars of
	   [x] => SET_TERM (a, x, mk_rterm f)
	 | _ => raise Fail "fo_subst_aux:SET_TERM"
    end
  | fo_subst_aux (sub, ren) (term as LES_TERM (a, b, rterm1, rterm2)) =
    let val a  = fo_subst_rterm (sub, ren) a
	val b  = fo_subst_rterm (sub, ren) b
	val t1 = fo_subst_rterm (sub, ren) rterm1
	val t2 = fo_subst_rterm (sub, ren) rterm2
    in LES_TERM (a, b, t1, t2)
    end
  | fo_subst_aux (sub, ren) (term as IEQ_TERM (a, b, rterm1, rterm2)) =
    let val a  = fo_subst_rterm (sub, ren) a
	val b  = fo_subst_rterm (sub, ren) b
	val t1 = fo_subst_rterm (sub, ren) rterm1
	val t2 = fo_subst_rterm (sub, ren) rterm2
    in IEQ_TERM (a, b, t1, t2)
    end
  | fo_subst_aux (sub, ren) (term as SPR_TERM (pair, var1, var2, rterm)) =
    let val p = fo_subst_rterm (sub, ren) pair
	val (vars, t) = fo_rename (sub, ren) ([var1, var2], rterm2term rterm)
    in case vars of
	   [v1,v2] => SPR_TERM (p, v1, v2, mk_rterm t)
	 | _ => raise Fail "fo_subst_aux:SPR_TERM"
    end
  | fo_subst_aux (sub, ren) (term as AEQ_TERM (n, a, b, rterm1, rterm2)) =
    let val a  = fo_subst_rterm (sub, ren) a
	val b  = fo_subst_rterm (sub, ren) b
	val t1 = fo_subst_rterm (sub, ren) rterm1
	val t2 = fo_subst_rterm (sub, ren) rterm2
    in AEQ_TERM (n, a, b, t1, t2)
    end
  | fo_subst_aux (sub, ren) (term as DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    let val d = fo_subst_rterm (sub, ren) dec
	val (vars1, t1) = fo_rename (sub, ren) ([var1], rterm2term rterm1)
	val (vars2, t2) = fo_rename (sub, ren) ([var2], rterm2term rterm2)
    in case (vars1, vars2) of
	   ([v1], [v2]) => DEC_TERM (d, v1, mk_rterm t1, v2, mk_rterm t2)
	 | _ => raise Fail "fo_subst_aux:DEC_TERM"
    end
  | fo_subst_aux (sub, ren) (term as IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) =
    let val i = fo_subst_rterm (sub, ren) i
	val (vars1, d) = fo_rename (sub, ren) ([x, rd], rterm2term downcase)
	val b = fo_subst_rterm (sub, ren) basecase
	val (vars2, u) = fo_rename (sub, ren) ([y, ru], rterm2term upcase)
    in case (vars1, vars2) of
	   ([vx, vrd], [vy, vru]) =>
	   IND_TERM (i, vx, vrd, mk_rterm d, b, vy, vru, mk_rterm u)
	 | _ => raise Fail "fo_subst_aux:IND_TERM"
    end
  | fo_subst_aux (sub, ren) (term as CLO_TERM clos) =
    CLO_TERM (fo_subst_clos (sub, ren) clos)

and fo_subst_rterm (sub, ren) rterm =
    mk_rterm (fo_subst_aux (sub, ren) (rterm2term rterm))

and fo_subst_clos (sub, ren) (rterm, env) =
    let val term  = rterm2term rterm
	val dom   = domain env
	val sub'  = filter_sub dom sub
	val ren'  = filter_ren dom ren
	val term' = fo_subst_aux (sub', ren') term
	val env'  = fo_subst_env (sub', ren') env
    in (mk_rterm term', env')
    end

and fo_subst_env (sub, ren) (ENV m) =
    let val _ =
	    MAP.app
		(fn (_,_,termref) =>
		    let val (t,b) = !termref
			val term = fo_subst_aux (sub, ren) t
		    in termref := (term, b)
		    end)
		m
    in ENV m
    end

and fo_subst_bterm (sub, ren) (B_TERM (vars, term)) =
    let val (vars', term') = fo_rename (sub, ren) (vars, rterm2term term)
    in B_TERM (vars', mk_rterm term')
    end

and apply_sub (sub, ren) (v, ts) t =
    case SUB.find (sub, v) of
	NONE => t
      | SOME (FO t') => t'
      | SOME (SO (vars, t')) =>
	let val lst =
		ListPair.map
		    (fn (v1, t2) => (v1, fo_subst_aux (sub, empty_ren) t2))
		    (vars, ts)
	in fo_subst_aux (gen_sub lst, empty_ren) t'
	end handle _ => raise Fail "apply_sub"

(* renames the variables in vars (and term) that occur in the
 * range of sub, and also remove the part of sub such that its
 * domain is in vars. *)
and fo_rename (sub, ren) (vars, term) =
    let val set = nvars2set VARS.empty vars
	val sub' = filter_sub set sub
	(* freesSub is the free variable list of sub's range.
	 * In the (SO (vars, t)) case, we only need to get the free variables
	 * in t that are not in vars. *)
	val freesSub =
	    SUB.foldr
		(fn (SO (vs, t), vars) =>
		    VARS.union (vars, fo_free_vars (VARS.fromList vs) t)
		  | (FO t, vars) =>
		    VARS.union (vars, free_vars t))
		empty_vars
		sub'
	val freesTerm = free_vars term
	val getNewFreeVar = new_free_var (VARS.union (freesSub, freesTerm))
	val (vars', sub'') =
	    List.foldr (fn (nvar, (vars, sub)) =>
			   let val var = dest_nuprl_var nvar
			   in if VARS.member (freesSub, var)
			      (* then the bound variable would capture one of
			       * the free variables of the substitution. *)
			      then let val var'  = getNewFreeVar var
				       val tvar' = mk_variable_term var'
				       val nvar' = upd_nuprl_var nvar var'
				   in (nvar' :: vars, insert_sub sub (var, tvar'))
				   end
			      else (nvar :: vars, sub)
			   end)
		       ([], sub')
		       vars
    in (vars', fo_subst_aux (sub'', ren) term)
    end

fun fo_subst list term =
    fo_subst_aux
	(foldr (fn ((v,t),sub) =>
		   insert_sub sub (dest_nuprl_var v,t))
	       empty_sub
	       list,
	 empty_ren)
	term

fun correct_lhs (vars, rterm) =
    let val term = rterm2term rterm
    in if List.null vars
       then if is_nuprl_variable_term term
	    then dest_variable term
	    else raise Fail "correct_lhs"
       else let val (v, ts) = dest_so_variable term
	    in if ListPair.all
		      (fn (v, t) =>
			  (dest_nuprl_var v = dest_variable t)
			  handle _ => false)
		      (vars, ts)
	       then v
	       else raise Fail "correct_lhs"
	    end handle _ => raise Fail "correct_lhs"
    end

fun matching term1 (*term*) term2 (*lhs*) =
    let val ((opid1, params1), bterms1) = dest_term term1
	val ((opid2, params2), bterms2) = dest_term term2
	val (sub_subterms, is_fo_sub) =
	    if opid1 = opid2 andalso length bterms1 = length bterms2
	    then ListPair.foldr
		     (fn ((vs1, t1), bterm2, (sub, is_fo_sub)) =>
			 let val b = is_fo_sub andalso null vs1
			     val v = correct_lhs bterm2
			     val so = SO (map dest_nuprl_var vs1, rterm2term t1)
			 in (SUB.insert (sub, v, so), b)
			   end)
		     (empty_sub, true)
		     (bterms1, bterms2)
	    else raise Fail "matching"
	val (ren_params, is_em_ren) =
	    if length params1 = length params2
	    then ListPair.foldr
		     (fn ((v1, k1), (v2, k2), (ren, is_em_ren)) =>
			 if eq_kinds (k1, k2)
			    andalso
			    not (is_abstract_metavar v1)
			 then if is_abstract_metavar v2
			      then (insert_ren ren (v2, v1), false)
			      else if v1 = v2
			      then (ren, is_em_ren)
			      else raise Fail "matching"
			 else raise Fail "matching")
		     (empty_ren, true)
		     (params1, params2)
	    else raise Fail "matching"
    in ((sub_subterms, is_fo_sub), (ren_params, is_em_ren))
    end


(* -- ... -- *)

fun replace_prog (id, p) (TERM (operator as ((opid, tag), params), bterms)) =
    if id = opid
    then p
    else let val bterms' =
		 map (fn (B_TERM (vars, term)) =>
			 B_TERM (vars, mk_rterm (replace_prog (id, p) (rterm2term term))))
		     bterms
	 in TERM (operator, bterms')
	 end
  | replace_prog (id, p) (term as AXM_TERM) = term
  | replace_prog (id, p) (term as BOT_TERM) = term
  | replace_prog (id, p) (term as INT_TERM) = term
  | replace_prog (id, p) (term as VOI_TERM) = term
  | replace_prog (id, p) (term as DUM_TERM) = term
  | replace_prog (id, p) (term as ATM_TERM _) = term
  | replace_prog (id, p) (term as TOK_TERM _) = term
  | replace_prog (id, p) (term as NAT_TERM _) = term
  | replace_prog (id, p) (term as VAR_TERM _) = term
  | replace_prog (id, p) (term as INL_TERM rterm) =
    INL_TERM (mk_rterm (replace_prog (id, p) (rterm2term rterm)))
  | replace_prog (id, p) (term as INR_TERM rterm) =
    INR_TERM (mk_rterm (replace_prog (id, p) (rterm2term rterm)))
  | replace_prog (id, p) (term as FIX_TERM rterm) =
    FIX_TERM (mk_rterm (replace_prog (id, p) (rterm2term rterm)))
  | replace_prog (id, p) (term as MIN_TERM rterm) =
    MIN_TERM (mk_rterm (replace_prog (id, p) (rterm2term rterm)))
  | replace_prog (id, p) (term as LAM_TERM (var, rterm)) =
    LAM_TERM (var, mk_rterm (replace_prog (id, p) (rterm2term rterm)))
  | replace_prog (id, p) (term as REC_TERM (var, rterm)) =
    REC_TERM (var, mk_rterm (replace_prog (id, p) (rterm2term rterm)))
  | replace_prog (id, p) (term as WAI_TERM (rterm1, rterm2)) =
    WAI_TERM (replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as APP_TERM (rterm1, rterm2)) =
    APP_TERM (replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as PAI_TERM (rterm1, rterm2)) =
    PAI_TERM (replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as ADD_TERM (rterm1, rterm2)) =
    ADD_TERM (replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as SUB_TERM (rterm1, rterm2)) =
    SUB_TERM (replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as MUL_TERM (rterm1, rterm2)) =
    MUL_TERM (replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as DIV_TERM (rterm1, rterm2)) =
    DIV_TERM (replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as REM_TERM (rterm1, rterm2)) =
    REM_TERM (replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as EQT_TERM (rterm1, rterm2)) =
    EQT_TERM (replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as UNI_TERM (rterm1, rterm2)) =
    UNI_TERM (replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as EQU_TERM (a, rterm1, rterm2)) =
    EQU_TERM (replace_rprog (id, p) a,
	      replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as IAX_TERM (a, rterm1, rterm2)) =
    IAX_TERM (replace_rprog (id, p) a,
	      replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as IPA_TERM (a, rterm1, rterm2)) =
    IPA_TERM (replace_rprog (id, p) a,
	      replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as IIR_TERM (a, rterm1, rterm2)) =
    IIR_TERM (replace_rprog (id, p) a,
	      replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as IIL_TERM (a, rterm1, rterm2)) =
    IIL_TERM (replace_rprog (id, p) a,
	      replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as IIN_TERM (a, rterm1, rterm2)) =
    IIN_TERM (replace_rprog (id, p) a,
	      replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as ILA_TERM (a, rterm1, rterm2)) =
    ILA_TERM (replace_rprog (id, p) a,
	      replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as IAT_TERM (a, rterm1, rterm2)) =
    IAT_TERM (replace_rprog (id, p) a,
	      replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as CBV_TERM (a, x, f)) =
    CBV_TERM (replace_rprog (id, p) a,
	      x,
	      replace_rprog (id, p) f)
  | replace_prog (id, p) (term as CBA_TERM (a, x, f)) =
    CBA_TERM (replace_rprog (id, p) a,
	      x,
	      replace_rprog (id, p) f)
  | replace_prog (id, p) (term as FUN_TERM (a, x, f)) =
    FUN_TERM (replace_rprog (id, p) a,
	      x,
	      replace_rprog (id, p) f)
  | replace_prog (id, p) (term as PRD_TERM (a, x, f)) =
    PRD_TERM (replace_rprog (id, p) a,
	      x,
	      replace_rprog (id, p) f)
  | replace_prog (id, p) (term as TUN_TERM (a, x, f)) =
    TUN_TERM (replace_rprog (id, p) a,
	      x,
	      replace_rprog (id, p) f)
  | replace_prog (id, p) (term as SET_TERM (a, x, f)) =
    SET_TERM (replace_rprog (id, p) a,
	      x,
	      replace_rprog (id, p) f)
  | replace_prog (id, p) (term as LES_TERM (a, b, rterm1, rterm2)) =
    LES_TERM (replace_rprog (id, p) a,
	      replace_rprog (id, p) b,
	      replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as IEQ_TERM (a, b, rterm1, rterm2)) =
    IEQ_TERM (replace_rprog (id, p) a,
	      replace_rprog (id, p) b,
	      replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as SPR_TERM (pair, var1, var2, rterm)) =
    SPR_TERM (replace_rprog (id, p) pair,
	      var1,
	      var2,
	      replace_rprog (id, p) rterm)
  | replace_prog (id, p) (term as AEQ_TERM (n, a, b, rterm1, rterm2)) =
    AEQ_TERM (n,
	      replace_rprog (id, p) a,
	      replace_rprog (id, p) b,
	      replace_rprog (id, p) rterm1,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    DEC_TERM (replace_rprog (id, p) dec,
	      var1,
	      replace_rprog (id, p) rterm1,
	      var2,
	      replace_rprog (id, p) rterm2)
  | replace_prog (id, p) (term as IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) =
    IND_TERM (replace_rprog (id, p) i,
	      x,
	      rd,
	      replace_rprog (id, p) downcase,
	      replace_rprog (id, p) basecase,
	      y,
	      ru,
	      replace_rprog (id, p) upcase)
  | replace_prog (id, p) (CLO_TERM clos) = raise Fail "replace_prog:C_TERM"

and replace_rprog (id,p) rterm = mk_rterm (replace_prog (id,p) (rterm2term rterm))

fun replace_terms [] term = term
  | replace_terms (sub :: subs) term = replace_prog sub (replace_terms subs term)


(* ------ TIMER ------ *)

type timer = {real : Timer.real_timer,
	      cpu  : Timer.cpu_timer}

fun startTimer () = {real = Timer.startRealTimer (),
		     cpu  = Timer.startCPUTimer ()}

fun getTime (timer : timer) = Timer.checkRealTimer (#real timer)

fun getMilliTime timer = Time.toMilliseconds (getTime timer)

val time = ref (0 : IntInf.int)


(* ------ STATS ------ *)

fun print_lib_stats {abs, tof} =
    let val n1 = MAP.numItems (!abs)
	val n2 = MAP.numItems (!tof)
    in print ("[library size: "
	      ^ Int.toString n1
	      ^ " abstractions, "
	      ^ Int.toString n2
	      ^ " extracts"
	      ^ "]\n")
    end

fun union_stats_term map1 map2 =
    MAP.unionWith (fn (x, y) => x + y) (map1, map2)

fun get_stats_term (TERM (operator as ((opid, _), _), bterms)) =
    union_stats_term
	(MAP.singleton (opid, 1))
	(get_stats_bterms bterms)
  | get_stats_term (term as AXM_TERM) = MAP.singleton ("axiom",  1)
  | get_stats_term (term as BOT_TERM) = MAP.singleton ("bottom", 1)
  | get_stats_term (term as INT_TERM) = MAP.singleton ("int",    1)
  | get_stats_term (term as VOI_TERM) = MAP.singleton ("void",   1)
  | get_stats_term (term as DUM_TERM) = raise Fail "get_stats_term:DUM_TERM"
  | get_stats_term (term as ATM_TERM _) = MAP.singleton ("atom", 1)
  | get_stats_term (term as TOK_TERM _) = MAP.singleton ("token", 1)
  | get_stats_term (term as NAT_TERM _) = MAP.singleton ("natural_number", 1)
  | get_stats_term (term as VAR_TERM _) = MAP.singleton ("variable", 1)
  | get_stats_term (term as INL_TERM rterm) =
    union_stats_term
	(MAP.singleton ("inl", 1))
	(get_stats_rterm rterm)
  | get_stats_term (term as INR_TERM rterm) =
    union_stats_term
	(MAP.singleton ("inr", 1))
	(get_stats_rterm rterm)
  | get_stats_term (term as FIX_TERM rterm) =
    union_stats_term
	(MAP.singleton ("fix", 1))
	(get_stats_rterm rterm)
  | get_stats_term (term as MIN_TERM rterm) =
    union_stats_term
	(MAP.singleton ("minus", 1))
	(get_stats_rterm rterm)
  | get_stats_term (term as LAM_TERM (_, rterm)) =
    union_stats_term
	(MAP.singleton ("lambda", 1))
	(get_stats_rterm rterm)
  | get_stats_term (term as REC_TERM (_, rterm)) =
    union_stats_term
	(MAP.singleton ("rec", 1))
	(get_stats_rterm rterm)
  | get_stats_term (term as WAI_TERM (rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("wait", 1))
	(union_stats_term
	     (get_stats_rterm rterm1)
	     (get_stats_rterm rterm2))
  | get_stats_term (term as APP_TERM (rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("apply", 1))
	(union_stats_term
	     (get_stats_rterm rterm1)
	     (get_stats_rterm rterm2))
  | get_stats_term (term as PAI_TERM (rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("pair", 1))
	(union_stats_term
	     (get_stats_rterm rterm1)
	     (get_stats_rterm rterm2))
  | get_stats_term (term as ADD_TERM (rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("add", 1))
	(union_stats_term
	     (get_stats_rterm rterm1)
	     (get_stats_rterm rterm2))
  | get_stats_term (term as SUB_TERM (rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("subtract", 1))
	(union_stats_term
	     (get_stats_rterm rterm1)
	     (get_stats_rterm rterm2))
  | get_stats_term (term as MUL_TERM (rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("multiply", 1))
	(union_stats_term
	     (get_stats_rterm rterm1)
	     (get_stats_rterm rterm2))
  | get_stats_term (term as DIV_TERM (rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("divide", 1))
	(union_stats_term
	     (get_stats_rterm rterm1)
	     (get_stats_rterm rterm2))
  | get_stats_term (term as REM_TERM (rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("remainder", 1))
	(union_stats_term
	     (get_stats_rterm rterm1)
	     (get_stats_rterm rterm2))
  | get_stats_term (term as EQT_TERM (rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("eq_term", 1))
	(union_stats_term
	     (get_stats_rterm rterm1)
	     (get_stats_rterm rterm2))
  | get_stats_term (term as UNI_TERM (rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("union", 1))
	(union_stats_term
	     (get_stats_rterm rterm1)
	     (get_stats_rterm rterm2))
  | get_stats_term (term as EQU_TERM (a, rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("equal", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (union_stats_term
		  (get_stats_rterm rterm1)
		  (get_stats_rterm rterm2)))
  | get_stats_term (term as IAX_TERM (a, rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("isaxiom", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (union_stats_term
		  (get_stats_rterm rterm1)
		  (get_stats_rterm rterm2)))
  | get_stats_term (term as IPA_TERM (a, rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("ispair", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (union_stats_term
		  (get_stats_rterm rterm1)
		  (get_stats_rterm rterm2)))
  | get_stats_term (term as IIR_TERM (a, rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("isinr", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (union_stats_term
		  (get_stats_rterm rterm1)
		  (get_stats_rterm rterm2)))
  | get_stats_term (term as IIL_TERM (a, rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("isinl", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (union_stats_term
		  (get_stats_rterm rterm1)
		  (get_stats_rterm rterm2)))
  | get_stats_term (term as IIN_TERM (a, rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("isint", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (union_stats_term
		  (get_stats_rterm rterm1)
		  (get_stats_rterm rterm2)))
  | get_stats_term (term as ILA_TERM (a, rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("islambda", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (union_stats_term
		  (get_stats_rterm rterm1)
		  (get_stats_rterm rterm2)))
  | get_stats_term (term as IAT_TERM (a, rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("isatom2", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (union_stats_term
		  (get_stats_rterm rterm1)
		  (get_stats_rterm rterm2)))
  | get_stats_term (term as CBV_TERM (a, x, f)) =
    union_stats_term
	(MAP.singleton ("callbyvalue", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (get_stats_rterm f))
  | get_stats_term (term as CBA_TERM (a, x, f)) =
    union_stats_term
	(MAP.singleton ("callbyvalueall", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (get_stats_rterm f))
  | get_stats_term (term as FUN_TERM (a, x, f)) =
    union_stats_term
	(MAP.singleton ("function", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (get_stats_rterm f))
  | get_stats_term (term as PRD_TERM (a, x, f)) =
    union_stats_term
	(MAP.singleton ("product", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (get_stats_rterm f))
  | get_stats_term (term as TUN_TERM (a, x, f)) =
    union_stats_term
	(MAP.singleton ("tunion", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (get_stats_rterm f))
  | get_stats_term (term as SET_TERM (a, x, f)) =
    union_stats_term
	(MAP.singleton ("set", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (get_stats_rterm f))
  | get_stats_term (term as LES_TERM (a, b, rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("less", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (union_stats_term
		  (get_stats_rterm b)
		  (union_stats_term
		       (get_stats_rterm rterm1)
		       (get_stats_rterm rterm2))))
  | get_stats_term (term as IEQ_TERM (a, b, rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("int_eq", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (union_stats_term
		  (get_stats_rterm b)
		  (union_stats_term
		       (get_stats_rterm rterm1)
		       (get_stats_rterm rterm2))))
  | get_stats_term (term as SPR_TERM (pair, var1, var2, rterm)) =
    union_stats_term
	(MAP.singleton ("spread", 1))
	(union_stats_term
	     (get_stats_rterm pair)
	     (get_stats_rterm rterm))
  | get_stats_term (term as AEQ_TERM (n, a, b, rterm1, rterm2)) =
    union_stats_term
	(MAP.singleton ("atom_eq", 1))
	(union_stats_term
	     (get_stats_rterm a)
	     (union_stats_term
		  (get_stats_rterm b)
		  (union_stats_term
		       (get_stats_rterm rterm1)
		       (get_stats_rterm rterm2))))
  | get_stats_term (term as DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    union_stats_term
	(MAP.singleton ("decide", 1))
	(union_stats_term
	     (get_stats_rterm dec)
	     (union_stats_term
		  (get_stats_rterm rterm1)
		  (get_stats_rterm rterm2)))
  | get_stats_term (term as IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) =
    union_stats_term
	(MAP.singleton ("ind", 1))
	(union_stats_term
	     (get_stats_rterm i)
	     (union_stats_term
		  (get_stats_rterm downcase)
		  (union_stats_term
		       (get_stats_rterm basecase)
		       (get_stats_rterm upcase))))
  | get_stats_term (CLO_TERM clos) = raise Fail "get_stats_term:CLO_TERM"

and get_stats_bterms bterms =
    foldr (fn (bterm, map) => union_stats_term (get_stats_bterm bterm) map)
	  MAP.empty
	  bterms

and get_stats_bterm (B_TERM (vars, rterm)) = get_stats_rterm rterm

and get_stats_rterm rterm = get_stats_term (rterm2term rterm)

fun stats_term_to_string term =
    MAP.foldri
	(fn (opid, n, str) => opid ^ "(" ^ Int.toString n ^ ")" ^ str)
	""
	(get_stats_term term)


(* -- ... -- *)

fun is_in_lib {abs, tof} id =
    case MAP.find (!abs, id) of
	SOME _ => true
      | NONE => false

fun is_in_lib_tof_sub {abs, tof} id =
    List.exists
	(fn (x,_) => Substring.isSubstring id (Substring.full x))
	(MAP.listItemsi (!tof))

fun print_debug_opid opid str =
    if opid = "pv11_p1_Acceptor-program"
    then print ("[unfolding(" ^ opid ^ "):" ^ str ^ "]\n")
    else ()


(* -- GETTING ALL OPIDS -- *)

fun get_all_opids (lib as {abs, tof}) (term as TERM (operator, bterms)) =
    let val opid = opid_of_term term
	val set  = SET.add (get_all_opids_bterms lib bterms, opid)
    in let val {id, sign, obid, lhs, rhs, wfs} = find_sign abs term
       in SET.union (get_all_opids_rterm lib rhs, set)
       end handle err =>
		  if is_nuprl_term "TERMOF" term
		  then case get_obid_parameters (parameters_of_term term) of
			   SOME obid =>
			   (case MAP.find (!tof, obid) of
				SOME rhs => SET.add (SET.union (get_all_opids lib (get rhs), set), obid)
			      | NONE => raise Fail ("get_all_opids:not_found:" ^ obid))
			 | NONE => raise Fail "get_all_opids:wrong_format"
		  else set (*(print ("get_all_opids:not_an_abs_not_a_termof:" ^ opid);
			raise Fail ("get_all_opids:not_an_abs_not_a_termof:" ^ opid))*)
    end
  | get_all_opids lib (term as AXM_TERM) = SET.empty
  | get_all_opids lib (term as BOT_TERM) = SET.empty
  | get_all_opids lib (term as INT_TERM) = SET.empty
  | get_all_opids lib (term as VOI_TERM) = SET.empty
  | get_all_opids lib (term as DUM_TERM) = SET.empty
  | get_all_opids lib (term as ATM_TERM _) = SET.empty
  | get_all_opids lib (term as TOK_TERM _) = SET.empty
  | get_all_opids lib (term as NAT_TERM _) = SET.empty
  | get_all_opids lib (term as VAR_TERM _) = SET.empty
  | get_all_opids lib (term as INL_TERM rterm) = get_all_opids_rterm lib rterm
  | get_all_opids lib (term as INR_TERM rterm) = get_all_opids_rterm lib rterm
  | get_all_opids lib (term as FIX_TERM rterm) = get_all_opids_rterm lib rterm
  | get_all_opids lib (term as MIN_TERM rterm) = get_all_opids_rterm lib rterm
  | get_all_opids lib (term as LAM_TERM (var, rterm)) = get_all_opids_rterm lib rterm
  | get_all_opids lib (term as REC_TERM (var, rterm)) = get_all_opids_rterm lib rterm
  | get_all_opids lib (term as WAI_TERM (rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib rterm1,
	       get_all_opids_rterm lib rterm2)
  | get_all_opids lib (term as APP_TERM (rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib rterm1,
	       get_all_opids_rterm lib rterm2)
  | get_all_opids lib (term as PAI_TERM (rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib rterm1,
	       get_all_opids_rterm lib rterm2)
  | get_all_opids lib (term as ADD_TERM (rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib rterm1,
	       get_all_opids_rterm lib rterm2)
  | get_all_opids lib (term as SUB_TERM (rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib rterm1,
	       get_all_opids_rterm lib rterm2)
  | get_all_opids lib (term as MUL_TERM (rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib rterm1,
	       get_all_opids_rterm lib rterm2)
  | get_all_opids lib (term as DIV_TERM (rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib rterm1,
	       get_all_opids_rterm lib rterm2)
  | get_all_opids lib (term as REM_TERM (rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib rterm1,
	       get_all_opids_rterm lib rterm2)
  | get_all_opids lib (term as EQT_TERM (rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib rterm1,
	       get_all_opids_rterm lib rterm2)
  | get_all_opids lib (term as UNI_TERM (rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib rterm1,
	       get_all_opids_rterm lib rterm2)
  | get_all_opids lib (term as EQU_TERM (a, rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib a,
	       SET.union (get_all_opids_rterm lib rterm1,
			  get_all_opids_rterm lib rterm2))
  | get_all_opids lib (term as IAX_TERM (a, rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib a,
	       SET.union (get_all_opids_rterm lib rterm1,
			  get_all_opids_rterm lib rterm2))
  | get_all_opids lib (term as IPA_TERM (a, rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib a,
	       SET.union (get_all_opids_rterm lib rterm1,
			  get_all_opids_rterm lib rterm2))
  | get_all_opids lib (term as IIR_TERM (a, rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib a,
	       SET.union (get_all_opids_rterm lib rterm1,
			  get_all_opids_rterm lib rterm2))
  | get_all_opids lib (term as IIL_TERM (a, rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib a,
	       SET.union (get_all_opids_rterm lib rterm1,
			  get_all_opids_rterm lib rterm2))
  | get_all_opids lib (term as IIN_TERM (a, rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib a,
	       SET.union (get_all_opids_rterm lib rterm1,
			  get_all_opids_rterm lib rterm2))
  | get_all_opids lib (term as ILA_TERM (a, rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib a,
	       SET.union (get_all_opids_rterm lib rterm1,
			  get_all_opids_rterm lib rterm2))
  | get_all_opids lib (term as IAT_TERM (a, rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib a,
	       SET.union (get_all_opids_rterm lib rterm1,
			  get_all_opids_rterm lib rterm2))
  | get_all_opids lib (term as CBV_TERM (a, x, f)) =
    SET.union (get_all_opids_rterm lib a,
	       get_all_opids_rterm lib f)
  | get_all_opids lib (term as CBA_TERM (a, x, f)) =
    SET.union (get_all_opids_rterm lib a,
	       get_all_opids_rterm lib f)
  | get_all_opids lib (term as FUN_TERM (a, x, f)) =
    SET.union (get_all_opids_rterm lib a,
	       get_all_opids_rterm lib f)
  | get_all_opids lib (term as PRD_TERM (a, x, f)) =
    SET.union (get_all_opids_rterm lib a,
	       get_all_opids_rterm lib f)
  | get_all_opids lib (term as TUN_TERM (a, x, f)) =
    SET.union (get_all_opids_rterm lib a,
	       get_all_opids_rterm lib f)
  | get_all_opids lib (term as SET_TERM (a, x, f)) =
    SET.union (get_all_opids_rterm lib a,
	       get_all_opids_rterm lib f)
  | get_all_opids lib (term as LES_TERM (a, b, rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib a,
	       SET.union (get_all_opids_rterm lib b,
			  SET.union (get_all_opids_rterm lib rterm1,
				     get_all_opids_rterm lib rterm2)))
  | get_all_opids lib (term as IEQ_TERM (a, b, rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib a,
	       SET.union (get_all_opids_rterm lib b,
			  SET.union (get_all_opids_rterm lib rterm1,
				     get_all_opids_rterm lib rterm2)))
  | get_all_opids lib (term as SPR_TERM (pair, var1, var2, rterm)) =
    SET.union (get_all_opids_rterm lib pair,
	       get_all_opids_rterm lib rterm)
  | get_all_opids lib (term as AEQ_TERM (n, a, b, rterm1, rterm2)) =
    SET.union (get_all_opids_rterm lib a,
	       SET.union (get_all_opids_rterm lib b,
			  SET.union (get_all_opids_rterm lib rterm1,
				     get_all_opids_rterm lib rterm2)))
  | get_all_opids lib (term as DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    SET.union (get_all_opids_rterm lib dec,
	       SET.union (get_all_opids_rterm lib rterm1,
			  get_all_opids_rterm lib rterm2))
  | get_all_opids lib (term as IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) =
    SET.union (get_all_opids_rterm lib i,
	       SET.union (get_all_opids_rterm lib downcase,
			  SET.union (get_all_opids_rterm lib basecase,
				     get_all_opids_rterm lib upcase)))
  | get_all_opids lib (term as CLO_TERM _) = raise Fail "unfold_all:CLO_TERM"

and get_all_opids_rterm lib rterm = get_all_opids lib (rterm2term rterm)

and get_all_opids_bterm lib (B_TERM (vars, rterm)) = get_all_opids_rterm lib rterm

and get_all_opids_bterms lib bterms =
    foldr (fn (bterm, set) => SET.union (set, get_all_opids_bterm lib bterm))
	  SET.empty
	  bterms


(* -- TRIMING OF LIBRARY -- *)

fun trim_lib (lib as {abs, tof}) set = (* set is the set to keep *)
    (abs := MAP.filteri (fn (k, _) => SET.member (set, k)) (!abs);
     tof := MAP.filteri (fn (k, _) => SET.member (set, k)) (!tof))

fun trim_lib_term lib term = trim_lib lib (get_all_opids lib term)

fun trim_lib_terms lib terms =
    let val opids =
	    foldr (fn (term, set) => SET.union (set, get_all_opids lib term))
		  SET.empty
		  terms
    in trim_lib lib opids
    end


(* -- REFRESH TERM -- *)

fun refresh_term term =
    case term of
	VAR_TERM _ => term
      | TOK_TERM _ => term
      | PAI_TERM (rterm1, rterm2) => PAI_TERM (refresh_rterm rterm1, refresh_rterm rterm2)
      | CLO_TERM clos =>
	let val (t,e) = dest_clos clos
	in mk_rct (refresh_term t, refresh_env e)
	end
      | TERM (opr, brterms) => TERM (opr, refresh_brterms brterms)
      | _ => raise Fail ("refresh_term(" ^ opid_of_term term ^ ")")

and refresh_rterm rterm = mk_rterm (refresh_term (rterm2term rterm))

and refresh_brterms brterms =
    map (fn B_TERM (vars, rterm) => B_TERM (vars, refresh_rterm rterm))
	brterms

and refresh_env (ENV m) =
    ENV (MAP.map (fn (v,a,termref) =>
		     let val (t,b) = !termref
		     in (v, a, ref (refresh_term t, b))
		     end)
		 m)


(* -- UNFOLDING -- *)

fun unfold_ab' term lhs rhs =
    let val ((sub_t, is_fo_sub), (ren_p, is_em_ren)) = matching term lhs
    in fo_subst_aux (sub_t, ren_p) rhs
    end

fun unfold_ab term lhs rhs =
    (* once we have unfolded, we try to generate (lighter) internal terms *)
    TERM2term (unfold_ab' term lhs rhs)

fun ct_unfold_ab NONE term lhs rhs = unfold_ab term lhs rhs
  | ct_unfold_ab (SOME env1) term lhs rhs =
    let val ((sub_t, is_fo_sub), (ren_p, is_em_ren)) = matching term lhs
	val rhs = TERM2term rhs
    in if is_fo_sub andalso is_em_ren
       then let val env2 =
		    SUB.foldli (fn (v, FO t, env) =>
				   add2env_one env (v,false,mk_rterm t,env1)
				 | (v, SO ([], t), env) =>
				   add2env_one env (v,false,mk_rterm t,env1)
				 | _ => raise Fail "ct_unfold_ab:closure")
			       em_env
			       sub_t
		val env3 = filter_env rhs env2
	    in mk_rct (rhs, env3)
	    end
       else fo_subst_aux (sub_t, ren_p) rhs
    end

fun unfold_all (lib as {abs, tof}) (term as TERM (operator, bterms)) =
    (let val opid = opid_of_term term
	 (*val _    = print_debug_opid opid "trying to unfold"*)
	 val {id, sign, obid, lhs, rhs, wfs} = find_sign abs term
	 (*handle Fail str => (print ("--error--" ^ str ^ "\n"); raise Fail str)*)
	 (*val _    = print_debug_opid opid "found in lib"*)
	 val t    = unfold_ab term (rterm2term lhs) (rterm2term rhs)
     (*handle Fail str => (print_debug_opid opid ("cannot unfold(" ^ str ^ ")"); raise Fail str)
	    | err => (print_debug_opid opid "cannot unfold"; raise err)*)
     (*val _    = print_debug_opid opid "unfolded"*)
     in unfold_all lib t
     end handle err =>
		if is_nuprl_term "TERMOF" term
		then case get_obid_parameters (parameters_of_term term) of
			 SOME obid =>
			 (case MAP.find (!tof, obid) of
			      SOME rhs => unfold_all lib (get rhs)
			    | NONE => raise Fail ("unfold_all:not_found:" ^ obid))
		       | NONE => raise Fail "unfold_all:wrong_format"
		else TERM (operator, map (unfold_bterm_all lib) bterms))
  | unfold_all lib (term as AXM_TERM) = term
  | unfold_all lib (term as BOT_TERM) = term
  | unfold_all lib (term as INT_TERM) = term
  | unfold_all lib (term as VOI_TERM) = term
  | unfold_all lib (term as DUM_TERM) = term
  | unfold_all lib (term as ATM_TERM _) = term
  | unfold_all lib (term as TOK_TERM _) = term
  | unfold_all lib (term as NAT_TERM _) = term
  | unfold_all lib (term as VAR_TERM _) = term
  | unfold_all lib (term as INL_TERM rterm) = INL_TERM (unfold_all_rterm lib rterm)
  | unfold_all lib (term as INR_TERM rterm) = INR_TERM (unfold_all_rterm lib rterm)
  | unfold_all lib (term as FIX_TERM rterm) = FIX_TERM (unfold_all_rterm lib rterm)
  | unfold_all lib (term as MIN_TERM rterm) = MIN_TERM (unfold_all_rterm lib rterm)
  | unfold_all lib (term as LAM_TERM (var, rterm)) = LAM_TERM (var, unfold_all_rterm lib rterm)
  | unfold_all lib (term as REC_TERM (var, rterm)) = REC_TERM (var, unfold_all_rterm lib rterm)
  | unfold_all lib (term as WAI_TERM (rterm1, rterm2)) =
    WAI_TERM (unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as APP_TERM (rterm1, rterm2)) =
    APP_TERM (unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as PAI_TERM (rterm1, rterm2)) =
    PAI_TERM (unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as ADD_TERM (rterm1, rterm2)) =
    ADD_TERM (unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as SUB_TERM (rterm1, rterm2)) =
    SUB_TERM (unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as MUL_TERM (rterm1, rterm2)) =
    MUL_TERM (unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as DIV_TERM (rterm1, rterm2)) =
    DIV_TERM (unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as REM_TERM (rterm1, rterm2)) =
    REM_TERM (unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as EQT_TERM (rterm1, rterm2)) =
    EQT_TERM (unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as UNI_TERM (rterm1, rterm2)) =
    UNI_TERM (unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as EQU_TERM (a, rterm1, rterm2)) =
    EQU_TERM (unfold_all_rterm lib a,
	      unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as IAX_TERM (a, rterm1, rterm2)) =
    IAX_TERM (unfold_all_rterm lib a,
	      unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as IPA_TERM (a, rterm1, rterm2)) =
    IPA_TERM (unfold_all_rterm lib a,
	      unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as IIR_TERM (a, rterm1, rterm2)) =
    IIR_TERM (unfold_all_rterm lib a,
	      unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as IIL_TERM (a, rterm1, rterm2)) =
    IIL_TERM (unfold_all_rterm lib a,
	      unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as IIN_TERM (a, rterm1, rterm2)) =
    IIN_TERM (unfold_all_rterm lib a,
	      unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as ILA_TERM (a, rterm1, rterm2)) =
    ILA_TERM (unfold_all_rterm lib a,
	      unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as IAT_TERM (a, rterm1, rterm2)) =
    IAT_TERM (unfold_all_rterm lib a,
	      unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as CBV_TERM (a, x, f)) =
    CBV_TERM (unfold_all_rterm lib a,
	      x,
	      unfold_all_rterm lib f)
  | unfold_all lib (term as CBA_TERM (a, x, f)) =
    CBA_TERM (unfold_all_rterm lib a,
	      x,
	      unfold_all_rterm lib f)
  | unfold_all lib (term as FUN_TERM (a, x, f)) =
    FUN_TERM (unfold_all_rterm lib a,
	      x,
	      unfold_all_rterm lib f)
  | unfold_all lib (term as PRD_TERM (a, x, f)) =
    PRD_TERM (unfold_all_rterm lib a,
	      x,
	      unfold_all_rterm lib f)
  | unfold_all lib (term as TUN_TERM (a, x, f)) =
    TUN_TERM (unfold_all_rterm lib a,
	      x,
	      unfold_all_rterm lib f)
  | unfold_all lib (term as SET_TERM (a, x, f)) =
    SET_TERM (unfold_all_rterm lib a,
	      x,
	      unfold_all_rterm lib f)
  | unfold_all lib (term as LES_TERM (a, b, rterm1, rterm2)) =
    LES_TERM (unfold_all_rterm lib a,
	      unfold_all_rterm lib b,
	      unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as IEQ_TERM (a, b, rterm1, rterm2)) =
    IEQ_TERM (unfold_all_rterm lib a,
	      unfold_all_rterm lib b,
	      unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as SPR_TERM (pair, var1, var2, rterm)) =
    SPR_TERM (unfold_all_rterm lib pair,
	      var1,
	      var2,
	      unfold_all_rterm lib rterm)
  | unfold_all lib (term as AEQ_TERM (n, a, b, rterm1, rterm2)) =
    AEQ_TERM (n,
	      unfold_all_rterm lib a,
	      unfold_all_rterm lib b,
	      unfold_all_rterm lib rterm1,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    DEC_TERM (unfold_all_rterm lib dec,
	      var1,
	      unfold_all_rterm lib rterm1,
	      var2,
	      unfold_all_rterm lib rterm2)
  | unfold_all lib (term as IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) =
    IND_TERM (unfold_all_rterm lib i,
	      x,
	      rd,
	      unfold_all_rterm lib downcase,
	      unfold_all_rterm lib basecase,
	      y,
	      ru,
	      unfold_all_rterm lib upcase)
  | unfold_all lib (term as CLO_TERM _) = raise Fail "unfold_all:CLO_TERM"

and unfold_all_rterm lib rterm = mk_rterm (unfold_all lib (rterm2term rterm))

and unfold_bterm_all lib (B_TERM (vars, term)) =
    B_TERM (vars, mk_rterm (unfold_all lib (rterm2term term)))


(* ------ alpha equality ------ *)

fun alpha_equal_bterms_aux ren (vars1, term1) (vars2, term2) =
    length vars1 = length vars2
    andalso
    let val ren' =
	    ListPair.foldr
		(fn (v1,v2,ren) =>
		    insert_ren ren (dest_nuprl_var v1, dest_nuprl_var v2))
		ren
		(vars1,vars2)
    in alpha_equal_terms_aux ren' (rterm2term term1) (rterm2term term2)
    end

and alpha_equal_terms_aux ren term1 term2 =
    (is_nuprl_variable_term term1
     andalso
     is_nuprl_variable_term term2
     andalso
     let val v1 = dest_variable term1
	 val v2 = dest_variable term2
     in apply_ren ren v1 = v2
     end)
    orelse
    (let val (opr1,bterms1) = dest_term term1
	 val (opr2,bterms2) = dest_term term2
     in opr1 = opr2
	andalso
	length bterms1 = length bterms2
	andalso
	ListPair.all
	    (fn (bterm1, bterm2) => alpha_equal_bterms_aux ren bterm1 bterm2)
	    (bterms1, bterms2)
     end)

val alpha_equal_terms = alpha_equal_terms_aux empty_ren

fun alpha_eq_bterms_aux ren (vars1, term1) (vars2, term2) =
    if length vars1 = length vars2
    then let val ren' =
		 ListPair.foldr
		     (fn (v1,v2,ren) =>
			 insert_ren ren (dest_nuprl_var v1, dest_nuprl_var v2))
		     ren
		     (vars1,vars2)
	 in alpha_eq_terms_aux ren' (rterm2term term1) (rterm2term term2)
	 end
    else raise Fail "different lengths of bound variables"

and alpha_eq_terms_aux ren term1 term2 =
    if is_nuprl_variable_term term1
       andalso
       is_nuprl_variable_term term2
    then let val v1 = dest_variable term1
	     val v2 = dest_variable term2
	 in if apply_ren ren v1 = v2
	    then ()
	    else raise Fail "variables are not equal"
	 end
    else let val (opr1 as (op1,params1),bterms1) = dest_term term1
	     val (opr2 as (op2,params2),bterms2) = dest_term term2
	 in if opr1 = opr2
	    then if length bterms1 = length bterms2
		 then ListPair.app
			  (fn (bterm1, bterm2) => alpha_eq_bterms_aux ren bterm1 bterm2)
			  (bterms1, bterms2)
		 else raise Fail "different lengths of bound terms"
	    else raise Fail ("operators are different: " ^ op1 ^ " vs " ^ op2
			     ^ "\n********************************\n"
			     ^ toStringTerm term1
			     ^ "\n********************************\n"
			     ^ toStringTerm term2)
	 end

val alpha_eq_terms = alpha_eq_terms_aux empty_ren


(* ------ PARTIAL EVALUATION & STRICTNESS ANALYSIS ------ *)

fun partial_ev_opt (term as TERM (opr as ((opid, tag), params), bterms)) =
    let val bterms' = partial_ev_opt_bterms bterms
    in if opid = "apply"
	  orelse opid = "lambda"
	  orelse opid = "variable"
	  orelse opid = "pair"
	  orelse opid = "axiom"
	  orelse opid = "bottom"
	  orelse opid = "int"
	  (*orelse opid = "atom"*)
	  orelse opid = "natural_number"
	  orelse opid = "inl"
	  orelse opid = "inr"
	  orelse opid = "fix"
	  orelse opid = "spread"
	  orelse opid = "decide"
	  orelse opid = "int_eq"
       then raise Fail ("partial_ev_opt:" ^ opid ^ "-" ^ toStringTerm term)
       else if opid = "add"
	       orelse opid = "subtract"
	       orelse opid = "multiply"
	       orelse opid = "divide"
	       orelse opid = "remainder"
	       orelse opid = "less"
       then case bterms' of
		[B_TERM ([], n1), B_TERM ([], n2)] =>
		let val n1 = rterm2term n1
		    val n2 = rterm2term n2
		in if is_nuprl_integer_term n1
		      andalso is_nuprl_integer_term n2
		   then if opid = "add"
			   orelse opid = "subtract"
			   orelse opid = "multiply"
			   orelse opid = "divide"
			   orelse opid = "remainder"
			then do_primitive_int_op opid n1 n2
			else if opid = "less"
			then if do_primitive_cmp opid n1 n2
			     then mk_nuprl_btrue_term
			     else mk_nuprl_bfalse_term
			else TERM (opr, bterms')
    		   else TERM (opr, bterms')
		end
	      | _ => TERM (opr, bterms')
       else if opid = "atom_eq"
       then case bterms' of
		[B_TERM ([], a1),
		 B_TERM ([], a2),
		 B_TERM ([], t1),
		 B_TERM ([], t2)] =>
		(let val a1 = rterm2term a1
		     val a2 = rterm2term a2
		     val n  = firstnat term handle _ => 0
		 in if compare_atomn n a1 a2
		    then rterm2term t1
		    else rterm2term t2
		 end handle _ =>  TERM (opr, bterms'))
	      | _ => TERM (opr, bterms')
       else TERM (opr, bterms') (* !! not finished - I should also check the other reductions spread, decide *)
    end
  | partial_ev_opt (term as AXM_TERM) = term
  | partial_ev_opt (term as BOT_TERM) = term
  | partial_ev_opt (term as INT_TERM) = term
  | partial_ev_opt (term as VOI_TERM) = term
  | partial_ev_opt (term as DUM_TERM) = term
  | partial_ev_opt (term as ATM_TERM _) = term
  | partial_ev_opt (term as TOK_TERM _) = term
  | partial_ev_opt (term as NAT_TERM _) = term
  | partial_ev_opt (term as VAR_TERM _) = term
  | partial_ev_opt (term as INL_TERM rterm) = INL_TERM (partial_ev_opt_rterm rterm)
  | partial_ev_opt (term as INR_TERM rterm) = INR_TERM (partial_ev_opt_rterm rterm)
  | partial_ev_opt (term as FIX_TERM rterm) = FIX_TERM (partial_ev_opt_rterm rterm)
  | partial_ev_opt (term as MIN_TERM rterm) = MIN_TERM (partial_ev_opt_rterm rterm)
  | partial_ev_opt (term as LAM_TERM (var, rterm)) = LAM_TERM (var, partial_ev_opt_rterm rterm)
  | partial_ev_opt (term as REC_TERM (var, rterm)) = REC_TERM (var, partial_ev_opt_rterm rterm)
  | partial_ev_opt (term as WAI_TERM (rterm1, rterm2)) = WAI_TERM (partial_ev_opt_rterm rterm1, partial_ev_opt_rterm rterm2)
  | partial_ev_opt (term as APP_TERM (rterm1, rterm2)) =
    let val rterm1' = partial_ev_opt_rterm rterm1
	val rterm2' = partial_ev_opt_rterm rterm2
	val f       = rterm2term rterm1'
	val arg     = rterm2term rterm2'
    in if opid_of_term f = "lambda"
       then let val (x,B) = dest_lambda 0 f
		val v = dest_nuprl_var x
	    in case find_free_vars_map (free_vars_map B) v of
		   0 => B (* x does not occur in B *)
		 | 1 => partial_ev_opt (fo_subst [(x,arg)] B)
		 | _ => if List.length (subterms arg) = 0 (* if the argument has no subterms we might as well do the substitution *)
			then partial_ev_opt (fo_subst [(x,arg)] B)
			else APP_TERM (rterm1', rterm2')
	    end
       else APP_TERM (rterm1', rterm2')
    end
  | partial_ev_opt (term as PAI_TERM (rterm1, rterm2)) = PAI_TERM (partial_ev_opt_rterm rterm1, partial_ev_opt_rterm rterm2)
  | partial_ev_opt (term as ADD_TERM (rterm1, rterm2)) =
    let val n1 = partial_ev_opt_rterm rterm1
	val n2 = partial_ev_opt_rterm rterm2
	val v1 = rterm2term n1
	val v2 = rterm2term n2
    in if is_nuprl_integer_term v1 andalso is_nuprl_integer_term v2
       then do_primitive_int_op "add" v1 v2
       else ADD_TERM (n1, n2)
    end
  | partial_ev_opt (term as SUB_TERM (rterm1, rterm2)) =
    let val n1 = partial_ev_opt_rterm rterm1
	val n2 = partial_ev_opt_rterm rterm2
	val v1 = rterm2term n1
	val v2 = rterm2term n2
    in if is_nuprl_integer_term v1 andalso is_nuprl_integer_term v2
       then do_primitive_int_op "subtract" v1 v2
       else SUB_TERM (n1, n2)
    end
  | partial_ev_opt (term as MUL_TERM (rterm1, rterm2)) =
    let val n1 = partial_ev_opt_rterm rterm1
	val n2 = partial_ev_opt_rterm rterm2
	val v1 = rterm2term n1
	val v2 = rterm2term n2
    in if is_nuprl_integer_term v1 andalso is_nuprl_integer_term v2
       then do_primitive_int_op "multiply" v1 v2
       else MUL_TERM (n1, n2)
    end
  | partial_ev_opt (term as DIV_TERM (rterm1, rterm2)) =
    let val n1 = partial_ev_opt_rterm rterm1
	val n2 = partial_ev_opt_rterm rterm2
	val v1 = rterm2term n1
	val v2 = rterm2term n2
    in if is_nuprl_integer_term v1 andalso is_nuprl_integer_term v2
       then do_primitive_int_op "divide" v1 v2
       else DIV_TERM (n1, n2)
    end
  | partial_ev_opt (term as REM_TERM (rterm1, rterm2)) =
    let val n1 = partial_ev_opt_rterm rterm1
	val n2 = partial_ev_opt_rterm rterm2
	val v1 = rterm2term n1
	val v2 = rterm2term n2
    in if is_nuprl_integer_term v1 andalso is_nuprl_integer_term v2
       then do_primitive_int_op "remainder" v1 v2
       else REM_TERM (n1, n2)
    end
  | partial_ev_opt (term as EQT_TERM (rterm1, rterm2)) =
    EQT_TERM (partial_ev_opt_rterm rterm1,
	      partial_ev_opt_rterm rterm2)
  | partial_ev_opt (term as UNI_TERM (rterm1, rterm2)) =
    UNI_TERM (partial_ev_opt_rterm rterm1,
	      partial_ev_opt_rterm rterm2)
  | partial_ev_opt (term as EQU_TERM (a, rterm1, rterm2)) =
    let val a'      = partial_ev_opt_rterm a
	val rterm1' = partial_ev_opt_rterm rterm1
	val rterm2' = partial_ev_opt_rterm rterm2
    in EQU_TERM (a', rterm1', rterm2')
    end
  | partial_ev_opt (term as IAX_TERM (a, rterm1, rterm2)) =
    let val a'      = partial_ev_opt_rterm a
	val rterm1' = partial_ev_opt_rterm rterm1
	val rterm2' = partial_ev_opt_rterm rterm2
	val a       = rterm2term a'
    in if is_nuprl_axiom_term a
       then rterm2term rterm1'
       else if is_canonical_term a
       then rterm2term rterm2'
       else IAX_TERM (a', rterm1', rterm2')
    end
  | partial_ev_opt (term as IPA_TERM (a, rterm1, rterm2)) =
    let val a'      = partial_ev_opt_rterm a
	val rterm1' = partial_ev_opt_rterm rterm1
	val rterm2' = partial_ev_opt_rterm rterm2
	val a       = rterm2term a'
    in if is_nuprl_pair_term a
       then rterm2term rterm1'
       else if is_canonical_term a
       then rterm2term rterm2'
       else IPA_TERM (a', rterm1', rterm2')
    end
  | partial_ev_opt (term as IIR_TERM (a, rterm1, rterm2)) =
    let val a'      = partial_ev_opt_rterm a
	val rterm1' = partial_ev_opt_rterm rterm1
	val rterm2' = partial_ev_opt_rterm rterm2
	val a       = rterm2term a'
    in if is_nuprl_inr_term a
       then rterm2term rterm1'
       else if is_canonical_term a
       then rterm2term rterm2'
       else IIR_TERM (a', rterm1', rterm2')
    end
  | partial_ev_opt (term as IIL_TERM (a, rterm1, rterm2)) =
    let val a'      = partial_ev_opt_rterm a
	val rterm1' = partial_ev_opt_rterm rterm1
	val rterm2' = partial_ev_opt_rterm rterm2
	val a       = rterm2term a'
    in if is_nuprl_inl_term a
       then rterm2term rterm1'
       else if is_canonical_term a
       then rterm2term rterm2'
       else IIL_TERM (a', rterm1', rterm2')
    end
  | partial_ev_opt (term as IIN_TERM (a, rterm1, rterm2)) =
    let val a'      = partial_ev_opt_rterm a
	val rterm1' = partial_ev_opt_rterm rterm1
	val rterm2' = partial_ev_opt_rterm rterm2
	val a       = rterm2term a'
    in if is_nuprl_integer_term a
       then rterm2term rterm1'
       else if is_canonical_term a
       then rterm2term rterm2'
       else IIN_TERM (a', rterm1', rterm2')
    end
  | partial_ev_opt (term as ILA_TERM (a, rterm1, rterm2)) =
    let val a'      = partial_ev_opt_rterm a
	val rterm1' = partial_ev_opt_rterm rterm1
	val rterm2' = partial_ev_opt_rterm rterm2
	val a       = rterm2term a'
    in if is_nuprl_lambda_term a
       then rterm2term rterm1'
       else if is_canonical_term a
       then rterm2term rterm2'
       else ILA_TERM (a', rterm1', rterm2')
    end
  | partial_ev_opt (term as IAT_TERM (a, rterm1, rterm2)) =
    let val a'      = partial_ev_opt_rterm a
	val rterm1' = partial_ev_opt_rterm rterm1
	val rterm2' = partial_ev_opt_rterm rterm2
    in IAT_TERM (a', rterm1', rterm2')
    end
  | partial_ev_opt (term as CBV_TERM (a, x, f)) =
    let val a' = partial_ev_opt_rterm a
	val f' = partial_ev_opt_rterm f
    in CBV_TERM (a', x, f')
    end
  | partial_ev_opt (term as CBA_TERM (a, x, f)) =
    let val a' = partial_ev_opt_rterm a
	val f' = partial_ev_opt_rterm f
    in CBA_TERM (a', x, f')
    end
  | partial_ev_opt (term as FUN_TERM (a, x, f)) =
    let val a' = partial_ev_opt_rterm a
	val f' = partial_ev_opt_rterm f
    in FUN_TERM (a', x, f')
    end
  | partial_ev_opt (term as PRD_TERM (a, x, f)) =
    let val a' = partial_ev_opt_rterm a
	val f' = partial_ev_opt_rterm f
    in PRD_TERM (a', x, f')
    end
  | partial_ev_opt (term as TUN_TERM (a, x, f)) =
    let val a' = partial_ev_opt_rterm a
	val f' = partial_ev_opt_rterm f
    in TUN_TERM (a', x, f')
    end
  | partial_ev_opt (term as SET_TERM (a, x, f)) =
    let val a' = partial_ev_opt_rterm a
	val f' = partial_ev_opt_rterm f
    in SET_TERM (a', x, f')
    end
  | partial_ev_opt (term as LES_TERM (a, b, rterm1, rterm2)) =
    let val a'      = partial_ev_opt_rterm a
	val b'      = partial_ev_opt_rterm b
	val rterm1' = partial_ev_opt_rterm rterm1
	val rterm2' = partial_ev_opt_rterm rterm2
	val ta      = rterm2term a'
	val tb      = rterm2term b'
    in if is_nuprl_integer_term ta andalso is_nuprl_integer_term tb
       then if do_primitive_cmp "less" ta tb
	    then rterm2term rterm1'
	    else rterm2term rterm2'
       else LES_TERM (a', b', rterm1', rterm2')
    end
  | partial_ev_opt (term as IEQ_TERM (a, b, rterm1, rterm2)) =
    let val a'      = partial_ev_opt_rterm a
	val b'      = partial_ev_opt_rterm b
	val rterm1' = partial_ev_opt_rterm rterm1
	val rterm2' = partial_ev_opt_rterm rterm2
	val ta      = rterm2term a'
	val tb      = rterm2term b'
    in if is_nuprl_integer_term ta andalso is_nuprl_integer_term tb
       then if do_primitive_cmp "int_eq" ta tb
	    then rterm2term rterm1'
	    else rterm2term rterm2'
       else IEQ_TERM (a', b', rterm1', rterm2')
    end
  | partial_ev_opt (term as SPR_TERM (pair, var1, var2, rterm)) =
    let val pair'  = partial_ev_opt_rterm pair
	val rterm' = partial_ev_opt_rterm rterm
	val p      = rterm2term pair'
    in if opid_of_term p = "pair"
       then let val t = rterm2term rterm'
		val m = free_vars_map t
		val (a,b) = dest_pair 7 p
		val v1 = dest_nuprl_var var1
		val v2 = dest_nuprl_var var2
	    in case (find_free_vars_map m v1, find_free_vars_map m v2) of
		   (0, 0) => t
		 | (1, 0) => partial_ev_opt (fo_subst [(var1,a)] t)
		 | (0, 1) => partial_ev_opt (fo_subst [(var2,b)] t)
		 | (_, 0) => if List.length (subterms a) = 0
			     then partial_ev_opt (fo_subst [(var1,a)] t)
			     else APP_TERM (mk_rterm (LAM_TERM (mk_new_nuprl_var v1,rterm')), mk_rterm a)
		 | (0, _) => if List.length (subterms b) = 0
			     then partial_ev_opt (fo_subst [(var2,b)] t)
			     else APP_TERM (mk_rterm (LAM_TERM (mk_new_nuprl_var v2,rterm')), mk_rterm b)
		 | (1, 1) => partial_ev_opt (fo_subst [(var1,a),(var2,b)] t)
		 | (_, _) => if List.length (subterms a) = 0 andalso List.length (subterms b) = 0
			     then partial_ev_opt (fo_subst [(var1,a),(var2,b)] t)
			     else SPR_TERM (pair', var1, var2, rterm')
	    end
       else SPR_TERM (pair', var1, var2, rterm')
    end
  | partial_ev_opt (term as AEQ_TERM (n, a, b, rterm1, rterm2)) =
    let val a'      = partial_ev_opt_rterm a
	val b'      = partial_ev_opt_rterm b
	val rterm1' = partial_ev_opt_rterm rterm1
	val rterm2' = partial_ev_opt_rterm rterm2
	val ta      = rterm2term a'
	val tb      = rterm2term b'
    in (if compare_atomn n ta tb
	then rterm2term rterm1'
	else rterm2term rterm2')
       handle _ => AEQ_TERM (n, a', b', rterm1', rterm2')
    end
  | partial_ev_opt (term as DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    let val dec'    = partial_ev_opt_rterm dec
	val rterm1' = partial_ev_opt_rterm rterm1
	val rterm2' = partial_ev_opt_rterm rterm2
	val d       = rterm2term dec'
	val opid    = opid_of_term d
	val b_inl   = opid = "inl"
	val b_inr   = opid = "inr"
    in if b_inl orelse b_inr
       then let val (x,nv,rt) =
		    if b_inl
		    then (dest_inl d, var1, rterm1')
		    else (dest_inr d, var2, rterm2')
		val t = rterm2term rt
		val v = dest_nuprl_var nv
	    in case find_free_vars_map (free_vars_map t) v of
		   0 => t
		 | 1 => partial_ev_opt (fo_subst [(nv,x)] t)
		 | _ => if List.length (subterms x) = 0
			then partial_ev_opt (fo_subst [(nv,x)] t)
			else APP_TERM (mk_rterm (LAM_TERM (mk_new_nuprl_var v,rt)), mk_rterm x)
	    end
       else DEC_TERM (dec', var1, rterm1', var2, rterm2')
    end
  | partial_ev_opt (term as IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) =
    let val i'        = partial_ev_opt_rterm i
	val downcase' = partial_ev_opt_rterm downcase
	val basecase' = partial_ev_opt_rterm basecase
	val upcase'   = partial_ev_opt_rterm upcase
    in IND_TERM (i', x, rd, downcase', basecase', y, ru, upcase')
    end
  | partial_ev_opt (term as CLO_TERM clos) =
    let val (clos' as (rterm, env)) = partial_ev_opt_clos clos
    in if is_em_env env
       then rterm2term rterm
       else CLO_TERM clos'
    end

and partial_ev_opt_clos (rterm, env) =
    let val rterm' = partial_ev_opt_rterm rterm
	val (ENV env') = partial_ev_opt_env env
	val term = rterm2term rterm'
	val vars = free_vars_map term
	val (sub, m) =
	    MAP.foldri
		(fn (v, x as (_,_,termref), (sub, m)) =>
		    case MAP.find (vars, v) of
			SOME n =>
			if n = 0
			then (sub, m)
			else if n = 1
			then ((mk_new_nuprl_var v, #1 (!termref)) :: sub, m)
			else (sub, MAP.insert (m, v, x))
		      | NONE => (sub, m))
		([], MAP.empty) (* we reconstruct the term (a substitution) and environment *)
		env'
	val term' = if List.null sub then term else fo_subst sub term
    in (mk_rterm term', ENV m)
    end

and partial_ev_opt_env (ENV m) =
    let val _ =
	    MAP.app
		(fn (_,_,termref) =>
		    let val (t,b) = !termref
			val term = partial_ev_opt t
		    in termref := (term, b)
		    end)
		m
    in ENV m
    end

and partial_ev_opt_rterm rterm = mk_rterm (partial_ev_opt (rterm2term rterm))

and partial_ev_opt_bterms bterms = map partial_ev_opt_bterm bterms

and partial_ev_opt_bterm (B_TERM (vars, rterm)) =
    B_TERM (vars, partial_ev_opt_rterm rterm)


(* +TEMP++++++++++++++++++++++++++++++ *)
(*fun partial_ev_opt term = term*)
(* +++++++++++++++++++++++++++++++ *)



(* ------ NUPRL TO EventML ------ *)

datatype alpha = NEXT of string * (unit -> alpha)

val lstAlpha =
    ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
     "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

fun streamId [] pref () = streamId lstAlpha (pref ^ "a") ()
  | streamId (x :: xs) pref () =
    let val f = streamId xs pref
    in NEXT (pref ^ x, f)
    end

fun is_list (TERM ((("cons", _), []), [B_TERM ([], x), B_TERM ([], xs)])) =
    Option.map (fn lst => x :: lst) (is_ref_list xs)
  | is_list (TERM ((("nil", _), []), [])) = SOME []
  | is_list _ = NONE

and is_ref_list rterm = is_list (rterm2term rterm)

fun isAtomic t =
    let val opid = opid_of_term t
	val lst  = ["variable", "int", "pair", "natural_number", "prop", "universe"]
    in Option.isSome (List.find (fn x => x = opid) lst)
    end

fun printUkn t n = "-"
    (*(print (toStringTerm t ^ "\n"); "---" ^ Int.toString n)*)

fun nuprl2eml_term sub (TERM ((("apply", _), []), [B_TERM ([], f), B_TERM ([], a)])) =
    nuprl2eml_ref_atm sub f ^ " " ^ nuprl2eml_ref_atm sub a
  | nuprl2eml_term sub (TERM ((("cons", _), []), [B_TERM ([], x), B_TERM ([], xs)])) =
    (case is_ref_list xs of
	 SOME lst => ListFormat.fmt {final = "]", fmt = nuprl2eml_ref_term sub, init = "[", sep = "; "} (x :: lst)
       | NONE => nuprl2eml_ref_term sub x ^ " . " ^ nuprl2eml_ref_term sub xs)
  | nuprl2eml_term sub (TERM ((("subtract", _), []), [B_TERM ([], x), B_TERM ([], y)])) =
    nuprl2eml_ref_term sub x ^ " - " ^ nuprl2eml_ref_term sub y
  | nuprl2eml_term sub (TERM ((("add", _), []), [B_TERM ([], x), B_TERM ([], y)])) =
    nuprl2eml_ref_term sub x ^ " + " ^ nuprl2eml_ref_term sub y
  | nuprl2eml_term sub (TERM ((("pair", _), []), [B_TERM ([], x), B_TERM ([], y)])) =
    "(" ^ nuprl2eml_ref_term sub x ^ "," ^ nuprl2eml_ref_term sub y ^ ")"
  | nuprl2eml_term sub (TERM ((("variable", _), [(v,vkind)]), [])) =
    (case SUB.find (sub, v) of
	 SOME t => t
       | NONE => v)
  | nuprl2eml_term sub (TERM ((("natural_number", _), [(n,nkind)]), [])) = n
  | nuprl2eml_term sub (TERM ((("token", _), [(t,tkind)]), [])) = "`" ^ t ^ "`"
  | nuprl2eml_term sub (term as TERM ((("lambda", _), []), [B_TERM ([x], f)])) =
    printUkn term 0 (*"\\" ^ x ^ ". " ^ nuprl2eml_term sub f*)
  | nuprl2eml_term sub (TERM ((("inr", _), []), [B_TERM ([], t)])) =
    if is_nuprl_ref_term "axiom" t
    then "false" (* NOTE: Arghh, this can be bad because inr(axiom) is not actually equal to false in EML. *)
    else "inr(" ^ nuprl2eml_ref_term sub t ^ ")"
  | nuprl2eml_term sub (TERM ((("inl", _), []), [B_TERM ([], t)])) =
    if is_nuprl_ref_term "axiom" t
    then "true"
    else "inl(" ^ nuprl2eml_ref_term sub t ^ ")"
  | nuprl2eml_term sub (TERM ((("minus", _), []), [B_TERM ([], t)])) =
    "~" ^ nuprl2eml_ref_term sub t
  | nuprl2eml_term sub (TERM ((("it", _), []), [])) = "it"
  | nuprl2eml_term sub (TERM ((("int", _), []), [])) = "Int"
  | nuprl2eml_term sub (TERM ((("bool", _), []), [])) = "Bool"
  | nuprl2eml_term sub (TERM ((("real", _), []), [])) = "Real"
  | nuprl2eml_term sub (TERM ((("atom", _), []), [])) = "Atom"
  | nuprl2eml_term sub (TERM ((("universe", _), params), [])) = "Type"
  | nuprl2eml_term sub (TERM ((("prop", _), params), [])) = "Prop"
  | nuprl2eml_term sub (TERM ((("list", _), []), [B_TERM ([], t)])) =
    nuprl2eml_ref_term sub t ^ " List"
  (* NOTE: This is just a crude hack. For class we should check that the level expression
   * is 'correct', that the Info is a Msg and that es and e don't occur in t. *)
  | nuprl2eml_term sub (TERM ((("eclass", _), [lvl_exp]), [B_TERM ([], info), B_TERM ([es, e], t)])) =
    nuprl2eml_ref_term sub t ^ " Class"
  | nuprl2eml_term sub (TERM ((("product", _), []), [B_TERM ([], t1), B_TERM ([v], t2)])) =
    if is_null_nuprl_var v
    then nuprl2eml_ref_atm sub t1 ^ " * " ^ nuprl2eml_ref_atm sub t2
    else raise Fail "nuprl2eml_term:product"
  | nuprl2eml_term sub (TERM ((("union", _), []), [B_TERM ([], t1), B_TERM ([], t2)])) =
    nuprl2eml_ref_atm sub t1 ^ " + " ^ nuprl2eml_ref_atm sub t2
  | nuprl2eml_term sub (TERM ((("function", _), []), [B_TERM ([], t1), B_TERM ([v], t2)])) =
    if is_null_nuprl_var v
    then nuprl2eml_ref_atm sub t1 ^ " -> " ^ nuprl2eml_ref_term sub t2
    else raise Fail "nuprl2eml_term:function"
  | nuprl2eml_term sub (TERM ((("nil", _), []), [])) = "[]"
  | nuprl2eml_term sub (TERM ((("make-Msg", _), []), [B_TERM ([], header), B_TERM ([], typ), B_TERM ([], body)])) =
    "(" ^ nuprl2eml_ref_term sub header ^ "," ^ nuprl2eml_ref_term sub typ ^ "," ^ nuprl2eml_ref_term sub body ^ ")"
  | nuprl2eml_term sub (VAR_TERM var) =
    let val v = dest_nuprl_var var
    in case SUB.find (sub, v) of
	   SOME t => t
	 | NONE => v
    end
  | nuprl2eml_term sub (term as LAM_TERM _) = printUkn term 0
  | nuprl2eml_term sub (term as REC_TERM _) = printUkn term 0
  | nuprl2eml_term sub (WAI_TERM (t, e)) = "wait(" ^ nuprl2eml_ref_atm sub t ^ ", " ^ nuprl2eml_ref_atm sub e ^ ")"
  | nuprl2eml_term sub (APP_TERM (f, a)) = nuprl2eml_ref_atm sub f ^ " " ^ nuprl2eml_ref_atm sub a
  | nuprl2eml_term sub (PAI_TERM (x, y)) = "(" ^ nuprl2eml_ref_term sub x ^ "," ^ nuprl2eml_ref_term sub y ^ ")"
  | nuprl2eml_term sub (term as AXM_TERM) = printUkn term 0 (*raise Fail "nuprl2eml_term:AXM_TERM"*)
  | nuprl2eml_term sub (term as BOT_TERM) = printUkn term 0 (*raise Fail "nuprl2eml_term:BOT_TERM"*)
  | nuprl2eml_term sub INT_TERM = "Int"
  | nuprl2eml_term sub VOI_TERM = raise Fail "nuprl2eml_term:VOI_TERM"
  | nuprl2eml_term sub DUM_TERM = raise Fail "nuprl2eml_term:DUM_TERM"
  | nuprl2eml_term sub (ATM_TERM NONE) = "Atom"
  | nuprl2eml_term sub (ATM_TERM (SOME 2)) = "Loc"
  | nuprl2eml_term sub (term as ATM_TERM _) = printUkn term 0
  | nuprl2eml_term sub (TOK_TERM (t,k)) =  "\"" ^ t ^ "\""
  | nuprl2eml_term sub (NAT_TERM n) = II.toString n
  | nuprl2eml_term sub (INL_TERM t) =
    if is_nuprl_ref_term "axiom" t
    then "true"
    else "inl(" ^ nuprl2eml_ref_term sub t ^ ")"
  | nuprl2eml_term sub (INR_TERM t) =
    if is_nuprl_ref_term "axiom" t
    then "false" (* NOTE: Arghh, this can be bad because inr(axiom) is not actually equal to false in EML. *)
    else "inr(" ^ nuprl2eml_ref_term sub t ^ ")"
  | nuprl2eml_term sub (FIX_TERM t) = "fix(" ^ nuprl2eml_ref_term sub t ^ ")"
  | nuprl2eml_term sub (term as SPR_TERM _) = printUkn term 0
  | nuprl2eml_term sub (term as DEC_TERM _) = printUkn term 0
  | nuprl2eml_term sub (term as IAX_TERM _) = printUkn term 0
  | nuprl2eml_term sub (term as IPA_TERM _) = printUkn term 0
  | nuprl2eml_term sub (term as IEQ_TERM _) = printUkn term 0
  | nuprl2eml_term sub (term as AEQ_TERM _) = printUkn term 0
  | nuprl2eml_term sub (term as CBA_TERM _) = printUkn term 0
  (*| nuprl2eml_term sub (term as LIN_TERM _) = printUkn term 0*)
  | nuprl2eml_term sub term = (*printUkn term 1*)
    let val ((opid, params), bterms) = dest_term term
    in if List.null bterms andalso List.null params
       then opid
       else printUkn term 1
    end
    (*let val ((opid, params), bterms) = dest_term term
    in if List.all (fn (vars, t) => List.null vars) bterms
       then foldl (fn ((vars, t), str) => str ^ " " ^ nuprl2eml' t)
		  opid
		  bterms
       else toStringTerm term
    end*)

and nuprl2eml_ref_term sub rterm = nuprl2eml_term sub (rterm2term rterm)

and nuprl2eml_atm sub t =
    let val str = nuprl2eml_term sub t
    in if isAtomic t then str else "(" ^ str ^ ")"
    end

and nuprl2eml_ref_atm sub rterm = nuprl2eml_atm sub (rterm2term rterm)

fun nuprl2eml_isect sub stream (term as TERM ((("isect", _), []),
					      [B_TERM ([],  t1),
					       B_TERM ([nv], t2)])) =
    let val v = dest_nuprl_var nv
    in if is_nuprl_ref_term "universe" t1
       then if Option.isSome (SUB.find (sub, v))
	    then nuprl2eml_ref_isect sub stream t2
	    else let val NEXT (tv, str) = stream ()
		     val sub' = SUB.insert (sub, v, tv)
		 in nuprl2eml_ref_isect sub' str t2
		 end
       else printUkn term 9
    end
  | nuprl2eml_isect sub _ term = nuprl2eml_term sub term

and nuprl2eml_ref_isect sub stream rterm =
    nuprl2eml_isect sub stream (rterm2term rterm)

fun nuprl2eml_all id sub stream (term as TERM ((("all", _), []),
					       [B_TERM ([],  t1),
						B_TERM ([nv], t2)])) =
    let val v = dest_nuprl_var nv
    in if is_nuprl_ref_term "universe" t1
       then if Option.isSome (SUB.find (sub, v))
	    then nuprl2eml_ref_all id sub stream t2
	    else let val NEXT (tv, str) = stream ()
		     val sub' = SUB.insert (sub, v, tv)
		 in nuprl2eml_ref_all id sub' str t2
		 end
       else printUkn term 2
    end
  | nuprl2eml_all id sub stream (term as TERM ((("uall", _), []),
					       [B_TERM ([],  t1),
						B_TERM ([nv], t2)])) =
    let val v = dest_nuprl_var nv
    in if is_nuprl_ref_term "universe" t1
       then if Option.isSome (SUB.find (sub, v))
	    then nuprl2eml_ref_all id sub stream t2
	    else let val NEXT (tv, str) = stream ()
		     val sub' = SUB.insert (sub, v, tv)
		 in nuprl2eml_ref_all id sub' str t2
		 end
       else printUkn term 3
    end
  | nuprl2eml_all id sub stream (term as TERM ((("member", _), []),
					       [B_TERM ([], t1),
						B_TERM ([], t2)])) =
    if is_nuprl_ref_term id t2
    then nuprl2eml_ref_isect sub stream t1
    else printUkn term 4
  | nuprl2eml_all id sub stream term = printUkn term 5

and nuprl2eml_ref_all id sub stream rterm =
    nuprl2eml_all id sub stream (rterm2term rterm)

fun nuprl2eml_wf id (TERM ((("!theorem", _), [name]),
			   [B_TERM ([], t)])) =
    nuprl2eml_ref_all id
		      (SUB.empty : string SUB.map)
		      (streamId lstAlpha "'")
		      t
  | nuprl2eml_wf id term = printUkn term 6

fun nuprl2eml_abs id (term as TERM ((("!abstraction", _), []),
				    [B_TERM ([], cond),
				     B_TERM ([], t1),
				     B_TERM ([], t2)])) =
     if is_nuprl_ref_term id t1
     then nuprl2eml_ref_term SUB.empty t2
     else printUkn term 7
   | nuprl2eml_abs id term = printUkn term 8

fun nuprlTerm2eml term = nuprl2eml_term (SUB.empty : string SUB.map) term

(* nuprl2eml_wf "it" *)


(* ------ Nuprl to Haskell ------ *)
fun nuprl2haskell_var var =
    "hs_" ^ (String.map (fn #"%" => #"p" | c => c) (dest_nuprl_var var)) ^ "_var"

fun nuprl2haskell_term ind sub term =
    case term of
	TERM ((("apply", _), []), [B_TERM ([], f), B_TERM ([], a)]) =>
	if is_nuprl_lambda_term (rterm2term f)
	then let val (var, b) = dest_lambda 0 (rterm2term f)
	     in "let "
		^ nuprl2haskell_var var
		^ " = "
		^ nuprl2haskell_ref_term ind sub a
		^ " in\n"
		^ nuprl2haskell_term ind sub b
	     end
	else nuprl2haskell_ref_atm ind sub f ^ " " ^ nuprl2haskell_ref_atm ind sub a
      | TERM ((("cons", _), []), [B_TERM ([], x), B_TERM ([], xs)]) =>
	(case is_ref_list xs of
	     SOME lst => T.fmt {init = "[", sep = ", ", final = "]", fmt = nuprl2haskell_ref_term ind sub} (x :: lst)
	   | NONE => nuprl2haskell_ref_term ind sub x ^ " : " ^ nuprl2haskell_ref_term ind sub xs)
      | TERM ((("subtract", _), []), [B_TERM ([], x), B_TERM ([], y)]) =>
	nuprl2haskell_ref_term ind sub x ^ " - " ^ nuprl2haskell_ref_term ind sub y
      | TERM ((("add", _), []), [B_TERM ([], x), B_TERM ([], y)]) =>
	nuprl2haskell_ref_term ind sub x ^ " + " ^ nuprl2haskell_ref_term ind sub y
      | TERM ((("divide", _), []), [B_TERM ([], x), B_TERM ([], y)]) =>
	"div " ^ nuprl2haskell_ref_term ind sub x ^ " " ^ nuprl2haskell_ref_term ind sub y
      | TERM ((("pair", _), []), [B_TERM ([], x), B_TERM ([], y)]) =>
	"(" ^ nuprl2haskell_ref_term ind sub x ^ "," ^ nuprl2haskell_ref_term ind sub y ^ ")"
      | TERM ((("variable", _), [(v,vkind)]), []) =>
	(case SUB.find (sub, v) of
	     SOME v => v
	   | NONE => nuprl2haskell_var (mk_new_nuprl_var v))
      | TERM ((("natural_number", _), [(n,nkind)]), []) => n
      | TERM ((("token", _), [(t,tkind)]), []) => "\"" ^ t ^ "\""
      | TERM ((("lambda", _), []), [B_TERM ([x], f)]) =>
	let val (vars, b) = dest_lambdas 0 (rterm2term f)
	in "\\"
	   ^ T.fmt {init = "", sep = " ", final = "", fmt = nuprl2haskell_var} (x :: vars)
	   ^ " ->\n" ^ nuprl2haskell_term ind sub b
	end
      | TERM ((("inr", _), []), [B_TERM ([], t)]) =>
	if is_nuprl_ref_term "axiom" t
	then "False" (* NOTE: Arghh, this can be bad because inr(axiom) is not actually equal to false in EML. *)
	else "Inr(" ^ nuprl2haskell_ref_term ind sub t ^ ")"
      | TERM ((("inl", _), []), [B_TERM ([], t)]) =>
	if is_nuprl_ref_term "axiom" t
	then "True"
	else "Inl(" ^ nuprl2haskell_ref_term ind sub t ^ ")"
      | TERM ((("minus", _), []), [B_TERM ([], t)]) =>
	"-" ^ nuprl2haskell_ref_term ind sub t
      | TERM ((("it", _),   []), []) => "()"
      | TERM ((("int", _),  []), []) => "NInt"
      | TERM ((("bool", _), []), []) => "NBool"
      | TERM ((("real", _), []), []) => "NReal"
      | TERM ((("atom", _), []), []) => "NAtom"
      | TERM ((("atom", _), [(n,nkind)]), []) =>
	(case n of
	     "2" => "NLoc"
	   | "1" => "NAtom"
	   | _   => raise Fail ("nuprl2haskell:atom(" ^ n ^ ")"))
      | TERM ((("universe", _), params), []) => "NType"
      | TERM ((("prop", _), params), []) => "NProp"
      | TERM ((("list", _), []), [B_TERM ([], t)]) =>
	"NHiddenType" (*"NList(" ^ nuprl2haskell_ref_term ind sub t ^ ")"*)
      | TERM ((("eclass", _), [lvl_exp]), [B_TERM ([], info), B_TERM ([es, e], t)]) =>
	"NHiddenType" (*"NClass(" ^ nuprl2haskell_ref_term ind sub t ^ ")"*)
      | TERM ((("union", _), []), [B_TERM ([], t1), B_TERM ([], t2)]) =>
	"NHiddenType" (*"NUnion (" ^ nuprl2haskell_ref_atm ind sub t1 ^ ") (" ^ nuprl2haskell_ref_atm ind sub t2 ^ ")"*)
      | TERM ((("product", _), []), [B_TERM ([], t1), B_TERM ([v], t2)]) =>
	"NHiddenType" (*"NProd " ^ nuprl2haskell_var v ^ " (" ^ nuprl2haskell_ref_atm ind sub t1 ^ ") (" ^ nuprl2haskell_ref_atm ind sub t2 ^ ")"*)
      | TERM ((("function", _), []), [B_TERM ([], t1), B_TERM ([v], t2)]) =>
	"NHiddenType" (*"NFun " ^ nuprl2haskell_var v ^ " (" ^ nuprl2haskell_ref_atm ind sub t1 ^ ") (" ^ nuprl2haskell_ref_term ind sub t2 ^ ")"*)
      | TERM ((("set", _), []), [B_TERM ([], t1), B_TERM ([v], t2)]) =>
	"NHiddenType" (*"NSet (" ^ nuprl2haskell_ref_atm ind sub t1 ^ ")" ^ nuprl2haskell_var v ^ " (" ^ nuprl2haskell_ref_term ind sub t2 ^ ")"*)
      | TERM ((("quotient", _), []), [B_TERM ([], t1), B_TERM ([v1,v2], f)]) =>
	"NQuot" (* To finish!!!!!!! *)
      | TERM ((("equal", _), []), [B_TERM ([], typ), B_TERM ([], t1), B_TERM ([], t2)]) =>
	"NEq" (* To finish!!!!!!! *)
      | TERM ((("valueall-type", _), []), [B_TERM ([], typ)]) =>
	"NVAType" (* To finish!!!!!!! *)
      | TERM ((("nil", _), []), []) => "[]"
      | TERM ((("make-Msg", _), []), [B_TERM ([], header), B_TERM ([], typ), B_TERM ([], body)]) =>
	"(" ^ nuprl2haskell_ref_term ind sub header ^ "," ^ nuprl2haskell_ref_term ind sub typ ^ "," ^ nuprl2haskell_ref_term ind sub body ^ ")"
      | TERM ((("atom_eq", _), params), [B_TERM ([], t1), B_TERM ([], t2), B_TERM ([], t3), B_TERM ([], t4)]) =>
	"if "
	^ nuprl2haskell_ref_term ind sub t1
	^ " = "
	^ nuprl2haskell_ref_term ind sub t2
	^ " then "
	^ nuprl2haskell_ref_term ind sub t3
	^ " else "
	^ nuprl2haskell_ref_term ind sub t4
      | TERM ((("eq_term", _), params), [B_TERM ([], t1), B_TERM ([], t2)]) =>
	nuprl2haskell_ref_term ind sub t1 ^ " = " ^ nuprl2haskell_ref_term ind sub t2
      | TERM ((("less", _), []), [B_TERM ([], t1), B_TERM ([], t2), B_TERM ([], t3), B_TERM ([], t4)]) =>
	"if "
	^ nuprl2haskell_ref_term ind sub t1
	^ " < "
	^ nuprl2haskell_ref_term ind sub t2
	^ " then "
	^ nuprl2haskell_ref_term ind sub t3
	^ " else "
	^ nuprl2haskell_ref_term ind sub t4
      | TERM ((("int_eq", _), []), [B_TERM ([], t1), B_TERM ([], t2), B_TERM ([], t3), B_TERM ([], t4)]) =>
	"if "
	^ nuprl2haskell_ref_term ind sub t1
	^ " = "
	^ nuprl2haskell_ref_term ind sub t2
	^ " then "
	^ nuprl2haskell_ref_term ind sub t3
	^ " else "
	^ nuprl2haskell_ref_term ind sub t4
      | TERM ((("callbyvalueall", _), []), [B_TERM ([], arg), B_TERM ([x], B)]) =>
	"(\\" ^ nuprl2haskell_var x ^ " -> " ^ nuprl2haskell_ref_term ind sub B ^ ") $! " ^ nuprl2haskell_ref_term ind sub arg
      | TERM ((("callbyvalue", _), []), [B_TERM ([], arg), B_TERM ([x], B)]) =>
	"(\\" ^ nuprl2haskell_var x ^ " -> " ^ nuprl2haskell_ref_term ind sub B ^ ") $! " ^ nuprl2haskell_ref_term ind sub arg
      | TERM ((("list_ind", _), []), [B_TERM ([], L), B_TERM ([], nilcase), B_TERM ([x, xs, r], conscase)]) =>
	let val f   = "f"
	    val lst = "L"
	in "let " ^ f ^ " " ^ lst ^ " = case " ^ lst ^ " of"
	   ^ "\n"
	   ^ "  [] -> " ^ nuprl2haskell_ref_term ind sub nilcase
	   ^ "\n"
	   ^ "  " ^ nuprl2haskell_var x ^ " : " ^ nuprl2haskell_var xs ^ " -> (let " ^ nuprl2haskell_var r ^ " = " ^ f ^ " " ^ nuprl2haskell_var xs ^ " in " ^ nuprl2haskell_ref_term ind sub conscase ^ ")"
	   ^ "\n"
	   ^ "in " ^ f ^ " " ^ nuprl2haskell_ref_term ind sub L
	end
      | TERM ((("axiom", _), []), []) => "()"
      | AXM_TERM => raise Fail "nuprl2haskell_term:AXM_TERM"
      | BOT_TERM => raise Fail "nuprl2haskell_term:BOT_TERM"
      | INT_TERM => raise Fail "nuprl2haskell_term:INT_TERM"
      | VOI_TERM => raise Fail "nuprl2haskell_term:VOI_TERM"
      | DUM_TERM => raise Fail "nuprl2haskell_term:DUM_TERM"
      | ATM_TERM _ => raise Fail "nuprl2haskell_term:ATM_TERM"
      | TOK_TERM _ => raise Fail "nuprl2haskell_term:TOK_TERM"
      | NAT_TERM n => II.toString n
      | VAR_TERM var =>
	let val v = dest_nuprl_var var
	in case SUB.find (sub, v) of
	       SOME v => v
	     | NONE => nuprl2haskell_var var
	end
      | INL_TERM t =>
	if is_nuprl_ref_term "axiom" t
	then "True"
	else "Inl(" ^ nuprl2haskell_ref_term ind sub t ^ ")"
      | INR_TERM t =>
	if is_nuprl_ref_term "axiom" t
	then "False" (* NOTE: Arghh, this can be bad because inr(axiom) is not actually equal to false in EML. *)
	else "Inr(" ^ nuprl2haskell_ref_term ind sub t ^ ")"
      | FIX_TERM _ => raise Fail "to_haskell:FIX_TERM"
      | MIN_TERM _ => raise Fail "to_haskell:MIN_TERM"
      | LAM_TERM (x, f) =>
	let val (vars, b) = dest_lambdas 1 (rterm2term f)
	in "\\"
	   ^ T.fmt {init  = "",
		    sep   = " ",
		    final = "",
		    fmt   = fn nv => nuprl2haskell_var nv}
		   (x :: vars)
	   ^ " ->\n" ^ nuprl2haskell_term ind sub b
	end
      | REC_TERM _ => raise Fail "to_haskell:REC_TERM"
      | WAI_TERM (t, e) => raise Fail "nuprl2haskell_term:WAI_TERM"
      | APP_TERM (f, a) =>
	if is_nuprl_lambda_term (rterm2term f)
	then let val (var, b) = dest_lambda 0 (rterm2term f)
	     in "let "
		^ nuprl2haskell_var var
		^ " = "
		^ nuprl2haskell_ref_term ind sub a
		^ " in\n"
		^ nuprl2haskell_term ind sub b
	     end
	else nuprl2haskell_ref_atm ind sub f ^ " " ^ nuprl2haskell_ref_atm ind sub a
      | PAI_TERM (x, y) => "(" ^ nuprl2haskell_ref_term ind sub x ^ "," ^ nuprl2haskell_ref_term ind sub y ^ ")"
      | ADD_TERM _ => raise Fail "to_haskell:ADD_TERM"
      | SUB_TERM _ => raise Fail "to_haskell:SUB_TERM"
      | MUL_TERM _ => raise Fail "to_haskell:MUL_TERM"
      | DIV_TERM _ => raise Fail "to_haskell:DIV_TERM"
      | REM_TERM _ => raise Fail "to_haskell:REM_TERM"
      | EQT_TERM _ => raise Fail "to_haskell:EQT_TERM"
      | UNI_TERM _ => raise Fail "to_haskell:UNI_TERM"
      | EQU_TERM (a, rterm1, rterm2) => raise Fail "nuprl2haskell:EQU_TERM"
      | IAX_TERM (a, rterm1, rterm2) => raise Fail "nuprl2haskell:IAX_TERM"
      | IPA_TERM (a, rterm1, rterm2) => raise Fail "nuprl2haskell:IPA_TERM"
      | IIR_TERM (a, rterm1, rterm2) => raise Fail "nuprl2haskell:IIR_TERM"
      | IIL_TERM (a, rterm1, rterm2) => raise Fail "nuprl2haskell:IIL_TERM"
      | IIN_TERM (a, rterm1, rterm2) => raise Fail "nuprl2haskell:IIN_TERM"
      | ILA_TERM (a, rterm1, rterm2) => raise Fail "nuprl2haskell:ILA_TERM"
      | IAT_TERM (a, rterm1, rterm2) => raise Fail "nuprl2haskell:IAT_TERM"
      | CBV_TERM (arg, x, B) => raise Fail ("nuprl2haskell:CBV_TERM")
      | CBA_TERM (arg, x, B) => raise Fail ("nuprl2haskell:CBA_TERM")
      | FUN_TERM (arg, x, B) => raise Fail ("nuprl2haskell:FUN_TERM")
      | PRD_TERM (arg, x, B) => raise Fail ("nuprl2haskell:PRD_TERM")
      | TUN_TERM (arg, x, B) => raise Fail ("nuprl2haskell:TUN_TERM")
      | SET_TERM (arg, x, B) => raise Fail ("nuprl2haskell:SET_TERM")
      | LES_TERM (a, b, rterm1, rterm2) => raise Fail ("nuprl2haskell:LES_TERM")
      | IEQ_TERM (a, b, rterm1, rterm2) => raise Fail ("nuprl2haskell:IEQ_TERM")
      | SPR_TERM (pair, var1, var2, rterm) =>
	"let (" ^ nuprl2haskell_var var1 ^ "," ^ nuprl2haskell_var var2 ^ ") ="
	^ "\n"
	^ nuprl2haskell_ref_term ind sub pair ^ " in"
	^ "\n"
	^ nuprl2haskell_ref_term ind sub rterm
      | AEQ_TERM (n, a, b, rterm1, rterm2) => raise Fail ("nuprl2haskell:AEQ_TERM")
      | DEC_TERM (dec, var1, rterm1, var2, rterm2) =>
	"case " ^ nuprl2haskell_ref_term ind sub dec ^ " of"
	^ "\n"
	^ "  Inl " ^ nuprl2haskell_var var1 ^ " -> " ^ nuprl2haskell_ref_term ind sub rterm1
	^ "\n"
	^ "  Inr " ^ nuprl2haskell_var var2 ^ " -> " ^ nuprl2haskell_ref_term ind sub rterm2
      | IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase) => raise Fail ("nuprl2haskell:IND_TERM")
      | CLO_TERM _ => raise Fail ("nuprl2haskell:CLO_TERM")
      | TERM (((opid, tag), parameters), subterms) =>
	raise Fail ("nuprl2haskell:"
		    ^ opid
		    ^ "("
		    ^ Int.toString (List.length parameters)
		    ^ "-"
		    ^ Int.toString (List.length subterms)
		    ^ ")")

and nuprl2haskell_ref_term ind sub rterm = nuprl2haskell_term ind sub (rterm2term rterm)

and nuprl2haskell_atm ind sub t =
    let val str = nuprl2haskell_term ind sub t
    in if isAtomic t then str else "(" ^ str ^ ")"
    end

and nuprl2haskell_ref_atm ind sub rterm = nuprl2haskell_atm ind sub (rterm2term rterm)

fun nuprl2haskell term =
    let val prelude1 = "data NuprlDisjointUnion a b = Inl a | Inr b"
	val prelude2 = "data NuprlExp = NuprlType of string | NuprlAxiom | NuprlBottom"
	val str = nuprl2haskell_term "  " SUB.empty term
    in prelude1 ^ "\n\n" ^ prelude2 ^ "\n\n" ^ "main =\n" ^ str
    end

(* ------ Nuprl to SML ------ *)
fun nuprl2sml_var var =
    "v" ^ String.map (fn #"-" => #"_" | c => c) (dest_nuprl_var var)

fun nuprl2sml_term ind sub term =
    case term of
	AXM_TERM => "[]"
      | BOT_TERM => "(let fun f () = f () in f () end)"
      | INT_TERM => "Type(\"int\")"
      | VOI_TERM => "Type(\"void\")"
      | DUM_TERM => raise Fail "nuprl2sml_term:DUM_TERM"
      | ATM_TERM NONE => "Type(\"atom\")"
      | ATM_TERM (SOME n) => "Type(\"atom" ^ Int.toString n ^ "\")"
      | TOK_TERM p => raise Fail "nuprl2sml_term:TOK_TERM"
      | NAT_TERM n => II.toString n
      | VAR_TERM var =>
	let val v = dest_nuprl_var var
	in case SUB.find (sub, v) of
	       SOME v => v
	     | NONE => nuprl2sml_var var
	end
      | INL_TERM t => "Inl(" ^ nuprl2sml_ref_term ind sub t ^ ")"
      | INR_TERM t => "Inr(" ^ nuprl2sml_ref_term ind sub t ^ ")"
      | FIX_TERM t =>
	let val f = rterm2term t
	in if is_nuprl_lambda_term f
	   then let val (r,g) = dest_lambda 0 f
		in if is_nuprl_lambda_term g
		   then let val (x,b) = dest_lambda 0 g
			in "let fun "
			   ^ nuprl2sml_var r
			   ^ " "
			   ^ nuprl2sml_var x
			   ^ " =\n"
			   ^ nuprl2sml_term ind sub b
			   ^ "\n"
			   ^ "in "
			   ^ nuprl2sml_var r
			   ^ "\n"
			   ^ "end"
			end
		   else raise Fail "nuprl2sml_term:FIX_TERM(2)"
		end
	   else raise Fail "nuprl2sml_term:FIX_TERM(1)"
	end
      | MIN_TERM t => raise Fail "nuprl2sml_term:MIN_TERM"
      | LAM_TERM (x, f) => "fn " ^ nuprl2sml_var x ^ " =>\n" ^ nuprl2sml_ref_term ind sub f
      | REC_TERM (t, e) => raise Fail "nuprl2sml_term:REC_TERM"
      | WAI_TERM (t, e) => raise Fail "nuprl2sml_term:WAI_TERM"
      | APP_TERM (f, a) =>
	let val g = rterm2term f in
	    if is_nuprl_lambda_term g
	    then let val (var, b) = dest_lambda 0 g
		 in "let val "
		    ^ nuprl2sml_var var
		    ^ " = "
		    ^ nuprl2sml_ref_term ind sub a
		    ^ "\n"
		    ^ "in "
		    ^ nuprl2sml_term ind sub b
		    ^ "\n"
		    ^ "end"
		 end
	    else nuprl2sml_ref_atm ind sub f ^ " " ^ nuprl2sml_ref_atm ind sub a
	end
      | PAI_TERM (x, y) => "[" ^ nuprl2sml_ref_term ind sub x ^ "," ^ nuprl2sml_ref_term ind sub y ^ "]"
      | ADD_TERM (x, y) => raise Fail "nuprl2sml_term:ADD_TERM"
      | SUB_TERM (x, y) => raise Fail "nuprl2sml_term:SUB_TERM"
      | MUL_TERM (x, y) => raise Fail "nuprl2sml_term:MUL_TERM"
      | DIV_TERM (x, y) => raise Fail "nuprl2sml_term:DIV_TERM"
      | REM_TERM (x, y) => raise Fail "nuprl2sml_term:REM_TERM"
      | EQT_TERM (x, y) => raise Fail "nuprl2sml_term:EQT_TERM"
      | UNI_TERM (x, y) => raise Fail "nuprl2sml_term:UNI_TERM"
      | EQU_TERM (a, rterm1, rterm2) => raise Fail "nuprl2sml_term:EQU_TERM"
      | IAX_TERM (a, rterm1, rterm2) =>
	"(case " ^ nuprl2sml_ref_term ind sub a ^ " of"
	^ "\n  [] =>\n" ^ nuprl2sml_ref_term ind sub rterm1
	^ "\n| x =>\n" ^ nuprl2sml_ref_term ind sub rterm2
	^ ")"
      | IPA_TERM (a, rterm1, rterm2) =>
	"(case " ^ nuprl2sml_ref_term ind sub a ^ " of"
	^ "\n  [x,y] =>\n" ^ nuprl2sml_ref_term ind sub rterm1
	^ "\n| x =>\n" ^ nuprl2sml_ref_term ind sub rterm2
	^ ")"
      | IIR_TERM (a, rterm1, rterm2) => raise Fail "nuprl2sml_term:IIR_TERM"
      | IIL_TERM (a, rterm1, rterm2) => raise Fail "nuprl2sml_term:IIL_TERM"
      | IIN_TERM (a, rterm1, rterm2) => raise Fail "nuprl2sml_term:IIN_TERM"
      | ILA_TERM (a, rterm1, rterm2) => raise Fail "nuprl2sml_term:ILA_TERM"
      | IAT_TERM (a, rterm1, rterm2) => raise Fail "nuprl2sml_term:IAT_TERM"
      | CBV_TERM (arg, x, B) =>
	"let val "
	^ nuprl2sml_var x
	^ " = "
	^ nuprl2sml_ref_term ind sub arg
	^ "\n"
	^ "in "
	^ nuprl2sml_ref_term ind sub B
	^ "\n"
	^ "end"
      | CBA_TERM (arg, x, B) =>
	"let val "
	^ nuprl2sml_var x
	^ " = "
	^ nuprl2sml_ref_term ind sub arg
	^ "\n"
	^ "in "
	^ nuprl2sml_ref_term ind sub B
	^ "\n"
	^ "end"
      | FUN_TERM (a, rterm1, rterm2) => raise Fail "nuprl2sml_term:FUN_TERM"
      | PRD_TERM (a, rterm1, rterm2) => raise Fail "nuprl2sml_term:PRD_TERM"
      | TUN_TERM (a, rterm1, rterm2) => raise Fail "nuprl2sml_term:TUN_TERM"
      | SET_TERM (a, rterm1, rterm2) => raise Fail "nuprl2sml_term:SET_TERM"
      | LES_TERM (a, b, rterm1, rterm2) =>
	"if "
	^ nuprl2sml_ref_term ind sub a
	^ " < "
	^ nuprl2sml_ref_term ind sub b
	^ " then "
	^ nuprl2sml_ref_term ind sub rterm1
	^ " else "
	^ nuprl2sml_ref_term ind sub rterm2
      | IEQ_TERM (a, b, rterm1, rterm2) =>
	"if "
	^ nuprl2sml_ref_term ind sub a
	^ " = "
	^ nuprl2sml_ref_term ind sub b
	^ " then "
	^ nuprl2sml_ref_term ind sub rterm1
	^ " else "
	^ nuprl2sml_ref_term ind sub rterm2
      | SPR_TERM (pair, var1, var2, rterm) =>
	"(case " ^ nuprl2sml_ref_term ind sub pair ^ " of"
	^ "\n  [" ^ nuprl2sml_var var1 ^ "," ^ nuprl2sml_var var2 ^ "] =>\n" ^ nuprl2sml_ref_term ind sub rterm
	^ "\n| x => raise Fail \"SPR_TERM\""
	^ ")"
      | AEQ_TERM (n, a, b, rterm1, rterm2) =>
	"if "
	^ nuprl2sml_ref_term ind sub a
	^ " = "
	^ nuprl2sml_ref_term ind sub b
	^ " then "
	^ nuprl2sml_ref_term ind sub rterm1
	^ " else "
	^ nuprl2sml_ref_term ind sub rterm2
      | DEC_TERM (dec, var1, rterm1, var2, rterm2) =>
	"(case " ^ nuprl2sml_ref_term ind sub dec ^ " of"
	^ "\n  Inl(" ^ nuprl2sml_var var1 ^ ") =>\n" ^ nuprl2sml_ref_term ind sub rterm1
	^ "\n| Inr(" ^ nuprl2sml_var var2 ^ ") =>\n" ^ nuprl2sml_ref_term ind sub rterm2
	^ ")"
      | IND_TERM _ => raise Fail ("nuprl2sml:IND_TERM")
      | CLO_TERM _ => raise Fail ("nuprl2sml:CLO_TERM")
      | TERM ((("subtract", _), []), [B_TERM ([], x), B_TERM ([], y)]) =>
	nuprl2sml_ref_term ind sub x ^ " - " ^ nuprl2sml_ref_term ind sub y
      | TERM ((("add", _), []), [B_TERM ([], x), B_TERM ([], y)]) =>
	nuprl2sml_ref_term ind sub x ^ " + " ^ nuprl2sml_ref_term ind sub y
      | TERM ((("divide", _), []), [B_TERM ([], x), B_TERM ([], y)]) =>
	nuprl2sml_ref_term ind sub x ^ " div " ^ nuprl2sml_ref_term ind sub y
      | TERM ((("callbyvalueall", _), _), [B_TERM ([], arg), B_TERM ([x], B)]) =>
	"let val "
	^ nuprl2sml_var x
	^ " = "
	^ nuprl2sml_ref_term ind sub arg
	^ "\n"
	^ "in "
	^ nuprl2sml_ref_term ind sub B
	^ "\n"
	^ "end"
      | TERM ((("callbyvalue", _), _), [B_TERM ([], arg), B_TERM ([x], B)]) =>
	"let val "
	^ nuprl2sml_var x
	^ " = "
	^ nuprl2sml_ref_term ind sub arg
	^ "\n"
	^ "in "
	^ nuprl2sml_ref_term ind sub B
	^ "\n"
	^ "end"
      | TERM ((("atom_eq", _), _), [B_TERM ([], a), B_TERM ([], b), B_TERM ([], rterm1), B_TERM ([], rterm2)]) =>
	"if "
	^ nuprl2sml_ref_term ind sub a
	^ " = "
	^ nuprl2sml_ref_term ind sub b
	^ " then "
	^ nuprl2sml_ref_term ind sub rterm1
	^ " else "
	^ nuprl2sml_ref_term ind sub rterm2
      | TERM ((("less", _), _), [B_TERM ([], a), B_TERM ([], b), B_TERM ([], rterm1), B_TERM ([], rterm2)]) =>
	"if "
	^ nuprl2sml_ref_term ind sub a
	^ " < "
	^ nuprl2sml_ref_term ind sub b
	^ " then "
	^ nuprl2sml_ref_term ind sub rterm1
	^ " else "
	^ nuprl2sml_ref_term ind sub rterm2
      | TERM ((("eq_term", _), _), [B_TERM ([], a), B_TERM ([], b)]) =>
	"if "
	^ nuprl2sml_ref_term ind sub a
	^ " = "
	^ nuprl2sml_ref_term ind sub b
	^ " then Inl(NONE)"
	^ " else Inr(NONE)"
      | (term as TERM ((("product", _), _), [B_TERM ([], a), B_TERM ([v], b)])) =>
	if is_null_nuprl_var v
	then "Type(\"" ^ ppTerm term ^ "\")"
	else raise Fail "nuprl2sml_term:product"
      | (term as TERM ((("union", _), _), [B_TERM ([], a), B_TERM ([], b)])) =>
	"Type(\"" ^ ppTerm term ^ "\")"
      | TERM ((("token", _), [(t,tkind)]), []) => "\"" ^ t ^ "\""
      | TERM ((("ifthenelse", _), _), [B_TERM ([], a), B_TERM ([], b), B_TERM ([], c)]) =>
	"(case " ^ nuprl2sml_ref_term ind sub a ^ " of"
	^ "\n  Inl _ => " ^ nuprl2sml_ref_term ind sub b
	^ "\n| Inr _ => " ^ nuprl2sml_ref_term ind sub c
	^ ")"
      | TERM ((("cons", _), _), [B_TERM ([], a), B_TERM ([], b)]) =>
	"(" ^ nuprl2sml_ref_atm ind sub a ^ "::" ^ nuprl2sml_ref_atm ind sub b ^ ")"
      | TERM ((("map", _), _), [B_TERM ([], f), B_TERM ([], a)]) =>
	"map "
	^ nuprl2sml_ref_atm ind sub f
	^ " "
	^ nuprl2sml_ref_atm ind sub a
      | TERM ((("null", _), _), [B_TERM ([], a)]) =>
	"null(" ^ nuprl2sml_ref_term ind sub a ^ ")"
      | TERM ((("append", _), _), [B_TERM ([], a), B_TERM ([], b)]) =>
	"(" ^ nuprl2sml_ref_atm ind sub a ^ " @ " ^ nuprl2sml_ref_atm ind sub b ^ ")"
      | TERM ((("reduce", _), _), [B_TERM ([], f), B_TERM ([], i), B_TERM ([], l)]) =>
	"foldr "
	^ nuprl2sml_ref_atm ind sub f
	^ nuprl2sml_ref_atm ind sub i
	^ nuprl2sml_ref_atm ind sub l
      | TERM ((("list_ind", _), _), [B_TERM ([], l), B_TERM ([], i), B_TERM ([x, xs, r], g)]) =>
	"let fun list_ind [] = " ^ nuprl2sml_ref_atm ind sub i ^ "\n"
	^ "      | list_ind (" ^ nuprl2sml_var x ^ " :: " ^ nuprl2sml_var xs ^ ") =\n"
	^ "          let val " ^ nuprl2sml_var r ^ " = list_ind " ^ nuprl2sml_var xs ^ "\n"
	^ "          in " ^ nuprl2sml_ref_term ind sub g ^ "\n"
	^ "          end\n"
	^ "in list_ind " ^ nuprl2sml_ref_term ind sub l ^ "\n"
	^ "end"
      | TERM ((("nil", _), _), []) => "[]"
      | TERM (((opid, tag), parameters), subterms) =>
	raise Fail ("nuprl2sml:"
		    ^ opid
		    ^ "("
		    ^ Int.toString (List.length parameters)
		    ^ "-"
		    ^ Int.toString (List.length subterms)
		    ^ ")")

and nuprl2sml_ref_term ind sub rterm = nuprl2sml_term ind sub (rterm2term rterm)

and nuprl2sml_atm ind sub t =
    let val str = nuprl2sml_term ind sub t
    in if isAtomic t then str else "(" ^ str ^ ")"
    end

and nuprl2sml_ref_atm ind sub rterm = nuprl2sml_atm ind sub (rterm2term rterm)

fun nuprl2sml term =
    let val prelude1 = "datatype ('a, 'b) nuprlInj = Inl of 'a | Inr of 'b"
	val prelude2 = "datatype nuprlType = Type of string"
	val str = nuprl2sml_term "  " SUB.empty term
    in  prelude1 ^ "\n\n" ^ prelude2 ^ "\n\n" ^ "val main =\n" ^ str
    end


(* ------ Nuprl to Lisp ------ *)
fun nuprl2lisp_var' var =
    "nv"
    ^ String.translate
	  (fn #"'" => "_"
	    | c => String.str c)
	  var

fun nuprl2lisp_var var = nuprl2lisp_var' (dest_nuprl_var var)

(* vars is the list of bound variables boud in the type in which that term is *)
fun nuprl2lisp_term_sp vars term =
    case term of
	NAT_TERM n => II.toString n
      | VAR_TERM var =>
	let val v = dest_nuprl_var var
	in if VARS.member (vars, v)
	   then v
	   else raise Fail ("nuprl2lisp_term_sp:unbound_variable(" ^ v ^ ")")
	end
      | _ => raise Fail ("nuprl2lisp_term_sp(" ^ opid_of_term term ^ ")")

fun nuprl2lisp_rterm_sp vars rterm = nuprl2lisp_term_sp vars (rterm2term rterm)

datatype lisp_type_mark =
	 LTM_TYP of string
       | LTM_VAR of string

fun nuprl2lisp_type_aux sub vars term =
    case term of
	VAR_TERM var =>
	let val v = dest_nuprl_var var
 	in if VARS.member (vars, v) (* then v is bound in the type *)
	   then [LTM_TYP v]
	   else case SUB.find (sub, v) of (* then v is bound outside the type *)
		    SOME (f,b,args) => raise Fail "nuprl2lisp_type_aux:VAR_TERM" (*[LTM_VAR str]*)
		  | NONE => [LTM_VAR (nuprl2lisp_var var)]
	end
      | INT_TERM => [LTM_TYP "int"]
      | VOI_TERM => [LTM_TYP "void"]
      | ATM_TERM NONE => [LTM_TYP "atom"]
      | ATM_TERM (SOME n) => [LTM_TYP ("atom" ^ Int.toString n)]
      | EQU_TERM (a, rterm1, rterm2) =>
	let val lst = nuprl2lisp_type_aux sub vars (rterm2term a)
	    (*val _ = print (toStringTerm term ^ "\n")*)
	    val x1  = nuprl2lisp_rterm_sp vars rterm1
	    val x2  = nuprl2lisp_rterm_sp vars rterm2
	in [LTM_TYP "eq("]
	   @ lst
	   @ [LTM_TYP ","]
	   @ [LTM_TYP x1]
	   @ [LTM_TYP ","]
	   @ [LTM_TYP x2]
	   @ [LTM_TYP ")"]
	end
      | FUN_TERM (arg, x, B) =>
	let val lst1  = nuprl2lisp_type_aux sub vars (rterm2term arg)
	    val v     = dest_nuprl_var x
	    val vars' = VARS.add (vars, v)
	    val lst2  = nuprl2lisp_type_aux sub vars' (rterm2term B)
	in [LTM_TYP "function("]
	   @ lst1
	   @ [LTM_TYP ","]
	   @ [LTM_TYP v]
	   @ [LTM_TYP ","]
	   @ lst2
	   @ [LTM_TYP ")"]
	end
      | PRD_TERM (arg, x, B) =>
	let val lst1  = nuprl2lisp_type_aux sub vars (rterm2term arg)
	    val v     = dest_nuprl_var x
	    val vars' = VARS.add (vars, v)
	    val lst2  = nuprl2lisp_type_aux sub vars' (rterm2term B)
	in [LTM_TYP "product("]
	   @ lst1
	   @ [LTM_TYP ","]
	   @ [LTM_TYP v]
	   @ [LTM_TYP ","]
	   @ lst2
	   @ [LTM_TYP ")"]
	end
      | TUN_TERM (arg, x, B) =>
	let val lst1  = nuprl2lisp_type_aux sub vars (rterm2term arg)
	    val v     = dest_nuprl_var x
	    val vars' = VARS.add (vars, v)
	    val lst2  = nuprl2lisp_type_aux sub vars' (rterm2term B)
	in [LTM_TYP "tunion("]
	   @ lst1
	   @ [LTM_TYP ","]
	   @ [LTM_TYP v]
	   @ [LTM_TYP ","]
	   @ lst2
	   @ [LTM_TYP ")"]
	end
      | SET_TERM (arg, x, B) => [LTM_TYP "set"] (* this will have to change *)
	(*let (*val _     = print (toStringTerm term ^ "\n")*)
	    val lst1  = nuprl2lisp_type_aux sub vars (rterm2term arg)
	    val v     = dest_nuprl_var x
	    val vars' = VARS.add (vars, v)
	    val lst2  = nuprl2lisp_type_aux sub vars' (rterm2term B)
	in [LTM_TYP "set("]
	   @ lst1
	   @ [LTM_TYP ","]
	   @ [LTM_TYP v]
	   @ [LTM_TYP ","]
	   @ lst2
	   @ [LTM_TYP ")"]
	end*)
      | UNI_TERM (x, y) =>
	let val lst1 = nuprl2lisp_type_aux sub vars (rterm2term x)
	    val lst2 = nuprl2lisp_type_aux sub vars (rterm2term y)
	in [LTM_TYP "union("]
	   @ lst1
	   @ [LTM_TYP ","]
	   @ lst2
	   @ [LTM_TYP ")"]
	end
      | REC_TERM (x, t) =>
	let val v     = dest_nuprl_var x
	    val vars' = VARS.add (vars, v)
	    val lst   = nuprl2lisp_type_aux sub vars' (rterm2term t)
	in [LTM_TYP "rec("]
	   @ [LTM_TYP v]
	   @ [LTM_TYP ","]
	   @ lst
	   @ [LTM_TYP ")"]
	end
      | SPR_TERM (pair, var1, var2, rterm) =>
	let val lst   = nuprl2lisp_type_aux sub vars (rterm2term pair)
	    val v1    = dest_nuprl_var var1
	    val v2    = dest_nuprl_var var2
	    val vars' = VARS.add (VARS.add (vars, v1), v2)
	    val lst'  = nuprl2lisp_type_aux sub vars' (rterm2term rterm)
	in [LTM_TYP "spread("]
	   @ lst
	   @ [LTM_TYP ","]
	   @ [LTM_TYP v1]
	   @ [LTM_TYP ","]
	   @ [LTM_TYP v2]
	   @ [LTM_TYP ","]
	   @ lst'
	   @ [LTM_TYP ")"]
	end
      | DEC_TERM (dec, var1, rterm1, var2, rterm2) =>
	let val lst    = nuprl2lisp_type_aux sub vars (rterm2term dec)
	    val v1     = dest_nuprl_var var1
	    val vars1' = VARS.add (vars, v1)
	    val lst1   = nuprl2lisp_type_aux sub vars1' (rterm2term rterm1)
	    val v2     = dest_nuprl_var var2
	    val vars2' = VARS.add (vars, v2)
	    val lst2   = nuprl2lisp_type_aux sub vars2' (rterm2term rterm2)
	in [LTM_TYP "decide("]
	   @ lst
	   @ [LTM_TYP ","]
	   @ [LTM_TYP v1]
	   @ [LTM_TYP ","]
	   @ lst1
	   @ [LTM_TYP ","]
	   @ [LTM_TYP v2]
	   @ [LTM_TYP ","]
	   @ lst2
	   @ [LTM_TYP ")"]
	end
      | _ => raise Fail ("nuprl2lisp_type_aux(" ^ opid_of_term term ^ ")")

fun nuprl2lisp_type libop sub term =
    let val term =
	    case libop of
		SOME lib => unfold_all lib term
	      | NONE => term
	val lst = nuprl2lisp_type_aux sub VARS.empty term
    in "(concatenate 'string "
       ^ T.fmt {init  = "",
		final = "",
		sep   = " ",
		fmt   = fn LTM_TYP v => "\"" ^ v ^ "\""
			 | LTM_VAR v => v}
	       lst ^ ")"
    end

(*fun unionOps ops1 ops2 = MAP.unionWith (fn (x,y) => x @ y) (ops1, ops2)*)

fun unionFuns funs1 funs2 =
    MAP.unionWith
	(fn (x,y) => x) (* x and y should be equal *)
	(funs1, funs2)

fun fmtStr init final sep lst =
    T.fmt {init  = init,
	   final = final,
	   sep   = sep,
	   fmt   = fn k => k}
	  lst

fun ifCheck check str =
    if check then "\n" ^ str else ""

datatype arg = ARG_V of string | ARG_R of string

fun strip_args args = map (fn (ARG_V v) => v | (ARG_R v) => v) args

fun strip_fargs fv fr args = map (fn (ARG_V v) => fv v | (ARG_R v) => fr v) args

fun app2lisp b name args =
    "("
    ^ (if b then "" else "funcall ")
    ^ nuprl2lisp_var' name
    ^ (if List.null args
       then ""
       else T.fmt {init = " ",
		   final = "",
		   sep   = " ",
		   fmt   = fn v => v}
		  args)
    ^ ")"

fun nuprl2lisp_term check count funs ind libop sub term =
    case term of
	AXM_TERM => (funs, "(make-axiom)", count, VARS.empty)
      | BOT_TERM => (funs, "(error \"bottom\")", count, VARS.empty)
      | INT_TERM => (funs, nuprl2lisp_type libop sub term, count, VARS.empty)
      | VOI_TERM => (funs, nuprl2lisp_type libop sub term, count, VARS.empty)
      | DUM_TERM => raise Fail "nuprl2lisp_term:DUM_TERM"
      | ATM_TERM NONE => (funs, nuprl2lisp_type libop sub term, count, VARS.empty)
      | ATM_TERM (SOME n) => (funs, nuprl2lisp_type libop sub term, count, VARS.empty)
      | TOK_TERM (t,k) =>  (funs, "\"" ^ t ^ "\"", count, VARS.empty)
      | NAT_TERM n => (funs, II.toString n, count, VARS.empty)
      | VAR_TERM var =>
	(funs,
	 case SUB.find (sub, dest_nuprl_var var) of
	     SOME (f,b,args) => app2lisp b f (map nuprl2lisp_var' args)
	   | NONE => nuprl2lisp_var var,
	 count,
	 VARS.empty)
      | INL_TERM t =>
	let val (funs',str,count',deps) = nuprl2lisp_ref_term check count funs ind libop sub t
	in (funs', "(make-inl :val " ^ str ^ ")", count', deps)
	end
      | INR_TERM t =>
	let val (funs',str,count',deps) = nuprl2lisp_ref_term check count funs ind libop sub t
	in (funs', "(make-inr :val " ^ str ^ ")", count', deps)
	end
      | FIX_TERM t =>
	let val f = rterm2term t
	(* we have a term of the form fix(f) and we want to extrat this fixpoint
	 * as a new independent function using defun *)
	in if is_nuprl_lambda_term f
	   then let val (r,g)  = dest_lambda 0 f
		    (* the term is actually of the form fix(\r.g[r])
		     * given
		     *     frees: (x1,...,xn)
		     * the free variable of that term, we want to, replace the fix with:
		     *     (nvfix-auxk x1 ... xn)
		     * and define
		     *     (defun (nvfix-auxk (x1 ... xn) g[(fix-auxk x1 ... xn)/r])
		     *
		     * now some of the xi can functions ...
		     *)
		    val vr     = dest_nuprl_var r
		    val count1 = count + 1
		    val var    = "fix-aux" ^ Int.toString count
		    val nvar   = nuprl2lisp_var' var
		    val frees  = VARS.listItems (free_vars f)
		    val margs  = map (fn v =>
					 case SUB.find (sub, v) of
					     SOME (v,_,_) => ARG_R v
					   | NONE => ARG_V v)
				     frees
		    val args   = strip_args margs
		    val sub1   = SUB.foldri (fn (i, (var,b,args), sub) =>
						if List.exists (fn x => x = i) frees
						then SUB.insert (sub, i, (var,false,args))
						else sub)
					    SUB.empty
					    sub
		    val sub2   = SUB.insert (sub1, vr, (var,true,args))
		    val fargs  = strip_fargs nuprl2lisp_var' (fn v => "#'" ^ nuprl2lisp_var' v) margs
		    val app    = app2lisp true var fargs
		    val (funs',count2,deps) = nuprl2lisp_aux check count1 funs var nvar args libop sub2 g
		in (funs',app,count2,deps)
		end
		(*in if is_nuprl_lambda_term g
		   then let val (x,b) = dest_lambda 0 g
			    val r' = "#'" ^ nuprl2lisp_var r
			    val sub' = SUB.insert (sub, dest_nuprl_var r, r')
			    val (funs,str,count') = nuprl2lisp_term check count ind libop sub' b
			in (funs,
			    "(labels (("
			    ^ nuprl2lisp_var r
			    ^ " ("
			    ^ nuprl2lisp_var x
			    ^ ")"
			    ^ "\n"
			    ^ str
			    ^ "))\n"
			    ^ "#'" ^ nuprl2lisp_var r
			    ^ ")",
			    count')
			end
		   else raise Fail "nuprl2lisp_term:FIX_TERM:non_lambda_lambda"
		end*)
	   else raise Fail "nuprl2lisp_term:FIX_TERM:non_lambda"
	end(* raise Fail "nuprl2lisp:FIX_TERM:not_applied"*)
      | MIN_TERM t =>
	let val (funs',str,count',deps) = nuprl2lisp_ref_term check count funs ind libop sub t
	in (funs',
	    "(- "
	    ^ str
	    ^ ")",
	    count',
	    deps)
	end
      | LAM_TERM (x, f) =>
	let val vars = free_vars (rterm2term f)
	    val vx   = dest_nuprl_var x
	    val b    = VARS.member (vars, vx)
	    val v    = nuprl2lisp_var x
	    val sub' = (#1 (SUB.remove (sub, vx))) handle _ => sub
	    val (funs',str,count',deps) = nuprl2lisp_ref_term check count funs ind libop sub' f
	in (funs',
	    "(lambda ("
	    ^ v
	    ^ ")"
	    ^ (if b then "" else "\n(declare (ignore " ^ v ^ "))")
	    ^ "\n"
	    ^ str
	    ^ ")",
	    count',
	    deps)
	end
      | REC_TERM (t, e) => (funs, nuprl2lisp_type libop sub term, count, VARS.empty)
      | WAI_TERM (t, e) => raise Fail "nuprl2lisp_term:WAI_TERM"
      | APP_TERM (f, a) =>
	((*case rterm2term f of
	     FIX_TERM t =>
	     let val f = rterm2term t
	     in if is_nuprl_lambda_term f
		then let val (r,g) = dest_lambda 0 f
		     in if is_nuprl_lambda_term g
			then let val (x,b) = dest_lambda 0 g
				 val r' = "#'" ^ nuprl2lisp_var r
				 val sub' = SUB.insert (sub, dest_nuprl_var r, r')
				 val (funs1,str1,count1) = nuprl2lisp_term check count ind libop sub' b
				 val (funs2,str2,count2) = nuprl2lisp_ref_term check count1 ind libop sub a
			     in (unionFuns funs1 funs2,
				 "(labels (("
				 ^ nuprl2lisp_var r
				 ^ " ("
				 ^ nuprl2lisp_var x
				 ^ ")"
				 ^ "\n"
				 ^ str1
				 ^ "))\n"
				 ^ "(funcall #'" ^ nuprl2lisp_var r
				 ^ "\n"
				 ^ str2
				 ^ "))",
				 count2)
			     end
			else raise Fail "nuprl2lisp_term:APP_TERM:FIX_TERM:non_lamba_lambda"
		     end
		else raise Fail "nuprl2lisp_term:APP_TERM:FIX_TERM:non_lambda"
	     end
	   | _ =>*)
	     let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub f
		 val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub a
		 val str =
		     "(funcall "
		     ^ str1
		     ^ "\n"
		     ^ str2
		     ^ ")"
	     in (funs2, str, count2, SET.union (deps1, deps2))
	     end)
      | PAI_TERM (x, y) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub x
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub y
	    (* (defstruct pair fst snd)
	     * - defines pair-p, make-pair, pair-fst, pair-snd
	     * (defstruct axiom)
	     *)
	    val str =
		"(make-pair :fst "
		^ str1
		^ " :snd "
		^ str2
		^ ")"
	in (funs2, str, count2, SET.union (deps1, deps2))
	end
      | ADD_TERM (x, y) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub x
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub y
	    val str =
		"(+ "
		^ str1
		^ " "
		^ str2
		^ ")"
	in (funs2, str, count2, SET.union (deps1, deps2))
	end
      | SUB_TERM (x, y) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub x
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub y
	    val str =
		"(- "
		^ str1
		^ " "
		^ str2
		^ ")"
	in (funs2, str, count2, SET.union (deps1, deps2))
	end
      | MUL_TERM (x, y) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub x
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub y
	    val str =
		"(* "
		^ str1
		^ " "
		^ str2
		^ ")"
	in (funs2, str, count2, SET.union (deps1, deps2))
	end
      | DIV_TERM (x, y) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub x
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub y
	    val str =
		"(/ "
		^ str1
		^ " "
		^ str2
		^ ")"
	in (funs2, str, count2, SET.union (deps1, deps2))
	end
      | REM_TERM (x, y) => raise Fail "nuprl2lisp_term:REM_TERM"
      | EQT_TERM (x, y) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub x
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub y
	    val str =
		"(if (string= "
		^ str1
		^ " "
		^ str2
		^ ")"
		^ "\n"
		^ "(make-inl :val (make-axiom))"
		^ "\n"
		^ "(make-inr :val (make-axiom)))"
	in (funs2, str, count2, SET.union (deps1, deps2))
	end
      | UNI_TERM (x, y) => (funs, nuprl2lisp_type libop sub term, count, VARS.empty)
      | EQU_TERM (a, rterm1, rterm2) => (funs, nuprl2lisp_type libop sub term, count, VARS.empty)
      | IAX_TERM (a, rterm1, rterm2) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub a
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub rterm1
	    val (funs3,str3,count3,deps3) = nuprl2lisp_ref_term check count2 funs2 ind libop sub rterm2
	    val str =
		"(if (axiom-p "
		^ str1
		^ ")"
		^ "\n"
		^ str2
		^ "\n"
		^ str3
		^ ")"
	in (funs3, str, count3, SET.union (deps1, SET.union (deps2, deps3)))
	end
      | IPA_TERM (a, rterm1, rterm2) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub a
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub rterm1
	    val (funs3,str3,count3,deps3) = nuprl2lisp_ref_term check count2 funs2 ind libop sub rterm2
	    val str =
		"(if (pair-p "
		^ str1
		^ ")"
		^ "\n"
		^ str2
		^ "\n"
		^ str3
		^ ")"
	in (funs3, str, count3, SET.union (deps1, SET.union (deps2, deps3)))
	end
      | IIR_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp_term:IIR_TERM"
      | IIL_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp_term:IIL_TERM"
      | IIN_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp_term:IIN_TERM"
      | ILA_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp_term:ILA_TERM"
      | IAT_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp_term:IAT_TERM"
      | CBV_TERM (arg, x, B) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub B
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub arg
	    val s1 =
		"(funcall (lambda ("
		^ nuprl2lisp_var x
		^ ")\n"
		^ str1
		^ ")"
		^ "\n"
		^ str2
		^ ")"
	    val s2 =
		"(let (("
		^ nuprl2lisp_var x
		^ " "
		^ str2
		^ "))\n"
		^ str1
		^ ")"
	in (funs2, s2, count2, SET.union (deps1, deps2))
	end
      | CBA_TERM (arg, x, B) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub B
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub arg
	    val s1 =
		"(funcall (lambda ("
		^ nuprl2lisp_var x
		^ ")\n"
		^ str1
		^ ")"
		^ "\n"
		^ str2
		^ ")"
	    val s2 =
		"(let (("
		^ nuprl2lisp_var x
		^ " "
		^ str2
		^ "))\n"
		^ str1
		^ ")"
	in (funs2, s2, count2, SET.union (deps1, deps2))
	end
      | FUN_TERM (arg, x, B) => (funs, nuprl2lisp_type libop sub term, count, SET.empty)
      | PRD_TERM (arg, x, B) => (funs, nuprl2lisp_type libop sub term, count, SET.empty)
      | TUN_TERM (arg, x, B) => (funs, nuprl2lisp_type libop sub term, count, SET.empty)
      | SET_TERM (arg, x, B) => (funs, nuprl2lisp_type libop sub term, count, SET.empty)
      | LES_TERM (a, b, rterm1, rterm2) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub a
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub b
	    val (funs3,str3,count3,deps3) = nuprl2lisp_ref_term check count2 funs2 ind libop sub rterm1
	    val (funs4,str4,count4,deps4) = nuprl2lisp_ref_term check count3 funs3 ind libop sub rterm2
	    val str =
		"(if (< "
		^ str1
		^ " "
		^ str2
		^ ")"
		^ "\n"
		^ str3
		^ "\n"
		^ str4
		^ ")"
	    val deps = SET.union (deps1, SET.union (deps2, SET.union (deps3, deps4)))
	in (funs4, str, count4, deps)
	end
      | IEQ_TERM (a, b, rterm1, rterm2) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub a
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub b
	    val (funs3,str3,count3,deps3) = nuprl2lisp_ref_term check count2 funs2 ind libop sub rterm1
	    val (funs4,str4,count4,deps4) = nuprl2lisp_ref_term check count3 funs3 ind libop sub rterm2
	    val str =
		"(if (= "
		^ str1
		^ " "
		^ str2
		^ ")"
		^ "\n"
		^ str3
		^ "\n"
		^ str4
		^ ")"
	    val deps = SET.union (deps1, SET.union (deps2, SET.union (deps3, deps4)))
	in (funs4, str, count4, deps)
	end
      | SPR_TERM (pair, var1, var2, rterm) =>
	let val vars = free_vars (rterm2term rterm)
	    val dv1  = dest_nuprl_var var1
	    val dv2  = dest_nuprl_var var2
	    val b1   = VARS.member (vars, dv1)
	    val b2   = VARS.member (vars, dv2)
	    val v1   = nuprl2lisp_var var1
	    val v2   = nuprl2lisp_var var2
	    val sub1 = (#1 (SUB.remove (sub, dv1))) handle _ => sub
	    val sub2 = (#1 (SUB.remove (sub, dv2))) handle _ => sub
	    val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub1 pair
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub2 rterm
	    val str =
		"(let ((pair " (* first check that 'pair' is not already bound *)
		^ str1
		^ "))"
		^ ifCheck check "(if (pair-p pair)"
		^ "\n"
		^ "(let ("
		^ "(" ^ v1 ^ " (pair-fst pair))"
		^ "\n"
		^ "(" ^ v2 ^ " (pair-snd pair))"
		^ ")"
		^ (if b1 then "" else "\n(declare (ignore " ^ v1 ^ "))")
		^ (if b2 then "" else "\n(declare (ignore " ^ v2 ^ "))")
		^ "\n"
		^ str2
		^ ")"
		^ ifCheck check "(error \"spread:a_pair_should_be_a_list_of_length_2\"))"
		^ ")"
	in (funs2, str, count2, SET.union (deps1, deps2))
	end
      | AEQ_TERM (n, a, b, rterm1, rterm2) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub a
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub b
	    val (funs3,str3,count3,deps3) = nuprl2lisp_ref_term check count2 funs2 ind libop sub rterm1
	    val (funs4,str4,count4,deps4) = nuprl2lisp_ref_term check count3 funs3 ind libop sub rterm2
	    val str =
		"(if (string= "
		^ str1
		^ " "
		^ str2
		^ ")"
		^ "\n"
		^ str3
		^ "\n"
		^ str4
		^ ")"
	    val deps = SET.union (deps1, SET.union (deps2, SET.union (deps3, deps4)))
	in (funs4, str, count4, deps)
	end
      | DEC_TERM (dec, var1, rterm1, var2, rterm2) =>
	let val vars1 = free_vars (rterm2term rterm1)
	    val vars2 = free_vars (rterm2term rterm2)
	    val dv1   = dest_nuprl_var var1
	    val dv2   = dest_nuprl_var var2
	    val b1    = VARS.member (vars1, dv1)
	    val b2    = VARS.member (vars2, dv2)
	    val v1    = nuprl2lisp_var var1
	    val v2    = nuprl2lisp_var var2
	    val sub1  = (#1 (SUB.remove (sub, dv1))) handle _ => sub
	    val sub2  = (#1 (SUB.remove (sub, dv2))) handle _ => sub
	    val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub  dec
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub1 rterm1
	    val (funs3,str3,count3,deps3) = nuprl2lisp_ref_term check count2 funs2 ind libop sub2 rterm2
	    val str =
		"(let ((dec " (* first check that 'dec' is not already bound *)
		^ str1
		^ "))"
		^ "\n"
		^ "(if (inl-p dec)"
		^ "\n"
		^ "(let ((" ^ v1 ^ " (inl-val dec)))"
		^ (if b1 then "" else "\n(declare (ignore " ^ v1 ^ "))")
		^ "\n"
		^ str2
		^ ")"
		^ "\n"
		^ "(if (inr-p dec)"
		^ "\n"
		^ "(let ((" ^ v2 ^ " (inr-val dec)))"
		^ (if b2 then "" else "\n(declare (ignore " ^ v2 ^ "))")
		^ "\n"
		^ str3
		^ ")"
		^ "\n"
		^ "(error \"decide:an_injection_should_be_a_list_with_header_inl_or_inr\"))))"
	    val deps = SET.union (deps1, SET.union (deps2, deps3))
	in (funs3, str, count3, deps)
	end
      | IND_TERM _ => raise Fail ("nuprl2lisp:IND_TERM")
      | CLO_TERM _ => raise Fail ("nuprl2lisp:CLO_TERM")
      | TERM ((("ifthenelse", tag), params), [B_TERM ([], a), B_TERM ([], b), B_TERM ([], c)]) =>
	let val (funs1,str1,count1,deps1) = nuprl2lisp_ref_term check count  funs  ind libop sub a
	    val (funs2,str2,count2,deps2) = nuprl2lisp_ref_term check count1 funs1 ind libop sub b
	    val (funs3,str3,count3,deps3) = nuprl2lisp_ref_term check count2 funs2 ind libop sub c
	    val str =
		"(if "
		^ "(inl-p "
		^ str1
		^ ")"
		^ "\n"
		^ str2
		^ "\n"
		^ str3
		^ ")"
	    val deps = SET.union (deps1, SET.union (deps2, deps3))
	in (funs3, str, count3, deps)
	end
      | TERM (((opid, tag), params), sterms) =>
	(case libop of
	     SOME (lib as {abs,tof}) =>
	     if List.exists
		    (fn x => x = opid)
		    [ "unit"
		    , "cons"
		    , "nil"
		    , "it"
		    (*, "single-bag"*)
		    , "empty-bag"
		    (*, "bfalse"
		   , "btrue"
		   , "base-class-program"
		   , "hdf-halt"
		   , "make-Msg"
		   , "mk-msg-interface"
		   , "make-msg-interface"*)
		    , "id-deq"
		    , "atom2-deq"
		    ]
	     then let val term' = unfold_all lib term
		      (*val _ = print ("[unfolding all " ^ opid ^ "]\n")*)
		  in if is_sml_primitive term'
		     then nuprl2lisp_term check count funs ind libop sub term'
		     else raise Fail ("nuprl2lisp:not_primitive:"
				      ^ opid
				      ^ "("
				      ^ Int.toString (List.length params)
				      ^ "-"
				      ^ Int.toString (List.length sterms)
				      ^ ")")
		  end
	     else if simpleSignature (getSignature term)
	     then let val rterms = subterms term
		      val (funs1,strs,count1,deps1) =
			  foldl (fn (rterm, (funs,strs,count,deps)) =>
				    let val (funs',str,count',deps') = nuprl2lisp_ref_term check count funs ind libop sub rterm
				    in (funs',strs @ [str],count',SET.union (deps, deps'))
				    end)
				(funs,[],count,SET.empty)
				rterms
		      val (funs2,count2,deps2) =
			  case MAP.find (funs1, opid) of
			      SOME (_,deps2) => (funs1,count1,deps2)
			    | NONE =>
			      let val {id,sign,obid,lhs,rhs,wfs} = find_sign abs term
				  val args =
				      map (fn rt => dest_variable (rterm2term rt))
					  (subterms (rterm2term lhs))
				  val nid = nuprl2lisp_var' id
				  val sub = SUB.empty
			      in nuprl2lisp_aux check count1 funs1 id nid args libop sub (rterm2term rhs)
			      end
		  in (funs2,
		      (* similar to the APP case but we don't use `funcall` *)
		      app2lisp true opid strs,
		      count2,
		      SET.add(SET.union(deps1,deps2),opid))
		  end
	     else let val {id,sign,obid,lhs,rhs,wfs} = find_sign abs term
		      (*val _ = print ("[unfolding " ^ id ^ "]\n")*)
		      val term' = unfold_ab' term (rterm2term lhs) (rterm2term rhs)
		  in nuprl2lisp_term check count funs ind libop sub term'
		  end
	   | NONE =>
	     raise Fail ("nuprl2lisp:no_lib:"
			 ^ opid
			 ^ "("
			 ^ Int.toString (List.length params)
			 ^ "-"
			 ^ Int.toString (List.length sterms)
			 ^ ")"))

and nuprl2lisp_ref_term check count funs ind lib sub rterm =
    nuprl2lisp_term check count funs ind lib sub (rterm2term rterm)

and nuprl2lisp_aux check count funs name nname args libop sub term =
    let (*val _ = print ("[to lisp: " ^ name ^ "]\n")*)
	val ind = "  "
	val (funs1,str,count1,deps) = nuprl2lisp_term check count funs ind libop sub term
	(*val vars =
	    case libop of
		NONE => free_vars term
	      | SOME lib => free_vars (unfold_all lib term)*)
	(*val _ = print ("[generating function for " ^ name ^ "]\n")*)
	val newfun =
	    "(defun "
	    ^ nname
	    ^ " ("
	    ^ T.fmt {init  = "",
		     final = "",
		     sep   = " ",
		     fmt   = nuprl2lisp_var'} args
	    ^ ")\n"
	    (*^ foldr
		  (fn (arg, str) =>
		      if VARS.member (vars, arg)
		      then str
		      else "(declare (ignore "
			   ^ nuprl2lisp_var' arg
			   ^ "))\n"
			   ^ str) "" args*)
	    ^ str
	    ^ ")"
    in (MAP.insert(funs1,name,(newfun,deps)), count1, SET.add(deps,name))
    end

(*
and nuprl2lisp_aux' check count (lib as {abs,tof}) ops =
    let fun aux count i term terms =
	    let val sign = getSignature term (* this signature should be simple *)
	    in if List.all (fn t => eq_signs sign (getSignature t)) terms
	       then let val {id,sign,obid,lhs,rhs,wfs} = find_sign abs term
			(* i and id should be the same thing *)
			val args =
			    map (fn rt => dest_variable (rterm2term rt))
				(subterms (rterm2term lhs))
			val nid = nuprl2lisp_var' id
			val sub = SUB.empty
		    in nuprl2lisp_aux check count id nid args (SOME lib) sub (rterm2term rhs)
		    end
	       else raise Fail "nuprl2lisp_aux:diff_signs"
	    end
    in MAP.foldri
	   (fn (i, [], (m,count)) => (m,count)
	     | (i, term :: terms, (m,count)) =>
	       let val (m1,count') = aux count i term terms
	       in (unionFuns m m1,count')
	       end)
	   (MAP.empty,count)
	   ops
    end
*)

(*fun order_lisp_functions m = MAP.listItemsi m*)

(*
fun get_all_deps_set id set m =
    VARS.foldr
	(fn (x,set) =>
	    let (*val _ = print ("[dependency of " ^ id ^ ": " ^ x ^ "]\n")*)
		val set' =
		    case MAP.find (m, x) of
			SOME (f,deps) => get_all_deps_set x deps m
		      | NONE => raise Fail ("get_all_deps(" ^ x ^ ")")
	    in VARS.union (set, set')
	    end)
	set
	set
*)

fun vars2string set =
    "[" ^ #1 (VARS.foldr
		  (fn (var, (str, sep)) => (var ^ sep ^ str, ","))
		  ("","")
		  set) ^ "]"

fun compute_full_deps m =
    MAP.mapi
	(fn (id, (f, deps)) =>
	    let (*val _ = print ("[computing dependencies for " ^ id ^ "]\n")*)
		val _ = print ("[depenencies of " ^ id ^ ": " ^ vars2string deps ^ "]\n")
	    in (f, deps)
	    (* (f, get_all_deps_set id deps m)*)
	    end)
	m

fun order_lisp_functions m =
    MAP.foldri
	(fn (i, (func, deps), lst) =>
	    let val (lst1, lst2, _) =
		    foldr (fn ((x,(f,d)),(lst1,lst2,b)) =>
			      if b (*b means that we've already found something on which i depends*)
			      then ((x,(f,d)) :: lst1, lst2, true)
			      else if VARS.member (deps, x) (*i depends on x*)
			      then ((x,(f,d)) :: lst1, lst2, true)
			      else (lst1, (x,(f,d)) :: lst2, false))
			  ([],[],false)
			  lst
		(* i depends on some stuff in lst1
		 * i does not depend on anything in lst2 but some stuff in lst2
		 * might depend on i*)
		val (lst3, lst4, _) =
		    foldl (fn ((x,(f,d)),(lst1,lst2,b)) =>
			      if b (*b means that we've already found something that depends on i*)
			      then (lst1, lst2 @ [(x,(f,d))], true)
			      else if VARS.member (d, i) (*x depends on i*)
			      then (lst1, lst2 @ [(x,(f,d))], true)
			      else (lst1 @ [(x,(f,d))], lst2, false))
			  ([],[],false)
			  lst2
	    in lst1 @ lst3 @ [(i,(func,deps))] @ lst4
	    end)
	[]
	m

fun nuprl2lisp lib unfold term =
    let val term' = if unfold then unfold_all lib term else term
	val libop = if unfold then NONE else SOME lib
	val check = false
	val count = 1
	val id    = "main"
	val nid   = "main"
	val sub   = SUB.empty
	val funs  = MAP.empty
	(*val _     = print ("[translating to lisp]\n")*)
	val (m,_,_) = nuprl2lisp_aux check count funs id nid [] libop sub term'
	(*val _     = print ("[computing dependencies]\n")*)
	val m1    = (*compute_full_deps*) m
	(*val _     = print ("[ordering lisp functions]\n")*)
	val m2    = order_lisp_functions m1
	(*val _     = print ("[generating final string]\n")*)
	val prelude =
	    "(defstruct pair fst snd)"
	    ^ "\n"
	    ^ "(defstruct inl val)"
	    ^ "\n"
	    ^ "(defstruct inr val)"
	    ^ "\n"
	    ^ "(defstruct axiom)"
	    ^ "\n"
    (* we have to transform m into an ordred list depending on the deps *)
    in (*prelude
       ^*) foldr
	     (fn ((i,(func,deps)),str) => func ^ "\n\n" ^ str)
	     ""
	     m2
    end


(* --
 * Another version of nuprl2lisp_term.  The difference is in the way
 * we generate code for "fix" expressions.  It should be simpler.
 * Less efficient?
 * --
 *)

fun is_a_fun funs opid = List.exists (fn (x,_) => x = opid) funs

val emvars = SET.empty
fun singlevar v = SET.singleton v
fun isvar vars v = SET.member (vars, v)
fun remvar vars v = SET.delete (vars, v) handle _ => vars
fun uvars vars1 vars2 = SET.union (vars1, vars2)

(* o check is true to check in the lisp code that pairs are really pairs before
 *   spreading them (and similarly for decide).
 * o funs is the list of functions we are building.
 * o ind is the indentation string
 * o libop is the nuprl library mapping
 * o sub is a substitution function on variables used for recursive functions
 *   defined with fix.
 * o term is the term to transform
 *)
fun nuprl2lisp2_term check funs ind libop sub term =
    case term of
	AXM_TERM => (funs, "(make-axiom)", emvars)
      | BOT_TERM => (funs, "(error \"bottom\")", emvars)
      | INT_TERM => (funs, "\"INT\"", emvars) (*raise Fail "nuprl2lisp2_term:INT_TERM"*)
      | VOI_TERM => (funs, "\"VOID\"", emvars) (*raise Fail "nuprl2lisp2_term:VOI_TERM"*)
      | DUM_TERM => raise Fail "nuprl2lisp2_term:DUM_TERM"
      | ATM_TERM _ => (funs, "\"ATOM\"", emvars) (*raise Fail "nuprl2lisp2_term:ATM_TERM"*)
      | TOK_TERM (t,k) =>  (funs, "\"" ^ t ^ "\"", emvars)
      | NAT_TERM n => (funs, II.toString n, emvars)
      | VAR_TERM var =>
	let val v   = nuprl2lisp_var var
	    val str = case SUB.find (sub, dest_nuprl_var var) of
			  SOME str => str
			| NONE => v
	in (funs, str, singlevar v)
	end
      | INL_TERM t =>
	let val (funs',str,vars) = nuprl2lisp2_ref_term check funs ind libop sub t
	in (funs', "(make-inl :val " ^ str ^ ")",vars)
	end
      | INR_TERM t =>
	let val (funs',str,vars) = nuprl2lisp2_ref_term check funs ind libop sub t
	in (funs', "(make-inr :val " ^ str ^ ")",vars)
	end
      | MIN_TERM t =>
	let val (funs',str,vars) = nuprl2lisp2_ref_term check funs ind libop sub t
	in (funs', "(- " ^ str ^ ")", vars)
	end
      | FIX_TERM t =>
	let val f = rterm2term t
	in if is_nuprl_lambda_term f
	   then let val (r,g) = dest_lambda 0 f
		    val v     = nuprl2lisp_var r
		    val sub'  = SUB.insert (sub, dest_nuprl_var r, "(funcall " ^ v ^ ")")
		    val (funs',str,vars) = nuprl2lisp2_term check funs ind libop sub' g
		    val b     = isvar  vars v
		    val vars' = remvar vars v
		in (funs',
		    "(fix (lambda ("
		    ^ v
		    ^ ")"
		    ^ (if b then "" else "\n(declare (ignore " ^ v ^ "))")
		    ^ "\n"
		    ^ str
		    ^ "))",
		    vars')
		end
	   else raise Fail "nuprl2lisp2_term:FIX_TERM:non_lambda"
	end
      | LAM_TERM (x, f) =>
	let val vx    = dest_nuprl_var x
	    val v     = nuprl2lisp_var x
	    val sub'  = if false
			then (#1 (SUB.remove (sub, vx))) handle _ => sub
			else SUB.insert (sub, vx, "(funcall " ^ v ^ ")")
	    val (funs',str,vars) = nuprl2lisp2_ref_term check funs ind libop sub' f
	    val b     = isvar  vars v
	    val vars' = remvar vars v
	in (funs',
	    "(lambda ("
	    ^ v
	    ^ ")"
	    ^ (if b then "" else "\n(declare (ignore " ^ v ^ "))")
	    ^ "\n"
	    ^ str
	    ^ ")",
	    vars')
	end
      | APP_TERM (f, a) =>
	let val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub f
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub a
	    val str =
		"(funcall "
		^ str1
		^ "\n"
		^ (if false
		   then str2
		   else "(lambda () " ^ str2 ^ ")")
		^ ")"
	in (funs2, str, uvars vars1 vars2)
	end
      | REC_TERM (t, e) => (funs, "\"REC\"", emvars) (*raise Fail "nuprl2lisp2_term:REC_TERM"*)
      | WAI_TERM (t, e) => raise Fail "nuprl2lisp2_term:WAI_TERM"
      | PAI_TERM (x, y) =>
	let val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub x
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub y
	    val str =
		"(make-pair :fst "
		^ str1
		^ " :snd "
		^ str2
		^ ")"
	in (funs2, str, uvars vars1 vars2)
	end
      | ADD_TERM (x, y) =>
	let val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub x
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub y
	    val str =
		"(+ "
		^ str1
		^ " "
		^ str2
		^ ")"
	in (funs2, str, uvars vars1 vars2)
	end
      | SUB_TERM (x, y) =>
	let val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub x
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub y
	    val str =
		"(- "
		^ str1
		^ " "
		^ str2
		^ ")"
	in (funs2, str, uvars vars1 vars2)
	end
      | MUL_TERM (x, y) =>
	let val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub x
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub y
	    val str =
		"(* "
		^ str1
		^ " "
		^ str2
		^ ")"
	in (funs2, str, uvars vars1 vars2)
	end
      | DIV_TERM (x, y) =>
	let val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub x
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub y
	    val str =
		"(/ "
		^ str1
		^ " "
		^ str2
		^ ")"
	in (funs2, str, uvars vars1 vars2)
	end
      | REM_TERM (x, y) => raise Fail "nuprl2lisp2_term:REM_TERM"
      | EQT_TERM (x, y) => raise Fail "nuprl2lisp2_term:EQT_TERM:deprecated"
      | UNI_TERM (x, y) => (funs, "\"UNION\"", emvars) (*raise Fail "nuprl2lisp2_term:UNI_TERM"*)
      | EQU_TERM (a, rterm1, rterm2) => (funs, "\"EQUAL\"", emvars) (*raise Fail "nuprl2lisp2_term:EQU_TERM"*)
      | IAX_TERM (a, rterm1, rterm2) =>
	let val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub a
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub rterm1
	    val (funs3,str3,vars3) = nuprl2lisp2_ref_term check funs2 ind libop sub rterm2
	    val str =
		"(if (axiom-p "
		^ str1
		^ ")"
		^ "\n"
		^ str2
		^ "\n"
		^ str3
		^ ")"
	in (funs3, str, uvars vars1 (uvars vars2 vars3))
	end
      | IPA_TERM (a, rterm1, rterm2) =>
	let val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub a
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub rterm1
	    val (funs3,str3,vars3) = nuprl2lisp2_ref_term check funs2 ind libop sub rterm2
	    val str =
		"(if (pair-p "
		^ str1
		^ ")"
		^ "\n"
		^ str2
		^ "\n"
		^ str3
		^ ")"
	in (funs3, str, uvars vars1 (uvars vars2 vars3))
	end
      | IIR_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp2_term:IIR_TERM"
      | IIL_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp2_term:IIL_TERM"
      | IIN_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp2_term:IIN_TERM"
      | ILA_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp2_term:ILA_TERM"
      | IAT_TERM (a, rterm1, rterm2) => raise Fail "nuprl2lisp2_term:IAT_TERM"
      | CBV_TERM (arg, x, B) =>
	let val sub'   = (#1 (SUB.remove (sub, dest_nuprl_var x))) handle _ => sub
	    val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub arg
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub' B
	    val v      = nuprl2lisp_var x
	    val b      = isvar  vars2 v
	    val vars2' = remvar vars2 v
	    val str    =
		"(let (("
		^ v
		^ " "
		^ str1
		^ "))"
		^ (if b then "" else "\n(declare (ignore " ^ v ^ "))")
		^ "\n"
		^ str2
		^ ")"
	in (funs2, str, uvars vars1 vars2')
	end
      | CBA_TERM (arg, x, B) =>
	let val sub'   = (#1 (SUB.remove (sub, dest_nuprl_var x))) handle _ => sub
	    val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub arg
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub' B
	    val v      = nuprl2lisp_var x
	    val b      = isvar  vars2 v
	    val vars2' = remvar vars2 v
	    val str    =
		"(let (("
		^ v
		^ " "
		^ str1
		^ "))"
		^ (if b then "" else "\n(declare (ignore " ^ v ^ "))")
		^ "\n"
		^ str2
		^ ")"
	in (funs2, str, uvars vars1 vars2')
	end
      | FUN_TERM (arg, x, B) => (funs, "\"FUNCTION\"", emvars) (*raise Fail "nuprl2lisp2_term:FUN_TERM"*)
      | PRD_TERM (arg, x, B) => (funs, "\"PRODUCT\"", emvars) (*raise Fail "nuprl2lisp2_term:PRD_TERM"*)
      | TUN_TERM (arg, x, B) => (funs, "\"TUNION\"", emvars) (*raise Fail "nuprl2lisp2_term:TUN_TERM"*)
      | SET_TERM (arg, x, B) => (funs, "\"SET\"", emvars) (*raise Fail "nuprl2lisp2_term:SET_TERM"*)
      | LES_TERM (a, b, rterm1, rterm2) =>
	let val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub a
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub b
	    val (funs3,str3,vars3) = nuprl2lisp2_ref_term check funs2 ind libop sub rterm1
	    val (funs4,str4,vars4) = nuprl2lisp2_ref_term check funs3 ind libop sub rterm2
	    val str =
		"(if (< "
		^ str1
		^ " "
		^ str2
		^ ")"
		^ "\n"
		^ str3
		^ "\n"
		^ str4
		^ ")"
	in (funs4, str, uvars vars1 (uvars vars2 (uvars vars3 vars4)))
	end
      | IEQ_TERM (a, b, rterm1, rterm2) =>
	let val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub a
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub b
	    val (funs3,str3,vars3) = nuprl2lisp2_ref_term check funs2 ind libop sub rterm1
	    val (funs4,str4,vars4) = nuprl2lisp2_ref_term check funs3 ind libop sub rterm2
	    val str =
		"(if (= "
		^ str1
		^ " "
		^ str2
		^ ")"
		^ "\n"
		^ str3
		^ "\n"
		^ str4
		^ ")"
	in (funs4, str, uvars vars1 (uvars vars2 (uvars vars3 vars4)))
	end
      | SPR_TERM (pair, var1, var2, rterm) =>
	let val v1   = nuprl2lisp_var var1
	    val v2   = nuprl2lisp_var var2
	    val sub1 = (#1 (SUB.remove (sub,  dest_nuprl_var var1))) handle _ => sub
	    val sub2 = (#1 (SUB.remove (sub1, dest_nuprl_var var2))) handle _ => sub1
	    val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub  pair
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub2 rterm
	    val b1     = isvar vars2 v1
	    val b2     = isvar vars2 v2
	    val vars2' = remvar (remvar vars2 v1) v2
	    val str    =
		"(let ((pair " (* first check that 'pair' is not already bound *)
		^ str1
		^ "))"
		^ ifCheck check "(if (pair-p pair)"
		^ "\n"
		^ "(let ("
		^ "(" ^ v1 ^ " (pair-fst pair))"
		^ "\n"
		^ "(" ^ v2 ^ " (pair-snd pair))"
		^ ")"
		^ (if b1 then "" else "\n(declare (ignore " ^ v1 ^ "))")
		^ (if b2 then "" else "\n(declare (ignore " ^ v2 ^ "))")
		^ "\n"
		^ str2
		^ ")"
		^ ifCheck check "(error \"spread:a_pair_should_be_a_list_of_length_2\"))"
		^ ")"
	in (funs2, str, uvars vars1 vars2')
	end
      | AEQ_TERM (n, a, b, rterm1, rterm2) =>
	let val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub a
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub b
	    val (funs3,str3,vars3) = nuprl2lisp2_ref_term check funs2 ind libop sub rterm1
	    val (funs4,str4,vars4) = nuprl2lisp2_ref_term check funs3 ind libop sub rterm2
	    val str =
		"(if (string= "
		^ str1
		^ " "
		^ str2
		^ ")"
		^ "\n"
		^ str3
		^ "\n"
		^ str4
		^ ")"
	in (funs4, str, uvars vars1 (uvars vars2 (uvars vars3 vars4)))
	end
      | DEC_TERM (dec, var1, rterm1, var2, rterm2) =>
	let val sub1   = (#1 (SUB.remove (sub, dest_nuprl_var var1))) handle _ => sub
	    val sub2   = (#1 (SUB.remove (sub, dest_nuprl_var var2))) handle _ => sub
	    val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub  dec
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub1 rterm1
	    val (funs3,str3,vars3) = nuprl2lisp2_ref_term check funs2 ind libop sub2 rterm2
	    val v1     = nuprl2lisp_var var1
	    val v2     = nuprl2lisp_var var2
	    val b1     = isvar vars2 v1
	    val b2     = isvar vars3 v2
	    val vars2' = remvar vars2 v1
	    val vars3' = remvar vars3 v2
	    val str    =
		"(let ((dec " (* first check that 'dec' is not already bound *)
		^ str1
		^ "))"
		^ "\n"
		^ "(if (inl-p dec)"
		^ "\n"
		^ (if b1
		   then "(let ((" ^ v1 ^ " (inl-val dec)))\n"
		   else "")
		^ str2
		^ (if b1 then ")" else "")
		^ "\n"
		^ "(if (inr-p dec)"
		^ "\n"
		^ (if b2
		   then "(let ((" ^ v2 ^ " (inr-val dec)))\n"
		   else "")
		^ str3
		^ (if b2 then ")" else "")
		^ "\n"
		^ "(error \"decide:an_injection_should_be_a_list_with_header_inl_or_inr\"))))"
	in (funs3, str, uvars vars1 (uvars vars2' vars3'))
	end
      | IND_TERM _ => raise Fail "nuprl2lisp2:IND_TERM"
      | CLO_TERM _ => raise Fail "nuprl2lisp2:CLO_TERM"
      | TERM ((("ifthenelse", tag), params), [B_TERM ([], a), B_TERM ([], b), B_TERM ([], c)]) =>
	let val (funs1,str1,vars1) = nuprl2lisp2_ref_term check funs  ind libop sub a
	    val (funs2,str2,vars2) = nuprl2lisp2_ref_term check funs1 ind libop sub b
	    val (funs3,str3,vars3) = nuprl2lisp2_ref_term check funs2 ind libop sub c
	    val str =
		"(if "
		^ "(inl-p "
		^ str1
		^ ")"
		^ "\n"
		^ str2
		^ "\n"
		^ str3
		^ ")"
	in (funs3, str, uvars vars1 (uvars vars2 vars3))
	end
      | TERM (((opid, tag), params), sterms) =>
	(case libop of
	     SOME (lib as {abs,tof}) =>
	     if List.exists
		    (fn x => x = opid)
		    [ "unit"      , "cons"   , "nil"       , "it"
		    , "empty-bag" , "id-deq" , "atom2-deq"
		    (*, "bfalse" , "single-bag" , "btrue" , "base-class-program"
		   , "hdf-halt" , "make-Msg" , "mk-msg-interface" , "make-msg-interface"*)
		    ]
	     then let val term' = unfold_all lib term
		      (*val _ = print ("[unfolding all " ^ opid ^ "]\n")*)
		  in if is_sml_primitive term'
		     then nuprl2lisp2_term check funs ind libop sub term'
		     else raise Fail ("nuprl2lisp2:not_primitive:"
				      ^ opid
				      ^ "("
				      ^ Int.toString (List.length params)
				      ^ "-"
				      ^ Int.toString (List.length sterms)
				      ^ ")")
		  end
	     else let val sign = getSignature term
		  in if simpleSignature sign
		     then let val rterms = subterms term
			      val (funs1,strs,vars) =
				  foldl (fn (rterm,(funs,strs,vars)) =>
					    let val (funs',str,vars') = nuprl2lisp2_ref_term check funs ind libop sub rterm
					    in (funs', strs @ [str], uvars vars vars')
					    end)
					(funs,[],emvars)
					rterms
			      val funs2 =
				  if is_a_fun funs1 opid
				  then funs1
				  else let val {id,sign,obid,lhs,rhs,wfs} = find_sign abs term
					   val args =
					       map (fn rt => dest_variable (rterm2term rt))
						   (subterms (rterm2term lhs))
					   val nid = nuprl2lisp_var' id
					   val sub = SUB.empty
				       in nuprl2lisp2_aux check funs1 id nid args libop sub (rterm2term rhs)
				       end
			  in (funs2, app2lisp true opid strs, vars)
			  end
		     else let val {id,sign,obid,lhs,rhs,wfs} = find_sign abs term
			      (*val _ = print ("[unfolding " ^ id ^ " (" ^ sign_to_string sign ^ ")]\n")*)
			      val term' = unfold_ab' term (rterm2term lhs) (rterm2term rhs)
			      (*val _ = print ("[new opid: " ^ opid_of_term term' ^ "]\n")*)
			  in nuprl2lisp2_term check funs ind libop sub term'
			  end
		  end
	   | NONE =>
	     raise Fail ("nuprl2lisp2:no_lib:"
			 ^ opid
			 ^ "("
			 ^ Int.toString (List.length params)
			 ^ "-"
			 ^ Int.toString (List.length sterms)
			 ^ ")"))

and nuprl2lisp2_ref_term check funs ind lib sub rterm =
    nuprl2lisp2_term check funs ind lib sub (rterm2term rterm)

and nuprl2lisp2_aux check funs name nname args libop sub term =
    let	val ind = "  "
	val (funs1,str,vars) = nuprl2lisp2_term check funs ind libop sub term
	val newfun =
	    "(defun "
	    ^ nname
	    ^ " ("
	    ^ T.fmt {init  = "",
		     final = "",
		     sep   = " ",
		     fmt   = nuprl2lisp_var'} args
	    ^ ")\n"
	    ^ foldr
		  (fn (arg, str) =>
		      let val v = nuprl2lisp_var' arg
 		      in if isvar vars v
			 then str
			 else "(declare (ignore "
			      ^ v
			      ^ "))\n"
			      ^ str
		      end) "" args
	    ^ str
	    ^ ")"
    in funs1 @ [(name,newfun)]
    end

fun nuprl2lisp2 lib unfold term =
    let val term' = if unfold then unfold_all lib term else term
	val libop = if unfold then NONE else SOME lib
	val check = false
	val id    = "main"
	val nid   = "main"
	val sub   = SUB.empty
	val funs0 = []
	val args  = []
	val funs1 = nuprl2lisp2_aux check funs0 id nid args libop sub term'
	val prelude =
	    "(defstruct pair fst snd)"
	    ^ "\n"
	    ^ "(defstruct inl val)"
	    ^ "\n"
	    ^ "(defstruct inr val)"
	    ^ "\n"
	    ^ "(defstruct axiom)"
	    ^ "\n"
	    ^ "(defun fix (f) (funcall f (lambda () (fix f))))"
	    ^ "\n"
    in (*prelude ^*)
	foldr
	    (fn ((opid,funcstr),str) => funcstr ^ "\n\n" ^ str)
	    ""
	    funs1
    end


(*
(* ---- *)

(* -- meaning -- *)
datatype ('a, 'b) union = Inl of 'a | Inr of 'b

fun nuprl_eval_var var env =
    case MAP.find (env, var) of
	SOME term => term
      | NONE => raise Fail ("nuprl_eval_var:" ^ var)

fun nuprl_eval_app f a env = (f env) (a env)

fun nuprl_eval_lam x f env =
 fn v => f (MAP.insert (env, x, v))

fun nuprl_eval_inl x env = Inl (x env)

fun nuprl_eval_inr x env = Inr (x env)

fun nuprl_eval_fix f env =
    let fun g () = f env (g ())
    in g ()
    end

fun nuprl_eval_pair a b env = [a env, b env]

fun nuprl_eval_axiom env = []

fun nuprl_eval_bottom env =
    let fun f () = f ()
    in f ()
    end

fun nuprl_eval_int env = "int"

fun nuprl_eval_atom env = "atom"

fun nuprl_eval_nat (n : II.int) env = n

fun nuprl_eval_isaxiom ax t1 t2 env =
    if null (ax env)
    then t1 env
    else t2 env

fun nuprl_eval_ispair pair t1 t2 env =
    if length (pair env) = 2
    then t1 env
    else t2 env

fun nuprl_eval_int_eq a b t1 t2 env =
    if a env = b env
    then t1 env
    else t2 env

fun nuprl_eval_atom_eq a b t1 t2 env =
    if a env = b env
    then t1 env
    else t2 env

fun nuprl_eval_spread pair v1 v2 t env =
    case pair env of
	[a, b] => t (MAP.insert (MAP.insert (env, v1, a), v2, b))
      | _ => raise Fail "nuprl_eval_spread"

fun nuprl_eval_decide d v1 t1 v2 t2 env =
    case d env of
	Inl x => t1 (MAP.insert (env, v1, x))
      | Inr x => t2 (MAP.insert (env, v2, x))

fun nuprl_eval_callbyvalueall arg x t env =
    t (MAP.insert (env, x, arg env))

fun nuprl_eval_add n1 n2 env = (n1 env) + (n2 env)

fun nuprl_eval_divide n1 n2 env = (n1 env) / (n2 env)

fun nuprl_eval_token (t : string) env = t

fun nuprl_eval_eq_term t1 t2 env =
    if (t2 env = t2 env)
    then Inl []
    else Inr []

fun nuprl_eval_type (t : string) env = t

fun nuprl_eval_eq_term a b t1 t2 env =
    if (a env = b env)
    then t1 env
    else t2 env

(* -- interpretation -- *)
fun nuprl_eval_interp_var' var =
    "\"v" ^ (String.map (fn #"-" => #"_"
			  | #"@" => #"_"
			  | c => c)
			var) ^ "\""

fun nuprl_eval_interp_var var = nuprl_eval_interp_var' (dest_nuprl_var var)

fun nuprl_eval_interp term =
    case term of
	VAR_TERM var => "(nuprl_eval_var " ^ nuprl_eval_interp_var var ^ ")"
      | LAM_TERM (var, body) =>
	let val str = nuprl_eval_interp (rterm2term body)
	in "(nuprl_eval_lam\n"
	   ^ nuprl_eval_interp_var var
	   ^ "\n"
	   ^ str
	   ^ ")"
	end
      | APP_TERM (f, a) =>
	let val str1 = nuprl_eval_interp (rterm2term f)
	    val str2 = nuprl_eval_interp (rterm2term a)
	in "(nuprl_eval_app\n"
	   ^ str1
	   ^ "\n"
	   ^ str2
	   ^ ")"
	end
      | WAI_TERM _ => raise Fail "nuprl_eval_interp:WAI_TERM"
      | INL_TERM term =>
	let val str = nuprl_eval_interp (rterm2term term)
	in "(nuprl_eval_inl " ^ str ^ ")"
	end
      | INR_TERM term =>
	let val str = nuprl_eval_interp (rterm2term term)
	in "(nuprl_eval_inl " ^ str ^ ")"
	end
      | FIX_TERM term =>
	let val str = nuprl_eval_interp (rterm2term term)
	in "(nuprl_eval_fix " ^ str ^ ")"
	end
      | PAI_TERM (rterm1, rterm2) =>
	let val str1 = nuprl_eval_interp (rterm2term rterm1)
	    val str2 = nuprl_eval_interp (rterm2term rterm2)
	in "(nuprl_eval_pair\n"
	   ^ str1
	   ^ "\n"
	   ^ str2
	   ^ ")"
	end
      | AXM_TERM   => "nuprl_eval_axiom"
      | BOT_TERM   => "nuprl_eval_bottom"
      | INT_TERM   => "(nuprl_eval_type \"int\")"
      | DUM_TERM   => raise Fail "nuprl_eval_interp:DUM_TERM"
      | ATM_TERM _ => "(nuprl_eval_type \"atom\")"
      | NAT_TERM n =>
	let val str = II.toString n
	in "(nuprl_eval_nat " ^ str ^ ")"
	end
      | IAX_TERM (ax, rterm1, rterm2) =>
	let val str1 = nuprl_eval_interp (rterm2term ax)
	    val str2 = nuprl_eval_interp (rterm2term rterm1)
	    val str3 = nuprl_eval_interp (rterm2term rterm2)
	in "(nuprl_eval_isaxiom\n"
	   ^ str1
	   ^ "\n"
	   ^ str2
	   ^ "\n"
	   ^ str3
	   ^ ")"
	end
      | IPA_TERM (pair, rterm1, rterm2) =>
	let val str1 = nuprl_eval_interp (rterm2term pair)
	    val str2 = nuprl_eval_interp (rterm2term rterm1)
	    val str3 = nuprl_eval_interp (rterm2term rterm2)
	in "(nuprl_eval_ispair\n"
	   ^ str1
	   ^ "\n"
	   ^ str2
	   ^ "\n"
	   ^ str3
	   ^ ")"
	end
      | IEQ_TERM (a, b, rterm1, rterm2) =>
	let val str1 = nuprl_eval_interp (rterm2term a)
	    val str2 = nuprl_eval_interp (rterm2term b)
	    val str3 = nuprl_eval_interp (rterm2term rterm1)
	    val str4 = nuprl_eval_interp (rterm2term rterm2)
	in "(nuprl_eval_int_eq\n"
	   ^ str1
	   ^ "\n"
	   ^ str2
	   ^ "\n"
	   ^ str3
	   ^ "\n"
	   ^ str4
	   ^ ")"
	end
      | AEQ_TERM (n, a, b, rterm1, rterm2) =>
	let val str1 = nuprl_eval_interp (rterm2term a)
	    val str2 = nuprl_eval_interp (rterm2term b)
	    val str3 = nuprl_eval_interp (rterm2term rterm1)
	    val str4 = nuprl_eval_interp (rterm2term rterm2)
	in "(nuprl_eval_atom_eq\n"
	   ^ str1
	   ^ "\n"
	   ^ str2
	   ^ "\n"
	   ^ str3
	   ^ "\n"
	   ^ str4
	   ^ ")"
	end
      | SPR_TERM (pair, v1, v2, rterm) =>
	let val str1 = nuprl_eval_interp (rterm2term pair)
	    val str2 = nuprl_eval_interp (rterm2term rterm)
	in "(nuprl_eval_spread\n"
	   ^ str1
	   ^ "\n"
	   ^ nuprl_eval_interp_var v1
	   ^ "\n"
	   ^ nuprl_eval_interp_var v2
	   ^ "\n"
	   ^ str2
	   ^ ")"
	end
      | DEC_TERM (d, v1, rterm1, v2, rterm2) =>
	let val str1 = nuprl_eval_interp (rterm2term d)
	    val str2 = nuprl_eval_interp (rterm2term rterm1)
	    val str3 = nuprl_eval_interp (rterm2term rterm2)
	in "(nuprl_eval_decide\n"
	   ^ str1
	   ^ "\n"
	   ^ nuprl_eval_interp_var v1
	   ^ "\n"
	   ^ str2
	   ^ "\n"
	   ^ nuprl_eval_interp_var v2
	   ^ "\n"
	   ^ str3
	   ^ ")"
	end
      | CBA_TERM (arg, x, rterm) =>
	let val str1 = nuprl_eval_interp (rterm2term arg)
	    val str2 = nuprl_eval_interp (rterm2term rterm)
	in "(nuprl_eval_callbyvalueall\n"
	   ^ str1
	   ^ "\n"
	   ^ nuprl_eval_interp_var x
	   ^ "\n"
	   ^ str2
	   ^ ")"
	end
      | CLO_TERM _ =>  raise Fail "nuprl_eval_interp:CLO_TERM"
      | TERM ((("add", _), []), [B_TERM ([], x), B_TERM ([], y)]) =>
	let val str1 = nuprl_eval_interp (rterm2term x)
	    val str2 = nuprl_eval_interp (rterm2term y)
	in "(nuprl_eval_add\n"
	   ^ str1
	   ^ "\n"
	   ^ str2
	   ^ ")"
	end
      | TERM ((("divide", _), []), [B_TERM ([], x), B_TERM ([], y)]) =>
	let val str1 = nuprl_eval_interp (rterm2term x)
	    val str2 = nuprl_eval_interp (rterm2term y)
	in "(nuprl_eval_divide\n"
	   ^ str1
	   ^ "\n"
	   ^ str2
	   ^ ")"
	end
      | TERM ((("less", _), []), [B_TERM ([], a), B_TERM ([], b), B_TERM ([], c), B_TERM ([], d)]) =>
	let val str1 = nuprl_eval_interp (rterm2term a)
	    val str2 = nuprl_eval_interp (rterm2term b)
	    val str3 = nuprl_eval_interp (rterm2term c)
	    val str4 = nuprl_eval_interp (rterm2term d)
	in "(nuprl_eval_less\n"
	   ^ str1
	   ^ "\n"
	   ^ str2
	   ^ "\n"
	   ^ str3
	   ^ "\n"
	   ^ str4
	   ^ ")"
	end
      | TERM ((("atom_eq", _), _), [B_TERM ([], a), B_TERM ([], b), B_TERM ([], rterm1), B_TERM ([], rterm2)]) =>
	let val str1 = nuprl_eval_interp (rterm2term a)
	    val str2 = nuprl_eval_interp (rterm2term b)
	    val str3 = nuprl_eval_interp (rterm2term rterm1)
	    val str4 = nuprl_eval_interp (rterm2term rterm2)
	in "(nuprl_eval_atom_eq\n"
	   ^ str1
	   ^ "\n"
	   ^ str2
	   ^ "\n"
	   ^ str3
	   ^ "\n"
	   ^ str4
	   ^ ")"
	end
      | TERM ((("eq_term", _), _), [B_TERM ([], a), B_TERM ([], b)]) =>
	let val str1 = nuprl_eval_interp (rterm2term a)
	    val str2 = nuprl_eval_interp (rterm2term b)
	in "(nuprl_eval_eq_term\n"
	   ^ str1
	   ^ "\n"
	   ^ str2
	   ^ ")"
	end
      | TERM ((("product", _), _), [B_TERM ([], a), B_TERM ([""], b)]) =>
	let val str = ppTerm term
	in "(nuprl_eval_type \"" ^ str ^ "\")"
	end
      | TERM ((("union", _), _), [B_TERM ([], a), B_TERM ([], b)]) =>
	let val str = ppTerm term
	in "(nuprl_eval_type \"" ^ str ^ "\")"
	end
      | TERM ((("token", _), [(t,tkind)]), []) => "(nuprl_eval_token " ^ t ^ ")"
      | TERM _ => raise Fail ("nuprl_eval_interp:TERM(" ^ opid_of_term term ^ ")")
*)

fun tagless term =
    "val main =\n" (*^ nuprl_eval_interp term*)


end
