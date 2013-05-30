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
 *  o File name:   ToNuprl.sml
 *  o Description: Compiles EventML to Nuprl.
 *)


structure ToNuprl :> TONUPRL = struct

structure A  = Ast
structure E  = Env
structure D  = Deps
structure EN = Enum
structure T  = ListFormat
structure EH = LibBase
structure NT = NuprlTerms


val toStringTerm = NT.toStringTerm

(* ------ IDENTIFIERS ------ *)

structure IDS = A.IDS

fun get_all_ids_bterm (NT.B_TERM (variables, term)) =
    foldr (fn (nvar,set) => IDS.add (set, NT.dest_nuprl_var nvar))
	  (get_all_ids (NT.rterm2term term))
	  variables

and get_all_ids (term as NT.TERM (((opid, tag), params), bterms)) =
    foldr (fn (bterm, set) => IDS.union (set, get_all_ids_bterm bterm))
	  (if NT.is_nuprl_variable_term term
	   then IDS.add (IDS.singleton opid, NT.dest_variable term)
	   else IDS.singleton opid)
	  bterms
  | get_all_ids (term as NT.AXM_TERM) = IDS.empty
  | get_all_ids (term as NT.BOT_TERM) = IDS.empty
  | get_all_ids (term as NT.INT_TERM) = IDS.empty
  | get_all_ids (term as NT.VOI_TERM) = IDS.empty
  | get_all_ids (term as NT.DUM_TERM) = IDS.empty
  | get_all_ids (term as NT.ATM_TERM _) = IDS.empty
  | get_all_ids (term as NT.TOK_TERM _) = IDS.empty
  | get_all_ids (term as NT.NAT_TERM _) = IDS.empty
  | get_all_ids (term as NT.VAR_TERM var) =
    IDS.singleton (NT.dest_nuprl_var var)
  | get_all_ids (term as NT.INL_TERM rterm) = get_all_ids (NT.rterm2term rterm)
  | get_all_ids (term as NT.INR_TERM rterm) = get_all_ids (NT.rterm2term rterm)
  | get_all_ids (term as NT.FIX_TERM rterm) = get_all_ids (NT.rterm2term rterm)
  | get_all_ids (term as NT.MIN_TERM rterm) = get_all_ids (NT.rterm2term rterm)
  | get_all_ids (term as NT.LAM_TERM (var, rterm)) =
    IDS.add (get_all_ids (NT.rterm2term rterm), NT.dest_nuprl_var var)
  | get_all_ids (term as NT.REC_TERM (var, rterm)) =
    IDS.add (get_all_ids (NT.rterm2term rterm), NT.dest_nuprl_var var)
  | get_all_ids (term as NT.WAI_TERM (rterm1, rterm2)) =
    IDS.union (get_all_ids (NT.rterm2term rterm1), get_all_ids (NT.rterm2term rterm2))
  | get_all_ids (term as NT.APP_TERM (rterm1, rterm2)) =
    IDS.union (get_all_ids (NT.rterm2term rterm1), get_all_ids (NT.rterm2term rterm2))
  | get_all_ids (term as NT.PAI_TERM (rterm1, rterm2)) =
    IDS.union (get_all_ids (NT.rterm2term rterm1), get_all_ids (NT.rterm2term rterm2))
  | get_all_ids (term as NT.ADD_TERM (rterm1, rterm2)) =
    IDS.union (get_all_ids (NT.rterm2term rterm1), get_all_ids (NT.rterm2term rterm2))
  | get_all_ids (term as NT.SUB_TERM (rterm1, rterm2)) =
    IDS.union (get_all_ids (NT.rterm2term rterm1), get_all_ids (NT.rterm2term rterm2))
  | get_all_ids (term as NT.MUL_TERM (rterm1, rterm2)) =
    IDS.union (get_all_ids (NT.rterm2term rterm1), get_all_ids (NT.rterm2term rterm2))
  | get_all_ids (term as NT.DIV_TERM (rterm1, rterm2)) =
    IDS.union (get_all_ids (NT.rterm2term rterm1), get_all_ids (NT.rterm2term rterm2))
  | get_all_ids (term as NT.REM_TERM (rterm1, rterm2)) =
    IDS.union (get_all_ids (NT.rterm2term rterm1), get_all_ids (NT.rterm2term rterm2))
  | get_all_ids (term as NT.EQT_TERM (rterm1, rterm2)) =
    IDS.union (get_all_ids (NT.rterm2term rterm1), get_all_ids (NT.rterm2term rterm2))
  | get_all_ids (term as NT.UNI_TERM (rterm1, rterm2)) =
    IDS.union (get_all_ids (NT.rterm2term rterm1), get_all_ids (NT.rterm2term rterm2))
  | get_all_ids (term as NT.EQU_TERM (a, rterm1, rterm2)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term rterm1)
	val set3 = get_all_ids (NT.rterm2term rterm2)
    in IDS.union (set1, IDS.union (set2, set3))
    end
  | get_all_ids (term as NT.IAX_TERM (a, rterm1, rterm2)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term rterm1)
	val set3 = get_all_ids (NT.rterm2term rterm2)
    in IDS.union (set1, IDS.union (set2, set3))
    end
  | get_all_ids (term as NT.IPA_TERM (a, rterm1, rterm2)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term rterm1)
	val set3 = get_all_ids (NT.rterm2term rterm2)
    in IDS.union (set1, IDS.union (set2, set3))
    end
  | get_all_ids (term as NT.IIR_TERM (a, rterm1, rterm2)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term rterm1)
	val set3 = get_all_ids (NT.rterm2term rterm2)
    in IDS.union (set1, IDS.union (set2, set3))
    end
  | get_all_ids (term as NT.IIL_TERM (a, rterm1, rterm2)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term rterm1)
	val set3 = get_all_ids (NT.rterm2term rterm2)
    in IDS.union (set1, IDS.union (set2, set3))
    end
  | get_all_ids (term as NT.IIN_TERM (a, rterm1, rterm2)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term rterm1)
	val set3 = get_all_ids (NT.rterm2term rterm2)
    in IDS.union (set1, IDS.union (set2, set3))
    end
  | get_all_ids (term as NT.ILA_TERM (a, rterm1, rterm2)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term rterm1)
	val set3 = get_all_ids (NT.rterm2term rterm2)
    in IDS.union (set1, IDS.union (set2, set3))
    end
  | get_all_ids (term as NT.IAT_TERM (a, rterm1, rterm2)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term rterm1)
	val set3 = get_all_ids (NT.rterm2term rterm2)
    in IDS.union (set1, IDS.union (set2, set3))
    end
  | get_all_ids (term as NT.CBV_TERM (a, x, f)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term f)
    in IDS.add (IDS.union (set1, set2), NT.dest_nuprl_var x)
    end
  | get_all_ids (term as NT.CBA_TERM (a, x, f)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term f)
    in IDS.add (IDS.union (set1, set2), NT.dest_nuprl_var x)
    end
  | get_all_ids (term as NT.FUN_TERM (a, x, f)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term f)
    in IDS.add (IDS.union (set1, set2), NT.dest_nuprl_var x)
    end
  | get_all_ids (term as NT.PRD_TERM (a, x, f)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term f)
    in IDS.add (IDS.union (set1, set2), NT.dest_nuprl_var x)
    end
  | get_all_ids (term as NT.TUN_TERM (a, x, f)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term f)
    in IDS.add (IDS.union (set1, set2), NT.dest_nuprl_var x)
    end
  | get_all_ids (term as NT.SET_TERM (a, x, f)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term f)
    in IDS.add (IDS.union (set1, set2), NT.dest_nuprl_var x)
    end
  | get_all_ids (term as NT.LES_TERM (a, b, rterm1, rterm2)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term b)
	val set3 = get_all_ids (NT.rterm2term rterm1)
	val set4 = get_all_ids (NT.rterm2term rterm2)
    in IDS.union (set1, IDS.union (set2, IDS.union (set3, set4)))
    end
  | get_all_ids (term as NT.IEQ_TERM (a, b, rterm1, rterm2)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term b)
	val set3 = get_all_ids (NT.rterm2term rterm1)
	val set4 = get_all_ids (NT.rterm2term rterm2)
    in IDS.union (set1, IDS.union (set2, IDS.union (set3, set4)))
    end
  | get_all_ids (term as NT.SPR_TERM (pair, var1, var2, rterm)) =
    let val set1 = get_all_ids (NT.rterm2term pair)
	val set2 = get_all_ids (NT.rterm2term rterm)
    in IDS.addList (IDS.union (set1, set2), [NT.dest_nuprl_var var1, NT.dest_nuprl_var var2])
    end
  | get_all_ids (term as NT.AEQ_TERM (n, a, b, rterm1, rterm2)) =
    let val set1 = get_all_ids (NT.rterm2term a)
	val set2 = get_all_ids (NT.rterm2term b)
	val set3 = get_all_ids (NT.rterm2term rterm1)
	val set4 = get_all_ids (NT.rterm2term rterm2)
    in IDS.union (set1, IDS.union (set2, IDS.union (set3, set4)))
    end
  | get_all_ids (term as NT.DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    let val set1 = get_all_ids (NT.rterm2term dec)
	val set2 = get_all_ids (NT.rterm2term rterm1)
	val set3 = get_all_ids (NT.rterm2term rterm2)
    in IDS.addList (IDS.union (set1, IDS.union (set2, set3)), [NT.dest_nuprl_var var1, NT.dest_nuprl_var var2])
    end
  | get_all_ids (term as NT.IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) =
    let val set1 = get_all_ids (NT.rterm2term i)
	val set2 = get_all_ids (NT.rterm2term downcase)
	val set3 = get_all_ids (NT.rterm2term basecase)
	val set4 = get_all_ids (NT.rterm2term upcase)
    in IDS.addList (IDS.union (set1, IDS.union (set2, IDS.union (set3, set4))),
		    [NT.dest_nuprl_var x,
		     NT.dest_nuprl_var rd,
		     NT.dest_nuprl_var y,
		     NT.dest_nuprl_var ru])
    end
  | get_all_ids (term as NT.CLO_TERM clos) = raise Fail "get_all_ids:C_TERM"

fun get_all_ids_list lst =
    let val some_ids =
	    ["bag-map", "bag-eq", "lambda", "pair", "eclass",
	     "normal-locally-programmable", "programmable",
	     "variable", "apply"]
    in foldr (fn (term, set) => IDS.union (set, get_all_ids term))
	     (IDS.addList (IDS.empty, some_ids))
	     lst
    end


(* ------ CONSTRUCTORS ------ *)

fun mk_nuprl_eta1_term v term =
    let val set = get_all_ids_list [term]
	val id  = A.newIdIdSet set v
	val nid = NT.mk_variable_term id
	val app = NT.mk_apply_term term nid
    in NT.mk_lambda_term id term
    end

fun mk_nuprl_eta2_term v1 v2 term =
    let val set  = get_all_ids_list [term]
	val id1  = A.newIdIdSet set v1
	val id2  = A.newIdIdSet set v2
	val nid1 = NT.mk_variable_term id1
	val nid2 = NT.mk_variable_term id2
	val app1 = NT.mk_apply_term term nid1
	val app2 = NT.mk_apply_term app1 nid2
	val lam1 = NT.mk_lambda_term id2 app2
	val lam2 = NT.mk_lambda_term id1 lam1
    in lam2
    end

fun mk_nuprl_bag_deq_term deq =
    let val set  = get_all_ids_list [deq]
	val id1  = A.newIdIdSet set "x"
	val id2  = A.newIdIdSet set "y"
	val nid1 = NT.mk_variable_term id1
	val nid2 = NT.mk_variable_term id2
	val bdeq = NT.mk_nuprl_simple_term "bag-eq"  [deq, nid1, nid2]
	val lam1 = NT.mk_lambda_term id2 bdeq
	val lam2 = NT.mk_lambda_term id1 lam1
    in lam2
    end


(* ------ EXPORT E# TO NUPRL ------ *)

val minStr = "this term should have been transformed"

(* We have this environment as a reference to avoid passing it around in ToNuprl. *)
val unifenv = ref (EN.initSolved ())
fun setUnifEnv x  = unifenv := x
fun getUnifEnv () = !unifenv

(* This map is used to record the parameters used in the declarared function.
 * The number of parameters used in a function is then used to define the arity
 * of the function. *)
structure CONS = BinaryMapFn(type ord_key = string val compare = String.compare)

(* userFP stands for 'user functions with their parameters' *)
val userFP = ref (CONS.empty : string list CONS.map)
fun addUserFP id params = userFP := CONS.insert (!userFP, id, params)
fun getUserFP isbound id =
    if isbound id
    then NONE
    else CONS.find (!userFP, id)
fun resetUserFP () = userFP := CONS.empty

(* This set is used to record the parameters added to the declarations' bodies *)
structure DECFP = BinarySetFn(type ord_key = string val compare = String.compare)

val decFP = ref DECFP.empty
fun addDecFP param = decFP := DECFP.add (!decFP, param)
fun addDecFPList params = decFP := DECFP.addList (!decFP, params)
fun getDecFP () = DECFP.listItems (!decFP)
fun isDecFP id = DECFP.member(!decFP, id)
fun resetDecFP () = decFP := DECFP.empty

(*
(* This set is used to record the type parameters used in declarations' types. *)
structure PTYPES = BinarySetFn(type ord_key = string val compare = String.compare)

val ptypes = ref PTYPES.empty
fun addPType tparam = ptypes := PTYPES.add (!ptypes, tparam)
fun getPTypes () = PTYPES.listItems (!ptypes)
fun resetPTypes () = ptypes := PTYPES.empty
*)

(*
(* This set is used to record type parameters. *)
structure TPARAMS = BinarySetFn(type ord_key = string val compare = String.compare)

val tparams = ref TPARAMS.empty
fun addTParam tparam = tparams := TPARAMS.add (!tparams, tparam)
fun isTParam  tparam = TPARAMS.member (!tparams, tparam)
fun resetTParams  () = tparams := TPARAMS.empty
*)

(* Object id of the esharp file within Nuprl. *)
val obidref = ref ""
fun setObid obid = obidref := obid
fun getObid () = !obidref
fun resetObid () = obidref := ""

(* Prefix to use for the generated Nuprl objects. *)
val prefref = ref ""
fun setPrefix pref = prefref := pref
fun getPrefix () = !prefref
fun resetPrefix () = prefref := ""

(* header function type *)
val header_function_type : string option ref = ref NONE
fun set_header_function_type str = (header_function_type := SOME str)
fun get_header_function_type () = !header_function_type
fun reset_header_function_type () = (header_function_type := NONE)

fun get_user_name opid =
    let val pref = getPrefix ()
    in if pref = ""
       then opid
       else pref ^ "_" ^ opid
    end

fun mk_nuprl_user_term_lvlop opid terms lvlop =
    let val obid    = getObid ()
	val name    = get_user_name opid
	val bterms  = map (fn t => ([], t)) terms
	val params1 = if obid = "" then [] else [NT.mk_nuprl_obid_parameter obid]
	val params2 = case lvlop of SOME i => [NT.mk_nuprl_level_exp_parameter i] | NONE => []
    in  NT.mk_nuprl_term (name, params1 @ params2) bterms
    end

fun mk_nuprl_user_term opid terms = mk_nuprl_user_term_lvlop opid terms NONE

fun reset_but_obid () =
    (resetUserFP  ();
     resetDecFP   ();
     resetPrefix  ();
     reset_header_function_type ();
     ())

fun reset () =
    (reset_but_obid ();
     resetObid      ();
     ())


(* ------ BEGIN CONTEXT ------ *)

structure IDENTS = BinarySetFn(type ord_key = string val compare = String.compare)
structure MAPID  = BinaryMapFn(type ord_key = string val compare = String.compare)

(* map identifier to their programs *)
type map_progs = (string * NT.nuprl_term) list

(* map files to programs *)
structure FPROGS = MAPID
(* string      : file name (base)
 * string list : parameters
 * term        : program *)
type map_fprog = (string * string list * NT.nuprl_term) FPROGS.map

(* set of bounds identifier *)
structure BOUNDS = IDENTS
type set_bounds = BOUNDS.set

(* This map is used to record the declared equality deciders *)
structure EQDEC = MAPID
type map_eqdec = string EQDEC.map

(* This map is used to record the arity of imported constants. *)
structure ARITY = MAPID
type map_arity = (string * (string * string list) list) ARITY.map

(* This map is used to record the defined parameters along with their types *)
structure PARAMS = MAPID
datatype param_kind = PK_PRM
		    | PK_SET of (string * string) option
		    | PK_MAP of (string * string) option
type map_params = (E.scheme * param_kind * (NT.nuprl_term * string list) option) PARAMS.map

(* This map is used to record the datatypes and their parameters *)
structure DATA = MAPID
type map_data = string list DATA.map

(* This map is used to record the invariants declared in a spec *)
structure INV = MAPID
type inv_rec = {invariant : string list,
		progress  : string list,
		ordering  : string list,
		memory    : string list}
type map_inv = inv_rec INV.map

(* set of declarations that are believed to be state machines
 * State-comb (and not Memory) *)
structure STATES = IDENTS
type set_states = STATES.set

(* set of exported classes *)
structure EXPORTS = IDENTS
type set_exports = EXPORTS.set

(* assumptions *)
structure HYPS = IDENTS
type set_hyps = HYPS.set

(* Set of functions that are automatically defined when defining an abstype.
 * For example, when defining
 *
 *     abstype Pair = Int * Int with binds
 *
 * Then we introduce:
 *    abs_Pair : Int * Int -> Pair, and
 *    rep_Pair : Pair -> Int * Int
 *
 * These functions are local to binds.
 * They are just the identity function.  So when we translate EML to Nuprl,
 * we want to try and get rid of the occurences of these functions.
 * For example: abs_Pair (a, b) -----> (a,b) *)
structure FABS = IDENTS
type set_fabs = FABS.set

(* This map is used to record declarations. *)
structure DECS = MAPID
type map_decs = NT.nuprl_term MAPID.map

type context =
     {progs   : map_progs,
      fprogs  : map_fprog,
      arity   : map_arity,
      params  : map_params,
      eqdec   : map_eqdec,
      bounds  : set_bounds,
      data    : map_data,
      ndefs   : NT.lib option,
      poly    : bool,
      extra   : bool,
      addpref : bool,
      base    : string,
      nlp     : bool,
      inv     : map_inv,
      typ     : bool,
      states  : set_states,
      exports : set_exports,
      hyps    : set_hyps,
      fabs    : set_fabs,
      decs    : map_decs,
      cbva    : bool,  (* True (False is default) to convert lets into cbva lets and not lazy ones *)
      newprog : bool,  (* True (default) to generate the new process constructors *)
      prop    : bool}  (* True (default) to extract the invariants, ordering, ... properties *)

val mk_empty_context : context =
    {progs   = [],
     fprogs  = FPROGS.empty,
     arity   = ARITY.empty,
     params  = PARAMS.empty,
     eqdec   = EQDEC.empty,
     bounds  = BOUNDS.empty,
     data    = DATA.empty,
     ndefs   = NONE,
     poly    = true,
     extra   = true,
     addpref = true,
     base    = "",
     nlp     = true,
     inv     = INV.empty,
     typ     = true,
     states  = STATES.empty,
     exports = EXPORTS.empty,
     hyps    = HYPS.empty,
     fabs    = FABS.empty,
     decs    = DECS.empty,
     cbva    = false,
     newprog = true,
     prop    = true}

fun mk_context progs   fprogs  arity   params  eqdec bounds  data
               ndefs   poly    extra   addpref base  nlp     inv
	       typ     states  exports hyps    fabs  decs    cbva
	       newprog prop =
    {progs   = progs,
     fprogs  = fprogs,
     arity   = arity,
     params  = params,
     eqdec   = eqdec,
     bounds  = bounds,
     data    = data,
     ndefs   = ndefs,
     poly    = poly,
     extra   = extra,
     addpref = addpref,
     base    = base,
     nlp     = nlp,
     inv     = inv,
     typ     = typ,
     states  = states,
     exports = exports,
     hyps    = hyps,
     fabs    = fabs,
     decs    = decs,
     cbva    = cbva,
     newprog = newprog,
     prop    = prop}

fun get_progs_context   (context : context) = #progs   context
fun get_fprogs_context  (context : context) = #fprogs  context
fun get_arity_context   (context : context) = #arity   context
fun get_params_context  (context : context) = #params  context
fun get_eqdec_context   (context : context) = #eqdec   context
fun get_bounds_context  (context : context) = #bounds  context
fun get_data_context    (context : context) = #data    context
fun get_ndefs_context   (context : context) = #ndefs   context
fun get_poly_context    (context : context) = #poly    context
fun get_extra_context   (context : context) = #extra   context
fun get_addpref_context (context : context) = #addpref context
fun get_base_context    (context : context) = #base    context
fun get_nlp_context     (context : context) = #nlp     context
fun get_inv_context     (context : context) = #inv     context
fun get_typ_context     (context : context) = #typ     context
fun get_states_context  (context : context) = #states  context
fun get_exports_context (context : context) = #exports context
fun get_hyps_context    (context : context) = #hyps    context
fun get_fabs_context    (context : context) = #fabs    context
fun get_decs_context    (context : context) = #decs    context
fun get_cbva_context    (context : context) = #cbva    context
fun get_newprog_context (context : context) = #newprog context
fun get_prop_context    (context : context) = #prop    context

fun set_progs_context   {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context x     fprogs arity params eqdec bounds data ndefs poly extra addpref base nlp inv typ states exports hyps fabs decs cbva newprog prop
fun set_fprogs_context  {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs x      arity params eqdec bounds data ndefs poly extra addpref base nlp inv typ states exports hyps fabs decs cbva newprog prop
fun set_arity_context   {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs x     params eqdec bounds data ndefs poly extra addpref base nlp inv typ states exports hyps fabs decs cbva newprog prop
fun set_params_context  {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity x      eqdec bounds data ndefs poly extra addpref base nlp inv typ states exports hyps fabs decs cbva newprog prop
fun set_eqdec_context   {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params x     bounds data ndefs poly extra addpref base nlp inv typ states exports hyps fabs decs cbva newprog prop
fun set_bounds_context  {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec x      data ndefs poly extra addpref base nlp inv typ states exports hyps fabs decs cbva newprog prop
fun set_data_context    {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds x    ndefs poly extra addpref base nlp inv typ states exports hyps fabs decs cbva newprog prop
fun set_ndefs_context   {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data x     poly extra addpref base nlp inv typ states exports hyps fabs decs cbva newprog prop
fun set_poly_context    {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs x    extra addpref base nlp inv typ states exports hyps fabs decs cbva newprog prop
fun set_extra_context   {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly x     addpref base nlp inv typ states exports hyps fabs decs cbva newprog prop
fun set_addpref_context {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly extra x       base nlp inv typ states exports hyps fabs decs cbva newprog prop
fun set_base_context    {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly extra addpref x    nlp inv typ states exports hyps fabs decs cbva newprog prop
fun set_nlp_context     {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly extra addpref base x   inv typ states exports hyps fabs decs cbva newprog prop
fun set_inv_context     {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly extra addpref base nlp x   typ states exports hyps fabs decs cbva newprog prop
fun set_typ_context     {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly extra addpref base nlp inv x   states exports hyps fabs decs cbva newprog prop
fun set_states_context  {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly extra addpref base nlp inv typ x      exports hyps fabs decs cbva newprog prop
fun set_exports_context {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly extra addpref base nlp inv typ states x       hyps fabs decs cbva newprog prop
fun set_hyps_context    {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly extra addpref base nlp inv typ states exports x    fabs decs cbva newprog prop
fun set_fabs_context    {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly extra addpref base nlp inv typ states exports hyps x    decs cbva newprog prop
fun set_decs_context    {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly extra addpref base nlp inv typ states exports hyps fabs x    cbva newprog prop
fun set_cbva_context    {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly extra addpref base nlp inv typ states exports hyps fabs decs x    newprog prop
fun set_newprog_context {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly extra addpref base nlp inv typ states exports hyps fabs decs cbva x       prop
fun set_prop_context    {progs, fprogs, arity, params, eqdec, bounds, data, ndefs, poly, extra, addpref, base, nlp, inv, typ, states, exports, hyps, fabs, decs, cbva, newprog, prop} x = mk_context progs fprogs arity params eqdec bounds data ndefs poly extra addpref base nlp inv typ states exports hyps fabs decs cbva newprog x

fun maybe_set_newprog_context context newprog =
    if newprog
    then set_newprog_context context newprog
    else context

fun add_prog_context context id prog =
    let val progs = (get_progs_context context) @ [(id, prog)]
    in set_progs_context context progs
    end

fun add_prog_op_context context id (SOME prog) _ = add_prog_context context id prog
  | add_prog_op_context context id NONE prog     = add_prog_context context id prog

fun add_prog_list_context context lst =
    foldl (fn ((id,prog), context) => add_prog_context context id prog) context lst

fun add_progs_context context progs =
    let val progs0 = get_progs_context context
    in set_progs_context context (progs0 @ progs)
    end

fun add_file_prog_context context _ NONE = context
  | add_file_prog_context context file (SOME (params, prog)) =
    let val {dir, file} = OS.Path.splitDirFile file
	val {base, ext} = OS.Path.splitBaseExt file
	val info   = (getPrefix (), params, prog)
	val fprogs = FPROGS.insert (get_fprogs_context context, base, info)
    in set_fprogs_context context fprogs
    end

fun get_fprog_context base context = FPROGS.find (get_fprogs_context context, base)

fun add_arity_context context id name ar =
    let val arity = ARITY.insert (get_arity_context context, id, (name, ar))
    in set_arity_context context arity
    end

fun add_param_context context param typ kind =
    let val params = PARAMS.insert (get_params_context context, param, (typ, kind, NONE))
    in set_params_context context params
    end

fun add_param_context' context param typ kind (exp,params) =
    let val params = PARAMS.insert (get_params_context context, param, (typ, kind, SOME (exp, params)))
    in set_params_context context params
    end

fun add_eqdec_context context tycon eq =
    let val eqdec = EQDEC.insert (get_eqdec_context context, tycon, eq)
    in set_eqdec_context context eqdec
    end

fun rm_eqdec_context context tycon =
    let val eqdec = #1 (EQDEC.remove (get_eqdec_context context, tycon))
    in set_eqdec_context context eqdec
    end handle _ => context

fun add_bound_context context id =
    let val bounds = BOUNDS.add (get_bounds_context context, id)
    in set_bounds_context context bounds
    end

fun add_bounds_context context ids =
    let val bounds = BOUNDS.addList (get_bounds_context context, ids)
    in set_bounds_context context bounds
    end

fun add_data_context context tycon prms =
    let val data = DATA.insert (get_data_context context, tycon, prms)
    in set_data_context context data
    end

fun is_bound_context context id = BOUNDS.member (get_bounds_context context, id)

val is_cbva_context    = get_cbva_context
val is_newprog_context = get_newprog_context
val is_prop_context    = get_prop_context

fun is_data_context context id = DATA.find (get_data_context context, id)

fun get_eqdec_context_op context tycon =
    EQDEC.find (get_eqdec_context context, tycon)

fun dest_record_acc name =
    let val lst =
	    String.tokens
		(fn #"'" => true | _ => false)
		name
    in case lst of
	   [record,acc] => SOME (record, acc)
	 | _ => NONE
    end

fun get_param_context context param =
    case PARAMS.find (get_params_context context, param) of
	SOME x => SOME x
      | NONE =>
	case dest_record_acc param of
	    SOME (record, acc) =>
	    (case get_param_context context record of
		 SOME (sch, PK_SET x, propop) => SOME (sch, PK_SET (SOME (record, acc)), propop)
	       | SOME (sch, PK_MAP x, propop) => SOME (sch, PK_MAP (SOME (record, acc)), propop)
	       | _ => NONE)
	  | NONE => NONE

fun get_arity_context_op context id =
    if is_bound_context context id
    then NONE
    else case ARITY.find (get_arity_context context, id) of
	     SOME x => SOME x
	   | NONE =>
	     case dest_record_acc id of
		 SOME (record, acc) =>
		 (case get_param_context context record of
		      SOME (_, PK_SET _, _) => get_arity_context_op context record
		    | SOME (_, PK_MAP _, _) => get_arity_context_op context record
		    | _ => NONE)
	       | NONE => NONE

fun get_all_params_context context =
    PARAMS.foldri (fn (i, _, list) => i :: list) [] (get_params_context context)

fun is_param_context context param =
    Option.isSome (get_param_context context param)

fun is_export_context context id =
    EXPORTS.member (get_exports_context context, id)

fun get_list_exports_context context =
    EXPORTS.listItems (get_exports_context context)

fun get_list_hyps_context context =
    HYPS.listItems (get_hyps_context context)

fun add_hyps_context context idents =
    let val hyps =
	    foldr (fn (hyp, context) => HYPS.add (context, hyp))
		  (get_hyps_context context)
		  idents
    in set_hyps_context context hyps
    end

fun add_fabs_context context id =
    let val fabs = FABS.add (get_fabs_context context, id)
    in set_fabs_context context fabs
    end

fun is_fabs_context context id = FABS.member (get_fabs_context context, id)

fun add_dec_context context id term =
    let val decs = DECS.insert (get_decs_context context, id, term)
    in set_decs_context context decs
    end

fun find_dec_context context id =
    DECS.find (get_decs_context context, id)

val empty_inv_entry =
    {invariant = [],
     ordering  = [],
     progress  = [],
     memory    = []}

fun add_invariant {invariant, ordering, progress, memory} x =  {invariant = x :: invariant, ordering = ordering,      progress = progress,      memory = memory}
fun add_ordering  {invariant, ordering, progress, memory} x =  {invariant = invariant,      ordering = x :: ordering, progress = progress,      memory = memory}
fun add_progress  {invariant, ordering, progress, memory} x =  {invariant = invariant,      ordering = ordering,      progress = x :: progress, memory = memory}
fun add_memory    {invariant, ordering, progress, memory} x =  {invariant = invariant,      ordering = ordering,      progress = progress,      memory = x :: memory}

fun add_gen_inv_context add context cls_name inv_name =
    let val inv    = get_inv_context context
	val entry  = Option.getOpt (INV.find (inv, cls_name), empty_inv_entry)
	val entry' = add entry inv_name
	val inv'   = INV.insert (inv, cls_name, entry')
    in set_inv_context context inv'
    end

val add_invariant_context = add_gen_inv_context add_invariant
val add_ordering_context  = add_gen_inv_context add_ordering
val add_progress_context  = add_gen_inv_context add_progress
val add_memory_context    = add_gen_inv_context add_memory

fun get_invariants context cls_name =
    case INV.find (get_inv_context context, cls_name) of
	SOME {invariant, ordering, progress, memory} => invariant
      | NONE => []

fun add_state_context context state =
    let val states = STATES.add (get_states_context context, state)
    in set_states_context context states
    end

fun reset_progs_context   context = set_progs_context   context []
fun reset_arity_context   context = set_arity_context   context ARITY.empty
fun reset_params_context  context = set_params_context  context PARAMS.empty
fun reset_eqdec_context   context = set_eqdec_context   context EQDEC.empty
fun reset_bounds_context  context = set_bounds_context  context BOUNDS.empty
fun reset_data_context    context = set_data_context    context DATA.empty
fun reset_states_context  context = set_states_context  context STATES.empty
fun reset_hyps_context    context = set_hyps_context    context IDENTS.empty
fun reset_fabs_context    context = set_fabs_context    context IDENTS.empty
fun reset_decs_context    context = set_decs_context    context DECS.empty
fun reset_cbva_context    context = set_cbva_context    context false
fun reset_newprog_context context = set_newprog_context context true
fun reset_prop_context    context = set_prop_context    context true

fun reset_params_arity_context context =
    let val arity =
	    foldr (fn (param, arity) =>
		      (#1 (ARITY.remove (arity, param)))
		      handle _ => arity)
		  (get_arity_context context)
		  (get_all_params_context context)
    (* got rid of the entries of arity that where in params *)
    in reset_params_context (set_arity_context context arity)
    end

fun reset_context context =
    (reset_progs_context
     (*o reset_arity_context*)
     (*o reset_params_context*)
     o reset_params_arity_context
     o reset_eqdec_context
     o reset_bounds_context
     o reset_data_context
     (*o reset_states_context*)
     o reset_hyps_context
     o reset_fabs_context
     o reset_decs_context)
	context

fun is_maybe_state_term term =
    let val (vars, t) = NT.dest_lambdas 2 term
    in NT.is_nuprl_term "State-comb" t
       orelse NT.is_nuprl_term "State-loc-comb" t
       orelse NT.is_nuprl_term "State1" t
       orelse NT.is_nuprl_term "State2" t
       orelse NT.is_nuprl_term "State3" t
       orelse NT.is_nuprl_term "State4" t
       orelse NT.is_nuprl_term "loop-class-state" t
       orelse NT.is_nuprl_term "state-class1" t
       orelse NT.is_nuprl_term "state-class2" t
       orelse NT.is_nuprl_term "state-class3" t
       orelse NT.is_nuprl_term "state-class4" t
    end

fun is_maybe_memory_term term =
    let val (vars, t) = NT.dest_lambdas 3 term
    in NT.is_nuprl_term "Memory-class" t
       orelse NT.is_nuprl_term "Memory-loc-class" t
       orelse NT.is_nuprl_term "Memory1" t
       orelse NT.is_nuprl_term "Memory2" t
       orelse NT.is_nuprl_term "Memory3" t
       orelse NT.is_nuprl_term "Memory4" t
       orelse NT.is_nuprl_term "loop-class-memory" t
       orelse NT.is_nuprl_term "memory-class1" t
       orelse NT.is_nuprl_term "memory-class2" t
       orelse NT.is_nuprl_term "memory-class3" t
       orelse NT.is_nuprl_term "memory-class4" t
    end

fun is_maybe_sm_term term =
    is_maybe_state_term term
    orelse
    is_maybe_memory_term term

fun add_maybe_state_context context id term =
    if is_maybe_state_term term
    then add_state_context context id
    else context

fun is_state_context context term =
    let val (t,args) = NT.dest_applies term
    in STATES.member (get_states_context context, NT.opid_of_term t)
    end


(* -- hiddent event ordering -- *)
val hidden_event_ordering = ref "es"
val hidden_msg_fun        = ref "f"

fun is_hidden x =
    x = !hidden_event_ordering
    orelse
    x = !hidden_msg_fun

fun set_hidden_eo context term =
    let val es    = A.newIdIdSet (A.getIdentsTerm term) "es"
	(*val _     = print ("[setting hidden event ordering: " ^ es ^ "]\n")*)
	val _     = hidden_event_ordering := es
	val bound = (E.ITVS.empty, E.ETVS.empty, E.OTVS.empty)
	val cssch = E.CSSCH ([], E.CSSUB [])
	val ity   = E.mk_type_eo D.dummy_label
	val ctxt  = add_param_context context es (E.VPOLY (bound, (cssch, ity))) PK_PRM
    in ctxt
    end

fun set_hidden_msg_fun context term =
    let val f     = A.newIdIdSet (A.getIdentsTerm term) "f"
	(*val _     = print ("[setting hidden message function: " ^ f ^ "]\n")*)
	val _     = hidden_msg_fun := f
	val bound = (E.ITVS.empty, E.ETVS.empty, E.OTVS.empty)
	val cssch = E.CSSCH ([], E.CSSUB [])
	val ity   = E.mk_type_msgfun D.dummy_label
	val ctxt  = add_param_context context f (E.VPOLY (bound, (cssch, ity))) PK_PRM
    in ctxt
    end

fun set_hiddens context term = set_hidden_msg_fun (set_hidden_eo context term) term

fun mk_nuprl_hidden_event_ordering () =
    let val es = !hidden_event_ordering
	val _  = addDecFP es
    in NT.mk_variable_term es
    end

fun mk_nuprl_hidden_msg_fun () =
    let val f = !hidden_msg_fun
	val params =
	    case get_header_function_type () of
		NONE => raise Fail "mk_nuprl_hidden_msg_fun:header_function_type_not_set"
	      | SOME typ_name =>
		case getUserFP (fn x => false) typ_name of
		    NONE => raise Fail "mk_nuprl_hidden_msg_fun:header_function_type_not_declared"
		  | SOME params => params
	val _ = addDecFP f
	val _ = addDecFPList params
    in NT.mk_variable_term f
    end


(* ------ END CONTEXT ------ *)

fun getArityArgs (A.N {kind = (A.ARGS, A.ARGS_EM),   label, value, regions, parents, children = []}) = []
  | getArityArgs (A.N {kind = (A.ARGS, A.ARGS_PSEQ), label, value, regions, parents, children})      =
    map (fn (A.N {kind = (A.ARG, A.ARG_A), label, value, regions, parents, children = [id, args, ty]}) =>
	    (case args of
		 A.N {kind = (A.ARGS, A.ARGS_EM), label, value, regions, parents, children = []} => (A.getIdIdent id, [])
	       | A.N {kind = (A.ARGS, A.ARGS_LSEQ), label, value, regions, parents, children}    =>
		 let val lst = map (fn (A.N {kind = (A.ARG, A.ARG_A), label, value, regions, parents, children = [id, args, ty]}) => A.getIdIdent id
				     | (A.N {kind = (A.ARG, A.ARG_T), label, value, regions, parents, children = [tv]}) => A.getIdIdent tv
				     | _ => raise Fail "impossible:unexpected term")
				   children
		 in (A.getIdIdent id, lst)
		 end
	       | _ => raise Fail "impossible:unexpected term")
	  | (A.N {kind = (A.ARG, A.ARG_T), label, value, regions, parents, children = [tv]}) =>
	    (A.getIdIdent tv, [])
	  | _ => raise Fail "impossible:unexpected term")
	children
  | getArityArgs _ = raise Fail "impossible:unexpected term"

fun getArityTyvarSeq (A.N {kind = (A.TYPEVARSEQ, A.TYPEVARSEQ_EM),  label, value, regions, parents, children}) = 0
  | getArityTyvarSeq (A.N {kind = (A.TYPEVARSEQ, A.TYPEVARSEQ_ONE), label, value, regions, parents, children}) = 1
  | getArityTyvarSeq (A.N {kind = (A.TYPEVARSEQ, A.TYPEVARSEQ_SEQ), label, value, regions, parents, children}) = List.length children
  | getArityTyvarSeq term = raise Fail "impossible:unexpected term"

fun flattenExpApp (A.N {kind = (A.EXP, A.EXP_APP), label, value, regions, parents, children = [exp, atexp]}) =
    let val (term, terms) = flattenExpApp exp
    in (term, terms @ [atexp])
    end
  | flattenExpApp term = (term, [])

fun toNuprlListAtoms [] = NT.mk_nuprl_nil_term
  | toNuprlListAtoms (x :: xs) =
    let val t = NT.mk_regular_token_term x
    in NT.mk_nuprl_cons_term t (toNuprlListAtoms xs)
    end

fun toAtomList atoms =
    case A.getKind atoms of
	(A.ATOMS, A.ATOMS_ATOMS) => toNuprlListAtoms (A.splitAtoms (A.getValue atoms))
      | _ => raise Fail "toAtomList"

fun getIdPat n pat =
    case A.getIdPat pat of
	SOME id => A.getIdIdent id
      | NONE =>
	(print ("--error(" ^ Int.toString n ^ ")\n" ^ A.toString pat ^ "\n");
	 raise Fail "impossible:getIdPat")

(* TODO: we don't need this function, we can just gather the free params, while
 * converting the EventML code, because anyway we make sure that the bound variables
 * don't clash with the parameter names. *)
fun getFreeParams context binders (A.N {kind = (A.ID, A.ID_VID), label, value, regions, parents, children = []}) =
    if List.exists (fn x => x = value) binders
    then []
    else (case get_param_context context value of
	      SOME (sch, PK_SET (SOME (r, s)), propop) => [r]
	    | SOME (sch, PK_MAP (SOME (r, s)), propop) => [r]
	    | SOME _ => [value]
	    | _ => [])
  | getFreeParams context binders (A.N {kind = (A.EXP, A.EXP_LET), label, value, regions, parents, children = [bind, exp]}) =
    (case bind of
	 A.N {kind = (A.BIND, A.BIND_DEC), label, value, regions, parents, children = [f, a, e]} =>
	 if A.termIsEmParam a
	 then let val fid = A.getIdIdent f
		  val params1 = getFreeParams context binders e
		  val params2 = getFreeParams context (fid :: binders) exp
	      in params1 @ params2
	      end
	 else raise Fail ("impossible:" ^ minStr)
       | A.N {kind = (A.BIND, A.BIND_PAT), label, value, regions, parents, children = [p, e]} =>
	 let val idents =
		 case p of
		     A.N {kind = (A.PAT, A.PAT_ATPAT), label, value, regions, parents, children = [atpat]} =>
		     (case atpat of
			  A.N {kind = (A.ATPAT, A.ATPAT_TUPLE), label, value, regions, parents, children} => map (getIdPat 1) children
			| A.N {kind = (A.ATPAT, A.ATPAT_ID), label, value, regions, parents, children = [id]} => [A.getIdIdent id]
			| term => (print (A.toString term ^ "\n"); raise Fail ("impossible:" ^ minStr)))
		   | _ => raise Fail ("impossible:" ^ minStr)
	     val params1 = getFreeParams context binders e
	     val params2 = getFreeParams context (idents @ binders) exp
	 in params1 @ params2
	 end
       | _ => raise Fail ("impossible:" ^ minStr))
  | getFreeParams context binders (A.N {kind = (A.EXP, A.EXP_CLASS), label, value, regions, parents, children = [bind, exp]}) = raise Fail ("impossible:" ^ minStr)
  | getFreeParams context binders (A.N {kind = (A.EXP, A.EXP_LETR), label, value, regions, parents, children = [bind, exp]}) =
    (case bind of
	 A.N {kind = (A.BIND, A.BIND_DEC), label, value, regions, parents, children = [f, a, e]} =>
	 if A.termIsEmParam a
	 then let val fid = A.getIdIdent f
		  val params1 = getFreeParams context (fid :: binders) exp
		  val params2 = getFreeParams context (fid :: binders) e
	      in params1 @ params2
	      end
	 else raise Fail ("impossible:" ^ minStr)
       | _ => raise Fail ("impossible:" ^ minStr))
  | getFreeParams context binders  (A.N {kind = (A.EXP, A.EXP_LAMBDA), label, value, regions, parents, children = [pat, exp]}) =
    getFreeParams context ((getIdPat 2 pat) :: binders) exp
  | getFreeParams context binders (A.N {kind = (A.EXP, A.EXP_BINDING), label, value, regions, parents, children = [exp1, pat, exp2]}) =
    let val params1 = getFreeParams context binders exp1
	val params2 = getFreeParams context ((getIdPat 3 pat) :: binders) exp2
    in params1 @ params2
    end
  | getFreeParams context binders (A.N {kind, label, value, regions, parents, children}) = List.concat (map (getFreeParams context binders) children)

structure BTV = BinaryMapFn(type ord_key = E.ityvar val compare = Int.compare)


(* ------ param kind functions ------ *)

fun mk_rec_sel_set record sel =
    let val r = NT.mk_variable_term record
    in case sel of
	   "set"       => NT.mk_nuprl_set_sig_set_term       r
	 | "member"    => NT.mk_nuprl_set_sig_member_term    r
	 | "empty"     => NT.mk_nuprl_set_sig_empty_term     r
	 | "isEmpty"   => NT.mk_nuprl_set_sig_isEmpty_term   r
	 | "singleton" => NT.mk_nuprl_set_sig_singleton_term r
	 | "add"       => NT.mk_nuprl_set_sig_add_term       r
	 | "union"     => NT.mk_nuprl_set_sig_union_term     r
	 | "remove"    => NT.mk_nuprl_set_sig_remove_term    r
	 | _  => raise Fail ("mk_rec_sel_set:" ^ sel)
    end

fun mk_rec_sel_map record sel =
    let val r = NT.mk_variable_term record
    in case sel of
	   "map"     => NT.mk_nuprl_map_sig_map_term     r
	 | "eqKey"   => NT.mk_nuprl_map_sig_eqKey_term   r
	 | "find"    => NT.mk_nuprl_map_sig_find_term    r
	 | "inDom"   => NT.mk_nuprl_map_sig_inDom_term   r
	 | "empty"   => NT.mk_nuprl_map_sig_empty_term   r
	 | "isEmpty" => NT.mk_nuprl_map_sig_isEmpty_term r
	 | "update"  => NT.mk_nuprl_map_sig_update_term  r
	 | "add"     => NT.mk_nuprl_map_sig_add_term     r
	 | "remove"  => NT.mk_nuprl_map_sig_remove_term  r
	 | _  => raise Fail ("mk_rec_sel_map:" ^ sel)
    end

fun param_kind_to_val (PK_SET (SOME (r, s))) param = r
  | param_kind_to_val (PK_MAP (SOME (r, s))) param = r
  | param_kind_to_val _ param = param

fun param_kind_to_kind context param =
    case get_param_context context param of
	SOME (sch, kind, propop) => kind
      | NONE => raise Fail "param_kind_to_kind"

fun mk_nuprl_param_kind (PK_SET (SOME (r, s))) param = mk_rec_sel_set r s
  | mk_nuprl_param_kind (PK_MAP (SOME (r, s))) param = mk_rec_sel_map r s
  | mk_nuprl_param_kind kind param = NT.mk_variable_term param

fun param_kind_to_nuprl context param =
    case get_param_context context param of
	SOME (sch, kind, propop) => mk_nuprl_param_kind kind param
      | NONE => raise Fail "param_kind_to_nuprl"

(* -- *)


(* - 'force' is to force the translation of unbound variables
 * - 'addctxt' is to add the type constructors that are also parameters to
 *   the context
 *)
fun toNuprlIty i force addctxt context vars (E.ITYVAR (ityvar, eq)) ityvarset etyvarset =
    (case BTV.find (ityvarset, ityvar) of
	 SOME str => NT.mk_variable_term str
       | NONE => if force
		 then NT.mk_variable_term (Int.toString ityvar)
		 else (print ("("  ^ Int.toString i ^ ")\n"); raise Fail "impossible:type variables are not allowed to be converted to nuprl"))
  (* We can have a type variable because we have a genuine generality or the
   * ityvar can come from a mono bind.  We might have to do more that recording
   * the type variable associated to a label.
   * We should be able to handle:
   *
   *     MSGS (``foo` : 'a List) ;;
   *     let m = MSG(``foo``, []) ;;
   *
   * But we don't. *)
  | toNuprlIty i force addctxt context vars (E.ITYETV (etyvar, lab)) ityvarset etyvarset =
    if E.isinETVS (etyvarset, etyvar)
    then NT.mk_variable_term etyvar
    else raise Fail "impossible:type variables are not allowed to be converted to nuprl "
  | toNuprlIty i force addctxt context vars (E.ITYCON (E.ITYSEQSEQ (itys, _), E.ITYCONNAM (tyconnam, _, _))) ityvarset etyvarset =
    let val n_itys = map (fn ity => toNuprlIty i force addctxt context vars ity ityvarset etyvarset) itys
    in case (tyconnam, n_itys) of
           ("*",         nt1 :: nt2 :: nts) => foldl (fn (nty, prd) => NT.mk_prod_term prd nty) (NT.mk_prod_term nt1 nt2) nts
	 | ("*",         []) => NT.mk_nuprl_unit_term
	 | ("->",        [nt1, nt2]) => NT.mk_fun_term nt1 nt2
	 | ("+",         [nt1, nt2]) => NT.mk_nuprl_union_term nt1 nt2
	 | ("Map",       [nt1, nt2]) => NT.mk_nuprl_map_sig_term "i" nt1 nt2
	 | ("Set",       [nt]) => NT.mk_nuprl_set_sig_term "i" nt
	 | ("List",      [nt]) => NT.mk_nuprl_list_term  nt
	 | ("Class",     [nt]) => NT.mk_nuprl_class_term (mk_nuprl_hidden_msg_fun ()) nt
	 | ("Bag",       [nt]) => NT.mk_nuprl_bag_term   nt
	 | ("Deq",       [nt]) => NT.mk_nuprl_deq_term   nt
	 | ("Int",       []) => NT.mk_int_term
	 | ("Bool",      []) => NT.mk_nuprl_bool_term
	 | ("Real",      []) => NT.mk_nuprl_real_term
	 | ("Msg",       []) => NT.mk_nuprl_msg_term (mk_nuprl_hidden_msg_fun ())
	 | ("Interface", []) => NT.mk_nuprl_interface_term (mk_nuprl_hidden_msg_fun ())
	 | ("Unit",      []) => NT.mk_nuprl_unit_term
	 | ("Tok",       []) => NT.mk_atom_term NONE
	 | ("Atom",      []) => NT.mk_atom_term NONE
	 | ("Type",      []) => NT.mk_nuprl_type_term "i"
	 | ("Prop",      []) => NT.mk_nuprl_prop_term "i"
	 | ("Event",     []) => NT.mk_nuprl_esE_term (mk_nuprl_hidden_event_ordering ())
	 | ("_EO",       []) => NT.mk_nuprl_event_orderingp_term "i" (mk_nuprl_hidden_msg_fun ())
	 | ("_MSGFUN",   []) =>
	   (case get_header_function_type () of
		NONE => raise Fail "_MSGFUN:header_function_type_not_set"
	      | SOME typ_name =>
		case getUserFP (is_bound_context context) typ_name of
		    NONE => raise Fail "_MSGFUN:header_function_type_not_declared"
		  | SOME params =>
		    let val aterms = map NT.mk_variable_term params
			val _      = addDecFPList params
			val lvlop  = SOME "i"
		    in mk_nuprl_user_term_lvlop typ_name aterms lvlop
		    end)
	 | ("Nat",       []) => NT.mk_nuprl_nat_term
	 | ("Loc",       []) => NT.mk_nuprl_loc_term
	 | (tycon, args) =>
	   (case is_data_context context tycon of
		SOME prms =>
		let val a_terms = map NT.mk_variable_term prms
		    val _ = if addctxt then addDecFPList prms else ()
		in if List.null args
		   then mk_nuprl_user_term tycon a_terms
		   else raise Fail "toNuprlIty"
		end
	      | NONE =>
		if IDENTS.member (vars, tycon)
		then if List.null args
		     then NT.mk_variable_term tycon
		     else raise Fail "toNuprlIty"
		else case get_param_context context tycon of
			 SOME (sch, kind, propop) =>
			 let val v = param_kind_to_val kind tycon
			     val _ = if addctxt then addDecFP v else ()
			 in foldl (fn (arg, term) => NT.mk_apply_term term arg)
				  (mk_nuprl_param_kind kind tycon)
				  args
			 end
		       | NONE => NT.mk_nuprl_simple_term tycon args)
    end
  | toNuprlIty i force addctxt context vars (E.ITYDEP (ity, _)) ityvarset etyvarset = toNuprlIty i force addctxt context vars ity ityvarset etyvarset
  | toNuprlIty i force addctxt context vars ity _ _ = (print ("*** (" ^ Int.toString i ^ ") " ^ E.toStringIty ity ^ "\n"); raise Fail "impossible:the type should be fully built up, no variable")

fun toNuprlIty_em i force context typ =
    let val addctxt = true
    in toNuprlIty i force addctxt context IDENTS.empty typ BTV.empty E.emptyETVS
    end

fun mk_deq_top () =
    NT.mk_lambda_term "a" (NT.mk_lambda_term "b" NT.mk_nuprl_btrue_term)

(* transforms a type into an equality decider *)
fun toDeq context (E.ITYVAR (ityvar, eq)) =
    raise Fail "impossible:type variables are not allowed to be converted to nuprl"
  | toDeq context (E.ITYETV (etyvar, lab)) =
    raise Fail "impossible:type variables are not allowed to be converted to nuprl "
  | toDeq context (E.ITYCON (E.ITYSEQSEQ (itys, _), E.ITYCONNAM (tyconnam, _, _))) =
    let val force = false
	fun toIty ity = toNuprlIty_em 0 force context (E.stripIty ity)
	val n_deqs = map (fn ity => (toIty ity, toDeq context ity)) itys
    in case (tyconnam, n_deqs) of
	   ("*", []) => NT.mk_nuprl_unit_deq_term
	 | ("*",     x :: y :: deqs) =>
	   (case List.rev n_deqs of
		(nty1, deq1) :: (nty2, deq2) :: deqs =>
		let val nty0 = NT.mk_prod_term nty2 nty1
		    val deq0 = NT.mk_nuprl_product_deq_term nty2 nty1 deq2 deq1
		    val (_, deq) =
			foldl (fn ((nty', deq'), (nty, deq)) =>
				  (NT.mk_prod_term nty' nty,
				   NT.mk_nuprl_proddeq_term deq' deq))
			      (nty0, deq0)
			      deqs
		in deq
		end
	      | _ => raise Fail "impossible:product type should have at least 2 arguments")
	 | ("->",        [(nty1, deq1), (nty2, deq2)]) => raise Fail "impossible:function type does not have an equality decider"
	 | ("+",         [(nty1, deq1), (nty2, deq2)]) => NT.mk_nuprl_union_deq_term nty1 nty2 deq1 deq2
	 | ("Class",     [(nty, deq)]) => raise Fail "impossible:Class does not have an equality decider"
	 | ("List",      [(nty, deq)]) => NT.mk_nuprl_list_deq_term deq
	 | ("Bag",       [(nty, deq)]) => mk_nuprl_bag_deq_term  deq
	 | ("Int",       []) => NT.mk_nuprl_int_deq_term
	 | ("Bool",      []) => NT.mk_nuprl_bool_deq_term
	 | ("Real",      []) => raise Fail "impossible:Real does not have an equality decider"
	 | ("Msg",       []) => raise Fail "impossible:Msg does not have an equality decider"
	 | ("Interface", []) => raise Fail "impossible:Interface does not have an equality decider"
	 | ("Unit",      []) => NT.mk_nuprl_unit_deq_term
	 | ("Tok",       []) => NT.mk_nuprl_atom_deq_term
	 | ("Atom",      []) => NT.mk_nuprl_atom_deq_term
	 | ("Type",      []) => raise Fail "impossible:Type does not have an equality decider"
	 | ("Prop",      []) => raise Fail "impossible:Type does not have an equality decider"
	 | ("Event",     []) => NT.mk_nuprl_es_eq_term (mk_nuprl_hidden_event_ordering ())
	 | ("Nat",       []) => NT.mk_nuprl_nat_deq_term
	 | ("Loc",       []) => NT.mk_nuprl_loc_deq_term
	 | (_, _) =>
	   if get_typ_context context
	   then case get_eqdec_context_op context tyconnam of
		    NONE =>
		    let val str = "no equality decider for " ^ tyconnam
			val _   = print (str ^ "\n")
		    in raise Fail ("impossible:" ^ str)
		    end
		  | SOME deq =>
		    if is_param_context context deq
		    then let val _ = addDecFPList [deq]
			 in NT.mk_nuprl_applies_term (NT.mk_variable_term deq) (map (fn (_, x) => x) n_deqs)
			 end
		    else NT.mk_nuprl_applies_term (NT.mk_nuprl_simple_term deq [])  (map (fn (_, x) => x) n_deqs)
	   else mk_deq_top ()
    end
  | toDeq context ity = (print ("*** " ^ E.toStringIty ity ^ "\n"); raise Fail "impossible:the type should be fully built up, no variable")

fun deq2eq term =
    let val (x, terms) = NT.dest_applies term
	fun aux t =
	    case (NT.opid_of_term t, terms) of
		("int-deq", [t1, t2]) => NT.mk_nuprl_eq_int_term  t1 t2
	      | ("id-deq",  [t1, t2]) => NT.mk_nuprl_eq_id_term   t1 t2
	      | ("es-eq",   [t1, t2]) =>
		let val es = NT.dest_es_eq t
		in NT.mk_nuprl_es_eq_E_term es t1 t2
		end
	      | _ => term
    in case (NT.opid_of_term x, terms) of
	   ("eqof", [t1, t2]) => aux (NT.dest_eqof x)
	 | _ => aux x
    end

(*
(* transforms a type and two terms into a boolean. *)
fun toEqDeq nt1 nt2 (E.ITYVAR (ityvar, eq)) =
    raise Fail "impossible:type variables are not allowed to be converted to nuprl"
  | toEqDeq nt1 nt2 (E.ITYETV (etyvar, lab)) =
    raise Fail "impossible:type variables are not allowed to be converted to nuprl "
  | toEqDeq nt1 nt2 (E.ITYCON (E.ITYSEQSEQ (itys, _), E.ITYCONNAM (tyconnam, _, _))) =
    let val n_deqs = map (fn ity => toDeq ity) itys
    in case (tyconnam, n_deqs) of
           ("*",     [deq1, deq2]) => NT.mk_nuprl_eq_prod_term deq1 deq2 nt1 nt2
	 | ("->",    [deq1, deq2]) => raise Fail "impossible:-> does not have an equality decider"
	 | ("+",     [deq1, deq2]) => NT.mk_nuprl_eq_union_term deq1 deq2 nt1 nt2
	 | ("Class", [deq]) => raise Fail "impossible:Class does not have an equality decider"
	 | ("List",  [deq]) => NT.mk_nuprl_eq_list_term deq nt1 nt2
	 | ("Bag",   [deq]) => NT.mk_nuprl_eq_bag_term deq nt1 nt2
	 | ("Int",   []) => NT.mk_nuprl_eq_int_term nt1 nt2
	 | ("Bool",  []) => NT.mk_nuprl_eq_bool_term nt1 nt2
	 | ("Real",  []) => raise Fail "impossible:Real does not have an equality decider"
	 | ("Msg",   []) => raise Fail "impossible:Msg does not have an equality decider"
	 | ("Unit",  []) => raise EH.Unimplemented ""
	 | ("Atom",  []) => NT.mk_nuprl_eq_atom_term nt1 nt2
	 | ("Type",  []) => raise Fail "impossible:Type does not have an equality decider"
	 | ("Nat",   []) => NT.mk_nuprl_eq_int_term nt1 nt2
	 | ("Loc",   []) => NT.mk_nuprl_eq_loc_term nt1 nt2
	 | (_, _) =>
	   (case getEqDec tyconnam of
		NONE => raise EH.Impossible ("no equality decider for " ^ tyconnam)
	      | SOME deq =>
		let val ndeq = NT.mk_nuprl_simple_term deq n_deqs
		    val app1 = NT.mk_apply_term ndeq nt1
		    val app2 = NT.mk_apply_term app1 nt2
		in app2
		end)
    end
  | toEqDeq nt1 nt2 ity = (print ("*** " ^ E.toStringIty ity ^ "\n"); raise Fail "impossible:the type should be fully built up, no variable")
*)

datatype alpha = NEXT of string * (unit -> alpha)

val lstAlpha =
    ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
     "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"]

fun getNewTyVarName [] suff () = getNewTyVarName lstAlpha (suff ^ "'") ()
  | getNewTyVarName (x :: xs) suff () =
    let val f = getNewTyVarName xs suff
    in NEXT (x ^ suff, f)
    end

val getNewTyVarName = getNewTyVarName lstAlpha ""

(* NOTE: idorset should be empty, we shouldn't have any or types in the types
 * of the top declarations. *)
fun toNuprlScheme context (E.VPOLY ((ityvarset, etyvarset, idorset), (overseq, ity))) =
    let val ity'  = E.stripIty ity
	val (nts1, ren) =
	    let val (nts, ren, _) =
		    E.foldrITVS (fn (tv, (nts, ren, f)) =>
				    let val NEXT (str, f') = f ()
				    in (str :: nts, BTV.insert (ren, tv, str), f')
				    end)
				([], BTV.empty, getNewTyVarName)
				ityvarset
	    in (nts, ren)
	    end
	val nts2  = E.listItemsETVS etyvarset
	val force = false
	val add   = true
	val n_ity = toNuprlIty 1 force add context IDENTS.empty ity' ren etyvarset
    in (n_ity, nts2 @ nts1)
    end
  | toNuprlScheme context (E.VODEC _) = raise Fail "impossible:overloaded schemes are not allowed to be converted into NuPrl"
  | toNuprlScheme context (E.VMONO _) = raise Fail "impossible:monomorphic schemes are not allowed to be converted into NuPrl"

(* NOTE: ityvarset and idorset should be empty. *)
fun schemeParamToHead context (E.VPOLY ((ityvarset, etyvarset, idorset), (overseq, ity))) =
    let val ity'  = E.stripIty ity
	val nts   = E.listItemsETVS etyvarset
	val force = false
	val add   = true
	val n_ity = toNuprlIty 2 force add context IDENTS.empty ity' BTV.empty etyvarset
    in (n_ity, nts)
    end
  | schemeParamToHead context (E.VODEC _) = raise Fail "impossible:overloaded schemes are not allowed to be converted into NuPrl"
  | schemeParamToHead context (E.VMONO _) = raise Fail "impossible:monomorphic schemes are not allowed to be converted into NuPrl"

structure SETVARS = BinarySetFn(type ord_key = string val compare = String.compare)

fun getVariablesNuprlType term =
    if NT.is_nuprl_variable_term term
    then SETVARS.singleton (NT.dest_variable term)
    else let val (opid, sterms) = NT.dest_simple_null_term term
	 in foldr (fn (rterm, vars) =>
		      SETVARS.union (getVariablesNuprlType (NT.rterm2term rterm), vars))
		  SETVARS.empty
		  sterms
	 end

fun findNlpsInType term typ (args, set) =
    let val opid = NT.opid_of_term typ
    in if opid = "eclass"
       then let val (_, _, _, Y) = NT.dest_eclass typ
		val nlp = NT.mk_nuprl_nlp_term (mk_nuprl_hidden_msg_fun ()) Y term
		val prg = NT.mk_nuprl_programmable_term (mk_nuprl_hidden_msg_fun ()) Y term
	    in ([(nlp, prg)], args, set)
	    end
       else if opid = "product"
       then let val (left, right) = NT.dest_simple_product typ
		val (nlps1, args1, set1) =
		    findNlpsInType (NT.mk_nuprl_pi1_term term)
				   left
				   ([], set)
		val (nlps2, args2, set2) =
		    findNlpsInType (NT.mk_nuprl_pi2_term term)
				   right
				   ([], set1)
		val nlps  = nlps1 @ nlps2
		val args' = if null nlps then [] else args
	    in (nlps, args' @ args1 @ args2, set2)
	    end
       else if opid = "function"
       then let val (left, right) = NT.dest_simple_function typ
		val id    = A.newId ()
		val v     = NT.mk_variable_term id
		val args' = (id, left) :: args
		val set'  = IDS.add (set, id)
	    in findNlpsInType (NT.mk_apply_term term v) right (args', set')
	    end
       else ([], [], set)
    end

(* GenNormLocProg:
 *
 * (Id -> Class(A)) -> Class(B)
 *
 * ----->
 *
 * We generate: (x,Id -> Class(A))
 * then: (v, Id) (x v, Class(A))
 *
 *)

fun genNormLocProg set class nity cls args =
    let val opid = NT.opid_of_term nity
    in if opid = "eclass"
       then let val (msg, es, e, X) = NT.dest_eclass nity
		val v_args   = map (fn (v, typ) => NT.mk_variable_term v) args
		val applies  = NT.mk_nuprl_applies_term class v_args
		val setvars  = getVariablesNuprlType X
		val vars     = SETVARS.listItems setvars
		val nlp      = NT.mk_nuprl_nlp_term (mk_nuprl_hidden_msg_fun ()) X applies
		val prg      = NT.mk_nuprl_programmable_term (mk_nuprl_hidden_msg_fun ()) X applies
		val (nlpargs, oargs, set')  =
		    foldr (fn ((v, typ), (nlps, args, set)) =>
			      let val term = NT.mk_variable_term v
				  val (nlps', args', set') = findNlpsInType term typ ([], set)
			      in (nlps' @ nlps, args' @ args, set')
			      end)
			  ([], [], set)
			  args
		val (nlpsimp, prgsimp) =
		    foldr (fn ((leftN, leftP), (rightN, rightP)) =>
			      (NT.mk_nuprl_implies_term leftN rightN,
			       NT.mk_nuprl_implies_term leftP rightP))
			  (nlp, prg) (*vallimp*)
			  nlpargs
		val (nlpsimp', prgsimp') =
		    foldr (fn ((param, typ), (rightN, rightP)) =>
			      let val term    = NT.mk_variable_term param
				  val nlp     = NT.mk_nuprl_nlp_term (mk_nuprl_hidden_msg_fun ()) typ term
				  val prg     = NT.mk_nuprl_programmable_term (mk_nuprl_hidden_msg_fun ()) typ term
			      in (NT.mk_nuprl_implies_term nlp rightN,
				  NT.mk_nuprl_implies_term prg rightP)
			      end)
			  (nlpsimp, prgsimp)
			  cls
		val (nlp_term0, prg_term0) =
		    foldl (fn ((v, typ), (termN, termP)) =>
			      (NT.mk_nuprl_all_term  typ (v, termN),
			       NT.mk_nuprl_all_term  typ (v, termP)))
			  (nlpsimp', prgsimp')
			  oargs
		val (nlp_term, prg_term) =
		    foldl (fn ((v, typ), (termN, termP)) =>
			      (NT.mk_nuprl_all_term  typ (v, termN),
			       NT.mk_nuprl_all_term  typ (v, termP)))
			  (nlp_term0, prg_term0)
			  args
	    in SOME (vars, nlp_term, prg_term)
	    end
       else if opid = "function"
       then let val (t1, t2) = NT.dest_simple_function nity
		val narg  = "x" ^ Int.toString (List.length args)
		val id    = A.newIdIdSet set narg
		val args' = args @ [(id, t1)]
		val set'  = IDS.add (set, id)
	    in genNormLocProg set' class t2 cls args'
	    end
       else NONE
    end

(* -- local class -- *)

(* we start with something like that:
 * forall x : typ. LocalClass(F[term]), where term = var(x)
 *
 *)
fun genLocClassStmtArg set term typ =
    let val opid = NT.opid_of_term typ
    in if opid = "eclass"
       then let val (_,_,_,T) = NT.dest_eclass typ
		val v      = A.newIdIdSet set "X"
		val set'   = IDS.add(set, v)
		val args   = [(v, typ)]
		val term'  = NT.mk_variable_term v
		val typ'   = NT.mk_nuprl_local_class_msg_term (mk_nuprl_hidden_msg_fun ()) T term'
	    in (set', args, term', typ')
	    end
       else if opid = "product"
       then let val (left,right) = NT.dest_simple_product typ
		val (set1,args1,term1,typ1) =
		    genLocClassStmtArg set
				       (NT.mk_nuprl_pi1_term term)
				       left
		val (set2,args2,term2,typ2) =
		    genLocClassStmtArg set1
				       (NT.mk_nuprl_pi2_term term)
				       right
		val args  = args1 @ args2
		val term' = NT.mk_pair_term term1 term2
		val typ'  = NT.mk_prod_term typ1 typ2
	    in (set2, args, term', typ')
	    end
       else (set, [], term, typ)
    end

fun genLocClassStmt set class n_typ cls_params args alls prog lc_abs =
    let val opid = NT.opid_of_term n_typ
    in if opid = "eclass"
       then let val (msg,es,e,T) = NT.dest_eclass n_typ
		val setvars = getVariablesNuprlType T
		val vars    = SETVARS.listItems setvars
		val loctyp  = NT.mk_nuprl_local_class_msg_term (mk_nuprl_hidden_msg_fun ()) T class
		val typ     =
		    foldr (fn ((v,T),typ) => NT.mk_nuprl_all_term T (v,typ))
			  loctyp
			  alls
		val mem     = NT.mk_nuprl_member_term typ lc_abs
		val uall    =
		    foldr (fn ((v,T),uall) => NT.mk_nuprl_uall_term T (v,uall))
			  mem
			  args
		val (term,prog',set') =
		    foldr (fn ((v,T),(term,prog,set)) =>
			      let val pr    = A.newIdIdSet set ("pr" ^ v)
				  val set'  = IDS.add(set,pr)
				  val tpr   = NT.mk_variable_term pr
				  val tv    = NT.mk_variable_term v
				  val prog' = NT.fo_subst [(NT.mk_new_nuprl_var v,tpr)] prog
				  val loc   = NT.mk_nuprl_local_class_msg_term (mk_nuprl_hidden_msg_fun ()) T tv
				  val cls   = NT.mk_nuprl_class_term (mk_nuprl_hidden_msg_fun ()) T
				  val term1 = NT.mk_nuprl_uall_term loc (pr, term)
				  val term2 = NT.mk_nuprl_uall_term cls (v, term1)
			      in (term2,prog',set')
			      end)
			  (uall,prog,set)
			  cls_params
	    in SOME (vars,term,prog')
	    end
       else if opid = "function"
       then let val (t1,t2) = NT.dest_simple_function n_typ
		val id   = A.newIdIdSet set "x"
		val tid  = NT.mk_variable_term id
		val set' = IDS.add (set, id)
		val (set'',args', term, typ) = genLocClassStmtArg set' tid t1
		val alls' = alls @ [(id, typ)]
	    in if List.null args' (* then no need to use term, we can use tid *)
	       then let val class' = NT.mk_apply_term class tid
		    in genLocClassStmt set'' class' t2 cls_params args alls' prog lc_abs
		    end
	       else let val class' = NT.mk_apply_term class term
			val args'' = args @ args'
		    in genLocClassStmt set'' class' t2 cls_params args'' alls' prog lc_abs
		    end
	    end
       else NONE
    end (*handle _ => NONE*)

fun getItyAtLab context label msg =
    if get_typ_context context
    then case EN.getItyAtLab (getUnifEnv ()) label of
	     NONE => raise Fail ("impossible:" ^ msg)
	   | SOME ity => ity
    else E.mk_type_top label

fun getScheme context id lab poly msg =
    if get_typ_context context
    then case EN.getScheme id lab poly (get_base_context context) (getUnifEnv ()) of
	     NONE => (print msg; raise Fail ("impossible:" ^ msg))
	   | SOME scheme => scheme
    else E.mk_new_scheme [] (E.mk_type_top D.dummy_label)

fun is_fun_deq_typ qs term =
    let val opid = NT.opid_of_term term
    in case opid of
	   "!theorem" => is_fun_deq_typ qs (NT.subtermn 1 term)
	 | "all "     =>
	   let val (qs',t) = NT.dest_alls_ualls term
	   in is_fun_deq_typ (qs @ qs') t
	   end
	 | "uall"     =>
	   let val (qs',t) = NT.dest_alls_ualls term
	   in is_fun_deq_typ (qs @ qs') t
	   end
	 | "member"   =>
	   let val (typ,F) = NT.dest_member term
	   in if NT.is_nuprl_function_term typ
	      then let val (T1,T) = NT.dest_simple_function typ
		   in if NT.is_nuprl_function_term T
		      then let val (T2,B) = NT.dest_simple_function T
			   in if NT.alpha_equal_terms T1 T2
				 andalso
				 NT.is_nuprl_bool_term B
			      then SOME (qs,F,T1)
			      else NONE
			   end
		      else NONE
		   end
	      else NONE
	   end
	 | _ => NONE
    end

fun is_fun_deq_exp term =
    NT.is_nuprl_lambda_term term
    andalso
    let val (v1,B1) = NT.dest_lambda 8 term
    in NT.is_nuprl_lambda_term B1
       andalso
       let val (v2,B2) = NT.dest_lambda 9 B1
       in NT.is_nuprl_eq_int_term B2
	  orelse
	  NT.is_nuprl_eq_id_term B2
	  orelse
	  (NT.is_nuprl_apply_term B2
	   andalso
	   let val (F1,a1) = NT.dest_apply B2
	   in NT.is_nuprl_apply_term F1
	      andalso
	      let val (F2,a2) = NT.dest_apply F1
	      in NT.is_nuprl_eqof_term F2
	      end
	   end)
       end
    end

fun are_disjoint term1 term2 =
    let val opid1 = NT.opid_of_term term1
	val opid2 = NT.opid_of_term term2
    in if String.isSuffix "'base" opid1
       then if String.isSuffix "'base" opid2
	    then String.substring (opid1, 0, String.size opid1 - 5)
		 <>
		 String.substring (opid2, 0, String.size opid2 - 5)
	    else if opid2 = "eclass0"
	    then are_disjoint term1 (NT.subtermn 2 term2)
	    else false
       else if opid1 = "eclass0"
       then are_disjoint (NT.subtermn 2 term1) term2
       else false
    end

fun are_disjoint_list term [] = true
  | are_disjoint_list term (x :: xs) =
    are_disjoint term x
    andalso
    are_disjoint_list term xs

fun is_single_valued_fun context term =
    let val opid = NT.opid_of_term term
    in if opid = "single-bag"
       then true
       else if opid = "lambda"
       then let val (v,b) = NT.dest_lambda 0 term
	    in is_single_valued_fun context b
	    end
       else if opid = "spread"
       then let val (p, v1, v2, b) = NT.dest_spread 0 term
	    in is_single_valued_fun context b
	    end
       else case find_dec_context context opid of
		SOME t =>
		if NT.is_nuprl_iabstraction_term t (* here we should really unfold *)
		then let val (cond,lhs,rhs) = NT.dest_iabstraction t
		     in is_single_valued_fun context (NT.rterm2term rhs)
		     end
		else false
	      | NONE => false
    end

fun is_single_valued context term =
    let val opid = NT.opid_of_term term
    in if String.isSuffix "'base" opid
       then true
       else if opid = "eclass0"
       then is_single_valued context (NT.subtermn 2 term)
	    andalso
	    is_single_valued_fun context (NT.subtermn 1 term)
       else if opid = "parallel-class"
       then let val t1 = NT.subtermn 1 term
		val t2 = NT.subtermn 2 term
	    in is_single_valued context t1
	       andalso
	       is_single_valued context t2
	       andalso
	       are_disjoint t1 t2
	    end
       else case find_dec_context context opid of
		SOME t =>
		if NT.is_nuprl_iabstraction_term t (* here we should really unfold *)
		then let val (cond,lhs,rhs) = NT.dest_iabstraction t
		     in is_single_valued context (NT.rterm2term rhs)
		     end
		else false
	      | NONE => false
    end

fun is_single_valued_list context [] = true
  | is_single_valued_list context (x :: xs) =
    is_single_valued context x
    andalso
    is_single_valued_list context xs
    andalso
    are_disjoint_list x xs

fun is_functional_class context term =
    let val opid = NT.opid_of_term term
	(*val _ = print ("\n----\n" ^ NT.toStringTerm term ^ "\n-----\n")*)
    in if opid = "State1"
	  orelse opid = "Memory1"
	  orelse opid = "state-class1"
	  orelse opid = "memory-class1"
       then let val X = NT.subtermn 3 term
	    in is_single_valued context X
	    end
       else if opid = "State2"
	       orelse opid = "Memory2"
	       orelse opid = "state-class2"
	       orelse opid = "memory-class2"
       then let val X1 = NT.subtermn 3 term
		val X2 = NT.subtermn 5 term
	    in is_single_valued_list context [X1, X2]
	    end
       else if opid = "State3"
	       orelse opid = "Memory3"
	       orelse opid = "state-class3"
	       orelse opid = "memory-class3"
       then let val X1 = NT.subtermn 3 term
		val X2 = NT.subtermn 5 term
		val X3 = NT.subtermn 7 term
	    in is_single_valued_list context [X1, X2, X3]
	    end
       else if opid = "State4"
	       orelse opid = "Memory4"
	       orelse opid = "state-class4"
	       orelse opid = "memory-class4"
       then let val X1 = NT.subtermn 3 term
		val X2 = NT.subtermn 5 term
		val X3 = NT.subtermn 7 term
		val X4 = NT.subtermn 9 term
	    in is_single_valued_list context [X1, X2, X3, X4]
	    end
       else false
    end

fun is_functional_sm context qs term exp =
    let val opid = NT.opid_of_term term
    in case opid of
	   "!theorem" => is_functional_sm context qs (NT.subtermn 1 term) exp
	 | "all "     =>
	   let val (qs',t) = NT.dest_alls_ualls term
	   in is_functional_sm context (qs @ qs') t exp
	   end
	 | "uall"     =>
	   let val (qs',t) = NT.dest_alls_ualls term
	   in is_functional_sm context (qs @ qs') t exp
	   end
	 | "member"   =>
	   let val (typ,v) = NT.dest_member term
	       val (lst,typ') = NT.dest_simple_functions typ
	   in if NT.is_nuprl_eclass_term typ'
	      then let val (info,es,e,T) = NT.dest_eclass typ'
		       val (boubds,t) = NT.dest_lambdas 4 exp
		   in if is_functional_class context t
		      then SOME (qs, lst, v, T)
		      else NONE
		   end
	      else NONE
	   end
	 | _ => NONE
    end

fun reorder_parameters' [] params rest = params @ rest
  | reorder_parameters' (x :: xs) params rest =
    if x = !hidden_event_ordering
    then reorder_parameters' xs params (x :: rest)
    else if x = !hidden_msg_fun
    then reorder_parameters' xs params (rest @ [x])
    else reorder_parameters' xs (params @ [x]) rest

fun reorder_parameters params = reorder_parameters' params [] []

fun get_params_set_types context params =
    foldr (fn (param, lst) =>
	      case get_param_context context param of
		  SOME (scheme, kind, SOME (prop, prms)) => prms @ lst
		| SOME (scheme, kind, NONE) => lst
		| NONE => raise Fail ("get_params_set_types:" ^ param ^ ")"))
	  []
	  params

(* The label is not used when schemeop is SOME _.
 * When the label is used, it is used to find a type for id.
 *)
fun genAbstractionAndWF i context value id lab n_exp exp schemeop premises programmable localclass data lvlop =
    let val eid    = if A.isInternalId value then value else id
	val poly   = get_poly_context context
	(*val _ = print (NT.toStringTerm n_exp ^ "\n")*)
	(*val _ = print ("gen-abs (" ^ Int.toString i ^ ")\n")*)
	val _      = addDecFPList (getFreeParams context [] exp)
	(*val parms0 = getDecFP ()*)
	val n_typ  = NT.mk_nuprl_type_term "i'"
	val n_atyp = NT.mk_nuprl_valuealltype_term "i"
	val n_btyp = NT.mk_nuprl_valuealltype_term "i'"
	(* -- *)
	val ((n_ity, bounds), context') =
	    case schemeop of
		SOME scheme => (scheme, context)
	      | NONE =>
		let val msg1   = ":" ^ Int.toString i ^ ":" ^ value ^ ":" ^ id ^ ":" ^ eid
		    val msg2   = "Couldn't find " ^ eid ^ "'s environment"
		    val msg    = msg2 ^ "(" ^ msg1 ^ ")"
		    val scheme = getScheme context eid lab poly msg
		in (toNuprlScheme context scheme, context)
		end
	val _     = addDecFPList (get_params_set_types context (getDecFP ()))
	(* we now recompute the parameters with the ones contained in the type *)
	val parms = reorder_parameters (getDecFP ())
	(* we use parms instead of parms0 because for type inference it's
	 * easier to have the parameters occuring in the type, be
	 * parameters of the abstraction.
	 * (See also addUserFP below.) *)
	val n_prs  = map NT.mk_variable_term parms(*0*)
	val n_id   = mk_nuprl_user_term_lvlop id n_prs lvlop
	val n_abs  = NT.mk_nuprl_iabstraction_term n_id n_exp
	(*val _     = print ("--" ^ value ^ "-" ^ id
			   ^ "\n"
			   ^ ListFormat.fmt {init  = "{",
					     final = "}",
					     sep   = ",",
					     fmt   = fn x => x} parms0
			   ^ "\n"
			   ^ ListFormat.fmt {init  = "{",
					     final = "}",
					     sep   = ",",
					     fmt   = fn x => x} parms
			   ^ "\n--\n")*)
	(* -- *)
	val set = get_all_ids_list [n_id, n_ity]
	(* -- get the types of the parameters *)
	val nprms0 =
	    map (fn param =>
		    case get_param_context context param of
			SOME (scheme, kind, propop) =>
			let val (n_ity, nts) = schemeParamToHead context scheme
			in (param, (n_ity, nts, propop))
			end
		      | NONE =>
			raise Fail ("genAbstractionAndWF"
				    ^ ":get_param_context"
				    ^ ":impossible("
				    ^ param
				    ^ ","
				    ^ value
				    ^ ","
				    ^ id
				    ^ ")"))
		parms
	(* -- *)
	val (nprms1, nprms2, nprms_cls, eop) =
	    List.foldr (fn (nfo as (param, (nity,_,_)), (typs, ids, cls, eop)) =>
			   if NT.is_nuprl_type_term nity
			   then (nfo :: typs, ids, cls, eop)
			   else if NT.is_nuprl_eclass_term nity
			   then let val (M, es, e, T) = NT.dest_eclass nity
				in (typs, nfo :: ids, (param, T) :: cls, eop)
				end
			   else if NT.is_nuprl_event_orderingp_term "i'" nity
			   then (typs, nfo :: ids, cls, true)
			   else (typs, nfo :: ids, cls, eop))
		       ([],[],[],false)
		       nprms0
	val n_ity =
	    if eop andalso NT.is_nuprl_prop_term "i" n_ity
	    then NT.mk_nuprl_prop_term "i'"
	    else n_ity
	val nprms = nprms1 @ nprms2
	fun toUAllPrms term =
	    foldr (fn ((param, (nity, nts, propop)), n_term) =>
		      foldr (NT.mk_nuprl_uall_term n_typ)
			    (let val typ =
				      if NT.is_nuprl_type_term nity
				      then n_atyp
				      else nity
				 val typ' =
				     case propop of
					 NONE => typ
				       | SOME (prop,params) => NT.mk_set_term typ (param, prop)
			      in NT.mk_nuprl_uall_term typ' (param, n_term)
			      end (*NT.mk_nuprl_uall_term nity (param, n_term)*))
			    nts)
		  term
		  nprms
	fun toAllPrms term =
	    foldr (fn ((param, (nity, nts, propop)), n_term) =>
		      ((*print ("[" ^ param ^ "]\n");
		       print (toStringTerm nity ^ "\n");*)
		       foldr (NT.mk_nuprl_all_term n_typ)
			     (let val typ =
				      if NT.is_nuprl_type_term nity
				      then n_atyp
				      else nity
				 val typ' =
				     case propop of
					 NONE => typ
				       | SOME (prop,params) => NT.mk_set_term typ (param, prop)
			      in NT.mk_nuprl_all_term typ' (param, n_term)
			      end)
			     nts))
		  term
		  nprms
	(*val n_thm  = let val n_mem   = NT.mk_nuprl_member_term n_ity n_id
			 val all     = foldr (NT.mk_nuprl_uall_term n_typ)
					     n_mem
					     bounds
			 val allprms = toAllPrmsWf all
			 val wfname  = get_user_name id ^ "_wf"
		     in NT.mk_nuprl_itheorem_term wfname allprms
		     end*)
	val (n_thm, set) =
	    let val (term, n_ity', alls, asmps, set') =
		    foldl (fn (premise, (term, n_ity, alls, asmps, set)) =>
			      if NT.is_nuprl_function_term n_ity
			      then let val (A, B) = NT.dest_simple_function n_ity
				       val v      = A.newIdIdSet set "v"
				       val tv     = NT.mk_variable_term v
				       val term'  = NT.mk_apply_term term tv
				   in (term', B, alls @ [(v, A)], asmps @ [premise tv], IDS.add (set, v))
				   end
			      else raise Fail "genabstraction:too-many-premises")
			  (n_id, n_ity, [], [], set)
			  premises
		val isect   = foldr (NT.mk_nuprl_isect_term n_typ)
				    n_ity'
				    bounds
		val n_mem   = NT.mk_nuprl_member_term isect term
		val n_mem1  = foldr (fn (prem, term) => NT.mk_nuprl_uimplies_term prem term) n_mem asmps
		val n_mem2  = foldr (fn ((v, typ), term) => NT.mk_nuprl_uall_term typ (v, term)) n_mem1 alls
		val allprms = toUAllPrms n_mem2
		val wfname  = get_user_name id ^ "_wf"
	    in (NT.mk_nuprl_itheorem_term wfname allprms, set')
	    end
	val functional =
	    case is_functional_sm context [] n_thm n_exp of
		SOME (qs, lst, t, T) =>
		let val set     = get_all_ids_list [t,T]
		    val (qs1, set) =
			foldl (fn (t,(qs,set)) =>
				  let val x = A.newIdIdSet set "x"
				  in (qs @ [(x, t, false)], IDS.add (set, x))
				  end)
			      ([],set)
			      lst
		    val es      = A.newIdIdSet set "es"
		    val tes     = NT.mk_variable_term es
		    val qs2     = [(es, NT.mk_nuprl_event_orderingp_term "i" (mk_nuprl_hidden_msg_fun ()), false)]
		    val qs'     = qs @ qs2 @ qs1
		    val app     = foldl (fn ((x,_,_), t) => NT.mk_apply_term t (NT.mk_variable_term x)) t qs1
		    val stmt    = NT.mk_nuprl_es_functional_class_term tes T app
		    val fun_lem = foldr (fn ((v, typ, b), term) => NT.mk_nuprl_uall_term typ (v, term)) stmt qs'
		    val fun_nam = get_user_name id ^ "-functional"
		    val fun_thm = NT.mk_nuprl_itheorem_term fun_nam fun_lem
		    (**)
		    val e       = A.newIdIdSet set "e"
		    val te      = NT.mk_variable_term e
		    val fun_prm = n_prs @ (map (fn (x,_,_) => NT.mk_variable_term x) qs1) @ [tes, te]
		    val fun_id  = mk_nuprl_user_term_lvlop (id ^ "Fun") fun_prm lvlop
		    val fun_R   = NT.mk_nuprl_classfun_term tes app te
		    val fun_abs = NT.mk_nuprl_iabstraction_term fun_id fun_R
		    (**)
		    val qs3     = [(e, NT.mk_nuprl_esE_term tes, false)]
		    val qs''    = qs @ qs2 @ qs3 @ qs1
		    val fun_wfn = get_user_name id ^ "Fun_wf"
		    val fun_wfm = NT.mk_nuprl_member_term T fun_id
		    val fun_wfl = foldr (fn ((v, typ, b), term) => NT.mk_nuprl_uall_term typ (v, term)) fun_wfm qs''
		    val fun_wf  = NT.mk_nuprl_itheorem_term fun_wfn fun_wfl
		    (**)
		    val fun_crn = get_user_name id ^ "-classrel"
		    val v       = A.newIdIdSet set "v"
		    val tv      = NT.mk_variable_term v
		    val qs0     = qs2 @ qs3 @ qs1 @ [(v, T, true)]
		    val fun_crL = NT.mk_nuprl_classrel_term tes T app te tv
		    val fun_crR = NT.mk_equal_term T tv fun_id
		    val fun_cri = NT.mk_nuprl_iff_term fun_crL fun_crR
		    val fun_cra = foldr (fn ((v, typ, b), term) => NT.mk_nuprl_all_term  typ (v, term)) fun_cri qs0
		    val fun_cru = foldr (fn ((v, typ, b), term) => NT.mk_nuprl_uall_term typ (v, term)) fun_cra qs
		    val fun_cr  = NT.mk_nuprl_itheorem_term fun_crn fun_cru
		    val _ = print ("[functional class found: " ^ id ^ "]\n")
		in [fun_thm, fun_abs, fun_wf, fun_cr]
		end
	      | NONE => []
	val assert_eq =
	    case is_fun_deq_typ [] n_thm of
		SOME (qs, F, typ) =>
		if is_fun_deq_exp n_exp
		then let val assert_name  = get_user_name id ^ "-assert"
			 val assert_x     = A.newIdIdSet set "x"
			 val assert_y     = A.newIdIdSet set "y"
			 val assert_tx    = NT.mk_variable_term assert_x
			 val assert_ty    = NT.mk_variable_term assert_y
			 val assert_app1  = NT.mk_apply_term F assert_tx
			 val assert_app2  = NT.mk_apply_term assert_app1 assert_ty
			 val assert_a     = NT.mk_nuprl_assert_term assert_app2
			 val assert_eq    = NT.mk_equal_term typ assert_tx assert_ty
			 val assert_uiff  = NT.mk_nuprl_uiff_term assert_a assert_eq
			 val assert_qs    = qs @ [(assert_x, typ, false), (assert_y, typ, false)]
			 val assert_lemma = foldr (fn ((v, typ, b), term) => NT.mk_nuprl_uall_term typ (v, term)) assert_uiff assert_qs
			 val assert_thm   = NT.mk_nuprl_itheorem_term assert_name assert_lemma
			 val upd_name     = get_user_name id ^ "-assert\\ upd"
			 val upd_part1    = NT.mk_nuprl_itext_term "assert_pushdown_add\\ \\(ioid\\ "
			 val upd_part2    = NT.mk_nuprl_iinsert_object_term NT.mk_nuprl_ioid_term
			 val upd_part3    = NT.mk_nuprl_itext_term "\\)"
			 val upd_part4    = NT.mk_nuprl_inewline_term
			 val upd_part5    = NT.mk_nuprl_itext_term "\\(\\[\\]\\,\\ \\["
			 val upd_part6    = NT.mk_nuprl_iinsert_object_p_term assert_name NT.mk_nuprl_ioid_term
			 val upd_part7    = NT.mk_nuprl_itext_term "\\]\\)"
			 val upd_list     = [upd_part1, upd_part2, upd_part3, upd_part4, upd_part5, upd_part6, upd_part7]
			 val upd_code     = NT.mk_nuprl_itext_list_term upd_list
			 val upd          = NT.mk_nuprl_iupdate_term upd_name upd_code
			 val _ = print ("[generating eq assert + update for " ^ id ^ "]\n")
		     in [assert_thm, upd]
		     end
		else []
	      | NONE => []
	val n_nrms =
	    if get_nlp_context context
	    then case genNormLocProg set n_id n_ity nprms_cls [] of
		     SOME (vars, nlp_term, prg_term) =>
		     let val (n_all_N, n_all_P) =
			     foldr (fn (v, (tN, tP)) =>
				       if List.exists (fn x => x = v) vars
				       then (NT.mk_nuprl_all_term  n_btyp (v, tN),
					     NT.mk_nuprl_all_term  n_btyp (v, tP))
				       else (NT.mk_nuprl_all_term  n_typ  (v, tN),
					     NT.mk_nuprl_all_term  n_typ  (v, tP)))
				   (nlp_term, prg_term)
				   bounds
			 val all_nlp = toAllPrms  n_all_N
			 val all_prg = toAllPrms  n_all_P
			 val nlp_lem = NT.mk_nuprl_itheorem_term (get_user_name id ^ "_nlp") all_nlp
			 val prop    = NT.mk_nuprl_iproperty_term "extract_required" (NT.mk_nuprl_ibool_term true)
			 val cons    = NT.mk_nuprl_icons_cons_term prop NT.mk_nuprl_icons_nil_term
			 val ipropsn = NT.mk_nuprl_iinclude_properties_term cons nlp_lem
		     in if programmable
			then let val prg_lem = NT.mk_nuprl_itheorem_term (get_user_name id ^ "_programmable") all_prg
				 val ipropsp = NT.mk_nuprl_iinclude_properties_term cons prg_lem
			     in [ipropsn, ipropsp]
			     end
			else [ipropsn]
		     end
		   | NONE => []
	    else []
	val lc_abs = mk_nuprl_user_term_lvlop (id ^ "-program") n_prs lvlop
	val (extras, prog_abs) =
	    if is_newprog_context context
	    then case localclass of
		     SOME prog =>
		     (case genLocClassStmt set n_id n_ity nprms_cls [] [] prog lc_abs of
			  SOME (vars,all,prog') =>
			  let val all1 =
				  foldr (fn (v, all) =>
					    if List.exists (fn x => x = v) vars
					    then NT.mk_nuprl_all_term  n_btyp (v, all)
					    else NT.mk_nuprl_all_term  n_typ  (v, all))
					all
					bounds
			      val all2    = toUAllPrms all1
			      val loc_abs = NT.mk_nuprl_iabstraction_term lc_abs prog'
			      val loc_lem = NT.mk_nuprl_itheorem_term (get_user_name id ^ "-program_wf") all2
			  in ([loc_abs, loc_lem], SOME lc_abs)
			  end
			| _ => (n_nrms, SOME n_id))
		   | _ => (n_nrms, SOME n_id)
	    else (n_nrms, NONE)
	val context' =
	    if data (* see n_prs' definition above *)
	    then add_data_context context id parms(*0*)
	    else let val _ = addUserFP id parms(*0*)
		 in context
		 end
    in (n_id, prog_abs, n_abs, n_thm, functional @ assert_eq @ extras, context')
    end

fun genAbstractionAndWF' i context value id lab n_exp exp premises =
    let val schemeop     = NONE
	val programmable = false
	val localclass   = NONE
	val data         = false
	val lvlop        = NONE
	val (id, lc, abs, thm, rest, _) =
	    genAbstractionAndWF
		i
		context
		value
		id
		lab
		n_exp
		exp
		schemeop
		premises
		programmable
		localclass
		data
		lvlop
    in (id, lc, abs, thm, rest)
    end

val dummy_dot_term = A.mk_new_dum_term A.DOTS_D "" [] []

fun genAbstractionAndWFDots i context value id lab n_exp premises =
    genAbstractionAndWF' i context value id lab n_exp dummy_dot_term premises

fun selectEndTuple6 list =
    if List.length list <= 6
    then (list, NONE)
    else let val firsts = List.rev (List.take (list, 6))
	     val lasts  = List.rev (List.drop (list, 6))
	 in (firsts, SOME lasts)
	 end

(* let pat = exp1 in exp2,
 * where pat is a EML term, while exp1 and exp2 are Nuprl terms *)
fun mvPat2Exp set (pat as A.N {kind = (A.ATPAT, A.ATPAT_ID),    label, value, regions, parents, children = [id]})  exp1 exp2 =
    (NT.mk_nuprl_let_term (A.getIdIdent id) exp1 exp2, set)
  | mvPat2Exp set (pat as A.N {kind = (A.ATPAT, A.ATPAT_WILD),  label, value, regions, parents, children = []})       exp1 exp2 = (exp2, set)
  | mvPat2Exp set (pat as A.N {kind = (A.ATPAT, A.ATPAT_SCON),  label, value, regions, parents, children = []})       exp1 exp2 = raise Fail "impossible:mvPat2Exp:SCON"
  | mvPat2Exp set (pat as A.N {kind = (A.ATPAT, A.ATPAT_LIST),  label, value, regions, parents, children})            exp1 exp2 = raise Fail "impossible:mvPat2Exp:LIST"
  | mvPat2Exp set (pat as A.N {kind = (A.ATPAT, A.ATPAT_PAREN), label, value, regions, parents, children = [pat']})   exp1 exp2 = mvPat2Exp set pat' exp1 exp2
  | mvPat2Exp set (pat as A.N {kind = (A.ATPAT, A.ATPAT_STRUC), label, value, regions, parents, children = [ident]})  exp1 exp2 = raise EH.Unimplemented ""
  | mvPat2Exp set (pat as A.N {kind = (A.ATPAT, A.ATPAT_TUPLE), label, value, regions, parents, children = []})       exp1 exp2 = (exp2, set) (*raise EH.Unimplemented ""*)
  | mvPat2Exp set (pat as A.N {kind = (A.ATPAT, A.ATPAT_TUPLE), label, value, regions, parents, children})            exp1 exp2 =
    let val (list, listop) = selectEndTuple6 children
	fun newExpLet set listPat exp2 =
	    let val (ids, exp', set') =
		    foldr (fn (pat, (ids, exp, set)) =>
			      (case A.getIdPat pat of
				   SOME id => ((A.getIdIdent id) :: ids, exp, set)
				 | NONE =>
				   let val id   = A.newIdIdSet set "z"
				       val set' = IDS.add (set, id)
				       val tid  = NT.mk_variable_term id
				       val (exp', set'') = mvPat2Exp set' pat tid exp
				   in (id :: ids, exp', set'')
				   end))
			  ([], exp2, set)
			  listPat
		val _ = if List.null ids then print ("[atpat_tuple:unit]\n") else ()
	    in (NT.mk_nuprl_spreadn_term exp1 (ids, exp'), set')
	    end
    in case listop of
	   NONE => newExpLet set list exp2
	 | SOME [p] => newExpLet set (list @ [p]) exp2
	 | SOME list2 =>
	   let val id    = A.newIdIdSet set "z"
	       val tid   = NT.mk_variable_term id
	       val atpat = A.mk_new_dum_term A.ATPAT_TUPLE "" [] list2
	       val (exp', set') = mvPat2Exp set atpat tid exp2
	   in newExpLet set' (list @ [A.idToPat id]) exp'
	   end
    end
  | mvPat2Exp set (pat as A.N {kind = (A.PAT, A.PAT_TYPED), label, value, regions, parents, children})                exp1 exp2 = raise Fail "impossible:we should have get rid of all explicit type constraints"
  | mvPat2Exp set (pat as A.N {kind = (A.PAT, A.PAT_AS),    label, value, regions, parents, children = [ident, p]})   exp1 exp2 = raise EH.Unimplemented "mvPat2Exp:as"
  | mvPat2Exp set (pat as A.N {kind = (A.PAT, A.PAT_CONS),  label, value, regions, parents, children = [pat1, pat2]}) exp1 exp2 = raise EH.Unimplemented "mvPat2Exp:cons"
  | mvPat2Exp set (pat as A.N {kind = (A.PAT, A.PAT_ATPAT), label, value, regions, parents, children = [atpat]})      exp1 exp2 = mvPat2Exp set atpat exp1 exp2
  | mvPat2Exp set (pat as A.N {kind = (A.PAT, A.PAT_APP),   label, value, regions, parents, children})                exp1 exp2 = raise EH.Unimplemented "mvPat2Exp:app"
  | mvPat2Exp set pat exp1 exp2 = raise Fail "impossible:term is not a pattern"

fun pat2exp (pat as A.N {kind = (A.ATPAT, A.ATPAT_ID),    label, value, regions, parents, children = [id]})         = A.idToAtExp (A.getIdIdent id)
  | pat2exp (pat as A.N {kind = (A.ATPAT, A.ATPAT_WILD),  label, value, regions, parents, children = []})           = raise Fail "impossible:pat2Exp:WILD"
  | pat2exp (pat as A.N {kind = (A.ATPAT, A.ATPAT_SCON),  label, value, regions, parents, children = []})           = raise Fail "impossible:pat2Exp:SCON"
  | pat2exp (pat as A.N {kind = (A.ATPAT, A.ATPAT_LIST),  label, value, regions, parents, children})                = raise Fail "impossible:pat2Exp:LIST"
  | pat2exp (pat as A.N {kind = (A.ATPAT, A.ATPAT_PAREN), label, value, regions, parents, children = [pat']})       = pat2exp pat'
  | pat2exp (pat as A.N {kind = (A.ATPAT, A.ATPAT_STRUC), label, value, regions, parents, children = [ident]})      = raise EH.Unimplemented "pat2Exp:STRUCT"
  | pat2exp (pat as A.N {kind = (A.ATPAT, A.ATPAT_TUPLE), label, value, regions, parents, children})                = A.mk_new_dum_term A.ATEXP_TUPLE "" [] (map pat2exp (A.getChildren pat))
  | pat2exp (pat as A.N {kind = (A.PAT,   A.PAT_TYPED),   label, value, regions, parents, children})                = raise Fail "impossible:pat2Exp:TYPED"
  | pat2exp (pat as A.N {kind = (A.PAT,   A.PAT_AS),      label, value, regions, parents, children = [ident, p]})   = raise EH.Unimplemented "pat2Exp:AS"
  | pat2exp (pat as A.N {kind = (A.PAT,   A.PAT_CONS),    label, value, regions, parents, children = [pat1, pat2]}) = raise EH.Unimplemented "pat2Exp:CONS"
  | pat2exp (pat as A.N {kind = (A.PAT,   A.PAT_ATPAT),   label, value, regions, parents, children = [atpat]})      = pat2exp atpat
  | pat2exp (pat as A.N {kind = (A.PAT,   A.PAT_APP),     label, value, regions, parents, children})                = raise EH.Unimplemented "pat2Exp:APP"
  | pat2exp pat = raise Fail "impossible:term is not a pattern"

fun split_type_epat exp typ =
    if NT.is_nuprl_pair_term exp
    then let val (exp1, exp2) = NT.dest_pair 0 exp
	 in if NT.is_nuprl_product_term typ
	    then let val (A,B) = NT.dest_simple_product typ
		 in MAPID.unionWith (fn _ => raise Fail "split_type_epat:multi-occ")
				    (split_type_epat exp1 A, split_type_epat exp2 B)
		 end
	    else raise Fail "split_type_epat:not-a-product"
	 end
    else if NT.is_nuprl_variable_term exp
    then MAPID.singleton (NT.dest_variable exp, typ)
    else raise Fail "split_type_epat:not-an-epat"

fun genInvariant context name cls (pairs, args) vals wclsop Xop (nprop, prop) T ren single_val =
    let val _      = app (fn x => addDecFPList (getFreeParams context [] x)) args
	val assumptions =
	    map (fn hyp =>
		    case getUserFP (is_bound_context context) hyp of
			NONE => raise Fail "unknown assumption"
		      | SOME params =>
			let val a_terms = map NT.mk_variable_term params
			    val _ = addDecFPList params
			in mk_nuprl_user_term hyp a_terms
			end)
		(get_list_hyps_context context)
	val _      = addDecFPList (getFreeParams context [] prop)
	val terms  =
	    case wclsop of
		SOME (_, _, SOME (_, _, ndprop, dprop)) =>
		(addDecFPList (getFreeParams context [] dprop); [ndprop])
	      | _ => []
	val parms0 = getDecFP ()
	val _      = addDecFPList (get_params_set_types context (getDecFP ()))
	val parms  = reorder_parameters (getDecFP ())
	val (ncls,mapargs) =
	    foldl (fn ((param, typ), (cls, map)) =>
		      let val cls' = NT.mk_apply_term cls param
			  val map' =
			      MAPID.unionWith
				  (fn _ => raise Fail "split_type_epat:multi-occ")
				  (map, split_type_epat param typ)
		      in (cls', map')
		      end)
		  (cls, MAPID.empty)
		  pairs
	val n_typ  = NT.mk_nuprl_type_term "i'"
	val n_atyp = NT.mk_nuprl_valuealltype_term "i"
	val nprms0 = map (fn param =>
			     case get_param_context context param of
				 SOME (scheme, kind, propop) =>
				 let val (n_ity, nts) = schemeParamToHead context scheme
				 in (param, (n_ity, nts, propop))
				 end
			       | NONE => raise Fail "impossible:genInvariant:get_param_context")
			 parms
	val (nprms1, nprms2, nprms_cls) =
	    List.foldr (fn (nfo as (param, (nity,_,_)), (typs, ids, cls)) =>
			   if NT.is_nuprl_type_term nity
			   then (nfo :: typs, ids, cls)
			   else if NT.is_nuprl_eclass_term nity
			   then let val (M,es,e,T) = NT.dest_eclass nity
				in (typs, nfo :: ids, (param, T) :: cls)
				end
			   else (typs, nfo :: ids, cls))
		       ([],[],[])
		       nprms0
	val nparms = nprms1 @ nprms2
	(* We generate the 'es' variable *)
	val set    = get_all_ids_list ([ncls, nprop, T] @ terms)
	val eo     = A.newIdIdSet set "es"
	val teo    = NT.mk_variable_term eo
	(* some of the parameters are classes, we have to generate
	 * hypotheses that they these classes are single valued. *)
	val clprms =
	    List.map (fn (param, typ) =>
			 NT.mk_nuprl_single_valued_classrel_term
			     teo
			     (NT.mk_variable_term param)
			     typ)
		     nprms_cls
	(* We check whether cls is a State-comb class: *)
	val is_state_comb = is_state_context context cls
	val (cs, vs, es, ls, term, set, _, _) =
	    foldr (fn (pat, (cs, vs, es, ls, term, set, n, eop)) =>
		      let val ns = Int.toString n
			  val v  = getIdPat 6 pat
			  (*val v  = A.newIdIdSet set ("v" ^ ns)*)
			  val e  = A.newIdIdSet set ("e" ^ ns)
			  val tv = NT.mk_variable_term v
			  val te = NT.mk_variable_term e
			  val c  = NT.mk_nuprl_classrel_term teo T ncls te tv
			  val s' = IDS.add (IDS.add (set, v), e)
			  (*val (t',s'') = mvPat2Exp s' pat tv term*)
			  val t'  = term
			  val s'' = s'
			  val ls' = case eop of
					SOME e' => (NT.mk_nuprl_es_locl_term teo te e') :: ls
				      | NONE => ls
		      in (c :: cs, v :: vs, e :: es, ls', t', s'', n - 1, SOME te)
		      end)
		  ([], [], [], [], nprop, set, length vals, NONE)
		  vals
	val (exop, set) =
	    case wclsop of
		SOME (term, ident, decprop) =>
		let val ity   = getItyAtLab context (A.getLabel ident) ""
		    val force = false
		    val add   = true
		    val nty   = toNuprlIty 21 force add context IDENTS.empty (E.stripIty ity) ren E.emptyETVS
		in if NT.is_nuprl_eclass_term nty andalso List.length es >= 2
		   then let val (info,_,_,A) = NT.dest_eclass nty
			    val e1   = List.hd es
			    val e2   = List.last es
			    val e    = A.newIdIdSet set "e"
			    val te1  = NT.mk_variable_term e1
			    val te2  = NT.mk_variable_term e2
			    val te   = NT.mk_variable_term e
			    (* In the case of a State-comb, we generate a different progress statement. *)
			    val (exlst, tail, set') =
				case decprop of
				    SOME (v, p, ndprop, dprop) =>
				    let val vid  = getIdPat 7 v
					val pid  = getIdPat 8 p
					val tvid = NT.mk_variable_term vid
					val tpid = NT.mk_variable_term pid
					val ev   =
					    if is_state_comb
					    then NT.mk_nuprl_es_pred_term teo te
					    else te
					val srel = NT.mk_nuprl_classrel_term teo T ncls ev tpid
					val xrel = NT.mk_nuprl_classrel_term teo A term te tvid
					val and1 = NT.mk_nuprl_and_term srel ndprop
					val and2 = NT.mk_nuprl_and_term xrel and1
					val set' = IDS.add (set, e)
				    in ([(vid, A), (pid, T)], and2, set')
				    end
				  | NONE =>
				    let val a    = A.newIdIdSet set "a"
					val ta   = NT.mk_variable_term a
					val xrel = NT.mk_nuprl_classrel_term teo A term te ta
					val set' = IDS.add (IDS.add (set, a), e)
				    in ([(a, A)], xrel, set')
				    end
			    val lt   = if is_state_comb
				       then NT.mk_nuprl_es_le_term   teo te te2
				       else NT.mk_nuprl_es_locl_term teo te te2
			    val le   = if is_state_comb
				       then NT.mk_nuprl_es_locl_term teo te1 te
				       else NT.mk_nuprl_es_le_term   teo te1 te
			    val and1 = NT.mk_nuprl_and_term lt tail
			    (* and1: (e < e2) /\ (a in X(e)) *)
			    val and2 = NT.mk_nuprl_and_term le and1
			    (* and2: (e1 <= e) /\ (e < e2) /\ (a in X(e)) *)
			    val ex1  = NT.mk_nuprl_exists_term (NT.mk_nuprl_esE_term teo) (e, and2)
			    (* ex1: (exists e : E. (e1 <= e) /\ (e < e2) /\ (T. a in X(e))) *)
			    val ex2  = foldr (fn ((v, T), B) => NT.mk_nuprl_exists_term T (v, B)) ex1 exlst
			(* ex2: (exists a : T. exists e : E. (e1 <= e) /\ (e < e2) /\ (T. a in X(e))) *)
			in (SOME ex2, set')
			end
		   else raise Fail ""
		end
	      | NONE => (NONE, set)
	val (tvars, hyps) =
	    case Xop of
		SOME (v, nwcls, wcls) =>
		let val ity   = getItyAtLab context (A.getLabel wcls) ""
		    val force = false
		    val add   = true
		    val nty   = toNuprlIty 21 force add context IDENTS.empty (E.stripIty ity) ren E.emptyETVS
		in if NT.is_nuprl_eclass_term nty andalso List.length es >= 2
		   then let val (info,_,_,A) = NT.dest_eclass nty
			    val e1   = List.hd es
			    val e2   = List.last es
			    val te1  = NT.mk_variable_term e1
			    val te2  = NT.mk_variable_term e2
			    val vid  = getIdPat 7 v
			    val tvid = NT.mk_variable_term vid
			    val xrel = NT.mk_nuprl_classrel_term teo A nwcls te1 tvid
			(*val lt   = NT.mk_nuprl_es_locl_term teo te1 te2*)
			in ([(vid, A)], [xrel])
			end
		   else raise Fail ""
		end
	      | NONE => ([], [])
	val imp1   = foldr (fn (c, term) => NT.mk_nuprl_implies_term c term)  term cs
	val imp2   = foldr (fn (c, term) => NT.mk_nuprl_implies_term c term)  imp1 hyps
	val limp1  = foldr (fn (l, term) => NT.mk_nuprl_implies_term l term)  imp2 ls
	val limp2  = case exop of SOME t => NT.mk_nuprl_implies_term t limp1 | NONE => limp1
	val limp3  = if single_val
		     then foldr (fn (a, term) => NT.mk_nuprl_implies_term a term) limp2 clprms
		     else limp2
	val limp4  = foldr (fn (hyp, term) => NT.mk_nuprl_implies_term hyp term) limp3 assumptions
	val all1   = foldr (fn ((v, ty), term) => NT.mk_nuprl_all_term ty (v, term)) limp4 tvars
	val all2   = foldr (fn (v, term) => NT.mk_nuprl_all_term T (v, term)) all1 vs
	val all3   = MAPID.foldri (fn (v, T, term) => NT.mk_nuprl_all_term T (v, term)) all2 mapargs
	(* we bind the events *)
	val term1  =
	    foldr (fn (e, term) =>
		      NT.mk_nuprl_all_term
			  (NT.mk_nuprl_esE_term teo)
			  (e, term))
		  all3
		  es
	(* we bind the event ordering *)
	val term2  =
	    NT.mk_nuprl_all_term
		(NT.mk_nuprl_event_orderingp_term "i" (mk_nuprl_hidden_msg_fun ()))
		(eo, term1)
	val term3  =
	    foldr (fn ((param, (nity, nts, propop)), n_term) =>
		      let val typ = if NT.is_nuprl_type_term nity then n_atyp else nity
			  val typ' =
			      case propop of
				  NONE => typ
				| SOME (prop,params) => NT.mk_set_term typ (param, prop)
		      in foldr (NT.mk_nuprl_all_term n_typ)
			       (NT.mk_nuprl_all_term typ' (param, n_term))
		      	       nts
		      end)
		  term2
		  nparms
	val term4  =
	    BTV.foldr (fn (str, term) =>
			  NT.mk_nuprl_all_term
			      (NT.mk_nuprl_type_term  "i'")
			      (str, term))
		      term3
		      ren
	val thname = get_user_name name
	val thm    = NT.mk_nuprl_itheorem_term thname term4
    in thm
    end

fun toNuprlCombComp n_exp n_exps bprior optop =
    let val list =
	    foldr (fn ((exp, prg), list) =>
		      NT.mk_nuprl_cons_term exp list)
		  (NT.mk_nuprl_nil_term)
		  n_exps
	val set  = get_all_ids_list [n_exp, list]
	val id   = A.newIdIdSet set "n"
	val tid  = NT.mk_variable_term id
	val sel  = NT.mk_lambda_term id (NT.mk_nuprl_select_term tid list)
	(* NOTE: sel is the list of classes as a function. *)
	val fid  = A.newIdIdSet set "w"
	val sid  = A.newIdIdSet set "s"
	val tfid = NT.mk_variable_term fid
	val tsid = NT.mk_variable_term sid
	val lid  = A.newIdIdSet set "loc"
	val thd  = let val tlid = NT.mk_variable_term lid
		   in NT.mk_apply_term n_exp tlid
		   end
	val nt'  = #1 (foldr (fn (_, (nt, n)) =>
				 let val n_term = NT.mk_nuprl_small_natural_number_term n
				     val app1   = NT.mk_apply_term tfid n_term
				     val app2   = NT.mk_apply_term nt app1
				 in (app2, n + 1)
				 end)
			     (thd, 0)
			     n_exps)
	val app  = NT.mk_apply_term nt' tsid
	val lam0 = NT.mk_lambda_term sid app
	val lam1 = NT.mk_lambda_term fid lam0
	val lam2 = NT.mk_lambda_term lid lam1
    in if bprior
       then case optop of
		SOME (e_opt, p_opt) => NT.mk_nuprl_rec_comb_term lam2 sel e_opt
	      | NONE                => NT.mk_nuprl_rec_combined_loc_class_term lam2 sel
       else NT.mk_nuprl_combined_loc_class_term lam2 sel
    end

fun double x = (x, x)

fun toLift n exp blift bconcat bprior =
    let val (liftA, concatA, liftB, concatB) =
	    case n of
		0 => (NT.mk_nuprl_lifting_loc1_term,
		      NT.mk_nuprl_concat_lifting_loc1_term,
		      NT.mk_nuprl_lifting_loc0_term,
		      NT.mk_nuprl_concat_lifting_loc0_term)
	      | 1 => (NT.mk_nuprl_lifting_loc2_term,
		      NT.mk_nuprl_concat_lifting_loc2_term,
		      NT.mk_nuprl_lifting_loc1_term,
		      NT.mk_nuprl_concat_lifting_loc1_term)
	      | 2 => (NT.mk_nuprl_lifting_loc3_term,
		      NT.mk_nuprl_concat_lifting_loc3_term,
		      NT.mk_nuprl_lifting_loc2_term,
		      NT.mk_nuprl_concat_lifting_loc2_term)
	      | 3 => (NT.mk_nuprl_lifting_loc4_term,
		      NT.mk_nuprl_concat_lifting_loc4_term,
		      NT.mk_nuprl_lifting_loc3_term,
		      NT.mk_nuprl_concat_lifting_loc3_term)
	      | _ => let val nt =
			     if bprior
			     then NT.mk_nuprl_small_natural_number_term (n + 1)
			     else NT.mk_nuprl_small_natural_number_term n
		     in (NT.mk_nuprl_lifting_loc_gen_term nt,
			 NT.mk_nuprl_concat_lifting_loc_gen_term nt,
			 NT.mk_nuprl_lifting_loc_gen_term nt,
			 NT.mk_nuprl_concat_lifting_loc_gen_term nt)
		     end
	fun F flift fconcat =
	    if blift
	    then if bconcat
		 then fconcat exp
		 else flift exp
	    else exp
    in if bprior
       then F liftA concatA
       else F liftB concatB
    end

val default_strict = NT.mk_inr_term NT.mk_nuprl_it_term

fun toRecCombProg context Xprs F bprior opt typ =
    let val n     = List.length Xprs
	val set   = get_all_ids_list (F :: Xprs)
	val id    = A.newIdIdSet set "n"
	val tid   = NT.mk_variable_term id
	val lst   = NT.mk_nuprl_finite_list_term Xprs
	val sel   = NT.mk_nuprl_select_term tid lst
	val FXprs = NT.mk_lambda_term id sel
	(* FXprs is of the form: \n.[X1;X2;X3;X4][n] *)
	val num   = NT.mk_nuprl_small_integer_term n
	val l     = A.newIdIdSet set "l"
	val g     = A.newIdIdSet set "g"
	val p     = A.newIdIdSet set "p"
	val t_l   = NT.mk_variable_term l
	val t_g   = NT.mk_variable_term g
	val t_p   = NT.mk_variable_term p
	val tl    = if bprior then [p]   else []
	val t_tl  = if bprior then [t_p] else []
	val tab   = List.tabulate (n, fn x => x)
	val apgs  = map (NT.mk_apply_term t_g o NT.mk_nuprl_small_integer_term) tab
	(* apgs is of the form: [g 1;g 2; g 3;g 4] *)
	val app   = NT.mk_nuprl_applies_term F (t_l :: (apgs @ t_tl))
	(* app is of the form: (F l [g 1;g 2; g 3;g 4] p) *)
	val f     = NT.mk_nuprl_lambdas_term ([l, g] @ tl) app
	(* f is of the form: \l. \g. \p. (F l [g 1;g 2; g 3;g 4] p) *)
    in if bprior
       then case opt of
		SOME (_, init) =>
		if is_newprog_context context
		then raise Fail "toRecCombProg:newprog(1)"
		else NT.mk_nuprl_rec_comb_prc_term typ num FXprs init f default_strict
	      | NONE =>
		if is_newprog_context context
		then raise Fail "toRecCombProg:newprog(2)"
		else  NT.mk_nuprl_rec_combined_loc_class1_prc_term typ num FXprs f
       else if is_newprog_context context
       then raise Fail "toRecCombProg:newprog(3)"
       else NT.mk_nuprl_loc_comb1_prc_term typ num FXprs f
    end

fun preToNuprlCombComp' context (n_exp, n_prg) exps bprior opt typ blift bconcat =
    let val (f0, f1, f2, f3) =
	    if bprior
	    then case opt of
		     SOME (e_opt, p_opt) =>
		     (NT.mk_nuprl_rec_combined0_loc_class_opt_term e_opt,
		      NT.mk_nuprl_rec_combined1_loc_class_opt_term e_opt,
		      NT.mk_nuprl_rec_combined2_loc_class_opt_term e_opt,
		      NT.mk_nuprl_rec_combined3_loc_class_opt_term e_opt)
		   | NONE => (NT.mk_nuprl_rec_combined0_loc_class_term,
			      NT.mk_nuprl_rec_combined1_loc_class_term,
			      NT.mk_nuprl_rec_combined2_loc_class_term,
			      NT.mk_nuprl_rec_combined3_loc_class_term)
	    else (NT.mk_nuprl_combined0_loc_class_term,
		  NT.mk_nuprl_combined1_loc_class_term,
		  NT.mk_nuprl_combined2_loc_class_term,
		  NT.mk_nuprl_combined3_loc_class_term)
	val n = List.length exps
	fun lift f = toLift n f blift bconcat bprior
	val class =
	    case exps of
		[] => f0 (lift n_exp)
	      | [(ne, _)] => f1 (lift n_exp) ne
	      | [(ne1, _), (ne2, _)] => f2 (lift n_exp) ne1 ne2
	      | [(ne1, _), (ne2, _), (ne3, _)] => f3 (lift n_exp) ne1 ne2 ne3
	      | _ => toNuprlCombComp (lift n_exp) exps bprior opt
	val prog = toRecCombProg context (map (fn (_, x) => x) exps) (lift n_prg) bprior opt typ
    in (class, prog)
    end

fun preToNuprlCombComp context (n_exp, n_prg) exps bprior opt label blift bconcat =
    if is_newprog_context context
    then if blift andalso bconcat andalso not bprior andalso not (Option.isSome opt)
	 then case exps of
		  [] => raise Fail "preToNuprlCombComp:newprog:nil"
		| [(exp,prog)] =>
		  (NT.mk_nuprl_eclass0_term n_exp exp,
		   NT.mk_nuprl_eclass0_program_term n_prg prog)
		| ((exp,prog) :: lst) =>
		  let fun aux _ [] = raise Fail "preToNuprlCombComp:newprog:aux:nil"
			| aux (e,p) [(exp,prog)] =
			  (NT.mk_nuprl_eclass2_term e exp,
			   NT.mk_nuprl_eclass2_program_term p prog)
			| aux (e,p) ((exp,prog) :: lst) =
			  let val pair =
				  (NT.mk_nuprl_eclass3_term e exp,
				   NT.mk_nuprl_eclass3_program_term p prog)
			  in aux pair lst
			  end
		      val pair =
			  (NT.mk_nuprl_eclass1_term n_exp exp,
			   NT.mk_nuprl_eclass1_program_term n_prg prog)
		  in aux pair lst
		  end
	 else raise Fail "preToNuprlCombComp:newprog"
    else let val ity   = getItyAtLab context label "we should have generated a type for simple/rec combinators"
	     val force = true
	     val typ   = toNuprlIty_em 14 force context (E.stripIty ity)
	 in preToNuprlCombComp' context (n_exp, n_prg) exps bprior opt typ blift bconcat
	 end

fun get_pair_types [] map = []
  | get_pair_types ((term, etyp) :: lst) map =
    if NT.is_nuprl_variable_term term
    then let val var = NT.dest_variable term
	 in case MAPID.find (map, var) of
		SOME ntyp => (ntyp, etyp) :: (get_pair_types lst map)
	      | NONE => raise Fail ("get_pair_types:no-type-for-arg(" ^ var ^ ")")
	 end
    else raise Fail "get_pair_types:not-var"

fun set_missing var typ [] = []
  | set_missing var typ (lst as ((x : string, optyp) :: missings)) =
    if var = x
    then if Option.isSome optyp
	 then lst
	 else (x, SOME typ) :: missings
    else (x, optyp) :: (set_missing var typ missings)

fun get_type_vars [] missings = missings
  | get_type_vars ((ntyp, etyp) :: pairs) missings =
    if NT.is_nuprl_variable_term ntyp
    then let val var = NT.dest_variable ntyp
	 in get_type_vars pairs (set_missing var etyp missings)
	 end
    else if NT.is_nuprl_function_term ntyp
    then case E.destArrowType etyp of
	     SOME (eA, eB) =>
	     let val (nA, nB) = NT.dest_simple_function ntyp
	     in get_type_vars ((nA, eA) :: (nB, eB) :: pairs) missings
	     end
	   | NONE => raise Fail "get_type_vars:types-do-not-match-function"
    else if NT.is_nuprl_bag_term ntyp
    then case E.destBagType etyp of
	     SOME eA =>
	     let val nA = NT.dest_bag ntyp
	     in get_type_vars ((nA, eA) :: pairs) missings
	     end
	   | NONE => raise Fail "get_type_vars:types-do-not-match-bag"
    else get_type_vars pairs missings

fun find_missings_args_tr opid (wf_lemma :: _) nsubs typ =
    (case NT.subterms (NT.rterm2term wf_lemma) of
	 [wf] =>
	 let val (alls, mem) = NT.dest_alls_ualls (NT.rterm2term wf)
	     val typ_map =
		 foldr
		     (fn ((var, typ, b), map) =>
			 if b (* it's a all and not a uall *)
			 then MAPID.insert (map, var, typ)
			 else map)
		     MAPID.empty
		     alls
	     val (prc_typ, prc) = NT.dest_member mem
	     val (prc_opid, prc_subs) = NT.dest_simple_term prc
	 in if prc_opid = opid ^ "-prc"
	    then let val n = List.length prc_subs - (nsubs + 1)
		 in if n >= 0
		    then let val (targs, tcls) = E.destArrowsType typ
			     val terms = List.drop (prc_subs, n + 1)
			 in if List.length targs = List.length terms
			    then let val terms'    = map NT.rterm2term terms
				     val pairs     = get_pair_types (ListPair.zip (terms', targs)) typ_map
				     val missings  = List.take (prc_subs, n)
				     val missings' = map (fn t => (NT.dest_variable (NT.rterm2term t), NONE)) missings
				 in map (fn (v, SOME x) => (v, x)
					  | (v, NONE) => raise Fail ("find_missings_args_tr:(" ^ v ^ ")missing"))
					(get_type_vars pairs missings')
				 end
			    else raise Fail "find_missings_args_tr:eml-type-does-not-match"
			 end
		    else raise Fail "find_missings_args_tr:prc-does-not-have-enough-subterms"
		 end
	    else raise Fail "find_missings_args_tr:prc-name-does-not-match"
	 end
       | _ => raise Fail "find_missings_args_tr:more-than-one-subterms-in-wf")
  | find_missings_args_tr _ _ _ _ = raise Fail "find_missings_args_tr:no-wf"

(* Transformation of a class into a program *)
fun class2prog_tr context abs class typ Xtyp =
    let val opid = NT.opid_of_term class
	(*val _ = print (":" ^ opid ^ "\n" ^ E.ppIty' Xtyp ^ "\n-\n")*)
    in if opid = "rec-comb"
       then let val (Xs, F, init) = NT.dest_rec_comb class
		val num =
		    if NT.is_nuprl_lambda_term Xs
		    then let val (v,B) = NT.dest_lambda 10 Xs
			 in if NT.is_nuprl_select_term B
			    then let val (n,lst) = NT.dest_select B
				 in List.length (NT.dest_list lst)
				 end
			    else raise Fail "class2prog_tr"
			 end
		    else raise Fail "class2prog_tr"
		val n = NT.mk_nuprl_small_integer_term num
		(*val _ = print ("---rc\n")*)
	    in NT.mk_nuprl_rec_comb_prc_term typ n Xs init F default_strict
	    end
       else if opid = "bind-class"
       then raise Fail "class2prog_tr:bind-class"
       else if opid = "parallel-class"
       then raise Fail "class2prog_tr:parallel-class"
       else (*if opid = "let"
       then let val (exp1, v, exp2) = NT.dest_let class
		val prg1 = class2prog_tr abs exp1 ...
		val prg2 = class2prog_tr abs exp2 typ
	    in NT.mk_nuprl_let_term prog1 (v, prg2)
	    end
       else*)
	   let val prc = opid ^ "-prc"
	   in case NT.MAP.find (!abs, prc) of
		  SOME [ritem] =>
		  let val {id, sign, obid, lhs, rhs, wfs} = NT.get ritem
		      val (_,subs) = NT.dest_simple_term class
		      val ns = #2 (sign : NT.sign)
		      val nsubs = List.length subs
		      val subs' = map NT.rterm2term subs
		  in if List.all (fn x => x = 0) ns
		     then if List.length ns = nsubs + 1
			  (* NOTE: we could do something smarter here because
			   * class X is also mentioned in X-prc_wf *)
			  then NT.mk_nuprl_simple_term prc (typ :: subs')
			  else if List.length ns > nsubs + 1
			  (* then the abstractions takes extra parameters, most certainly types.*)
			  (* NOTE: Here we should really do something else! *)
			  then let val _ = print ("[trying to find missing arguments for " ^ opid ^ " (prc)]\n")
				   val missings = find_missings_args_tr opid wfs nsubs Xtyp
				   val force = true
				   val typs =
				       map (fn (x, t) =>
					       let (*val _ = print ("   :" ^ x ^ "\n")*)
						   (*val _ = print ("   :" ^ E.ppIty' t ^ "\n")*)
					       in toNuprlIty_em 19 force context t
					       end)
					   missings
			       in NT.mk_nuprl_simple_term prc (typs @ (typ :: subs'))
			       end
			  (* let val n = List.length ns - (List.length subs + 1)
				 val terms = map (fn _ => NT.mk_nuprl_it_term) (List.tabulate (n, fn _ => ()))
			     in NT.mk_nuprl_simple_term prc (terms @ (typ :: subs))
			     end*)
			  else raise Fail "class2prog:prc-1:prc-does-not-have-enough-parameters"
		     else raise Fail "class2prog:prc-2:prc-with-binders"
		  end
		| SOME []  => raise Fail ("class2prog:prc-3(" ^ prc ^ ")missing")
		| NONE     => raise Fail ("class2prog:prc-4(" ^ prc ^ ")missing")
		| SOME lst => raise Fail ("class2prog:prc-5(" ^ prc ^ ")more-than-one(" ^ Int.toString (List.length lst) ^ ")")
	   end
	   (*handle _ =>
		  let (*val _ = print (">" ^ NT.opid_of_term class ^ "\n")*)
		      val (_, _, _, lhs, rhs) = NT.find_sign abs class
		  in class2prog_tr abs (NT.unfold_ab NONE class lhs rhs) typ
		  end*)
    end

fun class2newprog_tr context abs class =
    let val opid = NT.opid_of_term class
	val prog = opid ^ "-program"
    in case NT.MAP.find (!abs, prog) of
	   SOME [ritem] =>
	   let val {id, sign, obid, lhs, rhs, wfs} = NT.get ritem
	       (* -- subs: subterms of class, e.g., [F,X] for eclass1(F,X) -- *)
	       val (_,subs) = NT.dest_simple_term class
	       (* -- ns: signature of the program -- *)
	       val ns       = #2 (sign : NT.sign)
	       (* -- nsubs: number of subterms -- *)
	       val nsubs    = List.length subs
	   in if List.all (fn x => x = 0) ns
	      then if List.length ns = nsubs
		   then NT.mk_nuprl_simple_term prog (map NT.rterm2term subs)
		   else raise Fail "class2newprog:prog-1:prog-does-not-have-correct-number-of-parameters"
	      else raise Fail "class2newprog:prog-2:prog-with-binders"
	   end
	 | SOME []      => raise Fail ("class2newprog:prc-3(" ^ prog ^ ")missing")
	 | NONE         => raise Fail ("class2newprog:prc-4(" ^ prog ^ ")missing")
	 | SOME lst     => raise Fail ("class2newprog:prc-5(" ^ prog ^ ")more-than-one(" ^ Int.toString (List.length lst) ^ ")")
    end

fun class2prog context label class =
    case get_ndefs_context context of
	SOME {abs, tof} =>
	let val id  = NT.opid_of_term class
	    val n   = List.length (NT.bterms_of_term class)
	    (*val (x,xs) = NT.dest_applies class*)
	    (*val _ = print (":" ^ NT.opid_of_term x ^ ":" ^ id ^ ":" ^ Int.toString (List.length xs) ^ "\n")*)
	    val ity = E.stripIty (getItyAtLab context label ("We shoud have generated a type for identifier " ^ id))
	    val (lst, ity') = E.destArrowsType ity
	    val prog = (* if n <> length lst then we've got to add apps to the class
			* to get a class and then put back some lambdas *)
		if List.length lst = n
		then case E.destClassType ity' of
			 SOME cls =>
			 if is_newprog_context context
			 then class2newprog_tr context abs class
			 else let val force = true
				  val typ   = toNuprlIty_em 15 force context cls
			      in class2prog_tr context abs class typ ity
			      end
		       | NONE => class
		else class
	in prog
	end
      | NONE => class

fun transfun #" " = "\\ "
  | transfun x = Char.toString x

val transform = String.translate transfun

fun protect #"(" = "\\("
  | protect #")" = "\\)"
  | protect #"{" = "\\{"
  | protect #"}" = "\\}"
  | protect #"\\" = "\\\\"
  | protect #":" = "\\:"
  | protect #";" = "\\;"
  | protect #"," = "\\,"
  | protect #"." = "\\."
  | protect x = Char.toString x

val protect = String.translate protect

fun mk_base_class context set ident atm typ hdr =
    let val id   = A.getIdIdent ident ^ "'base"
	val lab  = A.getLabel ident
	val term = NT.mk_nuprl_base_headers_msg_val_term atm
	val prog =
	    if is_newprog_context context
	    then NT.mk_nuprl_base_class_program_term atm
	    else NT.mk_nuprl_base_prc_term atm typ
	val schemeop     = NONE
	val premises     = []
	val programmable = false
	val localclass   = SOME prog
	val data         = false
	val lvlop        = NONE
	val (aid,lc,ab,th,rest,context') =
	    genAbstractionAndWF
		1
		context
		""
		id
		lab
		term
		hdr
		schemeop
		premises
		programmable
		localclass
		data
		lvlop
	val _ = resetDecFP ()
    in (ab :: th :: rest, [(get_user_name id, Option.getOpt (lc, prog))])
    end

fun mk_send_function context set ident atm typ hdr =
    let val id       = A.getIdIdent ident ^ "'send"
	val lab      = A.getLabel ident
	val v        = A.newIdIdSet set "z"
	val l        = A.newIdIdSet set "l"
	val tv       = NT.mk_variable_term v
	val tl       = NT.mk_variable_term l
	(* msg: <atom, value> *)
	val msg      = NT.mk_nuprl_make_msg_term atm tv
	(* pair: <loc, <atom, value>> *)
	val pair     = NT.mk_nuprl_mk_msg_interface_term tl msg
	(* lam: \value. <loc, <atom, value>> *)
	val lam      = NT.mk_lambda_term v pair
	(* term: \loc. \value. <loc, <atom, value>> *)
	val term     = NT.mk_lambda_term l lam
	val value    = ""
	val premises = []
	val (aid,lc,ab,th,rest) =
	    genAbstractionAndWF'
		2
		context
		value
		id
		lab
		term
		hdr
		premises
	val _ = resetDecFP ()
    in (ab :: th :: rest, [(get_user_name id, aid (*term*))])
    end

fun mk_nsend_function context set ident atm typ hdr =
    let val id       = A.getIdIdent ident ^ "''send"
	val lab      = A.getLabel ident
	val v        = A.newIdIdSet set "z"
	val l        = A.newIdIdSet set "l"
	val n        = A.newIdIdSet set "n"
	val tv       = NT.mk_variable_term v
	val tl       = NT.mk_variable_term l
	val tn       = NT.mk_variable_term n
	(* msg: <atom, value> *)
	val msg      = NT.mk_nuprl_make_msg_term atm tv
	(* interface: <n, loc, <atom, value>> *)
	val intf     = NT.mk_nuprl_make_msg_interface_term tn tl msg
	(* lam1: \value. <n, loc, <atom, value>> *)
	val lam1     = NT.mk_lambda_term v intf
	(* lam2: \loc. \value. <n, loc, <atom, value>> *)
	val lam2     = NT.mk_lambda_term l lam1
	(* term: \n. \loc. \value. <n, loc, <atom, value>> *)
	val term     = NT.mk_lambda_term n lam2
	val value    = ""
	val premises = []
	val (aid,lc,ab,th,rest) =
	    genAbstractionAndWF'
		2
		context
		value
		id
		lab
		term
		hdr
		premises
	val _ = resetDecFP ()
    in (ab :: th :: rest, [(get_user_name id, aid (*term*))])
    end

fun mk_broadcast_function context set ident atm typ hdr =
    let val id       = A.getIdIdent ident ^ "'broadcast"
	val lab      = A.getLabel ident
	val v        = A.newIdIdSet set "z"
	val l        = A.newIdIdSet set "l"
	val ls       = A.newIdIdSet set "locs"
	val tv       = NT.mk_variable_term v
	val tl       = NT.mk_variable_term l
	val tls      = NT.mk_variable_term ls
	(* msg: <atom, value> *)
	val msg      = NT.mk_nuprl_make_msg_term atm tv
	(* pair: <loc, <atom, value>> *)
	val pair     = NT.mk_nuprl_mk_msg_interface_term tl msg
	(* llam:  \loc. <loc, <atom, value>> *)
	val llam     = NT.mk_lambda_term l pair
	(* bmap: bag-map(\loc. <loc, <atom, value>>,locs) *)
	val bmap     = NT.mk_nuprl_bag_map_term llam tls
	(* lam: \value. bag-map(\loc. <loc, <atom, value>>,locs) *)
	val lam      = NT.mk_lambda_term v bmap
	(* term: \locs. \value. bag-map(\loc. <loc, <atom, type, value>>,locs) *)
	val term     = NT.mk_lambda_term ls lam
	val value    = ""
	val premises = []
	val (aid,lc,ab,th,rest) =
	    genAbstractionAndWF'
		3
		context
		value
		id
		lab
		term
		hdr
		premises
	val _ = resetDecFP ()
    in (ab :: th :: rest, [(get_user_name id, aid (*term*))])
    end

fun mk_nbroadcast_function context set ident atm typ hdr =
    let val id       = A.getIdIdent ident ^ "''broadcast"
	val lab      = A.getLabel ident
	val v        = A.newIdIdSet set "z"
	val l        = A.newIdIdSet set "l"
	val ls       = A.newIdIdSet set "locs"
	val n        = A.newIdIdSet set "n"
	val tv       = NT.mk_variable_term v
	val tl       = NT.mk_variable_term l
	val tls      = NT.mk_variable_term ls
	val tn       = NT.mk_variable_term n
	(* msg: <atom, value> *)
	val msg      = NT.mk_nuprl_make_msg_term atm tv
	(* interface: <loc, <atom, value>> *)
	val intf     = NT.mk_nuprl_make_msg_interface_term tn tl msg
	(* llam:  \loc. <loc, <atom, value>> *)
	val llam     = NT.mk_lambda_term l intf
	(* bmap: bag-map(\loc. <loc, <atom, value>>,locs) *)
	val bmap     = NT.mk_nuprl_bag_map_term llam tls
	(* lam1: \value. bag-map(\loc. <loc, <atom, value>>,locs) *)
	val lam1     = NT.mk_lambda_term v bmap
	(* lam2: \locs. \value. bag-map(\loc. <loc, <atom, value>>,locs) *)
	val lam2     = NT.mk_lambda_term ls lam1
	(* term: \locs. \value. bag-map(\loc. <loc, <atom, value>>,locs) *)
	val term     = NT.mk_lambda_term n lam2
	val value    = ""
	val premises = []
	val (aid,lc,ab,th,rest) =
	    genAbstractionAndWF'
		3
		context
		value
		id
		lab
		term
		hdr
		premises
	val _ = resetDecFP ()
    in (ab :: th :: rest, [(get_user_name id, aid (*term*))])
    end

fun fancy_mk_nuprl_assert_term term =
    let val opid = NT.opid_of_term term
    in case opid of
	   "lt_int" => let val (i1,i2) = NT.dest_lt_int term
		       in NT.mk_nuprl_less_than_term i1 i2
		       end
	 | "le_int" => let val (i1,i2) = NT.dest_le_int term
		       in NT.mk_nuprl_le_term i1 i2
		       end
	 | "eq_int" => let val (i1,i2) = NT.dest_eq_int term
		       in NT.mk_equal_term NT.mk_int_term i1 i2
		       end
	 | "eq_id"  => let val (i1,i2) = NT.dest_eq_id term
		       in NT.mk_equal_term NT.mk_nuprl_loc_term i1 i2
		       end
	 | "band"   => let val (b1,b2) = NT.dest_band term
			   val t1 = fancy_mk_nuprl_assert_term b1
			   val t2 = fancy_mk_nuprl_assert_term b2
		       in NT.mk_nuprl_and_term t1 t2
		       end
	 | "bnot"   => let val b = NT.dest_bnot term
		       in NT.mk_nuprl_not_term (fancy_mk_nuprl_assert_term b)
		       end
	 | "apply"  => let val (f,args) = NT.dest_applies term
		       in case (NT.opid_of_term f, args) of
			      ("eqof", [t1,t2]) =>
			      (case NT.dest_simple_term (NT.dest_eqof f) of
				   ("product-deq", [A,B,eqA,eqB]) =>
				   let val tA = NT.rterm2term A
				       val tB = NT.rterm2term B
				       val typ = NT.mk_prod_term tA tB
				   in NT.mk_equal_term typ t1 t2
				   end
				 | _ => NT.mk_nuprl_assert_term term)
			    | _ => NT.mk_nuprl_assert_term term
		       end
	 | _ => NT.mk_nuprl_assert_term term
    end

(* Transform a term into a nuprl term *)
fun toNuprlTerm context (term as A.N {kind = (A.SCON, A.SCON_INT),    label, value, regions, parents, children = []}) = double (NT.mk_nuprl_int_from_string value)
  | toNuprlTerm context (term as A.N {kind = (A.SCON, A.SCON_REAL),   label, value, regions, parents, children = []}) = double (NT.mk_nuprl_real_from_string value)
  | toNuprlTerm context (term as A.N {kind = (A.SCON, A.SCON_ATOM),   label, value, regions, parents, children = []}) = double (NT.mk_regular_token_term value)
  | toNuprlTerm context (term as A.N {kind = (A.SCON, A.SCON_ATOMS),  label, value, regions, parents, children = []}) = raise Fail ("impossible:" ^ minStr)
  | toNuprlTerm context (term as A.N {kind = (A.SCON, A.SCON_STRING), label, value, regions, parents, children = []}) = raise Fail ("impossible:" ^ minStr)

  (* ID *)
  | toNuprlTerm context (term as A.N {kind = (A.ID, A.ID_VID),    label, value, regions, parents, children = []}) =
    (case value of
	 "true"  => double (NT.mk_nuprl_btrue_term)
       | "false" => double (NT.mk_nuprl_bfalse_term)
       | "nil"   => double (NT.mk_nuprl_nil_term)
       | "="     => let val ity  = getItyAtLab context label "We shoud have generated a type for this equality"
			val ity1 = E.stripIty ity
			val ity2 = ity1 (*Option.valOf (E.destDeqType ity1)
					 handle _ => (print (E.toStringIty ity1);
						      raise Fail "impossible:")*)
		    in double (toDeq context ity2)
		    end
       | _ =>
	 (case get_arity_context_op context value of
	      NONE =>
	      (* NONE means that the id (value) is not a constant,
	       * it is a user defined function *)
	      (case getUserFP (is_bound_context context) value of
		   (* We sometimes have to add parameters to the argument list
		    * of a used function, if it does use the parameters. *)
		   NONE => double (NT.mk_variable_term value)
		 (* NONE means that the id (value) is not a top-level dec
		  * but a local one. *)
		 | SOME params =>
		   let val a_terms = map NT.mk_variable_term params
		       val _       = addDecFPList params
		       val uterm   = mk_nuprl_user_term value a_terms
		   in if is_cbva_context context
		      then (uterm, NT.mk_variable_term (get_user_name value))
		      else double uterm
		   end)
	    | SOME (idname, lst) =>
	      if is_param_context context value
	      then let (*val _ = print ("--param(1): " ^ value ^ "\n")*)
		   in if List.null lst
		      then double (param_kind_to_nuprl context value)
		      else raise Fail "impossible:toNuprlTerm:VID"
		   end
	      else let (*val _ = print ("--non param(1): " ^ value ^ "\n")*)
		       val name =
			   case EN.getNameAtLab (getUnifEnv ()) label of
			       NONE => idname
			     | SOME name => name
		       val set = IDS.empty
		       val (vars,set') =
			   List.foldl (fn ((id, args), (vars, set)) =>
					  let val set' = IDS.addList (set, name :: args)
					  in (vars @ [(A.newIdIdSet set id, args)], set')
					  end)
				      ([],set)
				      lst
		       val s_terms =
			   map (fn (id, args) =>
				   (map NT.mk_new_nuprl_var args,
				    foldl (fn (arg, term) =>
					      let val t_arg = NT.mk_variable_term arg
					      in NT.mk_apply_term term t_arg
					      end)
					  (NT.mk_variable_term id)
					  args))
			       vars
		       val s_terms =
			   if idname = "es-loc"
			   then ([], mk_nuprl_hidden_event_ordering ()) :: s_terms
			   else s_terms
		       val term = NT.mk_nuprl_term (name, []) s_terms
		   in double (foldr (fn ((id, _), term) =>
					NT.mk_lambda_term id term)
				    term
				    vars)
		   end))
  | toNuprlTerm context (term as A.N {kind = (A.ID, A.ID_TYCON),  label, value, regions, parents, children = []}) = raise EH.Unimplemented ""
  | toNuprlTerm context (term as A.N {kind = (A.ID, A.ID_TYVAR),  label, value, regions, parents, children = []}) = raise EH.Unimplemented ""

  (* EXP *)
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_OR),       label, value, regions, parents, children = [e1, e2]})      =
    let val (nt1, pr1) = toNuprlTerm context e1
	val (nt2, pr2) = toNuprlTerm context e2
    in (NT.mk_nuprl_bor_term nt1 nt2,
	NT.mk_nuprl_bor_term pr1 pr2)
    end
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_AND),      label, value, regions, parents, children = [e1, e2]})      =
    let val (nt1, pr1) = toNuprlTerm context e1
	val (nt2, pr2) = toNuprlTerm context e2
    in (NT.mk_nuprl_band_term nt1 nt2,
	NT.mk_nuprl_band_term pr1 pr2)
    end
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_TYPED),    label, value, regions, parents, children = [e, t]})        =
    raise Fail ("impossible:" ^ minStr)
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_LAMBDA),   label, value, regions, parents, children = [p, e]})        =
    let val id       = getIdPat 4 p
	(*val _ = print ("[lambda]\n" ^ A.toString term ^ "\n")*)
	val context' = add_bound_context context id
	val (nt, pr) = toNuprlTerm context' e
    in (NT.mk_lambda_term id nt,
	NT.mk_lambda_term id pr)
    end
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_ITE),      label, value, regions, parents, children = [e1, e2, e3]})  =
    let val (nt1, pr1) = toNuprlTerm context e1
	val (nt2, pr2) = toNuprlTerm context e2
	val (nt3, pr3) = toNuprlTerm context e3
    in (NT.mk_nuprl_ite_term nt1 nt2 nt3,
	NT.mk_nuprl_ite_term pr1 pr2 pr3)
    end
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_ATEXP),    label, value, regions, parents, children = [ae]})          =
    toNuprlTerm context ae
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_APP),      label, value, regions, parents, children = [f, x]})        =
    let val (t, ts) = flattenExpApp f
	val nts = map (toNuprlTerm context) (ts @ [x])
    in case t of
	   A.N {kind = (A.EXP, A.EXP_ATEXP), label, value, regions, parents, children = [atexp]} =>
	   (case atexp of
		A.N {kind = (A.ATEXP, A.ATEXP_ID), label, value, regions, parents, children = [v]} =>
		let val id  = A.getIdIdent v
		    val lab = A.getLabel v
		in case get_arity_context_op context id of
		       NONE => (* NONE means that id is a user defined identifier *)
		       let (*val _ = print ("[" ^ id ^ " is a user defined identifier]\n")*)
			   val ((nFirstElt, pFirstElt), list) =
			       if is_fabs_context context id
			       then (hd nts, tl nts)
			       else (toNuprlTerm context v, nts)
			   val (napp, papp) =
			       foldl (fn ((nt_arg, pr_arg), (nt_func, pr_func)) =>
					 (NT.mk_apply_term nt_func nt_arg,
					  NT.mk_apply_term pr_func pr_arg))
				     (nFirstElt, pFirstElt)
				     list
		       in (deq2eq napp, deq2eq papp)
		       end
		     | SOME (idname, lst) =>
		       if is_param_context context id
		       then let (*val _ = print ("--param(2): " ^ id ^ "\n")*)
			    in if List.null lst
			       then foldl (fn ((nt_arg, pr_arg), (nt_func, pr_func)) =>
					      (NT.mk_apply_term nt_func nt_arg,
					       NT.mk_apply_term pr_func pr_arg))
					  (double (param_kind_to_nuprl context id))
					  nts
			       else raise Fail "impossible:toNuprlTerm:APP"
			    end
		       else let (*val _ = print ("--non param(2): " ^ id ^ "\n")*)
				val n  = List.length lst
				val ln = List.length nts
				val (params, args, bound) =
				    if ln >= n
				    then let val list1 = List.take (nts, n)
					     val list2 = List.drop (nts, n)
					 in (list1, list2, [])
					 end
				    else let val vars = List.tabulate (n - ln, fn _ => A.newId ())
					     (*val vars = map (fn (id, _) => A.newIdId id) (List.drop (lst, ln))*)
					     val nvars = map (fn v => double (NT.mk_variable_term v)) vars
					 in (nts @ nvars, [], vars)
					 end
				val b_terms =
				    map (fn ((id, prms), (nt_arg, pr_arg)) =>
					    let val m = List.length prms
						fun toPair arg =
						    let val (bounds, body) = NT.dest_lambdas 5 arg
							val bounds = map NT.dest_nuprl_var bounds
							val bl = List.length bounds
						    in if bl >= m
						       then let val l1 = List.take (bounds, m)
								val l2 = List.drop (bounds, m)
							    in (map NT.mk_new_nuprl_var l1, NT.mk_nuprl_lambdas_term l2 body)
							    end
						       else let val vars = List.tabulate (m - bl, fn _ => A.newId ())
								val term = foldl (fn (arg, term) =>
										     NT.mk_apply_term term (NT.mk_variable_term arg))
										 body
										 vars
							    in (map NT.mk_new_nuprl_var (bounds @ vars), term)
							    end
						    end
					    in (toPair nt_arg, toPair pr_arg)
					    end)
					(ListPair.zip (lst, params))
				val name = case EN.getNameAtLab (getUnifEnv ()) lab of
					       NONE => idname
					     | SOME name => name
				val (b_nt_terms, b_pr_terms) = ListPair.unzip b_terms
				val (b_nt_terms, b_pr_terms) =
				    if idname = "es-loc"
				    then (([], mk_nuprl_hidden_event_ordering ()) :: b_nt_terms,
					  ([], mk_nuprl_hidden_event_ordering ()) :: b_pr_terms)
				    else (b_nt_terms, b_pr_terms)
				val nt_term  = NT.mk_nuprl_term (name, []) b_nt_terms
				val pr_term  = NT.mk_nuprl_term (name, []) b_pr_terms
				val pr_term' = class2prog context lab pr_term
				val (nt_app, pr_app) =
				    foldl (fn ((nt_arg, pr_arg), (nt_func, pr_func)) =>
					      (NT.mk_apply_term nt_func nt_arg,
					       NT.mk_apply_term pr_func pr_arg))
					  (nt_term, pr_term')
					  args
				val (nt_lam, pr_lam) =
				    foldr (fn (f, (nt_term, pr_term)) =>
					      (NT.mk_lambda_term f nt_term,
					       NT.mk_lambda_term f pr_term))
					  (nt_app, pr_app)
					  bound
			    in (nt_lam, pr_lam)
			    end handle Subscript => raise Fail "impossible:toNuprlTerm:APP:Subscript"
		end
	      | _ => foldl (fn ((nt_arg, pr_arg), (nt_func, pr_func)) =>
			       (NT.mk_apply_term nt_func nt_arg,
				NT.mk_apply_term pr_func pr_arg))
			   (toNuprlTerm context atexp)
			   nts)
	 | _ => foldl (fn ((nt_arg, pr_arg), (nt_func, pr_func)) =>
			  (NT.mk_apply_term nt_func nt_arg,
			   NT.mk_apply_term pr_func pr_arg))
		      (toNuprlTerm context t)
		      nts
    end
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_OP),       label, value, regions, parents, children = [e1, e2]})      =
    let val (nt1, pr1) = toNuprlTerm context e1
	val (nt2, pr2) = toNuprlTerm context e2
	fun addTypCls f =
	    let val msg = "no type for the " ^ value ^ " operator"
		val ity = getItyAtLab context label msg
		val t1  = toNuprlIty_em 8 false context (E.stripIty ity)
		val (_, t2) = NT.dest_simple_function t1
		val (_, t3) = NT.dest_simple_function t2
		val (info, es, e, typ) = NT.dest_eclass t3
	    (* NOTE: we should check that es and e do not occur in typ. *)
	    in f typ
	    end
    in case value of
	   "+"      => (NT.mk_add_term       nt1 nt2, NT.mk_add_term       pr1 pr2)
	 | "-"      => (NT.mk_subtract_term  nt1 nt2, NT.mk_subtract_term  pr1 pr2)
	 | "*"      => (NT.mk_multiply_term  nt1 nt2, NT.mk_multiply_term  pr1 pr2)
	 | "/"      => (NT.mk_divide_term    nt1 nt2, NT.mk_divide_term    pr1 pr2)
	 | "="      => (* NOTE: this use of labels assumes that we never change
		        * the label of a EXP_OP. *)
	   let val ity    = getItyAtLab context label "We shoud have generated a type for this equality"
	       val ity1   = E.stripIty ity
	       val ity2   = ity1 (*Option.valOf (E.destDeqType ity1)
				  handle _ => raise Fail "impossible:"*)
	       val deq    = NT.mk_nuprl_eqof_term (toDeq context ity2)
	       val nt_app = NT.mk_apply_term deq nt1
	       val pr_app = NT.mk_apply_term deq pr1
	   in (deq2eq (NT.mk_apply_term nt_app nt2),
	       deq2eq (NT.mk_apply_term pr_app pr2))
	   end
	 | "<>"    =>
	   let val ity = getItyAtLab context label "We shoud have generated a type for this equality"
	       val ity1   = E.stripIty ity
	       val ity2   = ity1 (*Option.valOf (E.destDeqType ity1)
				  handle _ => raise Fail "impossible:"*)
	       val deq     = NT.mk_nuprl_eqof_term (toDeq context ity2)
	       val nt_app1 = NT.mk_apply_term deq     nt1
	       val nt_app2 = NT.mk_apply_term nt_app1 nt2
	       val pr_app1 = NT.mk_apply_term deq     pr1
	       val pr_app2 = NT.mk_apply_term pr_app1 pr2
	   in (NT.mk_nuprl_bnot_term nt_app2,
	       NT.mk_nuprl_bnot_term pr_app2)
	   end
	 | "<="     => (NT.mk_nuprl_le_int_term nt1 nt2, NT.mk_nuprl_le_int_term pr1 pr2)
	 | ">="     => (NT.mk_nuprl_le_int_term nt2 nt1, NT.mk_nuprl_le_int_term pr2 pr1)
	 | "<"      => (NT.mk_nuprl_lt_int_term nt1 nt2, NT.mk_nuprl_lt_int_term pr1 pr2)
	 | ">"      => (NT.mk_nuprl_lt_int_term nt2 nt1, NT.mk_nuprl_lt_int_term pr2 pr1)
	 | "."      => (NT.mk_nuprl_cons_term   nt1 nt2, NT.mk_nuprl_cons_term   pr1 pr2)
	 | "++"     => (NT.mk_nuprl_concat_term nt1 nt2, NT.mk_nuprl_concat_term pr1 pr2)
	 | "@"      =>
	   let val cls  = NT.mk_nuprl_class_at_term nt1 nt2
	       val prog =
		   if is_newprog_context context
		   then NT.mk_nuprl_class_at_program_term pr1 pr2
		   else addTypCls (fn typ => NT.mk_nuprl_at_prc_term typ pr1 pr2)
	   in (cls, prog)
	   end
	 | "||"     =>
	   let val cls  = NT.mk_nuprl_parallel_class_term nt1 nt2
	       val prog =
		   if is_newprog_context context
		   then NT.mk_nuprl_parallel_class_program_term pr1 pr2
		   else addTypCls (fn typ => NT.mk_nuprl_parallel_prc_term typ pr1 pr2)
	   in (cls, prog)
	   end
	 | ">>="    => raise Fail "mbind:op"
	 | "until"  =>
	   let val cls  = NT.mk_nuprl_until_class_term nt1 nt2
	       val prog =
		   if is_newprog_context context
		   then NT.mk_nuprl_until_class_program_term pr1 pr2
		   else addTypCls (fn typ => NT.mk_nuprl_until_prc_term typ pr1 pr2)
	   in (cls, prog)
	   end
	 | "before" =>
	   let val es   = mk_nuprl_hidden_event_ordering ()
	       val term = NT.mk_nuprl_es_causl_term es nt1 nt2
	       val prog = NT.mk_nuprl_es_causl_term es pr1 pr2
	   in (term, prog)
	   end
	 | "l-before" =>
	   let val es   = mk_nuprl_hidden_event_ordering ()
	       val term = NT.mk_nuprl_es_locl_term es nt1 nt2
	       val prog = NT.mk_nuprl_es_locl_term es pr1 pr2
	   in (term, prog)
	   end
	 | "?"      =>
	   let val cls  =
		   if NT.opid_of_term nt1 = "primed-class"
		   then NT.mk_nuprl_primed_class_opt_term (NT.dest_primed_class nt1) nt2
		   else NT.mk_nuprl_class_opt_term nt1 nt2
	       val prog =
		   if is_newprog_context context
		   then raise Fail "toNuprl:op:?:newprog"
		   else if NT.opid_of_term pr1 = "prior-prc"
		   then let val (t,X) = NT.dest_prior_prc pr1
			    fun F typ = NT.mk_nuprl_prior_init_prc_term typ X pr2
			in addTypCls F
			end
		   else NT.mk_nuprl_class_opt_term pr1 pr2 (* opt-prc ? *)
	   in (cls, prog)
	   end
	 | "??"     => (NT.mk_nuprl_class_opt_class_term nt1 nt2,
			NT.mk_nuprl_class_opt_class_term pr1 pr2)
	 | _        =>
	   (case get_arity_context_op context value of
		NONE => (* the id is not a constant, it is a user function *)
		(case getUserFP (is_bound_context context) value of
		     (* We sometimes have to add parameters to the argument list
		      * of a used function, if it does use the parameters. *)
		     NONE => double (NT.mk_variable_term value)
		   | SOME params =>
		     let val a_terms = map NT.mk_variable_term params
			 val _       = addDecFPList params
			 val func    = mk_nuprl_user_term value a_terms
		     in (NT.mk_apply_term (NT.mk_apply_term func nt1) nt2,
			 NT.mk_apply_term (NT.mk_apply_term func pr1) pr2)
		     end)
	      | _ => raise Fail "impossible:a constant or a parameter cannot be infix")
    end
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_LET),      label, value, regions, parents, children = [b, e]})        =
    (case b of
	 A.N {kind = (A.BIND, A.BIND_DEC), label, value, regions, parents, children = [f, a, e']} =>
	 let val id         = A.getIdIdent f
	     val (nte, pre) = toNuprlTerm context e'
	     val context'   = add_bound_context context id
	     val (nt, pr)   = toNuprlTerm context' e
	 in (NT.mk_nuprl_let_term id nte nt,
	     if is_cbva_context context
	     then NT.mk_callbyvalueall_term pre (id, pr)
	     else NT.mk_nuprl_let_term id pre pr)
	 end
       | A.N {kind = (A.BIND, A.BIND_PAT), label, value = v, regions, parents, children = [p, e']} =>
	 (case p of
	      A.N {kind = (A.PAT, A.PAT_ATPAT), label, value, regions, parents, children = [atpat]} =>
	      (case atpat of
		   A.N {kind = (A.ATPAT, A.ATPAT_TUPLE), label, value, regions, parents, children} =>
		   if List.length children <= 7
		   then let val vars       = map (getIdPat 5) children
			    val (nte, pre) = toNuprlTerm context e'
			    val context'   = add_bounds_context context vars
			    val (nt, pr)   = toNuprlTerm context' e
			    val _ = if List.null vars
				    then print ("[bind_pat_atpat_tuple:unit]\n"
						^ A.toString term ^ "\n"
						^ A.export term ^ "\n")
				    else ()
			in (NT.mk_nuprl_spreadn_term nte (vars, nt),
			    if is_cbva_context context
			    then let val set = get_all_ids_list [nte, nt]
				     val id  = A.newIdIdSet set "x"
				     val spr = NT.mk_nuprl_spreadn_term (NT.mk_variable_term id) (vars, pr)
				 in NT.mk_callbyvalueall_term pre (id, spr)
				 end
			    else NT.mk_nuprl_spreadn_term pre (vars, pr))
			end
		   else raise Fail ("impossible:" ^ minStr)
		 | A.N {kind = (A.ATPAT, A.ATPAT_ID), label, value, regions, parents, children = [ident]} =>
		   let val id         = A.getIdIdent ident
		       val (nte, pre) = toNuprlTerm context e'
		       val context'   = add_bound_context context id
		       val (nt, pr)   = toNuprlTerm context' e
		   in (NT.mk_nuprl_let_term id nte nt,
		       if is_cbva_context context
		       then NT.mk_callbyvalueall_term pre (id, pr)
		       else NT.mk_nuprl_let_term id pre pr)
		   end
		 | atpat => (print (A.toString atpat ^ "\n"); raise Fail ("impossible:" ^ A.wrongFormat)))
	    | pat => (print (A.toString pat ^ "\n"); raise Fail ("impossible:" ^ minStr)))
       | _ => raise Fail ("impossible:" ^ minStr))
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_CLASS),    label, value, regions, parents, children = [b, e]})        = raise Fail ("impossible:" ^ minStr)
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_LETR),     label, value, regions, parents, children = [b, e]})        =
    (case b of
	 A.N {kind = (A.BIND, A.BIND_DEC), label, value, regions, parents, children = [f, a, e']} =>
	 let val fid        = A.getIdIdent f
	     val context'   = add_bound_context context fid
	     val (nte, pre) = toNuprlTerm context' e'
	     val (nt, pr)   = toNuprlTerm context' e
	     fun toRec e1 e2 =
		 if NT.is_nuprl_lambda_term e1
		 then let val (x, B) = NT.dest_lambda 11 e1
			  (* gind: letrec f x = B in f *)
			  val gind = NT.mk_nuprl_genrec_term (NT.dest_nuprl_var x, fid, B)
		      in if NT.is_nuprl_variable_term e2
			    andalso
			    NT.dest_variable e2 = fid
			 then gind
			 else let val lam = NT.mk_lambda_term fid e2
			      (* lam: \f. e *)
			      (* term: (\f. e) (letrec f x = B in f) *)
			      in NT.mk_apply_term lam gind
			      end
		      end
		 else (print (A.toString e'); raise Fail "impossible:recursive definition is not a function")
	 in (toRec nte nt, toRec pre pr)
	 end
       | _ => raise Fail ("impossible:" ^ minStr))
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_WHERE),    label, value, regions, parents, children = [e, b]})        =
    raise Fail ("impossible:" ^ minStr)
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_BINDING),  label, value, regions, parents, children = [e1, p, e2]})   =
    (case A.getIdPat p of
	 SOME ident =>
	 let val (nt1,pr1) = toNuprlTerm context e1
	     val id        = A.getIdIdent ident
	     val context'  = add_bound_context context id
	     val (nt2,pr2) = toNuprlTerm context' e2
	     val cls       = NT.mk_nuprl_bind_class_term nt1 (id, nt2)
	     val lam       = NT.mk_lambda_term id pr2
	     val prog      =
		 if is_newprog_context context
		 then NT.mk_nuprl_bind_class_program_term pr1 lam
		 else let val ity = getItyAtLab context label "bind combinator without type"
			  val typ = toNuprlIty_em 9 false context (E.stripIty ity)
			  val (A, right) = NT.dest_simple_function typ
			  val (info, es, e, B) = NT.dest_eclass right
		      in NT.mk_nuprl_bind_prc_term A B pr1 lam
		      end
	 in (cls, prog)
	 end
       | NONE => (print (A.toString p ^ "\n"); raise Fail ("impossible:" ^ minStr)))
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_MBIND),    label, value, regions, parents, children = [e1, e2]})      = raise Fail ("impossible:" ^ minStr)
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_COMP),     label, value, regions, parents, children = (exp :: exps)}) =
    let val (nt, pr) = toNuprlTerm context exp
	val lst = map (toNuprlTerm context) exps
	val bp  = String.isSubstring "P" value (* if Prior(self)?b *)
	val bo  = String.isSubstring "O" value (* the b of a Prior *)
	val bf  = String.isSubstring "F" value (* F is for lifting, i.e., A->B, ---> Bag(A)->Bag(B)*)
	val bc  = String.isSubstring "C" value (* C is for concat, i.e., A->Bag(B), ---> Bag(A)->Bag(B)*)
	fun getOpt nts =
	    if bo andalso not (List.null nts)
	    then let val rev = List.rev nts
		     val (e_last, p_last) = List.hd rev
		     val firsts = List.rev (List.tl rev)
		 in if A.isExpBag (List.last exps)
		    then let val id    = A.newIdIdSet (get_all_ids_list [e_last, p_last]) "l"
			     val e_lam = NT.mk_lambda_term id e_last
			     val p_lam = NT.mk_lambda_term id p_last
			 in (firsts, SOME (e_lam, p_lam))
			 end
		    else (firsts, SOME (e_last, p_last))
		 end
	    else (nts, NONE)
	val (lst', opt) = getOpt lst
    in preToNuprlCombComp context (nt, pr) lst' bp opt label bf bc
    end
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_QUOT),     label, value, regions, parents, children = [e]})           = toNuprlTerm context e
  | toNuprlTerm context (term as A.N {kind = (A.EXP, A.EXP_CASE),     label, value, regions, parents, children = [e, m]})        =
    let val (e_term, p_term) = toNuprlTerm context e
	(*val _ = print ("[case]\n" ^ A.toString term ^ "\n")*)
    in case A.caseMatch m of
	   A.CASE_LIST (exp1, (id1, id2, exp2)) =>
	   let val (nt1, pr1) = toNuprlTerm context exp1
	       val context'   = add_bounds_context context [id1, id2]
	       val (nt2, pr2) = toNuprlTerm context' exp2
	       fun toT t      = (id1, id2, A.newId (), t)
	   in (NT.mk_nuprl_list_ind_term e_term nt1 (toT nt2),
	       NT.mk_nuprl_list_ind_term p_term pr1 (toT pr2))
	   end
	 | A.CASE_INJ ((id1, exp1), (id2, exp2)) =>
	   let val context1   = add_bounds_context context [id1]
	       val (nt1, pr1) = toNuprlTerm context1 exp1
	       val context2   = add_bounds_context context [id2]
	       val (nt2, pr2) = toNuprlTerm context2 exp2
	   in (NT.mk_decide_term e_term (id1, nt1) (id2, nt2),
	       NT.mk_decide_term p_term (id1, pr1) (id2, pr2))
	   end
	 | A.CASE_OTH => (print (A.toString m ^ "\n" ^ A.export m); raise EH.Unimplemented "")
    end

  (* ATEXP *)
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_ID),      label, value, regions, parents, children = [i]})      = toNuprlTerm context i
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_SCON),    label, value, regions, parents, children = [sc]})     = toNuprlTerm context sc
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_TUPLE),   label, value, regions, parents, children = []})       = double (NT.mk_nuprl_it_term)
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_TUPLE),   label, value, regions, parents, children = [e1, e2]}) =
    let val (nt1, pr1) = toNuprlTerm context e1
	val (nt2, pr2) = toNuprlTerm context e2
    in (NT.mk_pair_term nt1 nt2,
	NT.mk_pair_term pr1 pr2)
    end
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_TUPLE),   label, value, regions, parents, children})            = raise Fail ("impossible:" ^ minStr)
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_PAREN),   label, value, regions, parents, children = [x]})      = toNuprlTerm context x
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_LIST),    label, value, regions, parents, children})            = raise Fail ("impossible:" ^ minStr)
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_BAG),     label, value, regions, parents, children})            =
    (case children of
	 [x] =>
	 let val (nt, pr) = toNuprlTerm context x
	 in (NT.mk_nuprl_single_bag_term nt, NT.mk_nuprl_single_bag_term pr)
	 end
       | _   => foldr (fn (x, (nt_term, pr_term)) =>
			  let val (nt, pr) = toNuprlTerm context x
			  in (NT.mk_nuprl_cons_bag_term nt nt_term,
			      NT.mk_nuprl_cons_bag_term pr pr_term)
			  end)
		      (double NT.mk_nuprl_empty_bag_term)
		      children)
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_PRIOR),   label, value, regions, parents, children = [e]})      =
    let val (nt, pr) = toNuprlTerm context e
	val cls  = NT.mk_nuprl_primed_class_term nt
	val prog =
	    if is_newprog_context context
	    then raise Fail "toNuprl:op:prior:newprog"
	    else let val ity = getItyAtLab context label "We should have generated a type for prior"
		     val typ = toNuprlIty_em 13 false context (E.stripIty ity)
		 in NT.mk_nuprl_prior_prc_term typ pr
		 end
    in (cls, prog)
    end
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_ANY),     label, value, regions, parents, children = [e]})      = raise Fail "impossible:cannot convert an any term"
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_MSG),     label, value, regions, parents, children = [a, e]})   =
    (* TODO: We need type information here, if we know that the piece of code
     * is typable and if we got the generated environment then, we can just
     * lookup the type associated to the atoms of the message.
     * We can't actually do that because we can have a type message of the form
     * (``foo`` : 'a List) and the type of the message we're looking at is of
     * type Int List. *)
    let val nta       = toAtomList a
	val (nte,pre) = toNuprlTerm context e
    in (NT.mk_nuprl_make_msg_term nta nte,
	NT.mk_nuprl_make_msg_term nta pre)
    end
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_ONCE), label, value, regions, parents, children = [e]}) =
    let val (nte, pre) = toNuprlTerm context e
	val cls  = NT.mk_nuprl_once_class_term nte
	val prog =
	    if is_newprog_context context
	    then NT.mk_nuprl_once_class_program_term pre
	    else let val ity = getItyAtLab context label "We should have generated a type for once classes"
		     val typ = toNuprlIty_em 10 false context (E.stripIty ity)
		 in NT.mk_nuprl_once_prc_term typ pre
		 end
    in (cls, prog)
    end
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_SENDOC), label, value, regions, parents, children = [e]}) =
    let val (nte, pre) = toNuprlTerm context e
	val cls  =
	    if is_newprog_context context
	    then NT.mk_nuprl_return_loc_bag_class_term nte
	    else NT.mk_nuprl_send_once_loc_class_term nte
	val prog =
	    if is_newprog_context context
	    then NT.mk_nuprl_return_loc_bag_class_program_term pre
	    else let val ity = getItyAtLab context label "We should have generated a type for once classes"
		     val typ = toNuprlIty_em 11 false context (E.stripIty ity)
		 in NT.mk_nuprl_send_once_loc_prc_term typ pre
		 end
    in (cls, prog)
    end
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_ONLOC), label, value, regions, parents, children = [e]}) =
    let val (nte, pre) = toNuprlTerm context e
	val cls  = NT.mk_nuprl_on_loc_class_term nte
	val prog =
	    if is_newprog_context context
	    then NT.mk_nuprl_on_loc_class_program_term pre
	    else let val ity = getItyAtLab context label "We should have generated a type for onloc classes"
		     val typ = toNuprlIty_em 12 false context (E.stripIty ity)
		 in NT.mk_nuprl_on_loc_prc_term typ pre
		 end
    in (cls, prog)
    end
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_SKIP), label, value, regions, parents, children = [e]}) =
    let val (nte, pre) = toNuprlTerm context e
	val cls  = NT.mk_nuprl_skip_first_class_term nte
	val prog =
	    if is_newprog_context context
	    then raise Fail "toNuprl:op:skip:newprog"
	    else let val ity = getItyAtLab context label "We should have generated a type for skip classes"
		     val typ = toNuprlIty_em 12 false context (E.stripIty ity)
 		 in NT.mk_nuprl_skip_first_prc_term typ pre
		 end
    in (cls, prog)
    end
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_STATEC), label, value, regions, parents, children}) =
    (case map (toNuprlTerm context) children of
	 [(nte1, pre1), (nte2, pre2), (nte3, pre3)] =>
	 if is_newprog_context context
	 then let val cls  = NT.mk_nuprl_state_class1_term nte1 nte2 nte3
		  val prog = NT.mk_nuprl_state_class1_program_term pre1 pre2 pre3
	      (*val prog = NT.mk_lambda_term "i" (NT.mk_nuprl_simple_term "hdf-state1-single-val" [NT.mk_apply_term pre2 (NT.mk_variable_term "i"), NT.mk_apply_term pre3 (NT.mk_variable_term "i"), NT.mk_apply_term pre1 (NT.mk_variable_term "i")])*)
	      in (cls, prog)
	      end
	 else let val ity  = getItyAtLab context label "We should have generated a type for State classes"
		  val typ  = toNuprlIty_em 12 false context (E.stripIty ity)
		  val (tB,_) = NT.dest_simple_product typ
		  val cls  = NT.mk_nuprl_state1_term nte1 nte2 nte3
		  val prog = NT.mk_nuprl_state1_prc_term tB pre1 pre2 pre3
	      in (cls, prog)
	      end
       | [(nte1, pre1), (nte2, pre2), (nte3, pre3), (nte4, pre4), (nte5, pre5)] =>
	 if is_newprog_context context
	 then let val cls  = NT.mk_nuprl_state_class2_term nte1 nte2 nte3 nte4 nte5
		  val prog = NT.mk_nuprl_state_class2_program_term pre1 pre2 pre3 pre4 pre5
	      in (cls, prog)
	      end
	 else let val ity  = getItyAtLab context label "We should have generated a type for skip classes"
		  val typ  = toNuprlIty_em 12 false context (E.stripIty ity)
		  val (t,tA2)  = NT.dest_simple_product typ
		  val (tB,tA1) = NT.dest_simple_product t
		  val cls  = NT.mk_nuprl_state2_term nte1 nte2 nte3 nte4 nte5
		  val prog = NT.mk_nuprl_state2_prc_term tA1 tA2 tB pre1 pre2 pre3 pre4 pre5
	      in (cls, prog)
	      end
       | [(nte1, pre1), (nte2, pre2), (nte3, pre3), (nte4, pre4), (nte5, pre5), (nte6, pre6), (nte7, pre7)] =>
	 if is_newprog_context context
	 then let val cls  = NT.mk_nuprl_state_class3_term nte1 nte2 nte3 nte4 nte5 nte6 nte7
		  val prog = NT.mk_nuprl_state_class3_program_term pre1 pre2 pre3 pre4 pre5 pre6 pre7
	      in (cls, prog)
	      end
	 else let val ity  = getItyAtLab context label "We should have generated a type for Memory classes"
		  val typ  = toNuprlIty_em 12 false context (E.stripIty ity)
		  val (t1,tA3) = NT.dest_simple_product typ
		  val (t2,tA2) = NT.dest_simple_product t1
		  val (tB,tA1) = NT.dest_simple_product t2
		  val cls  = NT.mk_nuprl_state3_term nte1 nte2 nte3 nte4 nte5 nte6 nte7
		  val prog = NT.mk_nuprl_state3_prc_term tA1 tA2 tA3 tB pre1 pre2 pre3 pre4 pre5 pre6 pre7
	      in (cls, prog)
	      end
       | [(nte1, pre1), (nte2, pre2), (nte3, pre3), (nte4, pre4), (nte5, pre5), (nte6, pre6), (nte7, pre7), (nte8, pre8), (nte9, pre9)] =>
	 if is_newprog_context context
	 then let val cls  = NT.mk_nuprl_state_class4_term nte1 nte2 nte3 nte4 nte5 nte6 nte7 nte8 nte9
		  val prog = NT.mk_nuprl_state_class4_program_term pre1 pre2 pre3 pre4 pre5 pre6 pre7 pre8 pre9
	      in (cls, prog)
	      end
	 else let val ity  = getItyAtLab context label "We should have generated a type for Memory classes"
		  val typ  = toNuprlIty_em 12 false context (E.stripIty ity)
		  val (t1,tA4) = NT.dest_simple_product typ
		  val (t2,tA3) = NT.dest_simple_product t1
		  val (t3,tA2) = NT.dest_simple_product t2
		  val (tB,tA1) = NT.dest_simple_product t3
		  val cls  = NT.mk_nuprl_state4_term nte1 nte2 nte3 nte4 nte5 nte6 nte7 nte8 nte9
		  val prog = NT.mk_nuprl_state4_prc_term tA1 tA2 tA3 tA4 tB pre1 pre2 pre3 pre4 pre5 pre6 pre7 pre8 pre9
	      in (cls, prog)
	      end
       | _ => raise Fail "toNuprl:STATEC:newprog:wrong_number_of_arguments")
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_MEMORYC), label, value, regions, parents, children}) =
    (case map (toNuprlTerm context) children of
	 [(nte1, pre1), (nte2, pre2), (nte3, pre3)] =>
	 if is_newprog_context context
	 then let val cls  = NT.mk_nuprl_memory_class1_term nte1 nte2 nte3
		  val prog = NT.mk_nuprl_memory_class1_program_term pre1 pre2 pre3
	      in (cls, prog)
	      end
	 else let val ity  = getItyAtLab context label "We should have generated a type for skip classes"
		  val typ  = toNuprlIty_em 12 false context (E.stripIty ity)
		  val (tB,_) = NT.dest_simple_product typ
		  val cls  = NT.mk_nuprl_memory1_term nte1 nte2 nte3
		  val prog = NT.mk_nuprl_memory1_prc_term tB pre1 pre2 pre3
	      in (cls, prog)
	      end
       | [(nte1, pre1), (nte2, pre2), (nte3, pre3), (nte4, pre4), (nte5, pre5)] =>
	 if is_newprog_context context
	 then let val cls  = NT.mk_nuprl_memory_class2_term nte1 nte2 nte3 nte4 nte5
		  val prog = NT.mk_nuprl_memory_class2_program_term pre1 pre2 pre3 pre4 pre5
	      in (cls, prog)
	      end
	 else let val ity  = getItyAtLab context label "We should have generated a type for skip classes"
		  val typ  = toNuprlIty_em 12 false context (E.stripIty ity)
		  val (t,tA2)  = NT.dest_simple_product typ
		  val (tB,tA1) = NT.dest_simple_product t
		  val cls  = NT.mk_nuprl_memory2_term nte1 nte2 nte3 nte4 nte5
		  val prog = NT.mk_nuprl_memory2_prc_term tA1 tA2 tB pre1 pre2 pre3 pre4 pre5
	      in (cls, prog)
	      end
       | [(nte1, pre1), (nte2, pre2), (nte3, pre3), (nte4, pre4), (nte5, pre5), (nte6, pre6), (nte7, pre7)] =>
	 if is_newprog_context context
	 then let val cls  = NT.mk_nuprl_memory_class3_term nte1 nte2 nte3 nte4 nte5 nte6 nte7
		  val prog = NT.mk_nuprl_memory_class3_program_term pre1 pre2 pre3 pre4 pre5 pre6 pre7
	      in (cls, prog)
	      end
	 else let val ity  = getItyAtLab context label "We should have generated a type for skip classes"
		  val typ  = toNuprlIty_em 12 false context (E.stripIty ity)
		  val (t1,tA3) = NT.dest_simple_product typ
		  val (t2,tA2) = NT.dest_simple_product t1
		  val (tB,tA1) = NT.dest_simple_product t2
		  val cls  = NT.mk_nuprl_memory3_term nte1 nte2 nte3 nte4 nte5 nte6 nte7
		  val prog = NT.mk_nuprl_memory3_prc_term tA1 tA2 tA3 tB pre1 pre2 pre3 pre4 pre5 pre6 pre7
	      in (cls, prog)
	      end
       | [(nte1, pre1), (nte2, pre2), (nte3, pre3), (nte4, pre4), (nte5, pre5), (nte6, pre6), (nte7, pre7), (nte8, pre8), (nte9, pre9)] =>
	 if is_newprog_context context
	 then let val cls  = NT.mk_nuprl_memory_class4_term nte1 nte2 nte3 nte4 nte5 nte6 nte7 nte8 nte9
		  val prog = NT.mk_nuprl_memory_class4_program_term pre1 pre2 pre3 pre4 pre5 pre6 pre7 pre8 pre9
	      in (cls, prog)
	      end
	 else let val ity  = getItyAtLab context label "We should have generated a type for skip classes"
		  val typ  = toNuprlIty_em 12 false context (E.stripIty ity)
		  val (t1,tA4) = NT.dest_simple_product typ
		  val (t2,tA3) = NT.dest_simple_product t1
		  val (t3,tA2) = NT.dest_simple_product t2
		  val (tB,tA1) = NT.dest_simple_product t3
		  val cls  = NT.mk_nuprl_memory4_term nte1 nte2 nte3 nte4 nte5 nte6 nte7 nte8 nte9
		  val prog = NT.mk_nuprl_memory4_prc_term tA1 tA2 tA3 tA4 tB pre1 pre2 pre3 pre4 pre5 pre6 pre7 pre8 pre9
	      in (cls, prog)
	      end
       | _ => raise Fail "toNuprl:MEMORYC:newprog:wrong_number_of_arguments")
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_WAIT), label, value, regions, parents, children = [t, e]}) =
    let val (ntt, prt) = toNuprlTerm context t
	val (nte, pre) = toNuprlTerm context e
    in (nte, NT.mk_wait_term prt pre)
    end
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_NULL), label, value, regions, parents, children = []}) =
    (NT.mk_nuprl_null_class_program_term, NT.mk_nuprl_null_class_term)
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_MINUS), label, value, regions, parents, children = [e]}) =
    let val (nte, pre) = toNuprlTerm context e
    in (NT.mk_minus_term nte,
	NT.mk_minus_term pre)
    end
  | toNuprlTerm context (term as A.N {kind = (A.ATEXP, A.ATEXP_TYPE), label, value, regions, parents, children = [typ]}) =
    let val ity = getItyAtLab context label "We should have generated a type for type expressions"
    in double (toNuprlIty_em 12 false context (E.stripIty ity))
    end

  (* PROP *)
  | toNuprlTerm context (term as A.N {kind = (A.PROP, A.PROP_EXP), label, value, regions, parents, children = [exp]})          =
    let val (nte, pre) = toNuprlTerm context exp
	val ity = getItyAtLab context label "We should have generated a type for type expressions"
    in if E.isItyProp ity
       then (nte, pre)
       else if E.isItyBool ity
       then (fancy_mk_nuprl_assert_term nte, pre)
       else raise Fail "toNuprlTerm:PROP_EXP"
    end
  | toNuprlTerm context (term as A.N {kind = (A.PROP, A.PROP_OR),  label, value, regions, parents, children = [e1, e2]})       =
    let val (nt1, pr1) = toNuprlTerm context e1
	val (nt2, pr2) = toNuprlTerm context e2
    in (NT.mk_nuprl_or_term nt1 nt2,
	NT.mk_nuprl_or_term pr1 pr2)
    end
  | toNuprlTerm context (term as A.N {kind = (A.PROP, A.PROP_AND), label, value, regions, parents, children = [e1, e2]})       =
    let val (nt1, pr1) = toNuprlTerm context e1
	val (nt2, pr2) = toNuprlTerm context e2
    in (NT.mk_nuprl_and_term nt1 nt2,
	NT.mk_nuprl_and_term pr1 pr2)
    end
  | toNuprlTerm context (term as A.N {kind = (A.PROP, A.PROP_IMP), label, value, regions, parents, children = [e1, e2]})       =
    let val (nt1, pr1) = toNuprlTerm context e1
	val (nt2, pr2) = toNuprlTerm context e2
    in (NT.mk_nuprl_implies_term nt1 nt2,
	NT.mk_nuprl_implies_term pr1 pr2)
    end
  | toNuprlTerm context (term as A.N {kind = (A.PROP, A.PROP_IFF), label, value, regions, parents, children = [e1, e2]})       =
    let val (nt1, pr1) = toNuprlTerm context e1
	val (nt2, pr2) = toNuprlTerm context e2
    in (NT.mk_nuprl_iff_term nt1 nt2,
	NT.mk_nuprl_iff_term pr1 pr2)
    end
  | toNuprlTerm context (term as A.N {kind = (A.PROP, A.PROP_ALL), label, value, regions, parents, children = [seq, typ, prp]}) =
    let val idents   = A.getIdIdents seq
	val context' = foldr (fn (ident,ctxt) => add_bound_context ctxt ident) context idents
	val ity      = getItyAtLab context label "We should have generated a type for once classes"
	val typ      = toNuprlIty_em 19 false context (E.stripIty ity)
	val (nt, pr) = toNuprlTerm context' prp
    in (foldr (fn (ident,term) => NT.mk_nuprl_all_term typ (ident, term)) nt idents,
	foldr (fn (ident,term) => NT.mk_nuprl_all_term typ (ident, term)) pr idents)
    end
  | toNuprlTerm context (term as A.N {kind = (A.PROP, A.PROP_EX), label, value, regions, parents, children = [seq, typ, prp]}) =
    let val idents   = A.getIdIdents seq
	val context' = foldr (fn (ident,ctxt) => add_bound_context ctxt ident) context idents
	val ity      = getItyAtLab context label "We should have generated a type for once classes"
	val typ      = toNuprlIty_em 19 false context (E.stripIty ity)
	val (nt, pr) = toNuprlTerm context' prp
    in (foldr (fn (ident,term) => NT.mk_nuprl_exists_term typ (ident, term)) nt idents,
	foldr (fn (ident,term) => NT.mk_nuprl_exists_term typ (ident, term)) pr idents)
    end
  | toNuprlTerm context (term as A.N {kind = (A.PROP, A.PROP_OBS), label, value, regions, parents, children = [class, v, event]}) =
    let val (nt_c, pr_c) = toNuprlTerm context class
	val (nt_v, pr_v) = toNuprlTerm context v
	val (nt_e, pr_e) = toNuprlTerm context event
	val ity  = getItyAtLab context label "We should have generated a type for once classes"
	val typ  = toNuprlIty_em 19 false context (E.stripIty ity)
	val crel = NT.mk_nuprl_classrel_term
		       (mk_nuprl_hidden_event_ordering ())
		       typ
		       nt_c
		       nt_e
		       nt_v
    in double crel
    end
  | toNuprlTerm context (term as A.N {kind = (A.PROP, A.PROP_PAREN), label, value, regions, parents, children = [x]}) = toNuprlTerm context x

  (* PARAM *)
  | toNuprlTerm context (term as A.N {kind = (A.PARAM, _),      label, value, regions, parents, children}) = raise EH.Unimplemented ""
  (* TYPEVARSEQ *)
  | toNuprlTerm context (term as A.N {kind = (A.TYPEVARSEQ, _), label, value, regions, parents, children}) = raise EH.Unimplemented ""
  (* TYPESEQ *)
  | toNuprlTerm context (term as A.N {kind = (A.TYPESEQ, _),    label, value, regions, parents, children}) = raise EH.Unimplemented ""
  (* DOTS *)
  | toNuprlTerm context (term as A.N {kind = (A.DOTS, _),       label, value, regions, parents, children}) = raise EH.Unimplemented ""

  (* wrong format *)
  | toNuprlTerm context term = (print (A.toString term); raise Fail ("impossible:" ^ A.wrongFormat))

fun toNuprlDoc [] = NT.mk_nuprl_itext_nil_term
  | toNuprlDoc (doc :: docs) =
    let val text = NT.mk_nuprl_itext_term (protect doc)
    in NT.mk_nuprl_itext_cons_term
	   text
	   (NT.mk_nuprl_itext_cons_term
		NT.mk_nuprl_inewline_term
		(toNuprlDoc docs))
    end

fun addDoc [] term = term
  | addDoc [""] term = term
  | addDoc doc term =
    let val prop = NT.mk_nuprl_iproperty_term "doc" (toNuprlDoc doc)
	val cons = NT.mk_nuprl_icons_cons_term prop NT.mk_nuprl_icons_nil_term
    in NT.mk_nuprl_iinclude_properties_term cons term
    end

fun toNuprlData context data n len (A.N {kind = (A.DATA, A.DATA_CONS), label, value, regions, parents, children = [id, typ]}) =
    let val ity    = getItyAtLab context label "We should have generated a type for this message"
	val typ    = toNuprlIty_em 17 false context (E.stripIty ity)
	val name   = A.getIdIdent id
	val lab    = A.getLabel id
	val id_is  = "is_"  ^ name
	val id_get = "get_" ^ name
	val tdata  =
	    case is_data_context context data of
		SOME prms =>
		let val a_terms = map NT.mk_variable_term prms
		    val _ = addDecFPList prms
		in mk_nuprl_user_term name a_terms
		end
	      | NONE => raise Fail "data"
	(*val typ1   = NT.mk_fun_term typ tdata
	val typ2   = NT.mk_fun_term tdata NT.mk_nuprl_bool_term
	val typ3   = NT.mk_fun_term tdata typ*)
	val xt = NT.mk_variable_term "x"
	fun set x m =
	    if len < 1 orelse m < 0
	    then raise Fail "toNuprlData:neg"
	    else if len = 1
	    then x
	    else if m = 0 andalso not (len - 1 = n) (* i.e., not last cons *)
	    then NT.mk_inl_term x
	    else NT.mk_inr_term (if m = 0 then x else set x (m - 1))
	fun get x m =
	    if len < 1 orelse m < 0
	    then raise Fail "toNuprlData:neg"
	    else if len = 1
	    then x
	    else if m = 0 andalso not (len - 1 = n)
	    then NT.mk_nuprl_outl_term x
	    else if m = 0
	    then NT.mk_nuprl_outr_term x
	    else get (NT.mk_nuprl_outr_term x) (m - 1)
	fun is x m =
	    if len < 1 orelse m < 0
	    then raise Fail "toNuprlData:neg"
	    else if len = 1
	    then NT.mk_nuprl_btrue_term
	    else if m = 0 andalso not (len - 1 = n)
	    then NT.mk_nuprl_isl_term x
	    else if m = 0
	    then NT.mk_nuprl_isr_term x
	    else let val b1 = NT.mk_nuprl_isr_term x
		     val b2 = is (NT.mk_nuprl_outr_term x) (m - 1)
		 in NT.mk_nuprl_band_term b1 b2
		 end
	val term1 = NT.mk_lambda_term "x" (set xt n)
	val term2 = NT.mk_lambda_term "x" (is  xt n)
	val term3 = NT.mk_lambda_term "x" (get xt n)
	val dumval      = ""
	val dumpremises = []
	val (aid1,lc1,ab1,th1,rest1) =
	    genAbstractionAndWF'
		5
		context
		dumval
		name
		lab
		term1
		dummy_dot_term
		dumpremises
	val (aid2,lc2,ab2,th2,rest2) =
	    genAbstractionAndWF'
		6
		context
		dumval
		id_is
		lab
		term2
		dummy_dot_term
		dumpremises
	val app = fn x => NT.mk_nuprl_assert_term (NT.mk_apply_term (#1 (toNuprlTerm context (A.idToExp id_is))) x)
	val (aid3,lc3,ab3,th3,rest3) =
	    genAbstractionAndWF'
		7
		context
		dumval
		id_get
		lab
		term3
		dummy_dot_term
		[app]
	val _ = resetDecFP ()
	val decs  = [ab1, th1] @ rest1 @ [ab2, th2] @ rest2 @ [ab3, th3] @ rest3
	val progs = [(get_user_name name, term1), (get_user_name id_is, term2),(get_user_name id_get, term3)]
    in (decs, progs)
    end
  | toNuprlData context data n len term = (print (A.toString term); raise Fail ("impossible:" ^ A.wrongFormat))

fun toNuprlBinds context (A.N {kind = (A.BINDS, A.BINDS_LIST), label, value, regions, parents, children}) =
    foldl (fn (A.N {kind = (A.BIND, A.BIND_DEC), label, value, regions, parents, children = [f, a, e]},
	       (terms, progs, context)) =>
	      if A.termIsEmParam a
	      then let val id       = A.getIdIdent f
		       val lab      = A.getLabel f
		       val (ne, pe) = toNuprlTerm context e
		       val premises = []
		       val (aid,lc,ab,th,rest) =
			   genAbstractionAndWF'
			       8
			       context
			       value
			       id
			       lab
			       ne
			       e
			       premises
		       val _        = resetDecFP ()
		       val uid      = get_user_name id
		       val context' = add_maybe_state_context context uid ne
		   in (terms @ (ab :: th :: rest), progs @ [(uid, aid (*pe*))], context')
		   end
	      else raise Fail ("impossible:" ^ minStr)
	    | _ => raise Fail ("impossible:" ^ minStr))
	  ([], [], context)
	  children
  | toNuprlBinds context term = (print (A.toString term); raise Fail ("impossible:" ^ A.wrongFormat))

fun toNuprlIncparm context (A.N {kind = (A.INCPARM, A.INCPARM_EXP), label, value, regions, parents, children = [id, exp]}) =
    let val (ne, pe) = toNuprlTerm context exp
    in (1, A.getIdIdent id, ne, exp, pe)
    end
  | toNuprlIncparm context (A.N {kind = (A.INCPARM, A.INCPARM_TYP), label, value, regions, parents, children = [id, typ]}) =
    let val ity = getItyAtLab context label "We should have generated a type for this message"
	val nty = toNuprlIty_em 17 false context (E.stripIty ity)
    in (2, A.getIdIdent id, nty, typ, nty)
    end
  | toNuprlIncparm context (A.N {kind = (A.INCPARM, A.INCPARM_INT), label, value, regions, parents, children = [id, atoms]}) =
    let val atm = toAtomList atoms
    in (3, A.getIdIdent id, atm, atoms, atm)
    end
  | toNuprlIncparm context term = (print (A.toString term); raise Fail ("impossible:" ^ A.wrongFormat))

fun toNuprlIncparms context (A.N {kind = (A.INCPARMS, A.INCPARMS_P), label, value, regions, parents, children}) =
    map (toNuprlIncparm context) children
  | toNuprlIncparms context term = (print (A.toString term); raise Fail ("impossible:" ^ A.wrongFormat))

fun get_prog_decs _ (SOME prog) = SOME prog
  | get_prog_decs (SOME prog) _ = SOME prog
  | get_prog_decs _ _ = NONE

(* -- replace term -- *)

val form_prog_subst1 = NT.replace_terms

fun form_prog_subst2 [] p = p
  | form_prog_subst2 ((id, idp) :: progs) p =
    let val _ = print ("[program part: " ^ id ^ "]\n")
    in (*NT.mk_callbyvalueall_term idp (id, form_prog_subst2 progs p)*)
	NT.mk_nuprl_let_term id idp (form_prog_subst2 progs p)
    end

fun form_prog_subst context p =
    if is_cbva_context context
    then form_prog_subst2 (get_progs_context context) p
    else form_prog_subst1 (get_progs_context context) p

(* -- replace atom -- *)

fun replace_prog_atm (id, p) (NT.TERM (operator as (("cons", tag), params), [NT.B_TERM ([], tok), NT.B_TERM ([], lst)])) =
    let fun default () =
	    let val term1 = NT.mk_rterm (replace_prog_atm (id, p) (NT.rterm2term tok))
		val term2 = NT.mk_rterm (replace_prog_atm (id, p) (NT.rterm2term lst))
	    in NT.TERM (operator, [NT.B_TERM ([], term1), NT.B_TERM ([], term2)])
	    end
    in if NT.opid_of_term (NT.rterm2term lst) = "nil"
       then case NT.rterm2term tok of
		NT.TERM ((("token", tag), ((t,k) :: params)), []) =>
		if id = t
		then p
		else default ()
	      | _ => default ()
       else default ()
    end
  | replace_prog_atm (id, p) (NT.TERM (operator, bterms)) =
    let val bterms' =
	    map (fn (NT.B_TERM (vars, term)) =>
		    NT.B_TERM (vars, NT.mk_rterm (replace_prog_atm (id, p) (NT.rterm2term term))))
		bterms
    in NT.TERM (operator, bterms')
    end
  | replace_prog_atm (id, p) (term as NT.AXM_TERM) = term
  | replace_prog_atm (id, p) (term as NT.BOT_TERM) = term
  | replace_prog_atm (id, p) (term as NT.INT_TERM) = term
  | replace_prog_atm (id, p) (term as NT.VOI_TERM) = term
  | replace_prog_atm (id, p) (term as NT.DUM_TERM) = term
  | replace_prog_atm (id, p) (term as NT.ATM_TERM _) = term
  | replace_prog_atm (id, p) (term as NT.TOK_TERM _) = term
  | replace_prog_atm (id, p) (term as NT.NAT_TERM n) = term
  | replace_prog_atm (id, p) (term as NT.VAR_TERM var) = term
  | replace_prog_atm (id, p) (term as NT.INL_TERM rterm) =
    NT.INL_TERM (replace_rprog_atm (id, p) rterm)
  | replace_prog_atm (id, p) (term as NT.INR_TERM rterm) =
    NT.INR_TERM (replace_rprog_atm (id, p) rterm)
  | replace_prog_atm (id, p) (term as NT.FIX_TERM rterm) =
    NT.FIX_TERM (replace_rprog_atm (id, p) rterm)
  | replace_prog_atm (id, p) (term as NT.MIN_TERM rterm) =
    NT.MIN_TERM (replace_rprog_atm (id, p) rterm)
  | replace_prog_atm (id, p) (term as NT.LAM_TERM (var, rterm)) =
    NT.LAM_TERM (var, replace_rprog_atm (id, p) rterm)
  | replace_prog_atm (id, p) (term as NT.REC_TERM (var, rterm)) =
    NT.REC_TERM (var, replace_rprog_atm (id, p) rterm)
  | replace_prog_atm (id, p) (term as NT.WAI_TERM (rterm1, rterm2)) =
    NT.WAI_TERM (replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.APP_TERM (rterm1, rterm2)) =
    NT.APP_TERM (replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.PAI_TERM (rterm1, rterm2)) =
    NT.PAI_TERM (replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.ADD_TERM (rterm1, rterm2)) =
    NT.ADD_TERM (replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.SUB_TERM (rterm1, rterm2)) =
    NT.SUB_TERM (replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.MUL_TERM (rterm1, rterm2)) =
    NT.MUL_TERM (replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.DIV_TERM (rterm1, rterm2)) =
    NT.DIV_TERM (replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.REM_TERM (rterm1, rterm2)) =
    NT.REM_TERM (replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.EQT_TERM (rterm1, rterm2)) =
    NT.EQT_TERM (replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.UNI_TERM (rterm1, rterm2)) =
    NT.UNI_TERM (replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.EQU_TERM (a, rterm1, rterm2)) =
    NT.EQU_TERM (replace_rprog_atm (id, p) a,
		 replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.IAX_TERM (a, rterm1, rterm2)) =
    NT.IAX_TERM (replace_rprog_atm (id, p) a,
		 replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.IPA_TERM (a, rterm1, rterm2)) =
    NT.IPA_TERM (replace_rprog_atm (id, p) a,
		 replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.IIR_TERM (a, rterm1, rterm2)) =
    NT.IIR_TERM (replace_rprog_atm (id, p) a,
		 replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.IIL_TERM (a, rterm1, rterm2)) =
    NT.IIL_TERM (replace_rprog_atm (id, p) a,
		 replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.IIN_TERM (a, rterm1, rterm2)) =
    NT.IIN_TERM (replace_rprog_atm (id, p) a,
		 replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.ILA_TERM (a, rterm1, rterm2)) =
    NT.ILA_TERM (replace_rprog_atm (id, p) a,
		 replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.IAT_TERM (a, rterm1, rterm2)) =
    NT.IAT_TERM (replace_rprog_atm (id, p) a,
		 replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.CBV_TERM (a, x, f)) =
    NT.CBV_TERM (replace_rprog_atm (id, p) a,
		 x,
		 replace_rprog_atm (id, p) f)
  | replace_prog_atm (id, p) (term as NT.CBA_TERM (a, x, f)) =
    NT.CBA_TERM (replace_rprog_atm (id, p) a,
		 x,
		 replace_rprog_atm (id, p) f)
  | replace_prog_atm (id, p) (term as NT.FUN_TERM (a, x, f)) =
    NT.FUN_TERM (replace_rprog_atm (id, p) a,
		 x,
		 replace_rprog_atm (id, p) f)
  | replace_prog_atm (id, p) (term as NT.PRD_TERM (a, x, f)) =
    NT.PRD_TERM (replace_rprog_atm (id, p) a,
		 x,
		 replace_rprog_atm (id, p) f)
  | replace_prog_atm (id, p) (term as NT.TUN_TERM (a, x, f)) =
    NT.TUN_TERM (replace_rprog_atm (id, p) a,
		 x,
		 replace_rprog_atm (id, p) f)
  | replace_prog_atm (id, p) (term as NT.SET_TERM (a, x, f)) =
    NT.SET_TERM (replace_rprog_atm (id, p) a,
		 x,
		 replace_rprog_atm (id, p) f)
  | replace_prog_atm (id, p) (term as NT.LES_TERM (a, b, rterm1, rterm2)) =
    NT.LES_TERM (replace_rprog_atm (id, p) a,
		 replace_rprog_atm (id, p) b,
		 replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.IEQ_TERM (a, b, rterm1, rterm2)) =
    NT.IEQ_TERM (replace_rprog_atm (id, p) a,
		 replace_rprog_atm (id, p) b,
		 replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.SPR_TERM (pair, var1, var2, rterm)) =
    NT.SPR_TERM (replace_rprog_atm (id, p) pair,
		 var1,
		 var2,
		 replace_rprog_atm (id, p) rterm)
  | replace_prog_atm (id, p) (term as NT.AEQ_TERM (n, a, b, rterm1, rterm2)) =
    NT.AEQ_TERM (n,
		 replace_rprog_atm (id, p) a,
		 replace_rprog_atm (id, p) b,
		 replace_rprog_atm (id, p) rterm1,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    NT.DEC_TERM (replace_rprog_atm (id, p) dec,
		 var1,
		 replace_rprog_atm (id, p) rterm1,
		 var2,
		 replace_rprog_atm (id, p) rterm2)
  | replace_prog_atm (id, p) (term as NT.IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) =
    NT.IND_TERM (replace_rprog_atm (id, p) i,
		 x,
		 rd,
		 replace_rprog_atm (id, p) downcase,
		 replace_rprog_atm (id, p) basecase,
		 y,
		 ru,
		 replace_rprog_atm (id, p) upcase)
  | replace_prog_atm (id, p) (NT.CLO_TERM clos) = raise Fail "replace_prog_atm:C_TERM"

and replace_rprog_atm (id, p) rterm =
    NT.mk_rterm (replace_prog_atm (id, p) (NT.rterm2term rterm))

fun form_prog_subst_atm' [] p = p
  | form_prog_subst_atm' (idp :: progs) p =
    replace_prog_atm idp (form_prog_subst_atm' progs p)

fun form_prog_subst_atm _ NONE = NONE
  | form_prog_subst_atm sub (SOME (params, p)) =
    SOME (params, form_prog_subst_atm' sub p)

(* -- replace variable -- *)

fun replace_prog_var (id, p) (NT.TERM (operator as ((opid, tag), params), bterms)) =
    if opid = "variable"
       andalso
       not (List.null params)
       andalso
       id = #1 (hd params)
    then p
    else let val bterms' =
		 map (fn (NT.B_TERM (vars, term)) =>
			 NT.B_TERM (vars, NT.mk_rterm (replace_prog_var (id, p) (NT.rterm2term term))))
		     bterms
	 in NT.TERM (operator, bterms')
	 end
  | replace_prog_var (id, p) (term as NT.AXM_TERM) = term
  | replace_prog_var (id, p) (term as NT.BOT_TERM) = term
  | replace_prog_var (id, p) (term as NT.INT_TERM) = term
  | replace_prog_var (id, p) (term as NT.VOI_TERM) = term
  | replace_prog_var (id, p) (term as NT.DUM_TERM) = term
  | replace_prog_var (id, p) (term as NT.ATM_TERM _) = term
  | replace_prog_var (id, p) (term as NT.TOK_TERM _) = term
  | replace_prog_var (id, p) (term as NT.NAT_TERM _) = term
  | replace_prog_var (id, p) (term as NT.VAR_TERM var) =
    if NT.dest_nuprl_var var = id
    then p
    else term
  | replace_prog_var (id, p) (term as NT.INL_TERM rterm) =
    NT.INL_TERM (replace_rprog_var (id, p) rterm)
  | replace_prog_var (id, p) (term as NT.INR_TERM rterm) =
    NT.INR_TERM (replace_rprog_var (id, p) rterm)
  | replace_prog_var (id, p) (term as NT.FIX_TERM rterm) =
    NT.FIX_TERM (replace_rprog_var (id, p) rterm)
  | replace_prog_var (id, p) (term as NT.MIN_TERM rterm) =
    NT.MIN_TERM (replace_rprog_var (id, p) rterm)
  | replace_prog_var (id, p) (term as NT.LAM_TERM (var, rterm)) =
    NT.LAM_TERM (var, replace_rprog_var (id, p) rterm)
  | replace_prog_var (id, p) (term as NT.REC_TERM (var, rterm)) =
    NT.REC_TERM (var, replace_rprog_var (id, p) rterm)
  | replace_prog_var (id, p) (term as NT.WAI_TERM (rterm1, rterm2)) =
    NT.WAI_TERM (replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.APP_TERM (rterm1, rterm2)) =
    NT.APP_TERM (replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.PAI_TERM (rterm1, rterm2)) =
    NT.PAI_TERM (replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.ADD_TERM (rterm1, rterm2)) =
    NT.ADD_TERM (replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.SUB_TERM (rterm1, rterm2)) =
    NT.SUB_TERM (replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.MUL_TERM (rterm1, rterm2)) =
    NT.MUL_TERM (replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.DIV_TERM (rterm1, rterm2)) =
    NT.DIV_TERM (replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.REM_TERM (rterm1, rterm2)) =
    NT.REM_TERM (replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.EQT_TERM (rterm1, rterm2)) =
    NT.EQT_TERM (replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.UNI_TERM (rterm1, rterm2)) =
    NT.UNI_TERM (replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.EQU_TERM (a, rterm1, rterm2)) =
    NT.EQU_TERM (replace_rprog_var (id, p) a,
		 replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.IAX_TERM (a, rterm1, rterm2)) =
    NT.IAX_TERM (replace_rprog_var (id, p) a,
		 replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.IPA_TERM (a, rterm1, rterm2)) =
    NT.IPA_TERM (replace_rprog_var (id, p) a,
		 replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.IIR_TERM (a, rterm1, rterm2)) =
    NT.IIR_TERM (replace_rprog_var (id, p) a,
		 replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.IIL_TERM (a, rterm1, rterm2)) =
    NT.IIL_TERM (replace_rprog_var (id, p) a,
		 replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.IIN_TERM (a, rterm1, rterm2)) =
    NT.IIN_TERM (replace_rprog_var (id, p) a,
		 replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.ILA_TERM (a, rterm1, rterm2)) =
    NT.ILA_TERM (replace_rprog_var (id, p) a,
		 replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.IAT_TERM (a, rterm1, rterm2)) =
    NT.IAT_TERM (replace_rprog_var (id, p) a,
		 replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.CBV_TERM (a, x, f)) =
    NT.CBV_TERM (replace_rprog_var (id, p) a,
		 x,
		 replace_rprog_var (id, p) f)
  | replace_prog_var (id, p) (term as NT.CBA_TERM (a, x, f)) =
    NT.CBA_TERM (replace_rprog_var (id, p) a,
		 x,
		 replace_rprog_var (id, p) f)
  | replace_prog_var (id, p) (term as NT.FUN_TERM (a, x, f)) =
    NT.FUN_TERM (replace_rprog_var (id, p) a,
		 x,
		 replace_rprog_var (id, p) f)
  | replace_prog_var (id, p) (term as NT.PRD_TERM (a, x, f)) =
    NT.PRD_TERM (replace_rprog_var (id, p) a,
		 x,
		 replace_rprog_var (id, p) f)
  | replace_prog_var (id, p) (term as NT.TUN_TERM (a, x, f)) =
    NT.TUN_TERM (replace_rprog_var (id, p) a,
		 x,
		 replace_rprog_var (id, p) f)
  | replace_prog_var (id, p) (term as NT.SET_TERM (a, x, f)) =
    NT.SET_TERM (replace_rprog_var (id, p) a,
		 x,
		 replace_rprog_var (id, p) f)
  | replace_prog_var (id, p) (term as NT.LES_TERM (a, b, rterm1, rterm2)) =
    NT.LES_TERM (replace_rprog_var (id, p) a,
		 replace_rprog_var (id, p) b,
		 replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.IEQ_TERM (a, b, rterm1, rterm2)) =
    NT.IEQ_TERM (replace_rprog_var (id, p) a,
		 replace_rprog_var (id, p) b,
		 replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.SPR_TERM (pair, var1, var2, rterm)) =
    NT.SPR_TERM (replace_rprog_var (id, p) pair,
		 var1,
		 var2,
		 replace_rprog_var (id, p) rterm)
  | replace_prog_var (id, p) (term as NT.AEQ_TERM (n, a, b, rterm1, rterm2)) =
    NT.AEQ_TERM (n,
		 replace_rprog_var (id, p) a,
		 replace_rprog_var (id, p) b,
		 replace_rprog_var (id, p) rterm1,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    NT.DEC_TERM (replace_rprog_var (id, p) dec,
		 var1,
		 replace_rprog_var (id, p) rterm1,
		 var2,
		 replace_rprog_var (id, p) rterm2)
  | replace_prog_var (id, p) (term as NT.IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) =
    NT.IND_TERM (replace_rprog_var (id, p) i,
		 x,
		 rd,
		 replace_rprog_var (id, p) downcase,
		 replace_rprog_var (id, p) basecase,
		 y,
		 ru,
		 replace_rprog_var (id, p) upcase)
  | replace_prog_var (id, p) (NT.CLO_TERM clos) = raise Fail "replace_prog_var:C_TERM"

and replace_rprog_var (id, p) rterm =
    NT.mk_rterm (replace_prog_var (id, p) (NT.rterm2term rterm))

fun form_prog_subst_var' [] p = p
  | form_prog_subst_var' (idp :: progs) p =
    replace_prog_var idp (form_prog_subst_var' progs p)

fun form_prog_subst_var _ NONE = NONE
  | form_prog_subst_var sub (SOME (params, p)) =
    SOME (params, form_prog_subst_var' sub p)

fun isAccessed label id = EN.isAccessed label id (getUnifEnv ())

fun mk_component id kind ident mk kinds atm hdr ity context set =
    let val lab = A.getLabel ident
    in if isAccessed lab id
	  andalso
	  List.exists (fn x => kind = x) kinds
       then let val typ = toNuprlIty_em 20 false context (E.stripIty ity)
	    (* we recompute typ each time to get the free parameters *)
	    in mk context set ident atm typ hdr
	    end
       else ([], [])
    end

(* DECS *)
fun toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_LET), label, value, regions, parents, children = [b]}) =
    (case b of
	 A.N {kind = (A.BIND, A.BIND_DEC), label, value, regions, parents, children = [f, a, e]} =>
	 if A.termIsEmParam a
	 then let val id           = A.getIdIdent f
		  val lab          = A.getLabel f
		  val (ne, pe)     = toNuprlTerm context e
		  val schemeop     = NONE
		  val premises     = []
		  val programmable = false
		  val localclass   = SOME (form_prog_subst context pe)
		  val data         = false
		  val lvlop        = NONE
		  val (aid,lc,ab,th,rest,_) =
		      genAbstractionAndWF
			  9
			  context
			  value
			  id
			  lab
			  ne
			  e
			  schemeop
			  premises
			  programmable
			  localclass
			  data
			  lvlop
		  val ab'          = addDoc doc ab
		  val _            = resetDecFP ()
		  val uid          = get_user_name id
		  val context1     = add_maybe_state_context context uid ne
		  val th'          =
		      if is_export_context context id
		      then let val prp  = NT.mk_nuprl_iproperty_term "EventML_export" (NT.mk_nuprl_ibool_term true)
			       val cons = NT.mk_nuprl_icons_cons_term prp NT.mk_nuprl_icons_nil_term
			   in NT.mk_nuprl_iinclude_properties_term cons th
			   end
		      else th
		  val context2 = add_prog_op_context context1 uid lc pe
		  val context3 = add_dec_context context2 uid ab
		  (*val _ = print ("\n------\nadding:\n" ^ uid ^ "\n" ^ NT.toStringTerm ab ^ "\n------\n")*)
		  (*val _ = print ("*********************************\n")
		  val _ = print ("--" ^ id ^ "--" ^ uid ^ "\n")
		  val _ = print ("*********************************\n")
		  val _ = print (NT.toStringTerm ab' ^ "\n")
		  val _ = print (NT.toStringTerm th' ^ "\n")
		  val _ = print (NT.toStringTerm pe ^ "\n")*)
	      in (ab' :: th' :: rest, context3, NONE)
	      end
	 else raise Fail ("impossible:" ^ minStr)
       | _ => raise Fail ("impossible:" ^ minStr))
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_CLASSREC), label, value, regions, parents, children = [f, arg, X, Y]}) =
    let val id        = A.getIdIdent f
	val lab       = A.getLabel f
	val a         = getIdPat 6 arg
	val ta        = NT.mk_variable_term a
	val lamx      = A.mk_new_dum_term A.EXP_LAMBDA "" [] [arg, X]
	val lamy      = A.mk_new_dum_term A.EXP_LAMBDA "" [] [arg, Y]
	val (nx, px)  = toNuprlTerm context lamx
	val (ny, py)  = toNuprlTerm context lamy
	val ity       = getItyAtLab context label "We should have generated a type for this message"
	val typ       = toNuprlIty_em 20 false context (E.stripIty ity)
	val (A, cB)   = NT.dest_simple_function typ
	val (_,_,_,B) = NT.dest_eclass cB
	val (ny',py') = let val (x,T) = NT.dest_lambda 12 ny
			    val (y,U) = NT.dest_lambda 13 py
			    val cskip = NT.mk_nuprl_skip_first_class_term T
			    val pskip =
				if is_newprog_context context
				then raise Fail "toNuprl:op:classrec:newprog"
				else NT.mk_nuprl_skip_first_prc_term A U
			    val clam  = NT.mk_lambda_term (NT.dest_nuprl_var x) cskip
			    val plam  = NT.mk_lambda_term (NT.dest_nuprl_var y) pskip
			in (clam, plam)
			end
	val nterm     = NT.mk_nuprl_rec_bind_class_term nx ny'
	val recbind   =
	    if is_newprog_context context
	    then raise Fail "toNuprl:op:classrec:newprog"
	    else NT.mk_nuprl_rec_bind_prc_term A B px py' ta
	val pterm     = NT.mk_lambda_term a recbind
	val e         = A.mk_new_dum_term A.EXP_LAMBDA "" [] [arg, A.mk_new_dum_term A.EXP_OP "||" [] [X, Y]]
	val premises  = []
	val (aid,lc,ab,th,rest) =
	    genAbstractionAndWF'
		9
		context
		value
		id
		lab
		nterm
		e
		premises
	val ab'       = addDoc doc ab
	val _         = resetDecFP ()
	val uid       = get_user_name id
	val context'  = add_maybe_state_context context uid nterm
	val th'       =
	    if is_export_context context id
	    then let val prp  = NT.mk_nuprl_iproperty_term "EventML_export" (NT.mk_nuprl_ibool_term true)
		     val cons = NT.mk_nuprl_icons_cons_term prp NT.mk_nuprl_icons_nil_term
		 in NT.mk_nuprl_iinclude_properties_term cons th
		 end
	    else th
    in (ab' :: th' :: rest, add_prog_context context' uid pterm, NONE)
    end
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_CONS),    label, value, regions, parents, children = [i, a, t]}) =
    let val id = A.getIdIdent i
	val ar = getArityArgs a
	val context1 = add_arity_context context id id ar
    in ([], context1, NONE)
    end
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_OCONS),   label, value, regions, parents, children = [i, a, t, v, s]}) =
    let val id = A.getIdIdent i
	val ar = getArityArgs a
	val context1 = add_arity_context context id id ar
    in ([], context1, NONE)
    end
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_TYCON),   label, value, regions, parents, children = [s, tc, ty]}) =
    let val id = A.getIdIdent tc
    in ([], rm_eqdec_context context id, NONE)
    end
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_EQTYCON), label, value, regions, parents, children = [s, tc, i, ty]}) =
    let val tc = A.getIdIdent tc
	val eq = A.getIdIdent i
	val context1 = add_eqdec_context context  tc eq
	val context2 = add_arity_context context1 eq eq []
    in ([], context2, NONE)
    end
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_QMSG), label, value, regions, parents, children = [ident, hdr, typ]}) =
    let val atm  = toAtomList hdr
	val ity  = getItyAtLab context label "We should have generated a type for this message"
	val typ  = toNuprlIty_em 20 false context (E.stripIty ity)
	val set  = get_all_ids_list [atm, typ]
	val idb  = A.getIdIdent ident ^ "'base"
	val ids  = A.getIdIdent ident ^ "'send"
	val idc  = A.getIdIdent ident ^ "'broadcast"
	val idns = A.getIdIdent ident ^ "''send"
	val idnc = A.getIdIdent ident ^ "''broadcast"
	val kinds1 = ["internal", "input", "output"]
	val kinds2 = ["internal", "output"]
	val (terms1, progs1) = mk_component idb  value ident mk_base_class          kinds1 atm hdr ity context set
	val (terms2, progs2) = mk_component ids  value ident mk_send_function       kinds2 atm hdr ity context set
	val (terms3, progs3) = mk_component idc  value ident mk_broadcast_function  kinds2 atm hdr ity context set
	val (terms4, progs4) = mk_component idns value ident mk_nsend_function      kinds2 atm hdr ity context set
	val (terms5, progs5) = mk_component idnc value ident mk_nbroadcast_function kinds2 atm hdr ity context set
	val progs    = progs1 @ progs2 @ progs3 @ progs4 @ progs5
	val context' = add_prog_list_context context progs
    in (terms1 @ terms2 @ terms3 @ terms4 @ terms5, context', NONE)
    end
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_EQMSG), label, value, regions, parents, children = [ident, hdr, typ]})   =
    let val atm  = #1 (toNuprlTerm context hdr)
	val ity  = getItyAtLab context label "We should have generated a type for this message"
	val typ  = toNuprlIty_em 20 false context (E.stripIty ity)
	val set  = get_all_ids_list [atm, typ]
	val idb  = A.getIdIdent ident ^ "'base"
	val ids  = A.getIdIdent ident ^ "'send"
	val idc  = A.getIdIdent ident ^ "'broadcast"
	val idns = A.getIdIdent ident ^ "''send"
	val idnc = A.getIdIdent ident ^ "''broadcast"
	val kinds1 = ["internal", "input", "output"]
	val kinds2 = ["internal", "output"]
	val (terms1, progs1) = mk_component idb  value ident mk_base_class          kinds1 atm hdr ity context set
	val (terms2, progs2) = mk_component ids  value ident mk_send_function       kinds2 atm hdr ity context set
	val (terms3, progs3) = mk_component idc  value ident mk_broadcast_function  kinds2 atm hdr ity context set
	val (terms4, progs4) = mk_component idns value ident mk_nsend_function      kinds2 atm hdr ity context set
	val (terms5, progs5) = mk_component idnc value ident mk_nbroadcast_function kinds2 atm hdr ity context set
	val progs    = progs1 @ progs2 @ progs3 @ progs4 @ progs5
	val context' = add_prog_list_context context progs
    in (terms1 @ terms2 @ terms3 @ terms4 @ terms5, context', NONE)
    end
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_MAIN),    label, value, regions, parents, children = [exp]})            =
    let val (nt, pr) = toNuprlTerm context exp
	val pr'      = form_prog_subst context pr
	val name     = "main"
	val prod     = NT.mk_nuprl_interface_term (mk_nuprl_hidden_msg_fun ())
	val cls      = NT.mk_nuprl_class_term (mk_nuprl_hidden_msg_fun ()) prod
	val schemeop = SOME (cls, [])
	val premises = []
	val prgm     = true
	val loccls   = SOME pr'
	val data     = false
	val lvlop    = NONE
	val (aid,lc,abs,thm,rest,_) =
	    genAbstractionAndWF
		10
		context
		value
		name
		label
		nt
		exp
		schemeop
		premises
		prgm
		loccls
		data
		lvlop
	val prp      = NT.mk_nuprl_iproperty_term "EventML_main" (NT.mk_nuprl_ibool_term true)
	val cons     = NT.mk_nuprl_icons_cons_term prp NT.mk_nuprl_icons_nil_term
	val thm'     = NT.mk_nuprl_iinclude_properties_term cons thm
	(* we form the program, and get rid of the hidden variables from the list of parameters *)
	val params   = List.mapPartial (fn p => if is_hidden p then NONE else SOME p) (getDecFP ())
	val progop   = SOME (params, pr')
	val _        = resetDecFP ()
    in (abs :: thm' :: rest, context, progop)
    end
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_ASSUME),  label, value, regions, parents, children})                    =
    ([], add_hyps_context context (map A.getIdIdent children), NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_INV),     label, value, regions, parents, children = [id, cls, params, args, prop]}) =
    if is_prop_context context
    then let val name   = A.getIdIdent id
	     val cname  = A.getIdIdent cls
	     val class  = A.mk_new_dum_term A.ID_VID cname [] []
	     val ncls   = #1 (toNuprlTerm context class)
	     val prms   = A.getChildren params
	     val nprop  = #1 (toNuprlTerm context prop)
	     val (typ, ren) =
		 let val ity = getItyAtLab context label "toNuprlDec:no-type-for-invariant"
		     val (ren, _) =
			 E.foldrITVS
			     (fn (tv, (ren, f)) =>
				 let val NEXT (str, f') = f ()
				 in (BTV.insert (ren, tv, str), f')
				 end)
			     (BTV.empty, getNewTyVarName)
			     (E.getItyvarsIty ity)
		     val force = true
		     val add   = true
		     val nty   = toNuprlIty 16 force add context IDENTS.empty (E.stripIty ity) ren E.emptyETVS
		 in (nty, ren)
		 end
	     val (pairs, nty) =
		 foldl (fn (param, (pairs, nty)) =>
			   if NT.is_nuprl_function_term nty
			   then let val (A,B) = NT.dest_simple_function nty
				    val eparam = #1 (toNuprlTerm context (pat2exp param))
				in (pairs @ [(eparam, A)], B)
				end
			   else raise Fail "INV:type")
		       ([], typ)
		       prms
	     val nty'   = if NT.is_nuprl_eclass_term nty
			  then let val (info, es, e, T) = NT.dest_eclass nty
			       in T
			       end
			  else raise Fail "toNuprlDec:to-a-class"
	     val thm    = genInvariant context name ncls (pairs, prms) [args] NONE NONE (nprop, prop) nty' ren true
	     val prp    = NT.mk_nuprl_iproperty_term "EventML_invariant" (NT.mk_nuprl_ibool_term true)
	     val cons   = NT.mk_nuprl_icons_cons_term prp NT.mk_nuprl_icons_nil_term
	     val ithm   = NT.mk_nuprl_iinclude_properties_term cons thm
	     val _      = resetDecFP ()
	     val context' = add_invariant_context context cname (get_user_name name)
	 in ([ithm], context', NONE)
	 end
    else ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_ORDER),   label, value, regions, parents, children = [id, cls, params, args1, args2, prop]}) =
    if is_prop_context context
    then let val name   = A.getIdIdent id
	     val cname  = A.getIdIdent cls
	     val class  = A.mk_new_dum_term A.ID_VID cname [] []
	     val ncls   = #1 (toNuprlTerm context class)
	     val prms   = A.getChildren params
	     val nprop  = #1 (toNuprlTerm context prop)
	     val (typ, ren) =
		 let val ity = getItyAtLab context label "toNuprlDec:no-type-for-invariant"
		     val (ren, _) =
			 E.foldrITVS
			     (fn (tv, (ren, f)) =>
				 let val NEXT (str, f') = f ()
				 in (BTV.insert (ren, tv, str), f')
				 end)
			     (BTV.empty, getNewTyVarName)
			     (E.getItyvarsIty ity)
		     val force = true
		     val add   = true
		     val nty   = toNuprlIty 16 force add context IDENTS.empty (E.stripIty ity) ren E.emptyETVS
		 in (nty, ren)
		 end
	     val (pairs, nty) =
		 foldl (fn (param, (pairs, nty)) =>
			   if NT.is_nuprl_function_term nty
			   then let val (A,B) = NT.dest_simple_function nty
				    val eparam = #1 (toNuprlTerm context (pat2exp param))
				in (pairs @ [(eparam,A)], B)
				end
			   else raise Fail "ORDER:type")
		       ([], typ)
		       prms
	     val nty'   = if NT.is_nuprl_eclass_term nty
			  then let val (info, es, e, T) = NT.dest_eclass nty
			       in T
			       end
			  else raise Fail "toNuprlDec:to-a-class"
	     val thm    = genInvariant context name ncls (pairs, prms) [args1, args2] NONE NONE (nprop, prop) nty' ren true
	     val prp    = NT.mk_nuprl_iproperty_term "EventML_ordering" (NT.mk_nuprl_ibool_term true)
	     val cons   = NT.mk_nuprl_icons_cons_term prp NT.mk_nuprl_icons_nil_term
	     val ithm   = NT.mk_nuprl_iinclude_properties_term cons thm
	     val _      = resetDecFP ()
	     val context' = add_ordering_context context cname (get_user_name name)
	 in ([ithm], context', NONE)
	 end
    else ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_PROGRESS), label, value, regions, parents, children = [id, cls, params, args1, args2, wcls, prop]}) =
    if is_prop_context context
    then let val name   = A.getIdIdent id
	     val cname  = A.getIdIdent cls
	     val class  = A.mk_new_dum_term A.ID_VID cname [] []
	     val ncls   = #1 (toNuprlTerm context class)
	     val prms   = A.getChildren params
	     val nprop  = #1 (toNuprlTerm context prop)
	     val (typ, ren) =
		 let val ity = getItyAtLab context label "toNuprlDec:no-type-for-invariant"
		     val (ren, _) =
			 E.foldrITVS
			     (fn (tv, (ren, f)) =>
				 let val NEXT (str, f') = f ()
				 in (BTV.insert (ren, tv, str), f')
				 end)
			     (BTV.empty, getNewTyVarName)
			     (E.getItyvarsIty ity)
		     val force = true
		     val add   = true
		     val nty   = toNuprlIty 16 force add context IDENTS.empty (E.stripIty ity) ren E.emptyETVS
		 in (nty, ren)
		 end
	     val (pairs, nty) =
		 foldl (fn (param, (pairs, nty)) =>
			   if NT.is_nuprl_function_term nty
			   then let val (A,B) = NT.dest_simple_function nty
				    val eparam = #1 (toNuprlTerm context (pat2exp param))
				in (pairs @ [(eparam,A)], B)
				end
			   else raise Fail "PROGRESS:type")
		       ([], typ)
		       prms
	     val nty'   = if NT.is_nuprl_eclass_term nty
			  then let val (info, es, e, T) = NT.dest_eclass nty
			       in T
			       end
			  else raise Fail "toNuprlDec:to-a-class"
	     val wop    = SOME (#1 (toNuprlTerm context wcls), wcls, NONE)
	     val thm    = genInvariant context name ncls (pairs, prms) [args1, args2] wop NONE (nprop, prop) nty' ren true
	     val prp    = NT.mk_nuprl_iproperty_term "EventML_progress" (NT.mk_nuprl_ibool_term true)
	     val cons   = NT.mk_nuprl_icons_cons_term prp NT.mk_nuprl_icons_nil_term
	     val ithm   = NT.mk_nuprl_iinclude_properties_term cons thm
	     val _      = resetDecFP ()
	     val context' = add_progress_context context cname (get_user_name name)
	 in ([ithm], context', NONE)
	 end
    else ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_STRICT), label, value, regions, parents, children = [id, cls, params, args1, args2, v, wcls, obs, prop, rel]}) =
    if is_prop_context context
    then let val name   = A.getIdIdent id
	     val cname  = A.getIdIdent cls
	     val class  = A.mk_new_dum_term A.ID_VID cname [] []
	     val ncls   = #1 (toNuprlTerm context class)
	     val prms   = A.getChildren params
	     val nrel   = #1 (toNuprlTerm context rel)
	     val (typ, ren) =
		 let val ity = getItyAtLab context label "toNuprlDec:no-type-for-invariant"
		     val (ren, _) =
			 E.foldrITVS
			     (fn (tv, (ren, f)) =>
				 let val NEXT (str, f') = f ()
				 in (BTV.insert (ren, tv, str), f')
				 end)
			     (BTV.empty, getNewTyVarName)
			     (E.getItyvarsIty ity)
		     val force = true
		     val add   = true
		     val nty   = toNuprlIty 16 force add context IDENTS.empty (E.stripIty ity) ren E.emptyETVS
		 in (nty, ren)
		 end
	     val (pairs, nty) =
		 foldl (fn (param, (pairs, nty)) =>
			   if NT.is_nuprl_function_term nty
			   then let val (A,B) = NT.dest_simple_function nty
				    val eparam = #1 (toNuprlTerm context (pat2exp param))
				in (pairs @ [(eparam,A)], B)
				end
			   else raise Fail "PROGRESS:type")
		       ([], typ)
		       prms
	     val nty'   = if NT.is_nuprl_eclass_term nty
			  then let val (info, es, e, T) = NT.dest_eclass nty
			       in T
			       end
			  else raise Fail "toNuprlDec:to-a-class"
	     val nprop  = #1 (toNuprlTerm context prop)
	     val propop = SOME (v, obs, nprop, prop)
	     val wop    = SOME (#1 (toNuprlTerm context wcls), wcls, propop)
	     val thm    = genInvariant context name ncls (pairs, prms) [args1, args2] wop NONE (nrel, rel) nty' ren true
	     val invs   = get_invariants context cname
	     val cons1  = if List.null invs
			  then NT.mk_nuprl_icons_nil_term
			  else let val tinvs = map NT.mk_regular_token_term invs
				   val lst   = NT.mk_nuprl_finite_list_term tinvs
				   val prp   = NT.mk_nuprl_iproperty_term "EventML_hints"  lst
			       in NT.mk_nuprl_icons_cons_term prp NT.mk_nuprl_icons_nil_term
			       end
	     val prp    = NT.mk_nuprl_iproperty_term "EventML_progress2" (NT.mk_nuprl_ibool_term true)
	     val cons   = NT.mk_nuprl_icons_cons_term prp cons1
	     val ithm   = NT.mk_nuprl_iinclude_properties_term cons thm
	     val _      = resetDecFP ()
	     val context' = add_progress_context context cname (get_user_name name)
	 in ([ithm], context', NONE)
	 end
    else ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_CONSIST), label, value, regions, parents, children = [id, cls, params, args1, args2, prop]}) =
    if is_prop_context context
    then let val name   = A.getIdIdent id
	     val cname  = A.getIdIdent cls
	     val class  = A.mk_new_dum_term A.ID_VID cname [] []
	     val ncls   = #1 (toNuprlTerm context class)
	     val prms   = A.getChildren params
	     val nprop  = #1 (toNuprlTerm context prop)
	     val (typ, ren) =
		 let val ity = getItyAtLab context label "toNuprlDec:no-type-for-invariant"
		     val (ren, _) =
			 E.foldrITVS
			     (fn (tv, (ren, f)) =>
				 let val NEXT (str, f') = f ()
				 in (BTV.insert (ren, tv, str), f')
				 end)
			     (BTV.empty, getNewTyVarName)
			     (E.getItyvarsIty ity)
		     val force = true
		     val add   = true
		     val nty   = toNuprlIty 16 force add context IDENTS.empty (E.stripIty ity) ren E.emptyETVS
		 in (nty, ren)
		 end
	     val (pairs, nty) =
		 foldl (fn (param, (pairs, nty)) =>
			   if NT.is_nuprl_function_term nty
			   then let val (A,B) = NT.dest_simple_function nty
				    val eparam = #1 (toNuprlTerm context (pat2exp param))
				in (pairs @ [(eparam,A)], B)
				end
			   else raise Fail "ORDER:type")
		       ([], typ)
		       prms
	     val nty'   = if NT.is_nuprl_eclass_term nty
			  then let val (info, es, e, T) = NT.dest_eclass nty
			       in T
			       end
			  else raise Fail "toNuprlDec:to-a-class"
	     val thm    = genInvariant context name ncls (pairs, prms) [args1, args2] NONE NONE (nprop, prop) nty' ren true
	     val prp    = NT.mk_nuprl_iproperty_term "EventML_consistency" (NT.mk_nuprl_ibool_term true)
	     val cons   = NT.mk_nuprl_icons_cons_term prp NT.mk_nuprl_icons_nil_term
	     val ithm   = NT.mk_nuprl_iinclude_properties_term cons thm
	     val _      = resetDecFP ()
	 in ([ithm], context, NONE)
	 end
    else ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_MEMORY), label, value, regions, parents, children = [id, cls, params, args1, args2, v, wcls, rel]}) =
    if is_prop_context context
    then let val name   = A.getIdIdent id
	     val cname  = A.getIdIdent cls
	     val class  = A.mk_new_dum_term A.ID_VID cname [] []
	     val ncls   = #1 (toNuprlTerm context class)
	     val prms   = A.getChildren params
	     val nrel   = #1 (toNuprlTerm context rel)
	     val (typ, ren) =
		 let val ity = getItyAtLab context label "toNuprlDec:no-type-for-invariant"
		     val (ren, _) =
			 E.foldrITVS
			     (fn (tv, (ren, f)) =>
				 let val NEXT (str, f') = f ()
				 in (BTV.insert (ren, tv, str), f')
				 end)
			     (BTV.empty, getNewTyVarName)
			     (E.getItyvarsIty ity)
		     val force = true
		     val add   = true
		     val nty   = toNuprlIty 16 force add context IDENTS.empty (E.stripIty ity) ren E.emptyETVS
		 in (nty, ren)
		 end
	     val (pairs, nty) =
		 foldl (fn (param, (pairs, nty)) =>
			   if NT.is_nuprl_function_term nty
			   then let val (A,B) = NT.dest_simple_function nty
				    val eparam = #1 (toNuprlTerm context (pat2exp param))
				in (pairs @ [(eparam,A)], B)
				end
			   else raise Fail "PROGRESS:type")
		       ([], typ)
		       prms
	     val nty'   = if NT.is_nuprl_eclass_term nty
			  then let val (info, es, e, T) = NT.dest_eclass nty
			       in T
			       end
			  else raise Fail "toNuprlDec:to-a-class"
	     val Xop    = SOME (v, #1 (toNuprlTerm context wcls), wcls)
	     val thm    = genInvariant context name ncls (pairs, prms) [args1, args2] NONE Xop (nrel, rel) nty' ren true
	     val invs   = get_invariants context cname
	     val cons1  = if List.null invs
			  then NT.mk_nuprl_icons_nil_term
			  else let val tinvs = map NT.mk_regular_token_term invs
				   val lst   = NT.mk_nuprl_finite_list_term tinvs
				   val prp   = NT.mk_nuprl_iproperty_term "EventML_hints" lst
			       in NT.mk_nuprl_icons_cons_term prp NT.mk_nuprl_icons_nil_term
			       end
	     val prp    = NT.mk_nuprl_iproperty_term "EventML_memory" (NT.mk_nuprl_ibool_term true)
	     val cons2  = NT.mk_nuprl_icons_cons_term prp cons1
	     val ithm   = NT.mk_nuprl_iinclude_properties_term cons2 thm
	     val _      = resetDecFP ()
	     val context' = add_memory_context context cname (get_user_name name)
	 in ([ithm], context', NONE)
	 end
    else ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_INCLUDE), label, value, regions, parents, children = [ident, parms]}) =
    let val id = A.getIdIdent ident
    in case get_fprog_context id context of
	   SOME (pref, params, prog) =>
	   let val lst   = toNuprlIncparms context parms
	       val sub1  =
		   List.mapPartial
		       (fn (n, id, _, _, pe) => if n = 1 orelse n = 2 then SOME (id, pe) else NONE)
		       lst
	       val prog1 = form_prog_subst_var sub1 (SOME (params, prog))
	       val sub2  =
		   List.mapPartial
		       (fn (n, id, _, _, pe) => if n = 3 then SOME (id, pe) else NONE)
		       lst
	       val prog2 = form_prog_subst_atm sub2 prog1
	   in case prog2 of
		  SOME (params, p) =>
		  let val main = pref ^ "_main"
		      val _    =
			  List.app
			      (fn (n, _, _, exp, _) =>
				  if n = 1
				  then addDecFPList (getFreeParams context [] exp)
				  else ())
			      lst
		      val ps   = getDecFP ()
		      val _    = addUserFP main ps
		  in ([], add_prog_context context (get_user_name main) p, NONE)
		  end
		| NONE => ([], context, NONE)
	   end
	 | NONE => ([], context, NONE)
    end
  (* -- parameters are treated separately, by getParamsFromTerm
   * -- which is the first thing we do when transforming a file -- *)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_PSET),    label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_PMAP),    label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_PARAM),   label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_PARAMP),  label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_TYPARAM), label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_ETPARAM), label, value, regions, parents, children}) = ([], context, NONE)
  (* -- and similarly for datatypes and abstract types -- *)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_DATA),    label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_ABSTYPE), label, value, regions, parents, children}) = ([], context, NONE)
  (* -- *)
  (* -- we don't have to do anything for these either -- *)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_SPEC),    label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_IMPORT),  label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_TIMPORT), label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_EXPORT),  label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_GUARANT), label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_DOC),     label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_OPTIONS), label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_TYPEOF),  label, value, regions, parents, children}) = ([], context, NONE)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_TYFUN),   label, value, regions, parents, children}) = ([], context, NONE)
  (* -- *)
  (* -- impossible cases, these have been transformed -- *)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_LETR),  label, value, regions, parents, children = [b]}) = raise Fail ("impossible:" ^ minStr)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_CLASS), label, value, regions, parents, children = [b]}) = raise Fail ("impossible:" ^ minStr)
  | toNuprlDec context doc (term as A.N {kind = (A.DEC, A.DEC_PARSE), label, value, regions, parents, children = []})  = raise Fail ("impossible:term is not parsable")
  (* -- *)
  | toNuprlDec context doc term =
    (print (A.toString term);
     raise Fail ("toNuprlDec:impossible:" ^ A.wrongFormat))


(* Extracts the parameters from an AST *)
fun getParamsFromTerm term context =
    case term of
	A.N {kind = (A.FILE, A.FILE_F), label, value, regions, parents, children} => getParamsFromTerms children context
      | A.N {kind = (A.DEC, A.DEC_PSET), label, value, regions, parents, children = [ident, typ]} =>
	let val id       = A.getIdIdent ident
	    val lab      = A.getLabel ident
	    val ity      = getItyAtLab context label "We should have generated a type for this message (PSET)"
	    val sch      = E.mk_new_scheme [] ity
	    (* we add to the context the fact that id has name id and
	     * does not take any argument *)
	    val context1 = add_arity_context context  id id []
	    (* We add to the context the fact that id is a parameter
	     * with type scheme 'sch' *)
	    val context2 = add_param_context context1 id sch (PK_SET NONE)
	in context2
	end
      | A.N {kind = (A.DEC, A.DEC_PMAP), label, value, regions, parents, children = [ident, typ]} =>
	let val id       = A.getIdIdent ident
	    val lab      = A.getLabel ident
	    val ity      = getItyAtLab context label "We should have generated a type for this message (PMAP)"
	    val sch      = E.mk_new_scheme [] ity
	    (* we add to the context the fact that id has name id and
	     * does not take any argument *)
	    val context1 = add_arity_context context  id id []
	    (* We add to the context the fact that id is a parameter
	     * with type scheme 'sch' *)
	    val context2 = add_param_context context1 id sch (PK_MAP NONE)
	in context2
	end
      | A.N {kind = (A.DEC, A.DEC_PARAM), label, value, regions, parents, children = [ident, typ]} =>
	let val id       = A.getIdIdent ident
	    val lab      = A.getLabel ident
	    val sch      = getScheme context id lab true ("Couldn't find " ^ id ^ "'s environment")
	    (* we add to the context the fact that id has name id and
	     * does not take any argument *)
	    val context1 = add_arity_context context  id id []
	    (* We add to the context the fact that id is a parameter
	     * with type scheme 'sch' *)
	    val context2 = add_param_context context1 id sch PK_PRM
	in context2
	end
      | A.N {kind = (A.DEC, A.DEC_PARAMP), label, value, regions, parents, children = [ident, typ, prop]} =>
	let val id       = A.getIdIdent ident
	    val lab      = A.getLabel ident
	    val sch      = getScheme context id lab true ("Couldn't find " ^ id ^ "'s environment")
	    val context1 = add_arity_context context id id []
	    (* we add id to the list of parameters twice, because in such forms,
	     * we're defining set types where the parameter can be used in the
	     * proposition part of the set type.
	     * The 1st time, we add it to mark it as a parameter.
	     * The 2nd time, we record that it's a set type with prop 'pe'. *)
	    val context2 = add_param_context context1 id sch PK_PRM
	    val (ne,pe)  = toNuprlTerm context2 prop
	    val params   = getFreeParams context [] prop
	    val context2 = add_param_context' context1 id sch PK_PRM (pe,params)
	in context2
	end
      | A.N {kind = (A.DEC, A.DEC_TYPARAM), label, value, regions, parents, children = [tvseq, tycon, ty]} =>
	let val t1  = (*E.mk_type_typ1*) E.mk_type_type D.dummy_label
	    val ar  = getArityTyvarSeq tvseq
	    val typ = foldr (fn (_,term) => E.mk_type_arrow (t1,term) D.dummy_label)
			    t1
			    (List.tabulate (ar, fn _ => ()))
	    val id  = A.getIdIdent tycon
	    val sch = E.mk_new_scheme [] typ
	    val context1 = add_param_context context id sch PK_PRM
	in context1
	end
      | A.N {kind = (A.DEC, A.DEC_ETPARAM), label, value, regions, parents, children = [tvseq, tycon, ident, ty]} =>
	let val tc   = A.getIdIdent tycon
	    val ar   = getArityTyvarSeq tvseq
	    val eq   = A.getIdIdent ident
	    val lab  = A.getLabel ident
	    val sch1 = getScheme context eq lab true ("Couldn't find " ^ eq ^ "'s environment")
	    val t1   = (*E.mk_type_typ1*) E.mk_type_type D.dummy_label
	    val typ  = foldr (fn (_,term) => E.mk_type_arrow (t1,term) D.dummy_label)
			     t1
			     (List.tabulate (ar, fn _ => ()))
	    val sch2 = E.mk_new_scheme [] typ
	    val context1 = add_param_context context  eq sch1 PK_PRM
	    val context2 = add_param_context context1 tc sch2 PK_PRM
	    val context3 = add_eqdec_context context2 tc eq
	    val context4 = add_arity_context context3 eq eq []
	in context4
	end
      | A.N {kind = (A.DEC, _), label, value, regions, parents, children} => context
      | _ => raise Fail "getParamsFromTerm:unexpected_term"

and getParamsFromTerms [] context = context
  | getParamsFromTerms (term :: terms) context =
    getParamsFromTerms terms (getParamsFromTerm term context)


(* Extracts the abstract and data tyesp from an AST *)
fun getTypesFromTerm term context =
    case term of
	A.N {kind = (A.FILE, A.FILE_F),  label, value, regions, parents, children} => getTypesFromTerms children context
      | A.N {kind = (A.DEC, A.DEC_DATA), label, value, regions, parents, children = (id :: cons)} =>
	let val name  = A.getIdIdent id
	    val lab   = A.getLabel id
	    val data' =
		foldr (fn (con, disju) =>
			  let val lab   = A.getLabel con
			      val ity   = getItyAtLab context lab "We should have generated a type for this message"
			      val force = false
			      val add   = true
			      val typ   = toNuprlIty 18 force add context (IDENTS.singleton name) (E.stripIty ity) BTV.empty E.emptyETVS
			  in case disju of
				 NONE => SOME typ
			       | SOME t => SOME (NT.mk_nuprl_union_term typ t)
			  end)
		      NONE
		      cons
	    (* - the types of the get_ functions of the cons have to be of the form is_ => get_.
	     * - we need to fix the types of data's wf lemma. *)
	    val data =
		case data' of
		    NONE => raise Fail "datatype"
		  | SOME t => NT.mk_rec_term name t
	    val schemeop  = SOME (NT.mk_nuprl_type_term "i", [])
	    val premises  = []
	    val prgm      = false
	    val loccls    = NONE
	    val bdata     = true
	    val lvlop     = NONE
	    val (aid,lc,ab,th,rest,context1) =
		genAbstractionAndWF
		    11
		    context
		    ""
		    name
		    lab
		    data
		    dummy_dot_term
		    schemeop
		    premises
		    prgm
		    loccls
		    bdata
		    lvlop
	    val _         = resetDecFP ()
	    val len       = List.length cons
	    val (decs, progs, _) =
		foldl (fn (t, (decs, progs, n)) =>
			  let val (decs', progs') = toNuprlData context1 name n len t
			  in (decs @ decs', progs @ progs', n + 1)
			  end)
		      ([], [], 0)
		      cons
	    val context2 = add_prog_list_context context1 progs
	    val terms = ab :: th :: rest @ decs
	in (terms, context2)
	end
      | A.N {kind = (A.DEC, A.DEC_ABSTYPE), label, value, regions, parents, children = [tycon, typ, binds]} =>
	let val name     = A.getIdIdent tycon
	    val ity      = getItyAtLab context label "getTypesFromTerm:ABSTYPE:getItyAtLab"
	    val force    = false
	    val addctxt  = true
	    val nty      = toNuprlIty 19 force addctxt context IDENTS.empty (E.stripIty ity) BTV.empty E.emptyETVS
	    val id_lam   = NT.mk_lambda_term "x" (NT.mk_variable_term "x")
	    val id_abs   = "abs_" ^ name
	    val id_rep   = "rep_" ^ name
	    val uni      = SOME (NT.mk_nuprl_type_term "i", [])
	    val premises = []
	    val prog     = false
	    val loccls   = NONE
	    val data     = false
	    val lvlop    = NONE
	    val (aid1,lc1,ab1,th1,rest1,context1) =
		genAbstractionAndWF
		    12
		    context
		    ""
		    name
		    label
		    nty
		    typ
		    uni
		    premises
		    prog
		    loccls
		    true
		    lvlop
	    val parms    = getDecFP ()
	    val n_params = map NT.mk_variable_term parms
	    val abstyp   = mk_nuprl_user_term name n_params
	    val sch_abs  = SOME (NT.mk_fun_term nty abstyp, [])
	    val sch_rep  = SOME (NT.mk_fun_term abstyp nty, [])
	    val (aid2,lc2,ab2,th2,rest2,context2) =
		genAbstractionAndWF
		    13
		    context1
		    ""
		    id_abs
		    label
		    id_lam
		    dummy_dot_term
		    sch_abs
		    premises
		    prog
		    loccls
		    data
		    lvlop
	    val (aid3,lc3,ab3,th3,rest3,context3) =
		genAbstractionAndWF
		    14
		    context1
		    ""
		    id_rep
		    label
		    id_lam
		    dummy_dot_term
		    sch_rep
		    premises
		    prog
		    loccls
		    data
		    lvlop
	    val _        = resetDecFP ()
	    val prog2    = (get_user_name id_abs, id_lam)
	    val prog3    = (get_user_name id_rep, id_lam)
	    val context4 = add_fabs_context context3 id_abs
	    val context5 = add_fabs_context context4 id_rep
	    val (terms, progs, context6) = toNuprlBinds context5 binds
	    val terms' = (ab1 :: th1 :: rest1) @ (ab2 :: th2 :: rest2) @ (ab3 :: th3 :: rest3) @ terms
	    val context7 = add_prog_list_context context6 (prog2 :: prog3 :: progs)
	in (terms', context7)
	end
      | A.N {kind = (A.DEC, _), label, value, regions, parents, children} => ([],context)
      | _ => raise Fail "getTypesFromTerm:unexpected_term"

and getTypesFromTerms [] context = ([], context)
  | getTypesFromTerms (term :: terms) context =
    let val (terms1, context1) = getTypesFromTerm  term  context
	val (terms2, context2) = getTypesFromTerms terms context1
    in (terms1 @ terms2, context2)
    end


(* Extracts the headers from an AST *)
fun getHeadersFromTerm term context =
    case term of
	A.N {kind = (A.FILE, A.FILE_F),  label, value, regions, parents, children} => getHeadersFromTerms children context
      | A.N {kind = (A.DEC, A.DEC_QMSG), label, value, regions, parents, children = [ident, hdr, typ]} =>
	let val atm = toAtomList hdr
	    val ity = getItyAtLab context label "We should have generated a type for this message"
	in [(value, atm, ity, hdr)]
	end
      | A.N {kind = (A.DEC, A.DEC_EQMSG), label, value, regions, parents, children = [ident, hdr, typ]} =>
	let val atm = #1 (toNuprlTerm context hdr)
	    val ity = getItyAtLab context label "We should have generated a type for this message"
	in [(value, atm, ity, hdr)]
	end
      | A.N {kind = (A.DEC, _), label, value, regions, parents, children} => []
      | _ => raise Fail "getHeadersFromTerm:unexpected_term"

and getHeadersFromTerms [] context = []
  | getHeadersFromTerms (term :: terms) context =
    (getHeadersFromTerm term context) @ (getHeadersFromTerms terms context)


fun (*generateHeaders context [] label = NONE
  |*) generateHeaders context hdrs label =
    let (* abstractions names *)
	val name_all      = "headers"
	val name_norep    = "headers_no_rep"
	val name_fun      = "headers_fun"
	val name_typ      = "headers_type"
	val name_internal = "headers_internal"
	val name_noinputs = "headers_no_inputs"
	val name_types    = "headers_no_inputs_types"
	(* we set header_function_type to be name_typ *)
	val _ = set_header_function_type name_typ
	(* the types of the lists *)
	val type_name  = NT.mk_nuprl_name_term
	val typi       = NT.mk_nuprl_type_term "i"
	val propi      = NT.mk_nuprl_prop_term "i"
	val propip     = NT.mk_nuprl_prop_term "i'"
	val type_prod  = NT.mk_prod_term type_name typi
	val type_lname = NT.mk_nuprl_list_term type_name
	val type_lprod = NT.mk_nuprl_list_term type_prod
	val type_fun   = NT.mk_fun_term type_name typi
	(* --- *)
	val lvl       = NONE
	val lvli      = SOME "i"
	val data      = false
	val prog      = false
	val loccls    = NONE
	val premises  = []
	val scheme_lname  = SOME (type_lname, [])
	val scheme_lprod  = SOME (type_lprod, [])
	val scheme_fun    = SOME (type_fun,   [])
	val scheme_propi  = SOME (propi,      [])
	val scheme_propip = SOME (propip,     [])

	(* all headers *)
	val (n_lst_all, e_hdrs_all) =
	    foldr (fn ((kind, n_hdr, typ, e_hdr), (lst, e_hdrs)) =>
		      (NT.mk_nuprl_cons_term n_hdr lst, e_hdr :: e_hdrs))
		  (NT.mk_nuprl_nil_term, [])
		  hdrs
	(*val _ = print ("[headers: " ^ NT.toStringTerm n_lst_all ^ "]\n")*)
	val e_hdrs_all = A.mk_new_dum_term A.DOTS_D "" [] e_hdrs_all
	val (aid_all,lc_all,ab_all,th_all,rest_all,_) =
	    genAbstractionAndWF 15
				context
				""
				name_all
				label
				n_lst_all
				e_hdrs_all
				scheme_lname
				premises
				prog
				loccls
				data
				lvl
	val objs_all = ab_all :: th_all :: rest_all

	(* no repeats of the headers *)
	val norep = NT.mk_nuprl_no_repeats_term type_name aid_all
	val (aid_norep,lc_norep,ab_norep,th_norep,rest_norep,_) =
	    genAbstractionAndWF 15
				context
				""
				name_norep
				label
				norep
				e_hdrs_all
				scheme_propi
				premises
				prog
				loccls
				data
				lvl
	val objs_norep = ab_norep :: th_norep :: rest_norep
	val _          = resetDecFP ()

	(* function from headers to types *)
	val hdrsp =
	    map (fn (kind, n_hdr, typ, e_hdr) =>
		    (n_hdr, toNuprlIty_em 7 false context (E.stripIty typ)))
		hdrs
	val set  = get_all_ids_list (n_lst_all :: map (fn (x,y) => y) hdrsp)
	val hdr  = A.newIdIdSet set "hdr"
	val nhdr = NT.mk_variable_term hdr
	val n_lst_fun =
	    foldr (fn ((n_hdr, nty), f) =>
		      let val nameeq = NT.mk_nuprl_name_eq_term nhdr n_hdr
		      in NT.mk_nuprl_ite_term nameeq nty f
		      end)
		  NT.mk_nuprl_top_term
		  hdrsp
	val n_lst_lam = NT.mk_lambda_term hdr n_lst_fun
	val (aid_fun,lc_fun,ab_fun,th_fun,rest_fun,_) =
	    genAbstractionAndWF 17
				context
				""
				name_fun
				label
				n_lst_lam
				e_hdrs_all
				scheme_fun
				premises
				prog
				loccls
				data
				lvl
	val objs_fun = ab_fun :: th_fun :: rest_fun

	(* type extension of msg interface *)
	val vset  = A.newIdIdSet set "f"
	val nvset = NT.mk_variable_term vset
	val tfun  = NT.mk_fun_term type_name (NT.mk_nuprl_valuealltype_term "i")
	val app1  = NT.mk_apply_term nvset nhdr
	val app2  = NT.mk_apply_term aid_fun nhdr
	val eqhdr = NT.mk_equal_term typi app1 app2
	val lall  = NT.mk_nuprl_lall_term aid_all (hdr, eqhdr)
	val pand  = NT.mk_nuprl_and_term aid_norep lall
	val sprop = NT.mk_set_term tfun (vset, pand)
	val (aid_eqtyp,lc_eqtyp,ab_eqtyp,th_eqtyp,rest_eqtyp,_) =
	    genAbstractionAndWF 17
				context
				""
				name_typ
				label
				sprop
				e_hdrs_all
				scheme_propip
				premises
				prog
				loccls
				data
				lvli
	val objs_eqtyp = ab_eqtyp :: th_eqtyp :: rest_eqtyp
	val _          = resetDecFP ()

	(* internal headers *)
	val (n_lst_internal, e_hdrs_internal) =
	    foldr (fn ((kind, n_hdr, typ, e_hdr), (lst, e_hdrs)) =>
		      if kind = "internal"
		      then (NT.mk_nuprl_cons_term n_hdr lst, e_hdr :: e_hdrs)
		      else (lst, e_hdrs))
		  (NT.mk_nuprl_nil_term, [])
		  hdrs
	val e_hdrs_internal = A.mk_new_dum_term A.DOTS_D "" [] e_hdrs_internal
	val (aid_internal,lc_internal,ab_internal,th_internal,rest_internal,_) =
	    genAbstractionAndWF 16
				context
				""
				name_internal
				label
				n_lst_internal
				e_hdrs_internal
				scheme_lname
				premises
				prog
				loccls
				data
				lvl
	val objs_internal = ab_internal :: th_internal :: rest_internal
	val _             = resetDecFP ()

	(* no input headers *)
	val (n_lst_noinputs, e_hdrs_no_inputs) =
	    foldr (fn ((kind, n_hdr, typ, e_hdr), (lst, e_hdrs)) =>
		      if kind = "input"
		      then (lst, e_hdrs)
		      else (NT.mk_nuprl_cons_term n_hdr lst, e_hdr :: e_hdrs))
		  (NT.mk_nuprl_nil_term, [])
		  hdrs
	val e_hdrs_no_inputs = A.mk_new_dum_term A.DOTS_D "" [] e_hdrs_no_inputs
	val (aid_noinputs,lc_inputs,ab_noinputs,th_noinputs,rest_noinputs,_) =
	    genAbstractionAndWF 16
				context
				""
				name_noinputs
				label
				n_lst_noinputs
				e_hdrs_no_inputs
				scheme_lname
				premises
				prog
				loccls
				data
				lvl
	val objs_noinputs = ab_noinputs :: th_noinputs :: rest_noinputs
	val _             = resetDecFP ()

	(* no input headers with types *)
	val n_lst_types =
	    foldr (fn ((kind, n_hdr, typ, e_hdr), lst) =>
		      if kind = "input"
		      then lst
		      else let val nty  = toNuprlIty_em 7 false context (E.stripIty typ)
			       val pair = NT.mk_pair_term n_hdr nty
			   in NT.mk_nuprl_cons_term pair lst
			   end)
		  (NT.mk_nuprl_nil_term)
		  hdrs
	val (aid_types,lc_types,ab_types,th_types,rest_types,_) =
	    genAbstractionAndWF 17
				context
				""
				name_types
				label
				n_lst_types
				e_hdrs_no_inputs
				scheme_lprod
				premises
				prog
				loccls
				data
				lvl
	val prop      = NT.mk_nuprl_iproperty_term "EventML_interface" (NT.mk_nuprl_ibool_term true)
	val cons      = NT.mk_nuprl_icons_cons_term prop NT.mk_nuprl_icons_nil_term
	val ab_types' = NT.mk_nuprl_iinclude_properties_term cons ab_types
	val objs_type = ab_types' :: th_types :: rest_types
	val _         = resetDecFP ()

	(* no input headers id *)
	val n_id_noinputs = mk_nuprl_user_term name_noinputs []
    in SOME (e_hdrs_no_inputs,
	     e_hdrs_internal,
	     objs_all @ objs_norep @ objs_fun @ objs_eqtyp,
	     objs_internal @ objs_noinputs @ objs_type)
    end

(*
fun generateHeaders context =
    let val hdrs  = getHeaders ()
    in if not (null hdrs)
       then let (* abstractions names *)
	       val name_all      = "headers"
	       val name_noinputs = "headers_no_inputs"
	       val name_types    = "headers_no_inputs_types"
	       (* all headers *)
	       val n_lst_all =
		   foldr (fn ((kind, hdr, typ), lst) => NT.mk_nuprl_cons_term hdr lst)
			 (NT.mk_nuprl_nil_term)
			 hdrs
	       (* no input headers *)
	       val n_lst_noinputs =
		   foldr (fn ((kind, hdr, typ), lst) =>
			     if kind = "input"
			     then lst
			     else NT.mk_nuprl_cons_term hdr lst)
			 (NT.mk_nuprl_nil_term)
			 hdrs
	       (* no input headers with types *)
	       val (n_lst_all, n_lst_noinputs, n_lst_types) =
		    foldr (fn ((kind, hdr, typ), (lst_all, lst_noinputs, lst_types)) =>
			      let val lst_all' = NT.mk_nuprl_cons_term hdr lst_all
				  val lst_noinputs' =
				      if kind = "input"
				      then lst_noinputs
				      else NT.mk_nuprl_cons_term hdr lst_noinputs
				  val lst_types' =
				      if kind = "input"
				      then lst_types
				      else let val nty  = toNuprlIty_em 7 false context (E.stripIty typ)
					       val pair = NT.mk_pair_term hdr nty
					   in NT.mk_nuprl_cons_term pair lst_types
					   end
			      in (lst_all', lst_noinputs', lst_types)
			      end)
			  (NT.mk_nuprl_nil_term,
			   NT.mk_nuprl_nil_term,
			   NT.mk_nuprl_nil_term)
			  hdrs

		(* parameters for the case of the list with types *)
		val parms  = getDecFP ()
		val n_prs  = map NT.mk_variable_term parms
		(* the abstraction names *)
		val name_all      = "headers"
		val name_noinputs = "headers_no_inputs"
		val name_types    = "headers_no_inputs_types"
		val n_id_all      = mk_nuprl_user_term name_all      []
		val n_id_noinputs = mk_nuprl_user_term name_noinputs []
		val n_id_types    = mk_nuprl_user_term name_types    n_prs
		(* the types of the lists *)
		val n_typ1 = NT.mk_nuprl_name_term
		val n_typ2 = NT.mk_prod_term n_typ1 (NT.mk_nuprl_type_term "i")
		(* the abstractions and WF lemmas *)
		fun gen name id lst typ =
		    let val n_abs = NT.mk_nuprl_iabstraction_term id lst
			val n_typ = NT.mk_nuprl_list_term typ
			val n_mem = NT.mk_nuprl_member_term n_typ id
			val n_thm = NT.mk_nuprl_itheorem_term (get_user_name name ^ "_wf") n_mem
		    in [n_abs, n_thm]
		    end
		val objs1 = gen name_all      n_id_all      n_lst_all      n_typ1
		val objs2 = gen name_noinputs n_id_noinputs n_lst_noinputs n_typ1
		val objs3 = gen name_types    n_id_types    n_lst_types    n_typ2
		val _     = resetHeaders ()
		val _     = resetDecFP ()
	    in SOME (n_id_noinputs, objs1 @ objs2 @ objs3)
	    end
       else NONE
    end
*)

fun getHeadersObjs (SOME (_, _, hdrs, hdrs')) = (hdrs, hdrs')
  | getHeadersObjs NONE = ([], [])

fun getHeadersNoInputs' [] = NT.mk_nuprl_nil_term
  | getHeadersNoInputs' (obj :: objs) =
    if NT.is_nuprl_iinclude_properties_term obj
    then getHeadersNoInputs' (NT.rterm2term (#2 (NT.dest_iinclude_properties obj)) :: objs)
    else if NT.is_nuprl_iabstraction_term obj
    then let val term = NT.rterm2term (#2 (NT.dest_iabstraction obj))
	 in if String.isSuffix "headers_no_inputs" (NT.opid_of_term term)
	    then term
	    else getHeadersNoInputs' objs
	 end
    else getHeadersNoInputs' objs

fun getHeadersNoInputs (SOME (e_hdrs_no_inputs, _, objs)) =
    (e_hdrs_no_inputs, getHeadersNoInputs' objs)
  | getHeadersNoInputs NONE = (dummy_dot_term, NT.mk_nuprl_nil_term)

fun getHeadersInternal' [] = NT.mk_nuprl_nil_term
  | getHeadersInternal' (obj :: objs) =
    if NT.is_nuprl_iinclude_properties_term obj
    then getHeadersInternal' (NT.rterm2term (#2 (NT.dest_iinclude_properties obj)) :: objs)
    else if NT.is_nuprl_iabstraction_term obj
    then let val term = NT.rterm2term (#2 (NT.dest_iabstraction obj))
	 in if String.isSuffix "headers_internal" (NT.opid_of_term term)
	    then term
	    else getHeadersInternal' objs
	 end
    else getHeadersInternal' objs

fun getHeadersInternal (SOME (_, e_hdrs_internal, _, objs)) =
    (e_hdrs_internal, getHeadersInternal' objs)
  | getHeadersInternal NONE = (dummy_dot_term, NT.mk_nuprl_nil_term)

fun getExports term =
    let val terms  = A.getChildren term
    in case A.getKind term of
	   (A.DEC, A.DEC_EXPORT) => EXPORTS.addList (EXPORTS.empty, map A.getIdIdent terms)
	 | (A.DEC, _) => EXPORTS.empty
	 | _ => foldr (fn (t, exports) => EXPORTS.union (exports, getExports t))
		      EXPORTS.empty
		      terms
    end

fun generateStdMa context (e_hdrs, n_hdrs) term =
    (case A.getKind term of
	 (A.DEC, A.DEC_MAIN) =>
	 (case A.getChildren term of
	      [_] =>
	      let (*val name_stdma      = "stdma"*)
		  val name_constraint = "message-constraint"
		  val name_delivered  = "messages-delivered"
		  val exp             = A.idToExp "main"
		  val (nt,pr)         = toNuprlTerm context exp
		  val propip          = NT.mk_nuprl_prop_term "i'"
		  val lvl             = SOME "i"
		  (*val stdma           = NT.mk_nuprl_std_ma_term neo nt n_hdrs*)
		  val constraint      = NT.mk_nuprl_msg_interface_constraint_term nt n_hdrs (mk_nuprl_hidden_msg_fun ())
		  val delivered       = NT.mk_nuprl_msgs_interface_delivered_term nt (mk_nuprl_hidden_msg_fun ())
		  (*val lstdma          = NT.mk_lambda_term eo stdma*)
		  val schemeop        = SOME (propip, [])
		  val premises        = []
		  val programmable    = false
		  val localclass      = NONE
		  val data            = false
		  val label           = A.getLabel term
		  fun toAbs n name nterm exp =
		      let val (aid,lc,ab,th,rest,_) =
			      genAbstractionAndWF
				  n
				  context
				  ""
				  name
				  label
				  nterm
				  exp
				  schemeop
				  premises
				  programmable
				  localclass
				  data
				  lvl
		      in ab :: th :: rest
		      end
		  val res =
		      (*toAbs 18 name_stdma      lstdma      e_hdrs @*)
		      toAbs 19 name_constraint constraint e_hdrs @
		      toAbs 20 name_delivered  delivered  dummy_dot_term
		  val _ = resetDecFP ()
	      in res
	      end
	    | _ => [])
       | (A.DEC, _) => []
       | _ => List.concat
		  (map (generateStdMa context (e_hdrs, n_hdrs))
		       (A.getChildren term)))

fun getSpecPrefix context [] =
    if get_addpref_context context
    then (get_base_context context, [])
    else ("", [])
  | getSpecPrefix context (lst as (spec :: terms)) =
    if A.getKind spec = (A.DEC, A.DEC_SPEC)
    then case A.getChildren spec of
	     [id] => (A.getIdIdent id, terms)
	   | _ => raise Fail ("impossible:" ^ A.wrongFormat)
    else if get_addpref_context context
    then (get_base_context context, lst)
    else ("", lst)

fun checkOptions context term =
    case A.getKind term of
	(A.DEC, A.DEC_OPTIONS) =>
	foldl (fn (id, context) =>
		  case A.getIdIdent id of
		      "mono"    => set_poly_context    context false
		    | "poly"    => set_poly_context    context true
		    | "noextra" => set_extra_context   context false
		    | "extra"   => set_extra_context   context true
		    | "nonlp"   => set_nlp_context     context false
		    | "nlp"     => set_nlp_context     context true
		    | "newprog" => set_newprog_context context true
		    | "noprop"  => set_prop_context    context false
		    | _ => context)
	      context
	      (A.getChildren term)
      | (A.DEC, _) => context
      | _ => foldl (fn (t, context) => checkOptions context t)
		   context
		   (A.getChildren term)

fun mk_comment pref str =
    let val name  = "------" ^ "\\ " ^ pref ^ "\\ -\\ " ^ str ^ "\\ ------"
	val body  = NT.mk_nuprl_itext_nil_term
    in NT.mk_nuprl_icomment_term name body
    end

fun toNuprlDecs context [] = ([], context, NONE)
  | toNuprlDecs context ((A.N {kind = (A.DEC, A.DEC_DOC), label, value, regions, parents, children}) :: dec :: decs) =
    let val doc = map A.getValue children
	val (terms1, context1, prog1) = toNuprlDec  context doc dec
	val (terms2, context2, prog2) = toNuprlDecs context1 decs
    in (terms1 @ terms2, context2, get_prog_decs prog1 prog2)
    end
  | toNuprlDecs context (dec :: decs) =
    let val (terms1, context1, prog1) = toNuprlDec  context [] dec
	val (terms2, context2, prog2) = toNuprlDecs context1 decs
    in (terms1 @ terms2, context2, get_prog_decs prog1 prog2)
    end

(* FILE *)
fun toNuprlFile context (term as A.N {kind = (A.FILE, A.FILE_F), label, value, regions, parents, children}) =
    let val fname    = #file (OS.Path.splitDirFile value)
	val fbase    = #base (OS.Path.splitBaseExt fname)
	val _        = print ("[converting " ^ value ^ " (" ^ fname ^ ")]\n")
	(* -- we set the context -- *)
	val context0 = set_hiddens context term
	val context1 = set_base_context context0 fbase
	val (pref, children') = getSpecPrefix context children
	val _        = setPrefix pref
	(* -- we get the parameters -- *)
	val context2 = getParamsFromTerm term context1
	(* -- we get the abstract and data types -- *)
	val (types,context3) = getTypesFromTerm term context2
	(* -- we get the headers -- *)
	val headers  = getHeadersFromTerm term context3
	(* !! this won't work like that, we have to also treat the type
	 * declarations separately (abstypes, datatypes, constants) *)
	val hop      = generateHeaders context3 headers label
	val (h, h')  = getHeadersObjs hop
	(* -- we transform the file -- *)
	val (terms,context4,prog) = toNuprlDecs context3 children'
	(* -- we put everything together -- *)
	(* -- type bloc -- *)
	val com_typ  = mk_comment pref "types"
	val typ      = if List.null types
		       then []
		       else com_typ :: types
	(* -- header bloc -- *)
	val com_hdr  = mk_comment pref "headers"
	val hdr      = if List.null h orelse fname = "alldefs.eml"
		       then []
		       else com_hdr :: h
	(* -- spec bloc -- *)
	val com_spec = mk_comment pref "specification"
	val terms'   = if List.null terms
		       then []
		       else com_spec :: terms
	(* -- extra bloc -- *)
	val com_ex   = mk_comment pref "extra"
	val stdma    = generateStdMa context4 (getHeadersInternal hop) term
	val extra    = h' @ stdma
	val extra'   = if get_extra_context context4
			  andalso not (List.null extra)
			  andalso not (fname = "alldefs.eml")
		       then com_ex :: extra
		       else []
	(* -- *)
	val context5 = add_file_prog_context context4 value prog
	val context6 = reset_context context5
	val _        = reset_but_obid ()
    in (pref, typ @ hdr @ terms' @ extra', context6, prog)
    end
  | toNuprlFile context term = (print (A.toString term); raise Fail ("impossible:" ^ A.wrongFormat))

fun toNuprlFiles context [] = ([], [], context, NONE)
  | toNuprlFiles context (file :: files) =
    let val (pref,  terms1, context1, prog1) = toNuprlFile  context  file
	val (prefs, terms2, context2, prog2) = toNuprlFiles context1 files
    in (pref :: prefs, terms1 @ terms2, context2, get_prog_decs prog1 prog2)
    end

(* PROG *)
fun toNuprlProg context (term as A.N {kind = (A.PROG, A.PROG_P), label, value, regions, parents, children}) =
    toNuprlFiles context children
  | toNuprlProg context term = (print (A.toString term); raise Fail ("impossible:" ^ A.wrongFormat))

fun generateProperties context fname prefixes =
    let val prefix = case prefixes of [] => "" | xs => List.last xs
	val st1    = NT.mk_nuprl_istring_term (protect fname)
	val prop1  = NT.mk_nuprl_iproperty_term "name" st1
	val st2    = NT.mk_nuprl_istring_term (protect prefix)
	val prop2  = NT.mk_nuprl_iproperty_term "prefix" st2
	val ilist  = NT.mk_nuprl_finite_list_term (map NT.mk_nuprl_istring_term (get_list_exports_context context))
	val prop3  = NT.mk_nuprl_iproperty_term "exports" ilist
	val lst0   = NT.mk_nuprl_icons_nil_term
	val lst1   = NT.mk_nuprl_icons_cons_term prop1 lst0
	val lst2   = NT.mk_nuprl_icons_cons_term prop2 lst1
	val lst3   = NT.mk_nuprl_icons_cons_term prop3 lst2
    in lst3
    end

fun loadNuprlPrelude () =
    let val lst = [("inl",       "inl",       [("x", [])]),
		   ("inr",       "inr",       [("x", [])]),
		   ("isl",       "isl",       [("x", [])]),
		   ("fix",       "ycomb_fix", [("x", [])]),
		   ("!",         "bnot",      [("x", [])]),
		   ("not",       "not",       [("x", [])]),
		   ("fst",       "pi1",       [("x", [])]),
		   ("snd",       "pi2",       [("x", [])]),
		   ("eqof",      "eqof",      [("x", [])]),
		   ("location",  "es-loc",    [("x", [])])]
    in foldl (fn ((id, name, ar), context) =>
		 add_arity_context context id name ar)
	     mk_empty_context
	     lst
    end

fun form_prog context NONE = NONE
  | form_prog context (SOME (params, p)) =
    (* It might be better to just get the parameters in the order in which
     * they occur in the spec. *)
    SOME (params, NT.mk_nuprl_lambdas_term params p)

fun toStringProg NONE = "---"
  | toStringProg (SOME p) = NT.toStringTerm p

fun get_rid_of lst (NT.TERM (((opid, tag), params), [NT.B_TERM ([], rterm)])) =
    if List.exists (fn str => str = opid) lst
    then NT.rterm2term rterm
    else NT.TERM (((opid, tag), params), [NT.B_TERM ([], get_rid_of_rterm lst rterm)])
  | get_rid_of lst (NT.TERM (((opid, tag), params), bterms)) = NT.TERM (((opid, tag), params), map (get_rid_of_bterm lst) bterms)
  | get_rid_of lst (term as NT.AXM_TERM)                  = term
  | get_rid_of lst (term as NT.BOT_TERM)                  = term
  | get_rid_of lst (term as NT.INT_TERM)                  = term
  | get_rid_of lst (term as NT.VOI_TERM)                  = term
  | get_rid_of lst (term as NT.DUM_TERM)                  = term
  | get_rid_of lst (term as NT.ATM_TERM _)                = term
  | get_rid_of lst (term as NT.TOK_TERM _)                = term
  | get_rid_of lst (term as NT.NAT_TERM _)                = term
  | get_rid_of lst (term as NT.VAR_TERM var)              = term
  | get_rid_of lst (term as NT.INL_TERM rterm)            = NT.INL_TERM (get_rid_of_rterm lst rterm)
  | get_rid_of lst (term as NT.INR_TERM rterm)            = NT.INR_TERM (get_rid_of_rterm lst rterm)
  | get_rid_of lst (term as NT.FIX_TERM rterm)            = NT.FIX_TERM (get_rid_of_rterm lst rterm)
  | get_rid_of lst (term as NT.MIN_TERM rterm)            = NT.MIN_TERM (get_rid_of_rterm lst rterm)
  | get_rid_of lst (term as NT.LAM_TERM (var, rterm))     = NT.LAM_TERM (var, get_rid_of_rterm lst rterm)
  | get_rid_of lst (term as NT.REC_TERM (var, rterm))     = NT.REC_TERM (var, get_rid_of_rterm lst rterm)
  | get_rid_of lst (term as NT.WAI_TERM (rterm1, rterm2)) = NT.WAI_TERM (get_rid_of_rterm lst rterm1, get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.APP_TERM (rterm1, rterm2)) = NT.APP_TERM (get_rid_of_rterm lst rterm1, get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.PAI_TERM (rterm1, rterm2)) = NT.PAI_TERM (get_rid_of_rterm lst rterm1, get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.ADD_TERM (rterm1, rterm2)) = NT.ADD_TERM (get_rid_of_rterm lst rterm1, get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.SUB_TERM (rterm1, rterm2)) = NT.SUB_TERM (get_rid_of_rterm lst rterm1, get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.MUL_TERM (rterm1, rterm2)) = NT.MUL_TERM (get_rid_of_rterm lst rterm1, get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.DIV_TERM (rterm1, rterm2)) = NT.DIV_TERM (get_rid_of_rterm lst rterm1, get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.REM_TERM (rterm1, rterm2)) = NT.REM_TERM (get_rid_of_rterm lst rterm1, get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.EQT_TERM (rterm1, rterm2)) = NT.EQT_TERM (get_rid_of_rterm lst rterm1, get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.UNI_TERM (rterm1, rterm2)) = NT.UNI_TERM (get_rid_of_rterm lst rterm1, get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.EQU_TERM (a, rterm1, rterm2)) =
    NT.EQU_TERM (get_rid_of_rterm lst a,
		 get_rid_of_rterm lst rterm1,
		 get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.IAX_TERM (a, rterm1, rterm2)) =
    NT.IAX_TERM (get_rid_of_rterm lst a,
		 get_rid_of_rterm lst rterm1,
		 get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.IPA_TERM (a, rterm1, rterm2)) =
    NT.IPA_TERM (get_rid_of_rterm lst a,
		 get_rid_of_rterm lst rterm1,
		 get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.IIR_TERM (a, rterm1, rterm2)) =
    NT.IIR_TERM (get_rid_of_rterm lst a,
		 get_rid_of_rterm lst rterm1,
		 get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.IIL_TERM (a, rterm1, rterm2)) =
    NT.IIL_TERM (get_rid_of_rterm lst a,
		 get_rid_of_rterm lst rterm1,
		 get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.IIN_TERM (a, rterm1, rterm2)) =
    NT.IIN_TERM (get_rid_of_rterm lst a,
		 get_rid_of_rterm lst rterm1,
		 get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.ILA_TERM (a, rterm1, rterm2)) =
    NT.ILA_TERM (get_rid_of_rterm lst a,
		 get_rid_of_rterm lst rterm1,
		 get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.IAT_TERM (a, rterm1, rterm2)) =
    NT.IAT_TERM (get_rid_of_rterm lst a,
		 get_rid_of_rterm lst rterm1,
		 get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.CBV_TERM (a, x, f)) =
    NT.CBV_TERM (get_rid_of_rterm lst a,
		 x,
		 get_rid_of_rterm lst f)
  | get_rid_of lst (term as NT.CBA_TERM (a, x, f)) =
    NT.CBA_TERM (get_rid_of_rterm lst a,
		 x,
		 get_rid_of_rterm lst f)
  | get_rid_of lst (term as NT.FUN_TERM (a, x, f)) =
    NT.FUN_TERM (get_rid_of_rterm lst a,
		 x,
		 get_rid_of_rterm lst f)
  | get_rid_of lst (term as NT.PRD_TERM (a, x, f)) =
    NT.PRD_TERM (get_rid_of_rterm lst a,
		 x,
		 get_rid_of_rterm lst f)
  | get_rid_of lst (term as NT.TUN_TERM (a, x, f)) =
    NT.TUN_TERM (get_rid_of_rterm lst a,
		 x,
		 get_rid_of_rterm lst f)
  | get_rid_of lst (term as NT.SET_TERM (a, x, f)) =
    NT.SET_TERM (get_rid_of_rterm lst a,
		 x,
		 get_rid_of_rterm lst f)
  | get_rid_of lst (term as NT.LES_TERM (a, b, rterm1, rterm2)) =
    NT.LES_TERM (get_rid_of_rterm lst a,
		 get_rid_of_rterm lst b,
		 get_rid_of_rterm lst rterm1,
		 get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.IEQ_TERM (a, b, rterm1, rterm2)) =
    NT.IEQ_TERM (get_rid_of_rterm lst a,
		 get_rid_of_rterm lst b,
		 get_rid_of_rterm lst rterm1,
		 get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.SPR_TERM (pair, var1, var2, rterm)) =
    NT.SPR_TERM (get_rid_of_rterm lst pair,
		 var1,
		 var2,
		 get_rid_of_rterm lst rterm)
  | get_rid_of lst (term as NT.AEQ_TERM (n, a, b, rterm1, rterm2)) =
    NT.AEQ_TERM (n,
		 get_rid_of_rterm lst a,
		 get_rid_of_rterm lst b,
		 get_rid_of_rterm lst rterm1,
		 get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.DEC_TERM (dec, var1, rterm1, var2, rterm2)) =
    NT.DEC_TERM (get_rid_of_rterm lst dec,
		 var1,
		 get_rid_of_rterm lst rterm1,
		 var2,
		 get_rid_of_rterm lst rterm2)
  | get_rid_of lst (term as NT.IND_TERM (i, x, rd, downcase, basecase, y, ru, upcase)) =
    NT.IND_TERM (get_rid_of_rterm lst i,
		 x,
		 rd,
		 get_rid_of_rterm lst downcase,
		 get_rid_of_rterm lst basecase,
		 y,
		 ru,
		 get_rid_of_rterm lst upcase)
  | get_rid_of lst (term as NT.CLO_TERM clos) = raise Fail "get_rid_of:C_TERM"

and get_rid_of_rterm lst rterm = NT.mk_rterm (get_rid_of lst (NT.rterm2term rterm))

and get_rid_of_bterm lst (NT.B_TERM (vars, rterm)) =
    NT.B_TERM (vars, get_rid_of_rterm lst rterm)

fun toNuprl mapop file obid term env sub addpref poly pextra btyp cbva newprog =
    let val fname   = #file (OS.Path.splitDirFile file)
	val fbase   = #base (OS.Path.splitBaseExt fname)
	val _       = A.setIdsTerm term
	val _       = reset ()
	val _       = setObid obid
	val ctxt1   = loadNuprlPrelude ()
	val ctxt2   = set_ndefs_context   ctxt1  mapop
	val ctxt3   = set_poly_context    ctxt2  poly
	val ctxt4   = set_extra_context   ctxt3  pextra
	val ctxt5   = set_base_context    ctxt4  fbase
	val ctxt6   = set_addpref_context ctxt5  addpref
	val ctxt7   = set_typ_context     ctxt6  btyp
	val ctxt8   = checkOptions        ctxt7  term
	val ctxt9   = set_exports_context ctxt8  (getExports term)
	(*val ctxt10  = set_hidden_eo       ctxt9  term*)
	val ctxt11  = set_cbva_context    (*ctxt10*) ctxt9 cbva
	val ctxt12  = maybe_set_newprog_context ctxt11 newprog
	val solved  = EN.solver env sub
	val _       = setUnifEnv solved
	val (prefs,terms,context,prog) = toNuprlProg ctxt12 term
	val terms   = List.map (get_rid_of ["eqof"]) terms
	val p       = form_prog context prog
	(*val _       = print (toStringProg p)*)
	val cons1   = generateProperties context fname prefs
	val n_terms = cons1 :: terms
	val _       = A.resetSimp ()
    in (n_terms, p)
    end


(* ------ EXPORT NUPRL TO E# ------ *)

(*fun prelude () =
    let val list_ind =
	    let val nilcase  = A.idToExp "nilcase"
		val patnil   = A.idToPat "nil"
		val branch1  = A.mk_new_term A.MRULE_M "" [] [patnil, nilcase]
		val conscase = A.idToExp "conscase"
		val x_id     = A.mk_new_term A.ID_VID "x"  [] []
		val xs_id    = A.mk_new_term A.ID_VID "xs" [] []
		val z_id     = A.mk_new_term A.ID_VID "z"  [] []
		val x_atexp  = A.mk_new_term A.ATEXP_ID "" [] [x_id]
		val xs_atexp = A.mk_new_term A.ATEXP_ID "" [] [xs_id]
		val z_atexp  = A.mk_new_term A.ATEXP_ID "" [] [z_id]
		val app1     = A.mk_new_term A.EXP_APP "" [] [conscase, x_atexp]
		val app2     = A.mk_new_term A.EXP_APP "" [] [app1, xs_atexp]
		val app3     = A.mk_new_term A.EXP_APP "" [] [app2, z_atexp]
		val lam      = A.mk_new_term A.EXP_LAMBDA "" [] [z_id, app3]
		val patcons  = A.mk_new_term A.PAT_CONS "." [] [A.idToPat "x", A.idToPat "xs"]
		val branch2  = A.mk_new_term A.MRULE_M "" [] [patcons, lam]
		val atwild   = A.mk_new_term A.ATPAT_WILD "" [] []
		val patwild  = A.mk_new_term A.PAT_ATPAT "" [] [atwild]
		(* NOTE: for the last branch, we don't case what's the expression,
		 * this branch is never gonna be reached. *)
		val branch3  = A.mk_new_term A.MRULE_M "" [] [patwild, nilcase]
		val match    = A.mk_new_term A.MATCH_M "" [] [branch1, branch2]
		val m_exp    = A.mk_new_term A.EXP_MATCH "" [] [A.idToExp "L", match]
		val L_id     = A.mk_new_term A.ID_VID "L" [] []
		val nil_id   = A.mk_new_term A.ID_VID "nilcase" [] []
		val cons_id  = A.mk_new_term A.ID_VID "conscase" [] []
		val L_arg    = A.mk_new_term A.ATPAT_ID "" [] [L_id]
		val nil_arg  = A.mk_new_term A.ATPAT_ID "" [] [nil_id]
		val cons_arg = A.mk_new_term A.ATPAT_ID "" [] [cons_id]
		val args     = A.mk_new_term A.PARAM_P "" [] [L_arg, nil_arg, cons_arg]
		val li_id    = A.mk_new_term A.ID_VID "list_ind" [] []
		val bind     = A.mk_new_term A.BIND_DEC "" [] [li_id, args, m_exp]
	    in A.mk_new_term A.DEC_LET "" [] [bind]
	    end
    in [list_ind]
    end*)

(*
fun fromNuprlExp (NT.TERM ((("lambda", "OPID"), parameters), [NT.B_TERM ([var], term)])) =
    let val id  = A.mk_new_term A.ID_VID var [] []
	val exp = fromNuprlExp term
    in A.mk_new_term A.EXP_LAMBDA "" [] [id, exp]
    end
  | fromNuprlExp (NT.TERM ((("natural_number", "OPID"), [(n, "n")]), [])) =
    (case Int.fromString n of
	 NONE     => raise Fail "impossible:wrong format"
       | SOME nat =>
	 let val sc    = A.mk_new_term A.SCON_INT n [] []
	     val atexp = A.mk_new_term A.ATEXP_SCON "" [] [sc]
	 in A.mk_new_term A.EXP_ATEXP "" [] [atexp]
	 end)
  | fromNuprlExp (NT.TERM ((("variable", "OPID"), [(var, "v")]), [])) = A.idToExp var
  | fromNuprlExp (NT.TERM ((("apply", "OPID"), []), [NT.B_TERM ([], term1), NT.B_TERM ([], term2)])) =
    let val exp1  = fromNuprlExp term1
	val exp2  = fromNuprlExp term2
	val atexp = A.mk_new_term A.ATEXP_PAREN "" [] [exp2]
    in A.mk_new_term A.EXP_APP "" [] [exp1, atexp]
    end
  | fromNuprlExp (NT.TERM ((("list_ind", "OPID"), []), [NT.B_TERM ([], L), NT.B_TERM ([], nilcase), NT.B_TERM ([x, xs, r], conscase)])) =
    let val li_exp     = A.idToExp "list_ind"
	val L_exp      = fromNuprlExp L
	val L_atexp    = A.mk_new_term A.ATEXP_PAREN "" [] [L_exp]
	val nil_exp    = fromNuprlExp nilcase
	val nil_atexp  = A.mk_new_term A.ATEXP_PAREN "" [] [nil_exp]
	val x_id       = A.mk_new_term A.ID_VID x  [] []
	val xs_id      = A.mk_new_term A.ID_VID xs [] []
	val r_id       = A.mk_new_term A.ID_VID r  [] []
	val cons_exp   = fromNuprlExp conscase
	val lam1       = A.mk_new_term A.EXP_LAMBDA "" [] [r_id,  cons_exp]
	val lam2       = A.mk_new_term A.EXP_LAMBDA "" [] [xs_id, lam1]
	val lam3       = A.mk_new_term A.EXP_LAMBDA "" [] [x_id,  lam2]
	val lam_atexp  = A.mk_new_term A.ATEXP_PAREN "" [] [lam3]
	val app1       = A.mk_new_term A.EXP_APP "" [] [li_exp, L_atexp]
	val app2       = A.mk_new_term A.EXP_APP "" [] [app1, nil_atexp]
	val app3       = A.mk_new_term A.EXP_APP "" [] [app2, lam_atexp]
    in app3
    end
  (* TODO: Othersize we've got to look it up *)
  | fromNuprlExp _ = raise Fail "impossible:can't import the Nuprl term"

fun fromNuprlLhsRhs id lhs rhs =
    case lhs of
	NT.TERM (((opid, "OPID"), parameters), []) =>
	if opid = id
	then let val id   = A.mk_new_term A.ID_VID id [] []
		 val exp  = fromNuprlExp rhs
		 val list = map (fn (value, kind) =>
				    let val id    = A.mk_new_term A.ID_VID value [] []
				    in A.mk_new_term A.ATPAT_ID "" [] [id]
				    end)
				  parameters
		 val args = A.mk_new_term A.PARAM_P  "" [] list
		 val bind = A.mk_new_term A.BIND_DEC "" [] [id, args, exp]
		 val dec  = A.mk_new_term A.DEC_LET  "" [] [bind]
	     in dec
	     end
	else raise Fail "impossible:wrong term"
      | _ => raise Fail "impossible:wrong format"
*)

end
