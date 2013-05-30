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
 *  o File name:   GenLib.sml
 *  o Description: generates a list of EML constant declarations.
 *)


structure GenLib :> GENLIB = struct

structure E  = Env
structure D  = Deps
structure T  = ListFormat
structure P  = ParserNuprlAscii
structure N  = NuprlTerms
structure EH = LibBase

structure MAP = BinaryMapFn(type ord_key = string val compare = String.compare)
structure SET = BinarySetFn(type ord_key = string val compare = String.compare)
structure IND = BinaryMapFn(type ord_key = int    val compare = Int.compare)

type nuprl_so_types = (string * E.ity) list

type nuprl_scheme = string list                              (* bound type variables       *)
		    * (string * nuprl_so_types * E.ity) list (* arguments with their types *)
		    * E.ity                                  (* returned type              *)

val empty_nuprl_map =
    ref (MAP.empty : (N.parameter_value
		      * N.nuprl_term
		      * N.nuprl_term
		      * (N.nuprl_term * nuprl_scheme option) list) list MAP.map)

fun map2Lisp map =
    let val lst =
	    MAP.foldri (fn (id, entries, lst) => (*(obid, lhs, rhs, wfs)*)
			   id :: lst)
		       []
		       map
    in T.fmt {init  = "(",
	      final = ")",
	      sep   = " . ",
	      fmt   = fn x => x}
	     lst
    end

(*
fun exportMap2Lisp' output =
    let val output' = output ^ ".tmp"
	val stout   = TextIO.openOut output'
	val ()      =
	    MAP.appi (fn (id, entries) => (*(obid, lhs, rhs, wfs)*)
			 (TextIO.output (stout, id ^ ":\n");
			  foldl (fn ((obid, lhs, rhs, wfs), n) =>
				    foldl (fn ((wf, NONE), n) =>
					      let val st = "    type"     ^
							   Int.toString n ^
							   "?\n"
					      in (TextIO.output (stout, st); n+1)
					      end
					    | ((wf, SOME (bound, args, ity)), n) =>
					      let val ast =
						      if List.null args
						      then ""
						      else T.fmt {init  = "(",
								  final = ")",
								  sep   = "; ",
								  fmt   = fn (x, ity) => x ^ ": " ^ E.ppIty'' ity (fn x => "'" ^ x)}
								 args
						  val st ="    type"     ^
							  Int.toString n ^
							  ": constant "  ^
							  id             ^
							  " "            ^
							  ast            ^
							  " : "          ^
							  E.ppIty' ity   ^
							  "\n"
					      in (TextIO.output (stout, st); n+1)
					      end)
					  n
					  wfs)
				1
				entries;
			  TextIO.output (stout, "\n");
			  ()))
		     (getMap ())
	val _       = TextIO.closeOut stout
	val _       = OS.FileSys.rename {old = output', new = output}
    in ()
    end
*)

fun is_defined_as_type' args ity =
    E.isItyType ity
    andalso
    List.all (fn (x,sos,ityt) =>
		 List.null sos andalso E.isItyType ity)
			      args

fun is_defined_as_type m opid =
    case MAP.find (m, opid) of
	SOME lst =>
	List.all (fn (obid, lhs, rhs, wfs) =>
		     List.all (fn (wf, NONE) => false
				| (wf, SOME (ebound, args, ty)) =>
				  is_defined_as_type' args ty)
			      wfs)
		 lst
      | NONE => false

fun get_nb_args_of_type m opid =
    case MAP.find (m, opid) of
	SOME ((obid, lhs, rhs, ((wf, SOME (ebound, args, ty)) :: wfs)) :: xs) =>
	List.length args
      | SOME _ => raise Fail "get_type_signature"
      | NONE => raise Fail "get_type_signature"

fun is_valid_id id =
    not (List.exists (fn x => id = x)
		     ["let",    "data", "false", "true",
		      "exists", "and",  "or",    "self",
		      "type",   "nil"]
	 orelse String.isSubstring "!"      id
	 orelse String.isSubstring "+"      id
	 orelse String.isSubstring "*"      id
	 orelse String.isSubstring "?"      id
	 orelse String.isSubstring "<"      id
	 orelse String.isSubstring ">"      id)

fun exportMap2Lisp m ind n output =
    let fun ppItyTik ity = E.ppIty'' ity (fn x => "'" ^ x)
	(* -- prints the type arguments for a non type constant -- *)
	fun ppSo (x, ity) =
	    if is_valid_id x
	    then x ^ ": " ^ ppItyTik ity
	    else raise Fail ""
	fun ppSos sos =
	    if List.null sos
	    then ""
	    else T.fmt {init  = "[",
			final = "]",
			sep   = "; ",
			fmt   = ppSo}
		       sos
	fun ppArg (x, sos, ity) =
	    if is_valid_id x
	    then let val pref =
			 if E.isItyType ity
			 then if List.null sos
			      then "'"
			      else raise Fail ""
			 else ""
		 in pref ^ x ^ ppSos sos ^ ": " ^ ppItyTik ity
		 end
	    else raise Fail ""
	fun ppArgs args =
	    if List.null args
	    then ""
	    else T.fmt {init  = " (",
			final = ")",
			sep   = "; ",
			fmt   = ppArg}
		       args
	(* -- prints the type arguments for a type constant -- *)
	fun ppArgTy (x, sos, ity) =
	    if is_valid_id x andalso List.null sos andalso E.isItyType ity
	    then "'" ^ x
	    else raise Fail "GenLib:ppArgTy"
	fun ppArgsTy [] = ""
	  | ppArgsTy [arg] = ppArgTy arg ^ " "
	  | ppArgsTy args =
	    T.fmt {init  = "(",
		   final = ") ",
		   sep   = ", ",
		   fmt   = ppArgTy}
		  args
	(* -- print a constant statement -- *)
	fun toConstant id args ity =
	    if is_valid_id id
	    then (if is_defined_as_type' args ity
		  then "constant "   ^
		       ppArgsTy args ^
		       id            ^
		       " : Type ;;\n"
		  else "constant "  ^
		       id           ^
		       ppArgs args  ^
		       " : "        ^
		       ppItyTik ity ^
		       " ;;\n")
		 handle _ => ""
	    else ""
	val output' = output ^ ".tmp"
	val stout   = TextIO.openOut output'
	val ()      =
	    IND.appi (fn (i, id) =>
			 case MAP.find (m, id) of
			     SOME entries => (*(obid, lhs, rhs, wfs)*)
			     if Substring.isSubstring "\\ " (Substring.full id)
			     then ()
			     else app (fn (obid, lhs, rhs, wfs) =>
					  let val lst =
						  List.mapPartial
						      (fn (wf, NONE) => NONE
							| (wf, SOME (bound, args, ity)) =>
							  let val st = toConstant id args ity
							  in SOME st
							  end)
						      wfs
					      (*val _ =
						  if List.length lst > 1
						  then print ("[" ^ id ^ "]\n")
						  else ()*)
					  (*app (fn st => TextIO.output (stout, st))
						lst*)
					  in case lst of
						 [] => ()
					       | st :: _ => TextIO.output (stout, st)
					  end)
				      entries
			   | NONE => ())
		     ind
	val _ = TextIO.closeOut stout
	val _ = OS.FileSys.rename {old = output', new = output}
    in ()
    end

(* 164, 186, 199, 206, 214, 273, 288 *)
(* pi1, pi2 *)
(* l_member : T : Type -> T List -> Prop
 *
 * l_member Int L
 *)

fun DBG id x =
    if id = "no_repeats"
    then print (x ^ "\n")
    else ()

datatype SUB = POS | NEG

fun is_nuprl_type_term (N.TERM ((("universe", tag), params), [])) = true
  | is_nuprl_type_term _ = false

fun dest_nuprl_applies_term (N.TERM ((("apply", tag), params),
				    [N.B_TERM ([], f),
				     N.B_TERM ([], arg)])) =
    let val (h, args) = dest_nuprl_applies_term (N.rterm2term f)
    in (h, args @ [N.rterm2term arg])
    end
  | dest_nuprl_applies_term (term as N.APP_TERM (rterm1, rterm2)) =
    let val (h, args) = dest_nuprl_applies_term (N.rterm2term rterm1)
    in (h, args @ [N.rterm2term rterm2])
    end
  | dest_nuprl_applies_term (term as N.TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.AXM_TERM)   = (term, [])
  | dest_nuprl_applies_term (term as N.BOT_TERM)   = (term, [])
  | dest_nuprl_applies_term (term as N.INT_TERM)   = (term, [])
  | dest_nuprl_applies_term (term as N.VOI_TERM)   = (term, [])
  | dest_nuprl_applies_term (term as N.DUM_TERM)   = (term, [])
  | dest_nuprl_applies_term (term as N.ATM_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.TOK_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.NAT_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.VAR_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.INL_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.INR_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.FIX_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.MIN_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.LAM_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.REC_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.WAI_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.PAI_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.ADD_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.SUB_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.MUL_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.DIV_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.REM_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.EQT_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.UNI_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.EQU_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.IAX_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.IPA_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.IIR_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.IIL_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.IIN_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.ILA_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.IAT_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.CBV_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.CBA_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.FUN_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.PRD_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.TUN_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.SET_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.LES_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.IEQ_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.SPR_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.AEQ_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.DEC_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.IND_TERM _) = (term, [])
  | dest_nuprl_applies_term (term as N.CLO_TERM clos) = raise Fail "dest_nuprl_applies_term:CLO_TERM"

fun nuprlType2Ity force m sub bound (N.TERM ((("variable", tag), [(v, "v")]), [])) =
    if List.exists (fn x => (x = v)) bound
    then SOME (E.ITYETV (v, D.dummy_label))
    else NONE
  | nuprlType2Ity force m sub bound (N.TERM ((("universe", tag), params), [])) =
    SOME (E.mk_type_type D.dummy_label)
  | nuprlType2Ity force m sub bound (N.TERM ((("prop", tag), params), [])) =
    SOME (E.mk_type_prop D.dummy_label)
  | nuprlType2Ity force m sub bound (N.TERM ((("list", tag), []), [N.B_TERM ([], t)])) =
    (case nuprlType2Ity force m sub bound (N.rterm2term t) of
	 SOME ity => SOME (E.mk_type_list ity D.dummy_label)
       | NONE => NONE)
  | nuprlType2Ity force m sub bound (N.TERM ((("bag", tag), []), [N.B_TERM ([], t)])) =
    (case nuprlType2Ity force m sub bound (N.rterm2term t) of
	 SOME ity => SOME (E.mk_type_bag ity D.dummy_label)
       | NONE => NONE)
  | nuprlType2Ity force m sub bound (N.TERM ((("eclass", tag), [lexp_param]), [N.B_TERM ([], info), N.B_TERM ([e, es], t)])) =
    (case nuprlType2Ity force m sub bound (N.rterm2term t) of
	 SOME ity => SOME (E.mk_type_class ity D.dummy_label)
       | NONE => NONE)
  | nuprlType2Ity force m sub bound (N.TERM ((("deq", tag), []), [N.B_TERM ([], t)])) =
    (case nuprlType2Ity force m sub bound (N.rterm2term t) of
	 SOME ity => SOME (E.mk_type_deq ity D.dummy_label)
       | NONE => NONE)
  | nuprlType2Ity force m sub bound (N.TERM ((("union", tag), []), [N.B_TERM ([], arg1), N.B_TERM ([], arg2)])) =
    (case (nuprlType2Ity force m sub bound (N.rterm2term arg1), nuprlType2Ity force m sub bound (N.rterm2term arg2)) of
	 (SOME ity1, SOME ity2) => SOME (E.mk_type_disju (ity1, ity2) D.dummy_label)
       | _ => NONE)
  | nuprlType2Ity force m sub bound (N.TERM ((("function", tag), []), [N.B_TERM ([], arg1), N.B_TERM ([v], arg2)])) =
    if N.is_null_nuprl_var v
    then let val (sub1, sub2) = case sub of NEG => (POS, NEG) | POS => (NEG, POS)
	 in case (nuprlType2Ity force m sub1 bound (N.rterm2term arg1), nuprlType2Ity force m sub2 bound (N.rterm2term arg2)) of
		(SOME ity1, SOME ity2) => SOME (E.mk_type_arrow (ity1, ity2) D.dummy_label)
	      | _ => NONE
	 end
    else NONE (*raise Fail "nuprlType2Ity:function"*)
  | nuprlType2Ity force m sub bound (N.TERM ((("product", tag), []), [N.B_TERM ([], arg1), N.B_TERM ([v], arg2)])) =
    if N.is_null_nuprl_var v
    then case (nuprlType2Ity force m sub bound (N.rterm2term arg1), nuprlType2Ity force m sub bound (N.rterm2term arg2)) of
	     (SOME ity1, SOME ity2) => SOME (E.mk_type_tuple [ity1, ity2] D.dummy_label)
	   | _ => NONE
    else NONE (*raise Fail "nuprlType2Ity:product"*)
  | nuprlType2Ity force m sub bound (N.TERM ((("nat", tag), []), [])) =
    (case sub of
	 POS => SOME (E.mk_type_int D.dummy_label)
       | NEG =>
	 if force
	 then SOME (E.mk_type_int D.dummy_label)
	 else NONE)
  | nuprlType2Ity force m sub bound (N.TERM ((("int_upper", tag), []), [N.B_TERM ([], n)])) =
    (case sub of
	 POS => SOME (E.mk_type_int D.dummy_label)
       | NEG =>
	 if force
	 then SOME (E.mk_type_int D.dummy_label)
	 else NONE)
  | nuprlType2Ity force m sub bound (N.TERM ((("set", tag), []), [N.B_TERM ([], typ), N.B_TERM ([x], prop)])) =
    (case sub of
	 POS => nuprlType2Ity force m sub bound (N.rterm2term typ)
       | NEG =>
	 if force
	 then nuprlType2Ity force m sub bound (N.rterm2term typ)
	 else NONE)
  | nuprlType2Ity force m sub bound (N.TERM ((("int",       tag), []), [])) = SOME (E.mk_type_int  D.dummy_label)
  | nuprlType2Ity force m sub bound (N.TERM ((("unit",      tag), []), [])) = SOME (E.mk_type_unit D.dummy_label)
  | nuprlType2Ity force m sub bound (N.TERM ((("bool",      tag), []), [])) = SOME (E.mk_type_bool D.dummy_label)
  | nuprlType2Ity force m sub bound (N.TERM ((("real",      tag), []), [])) = SOME (E.mk_type_real D.dummy_label)
  | nuprlType2Ity force m sub bound (N.TERM ((("atom",      tag), []), [])) = SOME (E.mk_type_atom D.dummy_label)
  | nuprlType2Ity force m sub bound (N.TERM ((("Id",        tag), []), [])) = SOME (E.mk_type_loc  D.dummy_label)
  | nuprlType2Ity force m sub bound (N.TERM ((("Message",   tag), []), [])) = SOME (E.mk_type_msg  D.dummy_label)
  | nuprlType2Ity force m sub bound (N.TERM (((opid, tag), params), bterms)) =
    if is_valid_id opid andalso is_defined_as_type m opid
    then let val itys   =
		 List.mapPartial
		     (fn N.B_TERM ([], typ) => nuprlType2Ity force m sub bound (N.rterm2term typ)
		       | _ => NONE)
		     bterms
	     val ityseq = E.ITYSEQSEQ (itys, D.dummy_label)
	     val itycon = E.mk_typecon_gen D.dummy_label opid false
	 in if List.length itys = List.length bterms
	       andalso
	       List.length itys = get_nb_args_of_type m opid
	    then SOME (E.ITYCON (ityseq, itycon))
	    else NONE
	 end
    else NONE
  | nuprlType2Ity force m sub bound N.AXM_TERM     = raise Fail "nuprlType2Ity:AXM_TERM"
  | nuprlType2Ity force m sub bound N.BOT_TERM     = raise Fail "nuprlType2Ity:BOT_TERM"
  | nuprlType2Ity force m sub bound N.INT_TERM     = SOME (E.mk_type_int D.dummy_label)
  | nuprlType2Ity force m sub bound N.VOI_TERM     = raise Fail "nuprlType2Ity:VOI_TERM"
  | nuprlType2Ity force m sub bound N.DUM_TERM     = raise Fail "nuprlType2Ity:DUM_TERM"
  | nuprlType2Ity force m sub bound (N.ATM_TERM nop) =
    (case nop of
	 NONE => SOME (E.mk_type_atom D.dummy_label)
       | SOME n => raise Fail "nuprlType2Ity:ATM_TERM")
  | nuprlType2Ity force m sub bound (N.TOK_TERM _) = raise Fail "nuprlType2Ity:TOK_TERM"
  | nuprlType2Ity force m sub bound (N.NAT_TERM _) = raise Fail "nuprlType2Ity:NAT_TERM"
  | nuprlType2Ity force m sub bound (N.VAR_TERM _) = raise Fail "nuprlType2Ity:VAR_TERM"
  | nuprlType2Ity force m sub bound (N.INL_TERM _) = raise Fail "nuprlType2Ity:INL_TERM"
  | nuprlType2Ity force m sub bound (N.INR_TERM _) = raise Fail "nuprlType2Ity:INR_TERM"
  | nuprlType2Ity force m sub bound (N.FIX_TERM _) = raise Fail "nuprlType2Ity:FIX_TERM"
  | nuprlType2Ity force m sub bound (N.MIN_TERM _) = raise Fail "nuprlType2Ity:MIN_TERM"
  | nuprlType2Ity force m sub bound (N.LAM_TERM _) = raise Fail "nuprlType2Ity:LAM_TERM"
  | nuprlType2Ity force m sub bound (N.REC_TERM _) = raise Fail "nuprlType2Ity:REC_TERM"
  | nuprlType2Ity force m sub bound (N.WAI_TERM _) = raise Fail "nuprlType2Ity:WAI_TERM"
  | nuprlType2Ity force m sub bound (N.APP_TERM _) = raise Fail "nuprlType2Ity:APP_TERM"
  | nuprlType2Ity force m sub bound (N.PAI_TERM _) = raise Fail "nuprlType2Ity:PAI_TERM"
  | nuprlType2Ity force m sub bound (N.ADD_TERM _) = raise Fail "nuprlType2Ity:ADD_TERM"
  | nuprlType2Ity force m sub bound (N.SUB_TERM _) = raise Fail "nuprlType2Ity:SUB_TERM"
  | nuprlType2Ity force m sub bound (N.MUL_TERM _) = raise Fail "nuprlType2Ity:MUL_TERM"
  | nuprlType2Ity force m sub bound (N.DIV_TERM _) = raise Fail "nuprlType2Ity:DIV_TERM"
  | nuprlType2Ity force m sub bound (N.REM_TERM _) = raise Fail "nuprlType2Ity:REM_TERM"
  | nuprlType2Ity force m sub bound (N.EQT_TERM _) = raise Fail "nuprlType2Ity:EQT_TERM"
  | nuprlType2Ity force m sub bound (N.UNI_TERM _) = raise Fail "nuprlType2Ity:UNI_TERM"
  | nuprlType2Ity force m sub bound (N.EQU_TERM _) = raise Fail "nuprlType2Ity:EQU_TERM"
  | nuprlType2Ity force m sub bound (N.IAX_TERM _) = raise Fail "nuprlType2Ity:IAX_TERM"
  | nuprlType2Ity force m sub bound (N.IPA_TERM _) = raise Fail "nuprlType2Ity:IPA_TERM"
  | nuprlType2Ity force m sub bound (N.IIR_TERM _) = raise Fail "nuprlType2Ity:IIR_TERM"
  | nuprlType2Ity force m sub bound (N.IIL_TERM _) = raise Fail "nuprlType2Ity:IIL_TERM"
  | nuprlType2Ity force m sub bound (N.IIN_TERM _) = raise Fail "nuprlType2Ity:IIN_TERM"
  | nuprlType2Ity force m sub bound (N.ILA_TERM _) = raise Fail "nuprlType2Ity:ILA_TERM"
  | nuprlType2Ity force m sub bound (N.IAT_TERM _) = raise Fail "nuprlType2Ity:IAT_TERM"
  | nuprlType2Ity force m sub bound (N.CBV_TERM _) = raise Fail "nuprlType2Ity:CBV_TERM"
  | nuprlType2Ity force m sub bound (N.CBA_TERM _) = raise Fail "nuprlType2Ity:CBA_TERM"
  | nuprlType2Ity force m sub bound (N.FUN_TERM _) = raise Fail "nuprlType2Ity:FUN_TERM"
  | nuprlType2Ity force m sub bound (N.PRD_TERM _) = raise Fail "nuprlType2Ity:PRD_TERM"
  | nuprlType2Ity force m sub bound (N.TUN_TERM _) = raise Fail "nuprlType2Ity:TUN_TERM"
  | nuprlType2Ity force m sub bound (N.SET_TERM _) = raise Fail "nuprlType2Ity:SET_TERM"
  | nuprlType2Ity force m sub bound (N.LES_TERM _) = raise Fail "nuprlType2Ity:LES_TERM"
  | nuprlType2Ity force m sub bound (N.IEQ_TERM _) = raise Fail "nuprlType2Ity:IEQ_TERM"
  | nuprlType2Ity force m sub bound (N.SPR_TERM _) = raise Fail "nuprlType2Ity:SPR_TERM"
  | nuprlType2Ity force m sub bound (N.AEQ_TERM _) = raise Fail "nuprlType2Ity:AEQ_TERM"
  | nuprlType2Ity force m sub bound (N.DEC_TERM _) = raise Fail "nuprlType2Ity:DEC_TERM"
  | nuprlType2Ity force m sub bound (N.IND_TERM _) = raise Fail "nuprlType2Ity:IND_TERM"
  | nuprlType2Ity force m sub bound (N.CLO_TERM _) = raise Fail "nuprlType2Ity:CLO_TERM"

fun isSOApply [] term = SOME (term, [])
  | isSOApply vars (N.TERM ((("so_apply", tag), []), (N.B_TERM ([], term)) :: args)) =
    (let val lst = ListPair.zipEq (vars, args)
     in if List.all (fn (v, N.B_TERM ([], rt)) =>
			let val t = N.rterm2term rt
			in N.is_nuprl_variable_term t
			   andalso
			   N.dest_variable t = v
			end
		      | _ => false)
		    lst
	then SOME (N.rterm2term term, vars)
	else NONE
     end handle ListPair.UnequalLengths => NONE)
  | isSOApply _ _ = NONE

fun nuprlType2ItyTerm force m id boundTypes boundIds (N.TERM (((opid, tag), params), bterms)) =
    if id = opid
    then foldr (fn (N.B_TERM (vars, term), SOME args) =>
		   (case isSOApply (map N.dest_nuprl_var vars) (N.rterm2term term) of
			SOME (term, vars) => SOME ((term, vars) :: args)
		      | NONE => NONE)
		 | _ => NONE)
	       (SOME [])
	       bterms
    else NONE
  | nuprlType2ItyTerm force m id boundTypes boundIds N.AXM_TERM     = raise Fail "nuprlType2ItyTerm:AXM_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds N.BOT_TERM     = raise Fail "nuprlType2ItyTerm:BOT_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds N.INT_TERM     = raise Fail "nuprlType2ItyTerm:INT_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds N.VOI_TERM     = raise Fail "nuprlType2ItyTerm:VOI_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds N.DUM_TERM     = raise Fail "nuprlType2ItyTerm:DUM_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.ATM_TERM _) = raise Fail "nuprlType2ItyTerm:ATM_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.TOK_TERM _) = raise Fail "nuprlType2ItyTerm:TOk_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.NAT_TERM _) = raise Fail "nuprlType2ItyTerm:NAT_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.VAR_TERM _) = raise Fail "nuprlType2ItyTerm:VAR_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.INL_TERM _) = raise Fail "nuprlType2ItyTerm:INL_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.INR_TERM _) = raise Fail "nuprlType2ItyTerm:INR_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.FIX_TERM _) = raise Fail "nuprlType2ItyTerm:FIX_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.MIN_TERM _) = raise Fail "nuprlType2ItyTerm:MIN_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.LAM_TERM _) = raise Fail "nuprlType2ItyTerm:LAM_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.REC_TERM _) = raise Fail "nuprlType2ItyTerm:REC_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.WAI_TERM _) = raise Fail "nuprlType2ItyTerm:WAI_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.APP_TERM _) = raise Fail "nuprlType2ItyTerm:APP_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.PAI_TERM _) = raise Fail "nuprlType2ItyTerm:PAI_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.ADD_TERM _) = raise Fail "nuprlType2ItyTerm:ADD_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.SUB_TERM _) = raise Fail "nuprlType2ItyTerm:SUB_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.MUL_TERM _) = raise Fail "nuprlType2ItyTerm:MUL_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.DIV_TERM _) = raise Fail "nuprlType2ItyTerm:DIV_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.REM_TERM _) = raise Fail "nuprlType2ItyTerm:REM_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.EQT_TERM _) = raise Fail "nuprlType2ItyTerm:EQT_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.UNI_TERM _) = raise Fail "nuprlType2ItyTerm:UNI_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.EQU_TERM _) = raise Fail "nuprlType2ItyTerm:EQU_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.IAX_TERM _) = raise Fail "nuprlType2ItyTerm:IAX_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.IPA_TERM _) = raise Fail "nuprlType2ItyTerm:IPA_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.IIR_TERM _) = raise Fail "nuprlType2ItyTerm:IIR_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.IIL_TERM _) = raise Fail "nuprlType2ItyTerm:IIL_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.IIN_TERM _) = raise Fail "nuprlType2ItyTerm:IIN_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.ILA_TERM _) = raise Fail "nuprlType2ItyTerm:ILA_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.IAT_TERM _) = raise Fail "nuprlType2ItyTerm:IAT_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.CBV_TERM _) = raise Fail "nuprlType2ItyTerm:CBV_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.CBA_TERM _) = raise Fail "nuprlType2ItyTerm:CBA_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.FUN_TERM _) = raise Fail "nuprlType2ItyTerm:FUN_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.PRD_TERM _) = raise Fail "nuprlType2ItyTerm:PRD_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.TUN_TERM _) = raise Fail "nuprlType2ItyTerm:TUN_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.SET_TERM _) = raise Fail "nuprlType2ItyTerm:SET_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.LES_TERM _) = raise Fail "nuprlType2ItyTerm:LES_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.IEQ_TERM _) = raise Fail "nuprlType2ItyTerm:IEQ_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.SPR_TERM _) = raise Fail "nuprlType2ItyTerm:SPR_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.AEQ_TERM _) = raise Fail "nuprlType2ItyTerm:AEQ_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.DEC_TERM _) = raise Fail "nuprlType2ItyTerm:DEC_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.IND_TERM _) = raise Fail "nuprlType2ItyTerm:IND_TERM"
  | nuprlType2ItyTerm force m id boundTypes boundIds (N.CLO_TERM _) = raise Fail "nuprlType2ItyTerm:CLO_TERM"

fun nuprlType2ItyArg force m boundTypes boundIds (N.TERM ((("variable", tag), [(v, "v")]), [])) =
    (case List.find (fn (x, ity) => (x = v)) boundIds of
	 SOME pair => SOME pair
       | NONE => (* (* NONE *) NEW: *)
	 (case List.find (fn x => x = v) boundTypes of
	      SOME _ => SOME (v, E.mk_type_type D.dummy_label)
	    | NONE => NONE))
  | nuprlType2ItyArg force m boundTypes boundIds (N.TERM ((("pair", tag), []), [N.B_TERM ([], term1), N.B_TERM ([], term2)])) =
    let val op1 = nuprlType2ItyArg force m boundTypes boundIds (N.rterm2term term1)
	val op2 = nuprlType2ItyArg force m boundTypes boundIds (N.rterm2term term2)
    in case (op1, op2) of
	   (SOME (v1, typ1), SOME (v2, typ2)) =>
	   SOME (v1 ^ v2, E.mk_type_tuple [typ1, typ2] D.dummy_label)
	 | _ => NONE
    end
  | nuprlType2ItyArg force m boundTypes boundIds (N.TERM (((opid, tag), params), bterms)) = NONE
  | nuprlType2ItyArg force m boundTypes boundIds N.AXM_TERM     = raise Fail "nuprlType2ItyArg:AXM_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds N.BOT_TERM     = raise Fail "nuprlType2ItyArg:BOT_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds N.INT_TERM     = raise Fail "nuprlType2ItyArg:INT_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds N.VOI_TERM     = raise Fail "nuprlType2ItyArg:VOI_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds N.DUM_TERM     = raise Fail "nuprlType2ItyArg:DUM_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.ATM_TERM _) = raise Fail "nuprlType2ItyArg:ATM_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.TOK_TERM _) = raise Fail "nuprlType2ItyArg:TOK_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.NAT_TERM _) = raise Fail "nuprlType2ItyArg:NAT_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.VAR_TERM _) = raise Fail "nuprlType2ItyArg:VAR_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.INL_TERM _) = raise Fail "nuprlType2ItyArg:INL_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.INR_TERM _) = raise Fail "nuprlType2ItyArg:INR_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.FIX_TERM _) = raise Fail "nuprlType2ItyArg:FIX_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.MIN_TERM _) = raise Fail "nuprlType2ItyArg:MIN_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.LAM_TERM _) = raise Fail "nuprlType2ItyArg:LAM_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.REC_TERM _) = raise Fail "nuprlType2ItyArg:REC_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.WAI_TERM _) = raise Fail "nuprlType2ItyArg:WAI_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.APP_TERM _) = raise Fail "nuprlType2ItyArg:APP_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.PAI_TERM _) = raise Fail "nuprlType2ItyArg:PAI_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.ADD_TERM _) = raise Fail "nuprlType2ItyArg:ADD_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.SUB_TERM _) = raise Fail "nuprlType2ItyArg:SUB_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.MUL_TERM _) = raise Fail "nuprlType2ItyArg:MUL_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.DIV_TERM _) = raise Fail "nuprlType2ItyArg:DIV_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.REM_TERM _) = raise Fail "nuprlType2ItyArg:REM_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.EQT_TERM _) = raise Fail "nuprlType2ItyArg:EQT_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.UNI_TERM _) = raise Fail "nuprlType2ItyArg:UNI_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.EQU_TERM _) = raise Fail "nuprlType2ItyArg:EQU_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.IAX_TERM _) = raise Fail "nuprlType2ItyArg:IAX_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.IPA_TERM _) = raise Fail "nuprlType2ItyArg:IPA_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.IIR_TERM _) = raise Fail "nuprlType2ItyArg:IIR_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.IIL_TERM _) = raise Fail "nuprlType2ItyArg:IIL_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.IIN_TERM _) = raise Fail "nuprlType2ItyArg:IIN_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.ILA_TERM _) = raise Fail "nuprlType2ItyArg:ILA_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.IAT_TERM _) = raise Fail "nuprlType2ItyArg:IAT_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.CBV_TERM _) = raise Fail "nuprlType2ItyArg:CBV_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.CBA_TERM _) = raise Fail "nuprlType2ItyArg:CBA_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.FUN_TERM _) = raise Fail "nuprlType2ItyArg:FUN_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.PRD_TERM _) = raise Fail "nuprlType2ItyArg:PRD_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.TUN_TERM _) = raise Fail "nuprlType2ItyArg:TUN_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.SET_TERM _) = raise Fail "nuprlType2ItyArg:SET_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.LES_TERM _) = raise Fail "nuprlType2ItyArg:LES_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.IEQ_TERM _) = raise Fail "nuprlType2ItyArg:IEQ_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.SPR_TERM _) = raise Fail "nuprlType2ItyArg:SPR_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.AEQ_TERM _) = raise Fail "nuprlType2ItyArg:AEQ_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.DEC_TERM _) = raise Fail "nuprlType2ItyArg:DEC_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.IND_TERM _) = raise Fail "nuprlType2ItyArg:IND_TERM"
  | nuprlType2ItyArg force m boundTypes boundIds (N.CLO_TERM _) = raise Fail "nuprlType2ItyArg:CLO_TERM"

fun soArgs [] (v, ity) args =
    if E.isItyType ity
    then if List.null args
	 then SOME (v, args, ity)
	 else NONE
    else SOME (v, args, ity)
  | soArgs (var :: vars) (v, ity) args =
    case E.destArrowType ity of
	SOME (t1, t2) => soArgs vars (v, t2) (args @ [(var, t1)])
      | NONE => NONE

fun nuprlType2ItyMember force m id boundTypes boundIds (N.TERM ((("member", tag), params), [N.B_TERM ([], typ), N.B_TERM ([], term)])) =
    let val (h, args) = dest_nuprl_applies_term (N.rterm2term term)
    in case nuprlType2Ity force m POS boundTypes (N.rterm2term typ) of
	   SOME ity =>
	   (case nuprlType2ItyTerm force m id boundTypes boundIds h of
		SOME args' =>
		let val tailop =
			foldr (fn (term, SOME ity) =>
				  (case nuprlType2ItyArg force m boundTypes boundIds term of
				       SOME (_, ity') => SOME (E.mk_type_arrow (ity', ity) D.dummy_label)
				     | NONE => NONE)
				| _ => NONE)
			      (SOME ity)
			      args
		in case tailop of
		       SOME itytail =>
		       (foldr (fn ((term, vars), SOME (xs, itytail)) =>
				  (case nuprlType2ItyArg force m boundTypes boundIds term of
				       SOME pair =>
				       (case soArgs vars pair [] of
					    SOME x => SOME (x :: xs, itytail)
					  | NONE => NONE)
				     | NONE => NONE)
				| _ => NONE)
			      (SOME ([], itytail))
			      args')
		     | NONE => NONE
		end
	      | NONE => NONE)
	 | NONE => NONE
    end
  | nuprlType2ItyMember force m id boundTypes boundIds (N.TERM ((("uimplies", tag), []), [N.B_TERM ([], A), N.B_TERM ([], B)])) =
    (* NOTE: Arg, that's not the best thing to do but we want to get
     * a shot at generating a type. *)
    nuprlType2ItyMember force m id boundTypes boundIds (N.rterm2term B)
  | nuprlType2ItyMember force m _ _ _ (N.TERM (((opid, tag), params), bterms)) = NONE
  | nuprlType2ItyMember force m _ _ _ N.AXM_TERM     = raise Fail "nuprlType2ItyMember:AXM_TERM"
  | nuprlType2ItyMember force m _ _ _ N.BOT_TERM     = raise Fail "nuprlType2ItyMember:BOT_TERM"
  | nuprlType2ItyMember force m _ _ _ N.INT_TERM     = raise Fail "nuprlType2ItyMember:INT_TERM"
  | nuprlType2ItyMember force m _ _ _ N.VOI_TERM     = raise Fail "nuprlType2ItyMember:VOI_TERM"
  | nuprlType2ItyMember force m _ _ _ N.DUM_TERM     = raise Fail "nuprlType2ItyMember:DUM_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.ATM_TERM _) = raise Fail "nuprlType2ItyMember:ATM_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.TOK_TERM _) = raise Fail "nuprlType2ItyMember:TOK_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.NAT_TERM _) = raise Fail "nuprlType2ItyMember:NAT_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.VAR_TERM _) = raise Fail "nuprlType2ItyMember:VAR_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.INL_TERM _) = raise Fail "nuprlType2ItyMember:INL_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.INR_TERM _) = raise Fail "nuprlType2ItyMember:INR_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.FIX_TERM _) = raise Fail "nuprlType2ItyMember:FIX_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.MIN_TERM _) = raise Fail "nuprlType2ItyMember:MIN_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.LAM_TERM _) = raise Fail "nuprlType2ItyMember:LAM_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.REC_TERM _) = raise Fail "nuprlType2ItyMember:REC_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.WAI_TERM _) = raise Fail "nuprlType2ItyMember:WAI_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.APP_TERM _) = raise Fail "nuprlType2ItyMember:APP_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.PAI_TERM _) = raise Fail "nuprlType2ItyMember:PAI_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.ADD_TERM _) = raise Fail "nuprlType2ItyMember:ADD_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.SUB_TERM _) = raise Fail "nuprlType2ItyMember:SUB_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.MUL_TERM _) = raise Fail "nuprlType2ItyMember:MUL_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.DIV_TERM _) = raise Fail "nuprlType2ItyMember:DIV_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.REM_TERM _) = raise Fail "nuprlType2ItyMember:REM_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.EQT_TERM _) = raise Fail "nuprlType2ItyMember:EQT_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.UNI_TERM _) = raise Fail "nuprlType2ItyMember:UNI_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.EQU_TERM _) = raise Fail "nuprlType2ItyMember:EQU_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.IAX_TERM _) = raise Fail "nuprlType2ItyMember:IAX_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.IPA_TERM _) = raise Fail "nuprlType2ItyMember:IPA_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.IIR_TERM _) = raise Fail "nuprlType2ItyMember:IIR_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.IIL_TERM _) = raise Fail "nuprlType2ItyMember:IIL_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.IIN_TERM _) = raise Fail "nuprlType2ItyMember:IIN_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.ILA_TERM _) = raise Fail "nuprlType2ItyMember:ILA_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.IAT_TERM _) = raise Fail "nuprlType2ItyMember:IAT_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.CBV_TERM _) = raise Fail "nuprlType2ItyMember:CBV_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.CBA_TERM _) = raise Fail "nuprlType2ItyMember:CBA_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.FUN_TERM _) = raise Fail "nuprlType2ItyMember:FUN_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.PRD_TERM _) = raise Fail "nuprlType2ItyMember:PRD_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.TUN_TERM _) = raise Fail "nuprlType2ItyMember:TUN_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.SET_TERM _) = raise Fail "nuprlType2ItyMember:SET_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.LES_TERM _) = raise Fail "nuprlType2ItyMember:LES_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.IEQ_TERM _) = raise Fail "nuprlType2ItyMember:IEQ_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.SPR_TERM _) = raise Fail "nuprlType2ItyMember:SPR_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.AEQ_TERM _) = raise Fail "nuprlType2ItyMember:AEQ_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.DEC_TERM _) = raise Fail "nuprlType2ItyMember:DEC_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.IND_TERM _) = raise Fail "nuprlType2ItyMember:IND_TERM"
  | nuprlType2ItyMember force m _ _ _ (N.CLO_TERM _) = raise Fail "nuprlType2ItyMember:CLO_TERM"

fun nuprlType2ItyAll force m id boundTypes boundIds (N.TERM ((("all", tag), params), [N.B_TERM ([], T), N.B_TERM ([x], P)])) =
    if is_nuprl_type_term (N.rterm2term T)
    then let val boundTypes' = N.dest_nuprl_var x :: boundTypes
	 in nuprlType2ItyAll force m id boundTypes' boundIds (N.rterm2term P)
	 end
    else (case nuprlType2Ity force m NEG boundTypes (N.rterm2term T) of
	      SOME ity =>
	      let val boundIds' = (N.dest_nuprl_var x, ity) :: boundIds
	      in nuprlType2ItyAll force m id boundTypes boundIds' (N.rterm2term P)
	      end
	    | NONE => NONE)
  | nuprlType2ItyAll force m id boundTypes boundIds (N.TERM ((("uall", tag), params), [N.B_TERM ([], T), N.B_TERM ([x], P)])) =
    if is_nuprl_type_term (N.rterm2term T)
    then let val boundTypes' = N.dest_nuprl_var x :: boundTypes
	 in nuprlType2ItyAll force m id boundTypes' boundIds (N.rterm2term P)
	 end
    else (case nuprlType2Ity force m NEG boundTypes (N.rterm2term T) of
	      SOME ity =>
	      let val boundIds' = (N.dest_nuprl_var x, ity) :: boundIds
	      in nuprlType2ItyAll force m id boundTypes boundIds' (N.rterm2term P)
	      end
	    | NONE => NONE)
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.TERM (((opid, tag), params), bterms)) =
    let val Tlen = length boundTypes
	val Ilen = length boundIds
	val set1 = SET.fromList boundTypes
	val set2 = List.foldr (fn ((x, _), set) => SET.add (set, x)) set1 boundIds
    in if Tlen + Ilen = SET.numItems set2
       then case nuprlType2ItyMember force m id boundTypes boundIds term of
		SOME (args, ity) => SOME (boundTypes, args, ity)
	      | NONE => NONE
       else NONE
    end
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.AXM_TERM)   = raise Fail "nuprlType2ItyAll:AXM_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.BOT_TERM)   = raise Fail "nuprlType2ItyAll:BOT_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.INT_TERM)   = raise Fail "nuprlType2ItyAll:INT_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.VOI_TERM)   = raise Fail "nuprlType2ItyAll:VOI_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.DUM_TERM)   = raise Fail "nuprlType2ItyAll:DUM_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.ATM_TERM _) = raise Fail "nuprlType2ItyAll:ATM_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.TOK_TERM _) = raise Fail "nuprlType2ItyAll:TOK_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.NAT_TERM _) = raise Fail "nuprlType2ItyAll:NAT_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.VAR_TERM _) = raise Fail "nuprlType2ItyAll:VAR_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.INL_TERM _) = raise Fail "nuprlType2ItyAll:INL_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.INR_TERM _) = raise Fail "nuprlType2ItyAll:INR_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.FIX_TERM _) = raise Fail "nuprlType2ItyAll:FIX_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.MIN_TERM _) = raise Fail "nuprlType2ItyAll:MIN_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.LAM_TERM _) = raise Fail "nuprlType2ItyAll:LAM_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.REC_TERM _) = raise Fail "nuprlType2ItyAll:REC_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.WAI_TERM _) = raise Fail "nuprlType2ItyAll:WAI_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.APP_TERM _) = raise Fail "nuprlType2ItyAll:APP_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.PAI_TERM _) = raise Fail "nuprlType2ItyAll:PAI_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.ADD_TERM _) = raise Fail "nuprlType2ItyAll:ADD_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.SUB_TERM _) = raise Fail "nuprlType2ItyAll:SUB_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.MUL_TERM _) = raise Fail "nuprlType2ItyAll:MUL_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.DIV_TERM _) = raise Fail "nuprlType2ItyAll:DIV_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.REM_TERM _) = raise Fail "nuprlType2ItyAll:REM_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.EQT_TERM _) = raise Fail "nuprlType2ItyAll:EQT_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.UNI_TERM _) = raise Fail "nuprlType2ItyAll:UNI_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.EQU_TERM _) = raise Fail "nuprlType2ItyAll:EQU_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.IAX_TERM _) = raise Fail "nuprlType2ItyAll:IAX_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.IPA_TERM _) = raise Fail "nuprlType2ItyAll:IPA_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.IIR_TERM _) = raise Fail "nuprlType2ItyAll:IIR_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.IIL_TERM _) = raise Fail "nuprlType2ItyAll:IIL_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.IIN_TERM _) = raise Fail "nuprlType2ItyAll:IIN_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.ILA_TERM _) = raise Fail "nuprlType2ItyAll:ILA_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.IAT_TERM _) = raise Fail "nuprlType2ItyAll:IAT_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.CBV_TERM _) = raise Fail "nuprlType2ItyAll:CBV_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.CBA_TERM _) = raise Fail "nuprlType2ItyAll:CBA_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.FUN_TERM _) = raise Fail "nuprlType2ItyAll:FUN_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.PRD_TERM _) = raise Fail "nuprlType2ItyAll:PRD_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.TUN_TERM _) = raise Fail "nuprlType2ItyAll:TUN_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.SET_TERM _) = raise Fail "nuprlType2ItyAll:SET_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.LES_TERM _) = raise Fail "nuprlType2ItyAll:LES_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.IEQ_TERM _) = raise Fail "nuprlType2ItyAll:IEQ_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.SPR_TERM _) = raise Fail "nuprlType2ItyAll:SPR_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.AEQ_TERM _) = raise Fail "nuprlType2ItyAll:AEQ_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.DEC_TERM _) = raise Fail "nuprlType2ItyAll:DEC_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.IND_TERM _) = raise Fail "nuprlType2ItyAll:IND_TERM"
  | nuprlType2ItyAll force m id boundTypes boundIds (term as N.CLO_TERM _) = raise Fail "nuprlType2ItyAll:CLO_TERM"

fun nuprlType2ItyAllWf force m id boundTypes boundIds (N.TERM (((opid, tag), []), [N.B_TERM ([], wf)])) =
    nuprlType2ItyAll force m id boundTypes boundIds (N.rterm2term wf)
  | nuprlType2ItyAllWf _ _ _ _ _ _ = raise Fail "wrong_format:nuprlType2ItyAllWf"

fun term2map force m (N.TERM (((id, tag), [(obid, "o")]),
			      [N.B_TERM ([], lhs),
			       N.B_TERM ([], rhs),
			       N.B_TERM ([], wfs)])) =
    if N.get_tag tag = "t"
    then let val tlhs = N.rterm2term lhs
	     val trhs = N.rterm2term rhs
	     val twfs = N.rterm2term wfs
	     val subs  = if N.is_nuprl_iwf_lemmas_term twfs
			 then N.subterms twfs
			 else raise Fail "term2map:wf_lemmas"
	     val wfs'  = map (fn rwf =>
				 let val wf = N.rterm2term rwf
				 in case nuprlType2ItyAllWf force m id [] [] wf of
					NONE => (wf, NONE)
				      | SOME (x as (ebound,args,ty)) => (wf, SOME x)
				 end)
			     subs
	 in SOME (id, obid, tlhs, trhs, wfs')
	 end
    else raise Fail "term2map:abstraction:wrong_tag"
  | term2map force m (N.TERM (((id, tag), [(obid, "o")]),
			      [N.B_TERM ([], termof),
			       N.B_TERM ([], extract)])) =
    if N.get_tag tag = "t"
    then NONE
    else raise Fail "term2map:termof:wrong_tag"
  (* We don't collect the extracts because this map is meant to collect
   * the wf lemmas to generate EventML constant declarations. *)
  | term2map force m term =
    (print (N.toStringTerm term);
     raise Fail "term2map:unrecognized_term_format")

(*fun term2map force m term = term2map' force m (N.term2TERM term)*)

fun b_terms2map force m ind n [] = (m, ind, n )
  | b_terms2map force m ind n ((N.B_TERM ([], term)) :: b_terms) =
    (case term2map force m (N.rterm2term term) of
	 SOME (id, obid, lhs, rhs, wfs) =>
	 let val (lst', ind', n') =
		 case MAP.find (m, id) of
		     SOME lst => ((obid, lhs, rhs, wfs) :: lst, ind, n)
		   | NONE => ([(obid, lhs, rhs, wfs)], IND.insert (ind, n, id), n + 1)
	     val m' = MAP.insert (m, id, lst')
	 in b_terms2map force m' ind' n' b_terms
	 end
       | NONE => b_terms2map force m ind n b_terms)
  | b_terms2map _ _ _ _ _ = raise Fail "b_terms2map:wrong_format"

fun terms2map force m ind n [] = (m, ind, n)
  | terms2map force m ind n ((N.TERM (((opid, tag), []), b_terms)) :: terms) =
    if N.get_tag tag = "t"
    then let val (map1, ind1, n1) = b_terms2map force m    ind  n  b_terms
	     val (map2, ind2, n2) = terms2map   force map1 ind1 n1 terms
	 in (map2, ind2, n2)
	 end
    else raise Fail "terms2map:wrong_tag"
  | terms2map _ _ _ _ _ = raise Fail "terms2map:wrong_format"

type timer = {real : Timer.real_timer,
	      cpu  : Timer.cpu_timer}

fun startTimer () = {real = Timer.startRealTimer (),
		     cpu  = Timer.startCPUTimer ()}

fun getTime (timer : timer) = Timer.checkRealTimer (#real timer)

fun getMilliTime timer = Time.toMilliseconds (getTime timer)
fun getSecTime   timer = Time.toSeconds      (getTime timer)

fun cond_print true str = print str
  | cond_print false _ = ()

fun parseTerms input prt light split = P.parse prt light [] input split

fun exportTerms input outputop prt light split force =
    let val terms  = P.parse prt light [] input split
	val _      = cond_print prt ("[start mapping]\n")
	val timer  = startTimer ()
	val (map,ind,n) = terms2map force MAP.empty IND.empty 0 terms
	val _      = if prt
		     then app (fn t => cond_print prt (N.toStringTerm t ^ "\n")) terms
		     else ()
	val time   = getSecTime timer
	val _      = cond_print prt ("[end mapping: " ^ LargeInt.toString time ^ "s]\n")
	val _      =
	    case outputop of
		NONE => ()
	      | SOME output => exportMap2Lisp map ind n output
	val _      = cond_print prt ("[export done]\n")
    in ()
    end

end
