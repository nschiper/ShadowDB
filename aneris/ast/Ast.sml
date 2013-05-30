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
 *  o File name:   Ast.sml
 *  o Description: Structure to handle EventML ASTs.
 *)


structure Ast :> AST = struct

structure D  = Deps
structure R  = Reg
structure T  = ListFormat

datatype class =
	 SCON
       | ID
       | IDENTS
       | TYPEVAR
       | ATPAT
       | PAT
       | TYPE
       | MRULE
       | MATCH
       | EXP
       | ATEXP
       | PROP
       | CASEPAT
       | ATOMS
       | DEC
       | INCPARM
       | INCPARMS
       | DATA
       | DOC
       | TYPEVARSEQ
       | TYPESEQ
       | OTYPESEQ
       | TYPESEQSET
       | BIND
       | BINDS
       | PARAM
       | ARG
       | ARGS
       | PROG
       | DOTS
       | FILE

datatype prod =
	 (* SCON *)
	 SCON_INT
       | SCON_REAL
       | SCON_ATOM
       | SCON_ATOMS
       | SCON_STRING
       (* ID *)
       | ID_VID
       | ID_TYCON
       | ID_TYVAR
       (* IDENTS *)
       | IDENTS_LIST
       (* TYPEVAR *)
       | TYPEVAR_VAR
       (* TYPE *)
       | TYPE_ARROW
       | TYPE_DARROW
       | TYPE_DISJU
       | TYPE_TUPLE
       | TYPE_TYCON
       | TYPE_VAR
       | TYPE_PAREN
       | TYPE_DEP
       | TYPE_SET
       | TYPE_SPSET
       (* ATPAT *)
       | ATPAT_ID
       | ATPAT_WILD
       | ATPAT_SCON
       | ATPAT_LIST
       | ATPAT_PAREN
       | ATPAT_STRUC
       | ATPAT_TUPLE
       (* PAT *)
       | PAT_TYPED
       | PAT_AS
       | PAT_CONS
       | PAT_ATPAT
       | PAT_APP
       (* MRULE *)
       | MRULE_M
       (* MATCH *)
       | MATCH_M
       (* EXP *)
       | EXP_OR
       | EXP_AND
       | EXP_TYPED
       | EXP_LAMBDA
       | EXP_ITE
       | EXP_ATEXP
       | EXP_APP
       | EXP_OP
       | EXP_LET
       | EXP_LETR
       | EXP_CLASS
       | EXP_WHERE
       | EXP_BINDING
       | EXP_MBIND
       | EXP_COMP
       | EXP_CASE
       | EXP_QUOT
       (* ATEXP *)
       | ATEXP_ID
       | ATEXP_SCON
       | ATEXP_TUPLE
       | ATEXP_PAREN
       | ATEXP_LIST
       | ATEXP_BAG
       | ATEXP_PRIOR
       | ATEXP_ANY
       | ATEXP_MSG
       | ATEXP_ONCE
       | ATEXP_SENDOC
       | ATEXP_ONLOC
       | ATEXP_SKIP
       | ATEXP_WAIT
       | ATEXP_NULL
       | ATEXP_MINUS
       | ATEXP_TYPE
       | ATEXP_STATEC
       | ATEXP_MEMORYC
       (* PROP *)
       | PROP_EXP
       | PROP_OR
       | PROP_AND
       | PROP_IMP
       | PROP_IFF
       | PROP_ALL
       | PROP_EX
       | PROP_OBS
       | PROP_PAREN
       (* CASEPAT *)
       | CASEPAT_PAT
       (* ATOMS *)
       | ATOMS_ATOMS
       | ATOMS_LIST
       | ATOMS_WILD
       (* DEC *)
       | DEC_LET
       | DEC_LETR
       | DEC_CLASS
       | DEC_CLASSREC
       | DEC_CONS
       | DEC_OCONS
       | DEC_PSET
       | DEC_PMAP
       | DEC_PARAM
       | DEC_PARAMP
       | DEC_TYPARAM
       | DEC_TYPARAMP
       | DEC_ETPARAM
       | DEC_TYCON
       | DEC_EQTYCON
       | DEC_TYFUN
       | DEC_QMSG
       | DEC_EQMSG
       | DEC_PARSE
       | DEC_TYPEOF
       | DEC_INFIX
       | DEC_MAIN
       | DEC_SPEC
       | DEC_IMPORT
       | DEC_TIMPORT
       | DEC_EXPORT
       | DEC_INCLUDE
       | DEC_ASSUME
       | DEC_GUARANT
       | DEC_DOC
       | DEC_INV
       | DEC_ORDER
       | DEC_PROGRESS
       | DEC_STRICT
       | DEC_CONSIST
       | DEC_MEMORY
       | DEC_DATA
       | DEC_ABSTYPE
       | DEC_VAR
       | DEC_OPTIONS
       (* INCPARM *)
       | INCPARM_EXP
       | INCPARM_TYP
       | INCPARM_INT
       (* INCPARMS *)
       | INCPARMS_P
       (* DATA *)
       | DATA_CONS
       (* DOC *)
       | DOC_LINE
       (* TYPEVARSEQ *)
       | TYPEVARSEQ_ONE
       | TYPEVARSEQ_SEQ
       | TYPEVARSEQ_EM
       (* TYPESEQ *)
       | TYPESEQ_ONE
       | TYPESEQ_SEQ
       | TYPESEQ_EM
       (* OTYPESEQ *)
       | OTYPESEQ_UNM
       | OTYPESEQ_NAM
       (* TYPESEQSET *)
       | TYPESEQSET_SET
       (* BIND *)
       | BIND_DEC
       | BIND_TDEC
       | BIND_PAT
       | BIND_IOP
       | BIND_TIOP
       (* BINDS *)
       | BINDS_LIST
       (* PARAM *)
       | PARAM_P
       (* ARG *)
       | ARG_A
       | ARG_T
       (* ARGS *)
       | ARGS_EM
       | ARGS_PSEQ
       | ARGS_LSEQ
       (* PROG *)
       | PROG_P
       (* DOTS *)
       | DOTS_D
       (* FILE *)
       | FILE_F

type kind  = class * prod

type value = string

structure SET = BinarySetFn(type ord_key = D.label val compare = D.compareLabels)

datatype term = N of {kind     : kind,
		      label    : D.label,
		      value    : value,
		      regions  : R.region list,
		      parents  : R.region list,
		      children : term list}

val unexpectedFormat  = "term has an unexpected format"
val wrongFormat       =  "wrong term format"


(* ------ CONSTANTS ------ *)

val id_pair_fst     = "fst"
val id_pair_snd     = "snd"
val id_list_cons    = "."
val id_list_nil     = "nil"
val id_list_concat  = "++"
val id_bool_true    = "true"
val id_bool_false   = "false"
val id_bool_not     = "!"
val id_prop_not     = "not"
val id_int_plus     = "+"
val id_int_minus    = "-"
val id_int_mult     = "*"
val id_int_div      = "/"
val id_int_leq      = "<="
val id_int_geq      = ">="
val id_int_lt       = "<"
val id_int_gt       = ">"
val id_eq           = "="
val id_eqeq         = "=="
val id_diff         = "<>"
val id_new_prefix   = "_new"
val id_disju_inl    = "inl"
val id_disju_inr    = "inr"
val id_disju_isl    = "isl"
val id_deq_deq      = "op"   (* from eq ('a -> 'a -> Bool) to deq (Deq 'a) *)
val id_deq_eqof     = "eqof" (* from deq (Deq 'a) to eq ('a -> 'a -> Bool) *)
val id_class_par    = "||"
val id_class_bind   = ">>="
val id_class_at     = "@"
val id_class_until  = "until"
val id_class_opt    = "?"
val id_class_opt_c  = "??"
val id_fun_fix      = "fix"
val id_es_loc       = "location"
val id_es_causl     = "before"
val id_es_locl      = "l-before"

val constructors = [id_list_nil,                  (* list           *)
		    id_pair_fst,  id_pair_snd,    (* product        *)
		    id_bool_true, id_bool_false,  (* bool           *)
		    id_disju_inl, id_disju_inr]   (* disjoint union *)


(* ------ SIMPLE ACCESSORS ------ *)

fun getKind     (N {kind, label, value, regions, parents, children}) = kind
fun getLabel    (N {kind, label, value, regions, parents, children}) = label
fun getValue    (N {kind, label, value, regions, parents, children}) = value
fun getRegions  (N {kind, label, value, regions, parents, children}) = regions
fun getParents  (N {kind, label, value, regions, parents, children}) = parents
fun getChildren (N {kind, label, value, regions, parents, children}) = children

fun getClass term = (fn (class, _) => class) (getKind term)
fun getProd  term = (fn (_, prod)  => prod)  (getKind term)

fun get1Children term =
    case getChildren term of
	[term1] => term1
      | _ => raise Fail "term is supposed to have 1 children"

fun get2Children term =
    case getChildren term of
	[term1, term2] => (term1, term2)
      | _ => raise Fail "term is supposed to have 2 children"

fun get3Children term =
    case getChildren term of
	[term1, term2, term3] => (term1, term2, term3)
      | _ => raise Fail "term is supposed to have 3 children"

fun get4Children term =
    case getChildren term of
	[term1, term2, term3, term4] => (term1, term2, term3, term4)
      | _ => raise Fail "term is supposed to have 4 children"

fun get5Children term =
    case getChildren term of
	[term1, term2, term3, term4, term5] => (term1, term2, term3, term4, term5)
      | _ => raise Fail "term is supposed to have 5 children"

fun get6Children term =
    case getChildren term of
	[term1, term2, term3, term4, term5, term6] => (term1, term2, term3, term4, term5, term6)
      | _ => raise Fail "term is supposed to have 6 children"

fun get7Children term =
    case getChildren term of
	[term1, term2, term3, term4, term5, term6, term7] => (term1, term2, term3, term4, term5, term6, term7)
      | _ => raise Fail "term is supposed to have 7 children"

fun get8Children term =
    case getChildren term of
	[term1, term2, term3, term4, term5, term6, term7, term8] => (term1, term2, term3, term4, term5, term6, term7, term8)
      | _ => raise Fail "term is supposed to have 8 children"

fun get9Children term =
    case getChildren term of
	[term1, term2, term3, term4, term5, term6, term7, term8, term9] => (term1, term2, term3, term4, term5, term6, term7, term8, term9)
      | _ => raise Fail "term is supposed to have 9 children"

fun get10Children term =
    case getChildren term of
	[term1, term2, term3, term4, term5, term6, term7, term8, term9, term10] => (term1, term2, term3, term4, term5, term6, term7, term8, term9, term10)
      | _ => raise Fail "term is supposed to have 10 children"

(* ------ UPDATERS ------ *)

fun updKind     (N {kind = _, label, value, regions, parents, children}) kind     = N {kind = kind, label = label, value = value, regions = regions, parents = parents, children = children}
fun updLabel    (N {kind, label = _, value, regions, parents, children}) label    = N {kind = kind, label = label, value = value, regions = regions, parents = parents, children = children}
fun updValue    (N {kind, label, value = _, regions, parents, children}) value    = N {kind = kind, label = label, value = value, regions = regions, parents = parents, children = children}
fun updRegions  (N {kind, label, value, regions = _, parents, children}) regions  = N {kind = kind, label = label, value = value, regions = regions, parents = parents, children = children}
fun updParents  (N {kind, label, value, regions, parents = _, children}) parents  = N {kind = kind, label = label, value = value, regions = regions, parents = parents, children = children}
fun updChildren (N {kind, label, value, regions, parents, children = _}) children = N {kind = kind, label = label, value = value, regions = regions, parents = parents, children = children}


(* ------ CHECKERS ------ *)

fun is_empty_term term =
    getProd term = PROG_P
    andalso
    List.null (getChildren term)

fun kindIsExp (EXP, _) = true
  | kindIsExp _ = false

fun kindIsAtExp (ATEXP, _) = true
  | kindIsAtExp _ = false

fun kindIsDec (DEC, _) = true
  | kindIsDec _ = false

fun kindIsDots (DOTS, _) = true
  | kindIsDots _ = false

fun kindIsFile (FILE, _) = true
  | kindIsFile _ = false

fun kindIsPatApp (PAT, PAT_APP) = true
  | kindIsPatApp _ = false

fun kindIsPatTyped (PAT, PAT_TYPED) = true
  | kindIsPatTyped _ = false

fun kindIsIdVid (ID, ID_VID) = true
  | kindIsIdVid _ = false

fun kindIsId (ID, _) = true
  | kindIsId _ = false

fun kindIsParamP (PARAM, PARAM_P) = true
  | kindIsParamP _ = false

(* Checks whether a term is an empty function parameter list. *)
fun termIsEmParam term =
    kindIsParamP (getKind term)
    andalso
    List.null (getChildren term)

fun isPatApp term = kindIsPatApp (getKind term)

fun isPatTyped term = kindIsPatTyped (getKind term)

fun isIdVid term = kindIsIdVid (getKind term)

fun isIdVidAtPat term =
    getKind term = (ATPAT, ATPAT_ID)
    andalso
    isIdVid (get1Children term)

fun isIdVidPat term =
    getKind term = (PAT, PAT_ATPAT)
    andalso
    isIdVidAtPat (get1Children term)

fun getIdVidAtPat term =
    if getKind term = (ATPAT, ATPAT_ID)
    then SOME (get1Children term)
    else NONE

fun getIdVidPat term =
    if getKind term = (PAT, PAT_ATPAT)
    then getIdVidAtPat (get1Children term)
    else NONE

fun isWildPat term =
    let val kind = getKind term
    in (kind = (ATPAT, ATPAT_PAREN)
	andalso
	isWildPat (get1Children term))
       orelse
       (kind = (PAT, PAT_ATPAT)
	andalso
	isWildPat (get1Children term))
       orelse
       kind = (ATPAT, ATPAT_WILD)
    end

fun isUnitPat term =
    let val kind = getKind term
    in (kind = (ATPAT, ATPAT_PAREN)
	andalso
	isUnitPat (get1Children term))
       orelse
       (kind = (PAT, PAT_ATPAT)
	andalso
	isUnitPat (get1Children term))
       orelse
       (kind = (ATPAT, ATPAT_TUPLE)
	andalso
	List.null (getChildren term))
    end

fun isWildMrule term =
    getKind term = (MRULE, MRULE_M)
    andalso
    let val (casepat, _) = get2Children term
    in getKind casepat = (CASEPAT, CASEPAT_PAT)
       andalso
       isWildPat (get1Children casepat)
    end

fun typeIsType term =
    getKind term = (TYPE, TYPE_TYCON)
    andalso
    let val (tn, ts) = get2Children term
    in getKind tn = (ID, ID_TYCON)
       andalso
       getValue tn = "Type"
       andalso
       getKind ts = (TYPESEQ, TYPESEQ_EM)
    end

fun isExpBag term =
    (getKind term = (EXP, EXP_ATEXP)
     andalso
     isExpBag (get1Children term))
    orelse
    getKind term = (ATEXP, ATEXP_BAG)
    orelse
    (getKind term = (ATEXP, ATEXP_PAREN)
     andalso
     isExpBag (get1Children term))


(* ------ CLASSES ------ *)

fun prod2class SCON_INT    = SCON
  | prod2class SCON_REAL   = SCON
  | prod2class SCON_ATOM   = SCON
  | prod2class SCON_ATOMS  = SCON
  | prod2class SCON_STRING = SCON
  (* ID *)
  | prod2class ID_VID    = ID
  | prod2class ID_TYCON  = ID
  | prod2class ID_TYVAR  = ID
  (* IDENTS *)
  | prod2class IDENTS_LIST = IDENTS
  (* TYPEVAR *)
  | prod2class TYPEVAR_VAR = TYPEVAR
  (* TYPE *)
  | prod2class TYPE_ARROW  = TYPE
  | prod2class TYPE_DARROW = TYPE
  | prod2class TYPE_DISJU  = TYPE
  | prod2class TYPE_TUPLE  = TYPE
  | prod2class TYPE_TYCON  = TYPE
  | prod2class TYPE_VAR    = TYPE
  | prod2class TYPE_PAREN  = TYPE
  | prod2class TYPE_DEP    = TYPE
  | prod2class TYPE_SET    = TYPE
  | prod2class TYPE_SPSET  = TYPE
  (*| prod2class TYPE_ID    = TYPE*)
  (* ATPAT *)
  | prod2class ATPAT_ID    = ATPAT
  | prod2class ATPAT_WILD  = ATPAT
  | prod2class ATPAT_SCON  = ATPAT
  | prod2class ATPAT_LIST  = ATPAT
  | prod2class ATPAT_PAREN = ATPAT
  | prod2class ATPAT_STRUC = ATPAT
  | prod2class ATPAT_TUPLE = ATPAT
  (* PAT *)
  | prod2class PAT_TYPED = PAT
  | prod2class PAT_AS    = PAT
  | prod2class PAT_CONS  = PAT
  | prod2class PAT_ATPAT = PAT
  | prod2class PAT_APP   = PAT
  (* MRULE *)
  | prod2class MRULE_M = MRULE
  (* MATCH *)
  | prod2class MATCH_M = MATCH
  (* EXP *)
  | prod2class EXP_OR       = EXP
  | prod2class EXP_AND      = EXP
  | prod2class EXP_TYPED    = EXP
  | prod2class EXP_LAMBDA   = EXP
  | prod2class EXP_ITE      = EXP
  | prod2class EXP_ATEXP    = EXP
  | prod2class EXP_APP      = EXP
  | prod2class EXP_OP       = EXP
  | prod2class EXP_LET      = EXP
  | prod2class EXP_LETR     = EXP
  | prod2class EXP_CLASS    = EXP
  | prod2class EXP_WHERE    = EXP
  | prod2class EXP_BINDING  = EXP
  | prod2class EXP_MBIND    = EXP
  | prod2class EXP_COMP     = EXP
  | prod2class EXP_CASE     = EXP
  | prod2class EXP_QUOT     = EXP
  (* ATEXP *)
  | prod2class ATEXP_ID      = ATEXP
  | prod2class ATEXP_SCON    = ATEXP
  | prod2class ATEXP_TUPLE   = ATEXP
  | prod2class ATEXP_PAREN   = ATEXP
  | prod2class ATEXP_LIST    = ATEXP
  | prod2class ATEXP_BAG     = ATEXP
  | prod2class ATEXP_PRIOR   = ATEXP
  | prod2class ATEXP_ANY     = ATEXP
  | prod2class ATEXP_MSG     = ATEXP
  | prod2class ATEXP_ONCE    = ATEXP
  | prod2class ATEXP_SENDOC  = ATEXP
  | prod2class ATEXP_ONLOC   = ATEXP
  | prod2class ATEXP_SKIP    = ATEXP
  | prod2class ATEXP_WAIT    = ATEXP
  | prod2class ATEXP_NULL    = ATEXP
  | prod2class ATEXP_MINUS   = ATEXP
  | prod2class ATEXP_TYPE    = ATEXP
  | prod2class ATEXP_STATEC  = ATEXP
  | prod2class ATEXP_MEMORYC = ATEXP
  (* CASEPAT *)
  | prod2class CASEPAT_PAT    = CASEPAT
  (* ATOMS *)
  | prod2class ATOMS_ATOMS = ATOMS
  | prod2class ATOMS_LIST  = ATOMS
  | prod2class ATOMS_WILD  = ATOMS
  (* DEC *)
  | prod2class DEC_LET      = DEC
  | prod2class DEC_LETR     = DEC
  | prod2class DEC_CLASS    = DEC
  | prod2class DEC_CLASSREC = DEC
  | prod2class DEC_CONS     = DEC
  | prod2class DEC_OCONS    = DEC
  | prod2class DEC_PSET     = DEC
  | prod2class DEC_PMAP     = DEC
  | prod2class DEC_PARAM    = DEC
  | prod2class DEC_PARAMP   = DEC
  | prod2class DEC_TYPARAM  = DEC
  | prod2class DEC_TYPARAMP = DEC
  | prod2class DEC_ETPARAM  = DEC
  | prod2class DEC_TYCON    = DEC
  | prod2class DEC_EQTYCON  = DEC
  | prod2class DEC_TYFUN    = DEC
  | prod2class DEC_QMSG     = DEC
  | prod2class DEC_EQMSG    = DEC
  | prod2class DEC_PARSE    = DEC
  | prod2class DEC_TYPEOF   = DEC
  | prod2class DEC_INFIX    = DEC
  | prod2class DEC_MAIN     = DEC
  | prod2class DEC_SPEC     = DEC
  | prod2class DEC_IMPORT   = DEC
  | prod2class DEC_TIMPORT  = DEC
  | prod2class DEC_EXPORT   = DEC
  | prod2class DEC_INCLUDE  = DEC
  | prod2class DEC_ASSUME   = DEC
  | prod2class DEC_GUARANT  = DEC
  | prod2class DEC_DOC      = DEC
  | prod2class DEC_INV      = DEC
  | prod2class DEC_ORDER    = DEC
  | prod2class DEC_PROGRESS = DEC
  | prod2class DEC_STRICT   = DEC
  | prod2class DEC_CONSIST  = DEC
  | prod2class DEC_MEMORY   = DEC
  | prod2class DEC_DATA     = DEC
  | prod2class DEC_ABSTYPE  = DEC
  | prod2class DEC_VAR      = DEC
  | prod2class DEC_OPTIONS  = DEC
  (* INCPARM *)
  | prod2class INCPARM_EXP = INCPARM
  | prod2class INCPARM_TYP = INCPARM
  | prod2class INCPARM_INT = INCPARM
  (* INCPARMS *)
  | prod2class INCPARMS_P = INCPARMS
  (* DATA *)
  | prod2class DATA_CONS = DATA
  (* PROP *)
  | prod2class PROP_EXP   = PROP
  | prod2class PROP_OR    = PROP
  | prod2class PROP_AND   = PROP
  | prod2class PROP_IMP   = PROP
  | prod2class PROP_IFF   = PROP
  | prod2class PROP_ALL   = PROP
  | prod2class PROP_EX    = PROP
  | prod2class PROP_OBS   = PROP
  | prod2class PROP_PAREN = PROP
  (* DOC *)
  | prod2class DOC_LINE = DOC
  (* TYPEVARSEQ *)
  | prod2class TYPEVARSEQ_ONE = TYPEVARSEQ
  | prod2class TYPEVARSEQ_SEQ = TYPEVARSEQ
  | prod2class TYPEVARSEQ_EM  = TYPEVARSEQ
  (* TYPESEQ *)
  | prod2class TYPESEQ_ONE = TYPESEQ
  | prod2class TYPESEQ_SEQ = TYPESEQ
  | prod2class TYPESEQ_EM  = TYPESEQ
  (* OTYPESEQ *)
  | prod2class OTYPESEQ_UNM = OTYPESEQ
  | prod2class OTYPESEQ_NAM = OTYPESEQ
  (* TYPESEQSET *)
  | prod2class TYPESEQSET_SET = TYPESEQSET
  (* BIND *)
  | prod2class BIND_DEC  = BIND
  | prod2class BIND_TDEC = BIND
  | prod2class BIND_PAT  = BIND
  | prod2class BIND_IOP  = BIND
  | prod2class BIND_TIOP = BIND
  (* BINDS *)
  | prod2class BINDS_LIST = BINDS
  (* PARAM *)
  | prod2class PARAM_P = PARAM
  (* ARG *)
  | prod2class ARG_A = ARG
  | prod2class ARG_T = ARG
  (* ARGS *)
  | prod2class ARGS_EM   = ARGS
  | prod2class ARGS_PSEQ = ARGS
  | prod2class ARGS_LSEQ = ARGS
  (* PROG *)
  | prod2class PROG_P = PROG
  (* DOTS *)
  | prod2class DOTS_D = DOTS
  (* FILE *)
  | prod2class FILE_F = FILE


(* ------ INTERNAL IDENTIFIERS ------ *)

val new_internal_id = ref 1

fun newInternalId () =
    let val x = !new_internal_id
	val _ = new_internal_id := x + 1
    in id_new_prefix ^ Int.toString x
    end

fun resetInternalId () = new_internal_id := 1

fun reset () = (resetInternalId (); ())

fun isInternalId id = String.isPrefix id_new_prefix id


(* ------ CONSTRUCTORS ------ *)

fun mk_term kind label value regs parents children =
    N {kind     = (prod2class kind, kind),
       label    = label,
       value    = value,
       regions  = regs,
       parents  = parents,
       children = children}

fun mk_new_pterm kind value regions parents children =
    mk_term kind (D.newLabel ()) value regions parents children

fun mk_new_term kind value regs children =
    mk_new_pterm kind value regs [] children

fun mk_new_internal_term kind regs children =
    mk_new_term kind (newInternalId ()) regs children

fun mk_new_dum_term kind value regs children =
    mk_term kind D.dummy_label value regs [] children

fun mk_new_dum_term' kind regs children =
    mk_term kind D.dummy_label "" regs [] children

val empty_term = mk_new_dum_term' PROG_P [] []


(* ------ OTHER ACCESSORS ------ *)

(*fun getRegAtomMrule (N {kind = (MRULE, MRULE_C), label, value, regions, parents, children = [atoms, _]}) = getRegions atoms
  | getRegAtomMrule _ = raise Fail "term is not a mrule"*)

fun getIdIdent (N {kind = (ID, ID_VID),   label, value, regions, parents, children = []}) = value
  | getIdIdent (N {kind = (ID, ID_TYCON), label, value, regions, parents, children = []}) = value
  | getIdIdent (N {kind = (ID, ID_TYVAR), label, value, regions, parents, children = []}) = value
  | getIdIdent term = raise Fail "term is not an identifier"

fun getIdIdents (N {kind = (IDENTS, IDENTS_LIST), label, value, regions, parents, children}) =
    map getIdIdent children
  | getIdIdents _ = raise Fail "getIdIdents"

fun getSpecName (N {kind = (DEC, DEC_SPEC), label, value, regions, parents, children = [id]}) =
    (case id of
	 N {kind = (ID, ID_VID), label, value, regions, parents, children = []} => value
       | _ => raise Fail "getSpecName")
  | getSpecName _ = raise Fail "getSpecName"

(*fun flattenPatTyped (N {kind = (PAT, PAT_TYPED), label, value, regions, parents, children = [pat, typ]}) = (pat, typ)
  | flattenPatTyped _ = raise Fail "Not a typed pattern"

fun flattenPatApp (N {kind = (PAT, PAT_APP), label, value, regions, parents, children = [id, atpat]}) =
    if isIdVid id
    then (id, [atpat])
    else raise Fail "Not the application of an identifier to arguments"
  | flattenPatApp (N {kind = (PAT, PAT_ATPAT), label, value, regions, parents, children = [atpat]}) = flattenPatApp atpat
  | flattenPatApp (N {kind = (ATPAT, ATPAT_ID), label, value, regions, parents, children = [id]}) =
    if isIdVid id
    then (id, [])
    else raise Fail "Not the application of an identifier to arguments"
  | flattenPatApp pat = raise Fail "Not the application of an identifier to arguments"*)

(* Returns NONE if the argument is not an id pattern, and returns SOME the id
 * of the pattern if the pattern is an id pattern *)
fun getIdPat (N {kind = (PAT,   PAT_ATPAT),   label, value, regions, parents, children = [atpat]}) = getIdPat atpat
  | getIdPat (N {kind = (ATPAT, ATPAT_PAREN), label, value, regions, parents, children = [pat]})   = getIdPat pat
  | getIdPat (N {kind = (ATPAT, ATPAT_STRUC), label, value, regions, parents, children = [atpat]}) = getIdPat atpat
  | getIdPat (N {kind = (ATPAT, ATPAT_ID),    label, value, regions, parents, children = [id]})    = SOME id
  | getIdPat term = NONE

(* Returns NONE if the argument is not an AS pattern, and returns SOME (id, pat),
 * if the pattern is an AS pattern, where id and pat are the parts of the AS. *)
fun getAsPat (N {kind = (PAT,   PAT_ATPAT),   label, value, regions, parents, children = [atpat]})   = getAsPat atpat
  | getAsPat (N {kind = (ATPAT, ATPAT_PAREN), label, value, regions, parents, children = [pat]})     = getAsPat pat
  | getAsPat (N {kind = (ATPAT, ATPAT_STRUC), label, value, regions, parents, children = [atpat]})   = getAsPat atpat
  | getAsPat (N {kind = (PAT,   PAT_AS),      label, value, regions, parents, children = [id, pat]}) = SOME (id, pat)
  | getAsPat term = NONE

(* Returns NONE if the argument is not an id exp, and returns SOME the id
 * if the expression is an id *)
fun getIdExp (N {kind = (EXP,   EXP_ATEXP),   label, value, regions, parents, children = [atexp]}) = getIdExp atexp
  | getIdExp (N {kind = (ATEXP, ATEXP_PAREN), label, value, regions, parents, children = [exp]})   = getIdExp exp
  | getIdExp (N {kind = (ATEXP, ATEXP_ID),    label, value, regions, parents, children = [id]})    = SOME id
  | getIdExp term = NONE

fun getIdBind (N {kind = (BIND, BIND_DEC), label, value, regions, parents, children = [id, args, exp]}) = getIdIdent id
  | getIdBind _ = raise Fail unexpectedFormat

(* Returns NONE if the argument is not an atom expression, and returns SOME the atom
 * if the expression is an atom *)
fun getAtomExp (N {kind = (EXP,   EXP_ATEXP),   label, value, regions, parents, children = [atexp]}) = getAtomExp atexp
  | getAtomExp (N {kind = (ATEXP, ATEXP_PAREN), label, value, regions, parents, children = [exp]})   = getAtomExp exp
  | getAtomExp (N {kind = (ATEXP, ATEXP_SCON),  label, value, regions, parents, children = [scon]})  =
    (case scon of
	 N {kind = (SCON, SCON_ATOMS), label, value, regions, parents, children} =>
	  SOME (mk_term ATOMS_ATOMS label value regions parents children)
       | _ => NONE)
  | getAtomExp term = NONE

fun get_rec_bind_components_args (N {kind = (PARAM, PARAM_P), label, value, regions, parents, children = [pat]}) = pat
  | get_rec_bind_components_args _ = raise Fail "get_rec_bind_components_args:not_a_single_parameter"

fun get_rec_bind_components_exp (N {kind = (EXP, EXP_OP), label, value, regions, parents, children = [exp1, exp2]}) =
    if value = id_class_par
    then case exp2 of
	     N {kind = (EXP, EXP_MBIND), label, value, regions, parents, children = [e1, e2]} => (exp1, e1, e2)
	   | _ => raise Fail "get_rec_bind_components_exp:not_the_bind_combinator"
    else raise Fail "get_rec_bind_components_exp:not_the_parallel_combinator"
  | get_rec_bind_components_exp _ = raise Fail "get_rec_bind_components_exp:not_an_operation"

fun get_rec_bind_components (N {kind = (BIND, BIND_DEC), label, value, regions, parents, children = [id, args, exp]}) =
    let val arg = get_rec_bind_components_args args
	val (X, Y, Z) = get_rec_bind_components_exp exp
    in case getIdExp Z of
	   SOME i =>
	   if getIdIdent i = getIdIdent id
	   then (id, arg, X, Y)
	   else raise Fail "get_rec_bind_components:different_recursive_function_name"
	 | NONE => raise Fail "get_rec_bind_components:last_component_is_not_an_identifier"
    end
  | get_rec_bind_components _ = raise Fail "get_rec_bind_components:not_a_bind_dec"

fun get_includes (N {kind = (DEC, prod), label, value, regions, parents, children}) =
    if prod = DEC_INCLUDE
    then case children of
	     [id, parms] => [getIdIdent id]
	   | _ => []
    else []
  | get_includes term = List.concat (map get_includes (getChildren term))


(* ------ TRANSFORMERS ------ *)

fun expToId (N {kind = (EXP,   EXP_ATEXP), label, value, regions, parents, children = [atexp]}) = expToId atexp
  | expToId (N {kind = (ATEXP, ATEXP_ID),  label, value, regions, parents, children = [id]})    = expToId id
  | expToId (N {kind = (ID,    ID_VID),    label, value, regions, parents, children = []})      = SOME value
  | expToId _ = NONE

fun intToAtExp n =
    let val term1 = mk_new_term SCON_INT (Int.toString n) [] []
    in mk_new_term ATEXP_SCON "" [] [term1]
    end

fun intToExp n = mk_new_term EXP_ATEXP "" [] [intToAtExp n]

fun idToAtExp id =
    let val term1 = mk_new_term ID_VID id [] []
    in mk_new_term ATEXP_ID "" [] [term1]
    end

fun idToExp id = mk_new_term EXP_ATEXP "" [] [idToAtExp id]

fun idToDumAtExp id =
    let val term = mk_new_dum_term ID_VID id [] []
    in mk_new_dum_term ATEXP_ID "" [] [term]
    end

fun idToDumExp id = mk_new_dum_term EXP_ATEXP "" [] [idToDumAtExp id]

fun idToAtPat id =
    let val term1 = mk_new_term ID_VID id [] []
    in mk_new_term ATPAT_ID "" [] [term1]
    end

fun idToPat id = mk_new_term PAT_ATPAT "" [] [idToAtPat id]

fun idToDumAtPat id =
    let val term1 = mk_new_dum_term ID_VID id [] []
    in mk_new_dum_term ATPAT_ID "" [] [term1]
    end

fun idToDumPat id = mk_new_dum_term PAT_ATPAT "" [] [idToDumAtPat id]

fun vid2tycon (N {kind = (ID, ID_VID), label, value, regions, parents, children = []}) =
    mk_term ID_TYCON label value regions parents []
  | vid2tycon _ = raise Fail "term is not a vid"

fun merge term1 term2 =
    if getKind term1 = (PROG, PROG_P)
       andalso
       getKind term2 = (PROG, PROG_P)
    then SOME (updChildren term1 (getChildren term1 @ getChildren term2))
    else NONE


(* ------ TO STRING ------ *)

fun toStringClass SCON       = "SCON"
  | toStringClass ID         = "ID"
  | toStringClass IDENTS     = "IDENTS"
  | toStringClass TYPEVAR    = "TYPEVAR"
  | toStringClass ATPAT      = "ATPAT"
  | toStringClass PAT        = "PAT"
  | toStringClass TYPE       = "TYPE"
  | toStringClass MRULE      = "MRULE"
  | toStringClass MATCH      = "MATCH"
  | toStringClass EXP        = "EXP"
  | toStringClass ATEXP      = "ATEXP"
  | toStringClass PROP       = "PROP"
  | toStringClass CASEPAT    = "CASEPAT"
  | toStringClass ATOMS      = "ATOMS"
  | toStringClass DEC        = "DEC"
  | toStringClass DOC        = "DOC"
  | toStringClass INCPARM    = "INCPARM"
  | toStringClass INCPARMS   = "INCPARMS"
  | toStringClass DATA       = "DATA"
  | toStringClass TYPEVARSEQ = "TYPEVARSEQ"
  | toStringClass TYPESEQ    = "TYPESEQ"
  | toStringClass OTYPESEQ   = "OTYPESEQ"
  | toStringClass TYPESEQSET = "TYPESEQSET"
  | toStringClass BIND       = "BIND"
  | toStringClass BINDS      = "BINDS"
  | toStringClass PARAM      = "PARAM"
  | toStringClass ARG        = "ARG"
  | toStringClass ARGS       = "ARGS"
  | toStringClass PROG       = "PROG"
  | toStringClass DOTS       = "DOTS"
  | toStringClass FILE       = "FILE"

fun toStringProd SCON_INT    = "SCON_INT"
  | toStringProd SCON_REAL   = "SCON_REAL"
  | toStringProd SCON_ATOM   = "SCON_ATOM"
  | toStringProd SCON_ATOMS  = "SCON_ATOMS"
  | toStringProd SCON_STRING = "SCON_STRING"
  (* ID *)
  | toStringProd ID_VID      = "ID_VID"
  | toStringProd ID_TYCON    = "ID_TYCON"
  | toStringProd ID_TYVAR    = "ID_TYVAR"
  (* IDENTS *)
  | toStringProd IDENTS_LIST = "IDENTS_LIST"
  (* TYPEVAR *)
  | toStringProd TYPEVAR_VAR = "TYPEVAR_VAR"
  (* TYPE *)
  | toStringProd TYPE_ARROW  = "TYPE_ARROW"
  | toStringProd TYPE_DARROW = "TYPE_DARROW"
  | toStringProd TYPE_DISJU  = "TYPE_DISJU"
  | toStringProd TYPE_TUPLE  = "TYPE_TUPLE"
  | toStringProd TYPE_TYCON  = "TYPE_TYCON"
  | toStringProd TYPE_VAR    = "TYPE_VAR"
  | toStringProd TYPE_PAREN  = "TYPE_PAREN"
  | toStringProd TYPE_DEP    = "TYPE_DEP"
  | toStringProd TYPE_SET    = "TYPE_SET"
  | toStringProd TYPE_SPSET  = "TYPE_SPSET"
  (*| toStringProd TYPE_ID     = "TYPE_ID"*)
  (* ATPAT *)
  | toStringProd ATPAT_ID    = "ATPAT_ID"
  | toStringProd ATPAT_WILD  = "ATPAT_WILD"
  | toStringProd ATPAT_SCON  = "ATPAT_SCON"
  | toStringProd ATPAT_LIST  = "ATPAT_LIST"
  | toStringProd ATPAT_PAREN = "ATPAT_PAREN"
  | toStringProd ATPAT_STRUC = "ATPAT_STRUC"
  | toStringProd ATPAT_TUPLE = "ATPAT_TUPLE"
  (* PAT *)
  | toStringProd PAT_TYPED   = "PAT_TYPED"
  | toStringProd PAT_AS      = "PAT_AS"
  | toStringProd PAT_CONS    = "PAT_CONS"
  | toStringProd PAT_ATPAT   = "PAT_ATPAT"
  | toStringProd PAT_APP     = "PAT_APP"
  (* MRULE *)
  | toStringProd MRULE_M = "MRULE_M"
  (* MATCH *)
  | toStringProd MATCH_M = "MATCH_M"
  (* EXP *)
  | toStringProd EXP_OR       = "EXP_OR"
  | toStringProd EXP_AND      = "EXP_AND"
  | toStringProd EXP_TYPED    = "EXP_TYPED"
  | toStringProd EXP_LAMBDA   = "EXP_LAMBDA"
  | toStringProd EXP_ITE      = "EXP_ITE"
  | toStringProd EXP_ATEXP    = "EXP_ATEXP"
  | toStringProd EXP_APP      = "EXP_APP"
  | toStringProd EXP_OP       = "EXP_OP"
  | toStringProd EXP_LET      = "EXP_LET"
  | toStringProd EXP_LETR     = "EXP_LETR"
  | toStringProd EXP_CLASS    = "EXP_CLASS"
  | toStringProd EXP_WHERE    = "EXP_WHERE"
  | toStringProd EXP_BINDING  = "EXP_BINDING"
  | toStringProd EXP_MBIND    = "EXP_MBIND"
  | toStringProd EXP_COMP     = "EXP_COMP"
  | toStringProd EXP_CASE     = "EXP_CASE"
  | toStringProd EXP_QUOT     = "EXP_QUOT"
  (* ATEXP *)
  | toStringProd ATEXP_ID      = "ATEXP_ID"
  | toStringProd ATEXP_SCON    = "ATEXP_SCON"
  | toStringProd ATEXP_TUPLE   = "ATEXP_TUPLE"
  | toStringProd ATEXP_PAREN   = "ATEXP_PAREN"
  | toStringProd ATEXP_LIST    = "ATEXP_LIST"
  | toStringProd ATEXP_BAG     = "ATEXP_BAG"
  | toStringProd ATEXP_PRIOR   = "ATEXP_PRIOR"
  | toStringProd ATEXP_ANY     = "ATEXP_ANY"
  | toStringProd ATEXP_MSG     = "ATEXP_MSG"
  | toStringProd ATEXP_ONCE    = "ATEXP_ONCE"
  | toStringProd ATEXP_SENDOC  = "ATEXP_SENDOC"
  | toStringProd ATEXP_ONLOC   = "ATEXP_ONLOC"
  | toStringProd ATEXP_SKIP    = "ATEXP_SKIP"
  | toStringProd ATEXP_WAIT    = "ATEXP_WAIT"
  | toStringProd ATEXP_NULL    = "ATEXP_NULL"
  | toStringProd ATEXP_MINUS   = "ATEXP_MINUS"
  | toStringProd ATEXP_TYPE    = "ATEXP_TYPE"
  | toStringProd ATEXP_STATEC  = "ATEXP_STATEC"
  | toStringProd ATEXP_MEMORYC = "ATEXP_MEMORYC"
  (* CASEPAT *)
  | toStringProd CASEPAT_PAT    = "CASEPAT_PAT"
  (* ATOMS *)
  | toStringProd ATOMS_ATOMS = "ATOMS_ATOMS"
  | toStringProd ATOMS_LIST  = "ATOMS_LIST"
  | toStringProd ATOMS_WILD  = "ATOMS_WILD"
  (* DEC *)
  | toStringProd DEC_LET      = "DEC_LET"
  | toStringProd DEC_LETR     = "DEC_LETR"
  | toStringProd DEC_CLASS    = "DEC_CLASS"
  | toStringProd DEC_CLASSREC = "DEC_CLASSREC"
  | toStringProd DEC_CONS     = "DEC_CONS"
  | toStringProd DEC_OCONS    = "DEC_OCONS"
  | toStringProd DEC_PSET     = "DEC_PSET"
  | toStringProd DEC_PMAP     = "DEC_PMAP"
  | toStringProd DEC_PARAM    = "DEC_PARAM"
  | toStringProd DEC_PARAMP   = "DEC_PARAMP"
  | toStringProd DEC_TYPARAM  = "DEC_TYPARAM"
  | toStringProd DEC_TYPARAMP = "DEC_TYPARAMP"
  | toStringProd DEC_ETPARAM  = "DEC_ETPARAM"
  | toStringProd DEC_TYCON    = "DEC_TYCON"
  | toStringProd DEC_EQTYCON  = "DEC_EQTYCON"
  | toStringProd DEC_TYFUN    = "DEC_TYFUN"
  | toStringProd DEC_QMSG     = "DEC_QMSG"
  | toStringProd DEC_EQMSG    = "DEC_EQMSG"
  | toStringProd DEC_PARSE    = "DEC_PARSE"
  | toStringProd DEC_TYPEOF   = "DEC_TYPEOF"
  | toStringProd DEC_INFIX    = "DEC_INFIX"
  | toStringProd DEC_MAIN     = "DEC_MAIN"
  | toStringProd DEC_SPEC     = "DEC_SPEC"
  | toStringProd DEC_IMPORT   = "DEC_IMPORT"
  | toStringProd DEC_TIMPORT  = "DEC_TIMPORT"
  | toStringProd DEC_EXPORT   = "DEC_EXPORT"
  | toStringProd DEC_INCLUDE  = "DEC_INCLUDE"
  | toStringProd DEC_ASSUME   = "DEC_ASSUME"
  | toStringProd DEC_GUARANT  = "DEC_GUARANT"
  | toStringProd DEC_DOC      = "DEC_DOC"
  | toStringProd DEC_INV      = "DEC_INV"
  | toStringProd DEC_ORDER    = "DEC_ORDER"
  | toStringProd DEC_PROGRESS = "DEC_PROGRESS"
  | toStringProd DEC_STRICT   = "DEC_STRICT"
  | toStringProd DEC_CONSIST  = "DEC_CONSIST"
  | toStringProd DEC_MEMORY   = "DEC_MEMORY"
  | toStringProd DEC_DATA     = "DEC_DATA"
  | toStringProd DEC_ABSTYPE  = "DEC_ABSTYPE"
  | toStringProd DEC_VAR      = "DEC_VAR"
  | toStringProd DEC_OPTIONS  = "DEC_OPTIONS"
  (* INCPARM *)
  | toStringProd INCPARM_EXP = "INCPARM_EXP"
  | toStringProd INCPARM_TYP = "INCPARM_TYP"
  | toStringProd INCPARM_INT = "INCPARM_INT"
  (* INCPARMS *)
  | toStringProd INCPARMS_P = "INCPARMS_P"
  (* DATA *)
  | toStringProd DATA_CONS = "DATA_CONS"
  (* PROP *)
  | toStringProd PROP_EXP   = "PROP_EXP"
  | toStringProd PROP_OR    = "PROP_OR"
  | toStringProd PROP_AND   = "PROP_AND"
  | toStringProd PROP_IMP   = "PROP_IMP"
  | toStringProd PROP_IFF   = "PROP_IFF"
  | toStringProd PROP_ALL   = "PROP_ALL"
  | toStringProd PROP_EX    = "PROP_EX"
  | toStringProd PROP_OBS   = "PROP_OBS"
  | toStringProd PROP_PAREN = "PROP_PAREN"
  (* DOC *)
  | toStringProd DOC_LINE = "DOC_LINE"
  (* TYPEVARSEQ *)
  | toStringProd TYPEVARSEQ_ONE = "TYPEVARSEQ_ONE"
  | toStringProd TYPEVARSEQ_SEQ = "TYPEVARSEQ_SEQ"
  | toStringProd TYPEVARSEQ_EM  = "TYPEVARSEQ_EM"
  (* TYPESEQ *)
  | toStringProd TYPESEQ_ONE = "TYPESEQ_ONE"
  | toStringProd TYPESEQ_SEQ = "TYPESEQ_SEQ"
  | toStringProd TYPESEQ_EM  = "TYPESEQ_EM"
  (* OTYPESEQ *)
  | toStringProd OTYPESEQ_UNM = "OTYPESEQ_UNM"
  | toStringProd OTYPESEQ_NAM = "OTYPESEQ_NAM"
  (* TYPESEQSET *)
  | toStringProd TYPESEQSET_SET = "TYPESEQSET_SET"
  (* BIND *)
  | toStringProd BIND_DEC  = "BIND_DEC"
  | toStringProd BIND_TDEC = "BIND_TDEC"
  | toStringProd BIND_PAT  = "BIND_PAT"
  | toStringProd BIND_IOP  = "BIND_IOP"
  | toStringProd BIND_TIOP = "BIND_TIOP"
  (* BINDS *)
  | toStringProd BINDS_LIST = "BINDS_LIST"
  (* PARAM *)
  | toStringProd PARAM_P = "PARAM_P"
  (* ARG *)
  | toStringProd ARG_A = "ARG_A"
  | toStringProd ARG_T = "ARG_T"
  (* ARGS *)
  | toStringProd ARGS_EM   = "ARGS_EM"
  | toStringProd ARGS_PSEQ = "ARGS_PSEQ"
  | toStringProd ARGS_LSEQ = "ARGS_LSEQ"
  (* PROG *)
  | toStringProd PROG_P = "PROG_P"
  (* DOTS *)
  | toStringProd DOTS_D = "DOTS_D"
  (* FILE *)
  | toStringProd FILE_F = "FILE_F"

fun toString_kind (class, prod) =
    "(" ^ toStringClass class ^ "," ^ toStringProd prod ^ ")"

fun toString_regions regions = R.toStringRegList regions

fun toString_value value = value

fun toString_term (N {kind, label, value, regions, parents, children}) =
    "N{kind="   ^ toString_kind     kind     ^ "," ^
    "label="    ^ D.toStringLabel   label    ^ "," ^
    "value="    ^ toString_value    value    ^ "," ^
    "regions="  ^ toString_regions  regions  ^ "," ^
    "parents="  ^ toString_regions  parents  ^ "," ^
    "children=" ^ toString_children children ^ "}"
and toString_children children =
    #1 (foldl (fn (term, (st, sep)) => (st ^ sep ^ toString_term term, ","))
	      ("[", "")
	      children) ^ "]"

val toString = toString_term

fun print_term term = print (toString term)


(* ------ EXPORT TO CODE ------*)

val sepnl = "\n"

val dots     = ".."
val ldots    = "\226\159\168"
val rdots    = "\226\159\169"

fun transfun #"`" = "\\`"
  | transfun #"\"" = "\\\""
  | transfun x = Char.toString x

val transform = String.translate transfun

fun splitAtoms' (a :: "" :: "" :: b :: lst) = a :: splitAtoms' (b :: lst)
  | splitAtoms' (a :: b :: lst) = splitAtoms' ((a ^ "`" ^ b) :: lst)
  | splitAtoms' (a :: lst) = a :: splitAtoms' lst
  | splitAtoms' [] = []

val splitAtoms = splitAtoms' o (String.fields (fn #"`" => true | _ => false))

(* SCON *)
fun export (N {kind = (SCON, SCON_INT),    label, value, regions, parents, children = []}) = value
  | export (N {kind = (SCON, SCON_REAL),   label, value, regions, parents, children = []}) = value
  | export (N {kind = (SCON, SCON_ATOM),   label, value, regions, parents, children = []}) = "`"  ^ transform value ^ "`"
  | export (N {kind = (SCON, SCON_ATOMS),  label, value, regions, parents, children = []}) = "``" ^ transform value ^ "``"
  | export (N {kind = (SCON, SCON_STRING), label, value, regions, parents, children = []}) = "\"" ^ transform value ^ "\""
  (* ID *)
  | export (N {kind = (ID, ID_VID),    label, value, regions, parents, children = []}) = value
  | export (N {kind = (ID, ID_TYCON),  label, value, regions, parents, children = []}) = value
  | export (N {kind = (ID, ID_TYVAR),  label, value, regions, parents, children = []}) = value
  (* IDENTS *)
  | export (N {kind = (IDENTS, IDENTS_LIST), label, value, regions, parents, children}) = exportList children " "
  (* TYPEVAR *)
  | export (N {kind = (TYPEVAR, TYPEVAR_VAR),  label, value, regions, parents, children = [tv]}) = export tv
  (* TYPE *)
  | export (N {kind = (TYPE, TYPE_ARROW),  label, value, regions, parents, children = [ty1, ty2]})  = export ty1 ^ " -> " ^ export ty2
  | export (N {kind = (TYPE, TYPE_DARROW), label, value, regions, parents, children = [i, t1, t2]}) = export i ^ ":" ^ export t1 ^ " -> " ^ export t2
  | export (N {kind = (TYPE, TYPE_DISJU),  label, value, regions, parents, children = [ty1, ty2]})  = export ty1 ^ " + " ^ export ty2
  | export (N {kind = (TYPE, TYPE_TUPLE),  label, value, regions, parents, children})               = exportList children " * "
  | export (N {kind = (TYPE, TYPE_TYCON),  label, value, regions, parents, children = [tn, ts]})    = export ts ^ " " ^ export tn
  | export (N {kind = (TYPE, TYPE_VAR),    label, value, regions, parents, children = [tv]})        = export tv
  | export (N {kind = (TYPE, TYPE_PAREN),  label, value, regions, parents, children = [ty]})        = "(" ^ export ty ^ ")"
  | export (N {kind = (TYPE, TYPE_DEP),    label, value, regions, parents, children = [id, ty]})    = export id ^ ":" ^ export ty
  | export (N {kind = (TYPE, TYPE_SET),    label, value, regions, parents, children = [id, t, e]})  = "{" ^ export id ^ ":" ^ export t ^ "|" ^ export e ^ "}"
  | export (N {kind = (TYPE, TYPE_SPSET),  label, value, regions, parents, children = [e]})         = "{" ^ export e ^ "}"
  (*| export (N {kind = (TYPE, TYPE_ID),     label, value, regions, parents, children = [id]}) = export id*)
  (* ATPAT *)
  | export (N {kind = (ATPAT, ATPAT_ID),    label, value, regions, parents, children = [i]})  = export i
  | export (N {kind = (ATPAT, ATPAT_WILD),  label, value, regions, parents, children = []})   = "_"
  | export (N {kind = (ATPAT, ATPAT_SCON),  label, value, regions, parents, children = [sc]}) = export sc
  | export (N {kind = (ATPAT, ATPAT_LIST),  label, value, regions, parents, children})        = "[" ^ exportList children "; " ^ "]"
  | export (N {kind = (ATPAT, ATPAT_PAREN), label, value, regions, parents, children = [x]})  = "(" ^ export x ^ ")"
  | export (N {kind = (ATPAT, ATPAT_STRUC), label, value, regions, parents, children = [x]})  = "struct:" ^ export x
  | export (N {kind = (ATPAT, ATPAT_TUPLE), label, value, regions, parents, children})        = "(" ^ exportList children ", " ^ ")"
  (* PAT *)
  | export (N {kind = (PAT, PAT_TYPED), label, value, regions, parents, children = [pat, ty]})    = export pat ^ " : " ^ export ty
  | export (N {kind = (PAT, PAT_AS),    label, value, regions, parents, children = [id, pat]})    = export id ^ " as " ^ export pat
  | export (N {kind = (PAT, PAT_CONS),  label, value, regions, parents, children = [pat1, pat2]}) = export pat1 ^ " " ^ value ^ " " ^ export pat2
  | export (N {kind = (PAT, PAT_ATPAT), label, value, regions, parents, children = [x]})          = export x
  | export (N {kind = (PAT, PAT_APP),   label, value, regions, parents, children = [f, x]})       = export f ^ " " ^ export x
  (* MRULE *)
  | export (N {kind = (MRULE, MRULE_M), label, value, regions, parents, children = [p, e]}) = "of " ^ export p ^ " => " ^ export e
  (* MATCH *)
  | export (N {kind = (MATCH, MATCH_M), label, value, regions, parents, children}) = exportList children " "
  (* EXP *)
  | export (N {kind = (EXP, EXP_OR),       label, value, regions, parents, children = [e1, e2]})      = export e1 ^ " or "  ^ export e2
  | export (N {kind = (EXP, EXP_AND),      label, value, regions, parents, children = [e1, e2]})      = export e1 ^ " & "   ^ export e2
  | export (N {kind = (EXP, EXP_TYPED),    label, value, regions, parents, children = [e, t]})        = export e  ^ " : "   ^ export t
  | export (N {kind = (EXP, EXP_LAMBDA),   label, value, regions, parents, children = [p, e]})        = "\\" ^ export p ^ "." ^ export e
  | export (N {kind = (EXP, EXP_ITE),      label, value, regions, parents, children = [e1, e2, e3]})  = "if " ^ export e1 ^ sepnl ^ " then " ^ export e2 ^ sepnl ^ " else " ^ export e3
  | export (N {kind = (EXP, EXP_ATEXP),    label, value, regions, parents, children = [a]})           = export a
  | export (N {kind = (EXP, EXP_APP),      label, value, regions, parents, children = [f, x]})        = export f ^ " " ^ export x
  | export (N {kind = (EXP, EXP_OP),       label, value, regions, parents, children = [e1, e2]})      = export e1 ^ " " ^ value ^ " " ^ export e2
  | export (N {kind = (EXP, EXP_LET),      label, value, regions, parents, children = [b, e]})        = "let "    ^ export b ^ " in" ^ sepnl ^ export e ^ sepnl
  | export (N {kind = (EXP, EXP_LETR),     label, value, regions, parents, children = [b, e]})        = "letrec " ^ export b ^ " in" ^ sepnl ^ export e ^ sepnl
  | export (N {kind = (EXP, EXP_CLASS),    label, value, regions, parents, children = [b, e]})        = "class "  ^ export b ^ " in" ^ sepnl ^ export e ^ sepnl
  | export (N {kind = (EXP, EXP_WHERE),    label, value, regions, parents, children = [e, b]})        = export e  ^ " where " ^ export b
  | export (N {kind = (EXP, EXP_BINDING),  label, value, regions, parents, children = [e1, p, e2]})   = export e1 ^ " >> " ^ export p ^ " >> " ^ export e2
  | export (N {kind = (EXP, EXP_MBIND),    label, value, regions, parents, children = [e1, e2]})      = export e1 ^ " >>= " ^ export e2
  | export (N {kind = (EXP, EXP_COMP),     label, value, regions, parents, children = (exp :: exps)}) =
    let val sp = if String.isSubstring "P" value then ";Prior(self)" else ""   (* P for Prior   *)
	val p1 = if String.isSubstring "F" value then "^|"           else "@|" (* F for liFting *)
	val p2 = if String.isSubstring "C" value then "|"            else p1   (* C for Concat  *)
	val (exps', opt) = if String.isSubstring "O" value                     (* O for option  *)
			   then let val rev    = List.rev exps
				    val last   = List.hd rev
				    val firsts = List.rev (List.tl rev)
				in (firsts, "?" ^ export last)
				end
			   else (exps, "")
    in export exp ^ p2 ^ exportList exps' ";" ^ sp ^ opt ^ "|"
    end
  | export (N {kind = (EXP, EXP_CASE),     label, value, regions, parents, children = [e, m]})        = "case " ^ export e ^ " " ^ export m
  | export (N {kind = (EXP, EXP_QUOT),     label, value, regions, parents, children = [e]})           = export e ^ "/~"
  (* ATEXP *)
  | export (N {kind = (ATEXP, ATEXP_ID),      label, value, regions, parents, children = [i]})      = export i
  | export (N {kind = (ATEXP, ATEXP_SCON),    label, value, regions, parents, children = [sc]})     = export sc
  | export (N {kind = (ATEXP, ATEXP_TUPLE),   label, value, regions, parents, children})            = "(" ^ exportList children ", " ^ ")"
  | export (N {kind = (ATEXP, ATEXP_PAREN),   label, value, regions, parents, children = [x]})      = "(" ^ export x ^ ")"
  | export (N {kind = (ATEXP, ATEXP_LIST),    label, value, regions, parents, children})            = "[" ^ exportList children "; " ^ "]"
  | export (N {kind = (ATEXP, ATEXP_BAG),     label, value, regions, parents, children})            = "{" ^ exportList children "; " ^ "}"
  | export (N {kind = (ATEXP, ATEXP_PRIOR),   label, value, regions, parents, children = [e]})      = "Prior("   ^ export e  ^ ")"
  | export (N {kind = (ATEXP, ATEXP_ANY),     label, value, regions, parents, children = [e]})      = "any("     ^ export e  ^ ")"
  | export (N {kind = (ATEXP, ATEXP_MSG),     label, value, regions, parents, children = [a, e]})   = "MSG("     ^ export a  ^ "," ^ export e ^ ")"
  | export (N {kind = (ATEXP, ATEXP_ONCE),    label, value, regions, parents, children = [e]})      = "Once("    ^ export e  ^ ")"
  | export (N {kind = (ATEXP, ATEXP_SENDOC),  label, value, regions, parents, children = [e]})      = "Output("  ^ export e  ^ ")"
  | export (N {kind = (ATEXP, ATEXP_ONLOC),   label, value, regions, parents, children = [e]})      = "OnLoc("   ^ export e  ^ ")"
  | export (N {kind = (ATEXP, ATEXP_SKIP),    label, value, regions, parents, children = [e]})      = "Skip("    ^ export e  ^ ")"
  | export (N {kind = (ATEXP, ATEXP_WAIT),    label, value, regions, parents, children = [t, e]})   = "wait("    ^ export t  ^ "," ^ export e  ^ ")"
  | export (N {kind = (ATEXP, ATEXP_NULL),    label, value, regions, parents, children = []})       = "Null"
  | export (N {kind = (ATEXP, ATEXP_MINUS),   label, value, regions, parents, children = [e]})      = "~"        ^ export e
  | export (N {kind = (ATEXP, ATEXP_TYPE),    label, value, regions, parents, children = [t]})      = "::"       ^ export t
  | export (N {kind = (ATEXP, ATEXP_STATEC),  label, value, regions, parents, children})            = "State("   ^ exportList children ", " ^ ")"
  | export (N {kind = (ATEXP, ATEXP_MEMORYC), label, value, regions, parents, children})            = "Memory("  ^ exportList children ", " ^ ")"
  (* CASEPAT *)
  | export (N {kind = (CASEPAT, CASEPAT_PAT), label, value, regions, parents, children = [p]})    =  export p
  (* ATOMS *)
  | export (N {kind = (ATOMS, ATOMS_ATOMS), label, value, regions, parents, children = []}) =
    let val toks = splitAtoms value
    in T.fmt {init = "``", final = "``", sep = " ", fmt = fn x => x} toks
    end
  | export (N {kind = (ATOMS, ATOMS_LIST),  label, value, regions, parents, children = []}) =
    let val toks = splitAtoms value
    in T.fmt {init = "[", final = "]", sep = ";", fmt = (fn st => "`" ^ st ^ "`")} toks
    end
  | export (N {kind = (ATOMS, ATOMS_WILD),  label, value, regions, parents, children = []}) = "_"
  (* DEC *)
  | export (N {kind = (DEC, DEC_LET),      label, value, regions, parents, children = [b]})             = "let "       ^ export b
  | export (N {kind = (DEC, DEC_LETR),     label, value, regions, parents, children = [b]})             = "letrec "    ^ export b
  | export (N {kind = (DEC, DEC_CLASS),    label, value, regions, parents, children = [b]})             = "class "     ^ export b
  | export (N {kind = (DEC, DEC_CLASSREC), label, value, regions, parents, children = [i, a, x, y]})    = "classrec "  ^ export i ^ " " ^ export a ^ " = " ^ export x ^ " || " ^ export y ^ " >>= " ^ export i
  | export (N {kind = (DEC, DEC_CONS),     label, value, regions, parents, children = [i, a, t]})       = "cons "      ^ export i ^ " " ^ export a ^ " : " ^ export t
  | export (N {kind = (DEC, DEC_OCONS),    label, value, regions, parents, children = [i, a, t, v, s]}) = "cons "      ^ export i ^ " " ^ export a ^ " : " ^ export t ^ " with " ^ export v ^ " in " ^ export s
  | export (N {kind = (DEC, DEC_PSET),     label, value, regions, parents, children = [i, t]})          = "set " ^ export i ^ " : " ^ export t
  | export (N {kind = (DEC, DEC_PMAP),     label, value, regions, parents, children = [i, t, u]})       = "map " ^ export i ^ " : " ^ export t ^ ", " ^ export u
  | export (N {kind = (DEC, DEC_PARAM),    label, value, regions, parents, children = [i, t]})          = "parameter " ^ export i ^ " : " ^ export t
  | export (N {kind = (DEC, DEC_PARAMP),   label, value, regions, parents, children = [i, t, p]})       = "parameter " ^ export i ^ " : " ^ export t ^ " | " ^ export p
  | export (N {kind = (DEC, DEC_TYPARAM),  label, value, regions, parents, children = [s, tc, ty]})     = "parameter " ^ export s ^ " " ^ export tc ^ " : " ^ export ty
  | export (N {kind = (DEC, DEC_TYPARAMP), label, value, regions, parents, children = [s, tc, ty, p]})  = "parameter " ^ export s ^ " " ^ export tc ^ " : " ^ export ty ^ " | " ^ export p
  | export (N {kind = (DEC, DEC_ETPARAM),  label, value, regions, parents, children = [s, tc, i, ty]})  = "parameter " ^ export s ^ " " ^ export tc ^ ", " ^ export i ^ " : " ^ export ty
  | export (N {kind = (DEC, DEC_TYCON),    label, value, regions, parents, children = [s, tc, ty]})     = "constant "  ^ export s ^ " " ^ export tc ^ " : " ^ export ty
  | export (N {kind = (DEC, DEC_EQTYCON),  label, value, regions, parents, children = [s, tc, i, ty]})  = "constant "  ^ export s ^ " " ^ export tc ^ ", " ^ export i ^ " : " ^ export ty
  | export (N {kind = (DEC, DEC_TYFUN),    label, value, regions, parents, children = [seq, tc, ty]})   = "type "      ^ export seq ^ " " ^ export tc ^ " = " ^ export ty
  | export (N {kind = (DEC, DEC_QMSG),     label, value, regions, parents, children = [id, atm, typ]})  = value ^ "  " ^ export id ^ " "  ^ export atm ^ " " ^ export typ
  | export (N {kind = (DEC, DEC_EQMSG),    label, value, regions, parents, children = [id, atm, typ]})  = value ^ "  " ^ export id ^ " "  ^ export atm ^ " " ^ export typ
  | export (N {kind = (DEC, DEC_PARSE),    label, value, regions, parents, children = []})              = "parsing_error"
  | export (N {kind = (DEC, DEC_TYPEOF),   label, value, regions, parents, children = [id]})            = "typeof?(" ^ export id ^ ")"
  | export (N {kind = (DEC, DEC_INFIX),    label, value, regions, parents, children = [id]})            =
    (if value = "L"
     then "infix "
     else if value = "R"
     then "infixr "
     else raise Fail "unknown fixity") ^ export id
  | export (N {kind = (DEC, DEC_MAIN),     label, value, regions, parents, children = [exp]})                          = "main "          ^ export exp
  | export (N {kind = (DEC, DEC_SPEC),     label, value, regions, parents, children = [id]})                           = "specification " ^ export id
  | export (N {kind = (DEC, DEC_IMPORT),   label, value, regions, parents, children})                                  = "import "        ^ exportList children " "
  | export (N {kind = (DEC, DEC_TIMPORT),  label, value, regions, parents, children})                                  = "import type "   ^ exportList children " "
  | export (N {kind = (DEC, DEC_EXPORT),   label, value, regions, parents, children})                                  = "export "        ^ exportList children " "
  | export (N {kind = (DEC, DEC_INCLUDE),  label, value, regions, parents, children = [id, parms]})                    = "include "       ^ export id ^ export parms
  | export (N {kind = (DEC, DEC_ASSUME),   label, value, regions, parents, children})                                  = "assume "        ^ exportList children " "
  | export (N {kind = (DEC, DEC_GUARANT),  label, value, regions, parents, children})                                  = "guarantee "     ^ exportList children " "
  | export (N {kind = (DEC, DEC_DOC),      label, value, regions, parents, children})                                  = "(** "           ^ exportList children "\n" ^ "*)"
  | export (N {kind = (DEC, DEC_INV),      label, value, regions, parents, children = [i, c, a, x, B]})                = "invariant "     ^ export i ^ " on " ^ export x ^ " in "   ^ export c ^ " " ^ export a ^ " == " ^ export B
  | export (N {kind = (DEC, DEC_ORDER),    label, value, regions, parents, children = [i, c, a, x, y, B]})             = "ordering "      ^ export i ^ " on " ^ export x ^ " then " ^ export y ^ " in " ^ export c ^ " " ^ export a ^ " == "   ^ export B
  | export (N {kind = (DEC, DEC_PROGRESS), label, value, regions, parents, children = [i, c, a, x, y, W, B]})          = "progress "      ^ export i ^ " on " ^ export x ^ " then " ^ export y ^ " in " ^ export c ^ " " ^ export a ^ " with " ^ export W ^ " == " ^ export B
  | export (N {kind = (DEC, DEC_STRICT),   label, value, regions, parents, children = [i, c, a, x, y, v, W, z, P, B]}) = "progress "      ^ export i ^ " on " ^ export x ^ " then " ^ export y ^ " in " ^ export c ^ " " ^ export a ^ " with " ^ export v ^ " in " ^ export W ^ " and " ^ export z ^ " => " ^ export P ^ " == " ^ export B
  | export (N {kind = (DEC, DEC_CONSIST),  label, value, regions, parents, children = [i, c, a, x, y, B]})             = "consistency "   ^ export i ^ " on " ^ export x ^ " and "  ^ export y ^ " in " ^ export c ^ " " ^ export a ^ " == "   ^ export B
  | export (N {kind = (DEC, DEC_MEMORY),   label, value, regions, parents, children = [i, c, a, x, y, v, W, B]})       = "memory "        ^ export i ^ " on " ^ export x ^ " and "  ^ export y ^ " in " ^ export c ^ " " ^ export a ^ " with " ^ export v ^ " in " ^ export W ^ " == " ^ export B
  | export (N {kind = (DEC, DEC_DATA),     label, value, regions, parents, children = (i :: cons)})                    = "data "          ^ export i ^ " = "  ^ exportList cons " | "
  | export (N {kind = (DEC, DEC_ABSTYPE),  label, value, regions, parents, children = [c, t, b]})                      = "abstype "       ^ export c ^ " = "  ^ export t ^ " with " ^ export b
  | export (N {kind = (DEC, DEC_VAR),      label, value, regions, parents, children = [i, t]})                         = "variable "      ^ export i ^ " : "  ^ export t
  | export (N {kind = (DEC, DEC_OPTIONS),  label, value, regions, parents, children})                                  = "options "       ^ exportList children " "
  (* INCPARMS *)
  | export (N {kind = (INCPARMS, INCPARMS_P), label, value, regions, parents, children}) = if List.null children then "" else " where " ^ exportList children ", "
  (* INCPARM *)
  | export (N {kind = (INCPARM, INCPARM_EXP), label, value, regions, parents, children = [id, exp]})   = export id ^ " = " ^ export exp
  | export (N {kind = (INCPARM, INCPARM_TYP), label, value, regions, parents, children = [id, typ]})   = "type " ^ export id ^ " = " ^ export typ
  | export (N {kind = (INCPARM, INCPARM_INT), label, value, regions, parents, children = [id, atoms]}) = "interface " ^ export id ^ " = " ^ export atoms
  (* DATA *)
  | export (N {kind = (DATA, DATA_CONS), label, value, regions, parents, children = [id, typ]}) = export id ^ " of " ^ export typ
  (* PROP *)
  | export (N {kind = (PROP, PROP_EXP),   label, value, regions, parents, children = [exp]})         = export exp
  | export (N {kind = (PROP, PROP_OR),    label, value, regions, parents, children = [e1, e2]})      = export e1 ^ " \\/ " ^ export e2
  | export (N {kind = (PROP, PROP_AND),   label, value, regions, parents, children = [e1, e2]})      = export e1 ^ " /\\ " ^ export e2
  | export (N {kind = (PROP, PROP_IMP),   label, value, regions, parents, children = [e1, e2]})      = export e1 ^ " => "  ^ export e2
  | export (N {kind = (PROP, PROP_IFF),   label, value, regions, parents, children = [e1, e2]})      = export e1 ^ " <=> " ^ export e2
  | export (N {kind = (PROP, PROP_ALL),   label, value, regions, parents, children = [v, typ, prp]}) = "forall " ^ export v ^ " : " ^ export typ ^ ". " ^ export prp
  | export (N {kind = (PROP, PROP_EX),    label, value, regions, parents, children = [v, typ, prp]}) = "exists " ^ export v ^ " : " ^ export typ ^ ". " ^ export prp
  | export (N {kind = (PROP, PROP_OBS),   label, value, regions, parents, children = [c, v, e]})     = export c ^ " observes " ^ export v ^ " @ " ^ export e
  | export (N {kind = (PROP, PROP_PAREN), label, value, regions, parents, children = [prp]})         = "( " ^ export prp ^ ")"
  (* DOC *)
  | export (N {kind = (DEC, DOC_LINE), label, value, regions, parents, children}) = value
  (* BIND *)
  | export (N {kind = (BIND, BIND_DEC),  label, value, regions, parents, children = [f, a, e]})          = export f ^ " " ^ export a ^ " = " ^ export e
  | export (N {kind = (BIND, BIND_TDEC), label, value, regions, parents, children = [f, a, t, e]})       = export f ^ " " ^ export a ^ " : " ^ export t ^ " = " ^ export e
  | export (N {kind = (BIND, BIND_PAT),  label, value, regions, parents, children = [p, e]})             = export p ^ " = " ^ export e
  | export (N {kind = (BIND, BIND_IOP),  label, value, regions, parents, children = [id, a1, a2, e]})    = (if value = "L" then "infix " else if value = "R" then "infixr " else raise Fail "unknow fixity") ^ export id ^ " (" ^ export a1 ^ ", " ^ export a2 ^ ") = " ^ export e
  | export (N {kind = (BIND, BIND_TIOP), label, value, regions, parents, children = [id, a1, a2, t, e]}) = (if value = "L" then "infix " else if value = "R" then "infixr " else raise Fail "unknow fixity") ^ export id ^ " (" ^ export a1 ^ ", " ^ export a2 ^ ") : " ^ export t ^ " = " ^ export e
  (* BINDS *)
  | export (N {kind = (BINDS, BINDS_LIST), label, value, regions, parents, children}) = exportList children " and "
  (* PARAM *)
  | export (N {kind = (PARAM, PARAM_P), label, value, regions, parents, children}) = exportList children " "
  (* ARG *)
  | export (N {kind = (ARG, ARG_A), label, value, regions, parents, children = [id, args, ty]}) = export id ^ export args ^ " : " ^ export ty
  | export (N {kind = (ARG, ARG_T), label, value, regions, parents, children = [tv]})           = export tv ^ " : Type"
  (* ARGS *)
  | export (N {kind = (ARGS, ARGS_EM),  label, value, regions, parents, children = []})  = ""
  | export (N {kind = (ARGS, ARGS_PSEQ), label, value, regions, parents, children})      = "(" ^ exportList children "; " ^ ")"
  | export (N {kind = (ARGS, ARGS_LSEQ), label, value, regions, parents, children})      = "[" ^ exportList children "; " ^ "]"
  (* TYPEVARSEQ *)
  | export (N {kind = (TYPEVARSEQ, TYPEVARSEQ_ONE), label, value, regions, parents, children = [t]}) = export t
  | export (N {kind = (TYPEVARSEQ, TYPEVARSEQ_SEQ), label, value, regions, parents, children}) = "(" ^ exportList children ", " ^ ")"
  | export (N {kind = (TYPEVARSEQ, TYPEVARSEQ_EM),  label, value, regions, parents, children = []}) = ""
  (* TYPESEQ *)
  | export (N {kind = (TYPESEQ, TYPESEQ_ONE), label, value, regions, parents, children = [t]}) = export t
  | export (N {kind = (TYPESEQ, TYPESEQ_SEQ), label, value, regions, parents, children}) = "(" ^ exportList children ", " ^ ")"
  | export (N {kind = (TYPESEQ, TYPESEQ_EM),  label, value, regions, parents, children = []}) = ""
  (* OTYPESEQ *)
  | export (N {kind = (OTYPESEQ, OTYPESEQ_UNM), label, value, regions, parents, children = [typeseq]}) = export typeseq
  | export (N {kind = (OTYPESEQ, OTYPESEQ_NAM), label, value, regions, parents, children = [typeseq]}) = value ^ " : " ^ export typeseq
  (* TYPESEQSET *)
  | export (N {kind = (TYPESEQSET, TYPESEQSET_SET), label, value, regions, parents, children}) = "{" ^ exportList children ", " ^ "}"
  (* DOTS *)
  | export (N {kind = (DOTS, DOTS_D), label, value, regions, parents, children}) = ldots ^ dots ^ exportList children dots ^ (if List.null children then "" else dots) ^ rdots
  (* FILE *)
  | export (N {kind = (FILE, FILE_F), label, value, regions, parents, children}) = exportList children (";;" ^ sepnl) ^ (if List.null children then "" else ";;" ^ sepnl)
  (* PROG *)
  | export (N {kind = (PROG, PROG_P), label, value, regions, parents, children}) = exportList children sepnl ^ "\n"
  (* wrong format *)
  | export term = (print (toString term); raise Fail wrongFormat)

and exportList list sep =
    T.fmt {init = "", final = "", sep = sep, fmt = export} list


(* ------ SLICING ------ *)

fun oneDec terms =
    List.exists (fn term => kindIsDec (getKind term))
		terms

fun flatten [] = []
  | flatten (term :: terms) =
    if kindIsDots (getKind term)
    then let val children = getChildren term
	 (*val _ = print (toString term ^ "\n")*)
	 in if oneDec children
	    then term :: (flatten terms)
	    else children @ (flatten terms)
	 end
    else term :: (flatten terms)

fun slicing' (term as N {kind, label, value, regions, parents, children}) deps =
    let val children' = map (fn x => slicing' x deps) children
    in if SET.member (deps, label)
       then if kindIsFile (getKind term)
	    (* NOTE: That's so we don't keep the extra empty sliced out declarations
	     * under a FILE node. *)
	    then updChildren term (flatten children')
	    else updChildren term children'
       else mk_new_dum_term' DOTS_D [] (flatten children')
    end

fun slicing term labs = slicing' term (SET.addList (SET.empty, labs))


(* ------ FREE IDENTIFIERS ------ *)

(* Identifier set of a program *)
structure IDS = BinarySetFn(type ord_key = string val compare = String.compare)

(* Extracts the set of value identifiers occurring in the argument. *)
fun getVIdentsTerm term =
    if kindIsIdVid (getKind term)
    then IDS.singleton (getValue term)
    else foldr (fn (term, set) => IDS.union (getVIdentsTerm term, set))
	       IDS.empty
	       (getChildren term)

(* Extracts the set of identifiers occurring in the argument. *)
fun getIdentsTerm term =
    if kindIsId (getKind term)
    then IDS.singleton (getValue term)
    else foldr (fn (term, set) => IDS.union (getIdentsTerm term, set))
	       IDS.empty
	       (getChildren term)

(* Identifier set of a program *)
structure FREES = BinaryMapFn(type ord_key = string val compare = String.compare)

(*type frees = D.deps FREES.map*)

(* Extracts the set of free value identifiers occurring in the argument. *)
fun freeVId bounds term =
    let fun freeUnionChildren () =
	    foldr (fn (term, set) =>
		      FREES.unionWith D.union (freeVId bounds term, set))
		  FREES.empty
		  (getChildren term)

	fun freePatExp () =
	    let val (p, e)  = get2Children term
		val bounds' = IDS.union (bounds, getVIdentsTerm p)
	    in freeVId bounds' e
	    end

	fun freeBind bind bounds brec =
	    case getKind bind of
		(BIND, BIND_DEC) =>
		let val (fid, args, exp) = get3Children bind
		    val bounds0 = getVIdentsTerm fid
		    val bounds1 = IDS.union (bounds, getVIdentsTerm args)
		    val bounds2 = IDS.union (bounds, bounds0)
		    val frees   =
			if brec
			then freeVId (IDS.union (bounds1, bounds0)) exp
			else freeVId bounds1 exp
		in (bounds2, frees)
		end
	      | (BIND, BIND_TDEC) =>
		let val (fid, args, typ, exp) = get4Children bind
		    val bounds0 = getVIdentsTerm fid
		    val bounds1 = IDS.union (bounds, getVIdentsTerm args)
		    val bounds2 = IDS.union (bounds, bounds0)
		    val frees   =
			if brec
			then freeVId (IDS.union (bounds1, bounds0)) exp
			else freeVId bounds1 exp
		in (bounds2, frees)
		end
	      | (BIND, BIND_PAT) =>
		let val (pat, exp) = get2Children bind
		    val bounds0 = getVIdentsTerm pat
		    val bounds1 = IDS.union (bounds, bounds0)
		    val frees   =
			if brec
			then freeVId bounds1 exp
			else freeVId bounds  exp
		in (bounds1, frees)
		end
	      | (BIND, BIND_IOP) =>
		let val (fid, arg1, arg2, exp) = get4Children bind
		    val boundsa = IDS.union (getVIdentsTerm arg1, getVIdentsTerm arg2)
		    val bounds0 = getVIdentsTerm fid
		    val bounds1 = IDS.union (bounds, boundsa)
		    val bounds2 = IDS.union (bounds, bounds0)
		    val frees   =
			if brec
			then freeVId (IDS.union (bounds1, bounds0)) exp
			else freeVId bounds1 exp
		in (bounds1, frees)
		end
	      | (BIND, BIND_TIOP) =>
		let val (fid, arg1, arg2, typ, exp) = get5Children bind
		    val boundsa = IDS.union (getVIdentsTerm arg1, getVIdentsTerm arg2)
		    val bounds0 = getVIdentsTerm fid
		    val bounds1 = IDS.union (bounds, boundsa)
		    val bounds2 = IDS.union (bounds, bounds0)
		    val frees   =
			if brec
			then freeVId (IDS.union (bounds1, bounds0)) exp
			else freeVId bounds1 exp
		in (bounds1, frees)
		end
	      | _ => raise Fail wrongFormat

	fun freeDec dec bounds =
	    case getKind dec of
		(DEC, DEC_LET) =>
		let val bind = get1Children dec
		in freeBind bind bounds false
		end
	      | (DEC, DEC_CLASS) =>
		let val bind = get1Children dec
		in freeBind bind bounds false
		end
	      | (DEC, DEC_CLASSREC) =>
		let val (fid, arg, X, Y) = get4Children dec
		    val bounds' = IDS.union (bounds, getVIdentsTerm arg)
		    val freesX  = freeVId bounds' X
		    val freesY  = freeVId bounds' Y
		    val frees   = FREES.unionWith D.union (freesX, freesY)
		in (IDS.union (bounds, getVIdentsTerm fid), frees)
		end
	      | (DEC, DEC_LETR) =>
		let val bind = get1Children dec
		in freeBind bind bounds true
		end
	      | (DEC, DEC_CONS) =>
		let val (fid, args, typ) = get3Children dec
		in (IDS.union (bounds, getVIdentsTerm fid), FREES.empty)
		end
	      | (DEC, DEC_OCONS) =>
		let val (fid, args, typ, tyvar, tyseq) = get5Children dec
		in (IDS.union (bounds, getVIdentsTerm fid), FREES.empty)
		end
	      | (DEC, DEC_PSET) =>
		let val (fid, typ) = get2Children dec
		in (IDS.union (bounds, getVIdentsTerm fid), FREES.empty)
		end
	      | (DEC, DEC_PMAP) =>
		let val (fid, typ1, typ2) = get3Children dec
		in (IDS.union (bounds, getVIdentsTerm fid), FREES.empty)
		end
	      | (DEC, DEC_PARAM) =>
		let val (fid, typ) = get2Children dec
		in (IDS.union (bounds, getVIdentsTerm fid), FREES.empty)
		end
	      | (DEC, DEC_PARAMP) =>
		let val (fid, typ, prop) = get3Children dec
		    val bounds' = IDS.union (bounds, getVIdentsTerm fid)
		in (bounds', freeVId bounds' prop)
		end
	      | (DEC, DEC_TYPARAM)  => (bounds, FREES.empty)
	      | (DEC, DEC_TYPARAMP) => (bounds, FREES.empty)
	      | (DEC, DEC_ETPARAM)  => (bounds, FREES.empty)
	      | (DEC, DEC_TYCON)    => (bounds, FREES.empty)
	      | (DEC, DEC_EQTYCON)  => (bounds, FREES.empty)
	      | (DEC, DEC_TYFUN)    => (bounds, FREES.empty)
	      | (DEC, DEC_QMSG)    =>
		let val (id, atm, typ) = get3Children dec
		    val i = getIdIdent id
		    val set =
			case getValue dec of
			    "internal" => IDS.fromList [i ^ "'base", i ^ "'send", i ^ "'broadcast", i ^ "''send", i ^ "''broadcast"]
			  | "input"    => IDS.fromList [i ^ "'base"]
			  | "output"   => IDS.fromList [i ^ "'send", i ^ "'broadcast", i ^ "''send", i ^ "''broadcast"]
			  | _ => raise Fail ""
		in (IDS.union (bounds, set), FREES.empty)
		end
	      | (DEC, DEC_EQMSG)    =>
		let val (id, atm, typ) = get3Children dec
		    val i = getIdIdent id
		    val set =
			case getValue dec of
			    "internal" => IDS.fromList [i ^ "'base", i ^ "'send", i ^ "'broadcast", i ^ "''send", i ^ "''broadcast"]
			  | "input"    => IDS.fromList [i ^ "'base"]
			  | "output"   => IDS.fromList [i ^ "'send", i ^ "'broadcast", i ^ "''send", i ^ "''broadcast"]
			  | _ => raise Fail ""
		    val frees = freeVId bounds atm
		in (IDS.union (bounds, set), frees)
		end
	      | (DEC, DEC_PARSE)   => (bounds, FREES.empty)
	      | (DEC, DEC_TYPEOF)  => (bounds, FREES.empty)
	      | (DEC, DEC_INFIX)   =>
		let val fid = get1Children dec
		in (IDS.union (bounds, getVIdentsTerm fid), FREES.empty)
		end
	      | (DEC, DEC_MAIN)   =>
		let val exp = get1Children dec
		in (bounds, freeVId bounds exp)
		end
	      | (DEC, DEC_SPEC)   => (bounds, FREES.empty)
	      | (DEC, DEC_IMPORT) =>
		let val ids =
			foldr (fn (id, set) =>
				  IDS.union (set, getVIdentsTerm id))
			      bounds
			      (getChildren dec)
		in (ids, FREES.empty)
		end
	      | (DEC, DEC_TIMPORT) => (bounds, FREES.empty)
	      | (DEC, DEC_EXPORT) =>
		let val frees =
			foldr (fn (id, frees) =>
				  let val v = getValue id
				  in if IDS.member (bounds, v)
				     then frees
				     else FREES.insert (frees, v, D.mk_deps (getLabel id))
				  end)
			      FREES.empty
			      (getChildren dec)
		in (bounds, frees)
		end
	      | (DEC, DEC_INCLUDE) => (bounds, FREES.empty)
	      | (DEC, DEC_ASSUME) =>
		let val frees =
			foldr (fn (id, frees) =>
				  let val v = getValue id
				  in if IDS.member (bounds, v)
				     then frees
				     else FREES.insert (frees, v, D.mk_deps (getLabel id))
				  end)
			      FREES.empty
			      (getChildren dec)
		in (bounds, frees)
		end
	      | (DEC, DEC_GUARANT) =>
		let val frees =
			foldr (fn (id, frees) =>
				  let val v = getValue id
				  in if IDS.member (bounds, v)
				     then frees
				     else FREES.insert (frees, v, D.mk_deps (getLabel id))
				  end)
			      FREES.empty
			      (getChildren dec)
		in (bounds, frees)
		end
	      | (DEC, DEC_DOC)    => (bounds, FREES.empty)
	      | (DEC, DEC_INV)    =>
		let val (id, cls, params, args, prop) = get5Children dec
		    val bounds1 = IDS.union (bounds,  getVIdentsTerm params)
		    val bounds2 = IDS.union (bounds1, getVIdentsTerm args)
		    val frees   = freeVId bounds2 prop
		    val i       = getIdIdent cls
		    val frees1  = if IDS.member (bounds, i)
				  then frees
				  else FREES.insert (frees, i, D.mk_deps (getLabel dec))
		in (bounds, frees1)
		end
	      | (DEC, DEC_ORDER)  =>
		let val (id, cls, params, args1, args2, prop) = get6Children dec
		    val bounds1 = IDS.union (bounds,  getVIdentsTerm params)
		    val bounds2 = IDS.union (bounds1, getVIdentsTerm args1)
		    val bounds3 = IDS.union (bounds2, getVIdentsTerm args2)
		    val frees   = freeVId bounds2 prop
		    val i       = getIdIdent cls
		    val frees1  = if IDS.member (bounds, i)
				  then frees
				  else FREES.insert (frees, i, D.mk_deps (getLabel dec))
		in (bounds, frees1)
		end
	      | (DEC, DEC_PROGRESS) =>
		let val (id, cls, params, args1, args2, wcls, prop) = get7Children dec
		    val bounds1 = IDS.union (bounds,  getVIdentsTerm params)
		    val bounds2 = IDS.union (bounds1, getVIdentsTerm args1)
		    val bounds3 = IDS.union (bounds2, getVIdentsTerm args2)
		    val frees   = freeVId bounds3 prop
		    val i       = getIdIdent cls
		    val w       = getIdIdent wcls
		    val frees1  = if IDS.member (bounds, i)
				  then frees
				  else FREES.insert (frees, i, D.mk_deps (getLabel dec))
		    val frees2  = if IDS.member (bounds1, w)
				  then frees1
				  else FREES.insert (frees1, w, D.mk_deps (getLabel dec))
		in (bounds, frees2)
		end
	      | (DEC, DEC_STRICT) =>
		let val (id, cls, params, args1, args2, wpat, wcls, ppat, pprop, prop) = get10Children dec
		    val bounds1 = IDS.union (bounds,  getVIdentsTerm params)
		    val bounds2 = IDS.union (bounds1, getVIdentsTerm args1)
		    val bounds3 = IDS.union (bounds2, getVIdentsTerm args2)
		    val bounds4 = IDS.union (bounds1, getVIdentsTerm wpat)
		    val bounds5 = IDS.union (bounds4, getVIdentsTerm ppat)
		    val frees1  = freeVId bounds3 prop
		    val frees2  = freeVId bounds5 pprop
		    val i       = getIdIdent cls
		    val w       = getIdIdent wcls
		    val frees3  = FREES.unionWith D.union (frees1, frees2)
		    val frees4  = if IDS.member (bounds, i)
				  then frees3
				  else FREES.insert (frees3, i, D.mk_deps (getLabel dec))
		    val frees5  = if IDS.member (bounds1, w)
				  then frees4
				  else FREES.insert (frees4, w, D.mk_deps (getLabel dec))
		in (bounds, frees5)
		end
	      | (DEC, DEC_CONSIST) =>
		let val (id, cls, params, args1, args2, prop) = get6Children dec
		    val bounds1 = IDS.union (bounds,  getVIdentsTerm params)
		    val bounds2 = IDS.union (bounds1, getVIdentsTerm args1)
		    val bounds3 = IDS.union (bounds2, getVIdentsTerm args2)
		    val frees   = freeVId bounds2 prop
		    val i       = getIdIdent cls
		    val frees1  = if IDS.member (bounds, i)
				  then frees
				  else FREES.insert (frees, i, D.mk_deps (getLabel dec))
		in (bounds, frees1)
		end
	      | (DEC, DEC_MEMORY) =>
		let val (id, cls, params, args1, args2, wpat, wcls, prop) = get8Children dec
		    val bounds1 = IDS.union (bounds,  getVIdentsTerm params)
		    val bounds2 = IDS.union (bounds1, getVIdentsTerm args1)
		    val bounds3 = IDS.union (bounds2, getVIdentsTerm args2)
		    val bounds4 = IDS.union (bounds3, getVIdentsTerm wpat)
		    val frees1  = freeVId bounds4 prop
		    val i       = getIdIdent cls
		    val w       = getIdIdent wcls
		    val frees2  = if IDS.member (bounds, i)
				  then frees1
				  else FREES.insert (frees1, i, D.mk_deps (getLabel dec))
		    val frees3  = if IDS.member (bounds1, w)
				  then frees2
				  else FREES.insert (frees2, w, D.mk_deps (getLabel dec))
		in (bounds, frees3)
		end
	      | _ => raise Fail wrongFormat

	fun freeDecs () =
	    #2 (foldr (fn (dec, (bounds, frees)) =>
			  let val (bounds', frees') = freeDec dec bounds
			      val frees0 = FREES.unionWith D.union (frees, frees')
			  in (bounds', frees0)
			  end)
		      (bounds, FREES.empty)
		      (getChildren term))

    in case getKind term of
	   (SCON,       _)             => FREES.empty
	 | (ID,        ID_VID)         =>
	   let val v = getValue term
	   in if IDS.member (bounds, v)
	      then FREES.empty
	      else FREES.singleton (v, D.mk_deps (getLabel term))
	   end
	 | (ID,         _)             => FREES.empty
	 | (IDENTS,     _)             => FREES.empty
	 | (TYPE,       _)             => FREES.empty
	 | (PAT,        _)             => FREES.empty
	 | (MRULE,      MRULE_M)       => freePatExp ()
	 | (MATCH,      MATCH_M)       => freeUnionChildren ()
	 | (EXP,        EXP_LAMBDA)    => freePatExp ()
	 | (EXP,        EXP_LET)       =>
	   let val (b, e) = get2Children term
	       val (bounds', frees) = freeBind b bounds false
	   in FREES.unionWith D.union (frees, freeVId bounds' e)
	   end
	 | (EXP,        EXP_CLASS)       =>
	   let val (b, e) = get2Children term
	       val (bounds', frees) = freeBind b bounds false
	   in FREES.unionWith D.union (frees, freeVId bounds' e)
	   end
	 | (EXP,        EXP_LETR)      =>
	   let val (b, e) = get2Children term
	       val (bounds', frees) = freeBind b bounds true
	   in FREES.unionWith D.union (frees, freeVId bounds' e)
	   end
	 | (EXP,        EXP_WHERE)     =>
	   let val (e, b) = get2Children term
	       val (bounds', frees) = freeBind b bounds false
	   in FREES.unionWith D.union (frees, freeVId bounds' e)
	   end
	 | (EXP,        EXP_BINDING)   =>
	   let val (e1, p, e2) = get3Children term
	       val bounds' = IDS.union (bounds, getVIdentsTerm p)
	       val map1    = freeVId bounds  e1
	       val map2    = freeVId bounds' e2
	   in FREES.unionWith D.union (map1, map2)
	   end
	 | (EXP,        _)             => freeUnionChildren ()
	 | (ATEXP,      _)             => freeUnionChildren ()
	 | (CASEPAT,    _)             => FREES.empty
	 | (ATOMS,      _)             => FREES.empty
	 | (DEC,        _)             => #2 (freeDec term bounds)
	 | (TYPEVARSEQ, _)             => FREES.empty
	 | (TYPESEQ,    _)             => FREES.empty
	 | (OTYPESEQ,   _)             => FREES.empty
	 | (TYPESEQSET, _)             => FREES.empty
	 | (BIND,       _)             => #2 (freeBind term bounds false) (* NOTE(2011-05-27): The choice of `false' is rather arbitrary *)
	 | (PARAM,      _)             => FREES.empty
	 | (ARG,        _)             => FREES.empty
	 | (ARGS,       _)             => FREES.empty
	 | (PROG,       _)             => freeDecs ()
	 | (DOTS,       _)             => freeUnionChildren ()
	 | (FILE,       _)             => freeUnionChildren ()
	 | _                           => raise Fail wrongFormat
    end

fun isFreeVid id term = FREES.find (freeVId IDS.empty term, id)


(* ------ SIMPLIFICATION ------ *)

(* We use that to rename identifiers. *)
structure REN = BinaryMapFn(type ord_key = string val compare = String.compare)

fun filter_ren ren idents = REN.filteri (fn (id, _) => not (IDS.member (idents, id))) ren

(* We use that to substitute identifier by expressions. *)
structure SUB = BinaryMapFn(type ord_key = string val compare = String.compare)

type sub   = term SUB.map
type subLL = sub list list

fun unionSUB (sub1, sub2) =
    SUB.unionWith (fn _ => raise Fail "an identifier should not occur twice in a pattern")
		  (sub1, sub2)

(* unionSUB' compared to unionSUB is used in for declarations sequences
 * where we allow a function to be rebound.  We then only keep the second
 * substitution which must be for the last declaration. *)
fun unionSUB' (sub1, sub2) = SUB.unionWith (fn (_, b) => b) (sub1, sub2)

(* This set is used to record the defined parameters *)
structure PARAMS = BinarySetFn(type ord_key = string val compare = String.compare)

val params = ref PARAMS.empty
fun addParam param = params := PARAMS.add (!params, param)
fun isParam  param = PARAMS.member (!params, param)
fun resetParams () = params := PARAMS.empty

datatype alpha = NEXT of string * (unit -> alpha)

fun dummy_stream () : alpha = raise Fail "the id stream is not set yet"

(* Identifier set of a piece of code.  We set and unset it. *)
val ids = ref IDS.empty
(*val new_id_base = ref ""*)
val stream = ref dummy_stream

fun updIds id  = ids := IDS.add (!ids, id)
fun setIds set = ids := set
fun getIds ()  = !ids

fun setStream strm = stream := strm
fun getStream () = !stream

(*fun setNewIdBase base = new_id_base := base
fun getNewIdBase ()   = !new_id_base*)

fun resetIds () =
    (ids    := IDS.empty;
     stream := dummy_stream;
     (*new_id_base     := "";*)
     ())

fun resetSimp () =
    (resetIds    ();
     resetParams ();
     ())

val lstAlpha =
    ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
     "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

fun streamId [] pref () = streamId lstAlpha (pref ^ "z") ()
  | streamId (x :: xs) pref () =
    let val f = streamId xs pref
    in NEXT (pref ^ x, f)
    end

fun newId () =
    let val ids = getIds ()
	val id  = ref ""
	val _   =
	    while !id = "" do
	    let val NEXT (id0, strm) = getStream () ()
		val _ = setStream strm
	    in if IDS.member (ids, id0)
	       then ()
	       else id := id0
	    end
	val _   = updIds (!id)
    in !id
    end

fun newIdSet ids =
    let val id  = ref ""
	val _   =
	    while !id = "" do
	    let val NEXT (id0, strm) = getStream () ()
		val _ = setStream strm
	    in if IDS.member (ids, id0)
	       then ()
	       else id := id0
	    end
	val _   = updIds (!id)
    in !id
    end
(*fun newIdSet set = newId ()*)

fun newIdId id =
    if IDS.member (getIds (), id)
    then newId ()
    else (updIds id; id)

fun newIdIdSet set id =
    if IDS.member (set, id)
    then newIdSet set
    else (updIds id; id)

(*fun isNewId string =
    let val base = getNewIdBase ()
    in if base = ""
       then raise Fail "the new_id_base is not set"
       else String.isPrefix base string
    end*)

(*fun newIdBase () =
    let val init_ident = "a_g_" (* stands for Autmatically Generated *)
	val n = ref 1
	val _ =
	    let val pref = init_ident ^ Int.toString (!n) ^ "_"
	    in if IDS.exists (fn st => String.isPrefix pref st) (getIds ())
	       then n := !n + 1
	       else ()
	    end
    in init_ident ^ Int.toString (!n) ^ "_"
    end*)

fun setIdsTerm term =
    let val _ = setIds (getVIdentsTerm term)
	val _ = setStream (streamId lstAlpha "z")
    in ()
    end

(* extracts the identifiers declared at binding positions in binds. *)
fun getIdentsBind (N {kind = (BIND, BIND_DEC),  label, value, regions, parents, children = [f, a, e]})         = getVIdentsTerm f
  | getIdentsBind (N {kind = (BIND, BIND_TDEC), label, value, regions, parents, children = [f, a, t, e]})      = getVIdentsTerm f
  | getIdentsBind (N {kind = (BIND, BIND_PAT),  label, value, regions, parents, children = [p, e]})            = getVIdentsTerm p
  | getIdentsBind (N {kind = (BIND, BIND_IOP),  label, value, regions, parents, children = [i, a1, a2, e]})    = getVIdentsTerm i
  | getIdentsBind (N {kind = (BIND, BIND_TIOP), label, value, regions, parents, children = [i, a1, a2, t, e]}) = getVIdentsTerm i
  | getIdentsBind term = (print (toString term); raise Fail unexpectedFormat)

(* extracts the identifiers declared at binding positions in decs. *)
fun getIdentsDec (dec as N {kind = (DEC, DEC_LET),      label, value, regions, parents, children = [b]})             = getIdentsBind b
  | getIdentsDec (dec as N {kind = (DEC, DEC_CLASS),    label, value, regions, parents, children = [b]})             = getIdentsBind b
  | getIdentsDec (dec as N {kind = (DEC, DEC_CLASSREC), label, value, regions, parents, children = [i, a, x, y]})    = getVIdentsTerm i
  | getIdentsDec (dec as N {kind = (DEC, DEC_LETR),     label, value, regions, parents, children = [b]})             = getIdentsBind b
  | getIdentsDec (dec as N {kind = (DEC, DEC_CONS),     label, value, regions, parents, children = [i, a, t]})       = getVIdentsTerm i
  | getIdentsDec (dec as N {kind = (DEC, DEC_OCONS),    label, value, regions, parents, children = [i, a, t, v, s]}) = getVIdentsTerm i
  | getIdentsDec (dec as N {kind = (DEC, DEC_PSET),     label, value, regions, parents, children = [i, t]})          = getVIdentsTerm i
  | getIdentsDec (dec as N {kind = (DEC, DEC_PMAP),     label, value, regions, parents, children = [i, t, u]})       = getVIdentsTerm i
  | getIdentsDec (dec as N {kind = (DEC, DEC_PARAM),    label, value, regions, parents, children = [i, t]})          = getVIdentsTerm i
  | getIdentsDec (dec as N {kind = (DEC, DEC_PARAMP),   label, value, regions, parents, children = [i, t, p]})       = getVIdentsTerm i
  | getIdentsDec (dec as N {kind = (DEC, DEC_TYPARAM),  label, value, regions, parents, children = [s, tc, ty]})     = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_TYPARAMP), label, value, regions, parents, children = [s, tc, ty, p]})  = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_ETPARAM),  label, value, regions, parents, children = [s, t, i]})       = getVIdentsTerm i
  | getIdentsDec (dec as N {kind = (DEC, DEC_TYCON),    label, value, regions, parents, children = [s, tc, ty]})     = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_EQTYCON),  label, value, regions, parents, children = [s, tc, i, ty]})  = getVIdentsTerm i
  | getIdentsDec (dec as N {kind = (DEC, DEC_TYFUN),    label, value, regions, parents, children = [s, tc, ty]})     = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_QMSG), label, value, regions, parents, children = [i, a, t]})       =
    let val i = getIdIdent i
    in case getValue dec of
	   "internal" => IDS.fromList [i ^ "'base", i ^ "'send", i ^ "'broadcast", i ^ "''send", i ^ "''broadcast"]
	 | "input"    => IDS.fromList [i ^ "'base"]
	 | "output"   => IDS.fromList [i ^ "'send", i ^ "'broadcast", i ^ "''send", i ^ "''broadcast"]
	 | _ => raise Fail ""
    end
  | getIdentsDec (dec as N {kind = (DEC, DEC_EQMSG), label, value, regions, parents, children = [i, a, t]})       =
    let val i = getIdIdent i
    in case getValue dec of
	   "internal" => IDS.fromList [i ^ "'base", i ^ "'send", i ^ "'broadcast", i ^ "''send", i ^ "''broadcast"]
	 | "input"    => IDS.fromList [i ^ "'base"]
	 | "output"   => IDS.fromList [i ^ "'send", i ^ "'broadcast", i ^ "''send", i ^ "''broadcast"]
	 | _ => raise Fail ""
    end
  | getIdentsDec (dec as N {kind = (DEC, DEC_PARSE),    label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_TYPEOF),   label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_INFIX),    label, value, regions, parents, children = [i]})             = getVIdentsTerm i
  | getIdentsDec (dec as N {kind = (DEC, DEC_MAIN),     label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_SPEC),     label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_IMPORT),   label, value, regions, parents, children})                   = foldr (fn (id, set) => IDS.union (set, getVIdentsTerm id)) IDS.empty children
  | getIdentsDec (dec as N {kind = (DEC, DEC_TIMPORT),  label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_EXPORT),   label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_INCLUDE),  label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_ASSUME),   label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_GUARANT),  label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_DOC),      label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_INV),      label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_ORDER),    label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_PROGRESS), label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_STRICT),   label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_CONSIST),  label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec (dec as N {kind = (DEC, DEC_MEMORY),   label, value, regions, parents, children})                   = IDS.empty
  | getIdentsDec _ = raise Fail unexpectedFormat

fun getIdentsFile (N {kind = (FILE, FILE_F),  label, value, regions, parents, children}) =
    foldr (fn (dec, set) => IDS.union (getIdentsDec dec, set))
	  IDS.empty
	  children
  | getIdentsFile _ = raise Fail unexpectedFormat

fun transformToConsPat pats =
    let fun transformToCons' [] = idToDumPat id_list_nil
	  | transformToCons' (x :: xs) =
	    let val t = transformToCons' xs
	    in mk_new_dum_term PAT_CONS id_list_cons [] [x, t]
	    end
	val pat = transformToCons' pats
    (* NOTE: next one is nonsense because we don't have parentheses
     * but it's the only solution to bring down a pat to a atpat. *)
    in mk_new_dum_term ATPAT_PAREN "" [] [pat]
    end

fun transformToConsExp exps =
    let fun transformToCons' [] = idToDumExp id_list_nil
	  | transformToCons' (x :: xs) =
	    let val t = transformToCons' xs
	    in mk_new_dum_term EXP_OP id_list_cons [] [x, t]
	    end
	val exp = transformToCons' exps
    (* NOTE: same remark as in the pattern function. *)
    in mk_new_dum_term ATEXP_PAREN "" [] [exp]
    end

fun transformToPairs' []        _    = raise Fail "a tuple should at least be a pair"
  | transformToPairs' [_]       _    = raise Fail "a tuple should at least be a pair"
  | transformToPairs' [x, y]    cons = cons [x, y]
  | transformToPairs' (x :: xs) cons = cons [x, transformToPairs' xs cons]

fun transformToPairs []        _    = raise Fail "a tuple should at least be a pair"
  | transformToPairs [_]       _    = raise Fail "a tuple should at least be a pair"
  | transformToPairs [x, y]    _    = [x, y]
  | transformToPairs (x :: xs) cons = [x, transformToPairs' xs cons]

fun transformToSeven pats cons =
    case pats of
	(x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: xs) =>
	[x1, x2, x3, x4, x5, x6, cons (transformToSeven (x7 :: x8 :: xs) cons)]
      | _ => pats

fun fo_subst sub (term as N {kind = (ATEXP, ATEXP_ID), label, value, regions, parents, children = [id]}) =
    if kindIsIdVid (getKind id)
    then let val ident = getValue id
	 in case SUB.find (sub, ident) of
		NONE => term
	      | SOME exp => mk_new_dum_term ATEXP_PAREN "" [] [exp]
	 end
    else raise Fail wrongFormat
  | fo_subst sub (term as N {kind = (MRULE, MRULE_M), label, value, regions, parents, children = [pat, exp]}) =
    let val idents = getVIdentsTerm pat
	val sub'   = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
    in updChildren term [pat, fo_subst sub' exp]
    end
  | fo_subst sub (term as N {kind = (MRULE, MRULE_B), label, value, regions, parents, children = [bpat, exp]}) = raise Fail "case branches should be dealt with when dealing with case expressions"
    (*let val idents = getVIdentsTerm pat
	val sub'   = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
    in updChildren term [pat, fo_subst sub' exp]
    end*)
  | fo_subst sub (term as N {kind = (EXP, EXP_LAMBDA), label, value, regions, parents, children = [pat, exp]}) =
    let val idents = getVIdentsTerm pat
	val sub'   = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
    in updChildren term [pat, fo_subst sub' exp]
    end
  | fo_subst sub (term as N {kind = (EXP, EXP_LET), label, value, regions, parents, children = [bind, exp]}) =
    let val idents = getIdentsBind bind
	val sub'   = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
    in updChildren term [fo_subst sub bind, fo_subst sub' exp]
    end
  | fo_subst sub (term as N {kind = (EXP, EXP_CLASS), label, value, regions, parents, children = [bind, exp]}) =
    let val idents = getIdentsBind bind
	val sub'   = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
    in updChildren term [fo_subst sub bind, fo_subst sub' exp]
    end
  | fo_subst sub (term as N {kind = (EXP, EXP_LETR), label, value, regions, parents, children = [bind, exp]}) =
    let val idents = getIdentsBind bind
	val sub'   = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
    in updChildren term [fo_subst sub' bind, fo_subst sub' exp]
    end
  | fo_subst sub (term as N {kind = (EXP, EXP_WHERE), label, value, regions, parents, children = [exp, bind]}) =
    let val idents = getIdentsBind bind
	val sub'   = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
    in updChildren term [fo_subst sub bind, fo_subst sub' exp]
    end
  | fo_subst sub (term as N {kind = (EXP, EXP_BINDING), label, value, regions, parents, children = [exp1, pat, exp2]}) =
    let val idents = getVIdentsTerm pat
	val sub'   = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
    in updChildren term [fo_subst sub exp1, pat, fo_subst sub' exp2]
    end
  | fo_subst sub (term as N {kind = (DEC, DEC_LETR), label, value, regions, parents, children = [bind]}) =
    let	val idents = getIdentsBind bind
	val sub'   = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
    in updChildren term [fo_subst sub' bind]
    end
  | fo_subst sub (term as N {kind = (BIND, BIND_DEC), label, value, regions, parents, children = [f, a, e]}) =
    let val idents = getVIdentsTerm a
	val sub'   = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
    in updChildren term [f, a, fo_subst sub' e]
    end
  | fo_subst sub (term as N {kind = (BIND, BIND_TDEC), label, value, regions, parents, children = [f, a, t, e]}) =
    let val idents = getVIdentsTerm a
	val sub'   = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
    in updChildren term [f, a, t, fo_subst sub' e]
    end
  | fo_subst sub (term as N {kind = (BIND, BIND_IOP), label, value, regions, parents, children = [id, a1, a2, e]}) =
    let val idents1 = getVIdentsTerm a1
	val idents2 = getVIdentsTerm a2
	val idents  = IDS.union (idents1, idents2)
	val sub'    = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
    in updChildren term [id, a1, a2, fo_subst sub' e]
    end
  | fo_subst sub (term as N {kind = (BIND, BIND_TIOP), label, value, regions, parents, children = [id, a1, a2, t, e]}) =
    let val idents1 = getVIdentsTerm a1
	val idents2 = getVIdentsTerm a2
	val idents  = IDS.union (idents1, idents2)
	val sub'    = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
    in updChildren term [id, a1, a2, t, fo_subst sub' e]
    end
  | fo_subst sub (term as N {kind = (FILE, FILE_F), label, value, regions, parents, children}) =
    let val (_, children') =
	    foldl (fn (dec, (sub, list)) =>
		      let val dec'   = fo_subst sub dec
			  val idents = getIdentsDec dec
			  val sub'   = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
		      in (sub', list @ [dec'])
		      end)
		  (sub, [])
		  children
    in updChildren term children'
    end
  | fo_subst sub (term as N {kind = (PROG, PROG_P), label, value, regions, parents, children}) =
    let val (_, children') =
	    foldl (fn (file, (sub, list)) =>
		      let val file'  = fo_subst sub file
			  val idents = getIdentsFile file
			  val sub'   = SUB.filteri (fn (id, exp) => not (IDS.member (idents, id))) sub
		      in (sub', list @ [file'])
		      end)
		  (sub, [])
		  children
    in updChildren term children'
    end
  | fo_subst sub term = updChildren term (map (fo_subst sub) (getChildren term))

fun fo_ren ren (term as N {kind = (ID, ID_VID), label, value, regions, parents, children = []}) =
    (case REN.find (ren, value) of
	 NONE => term
       | SOME id => updValue term id)
  | fo_ren ren (term as N {kind = (ATEXP, ATEXP_ID), label, value, regions, parents, children = [id]}) =
    if kindIsIdVid (getKind id)
    then let val ident = getValue id
	 in case REN.find (ren, ident) of
		NONE => term
	      | SOME i => updChildren term [updValue id i]
	 end
    else raise Fail wrongFormat
  | fo_ren ren (term as N {kind = (MRULE, MRULE_M), label, value, regions, parents, children = [pat, exp]}) =
    let val idents = getVIdentsTerm pat
	val ren'   = filter_ren ren idents
    in updChildren term [pat, fo_ren ren' exp]
    end
  | fo_ren ren (term as N {kind = (MRULE, MRULE_B), label, value, regions, parents, children = [bpat, exp]}) = raise Fail "case branches should be dealt with when dealing with case expressions"
  | fo_ren ren (term as N {kind = (EXP, EXP_LAMBDA), label, value, regions, parents, children = [pat, exp]}) =
    let val idents = getVIdentsTerm pat
	val ren'   = filter_ren ren idents
    in updChildren term [pat, fo_ren ren' exp]
    end
  | fo_ren ren (term as N {kind = (EXP, EXP_LET), label, value, regions, parents, children = [bind, exp]}) =
    let val idents = getIdentsBind bind
	val ren'   = filter_ren ren idents
    in updChildren term [fo_ren ren bind, fo_ren ren' exp]
    end
  | fo_ren ren (term as N {kind = (EXP, EXP_CLASS), label, value, regions, parents, children = [bind, exp]}) =
    let val idents = getIdentsBind bind
	val ren'   = filter_ren ren idents
    in updChildren term [fo_ren ren bind, fo_ren ren' exp]
    end
  | fo_ren ren (term as N {kind = (EXP, EXP_LETR), label, value, regions, parents, children = [bind, exp]}) =
    let val idents = getIdentsBind bind
	val ren'   = filter_ren ren idents
    in updChildren term [fo_ren ren' bind, fo_ren ren' exp]
    end
  | fo_ren ren (term as N {kind = (EXP, EXP_WHERE), label, value, regions, parents, children = [exp, bind]}) =
    let val idents = getIdentsBind bind
	val ren'   = filter_ren ren idents
    in updChildren term [fo_ren ren bind, fo_ren ren' exp]
    end
  | fo_ren ren (term as N {kind = (EXP, EXP_BINDING), label, value, regions, parents, children = [exp1, pat, exp2]}) =
    let val idents = getVIdentsTerm pat
	val ren'   = filter_ren ren idents
    in updChildren term [fo_ren ren exp1, pat, fo_ren ren' exp2]
    end
  | fo_ren ren (term as N {kind = (DEC, DEC_LETR), label, value, regions, parents, children = [bind]}) =
    let	val idents = getIdentsBind bind
	val ren'   = filter_ren ren idents
    in updChildren term [fo_ren ren' bind]
    end
  | fo_ren ren (term as N {kind = (BIND, BIND_DEC), label, value, regions, parents, children = [f, a, e]}) =
    let val idents = getVIdentsTerm a
	val ren'   = filter_ren ren idents
    in updChildren term [f, a, fo_ren ren' e]
    end
  | fo_ren ren (term as N {kind = (BIND, BIND_TDEC), label, value, regions, parents, children = [f, a, t, e]}) =
    let val idents = getVIdentsTerm a
	val ren'   = filter_ren ren idents
    in updChildren term [f, a, t, fo_ren ren' e]
    end
  | fo_ren ren (term as N {kind = (BIND, BIND_IOP), label, value, regions, parents, children = [id, a1, a2, e]}) =
    let val idents1 = getVIdentsTerm a1
	val idents2 = getVIdentsTerm a2
	val idents  = IDS.union (idents1, idents2)
	val ren'    = filter_ren ren idents
    in updChildren term [id, a1, a2, fo_ren ren' e]
    end
  | fo_ren ren (term as N {kind = (BIND, BIND_TIOP), label, value, regions, parents, children = [id, a1, a2, t, e]}) =
    let val idents1 = getVIdentsTerm a1
	val idents2 = getVIdentsTerm a2
	val idents  = IDS.union (idents1, idents2)
	val ren'    = filter_ren ren idents
    in updChildren term [id, a1, a2, t, fo_ren ren' e]
    end
  | fo_ren ren (term as N {kind = (FILE, FILE_F), label, value, regions, parents, children}) =
    let val (_, children') =
	    foldl (fn (dec, (ren, list)) =>
		      let val dec'   = fo_ren ren dec
			  val idents = getIdentsDec dec
			  val ren'   = filter_ren ren idents
		      in (ren', list @ [dec'])
		      end)
		  (ren, [])
		  children
    in updChildren term children'
    end
  | fo_ren ren (term as N {kind = (PROG, PROG_P), label, value, regions, parents, children}) =
    let val (_, children') =
	    foldl (fn (file, (ren, list)) =>
		      let val file'  = fo_ren ren file
			  val idents = getIdentsFile file
			  val ren'   = filter_ren ren idents
		      in (ren', list @ [file'])
		      end)
		  (ren, [])
		  children
    in updChildren term children'
    end
  | fo_ren ren term = updChildren term (map (fo_ren ren) (getChildren term))

fun selectEndTuple6 list =
    if List.length list <= 6
    then (list, NONE)
    else let val firsts = List.rev (List.take (list, 6))
	     val lasts  = List.rev (List.drop (list, 6))
	 in (firsts, SOME lasts)
	 end

(* let pat = exp1 in exp2 *)
fun mvPat2Exp (pat as N {kind = (ATPAT, ATPAT_ID),    label, value, regions, parents, children = [id]})  exp1 exp2 =
    let val bind  = mk_new_dum_term BIND_PAT "" [] [idToDumPat (getIdIdent id), exp1]
    in mk_new_dum_term EXP_LET "" [] [bind, exp2]
    end
  | mvPat2Exp (pat as N {kind = (ATPAT, ATPAT_WILD),  label, value, regions, parents, children = []})       exp1 exp2 = exp2
  | mvPat2Exp (pat as N {kind = (ATPAT, ATPAT_SCON),  label, value, regions, parents, children = []})       exp1 exp2 = raise Fail ""
  | mvPat2Exp (pat as N {kind = (ATPAT, ATPAT_LIST),  label, value, regions, parents, children})            exp1 exp2 = raise Fail ""
  | mvPat2Exp (pat as N {kind = (ATPAT, ATPAT_PAREN), label, value, regions, parents, children = [pat']})   exp1 exp2 = mvPat2Exp pat' exp1 exp2
  | mvPat2Exp (pat as N {kind = (ATPAT, ATPAT_STRUC), label, value, regions, parents, children = [ident]})  exp1 exp2 = raise Fail ""
  | mvPat2Exp (pat as N {kind = (ATPAT, ATPAT_TUPLE), label, value, regions, parents, children = []})       exp1 exp2 = exp2 (*raise Fail ""*)
  | mvPat2Exp (pat as N {kind = (ATPAT, ATPAT_TUPLE), label, value, regions, parents, children})            exp1 exp2 =
    let val (list, listop) = selectEndTuple6 children
	fun newExpLet listPat exp2 =
	    let val (lst, exp') =
		    foldr (fn (pat, (list, exp)) =>
			      let val set =
				      foldr (fn (id, set) => IDS.add (set, id))
					    (getVIdentsTerm exp)
					    list
				  val (pat', exp') =
				      case getIdPat pat of
					  SOME ident =>
					  let val i = getIdIdent ident
					  in if isParam i
					     then let val id  = newIdIdSet set "z"
						      val sub = SUB.singleton (i, idToDumExp id)
						  in (id, fo_subst sub exp)
						  end
					     else (i, exp)
					  end
					| NONE =>
					  let val id   = newIdIdSet set "z"
					      val exp' = mvPat2Exp pat (idToDumExp id) exp
					  in (id, exp')
					  end
			      in (pat' :: list, exp')
			      end)
			  ([], exp2)
			  listPat
		val pats  = map idToDumPat lst
		val atpat = mk_new_dum_term ATPAT_TUPLE "" [] pats
		val pat   = mk_new_dum_term PAT_ATPAT "" [] [atpat]
		val bind  = mk_new_dum_term BIND_PAT "" [] [pat, exp1]
	    in mk_new_dum_term EXP_LET "" [] [bind, exp']
	    end
    in case listop of
	   NONE => newExpLet list exp2
	 | SOME [p] => newExpLet (list @ [p]) exp2
	 | SOME list2 =>
	   let val ids   = IDS.union (getVIdentsTerm pat, getVIdentsTerm exp2)
	       val id    = newIdIdSet ids "z"
	       val atpat = mk_new_dum_term ATPAT_TUPLE "" [] list2
	       val exp'  = mvPat2Exp atpat (idToDumExp id) exp2
	   in newExpLet (list @ [idToPat id]) exp'
	   end
    end
  | mvPat2Exp (pat as N {kind = (PAT, PAT_TYPED), label, value, regions, parents, children})                exp1 exp2 = (print (toString pat ^ "\n"); raise Fail "we should have get rid of all explicit type constraints")
  | mvPat2Exp (pat as N {kind = (PAT, PAT_AS),    label, value, regions, parents, children = [ident, p]})   exp1 exp2 =
    (* let (id as pat) = exp1 in exp2
     *   ->
     * let id = exp1 in (let pat = id in exp2) *)
    let val (p', e1, e2) =
	    let val id = getIdIdent ident
	    in if isParam id
	       then let val id0 = newIdIdSet (getVIdentsTerm exp2) "z"
			val sub = SUB.singleton (id, idToDumExp id0)
		    in (idToDumPat id0, idToDumExp id0, fo_subst sub exp2)
		    end
	       else (idToDumPat id, idToDumExp id, exp2)
	    end
	val exp2' = mvPat2Exp p e1 e2
	val bind  = mk_new_dum_term BIND_PAT "" [] [p', exp1]
    in mk_new_dum_term EXP_LET "" [] [bind, exp2']
    end
  | mvPat2Exp (pat as N {kind = (PAT, PAT_CONS),  label, value, regions, parents, children = [pat1, pat2]}) exp1 exp2 = raise Fail ""
  | mvPat2Exp (pat as N {kind = (PAT, PAT_ATPAT), label, value, regions, parents, children = [atpat]})      exp1 exp2 = mvPat2Exp atpat exp1 exp2
  | mvPat2Exp (pat as N {kind = (PAT, PAT_APP),   label, value, regions, parents, children})                exp1 exp2 = raise Fail ""
  | mvPat2Exp pat exp1 exp2 = (print (toString pat ^ "\n"); raise Fail "term is not a pattern")

fun generateLambdas params exp =
    let fun replaceIfParam ident exp set =
	    let val id = getIdIdent ident
	    in if isParam id
	       then let val id0 = newIdIdSet set "z"
			val sub = SUB.singleton (id, idToDumExp id0)
		    in (id0, fo_subst sub exp)
		    end
	       else (id, exp)
	    end
	fun toLam [] = ([], exp)
	  | toLam (pat :: atpats) =
	    let val (vars,exp) = toLam atpats
		val set = IDS.addList (getVIdentsTerm exp, vars)
		val (fid,exp') =
		    case getIdPat pat of
			SOME ident => replaceIfParam ident exp set
		      | NONE =>
			let val ((id,exp'),pat') =
				case getAsPat pat of
				    SOME (ident,pat') => (replaceIfParam ident exp set,pat')
				  | NONE => ((newIdIdSet set "z", exp), pat)
			in (id, mvPat2Exp pat' (idToDumExp id) exp')
			end
	    in (fid :: vars, exp')
	    end
	val (vars, exp') =
	    case params of
		N {kind = (PARAM, PARAM_P), label, value, regions, parents, children} => toLam children
	      | N {kind = (PAT,   _),       label, value, regions, parents, children} => toLam [params]
	      | N {kind = (ATPAT, _),       label, value, regions, parents, children} => toLam [params]
	      | _ => raise Fail "unexpected term"
    in foldr (fn (var, exp) => mk_new_dum_term EXP_LAMBDA "" [] [idToPat var, exp])
	     exp'
	     vars
    end

(*fun patToExp (N {kind = (ATPAT, ATPAT_ID),    label, value, regions, parents, children = [id]})         = mk_new_dum_term ATEXP_ID    "" [] [id]
  | patToExp (N {kind = (ATPAT, ATPAT_WILD),  label, value, regions, parents, children})                = raise Fail "wildcards should have been replaced"
  | patToExp (N {kind = (ATPAT, ATPAT_SCON),  label, value, regions, parents, children = [sc]})         = mk_new_dum_term ATEXP_SCON  "" [] [sc]
  | patToExp (N {kind = (ATPAT, ATPAT_LIST),  label, value, regions, parents, children})                = mk_new_dum_term ATEXP_LIST  "" [] (map patToExp children)
  | patToExp (N {kind = (ATPAT, ATPAT_PAREN), label, value, regions, parents, children = [pat]})        = mk_new_dum_term ATEXP_PAREN "" [] [patToExp pat]
  | patToExp (N {kind = (ATPAT, ATPAT_STRUC), label, value, regions, parents, children = [id]})         = mk_new_dum_term ATEXP_ID    "" [] [id]
  | patToExp (N {kind = (ATPAT, ATPAT_TUPLE), label, value, regions, parents, children})                = mk_new_dum_term ATEXP_TUPLE "" [] (map patToExp children)
  | patToExp (N {kind = (PAT,   PAT_TYPED),   label, value, regions, parents, children = [pat, typ]})   = mk_new_dum_term EXP_TYPED   "" [] [patToExp pat, typ]
  | patToExp (N {kind = (PAT,   PAT_CONS),    label, value, regions, parents, children = [pat1, pat2]}) = mk_new_dum_term EXP_OP   value [] [patToExp pat1, patToExp pat2]
  | patToExp (N {kind = (PAT,   PAT_ATPAT),   label, value, regions, parents, children = [atpat]})      = mk_new_dum_term EXP_ATEXP   "" [] [patToExp atpat]
  | patToExp (N {kind = (PAT,   PAT_APP),     label, value, regions, parents, children = [id, atpat]})  = mk_new_dum_term EXP_APP     "" [] [id, patToExp atpat]
  | patToExp term = raise Fail "the term is not a pattern"*)

fun isIdListNil (N {kind = (ID, ID_VID), label, value, regions, parents, children = []}) = value = id_list_nil
  | isIdListNil _ = false

fun isIdVar (N {kind = (ID, ID_VID), label, value, regions, parents, children = []}) =
    if List.exists (fn x => x = value) [id_list_nil, id_bool_true, id_bool_false, id_bool_not]
    then NONE
    else SOME value
  | isIdVar _ = NONE

fun isPatListNil (N {kind = (PAT,   PAT_ATPAT),   label, value, regions, parents, children = [atpat]}) = isPatListNil atpat
  | isPatListNil (N {kind = (ATPAT, ATPAT_ID),    label, value, regions, parents, children = [id]})    = isIdListNil id
  | isPatListNil (N {kind = (ATPAT, ATPAT_LIST),  label, value, regions, parents, children = []})      = true
  | isPatListNil (N {kind = (ATPAT, ATPAT_PAREN), label, value, regions, parents, children = [pat]})   = isPatListNil pat
  | isPatListNil _ = false

fun isPatVar (N {kind = (PAT,   PAT_ATPAT),   label, value, regions, parents, children = [atpat]}) = isPatVar atpat
  | isPatVar (N {kind = (ATPAT, ATPAT_ID),    label, value, regions, parents, children = [id]})    = isIdVar id
  | isPatVar (N {kind = (ATPAT, ATPAT_PAREN), label, value, regions, parents, children = [pat]})   = isPatVar pat
  | isPatVar _ = NONE

fun isPatListCons (N {kind = (PAT, PAT_CONS), label, value, regions, parents, children = [pat1, pat2]}) =
    (case (isPatVar pat1, isPatVar pat2) of
	 (SOME id1, SOME id2) => SOME (id1, id2)
       | _ => NONE)
  | isPatListCons _ = NONE

fun isCasePatListNil (N {kind = (CASEPAT, CASEPAT_PAT), label, value, regions, parents, children = [pat]}) = isPatListNil pat
  | isCasePatListNil _ = false

fun isCasePatListCons (N {kind = (CASEPAT, CASEPAT_PAT), label, value, regions, parents, children = [pat]}) = isPatListCons pat
  | isCasePatListCons _ = NONE

fun isMRuleListNil (N {kind = (MRULE, MRULE_M), label, value, regions, parents, children = [casepat, exp]}) =
    if isCasePatListNil casepat
    then SOME exp
    else NONE
  | isMRuleListNil _ = NONE

fun isMRuleListCons (N {kind = (MRULE, MRULE_M), label, value, regions, parents, children = [casepat, exp]}) =
    (case isCasePatListCons casepat of
	 SOME (id1, id2) => SOME (id1, id2, exp)
       | NONE => NONE)
  | isMRuleListCons _ = NONE

fun isMatchList (N {kind = (MATCH, MATCH_M), label, value, regions, parents, children = [mrule1, mrule2]}) =
    (case (isMRuleListNil  mrule1,
	   isMRuleListCons mrule2,
	   isMRuleListNil  mrule2,
	   isMRuleListCons mrule1) of
	 (SOME exp1, SOME (id1, id2, exp2), _, _) => SOME (exp1, (id1, id2, exp2))
       | (_, _, SOME exp1, SOME (id1, id2, exp2)) => SOME (exp1, (id1, id2, exp2))
       | _ => NONE)
  | isMatchList _ = NONE

fun isPatInjL (N {kind = (PAT, PAT_APP), label, value, regions, parents, children = [id, atpat]}) =
    (case (getIdIdent id, isPatVar atpat) of
	 ("inl", SOME v) => SOME v
       | _ => NONE)
  | isPatInjL _ = NONE

fun isPatInjR (N {kind = (PAT, PAT_APP), label, value, regions, parents, children = [id, atpat]}) =
    (case (getIdIdent id, isPatVar atpat) of
	 ("inr", SOME v) => SOME v
       | _ => NONE)
  | isPatInjR _ = NONE

fun isCasePatInjL (N {kind = (CASEPAT, CASEPAT_PAT), label, value, regions, parents, children = [pat]}) = isPatInjL pat
  | isCasePatInjL _ = NONE

fun isCasePatInjR (N {kind = (CASEPAT, CASEPAT_PAT), label, value, regions, parents, children = [pat]}) = isPatInjR pat
  | isCasePatInjR _ = NONE

fun isMRuleInjL (N {kind = (MRULE, MRULE_M), label, value, regions, parents, children = [casepat, exp]}) =
    (case isCasePatInjL casepat of
	 SOME v => SOME (v, exp)
       | NONE => NONE)
  | isMRuleInjL _ = NONE

fun isMRuleInjR (N {kind = (MRULE, MRULE_M), label, value, regions, parents, children = [casepat, exp]}) =
    (case isCasePatInjR casepat of
	 SOME id => SOME (id, exp)
       | NONE => NONE)
  | isMRuleInjR _ = NONE

fun isMatchInj (N {kind = (MATCH, MATCH_M), label, value, regions, parents, children = [mrule1, mrule2]}) =
    (case (isMRuleInjL mrule1,
	   isMRuleInjR mrule2,
	   isMRuleInjL mrule2,
	   isMRuleInjL mrule1) of
	 (SOME (id1, exp1), SOME (id2, exp2), _, _) => SOME ((id1, exp1), (id2, exp2))
       | (_, _, SOME (id1, exp1), SOME (id2, exp2)) => SOME ((id1, exp1), (id2, exp2))
       | _ => NONE)
  | isMatchInj _ = NONE

fun isPatWild (N {kind = (PAT,   PAT_ATPAT),   label, value, regions, parents, children = [atpat]}) = isPatWild atpat
  | isPatWild (N {kind = (ATPAT, ATPAT_WILD),  label, value, regions, parents, children = []})      = true
  | isPatWild (N {kind = (ATPAT, ATPAT_PAREN), label, value, regions, parents, children = [pat]})   = isPatWild pat
  | isPatWild _ = false

fun isCasePatWild (N {kind = (CASEPAT, CASEPAT_PAT), label, value, regions, parents, children = [pat]}) = isPatWild pat
  | isCasePatWild _ = false

fun isMruleWild (N {kind = (MRULE, MRULE_M), label, value, regions, parents, children = [casepat, exp]}) =
    if isCasePatWild casepat
    then SOME exp
    else NONE
  | isMruleWild _ = NONE

type case_list = term * (value * value * term)
type case_inj  = (value * term) * (value * term)
datatype case_match = CASE_LIST of case_list
		    | CASE_INJ  of case_inj
		    | CASE_OTH

fun caseMatch term =
    case isMatchList term of
	SOME x => CASE_LIST x
      | NONE =>
	case isMatchInj term of
	    SOME x => CASE_INJ x
	  | _ => CASE_OTH

fun getExpMRule (term as N {kind = (MRULE, MRULE_M), label, value, regions, parents, children = [casepat, exp]}) = exp
  | getExpMRule _ = raise Fail "getExpMRule:not_a_mrule"

fun getPatCasePat (term as N {kind = (CASEPAT, CASEPAT_PAT), label, value, regions, parents, children = [pat]}) = pat
  | getPatCasePat _ = raise Fail "getPatCasePat:not_a_casepat"

fun getPatMRule (term as N {kind = (MRULE, MRULE_M), label, value, regions, parents, children = [casepat, exp]}) = getPatCasePat casepat
  | getPatMRule _ = raise Fail "getPatMRule:not_a_mrule"

(* ------ gets the kind of a mrule ------ *)
datatype kind_case = K_INJ
		   | K_LIST
		   | K_VAR
		   | K_WILD
		   | K_TUP
		   | K_INT

fun caseScon (term as N {kind = (SCON, SCON_INT),    label, value, regions, parents, children}) = K_INT
  | caseScon (term as N {kind = (SCON, SCON_REAL),   label, value, regions, parents, children}) = raise Fail "caseScon:real"
  | caseScon (term as N {kind = (SCON, SCON_ATOM),   label, value, regions, parents, children}) = raise Fail "caseScon:atom"
  | caseScon (term as N {kind = (SCON, SCON_ATOMS),  label, value, regions, parents, children}) = raise Fail "caseScon:atoms"
  | caseScon (term as N {kind = (SCON, SCON_STRING), label, value, regions, parents, children}) = raise Fail "caseScon:string"
  | caseScon _ = raise Fail "caseScon:not_a_scon"

fun casePat (term as N {kind = (ATPAT, ATPAT_ID),    label, value, regions, parents, children = [id]})         =
    (case getIdIdent id of
	 "nil" => K_LIST
       | _ => K_VAR)
  | casePat (term as N {kind = (ATPAT, ATPAT_WILD),  label, value, regions, parents, children})                = K_WILD
  | casePat (term as N {kind = (ATPAT, ATPAT_SCON),  label, value, regions, parents, children = [scon]})       = caseScon scon
  | casePat (term as N {kind = (ATPAT, ATPAT_LIST),  label, value, regions, parents, children})                = K_LIST
  | casePat (term as N {kind = (ATPAT, ATPAT_PAREN), label, value, regions, parents, children = [pat]})        = casePat pat
  | casePat (term as N {kind = (ATPAT, ATPAT_STRUC), label, value, regions, parents, children})                = raise Fail "casePat:struc"
  | casePat (term as N {kind = (ATPAT, ATPAT_TUPLE), label, value, regions, parents, children})                = K_TUP
  | casePat (term as N {kind = (PAT,   PAT_TYPED),   label, value, regions, parents, children = [pat, typ]})   = casePat pat
  | casePat (term as N {kind = (PAT,   PAT_AS),      label, value, regions, parents, children})                = raise Fail "casePat:as"
  | casePat (term as N {kind = (PAT,   PAT_CONS),    label, value, regions, parents, children = [pat1, pat2]}) = K_LIST
  | casePat (term as N {kind = (PAT,   PAT_ATPAT),   label, value, regions, parents, children = [atpat]})      = casePat atpat
  | casePat (term as N {kind = (PAT,   PAT_APP),     label, value, regions, parents, children = [id, atpat]})  =
    (case getIdIdent id of
	 "inl" => K_INJ
       | "inr" => K_INJ
       | _ => raise Fail "casePat:app_no_inj")
  | casePat _ = raise Fail "casePat:not_a_pat"

fun caseCasePat (term as N {kind = (CASEPAT, CASEPAT_PAT), label, value, regions, parents, children = [pat]}) = casePat pat
  | caseCasePat _ = raise Fail "caseCasePat:not_a_casepat"

fun caseMRule (term as N {kind = (MRULE, MRULE_M), label, value, regions, parents, children = [casepat, exp]}) = caseCasePat casepat
  | caseMRule _ = raise Fail "caseMRule:not_a_mrule"

(* ------ get the pairs (int, exp) from a matching of the form:
 * case foo
 *   of 1 => exp1
 *   of 2 => exp2
 *   ... ------ *)
fun getIntScon (term as N {kind = (SCON, SCON_INT),    label, value, regions, parents, children}) = Int.fromString value
  | getIntScon (term as N {kind = (SCON, SCON_REAL),   label, value, regions, parents, children}) = raise Fail "getIntScon:real"
  | getIntScon (term as N {kind = (SCON, SCON_ATOM),   label, value, regions, parents, children}) = raise Fail "getIntScon:atom"
  | getIntScon (term as N {kind = (SCON, SCON_ATOMS),  label, value, regions, parents, children}) = raise Fail "getIntScon:atoms"
  | getIntScon (term as N {kind = (SCON, SCON_STRING), label, value, regions, parents, children}) = raise Fail "getIntScon:string"
  | getIntScon _ = raise Fail "getIntScon:not_a_scon"

fun getIntPat (term as N {kind = (ATPAT, ATPAT_ID),    label, value, regions, parents, children = [id]})         = NONE
  | getIntPat (term as N {kind = (ATPAT, ATPAT_WILD),  label, value, regions, parents, children})                = NONE
  | getIntPat (term as N {kind = (ATPAT, ATPAT_SCON),  label, value, regions, parents, children = [scon]})       = getIntScon scon
  | getIntPat (term as N {kind = (ATPAT, ATPAT_LIST),  label, value, regions, parents, children})                = raise Fail "getIntPat:list_not_an_int"
  | getIntPat (term as N {kind = (ATPAT, ATPAT_PAREN), label, value, regions, parents, children = [pat]})        = getIntPat pat
  | getIntPat (term as N {kind = (ATPAT, ATPAT_STRUC), label, value, regions, parents, children})                = raise Fail "getIntPat:struc_not_an_int"
  | getIntPat (term as N {kind = (ATPAT, ATPAT_TUPLE), label, value, regions, parents, children})                = raise Fail "getIntPat:tuple_not_an_int"
  | getIntPat (term as N {kind = (PAT,   PAT_TYPED),   label, value, regions, parents, children = [pat, typ]})   = getIntPat pat
  | getIntPat (term as N {kind = (PAT,   PAT_AS),      label, value, regions, parents, children})                = raise Fail "getIntPat:as_not_an_int"
  | getIntPat (term as N {kind = (PAT,   PAT_CONS),    label, value, regions, parents, children = [pat1, pat2]}) = raise Fail "getIntPat:list_not_an_int"
  | getIntPat (term as N {kind = (PAT,   PAT_ATPAT),   label, value, regions, parents, children = [atpat]})      = getIntPat atpat
  | getIntPat (term as N {kind = (PAT,   PAT_APP),     label, value, regions, parents, children = [id, atpat]})  = raise Fail "getIntPat:app_not_an_int"
  | getIntPat _ = raise Fail "getIntPat:not_a_pat"

fun getIntCasePat (term as N {kind = (CASEPAT, CASEPAT_PAT), label, value, regions, parents, children = [pat]}) = getIntPat pat
  | getIntCasePat _ = raise Fail "getIntCasePat:not_a_casepat"

fun getIntMRule (term as N {kind = (MATCH, MATCH_M), label, value, regions, parents, children = [casepat, exp]}) = (getIntCasePat casepat, exp)
  | getIntMRule _ = raise Fail "getIntsMRule:not_a_mrule"

fun getIntsMatch (term as N {kind = (MATCH, MATCH_M), label, value, regions, parents, children}) =
    let val list =
	    List.mapPartial
		(fn mrule =>
		    let val (nop, exp) = getIntMRule mrule
		    in case nop of
			   NONE => NONE
			 | SOME n => SOME (n, exp)
		    end)
		children
	val (lastnop, lastexp) = getIntMRule (List.last children)
    in case lastnop of
	   NONE => (list, lastexp)
	 | SOME n => raise Fail "getIntsMatch:last_branch_is_not_wildcard"
    end
  | getIntsMatch _ = raise Fail "getIntsMatch:not_a_match"

fun caseIntToITE id [] lastexp = lastexp
  | caseIntToITE id ((n, exp) :: lst) lastexp =
    let val b = mk_new_dum_term EXP_OP "=" [] [idToExp id, intToExp n]
    in mk_new_dum_term EXP_ITE "" [] [b, exp, caseIntToITE id lst lastexp]
    end

(* ------ get the pairs (x, exp) from a matching of the form:
 * case foo
 *   of inl x => exp1
 *   of inr y => exp2
 *   ... ------ *)
datatype k_inj = K_INJ_L  of term
	       | K_INJ_R  of term
	       | K_NO_INJ of term

fun getInjPat (term as N {kind = (ATPAT, ATPAT_ID),    label, value, regions, parents, children = [id]})         = K_NO_INJ term
  | getInjPat (term as N {kind = (ATPAT, ATPAT_WILD),  label, value, regions, parents, children})                = K_NO_INJ term
  | getInjPat (term as N {kind = (ATPAT, ATPAT_SCON),  label, value, regions, parents, children = [scon]})       = raise Fail "getInjPat:scon_not_an_inj"
  | getInjPat (term as N {kind = (ATPAT, ATPAT_LIST),  label, value, regions, parents, children})                = raise Fail "getInjPat:list_not_an_inj"
  | getInjPat (term as N {kind = (ATPAT, ATPAT_PAREN), label, value, regions, parents, children = [pat]})        = getInjPat pat
  | getInjPat (term as N {kind = (ATPAT, ATPAT_STRUC), label, value, regions, parents, children})                = raise Fail "getInjPat:struc_not_an_inj"
  | getInjPat (term as N {kind = (ATPAT, ATPAT_TUPLE), label, value, regions, parents, children})                = raise Fail "getInjPat:tuple_not_an_inj"
  | getInjPat (term as N {kind = (PAT,   PAT_TYPED),   label, value, regions, parents, children = [pat, typ]})   = getInjPat pat
  | getInjPat (term as N {kind = (PAT,   PAT_AS),      label, value, regions, parents, children})                = raise Fail "getInjPat:as_not_an_inj"
  | getInjPat (term as N {kind = (PAT,   PAT_CONS),    label, value, regions, parents, children = [pat1, pat2]}) = raise Fail "getInjPat:list_not_an_inj"
  | getInjPat (term as N {kind = (PAT,   PAT_ATPAT),   label, value, regions, parents, children = [atpat]})      = getInjPat atpat
  | getInjPat (term as N {kind = (PAT,   PAT_APP),     label, value, regions, parents, children = [id, atpat]})  =
    (case getIdIdent id of
	 "inl" => K_INJ_L  atpat
       | "inr" => K_INJ_R  atpat
       | _     => K_NO_INJ term)
  | getInjPat _ = raise Fail "getInjPat:not_a_pat"

fun getInjCasePat (term as N {kind = (CASEPAT, CASEPAT_PAT), label, value, regions, parents, children = [pat]}) = getInjPat pat
  | getInjCasePat _ = raise Fail "getInjCasePat:not_a_casepat"

fun getInjMRule (term as N {kind = (MRULE, MRULE_M), label, value, regions, parents, children = [casepat, exp]}) = (getInjCasePat casepat, exp)
  | getInjMRule _ = raise Fail "getInjMRule:not_a_mrule"

fun getInjsMatch (term as N {kind = (MATCH, MATCH_M), label, value, regions, parents, children}) =
    foldl
	(fn (mrule, (listL, listR, listN)) =>
	    let val (inj, exp) = getInjMRule mrule
	    in case inj of
		   K_INJ_L  atpat => (listL @ [(atpat, exp)], listR, listN)
		 | K_INJ_R  atpat => (listL, listR @ [(atpat, exp)], listN)
		 | K_NO_INJ term  => (listL, listR, listN @ [(term, exp)])
	    end)
	([], [], [])
	children
  | getInjsMatch _ = raise Fail "getInjsMatch:not_a_match"


(* ------ *)

val em_dum_param = mk_new_dum_term PARAM_P "" [] []

fun fillOutWilds set exps (atpat as N {kind = (ATPAT, ATPAT_WILD),  label, value, regions, parents, children}) =
    let val id  = newIdIdSet set "z"
    in (idToDumAtPat id, exps, IDS.add (set, id))
    end
  | fillOutWilds set exps (atpat as N {kind = (ATPAT, ATPAT_ID), label, value, regions, parents, children = [id]}) =
    let val i = getIdIdent id
    in if isParam i
       then let val id  = newIdIdSet set "z"
		val ren = REN.singleton (i, id)
	    in (idToAtPat id, map (fo_ren ren) exps, IDS.add (set, id))
	    end
       else (atpat, exps, set)
    end
  | fillOutWilds set exps term =
    let val children = getChildren term
	val (children', exps', set') =
	    foldr (fn (child, (children, exps, set)) =>
		      let val (child', exps', set') = fillOutWilds set exps child
		      in (child' :: children, exps', set')
		      end)
		  ([], exps, set)
		  children
    in (updChildren term children', exps', set')
    end

fun transformPat2Id pat exp =
    case getIdPat pat of
	SOME ident =>
	let val i = getIdIdent ident
	in if isParam i
	   then let val id  = newIdIdSet (getVIdentsTerm exp) "z"
		    val ren = REN.singleton (i, id)
		in (id, fo_ren ren exp)
		end
	   else (i, exp)
	end
      | NONE =>
	if isWildPat pat orelse isUnitPat pat
	then (newIdIdSet (getVIdentsTerm exp) "z", exp)
	else let val id   = newIdIdSet (getVIdentsTerm exp) "z"
		 val bind = mk_new_dum_term BIND_PAT "" [] [pat, idToDumExp id]
		 val exp' = mk_new_dum_term EXP_LET  "" [] [bind, exp]
	     in (id, exp')
	     end

(* Simplification of a term to a bare minimum *)
fun simplify (term as N {kind = (ATPAT, ATPAT_SCON),  label, value, regions, parents, children = [sc]}) =
    (case sc of
	 N {kind = (SCON, SCON_ATOMS), label, value, regions, parents, children} =>
	 let val toks = String.tokens (fn #" " => true | _ => false) value
	     val list = map (fn tok => idToDumPat tok) toks
	 in transformToConsPat list
	 end
       | _ => term)
  | simplify (term as N {kind = (ATPAT, ATPAT_LIST),  label, value, regions, parents, children}) = transformToConsPat children
  | simplify (term as N {kind = (ATPAT, ATPAT_TUPLE), label, value, regions, parents, children = []}) = term
  | simplify (term as N {kind = (ATPAT, ATPAT_TUPLE), label, value, regions, parents, children}) =
    let fun cons pats = mk_new_dum_term PAT_ATPAT "" [] [mk_new_dum_term ATPAT_TUPLE "" [] pats]
    in updChildren term (transformToSeven (simplifyList children) cons)
    end
  | simplify (term as N {kind = (ATPAT, _), ...}) = updSimplifyChildren term

  (* PAT: we get rid of type annotations *)
  | simplify (term as N {kind = (PAT, PAT_TYPED), label, value, regions, parents, children = [pat, typ]}) = simplify pat
  | simplify (term as N {kind = (PAT, _), ...}) = updSimplifyChildren term

  (* EXP: get rid of type annotations *)
  | simplify (term as N {kind = (EXP, EXP_TYPED),    label, value, regions, parents, children = [exp, typ]}) = simplify exp
  | simplify (term as N {kind = (EXP, EXP_LAMBDA),   label, value, regions, parents, children = [p, e]}) =
    let val pat = simplify p
	val (id, e') = transformPat2Id pat e
    in updChildren term [idToDumPat id, simplify e']
    end
  | simplify (term as N {kind = (EXP, EXP_LET),      label, value, regions, parents, children = [b, e]})        =
    (case b of
	 t as N {kind = (BIND, BIND_PAT), label, value, regions, parents, children = [p, e0]} =>
	 let val pat  = simplify p
	     val exp1 = simplify e0
	     val exp2 = simplify e
	 in mvPat2Exp pat exp1 exp2
	 end
       | _ => updChildren term [simplifyBind b, simplify e])
  | simplify (term as N {kind = (EXP, EXP_CLASS),    label, value, regions, parents, children = [b, e]})        =
    simplify (mk_term EXP_LET label value regions parents [b,e])
  | simplify (term as N {kind = (EXP, EXP_LETR),     label, value, regions, parents, children = [b, e]})        =
    updChildren term [simplifyBind b, simplify e]
  | simplify (term as N {kind = (EXP, EXP_WHERE),    label, value, regions, parents, children = [e, b]})        =
    let val exp = mk_new_dum_term EXP_ATEXP "" [] [e]
    in simplify (mk_new_dum_term EXP_LET value regions [b, exp])
    end
  | simplify (term as N {kind = (EXP, EXP_BINDING),  label, value, regions, parents, children = [e1, p, e2]})   =
    let val pat  = simplify p
	val exp1 = simplify e1
	val (id, e2') = transformPat2Id pat e2
    in updChildren term [exp1, idToDumPat id, simplify e2']
    end
  | simplify (term as N {kind = (EXP, EXP_MBIND),    label, value, regions, parents, children = [e1, e2]})      =
    let fun destFun (N {kind = (ATEXP, ATEXP_PAREN), label, value, regions, parents, children = [exp]})      = destFun exp
	  | destFun (N {kind = (EXP,   EXP_ATEXP),   label, value, regions, parents, children = [atexp]})    = destFun atexp
	  | destFun (N {kind = (EXP,   EXP_LAMBDA),  label, value, regions, parents, children = [pat, exp]}) = SOME (pat, exp)
	  | destFun _ = NONE
    in case destFun e2 of
	   SOME (p, e) => simplify (mk_term EXP_BINDING label "" [] [] [e1, p, e])
	 | NONE =>
	   let val id = newIdIdSet (getVIdentsTerm e2) "z"
	       val p  = idToDumPat id
	       val e  = mk_new_dum_term EXP_APP "" [] [e2, idToDumAtExp id]
	   in simplify (mk_term EXP_BINDING label "" [] [] [e1, p, e])
	   end
    end
  | simplify (term as N {kind = (EXP, EXP_CASE), label, value, regions, parents, children = [e, m]}) =
    let val mexp  = simplify e
	val match = simplify m
    in case getChildren match of
	   (mrule :: mrules) =>
	   (case caseMRule mrule of
		K_WILD => getExpMRule mrule
	      | K_TUP  =>
		if List.null mrules
		then let val pat  = getPatMRule mrule
			 val exp  = getExpMRule mrule
			 val bind = mk_new_dum_term BIND_PAT "" [] [pat, mexp]
		     in mk_new_dum_term EXP_LET "" [] [bind, exp]
		     end
		else raise Fail "todo"
	    | K_INT  =>
	      let val (list, lastexp) = getIntsMatch match
		  val id   = newIdIdSet (getVIdentsTerm match) "z"
		  val bind = mk_new_dum_term BIND_PAT "" [] [idToPat id, mexp]
	      in mk_new_dum_term EXP_LET "" [] [bind, caseIntToITE id list lastexp]
	      end
	    | K_VAR  =>
	      let val pat  = getPatMRule mrule
		  val exp  = getExpMRule mrule
		  val bind = mk_new_dum_term BIND_PAT "" [] [pat, mexp]
	      in mk_new_dum_term EXP_LET "" [] [bind, exp]
	      end
	    | K_LIST => raise Fail "todo"
	    | K_INJ  =>
	      let val (listL, listR, listN) = getInjsMatch match
		  val nL = List.length listL
		  val nR = List.length listR
		  val nN = List.length listN
	      in if nL = 1 andalso nR = 1 andalso nN = 0
		 then let val (atpatL, expL) = List.hd listL
			  val (atpatR, expR) = List.hd listR
			  val patL      = mk_new_dum_term PAT_ATPAT "" [] [atpatL]
			  val patR      = mk_new_dum_term PAT_ATPAT "" [] [atpatR]
			  val (idL, eL) = transformPat2Id patL expL
			  val (idR, eR) = transformPat2Id patR expR
			  val vL        = mk_new_dum_term ID_VID "inl" [] []
			  val vR        = mk_new_dum_term ID_VID "inr" [] []
			  val apL       = mk_new_dum_term PAT_APP "" [] [vL, idToDumAtPat idL]
			  val apR       = mk_new_dum_term PAT_APP "" [] [vR, idToDumAtPat idR]
			  val conspatL  = mk_new_dum_term CASEPAT_PAT "" [] [apL]
			  val conspatR  = mk_new_dum_term CASEPAT_PAT "" [] [apR]
			  val mruleL    = mk_new_dum_term MRULE_M "" [] [conspatL, eL]
			  val mruleR    = mk_new_dum_term MRULE_M "" [] [conspatR, eR]
		      in updChildren term [mexp, updChildren match [mruleL, mruleR]]
		      end
		 else raise Fail "todo"
	      end)
	 | _ => raise Fail "structMatch:no_mrule"
    end

  | simplify (term as N {kind = (EXP, _), ...}) = updSimplifyChildren term

  (* ATEXP *)
  | simplify (term as N {kind = (ATEXP, ATEXP_ID),      label, value, regions, parents, children}) = term
  | simplify (term as N {kind = (ATEXP, ATEXP_SCON),    label, value, regions, parents, children = [sc]}) =
    (case sc of
	 N {kind = (SCON, SCON_ATOMS), label, value, regions, parents, children} =>
	 let val toks = String.tokens (fn #" " => true | _ => false) value
	     val list =
		 map (fn tok =>
			 let val sc = mk_new_dum_term SCON_ATOM tok [] []
			     val ae = mk_new_dum_term ATEXP_SCON "" [] [sc]
			 in mk_new_dum_term EXP_ATEXP "" [] [ae]
			 end)
		     toks
	 in transformToConsExp list
	 end
       | _ => term)
  | simplify (term as N {kind = (ATEXP, ATEXP_TUPLE),   label, value, regions, parents, children = []}) = term
  | simplify (term as N {kind = (ATEXP, ATEXP_TUPLE),   label, value, regions, parents, children}) =
    let fun cons exps = mk_new_dum_term EXP_ATEXP "" [] [mk_new_dum_term ATEXP_TUPLE "" [] exps]
    in updChildren term (transformToPairs (simplifyList children) cons)
    end
  | simplify (term as N {kind = (ATEXP, ATEXP_LIST),    label, value, regions, parents, children}) = transformToConsExp (simplifyList children)
  | simplify (term as N {kind = (ATEXP, _), ...}) = updSimplifyChildren term

  (* ATOMS *)
  | simplify (term as N {kind = (ATOMS, ATOMS_ATOMS), label, value, regions, parents, children = []}) = term
  | simplify (term as N {kind = (ATOMS, ATOMS_LIST),  label, value, regions, parents, children = []}) = mk_new_dum_term ATOMS_ATOMS value [] []
  | simplify (term as N {kind = (ATOMS, ATOMS_WILD),  label, value, regions, parents, children = []}) = term

  (* BINDS *)
  | simplify (term as N {kind = (BINDS, BINDS_LIST), label, value, regions, parents, children}) =
    updChildren term (map simplifyBind (getChildren term))

  (* CASEPAT *)
  | simplify (term as N {kind = (CASEPAT, CASEPAT_PAT), label, value, regions, parents, children = [pat]}) = updSimplifyChildren term
  (* MRULE  *)
  | simplify (term as N {kind = (MRULE, MRULE_M), label, value, regions, parents, children = [casepat, exp]}) = updSimplifyChildren term
  (* MATCH *)
  | simplify (term as N {kind = (MATCH, MATCH_M),  label, value, regions, parents, children}) = updSimplifyChildren term

  (* OTHER TERMS *)
  | simplify (term as N {kind = (TYPEVAR, _),    ...}) = term
  | simplify (term as N {kind = (SCON, _),       ...}) = term
  | simplify (term as N {kind = (ID, _),         ...}) = term
  | simplify (term as N {kind = (IDENTS, _),     ...}) = term
  | simplify (term as N {kind = (TYPE, _),       ...}) = updSimplifyChildren term
  | simplify (term as N {kind = (PROP, _),       ...}) = updSimplifyChildren term
  | simplify (term as N {kind = (INCPARM, _),    ...}) = updSimplifyChildren term
  | simplify (term as N {kind = (INCPARMS, _),   ...}) = updSimplifyChildren term
  | simplify (term as N {kind = (DATA, _),       ...}) = updSimplifyChildren term
  | simplify (term as N {kind = (PARAM, _),      ...}) = updSimplifyChildren term
  | simplify (term as N {kind = (ARG, _),        ...}) = updSimplifyChildren term
  | simplify (term as N {kind = (ARGS, _),       ...}) = updSimplifyChildren term
  | simplify (term as N {kind = (TYPEVARSEQ, _), ...}) = updSimplifyChildren term
  | simplify (term as N {kind = (TYPESEQ, _),    ...}) = updSimplifyChildren term
  | simplify (term as N {kind = (OTYPESEQ, _),   ...}) = updSimplifyChildren term
  | simplify (term as N {kind = (TYPESEQSET, _), ...}) = updSimplifyChildren term
  | simplify (term as N {kind = (DOTS, _),       ...}) = updSimplifyChildren term
  (* wrong format *)
  | simplify term = (print (toString term); raise Fail wrongFormat)

and simplifyList terms = map simplify terms

and updSimplifyChildren term = updChildren term (simplifyList (getChildren term))

and simplifyBind (term as N {kind = (BIND, BIND_DEC),  label, value, regions, parents, children = [f, a, e]})       =
    let val exp' = generateLambdas (simplify a) (simplify e)
    in updChildren term [f, em_dum_param, exp']
    end
  | simplifyBind (term as N {kind = (BIND, BIND_TDEC), label, value, regions, parents, children = [f, a, t, e]})    =
    let val exp' = generateLambdas (simplify a) (simplify e)
    (* NOTE: We get rid of the type annotation *)
    in mk_new_dum_term BIND_DEC "" [] [f, em_dum_param, exp']
    end
  | simplifyBind (term as N {kind = (BIND, BIND_PAT),  label, value, regions, parents, children = [p, e]})          =
    (case getIdPat (simplify p) of
	 SOME id => mk_new_dum_term BIND_DEC "" [] [id, em_dum_param, simplify e]
       | NONE => raise Fail "")
  | simplifyBind (term as N {kind = (BIND, BIND_IOP),  label, value, regions, parents, children = [id, a1, a2, e]}) =
    let val lam1 = generateLambdas (simplify a2) (simplify e)
	val lam2 = generateLambdas (simplify a1) lam1
    in mk_new_dum_term BIND_DEC "" [] [id, em_dum_param, lam2]
    end
  | simplifyBind (term as N {kind = (BIND, BIND_TIOP), label, value, regions, parents, children = [id, a1, a2, t, e]}) =
    let val lam1 = generateLambdas (simplify a2) (simplify e)
	val lam2 = generateLambdas (simplify a1) lam1
    in mk_new_dum_term BIND_DEC "" [] [id, em_dum_param, lam2]
    end
  | simplifyBind term = (print (toString term); raise Fail wrongFormat)

and simplifyDec (term as N {kind = (DEC, DEC_LET),     label, value, regions, parents, children = [b]})              =
    (case b of
	  N {kind = (BIND, BIND_PAT), label, value, regions, parents, children = [p, e]} =>
	  (* NOTE: This time, the ids from pat must be substituted in exp only if we're in a letrec *)
	  let val pat = simplify p
	      val exp = simplify e
	      val (id, ids) =
		  (* NOTE: we do the following to try to introduce less new identifiers
		   * as possible. *)
		  case getIdPat pat of
		      SOME id => (getIdIdent id, IDS.empty)
		    | NONE => (newIdIdSet (getVIdentsTerm exp) "z", getVIdentsTerm p)
	      (* NOTE: we put value in there so that we know that if an environment
	       * has been generated for this declaration then we should look at a
	       * binder of value and not id. *)
	      val tid   = mk_new_dum_term ID_VID id [] []
	      val bind  = mk_new_dum_term BIND_DEC value [] [tid, em_dum_param, exp]
	      val binds =
		  IDS.foldr (fn (id0, decs) =>
				let val fid = mk_new_dum_term ID_VID id0 [] []
				    val exp = mvPat2Exp pat (idToDumExp id) (idToDumExp id0)
				    val dec = mk_new_dum_term BIND_DEC "" [] [fid, em_dum_param, exp]
				in dec :: decs
				end)
			    []
			    ids
	  (* NOTE: we use sub in let-expression because we don't really care about what
	   * identifier is declared and we use binds in declaration because there we
	   * care about what identifier is declared. *)
	  in map (fn bind => mk_new_dum_term DEC_LET "" [] [bind]) (bind :: binds)
	  end
	| _ => [updChildren term [simplifyBind b]])
  | simplifyDec (term as N {kind = (DEC, DEC_CLASS),    label, value, regions, parents, children = [b]})              =
    simplifyDec (mk_term DEC_LET label value regions parents [b])
  | simplifyDec (term as N {kind = (DEC, DEC_CLASSREC), label, value, regions, parents, children = [i, a, x, y]})    =
    let val (id1, e1) = transformPat2Id a x
	val (id2, e2) = transformPat2Id a y
	val (id, e2) =
	    if id1 = id2
	    then (id1, e2)
	    else let val id = newIdIdSet (getVIdentsTerm e2) id1
		     val ren = REN.singleton (id2, id)
		 in (id, fo_ren ren e2)
		 end
    in [updChildren term [i, idToDumPat id, e1, e2]]
    end
  | simplifyDec (term as N {kind = (DEC, DEC_LETR),     label, value, regions, parents, children = [b]})             =
    (* If the code type checks then b is a function. *)
    let val bind  = simplifyBind b
	val id    = getIdBind bind
	val tid   = mk_new_dum_term ID_VID id [] []
	val rexp  = mk_new_dum_term EXP_LETR "" [] [bind, idToDumExp id]
	val bind' = mk_new_dum_term BIND_DEC "" [] [tid, em_dum_param, rexp]
    in [mk_term DEC_LET label value regions parents [bind']]
    end
  | simplifyDec (term as N {kind = (DEC, DEC_INV),      label, value, regions, parents, children = [i, c, a, x, B]}) =
    let val set = getVIdentsTerm term
	val a1  = simplify a
	val (a2, Bs, _) = fillOutWilds set [B] a1
	val B1  = case Bs of [B1] => B1 | _ => raise Fail ""
	val x1  = simplify x
	val B2  = simplify B1
	val (id, B3) = transformPat2Id x1 B2
    in [updChildren term [i, c, a2, idToDumPat id, simplify B3]]
    end
  | simplifyDec (term as N {kind = (DEC, DEC_ORDER),    label, value, regions, parents, children = [i, c, a, x, y, B]}) =
    let val set = getVIdentsTerm term
	val a1  = simplify a
	val (a2, Bs, _) = fillOutWilds set [B] a1
	val B1  = case Bs of [B1] => B1 | _ => raise Fail ""
	val x1  = simplify x
	val y1  = simplify y
	val B2  = simplify B1
	val (id2, B3) = transformPat2Id y1 B2
	val (id1, B4) = transformPat2Id x1 B3
    in [updChildren term [i, c, a2, idToDumPat id1, idToDumPat id2, simplify B4]]
    end
  | simplifyDec (term as N {kind = (DEC, DEC_PROGRESS), label, value, regions, parents, children = [i, c, a, x, y, W, B]}) =
    let val set = getVIdentsTerm term
	val a1 = simplify a
	val (a2, Bs, _) = fillOutWilds set [B] a1
	val B1  = case Bs of [B1] => B1 | _ => raise Fail ""
	val x1 = simplify x
	val y1 = simplify y
	val B2 = simplify B1
	val (id2, B3) = transformPat2Id y1 B2
	val (id1, B4) = transformPat2Id x1 B3
    in [updChildren term [i, c, a2, idToDumPat id1, idToDumPat id2, W, simplify B4]]
    end
  | simplifyDec (term as N {kind = (DEC, DEC_STRICT), label, value, regions, parents, children = [i, c, a, x, y, v, W, z, P, B]}) =
    let val set = getVIdentsTerm term
	val a1 = simplify a
	val (a2, Bs, _) = fillOutWilds set [B,W] a1
	val (B1,W1) = case Bs of [B1,W1] => (B1,W1) | _ => raise Fail ""
	val x1 = simplify x
	val y1 = simplify y
	val B2 = simplify B1
	val (id2, B3) = transformPat2Id y1 B2
	val (id1, B4) = transformPat2Id x1 B3
	val v1 = simplify v
	val z1 = simplify z
	val (idZ, P1) = transformPat2Id z1 P
	val (idV, P2) = transformPat2Id v1 P1
    in [updChildren term [i, c, a2, idToDumPat id1, idToDumPat id2, idToDumPat idV, W1, idToDumPat idZ, simplify P2, simplify B4]]
    end
  | simplifyDec (term as N {kind = (DEC, DEC_CONSIST),  label, value, regions, parents, children = [i, c, a, x, y, B]}) =
    let val set = getVIdentsTerm term
	val a1  = simplify a
	val (a2, Bs, _) = fillOutWilds set [B] a1
	val B1  = case Bs of [B1] => B1 | _ => raise Fail ""
	val x1  = simplify x
	val y1  = simplify y
	val B2  = simplify B1
	val (id2, B3) = transformPat2Id y1 B2
	val (id1, B4) = transformPat2Id x1 B3
    in [updChildren term [i, c, a2, idToDumPat id1, idToDumPat id2, simplify B4]]
    end
  | simplifyDec (term as N {kind = (DEC, DEC_MEMORY), label, value, regions, parents, children = [i, c, a, x, y, v, W, B]}) =
    let val set = getVIdentsTerm term
	val a1 = simplify a
	val (a2, Bs, _) = fillOutWilds set [B,W] a1
	val (B1,W1) = case Bs of [B1,W1] => (B1,W1) | _ => raise Fail ""
	val x1 = simplify x
	val y1 = simplify y
	val v1 = simplify v
	val B2 = simplify B1
	val (idY, B3) = transformPat2Id y1 B2
	val (idX, B4) = transformPat2Id x1 B3
	val (idV, B5) = transformPat2Id v1 B4
    in [updChildren term [i, c, a2, idToDumPat idX, idToDumPat idY, idToDumPat idV, W1, simplify B5]]
    end
  | simplifyDec (term as N {kind = (DEC, DEC_CONS),     ...}) = [updSimplifyChildren term]
  | simplifyDec (term as N {kind = (DEC, DEC_OCONS),    ...}) = [updSimplifyChildren term]
  | simplifyDec (term as N {kind = (DEC, DEC_TYFUN),    ...}) = [updSimplifyChildren term]
  | simplifyDec (term as N {kind = (DEC, DEC_QMSG),     ...}) = [updSimplifyChildren term]
  | simplifyDec (term as N {kind = (DEC, DEC_EQMSG),    ...}) = [updSimplifyChildren term]
  | simplifyDec (term as N {kind = (DEC, DEC_MAIN),     ...}) = [updSimplifyChildren term]
  | simplifyDec (term as N {kind = (DEC, DEC_DATA),     ...}) = [updSimplifyChildren term]
  | simplifyDec (term as N {kind = (DEC, DEC_ABSTYPE),  ...}) = [updSimplifyChildren term]
  | simplifyDec (term as N {kind = (DEC, DEC_INCLUDE),  ...}) = [updSimplifyChildren term]
  | simplifyDec (term as N {kind = (DEC, DEC_PARAMP),   ...}) = [updSimplifyChildren term]
  | simplifyDec (term as N {kind = (DEC, DEC_TYPARAMP), ...}) = [updSimplifyChildren term]
  | simplifyDec (term as N {kind = (DEC, DEC_PARSE),    ...}) = raise Fail "term is not parsable"
  | simplifyDec (term as N {kind = (DEC, DEC_INFIX),    ...}) = []
  | simplifyDec (term as N {kind = (DEC, DEC_VAR),      ...}) = []
  | simplifyDec (term as N {kind = (DEC, DEC_OPTIONS),  ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_PSET),     ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_PMAP),     ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_PARAM),    ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_TYPARAM),  ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_ETPARAM),  ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_TYCON),    ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_EQTYCON),  ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_TYPEOF),   ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_SPEC),     ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_IMPORT),   ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_TIMPORT),  ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_EXPORT),   ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_ASSUME),   ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_GUARANT),  ...}) = [term]
  | simplifyDec (term as N {kind = (DEC, DEC_DOC),      ...}) = [term]
  | simplifyDec term = (print (toString term); raise Fail wrongFormat)

and simplifyFile (term as N {kind = (FILE, FILE_F), label, value, regions, parents, children}) =
    let val children' = List.concat (map simplifyDec children)
    in updChildren term children'
    end
  | simplifyFile term = (print (toString term); raise Fail wrongFormat)

and simplifyProg (term as N {kind = (PROG, PROG_P), label, value, regions, parents, children}) =
    let val children' = map simplifyFile children
    in updChildren term children'
    end
  | simplifyProg term = (print (toString term); raise Fail wrongFormat)

fun collectParameters term =
    case getKind term of
	(DEC, prod) =>
	(case prod of
	     DEC_PSET =>
	     let val (id, t) = get2Children term
	     in IDS.singleton (getIdIdent id)
	     end
	   | DEC_PMAP =>
	     let val (id, t, u) = get3Children term
	     in IDS.singleton (getIdIdent id)
	     end
	   | DEC_PARAM =>
	     let val (id, t) = get2Children term
	     in IDS.singleton (getIdIdent id)
	     end
	   | DEC_PARAMP =>
	     let val (id, ty, prop) = get3Children term
	     in IDS.singleton (getIdIdent id)
	     end
	   | DEC_TYPARAM =>
	     let val (seq, tc, t) = get3Children term
	     in IDS.singleton (getIdIdent tc)
	     end
	   | DEC_TYPARAMP =>
	     let val (seq, tc, t, p) = get4Children term
	     in IDS.singleton (getIdIdent tc)
	     end
	   | DEC_ETPARAM =>
	     let val (seq, tc, eq, t) = get4Children term
	     in IDS.add (IDS.singleton (getIdIdent tc), getIdIdent eq)
	     end
	   | _ => IDS.empty)
      | _ => foldr (fn (term, set) =>
		       IDS.union (set, collectParameters term))
		   IDS.empty
		   (getChildren term)

fun simplify term =
    let val _     = setIdsTerm term
	val bound = collectParameters term
	val _     = IDS.app addParam bound
	val _     = addParam "es"
	val _     = addParam "f"
	val term' = simplifyProg term
	val _     = resetSimp ()
    in term'
    end

end
