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
 *  o File name:   Ast.sig
 *  o Description: Signature to handle EventML ASTs.
 *)


signature AST = sig

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
	   | DATA
	   | INCPARM
	   | INCPARMS
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
	   (* CASEPAT *)
	   | CASEPAT_PAT
	   (* ATOMS *)
	   | ATOMS_ATOMS
	   | ATOMS_LIST
	   | ATOMS_WILD
	   (* DEC *)
	   | DEC_LET      (* non-recursive let declaration                   *)
	   | DEC_LETR     (* recursive let declaration                       *)
	   | DEC_CLASS    (* class declaration                               *)
	   | DEC_CLASSREC (* class declaration                               *)
	   | DEC_CONS     (* declaration of a typed identifier               *)
	   | DEC_OCONS    (* overloaded declaration of a typed identifier    *)
	   | DEC_PSET     (* program parameter of type set                   *)
	   | DEC_PMAP     (* program parameter of type map                   *)
	   | DEC_PARAM    (* program parameter                               *)
	   | DEC_PARAMP   (* program parameter with proposition              *)
	   | DEC_TYPARAM  (* program type parameter                          *)
	   | DEC_TYPARAMP (* program type parameter with proposition         *)
	   | DEC_ETPARAM  (* program type parameter with equality decider    *)
	   | DEC_TYCON    (* declaration of a new type constructor           *)
	   | DEC_EQTYCON  (* declaration of a new type constructor           *)
	   | DEC_TYFUN    (* declaration of a new type function              *)
	   | DEC_QMSG     (* declaration of a msg header                     *)
	   | DEC_EQMSG    (* declaration of a msg header with hdr as an exp  *)
	   | DEC_PARSE    (* parsing error                                   *)
	   | DEC_TYPEOF   (* request of the type of a value identifier       *)
	   | DEC_INFIX    (* makes an operator infix                         *)
	   | DEC_MAIN     (* main class                                      *)
	   | DEC_SPEC     (* specification name                              *)
	   | DEC_IMPORT   (* import nuprl abstraction                        *)
	   | DEC_TIMPORT  (* import nuprl abstraction (types)                *)
	   | DEC_EXPORT   (* export classes                                  *)
	   | DEC_INCLUDE  (* includes another spec                           *)
	   | DEC_ASSUME   (* assumptions                                     *)
	   | DEC_GUARANT  (* assumptions                                     *)
	   | DEC_DOC      (* documentation string/comments                   *)
	   | DEC_INV      (* a class invariant                               *)
	   | DEC_ORDER    (* an ordering porperty on class                   *)
	   | DEC_PROGRESS (* a progress property on class                    *)
	   | DEC_STRICT   (* a real progress property on class               *)
	   | DEC_CONSIST  (* a consistency property on class                 *)
	   | DEC_MEMORY   (* a memory property on (Memory) class             *)
	   | DEC_DATA     (* a datatype declaration                          *)
	   | DEC_ABSTYPE  (* an abstract type declaration                    *)
	   | DEC_VAR      (* a variable declaration                          *)
	   | DEC_OPTIONS  (* an option declaration                           *)
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
	   | BIND_DEC   (* function declaration                    *)
	   | BIND_TDEC  (* typed function declaration              *)
	   | BIND_PAT   (* pattern declartion                      *)
	   | BIND_IOP   (* infix binary operator declaration       *)
	   | BIND_TIOP  (* typed infix binary operator declaration *)
	   (* BINDS *)
	   | BINDS_LIST
	   (* PARAM *)
	   | PARAM_P   (* Arguments of a function *)
	   (* ARG *)
	   | ARG_A
	   | ARG_T
	   (* ARGS *)
	   | ARGS_EM
	   | ARGS_PSEQ  (* Arguments of a constant *)
	   | ARGS_LSEQ
	   (* PROG *)
	   | PROG_P
	   (* DOTS *)
	   | DOTS_D
	   (* FILE *)
	   | FILE_F

    type kind = class * prod

    type value = string (* NOTE: same as Env.id *)

    datatype term = N of {kind     : kind,
			  label    : Deps.label,
			  value    : value,
			  regions  : Reg.region list,
			  parents  : Reg.region list,
			  children : term list}

    structure IDS : ORD_SET where type Key.ord_key = string

    val mk_term         : prod            ->
			  Deps.label      ->
			  string          ->
			  Reg.region list ->
			  Reg.region list ->
			  term list       ->
			  term

    (* Used by the parser *)
    val mk_new_term     : prod            ->
			  string          ->
			  Reg.region list ->
			  term list       ->
			  term

    (* Used by the compiler to NuPrl *)
    val mk_new_dum_term : prod            ->
			  string          ->
			  Reg.region list ->
			  term list       ->
			  term

    (* Used by the parser *)
    val mk_new_pterm     : prod            ->
			   string          ->
			   Reg.region list ->
			   Reg.region list ->
			   term list       ->
			   term

    val mk_new_internal_term  : prod            ->
				Reg.region list ->
				term list       ->
				term

    val empty_term      : term

    val is_empty_term   : term -> bool

    val isInternalId    : value -> bool

    val reset           : unit -> unit

    val toString_value  : value -> string

    val id_pair_fst     : string
    val id_pair_snd     : string
    val id_list_cons    : string
    val id_list_nil     : string
    val id_list_concat  : string
    val id_bool_true    : string
    val id_bool_false   : string
    val id_bool_not     : string
    val id_prop_not     : string
    val id_int_plus     : string
    val id_int_minus    : string
    val id_int_mult     : string
    val id_int_div      : string
    val id_int_leq      : string
    val id_int_geq      : string
    val id_int_lt       : string
    val id_int_gt       : string
    val id_eq           : string
    val id_eqeq         : string
    val id_diff         : string
    val id_disju_inl    : string
    val id_disju_inr    : string
    val id_disju_isl    : string
    val id_deq_deq      : string
    val id_deq_eqof     : string
    val id_class_par    : string
    val id_class_bind   : string
    val id_class_at     : string
    val id_class_until  : string
    val id_class_opt    : string
    val id_class_opt_c  : string
    val id_fun_fix      : string
    val id_es_loc       : string
    val id_es_causl     : string
    val id_es_locl      : string

    val constructors    : string list

    val isPatApp        : term -> bool
    val isPatTyped      : term -> bool
    val termIsEmParam   : term -> bool
    val isIdVid         : term -> bool
    val typeIsType      : term -> bool
    (*val isWildMrule     : term -> bool*)
    val isExpBag        : term -> bool

    (* The first element of the pair is for the nil part (exp), and the
     * second is for the cons (id, id, exp). *)
    type case_list = term * (value * value * term)
    type case_inj  = (value * term) * (value * term)
    datatype case_match = CASE_LIST of case_list
			| CASE_INJ  of case_inj
			| CASE_OTH

    val isMatchList     : term -> case_list option

    val caseMatch       : term -> case_match

    val getLabel        : term -> Deps.label
    val getValue        : term -> value
    val getChildren     : term -> term list
    val getKind         : term -> kind
    val getClass        : term -> class

    val get_includes    : term -> string list

    val updParents      : term -> Reg.region list -> term
    val updChildren     : term -> term list       -> term

    val getIdIdent      : term -> string
    val getIdIdents     : term -> string list
    val getIdPat        : term -> term option
    val getIdExp        : term -> term option
    val getAtomExp      : term -> term option
    (* The difference with getIdPat is that it returns only the identifier
     * if it is a direct id (not nester in parentheses, for example). *)
    val getIdVidPat     : term -> term option

    val getSpecName     : term -> string

    val isFreeVid       : string -> term -> Deps.deps option

    val idToExp         : value -> term
    val idToAtExp       : value -> term
    val idToPat         : value -> term

    val toString        : term -> string
    val export          : term -> string

    val slicing         : term -> Deps.label list -> term

    val setIdsTerm      : term    -> unit
    val newId           : unit    -> string
    val newIdId         : string  -> string
    val resetSimp       : unit    -> unit
    val newIdSet        : IDS.set -> string
    val newIdIdSet      : IDS.set -> string -> string

    val getVIdentsTerm  : term -> IDS.set
    val getIdentsTerm   : term -> IDS.set

    val vid2tycon       : term -> term
    val merge           : term -> term -> term option
    val simplify        : term -> term

    val wrongFormat     : string

    val splitAtoms      : string -> string list

    val get_rec_bind_components : term -> (term * term * term * term)

end
