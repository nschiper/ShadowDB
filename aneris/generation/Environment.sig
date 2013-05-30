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
 *  o File name:   Environment.sig
 *  o Description: Constraint environments.
 *)


signature ENV = sig

    type envvar      = int
    type tyconvar    = int
    type ityvar      = int
    type ityseqvar   = int
    type tyconname   = string
    type idor        = int

    type id          = string (* Note: same as Ast.value *)

    type marker      = int

    datatype eqTv    = NEQ | EQ of Deps.label

    type eqTc        = bool

    datatype ity     = ITYVAR of ityvar * eqTv
		     | ITYETV of id * Deps.label
		     | ITYCON of ityseq * itycon
		     | ITYOR  of orseq
		     | ITYTYP of ity * Deps.label
		     | ITYDEP of ity * Deps.dep

	 and orseq   = ORSEQ of ityseq * idor * Deps.label

	 and ityseq  = ITYSEQVAR of ityseqvar
		     | ITYSEQSEQ of ity list * Deps.label
		     | ITYSEQDEP of ityseq * Deps.dep

	 and itycon  = ITYCONVAR of tyconvar
		     | ITYCONNAM of tyconname * eqTc * Deps.label
		     | ITYCONDEP of itycon * Deps.dep
		     (*| ITYCONFUN of ityseq * ity*)

    structure ITVS : ORD_SET (* Set of internal type variables *)
    structure ETVS : ORD_SET (* Set of external type variables *)
    structure OTVS : ORD_SET (* Set of idors                   *)

    structure ITVM : ORD_MAP
    structure ETVM : ORD_MAP

    type bound = ITVS.set * ETVS.set * OTVS.set

    type overseq = (orseq * orseq * Deps.deps) list

    type ityfun = bound * (ityseq  * ity)

    type tfvar  = ityseqvar * ityvar

    datatype err_gen = PARSE
		     | REBOUND
		     | SYNTAX

    datatype tvbind_kind = EXPLICIT
			 | IMPLICIT

    type tvbind = ity * tvbind_kind

    (* Polymorphism marker: TOP means that we're dealing with a polymorphic
     * declaration at toplevel; NES means that the poly dec is nested in an
     * expression; CON means that we're dealing with a constant (which are
     * the only top declaration that we allow to be overloaded). *)
    datatype pol_mrk = TOP
		     | NES
		     | CON

    type ovenames = string option list

    datatype loc_kind = EQD | OTH

    type kityvar = ityvar * loc_kind

    datatype bind_kind = PRELUDE
		       | USERCODE

    datatype bind = BINDVID of id * Deps.label * scheme
		  | BINDTYC of id * Deps.label * ityfun
		  | BINDTYV of id * Deps.label * tvbind
		  | BINDATM of id * Deps.label * scheme
		  | BINDVAR of id * Deps.label * ity

	 and acc  = ACCVID of id * ityvar
		  | ACCTYC of id * tfvar
		  | ACCTYV of id * ityvar
		  | ACCATM of id * ityvar

	 and cst  = CSITY of ity    * ity
		  | CSNAM of itycon * itycon
		  | CSSEQ of ityseq * ityseq
		  | CSENV of envvar * env

	 and sub  = SUBITY of ity    * ity
		  | SUBNAM of itycon * itycon
		  | SUBSEQ of ityseq * ityseq
		  | SUBIMP of ityseq * ityseq

	 and env  = ENVVAR of envvar
		  | ENVBIN of bind * bind_kind
		  | ENVACC of acc
		  | ENVCST of cst
		  | ENVSUB of sub
		  | ENVPOL of env * pol_mrk
		  | ENVAPP of env * env
		  | ENVSET of env list
		  | ENVLOC of env * env
		  | ENVDEP of env * Deps.dep
		  (* These are more esoteric *)
		  | ENVFIL of string * env * env * Deps.label
		  | ENVERR of string * err_gen
		  | ENVOVL of id
		  | ENVTOF of id
		  | ENVMRK of marker
		  | ENVLAB of Deps.label * kityvar
		  | ENVOVE of idor * ovenames
		  (* Empty/True environment *)
		  | ENVNUL

	 and cssub  = CSSUB of sub list

	 and cssch  = CSSCH of overseq * cssub

	 and scheme = VMONO of ityvar * Deps.deps * bool
		    | VPOLY of bound * (cssch * ity)
		    | VODEC of bound * (idor  * ity)


    val tyconnameReal   : id
    val tyconnameClass  : id
    val tyconnameMsg    : id
    val tyconnameNat    : id
    val tyconnameBag    : id
    val tyconnameInstr  : id
    val tyconnameList   : id
    val tyconnameUnit   : id
    val tyconnameInt    : id
    val tyconnameAtom   : id
    val tyconnameToken  : id
    val tyconnameTop    : id
    val tyconnameLoc    : id
    val tyconnameType   : id
    val tyconnameProp   : id
    val tyconnameEvent  : id
    val tyconnameBool   : id
    val tyconnameTuple  : id
    val tyconnameArrow  : id
    val tyconnameDisjU  : id
    val tyconnameDeq    : id
    val tyconnameString : id
    val tyconnameSet    : id
    val tyconnameMap    : id

    val subList : (id * id) list

    val tyconnames      : id list

    val eqtyconReal     : eqTc
    val eqtyconClass    : eqTc
    val eqtyconMsg      : eqTc
    val eqtyconNat      : eqTc
    val eqtyconBag      : eqTc
    val eqtyconInstr    : eqTc
    val eqtyconList     : eqTc
    val eqtyconUnit     : eqTc
    val eqtyconInt      : eqTc
    val eqtyconAtom     : eqTc
    val eqtyconToken    : eqTc
    val eqtyconTop      : eqTc
    val eqtyconLoc      : eqTc
    val eqtyconType     : eqTc
    val eqtyconProp     : eqTc
    val eqtyconEvent    : eqTc
    val eqtyconBool     : eqTc
    val eqtyconTuple    : eqTc
    val eqtyconArrow    : eqTc
    val eqtyconDisjU    : eqTc
    val eqtyconDeq      : eqTc
    val eqtyconString   : eqTc
    val eqtyconSet      : eqTc
    val eqtyconMap      : eqTc

    val destArrowType   : ity -> (ity * ity) option
    val destArrowsType  : ity -> ity list * ity
    val destBagType     : ity -> ity option
    val destDeqType     : ity -> ity option
    val destClassType   : ity -> ity option

    val envvarFresh    : envvar
    val ityvarFresh    : ityvar
    val tyconvarFresh  : tyconvar
    val ityseqvarFresh : ityseqvar
    val idorFresh      : idor

    val nextItyvar     : unit -> ityvar
    val nextTyconvar   : unit -> tyconvar
    val nextItyseqvar  : unit -> ityseqvar
    val nextEnvvar     : unit -> envvar
    val nextIdor       : unit -> idor

    val isArrow        : itycon -> bool option

    val isItyType      : ity -> bool
    val isItyProp      : ity -> bool
    val isItyEvent     : ity -> bool
    val isItyBool      : ity -> bool

    val emptyITVS      : ITVS.set
    val emptyETVS      : ETVS.set
    val emptyOTVS      : OTVS.set
    val foldrITVS      : (ityvar * 'a -> 'a) -> 'a -> ITVS.set -> 'a
    val foldrETVS      : (id     * 'a -> 'a) -> 'a -> ETVS.set -> 'a
    val foldrOTVS      : (idor   * 'a -> 'a) -> 'a -> OTVS.set -> 'a
    val rmITVS         : ITVS.set * ityvar -> ITVS.set
    val isinITVS       : ITVS.set * ityvar -> bool
    val isinETVS       : ETVS.set * id     -> bool
    val isinOTVS       : OTVS.set * idor   -> bool
    val isEmptyITVS    : ITVS.set -> bool
    val isEmptyETVS    : ETVS.set -> bool
    val isEmptyOTVS    : OTVS.set -> bool
    val removeITVS     : ITVS.set * ityvar -> ITVS.set
    val removeETVS     : ETVS.set * id     -> ETVS.set
    val removeOTVS     : OTVS.set * idor   -> OTVS.set
    val unionITVS      : ITVS.set * ITVS.set -> ITVS.set
    val unionETVS      : ETVS.set * ETVS.set -> ETVS.set
    val unionOTVS      : OTVS.set * OTVS.set -> OTVS.set
    val listItemsITVS  : ITVS.set -> ityvar list
    val listItemsETVS  : ETVS.set -> id     list
    val listItemsOTVS  : OTVS.set -> idor   list

    val addListETVS    : (ETVS.set * string list) -> ETVS.set

    val emptyITVM      : 'a ITVM.map
    val emptyETVM      : 'a ETVM.map
    val unionWithITVM  : ('a * 'a -> 'a) -> ('a ITVM.map * 'a ITVM.map) -> 'a ITVM.map
    val unionWithETVM  : ('a * 'a -> 'a) -> ('a ETVM.map * 'a ETVM.map) -> 'a ETVM.map
    val insertITVM     : 'a ITVM.map * ityvar * 'a -> 'a ITVM.map
    val singletonITVM  : ityvar * 'a -> 'a ITVM.map
    val singletonETVM  : id     * 'a -> 'a ETVM.map
    (*val findITVM       : 'a ITVM.map * ityvar -> 'a option*)
    val foldriITVM     : (ityvar * 'b * 'a -> 'a) -> 'a -> 'b ITVM.map -> 'a
    val foldriETVM     : (id     * 'b * 'a -> 'a) -> 'a -> 'b ETVM.map -> 'a

    val applyDepsIty    : ity    -> Deps.deps -> ity
    val applyDepsItycon : itycon -> Deps.deps -> itycon
    val applyDepsItyseq : ityseq -> Deps.deps -> ityseq
    val applyDepsItyfun : ityfun -> Deps.deps -> ityfun
    val applyDepsScheme : scheme -> Deps.deps -> scheme
    val applyDepsTvbind : tvbind -> Deps.deps -> tvbind
    val applyDepsEnv    : env    -> Deps.deps -> env

    val getIdOrseq : orseq -> idor

    val getDepsIty     : ity    -> Deps.deps
    val getDepsOrseq   : orseq  -> Deps.deps
    val getDepsItyseq  : ityseq -> Deps.deps

    val getItyvarsIty       : ity    -> ITVS.set
    val getIEtyvarsItyseq   : ityseq -> bound
    val getIEtyvarsIty      : ity    -> bound
    (*val getDepItyvarsIty    : ity    -> Deps.deps ITVM.map*)
    val getDepsIEtyvarsIty  : ity    -> Deps.deps ITVM.map * Deps.deps ETVM.map

    val applyDepsIEtyvarsIty    : ity    -> (Deps.deps ITVM.map * Deps.deps ETVM.map) -> pol_mrk -> bound * ity
    val applyDepsIEtyvarsItyseq : ityseq -> (Deps.deps ITVM.map * Deps.deps ETVM.map) -> pol_mrk -> bound * ityseq

    val getItyvarseqsItyseq : ityseq -> ITVS.set (* TODO: this is not a ITVS.set but a type sequence variable set *)

    val stripBind           : env -> env * Deps.deps

    val stripIty            : ity    -> ity
    val stripItycon         : itycon -> itycon
    val stripItyseq         : ityseq -> ityseq
    val stripSub            : sub    -> sub

    val get_binders         : env -> string list

    val extractDepsIty      : ity    -> ity    * Deps.deps
    val extractDepsItyseq   : ityseq -> ityseq * Deps.deps
    val extractDepsItycon   : itycon -> itycon * Deps.deps

    val allEqItys     : ity list  -> bool
    val isConcreteIty : ity       -> bool

    val substituteIty : ity -> ityvar -> ity -> ity
    val substituteEty : ity -> id     -> ity -> ity

    val isNullEnv : env -> bool

    val isEmptyBound   : bound -> bool

    val mergeBound     : bound * bound -> bound

    val mergeEqTv      : eqTv * eqTv -> eqTv

    val mk_new_tyvar   : unit       -> ity
    val mk_new_eqtyvar : Deps.label -> ity
    val mk_tyvar       : ityvar     -> ity
    val mk_eqtyvar     : ityvar     -> Deps.label -> ity

    (*val mk_type_string : Deps.label -> ity*)
    val mk_type_real   : Deps.label -> ity
    val mk_type_int    : Deps.label -> ity
    val mk_type_bool   : Deps.label -> ity
    val mk_type_prop   : Deps.label -> ity
    val mk_type_event  : Deps.label -> ity
    val mk_type_atom   : Deps.label -> ity
    val mk_type_top    : Deps.label -> ity
    val mk_type_loc    : Deps.label -> ity
    val mk_type_msg    : Deps.label -> ity
    val mk_type_unit   : Deps.label -> ity
    val mk_type_string : Deps.label -> ity
    val mk_type_type   : Deps.label -> ity
    val mk_type_eo     : Deps.label -> ity
    val mk_type_msgfun : Deps.label -> ity
    val mk_type_instr  : Deps.label -> ity

    val mk_type_bag    : ity -> Deps.label -> ity
    val mk_type_list   : ity -> Deps.label -> ity
    val mk_type_class  : ity -> Deps.label -> ity
    val mk_type_deq    : ity -> Deps.label -> ity
    val mk_type_set    : ity -> Deps.label -> ity

    val mk_type_disju  : ity * ity -> Deps.label -> ity
    val mk_type_arrow  : ity * ity -> Deps.label -> ity
    val mk_type_map    : ity * ity -> Deps.label -> ity

    val mk_type_tuple  : ity list -> Deps.label -> ity

    val mk_type_or     : ityseq -> idor -> Deps.label -> ity

    val mk_typecon     : ityseqvar -> tyconvar -> Deps.label -> ity

    val mk_typecon_gen :        Deps.label -> string -> eqTc -> itycon
    val mk_typefun_gen : int -> Deps.label -> string -> eqTc -> ityfun
    (*val mk_typefun_msg : Deps.label -> ityfun*)

    val mk_new_vscheme   : ityvar -> scheme
    val mk_new_vvscheme  : ityvar -> scheme
    val mk_new_oscheme   : ityvar -> idor -> scheme
    val mk_new_scheme    : ityvar list -> ity -> scheme
    val mk_new_ityfun    : ityseq -> ity -> ityfun
    val mk_new_tvbind    : ityvar -> tvbind
    val mk_new_cs_scheme : unit -> cssch

    val mk_env_depcst  : cst     -> Deps.label -> env
    val mk_env_depsub  : sub     -> Deps.label -> env
    val mk_env_depbin  : bind    -> Deps.label -> env
    val mk_env_deppre  : bind    -> Deps.label -> env
    val mk_env_depacc  : acc     -> Deps.label -> env
    val mk_env_depvar  : envvar  -> Deps.label -> env
    val mk_env_deplab  : kityvar -> Deps.label -> env
    val mk_env_depove  : idor -> ovenames -> Deps.label -> env
    val mk_env_loc     : env * env -> env
    val mk_env_app     : env * env -> env

    val list2env       : env list -> env
    val sub2cst        : sub -> cst

    val nextMarker     : unit -> marker

    val reset          : unit -> unit

    val toStringTyconname     : tyconname -> string
    val toStringItyvar        : ityvar    -> string
    val toStringIty           : ity       -> string
    val toStringEnv           : env       -> string
    val toStringSub           : sub       -> string

    val ppScheme  : scheme -> string
    val ppIty'    : ity    -> string
    val ppItycon  : itycon -> string
    val ppItyseq' : ityseq -> string
    val ppSub'    : sub    -> string

    val ppIty''   : ity -> (string -> string) -> string

end
