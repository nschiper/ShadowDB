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
 *  o File name:   Environment.sml
 *  o Description: Constraint environments.
 *)


structure Env :> ENV = struct

structure T  = ListFormat
structure D  = Deps
structure EH = LibBase

type envvar    = int
type tyconvar  = int
type ityvar    = int
type ityseqvar = int
type tyconname = string
type idor      = int
(*type itokvar   = int (* variable in a dependent product *)*)

type id = string

(* true if it accepts equality *)
datatype eqTv = NEQ | EQ of D.label
(* NOTE: for type variables, NEQ does not mean that it's not an
 * equality constraint but that we don't request equality.
 * In contrast, for type construtor NEQ means that it does not
 * have decidable equality.  The label is then useless, because
 * we have the label in the ITYCONNAM.  We could have two types
 * for the two different cases. *)
type eqTc = bool

datatype ity     = ITYVAR of ityvar * eqTv    (* Implicit type variable   *)
		 | ITYETV of id * D.label     (* Explicit type variable   *)
		 | ITYCON of ityseq * itycon  (* Type construction        *)
		 | ITYOR  of orseq            (* Overloaded type          *)
		 | ITYTYP of ity * D.label    (* type Type with its value *)
		 | ITYDEP of ity * D.dep      (* Dependent type           *)
		 (*| ITYDPP of ity * ity*)

     and orseq   = ORSEQ of ityseq * idor * D.label

     and ityseq  = ITYSEQVAR of ityseqvar
		 | ITYSEQSEQ of ity list * D.label
		 | ITYSEQDEP of ityseq * D.dep

     and itycon  = ITYCONVAR of tyconvar
		 | ITYCONNAM of tyconname * eqTc * D.label
		 | ITYCONDEP of itycon * D.dep
		 (*| ITYCONFUN of ityseq * ity*)

     (*and itoken  = ITOKVAR of itokvar
		 | ITOKITY of ity
		 | ITOKDEP of itoken * dep*)

type marker = int

(* Internal type variable sets *)
structure ITVS = BinarySetFn(type ord_key = ityvar val compare = Int.compare)

(* External type variable sets *)
structure ETVS = BinarySetFn(type ord_key = id     val compare = String.compare)

(* Idor set *)
structure OTVS = BinarySetFn(type ord_key = idor   val compare = Int.compare)

(* Maps from Internal type variables to dependencies *)
structure ITVM = BinaryMapFn(type ord_key = ityvar val compare = Int.compare)

(* Maps from External type variables to dependencies *)
structure ETVM = BinaryMapFn(type ord_key = id     val compare = String.compare)

(* Bound variables of type schemes *)
type bound = ITVS.set * ETVS.set * OTVS.set

(* Unresolved equal overloaded types - used in type schemes *)
type overseq = (orseq * orseq * D.deps) list
(* for example [ (orseq1, orseq2),
 *               (orseq3, orseq4)
 *             ]
 * means that orseq1 = orseq2 and orseq3 = orseq4. *)

(* A type function, combined with a scheme *)
type ityfun = bound * (ityseq  * ity)

 (* A type function built with vars only *)
type tfvar  = ityseqvar * ityvar

(* NOTE: type function are used in binders for type constructors and
 * variable type functiona are used in their accessors.  We then do the
 * application when solving such an accessor is (ACCTYC (id (sv, tv)))
 * looks up (BINDTYC (id, (seq, ity))) then tv = (/\seq.ity)sv (which is
 * the result of reducing the application of the type function to sv). *)

(* NOTE: err_gen are the kinds of erros found at constraint generation.
 * They are mostly delayed from parsing so that we can maximise the
 * number of reported errors. *)
datatype err_gen = PARSE   (* parsing error                   *)
		 | REBOUND (* rebinding of a unreboundable id *)
		 | SYNTAX  (* syntax error                    *)

datatype tvbind_kind = EXPLICIT | IMPLICIT
type tvbind = ity * tvbind_kind

datatype pol_mrk = TOP | NES | CON

type ovenames = string option list

datatype loc_kind = EQD | OTH

type kityvar = ityvar * loc_kind

datatype bind_kind = PRELUDE | USERCODE

(* Binders *)
datatype bind = BINDVID of id * Deps.label * scheme  (* value variable binder   *)
	      | BINDTYC of id * Deps.label * ityfun  (* type constructor binder *)
	      | BINDTYV of id * Deps.label * tvbind  (* type variable binder    *)
	      | BINDATM of id * Deps.label * scheme  (* atom list binder        *)
	      | BINDVAR of id * Deps.label * ity     (* variable bind           *)

     (* NOTE: a variable bind is something like:
      *    variable loc : Loc
      * It is used to constrain once and for all the type of a variable.
      *)

     (* NOTE: the distinction between explicit and implicit type variable binders
      * is that when a explicit one is accessed we bind the bound variable to an
      * explicit type variable and similarly for the implicit case.  With this
      * difference we can use the explicit case to generate explicit type variables
      * that are then collected in type schemes and instantiated when needed.
      * The implicit case is used to generate monomorphic binders for MSGS decs.
      * For example, given MSGS (``foo`` : 'a List) then we want ``foo`` to be
      * bound to ('a List) where ('a) is an explicit type variable but we want
      * to generate (tv List) where (tv) is an implicit type variable, so that
      * it can be constraint to, e.g., to Int (while ('a = Int) would lead to a
      * type error). *)

     (* Accessors *)
     and acc  = ACCVID of id * ityvar
	      | ACCTYC of id * tfvar
	      | ACCTYV of id * ityvar
	      | ACCATM of id * ityvar
	      (*| ACCTOK of id * ityvar*)

     (* Equality constraints *)
     and cst  = CSITY of ity    * ity
	      | CSNAM of itycon * itycon
	      | CSSEQ of ityseq * ityseq
	      | CSENV of envvar * env
	      (*| CSARI of itycon * ityseq*)

     (* Subtyping constraints *)
     and sub  = SUBITY of ity    * ity
	      | SUBNAM of itycon * itycon
	      | SUBSEQ of ityseq * ityseq
	      | SUBIMP of ityseq * ityseq

     (* Environments *)
     and env  = ENVVAR of envvar              (* Environment variable       *)
	      | ENVBIN of bind * bind_kind    (* Binders                    *)
	      | ENVACC of acc                 (* Accessors                  *)
	      | ENVCST of cst                 (* Equality constriants       *)
	      | ENVSUB of sub                 (* Subtyping constraints      *)
	      | ENVPOL of env * pol_mrk       (* To polymorphic environment *)
	      | ENVAPP of env * env           (* Sequencing of environments *)
	      | ENVSET of env list            (* Set of environments
					       * These don't have any order,
					       * they are similar to ENVAPP
					       * otherwise.  The only diff is
					       * that when we do filtering,
					       * we put the dums at the start
					       * of the list.               *)
	      | ENVLOC of env * env           (* Local environments         *)
	      | ENVDEP of env * D.dep         (* Dependent environments     *)
	      | ENVFIL of string
			  * env * env
			  * D.label           (* Environments of a file
					       * Used to hold the label of the
			 		       * node containing the name of the
		 			       * file for which the env has been
	 				       * generated + exported env   *)
	      | ENVERR of string * err_gen    (* Parsing/Syntax/Gen  error  *)
	      | ENVOVL of id                  (* Overloading syntactic error
					       * Used when a type variable
					       * is overloaded twice to the
					       * same top type constructor  *)
	      | ENVTOF of id                  (* Request of an id's type    *)
	      | ENVMRK of marker              (* Marker in an environment, used
					       * during constraint solving, to
					       * be able to extract the beginning
					       * or end of ENVAPP sequences *)
	      | ENVLAB of D.label * kityvar   (* Records that ityvar has been
					       * generated as the type of the
					       * piece of code at the given
					       * label                      *)
	      | ENVOVE of idor * ovenames     (* To record the names associated
                                               * with the different choices of
                                               * on overloading             *)
	      | ENVNUL                        (* Empty/True environment     *)

     (* NOTE: in ENVLAB, we don't acutally need the label because we always
      * label such an environment, but it's cleaner this way. *)

     (* List of subtyping constraint *)
     and cssub = CSSUB of sub list

     (* Constraints on a type scheme *)
     and cssch = CSSCH of overseq * cssub

     (* A more or less usual type scheme *)
     and scheme = VMONO of ityvar * D.deps * bool (* monomorphic scheme - no generalised variable.
						   * The boolean is to indicate whether the type scheme depends on another type scheme. *)
		| VPOLY of bound * (cssch * ity)  (* polymorphic scheme *)
		| VODEC of bound * (idor  * ity)  (* overloaded scheme  *)


(* ------ CHECKERS ------ *)

fun eqIty (ITYVAR (ityvar1, _), ITYVAR (ityvar2, _)) = (ityvar1 = ityvar2)
  | eqIty (ITYETV (id1, _), ITYETV (id2, _)) = (id1 = id2)
  | eqIty (ITYCON (ityseq1, itycon1), ITYCON (ityseq2, itycon2)) = eqItycon (itycon1, itycon2) andalso eqItyseq (ityseq1, ityseq2)
  | eqIty (ITYOR orseq1, ITYOR orseq2) = eqOrseq (orseq1, orseq2)
  | eqIty (ITYTYP (ity1, _), ITYTYP (ity2, _)) = eqIty (ity1, ity2)
  | eqIty (ITYDEP (ity1, _), ity2) = eqIty (ity1, ity2)
  | eqIty (ity1, ITYDEP (ity2, _)) = eqIty (ity1, ity2)
  | eqIty _ = false

and eqItycon (ITYCONVAR var1, ITYCONVAR var2) = (var1 = var2)
  | eqItycon (ITYCONNAM (name1, _, _), ITYCONNAM (name2, _, _)) = (name1 = name2)
  | eqItycon (ITYCONDEP (itycon1, _), itycon2) = eqItycon (itycon1, itycon2)
  | eqItycon (itycon1, ITYCONDEP (itycon2, _)) = eqItycon (itycon1, itycon2)
  | eqItycon _ = false

and eqItyseq (ITYSEQVAR var1, ITYSEQVAR var2) = (var1 = var2)
  | eqItyseq (ITYSEQSEQ (itys1, _), ITYSEQSEQ (itys2, _)) =
    ((foldr (fn (pair, b) => b andalso eqIty pair)
	    true
	    (ListPair.zipEq (itys1, itys2)))
     handle _ => false)
  | eqItyseq (ITYSEQDEP (ityseq1, _), ityseq2) = eqItyseq (ityseq1, ityseq2)
  | eqItyseq (ityseq1, ITYSEQDEP (ityseq2, _)) = eqItyseq (ityseq1, ityseq2)
  | eqItyseq _ = false

and eqOrseq (ORSEQ (ityseq1, _, _), ORSEQ (ityseq2, _, _)) = eqItyseq (ityseq1, ityseq2)

fun allEqItys [] = true
  | allEqItys (ity :: itys) =
    foldr (fn (ity', b) => b andalso eqIty (ity, ity'))
	  true
	  itys

fun isConcreteIty (ITYVAR _) = false
  | isConcreteIty (ITYETV _) = false
  | isConcreteIty (ITYCON (ityseq, itycon)) = isConcreteItyseq ityseq andalso isConcreteItycon itycon
  | isConcreteIty (ITYOR _) = false
  | isConcreteIty (ITYTYP _) = false
  | isConcreteIty (ITYDEP (ity, _)) = isConcreteIty ity

and isConcreteItyseq (ITYSEQVAR _) = false
  | isConcreteItyseq (ITYSEQSEQ (itys, _)) = List.all isConcreteIty itys
  | isConcreteItyseq (ITYSEQDEP (ityseq, _)) = isConcreteItyseq ityseq

and isConcreteItycon (ITYCONVAR _) = false
  | isConcreteItycon (ITYCONNAM _) = true
  | isConcreteItycon (ITYCONDEP (itycon, _)) = isConcreteItycon itycon

fun isNullEnv ENVNUL                      = true
  | isNullEnv (ENVPOL (env, _))           = isNullEnv env
  | isNullEnv (ENVAPP (env1, env2))       = isNullEnv env1 andalso isNullEnv env2
  | isNullEnv (ENVSET envs)               = List.all isNullEnv envs
  | isNullEnv (ENVLOC (env1, env2))       = isNullEnv env1 andalso isNullEnv env2
  | isNullEnv (ENVDEP (env, _))           = isNullEnv env
  | isNullEnv (ENVFIL (f, env1, env2, _)) = isNullEnv env1 andalso isNullEnv env2
  | isNullEnv _                           = false

fun isEmptyBound (ityvarset, etyvarset, idorset) =
    ITVS.isEmpty ityvarset
    andalso
    ETVS.isEmpty etyvarset
    andalso
    OTVS.isEmpty idorset

fun isEmptyCsSch (CSSCH (overseq, CSSUB subs)) =
    List.null overseq andalso List.null subs


(* ------ TOSTRING ------ *)

fun toStringEnvvar    envvar    = Int.toString envvar
fun toStringItyvar    ityvar    = Int.toString ityvar
fun toStringItyseqvar ityseqvar = Int.toString ityseqvar
fun toStringTyconvar  tyconvar  = Int.toString tyconvar
fun toStringItokvar   itokvar   = Int.toString itokvar
fun toStringTyconname tyconname = tyconname

fun toStringId id = id

fun toStringIdor idor = Int.toString idor

fun toStringEqTv (EQ lab) = "EQ(" ^ D.toStringLabel lab ^ ")"
  | toStringEqTv NEQ      = "NEQ"

fun toStringEqTc eq = Bool.toString eq

fun toStringIty (ITYVAR (ityvar, eq))     = "ITYVAR(" ^ toStringItyvar ityvar ^ "," ^ toStringEqTv eq ^ ")"
  | toStringIty (ITYETV (etyvar, lab))    = "ITYETV(" ^ etyvar ^ "," ^ D.toStringLabel lab ^ ")"
  | toStringIty (ITYCON (ityseq, itycon)) = "ITYCON(" ^ toStringItyseq ityseq ^ "," ^ toStringItycon itycon ^ ")"
  | toStringIty (ITYOR orseq)             = "ITYOR("  ^ toStringOrseq orseq ^ ")"
  | toStringIty (ITYTYP (ity, lab))       = "ITYTYP(" ^ toStringIty ity ^ "," ^ D.toStringLabel lab ^ ")"
  | toStringIty (ITYDEP (ity, dep))       = "ITYDEP(" ^ toStringIty ity ^ "," ^ D.toStringDep dep ^ ")"
  (*| toStringIty (ITYDPP (ity1, ity2))       = "ITYDPP(" ^ toStringIty ity1 ^ "," ^ toStringIty ity2 ^ ")"*)

and toStringOrseq (ORSEQ (ityseq, idor, lab)) = "ORSEQ(" ^ toStringItyseq ityseq ^ "," ^ toStringIdor idor ^ "," ^ D.toStringLabel lab ^ ")"

(*and toStringItoken (ITOKVAR itokvar)     =
    "ITOKVAR(" ^ toStringItokvar itokvar ^ ")"
  | toStringItoken (ITOKITY ity)         =
    "ITOKITY(" ^ toStringIty ity ^ ")"
  | toStringItoken (ITOKDEP (itok, dep)) =
    "ITOKDEP(" ^ toStringItoken itok ^ "," ^ toStringDep dep ^ ")"*)

and toStringItyseq (ITYSEQVAR ityseqvar)     =
    "ITYSEQVAR(" ^ toStringItyseqvar ityseqvar ^ ")"
  | toStringItyseq (ITYSEQSEQ (itys, lab))   =
    "ITYSEQSEQ(" ^ toStringItyList itys ^ "," ^ D.toStringLabel lab ^ ")"
  | toStringItyseq (ITYSEQDEP (ityseq, dep)) =
    "ITYSEQDEP(" ^ toStringItyseq ityseq ^ "," ^ D.toStringDep dep ^ ")"

and toStringItyList itys = T.listToString toStringIty itys

and toStringItycon (ITYCONVAR tyconvar)               =
    "ITYCONVAR(" ^ toStringTyconvar tyconvar ^ ")"
  | toStringItycon (ITYCONNAM (tyconname, eq, label)) =
    "ITYCONNAM(" ^ toStringTyconname tyconname ^ "," ^ toStringEqTc eq ^ "," ^ D.toStringLabel label ^ ")"
  (*| toStringItycon (ITYCONFUN (ityseq, ity))          =
    "ITYCONFUN(" ^ toStringItyseq ityseq ^ "," ^ toStringIty ity ^ ")"*)
  | toStringItycon (ITYCONDEP (itycon, dep))          =
    "ITYCONDEP(" ^ toStringItycon itycon ^ "," ^ D.toStringDep dep ^ ")"

fun toStringItvs  set = T.fmt {init = "{", final = "}", sep = ",", fmt = toStringItyvar} (ITVS.listItems set)
fun toStringEtvs  set = T.fmt {init = "{", final = "}", sep = ",", fmt = (fn x => x)}    (ETVS.listItems set)
fun toStringIdors set = T.fmt {init = "{", final = "}", sep = ",", fmt = toStringIdor}   (OTVS.listItems set)

fun toStringMarker marker = Int.toString marker

fun toStringErrGen PARSE   = "PARSE"
  | toStringErrGen REBOUND = "REBOUND"
  | toStringErrGen SYNTAX  = "SYNTAX"

fun toStringTvbindKind EXPLICIT = "EXPLICIT"
  | toStringTvbindKind IMPLICIT = "IMPLICIT"

fun toStringPolMark TOP = "TOP"
  | toStringPolMark NES = "NES"
  | toStringPolMark CON = "CON"

fun toStringOvenames ovenames =
    T.listToString (fn NONE => "NONE" | SOME name => "SOME(" ^ name ^ ")") ovenames

fun toStringLocKind EQD = "EQD"
  | toStringLocKind OTH = "OTH"

fun toStringKItyvar (tv, kind) = "(" ^ toStringItyvar tv ^ "," ^ toStringLocKind kind ^ ")"

fun toStringBindKind PRELUDE  = "PRELUDE"
  | toStringBindKind USERCODE = "USERCODE"

fun toStringEnv (ENVVAR envvar)                 = "ENVVAR(" ^ toStringEnvvar envvar ^ ")"
  | toStringEnv (ENVBIN (bind, kind))           = "ENVBIN(" ^ toStringBind   bind   ^ toStringBindKind kind ^ ")"
  | toStringEnv (ENVCST cst)                    = "ENVCST(" ^ toStringCst    cst    ^ ")"
  | toStringEnv (ENVSUB sub)                    = "ENVSUB(" ^ toStringSub    sub    ^ ")"
  | toStringEnv (ENVACC acc)                    = "ENVACC(" ^ toStringAcc    acc    ^ ")"
  | toStringEnv (ENVPOL (env, mrk))             = "ENVPOL(" ^ toStringEnv env ^ "," ^ toStringPolMark mrk ^ ")"
  | toStringEnv (ENVAPP (env1, env2))           = "ENVAPP(" ^ toStringEnv env1 ^ ",\n" ^ toStringEnv env2 ^ ")"
  | toStringEnv (ENVSET envs)                   = "ENVSET(" ^ T.fmt {init = "", final = "", sep = ",", fmt = toStringEnv} envs ^ ")"
  | toStringEnv (ENVLOC (env1, env2))           = "ENVLOC(" ^ toStringEnv env1 ^ ",\n" ^ toStringEnv env2 ^ ")"
  | toStringEnv (ENVDEP (env, dep))             = "ENVDEP(" ^ toStringEnv env ^ "," ^ D.toStringDep dep ^ ")"
  | toStringEnv (ENVFIL (f, env1, env2, label)) = "ENVFIL(" ^ toStringEnv env1 ^ "," ^ toStringEnv env2 ^ "," ^ D.toStringLabel label ^ ")"
  | toStringEnv (ENVERR (st, kind))             = "ENVERR(" ^ st ^ "," ^ toStringErrGen kind ^ ")"
  | toStringEnv (ENVOVL id)                     = "ENVOVL(" ^ toStringId id ^ ")"
  | toStringEnv (ENVTOF id)                     = "ENVTOF(" ^ toStringId id ^ ")"
  | toStringEnv (ENVMRK marker)                 = "ENVMRK(" ^ toStringMarker marker ^ ")"
  | toStringEnv (ENVLAB (lab, tv))              = "ENVLAB(" ^ D.toStringLabel lab ^ "," ^ toStringKItyvar tv ^ ")"
  | toStringEnv (ENVOVE (id, names))            = "ENVOVE(" ^ toStringIdor id ^ "," ^ toStringOvenames names ^ ")"
  | toStringEnv ENVNUL                          = "ENVNUL"

and toStringCst (CSITY (ity1, ity2))       =
    "CSITY(" ^ toStringIty ity1 ^ "," ^ toStringIty ity2 ^ ")"
  | toStringCst (CSNAM (itycon1, itycon2)) =
    "CSNAM(" ^ toStringItycon itycon1 ^ "," ^ toStringItycon itycon2 ^ ")"
  | toStringCst (CSENV (envvar, env))      =
    "CSENV(" ^ toStringEnvvar envvar ^ "," ^ toStringEnv env ^ ")"
  | toStringCst (CSSEQ (ityseq1, ityseq2)) =
    "CSSEQ(" ^ toStringItyseq ityseq1 ^ "," ^ toStringItyseq ityseq2 ^ ")"
  (*| toStringCst (CSARI (itycon, ityseq)) =
    "CSSEQ(" ^ toStringItycon itycon ^ "," ^ toStringItyseq ityseq ^ ")"*)

and toStringSub (SUBITY (ity1, ity2))       =
    "SUBITY(" ^ toStringIty ity1 ^ "," ^ toStringIty ity2 ^ ")"
  | toStringSub (SUBNAM (itycon1, itycon2)) =
    "SUBNAM(" ^ toStringItycon itycon1 ^ "," ^ toStringItycon itycon2 ^ ")"
  | toStringSub (SUBSEQ (ityseq1, ityseq2)) =
    "SUBSEQ(" ^ toStringItyseq ityseq1 ^ "," ^ toStringItyseq ityseq2 ^ ")"
  | toStringSub (SUBIMP (ityseq1, ityseq2)) =
    "SUBIMP(" ^ toStringItyseq ityseq1 ^ "," ^ toStringItyseq ityseq2 ^ ")"

and toStringItyvars [] = ""
  | toStringItyvars [ityvar] = toStringItyvar ityvar
  | toStringItyvars (ityvar :: ityvars) = toStringItyvar ityvar ^ "," ^ toStringItyvars ityvars

and toStringBound (ityvarset, etyvarset, idorset) =
    "(" ^ toStringItvs  ityvarset ^
    "," ^ toStringEtvs  etyvarset ^
    "," ^ toStringIdors idorset   ^ ")"

and toStringOrseqPair (orseq1, orseq2, deps) =
    "(" ^ toStringOrseq  orseq1 ^
    "," ^ toStringOrseq  orseq2 ^
    "," ^ D.toStringDeps deps   ^ ")"

and toStringOverseq overseq = T.listToString toStringOrseqPair overseq

and toStringCssub (CSSUB subs) =
    "CSSUB(" ^ T.fmt {init = "", final = "", sep = ",", fmt = toStringSub} subs ^ ")"

and toStringCsSch (CSSCH (overseq, subs)) =
    "CSSCH(" ^ toStringOverseq overseq ^ "," ^ toStringCssub subs ^ ")"

and toStringScheme (VMONO (ityvar, deps, b)) =
    "VMONO(" ^ toStringItyvar ityvar ^ "," ^ D.toStringDeps deps ^ "," ^ Bool.toString b ^ ")"
  | toStringScheme (VPOLY (bound, (cs_scheme, ity))) =
    "VPOLY(" ^ toStringBound bound ^ "," ^ "(" ^ toStringCsSch cs_scheme ^ "," ^ toStringIty ity ^ "))"
  | toStringScheme (VODEC (bound, (idor, ity))) =
    "VODEC(" ^ toStringBound bound ^ "," ^ "(" ^ toStringIdor idor ^ "," ^ toStringIty ity ^ "))"

and toStringItyfun (bound, (ityseq, ity)) =
    "(" ^ toStringBound bound ^ ",(" ^ toStringItyseq ityseq ^ "," ^ toStringIty ity ^ "))"

and toStringTfvar (ityseqvar, ityvar) =
    "(" ^ toStringItyseqvar ityseqvar ^ "," ^ toStringItyvar ityvar ^ ")"

and toStringTvbind (ity, tvbind_kind) =
    "(" ^ toStringIty ity ^ "," ^ toStringTvbindKind tvbind_kind ^ ")"

and toStringAcc (ACCVID (vid,   ityvar)) = "ACCVID(" ^ toStringId vid   ^ "," ^ toStringItyvar ityvar ^ ")"
  | toStringAcc (ACCTYC (tycon, tfvar))  = "ACCTYC(" ^ toStringId tycon ^ "," ^ toStringTfvar  tfvar  ^ ")"
  | toStringAcc (ACCTYV (tyvar, ityvar)) = "ACCTYV(" ^ toStringId tyvar ^ "," ^ toStringItyvar ityvar ^ ")"
  | toStringAcc (ACCATM (atoms, ityvar)) = "ACCATM(" ^ toStringId atoms ^ "," ^ toStringItyvar ityvar ^ ")"
(*| toStringAcc (ACCTOK (token, ityvar))   = "ACCTOK(" ^ toStringId token ^ "," ^ toStringItyvar ityvar ^ ")"*)

and toStringBind (BINDVID (vid,   lab, scheme)) = "BINDVID(" ^ toStringId vid   ^ "," ^ D.toStringLabel lab ^ "," ^ toStringScheme scheme ^ ")"
  | toStringBind (BINDTYC (tycon, lab, ityfun)) = "BINDTYC(" ^ toStringId tycon ^ "," ^ D.toStringLabel lab ^ "," ^ toStringItyfun ityfun ^ ")"
  | toStringBind (BINDTYV (tyvar, lab, tvbind)) = "BINDTYV(" ^ toStringId tyvar ^ "," ^ D.toStringLabel lab ^ "," ^ toStringTvbind tvbind ^ ")"
  | toStringBind (BINDATM (atoms, lab, scheme)) = "BINDATM(" ^ toStringId atoms ^ "," ^ D.toStringLabel lab ^ "," ^ toStringScheme scheme ^ ")"
  | toStringBind (BINDVAR (var,   lab, ity))    = "BINDVAR(" ^ toStringId var   ^ "," ^ D.toStringLabel lab ^ "," ^ toStringIty    ity    ^ ")"
  (*| toStringBind (BINDTOK (token, ity))    = "BINDTOK(" ^ toStringId token ^ "," ^ toStringIty ity ^ ")"*)

and toStringEnvs envs =
    T.fmt {init = "", final = "", sep = ",", fmt = toStringEnv} envs

(* Pretty printing of a type scheme *)
fun ppIdor idor = "i" ^ Int.toString idor

fun ppIty renop f (ITYVAR (ityvar, NEQ)) =
    (case renop of
	 SOME ren =>
	 (case List.find (fn (x, _) => x = ityvar) ren of
	      NONE => "_"
	    | SOME (_, st) => st)
       | NONE => Int.toString ityvar)
  | ppIty renop f (ITYVAR (ityvar, EQ _)) =
    (case renop of
	 SOME ren =>
	 (case List.find (fn (x, _) => x = ityvar) ren of
	      NONE => "_"
	    | SOME (_, st) => st)
       | NONE => Int.toString ityvar)
  | ppIty renop f (ITYETV (etyvar, _)) = f etyvar
  | ppIty renop f (ITYCON (ityseq, itycon)) =
    let val tc = ppItycon itycon
    in case (ppItyseq renop f ityseq, tc) of
	   (NONE, _)           => "_ " ^ tc
	 | (SOME [], "*")      => "Unit"
	 | (SOME [], _)        => tc
	 | (SOME [a, b], "->") => "(" ^ a ^ ") -> " ^ b
	 | (SOME [a, b], "+")  => a ^ " + " ^ b
	 | (SOME xs, "*")      => T.fmt {init = "", final = "", sep = " * ", fmt = fn x => "(" ^ x ^ ")"} xs
	 (*| (SOME [x], _)       => x ^ " " ^ tc*)
	 | (SOME xs, _)        => T.fmt {init = "(", final = ")", sep = ",", fmt = fn x => x} xs ^ tc
    end
  | ppIty renop f (ITYOR orseq) = ppOrseq renop f orseq
  | ppIty renop f (ITYTYP (ity, _)) = "Type"
  | ppIty renop f (ITYDEP (ity, _)) = ppIty renop f ity

and ppOrseq renop f (ORSEQ (ityseq, idor, _)) =
    (case ppItyseq renop f ityseq of
	 NONE    => ppIdor idor ^ ":{_}"
       | SOME xs => ppIdor idor ^ ":" ^ T.fmt {init = "{", final = "}", sep = ",", fmt = fn x => x} xs)

and ppItyseq ren f (ITYSEQVAR _)           = NONE
  | ppItyseq ren f (ITYSEQSEQ (itys, _))   = SOME (map (fn ity => ppIty ren f ity) itys)
  | ppItyseq ren f (ITYSEQDEP (ityseq, _)) = ppItyseq ren f ityseq

and ppItycon (ITYCONVAR _)                 = "_"
  | ppItycon (ITYCONNAM (tyconname, _, _)) = toStringTyconname tyconname
  | ppItycon (ITYCONDEP (itycon, _))       = ppItycon itycon
(*| ppItycon (ITYCONFUN (ityseq, ity)) ren = "\206\155" ^ ppItyseq ityseq ren ^ "." ^ ppIty ity ren*)

fun ppOrseqPair iren f (orseq1, orseq2, deps) =
    "(" ^ ppOrseq iren f orseq1 ^ "=" ^ ppOrseq iren f orseq2 ^ ")"

fun ppOverseq iren f overseq =
    T.listToString (fn pair => ppOrseqPair iren f pair) overseq

fun ppItyseq' iren f ityseq =
    case ppItyseq iren f ityseq of
	NONE => "_"
      | SOME list => T.fmt {init = "(", final = ")", sep = ",", fmt = fn x => x} list

fun ppSub iren f (SUBITY (ity1, ity2)) = "(" ^ ppIty     iren f ity1 ^ " < " ^ ppIty     iren f ity2 ^ ")"
  | ppSub iren f (SUBNAM (nam1, nam2)) = "(" ^ ppItycon         nam1 ^ " < " ^ ppItycon         nam2 ^ ")"
  | ppSub iren f (SUBSEQ (seq1, seq2)) = "(" ^ ppItyseq' iren f seq1 ^ " < " ^ ppItyseq' iren f seq2 ^ ")"
  | ppSub iren f (SUBIMP (seq1, seq2)) = "(" ^ ppItyseq' iren f seq1 ^ " < " ^ ppItyseq' iren f seq2 ^ ")"

fun ppSub' sub = ppSub NONE (fn x => x) sub

fun ppIty' ity = ppIty NONE (fn x => x)  ity

fun ppIty'' ity f = ppIty NONE f ity

fun ppItyseq' ityseq =
    case ppItyseq NONE (fn x => x) ityseq of
	SOME xs => T.listToString (fn x => x) xs
      | NONE    => "_"

fun ppCsSub iren f (CSSUB subs) =
    T.listToString (fn sub => ppSub iren f sub) subs

fun ppCsSch iren f (CSSCH (overseq, cssub)) =
    "(" ^ ppOverseq iren f overseq ^ "," ^ ppCsSub iren f cssub ^ ")"

fun ppBound (itvs, etvs, otvs) =
    let val ilist  = ITVS.listItems itvs
	val elist  = ETVS.listItems etvs
	val olist  = OTVS.listItems otvs
	val iren   = map (fn ityvar => (ityvar, "'a" ^ toStringItyvar ityvar)) ilist
	val ibound = T.fmt {init = "{", final = "}", sep = ",", fmt = fn (_, tv) => tv} iren
	val ebound = T.fmt {init = "{", final = "}", sep = ",", fmt = fn x => x} elist
	val obound = T.fmt {init = "{", final = "}", sep = ",", fmt = ppIdor} olist
	val bound  = ibound ^ ebound ^ obound
    in (bound, iren)
    end

fun ppScheme (VMONO (ityvar, deps, b)) = "_"
  | ppScheme (VPOLY (bound, (cssch, ity))) =
    let val (bnd, iren) = ppBound bound
	val body  = ppIty (SOME iren) (fn x => x) ity
	val ovseq = if isEmptyCsSch cssch then "" else ppCsSch (SOME iren) (fn x => x) cssch
    in "\226\136\128" ^ bnd ^ "." ^ ovseq ^ body
    end
  | ppScheme (VODEC (bound, (idor, ity))) =
    let val (bnd, iren) = ppBound bound
	val body = ppIty (SOME iren) (fn x => x) ity
    in "\226\136\128" ^ bnd ^ "." ^ body
    end


(* ------ MARKERS ------ *)

fun next x = let val y = !x in (x := y + 1; y) end

val marker = ref 1
fun resetMarker () = marker := 1
fun nextMarker () = next marker


(* ------ VARIABLES ------ *)

val envvar    = ref 1
val tyconvar  = ref 1
val ityvar    = ref 1
val ityseqvar = ref 1
val idor      = ref 1

(*val tyconnameArrow = 0 (* This is the 'ar' of the paper. *)*)

val envvarFresh    = 0
val tyconvarFresh  = 0
val ityvarFresh    = 0
val ityseqvarFresh = 0
val idorFresh      = 0

fun isEnvvarFresh   ev  = (ev  = 0)
fun isTyconvarFresh tcv = (tcv = 0)
fun isItyvarFresh   tv  = (tv  = 0)
fun isIdorFresh     or  = (or  = 0)

fun nextEnvvar    () = next envvar
fun nextTyconvar  () = next tyconvar
fun nextItyvar    () = next ityvar
fun nextItyseqvar () = next ityseqvar
fun nextIdor      () = next idor

fun eqEnvvar    (x : int, y) = (x = y)
fun eqTyconvar  (x : int, y) = (x = y)
fun eqItyvar    (x : int, y) = (x = y)
fun eqItyseqvar (x : int, y) = (x = y)
fun eqIdor      (x : int, y) = (x = y)

fun resetVars () = (envvar    := 1;
		    tyconvar  := 1;
		    ityseqvar := 1;
		    ityvar    := 1;
		    idor      := 1)

fun reset () = (resetVars (); resetMarker (); ())


(* ------ MERGERS ------ *)

fun mergeBound ((ityvarset1, etyvarset1, idorset1),
		(ityvarset2, etyvarset2, idorset2)) =
    (ITVS.union (ityvarset1, ityvarset2),
     ETVS.union (etyvarset1, etyvarset2),
     OTVS.union (idorset1,   idorset2))

fun mergeEqTv (eqtv, NEQ)        = eqtv
  | mergeEqTv (NEQ, eqtv)        = eqtv
  | mergeEqTv (EQ lab1, EQ lab2) = EQ lab1
(* NOTE: it seems enough to keep only one label. *)


(* ------ TYPE CONSTRUCTORS ------ *)

val tyconnameArrow  = "->"
val tyconnameTuple  = "*"
val tyconnameDisjU  = "+"
val tyconnameInt    = "Int"
val tyconnameBool   = "Bool"
val tyconnameReal   = "Real"
val tyconnameList   = "List"
val tyconnameClass  = "Class"
val tyconnameBag    = "Bag"
val tyconnameInstr  = "Interface"
val tyconnameMsg    = "Msg"
val tyconnameUnit   = "Unit"
val tyconnameAtom   = "Atom"
val tyconnameToken  = "Tok"
val tyconnameTop    = "Top"
val tyconnameType   = "Type"
val tyconnameProp   = "Prop"
val tyconnameEvent  = "Event"
val tyconnameEO     = "_EO"
val tyconnameMFUN   = "_MSGFUN"
val tyconnameNat    = "Nat"
val tyconnameLoc    = "Loc"
val tyconnameDeq    = "Deq"
val tyconnameString = "String"
val tyconnameSet    = "Set"
val tyconnameMap    = "Map"

val tyconnames =
    [tyconnameArrow,
     tyconnameTuple,
     tyconnameDisjU,
     tyconnameInt,
     tyconnameBool,
     tyconnameReal,
     tyconnameList,
     tyconnameClass,
     tyconnameBag,
     tyconnameInstr,
     tyconnameMsg,
     tyconnameUnit,
     tyconnameAtom,
     tyconnameToken,
     tyconnameTop,
     tyconnameType,
     tyconnameProp,
     tyconnameEvent,
     tyconnameEO,
     tyconnameMFUN,
     tyconnameNat,
     tyconnameLoc,
     tyconnameDeq,
     tyconnameString,
     tyconnameSet,
     tyconnameMap]

val eqtyconTuple  = true
val eqtyconDisjU  = true
val eqtyconInt    = true
val eqtyconBool   = true
val eqtyconList   = true
val eqtyconBag    = true
val eqtyconUnit   = true
val eqtyconAtom   = true
val eqtyconToken  = true
val eqtyconTop    = true
val eqtyconLoc    = true
val eqtyconNat    = true
val eqtyconString = true
val eqtyconEvent  = true
val eqtyconInstr  = false
val eqtyconEO     = false
val eqtyconMFUN   = false
val eqtyconMsg    = false (* equality on messages is in general undecidable *)
val eqtyconArrow  = false (* equality on functions is not decidable         *)
val eqtyconReal   = false (* equality on reals is not decidable             *)
val eqtyconClass  = false (* equality on classes is not decidable           *)
val eqtyconType   = false (* equality on types is not decidable             *)
val eqtyconProp   = false (* equality on propositions is not decidable      *)
val eqtyconDeq    = false (* an equality decider is a function              *)
val eqtyconSet    = false (* equality on sets is in general undecidable     *)
val eqtyconMap    = false (* equality on maps is in general undecidable     *)

val subList =
    [(tyconnameNat,  tyconnameInt),
     (tyconnameList, tyconnameBag)]

fun destArrowType (ITYCON (ITYSEQSEQ ([ity1, ity2], lab1),
			   ITYCONNAM (tyconname, eq, lab2))) =
    if tyconname = tyconnameArrow
    then SOME (ity1, ity2)
    else NONE
  | destArrowType _ = NONE

fun destArrowsType (ity as ITYCON (ITYSEQSEQ ([ity1, ity2], lab1),
				   ITYCONNAM (tyconname, eq, lab2))) =
    if tyconname = tyconnameArrow
    then let val (lst, tail) = destArrowsType ity2
	 in (ity1 :: lst, tail)
	 end
    else ([], ity)
  | destArrowsType ity = ([], ity)

fun destBagType (ITYCON (ITYSEQSEQ ([ity], lab1),
			 ITYCONNAM (tyconname, eq, lab2))) =
    if tyconname = tyconnameBag
    then SOME ity
    else NONE
  | destBagType _ = NONE

fun destDeqType (ITYCON (ITYSEQSEQ ([ity], lab1),
			 ITYCONNAM (tyconname, eq, lab2))) =
    if tyconname = tyconnameDeq
    then SOME ity
    else NONE
  | destDeqType _ = NONE

fun destClassType (ITYCON (ITYSEQSEQ ([ity], lab1),
			   ITYCONNAM (tyconname, eq, lab2))) =
    if tyconname = tyconnameClass
    then SOME ity
    else NONE
  | destClassType _ = NONE

fun mk_new_tyvar   ()  = ITYVAR (nextItyvar (), NEQ)
fun mk_new_eqtyvar lab = ITYVAR (nextItyvar (), EQ lab)

fun mk_tyvar   ityvar     = ITYVAR (ityvar, NEQ)
fun mk_eqtyvar ityvar lab = ITYVAR (ityvar, EQ lab)

fun mk_typecon_gen label name eq = ITYCONNAM (name, eq, label)

fun mk_type_gen tys label name eq =
    let val seq = ITYSEQSEQ (tys ,label)
	val n   = List.length tys
    in ITYCON (seq, mk_typecon_gen label name eq)
    end

fun mk_typefun_gen n label name eq =
    let val itvs  = List.tabulate (n, fn _ => nextItyvar ())
	(* We take the type variables as not requiring equality *)
	val itys  = List.map (fn tv => ITYVAR (tv, NEQ)) itvs
	val seq   = ITYSEQSEQ (itys, label)
	val ity   = mk_type_gen itys label name eq
	val bound = (ITVS.addList (ITVS.empty, itvs), ETVS.empty, OTVS.empty)
    in (bound, (seq, ity))
    end

(*(* NOTE: This is a special type function for Msg so that we can record
 * the type of what's inside a message.
 * The function is as follows: forall 'a. Lam[]. 'a Msg *)
fun mk_typefun_msg label =
    let val tv    = nextItyvar ()
	val itvs  = [tv]
	val itys  = [ITYVAR (tv, NEQ)]
	val seq   = ITYSEQSEQ ([], label)
	val ity   = mk_type_gen itys label tyconnameMsg eqtyconMsg
	val bound = (ITVS.singleton tv, ETVS.empty)
    in (bound, (seq, ity))
    end*)

fun mk_type_bin ty1 ty2 label name eq = mk_type_gen [ty1, ty2] label name eq
fun mk_type_un  ty      label name eq = mk_type_gen [ty]       label name eq
fun mk_type_nul         label name eq = mk_type_gen []         label name eq

fun mk_type_arrow (ty1, ty2) label = mk_type_bin ty1 ty2 label tyconnameArrow eqtyconArrow
fun mk_type_disju (ty1, ty2) label = mk_type_bin ty1 ty2 label tyconnameDisjU eqtyconDisjU
fun mk_type_map   (ty1, ty2) label = mk_type_bin ty1 ty2 label tyconnameMap   eqtyconMap

fun mk_type_class ty label = mk_type_un ty label tyconnameClass eqtyconClass
fun mk_type_bag   ty label = mk_type_un ty label tyconnameBag   eqtyconBag
fun mk_type_list  ty label = mk_type_un ty label tyconnameList  eqtyconList
fun mk_type_deq   ty label = mk_type_un ty label tyconnameDeq   eqtyconDeq
fun mk_type_set   ty label = mk_type_un ty label tyconnameSet   eqtyconSet

fun mk_type_int    label = mk_type_nul label tyconnameInt    eqtyconInt
fun mk_type_bool   label = mk_type_nul label tyconnameBool   eqtyconBool
fun mk_type_real   label = mk_type_nul label tyconnameReal   eqtyconReal
fun mk_type_atom   label = mk_type_nul label tyconnameAtom   eqtyconAtom
fun mk_type_token  label = mk_type_nul label tyconnameToken  eqtyconToken
fun mk_type_top    label = mk_type_nul label tyconnameTop    eqtyconTop
fun mk_type_prop   label = mk_type_nul label tyconnameProp   eqtyconProp
fun mk_type_event  label = mk_type_nul label tyconnameEvent  eqtyconEvent
fun mk_type_eo     label = mk_type_nul label tyconnameEO     eqtyconEO
fun mk_type_msgfun label = mk_type_nul label tyconnameMFUN   eqtyconMFUN
fun mk_type_nat    label = mk_type_nul label tyconnameNat    eqtyconNat
fun mk_type_loc    label = mk_type_nul label tyconnameLoc    eqtyconLoc
fun mk_type_msg    label = mk_type_nul label tyconnameMsg    eqtyconMsg
fun mk_type_string label = mk_type_nul label tyconnameString eqtyconString
fun mk_type_type   label = mk_type_nul label tyconnameType   eqtyconType
fun mk_type_instr  label = mk_type_nul label tyconnameInstr  eqtyconInstr

fun mk_type_tuple tys label = mk_type_gen tys label tyconnameTuple eqtyconTuple

fun mk_type_unit label = mk_type_tuple [] label

fun mk_type_or ityseq idor label = ITYOR (ORSEQ (ityseq, idor, label))

fun mk_typecon sv tc label = ITYCON (ITYSEQVAR sv, ITYCONVAR tc)

(*(* TODO: this will have to change to the type: Atom List {*} T : Type {*} T
 * var is T in here.
 * Msg is now again a type constructor because we have a constructor of messages. *)
fun mk_type_msg var label = (* mk_type_nul label tyconnameMsg *)
    let val atom  = mk_type_atom label
	val alist = mk_type_list atom label
	val v2    = ITYVAR (nextItyvar ())
	val v3    = ITYVAR (nextItyvar ())
    in mk_type_tuple [alist, v2, v3] label
    end*)

(*fun mk_typecon_msg var label =
    ITYCONFUN (ITYSEQSEQ ([], label), mk_type_msg var label)*)

fun isArrow (ITYCONVAR _) = NONE
  | isArrow (ITYCONNAM (name, _, _)) = SOME (name = tyconnameArrow)
  | isArrow (ITYCONDEP (itycon, _)) = isArrow itycon

fun isItyconType (ITYCONVAR _) = false
  | isItyconType (ITYCONNAM (name, _, _)) = (name = tyconnameType)
  | isItyconType (ITYCONDEP (itycon, _)) = isItyconType itycon

fun isItyType (ITYCON (ityseq, itycon)) = isItyconType itycon
  | isItyType (ITYTYP _) = true
  | isItyType (ITYDEP (ity, dep)) = isItyType ity
  | isItyType _ = false

fun isItyconGen tyconname (ITYCONVAR _) = false
  | isItyconGen tyconname  (ITYCONNAM (name, _, _)) = (name = tyconname)
  | isItyconGen tyconname (ITYCONDEP (itycon, _)) = isItyconGen tyconname itycon

fun isItyGen tyconname (ITYCON (ityseq, itycon)) = isItyconGen tyconname itycon
  | isItyGen tyconname (ITYDEP (ity, dep)) = isItyGen tyconname ity
  | isItyGen _ _ = false

val isItyEvent = isItyGen tyconnameEvent
val isItyProp  = isItyGen tyconnameProp
val isItyBool  = isItyGen tyconnameBool


(* ------ ENVIRONMENT CONSTRUCTORS ------ *)

fun mk_new_bound () = (ITVS.empty, ETVS.empty, OTVS.empty)
fun mk_new_ityvars_bound ityvars = (ITVS.addList (ITVS.empty, ityvars), ETVS.empty, OTVS.empty)

fun mk_new_cs_scheme () = CSSCH ([], CSSUB [])

fun mk_new_scheme   ityvars ity = VPOLY (mk_new_ityvars_bound ityvars, (mk_new_cs_scheme (), ity))
fun mk_new_oscheme  ityvar idor = VODEC (mk_new_bound (), (idor, mk_tyvar ityvar))
fun mk_new_vscheme  ityvar      = VMONO (ityvar, D.empty, false)
fun mk_new_vvscheme ityvar      = VMONO (ityvar, D.empty, true)
fun mk_new_ityfun   ityseq ity  = (mk_new_bound (), (ityseq, ity))
fun mk_new_tvbind   ityvar      = (ITYVAR (ityvar, NEQ), EXPLICIT)

fun mk_env_depcst cst label = ENVDEP (ENVCST cst, D.mk_dep label)
fun mk_env_depsub sub label = ENVDEP (ENVSUB sub, D.mk_dep label)
fun mk_env_depacc acc label = ENVDEP (ENVACC acc, D.mk_dep label)
fun mk_env_depvar var label = ENVDEP (ENVVAR var, D.mk_dep label)
fun mk_env_depbin bin label = ENVDEP (ENVBIN (bin, USERCODE), D.mk_dep label)
fun mk_env_deppre bin label = ENVDEP (ENVBIN (bin, PRELUDE),  D.mk_dep label)
fun mk_env_deplab itv label = ENVDEP (ENVLAB (label, itv), D.mk_dep label)
fun mk_env_depove idor ovenames label = ENVDEP (ENVOVE (idor, ovenames), D.mk_dep label)

fun isNullEnv (ENVAPP (env1, env2)) = isNullEnv env1 andalso isNullEnv env2
  | isNullEnv (ENVSET envs) = List.all isNullEnv envs
  | isNullEnv ENVNUL  = true
  | isNullEnv _ = false

fun mk_env_app (env1, env2) =
    if isNullEnv env1
    then env2
    else if isNullEnv env2
    then env1
    else ENVAPP (env1, env2)

fun list2env []        = ENVNUL
  | list2env [x]       = if isNullEnv x then ENVNUL else x
  | list2env (x :: xs) = mk_env_app (x, list2env xs)

fun mk_env_loc (env1, env2) =
    if isNullEnv env1
    then env2
    else ENVLOC (env1, env2)

and sub2cst (SUBITY (ity1, ity2))       = CSITY (ity1, ity2)
  | sub2cst (SUBNAM (itycon1, itycon2)) = CSNAM (itycon1, itycon2)
  | sub2cst (SUBSEQ (ityseq1, ityseq2)) = CSSEQ (ityseq1, ityseq2)
  | sub2cst (SUBIMP (ityseq1, ityseq2)) = CSSEQ (ityseq1, ityseq2)


(* ------ SET MANIPULATIONS ------ *)

val emptyITVS = ITVS.empty
val emptyETVS = ETVS.empty
val emptyOTVS = OTVS.empty

val foldrITVS = ITVS.foldr
val foldrETVS = ETVS.foldr
val foldrOTVS = OTVS.foldr

val rmITVS    = ITVS.delete
val rmETVS    = ETVS.delete
val rmOTVS    = OTVS.delete

val isinITVS  = ITVS.member
val isinETVS  = ETVS.member
val isinOTVS  = OTVS.member

val isEmptyITVS  = ITVS.isEmpty
val isEmptyETVS  = ETVS.isEmpty
val isEmptyOTVS  = OTVS.isEmpty

val removeITVS = ITVS.delete
val removeETVS = ETVS.delete
val removeOTVS = OTVS.delete

val unionITVS  = ITVS.union
val unionETVS  = ETVS.union
val unionOTVS  = OTVS.union

val listItemsITVS  = ITVS.listItems
val listItemsETVS  = ETVS.listItems
val listItemsOTVS  = OTVS.listItems

val addListETVS = ETVS.addList


(* ------ MAP MANIPULATIONS ------ *)

val emptyITVM     = ITVM.empty
val emptyETVM     = ETVM.empty

val unionWithITVM = ITVM.unionWith
val unionWithETVM = ETVM.unionWith

val insertITVM    = ITVM.insert
val findITVM      = ITVM.find

val singletonITVM = ITVM.singleton
val singletonETVM = ETVM.singleton

val foldriITVM    = ITVM.foldri
val foldriETVM    = ETVM.foldri

(*fun labelPattern (env as ENVVAR _) _ = env
  | labelPattern (env as ENVBIN _) lab = ENVDEP (env, D.DEPL lab)
  | labelPattern (env as ENVACC (ACCVID _)) _ = env
  | labelPattern (env as ENVACC (ACCTYC _)) _ = env
  | labelPattern (env as ENVACC (ACCTYV _)) _ = env
  | labelPattern (env as ENVACC (ACCTOK _)) _ = env
  | labelPattern (env as ENVCST _) _ = env
  | labelPattern (env as ENVPOL _) _ = env
  | labelPattern (ENVAPP (env1, env2)) lab = ENVAPP (labelPattern env1 lab, labelPattern env2 lab)
  | labelPattern (ENVLOC (env1, env2)) lab = ENVLOC (labelPattern env1 lab, labelPattern env2 lab)
  | labelPattern (ENVDEP (env, dep)) lab = ENVDEP (labelPattern env lab, dep)
  | labelPattern (env as ENVNUL)   _ = env
  | labelPattern (env as ENVERR _) _ = env
  | labelPattern (ENVFIL (env, label)) lab = ENVFIL (labelPattern env lab, label)*)


(* ------ ACCESSORS ------ *)

fun getIdOrseq (ORSEQ (_, id, _)) = id


(* ------ DEPENDENCY EXTRATION ------ *)

(* Extract all the dependencies of a type *)
fun getDepsIty (ITYVAR _)                = D.empty
  | getDepsIty (ITYETV _)                = D.empty
  | getDepsIty (ITYCON (ityseq, itycon)) = D.union (getDepsItyseq ityseq, getDepsItycon itycon)
  | getDepsIty (ITYOR  orseq)            = getDepsOrseq orseq
  | getDepsIty (ITYTYP (ity, lab))       = getDepsIty ity
  | getDepsIty (ITYDEP (ity, dep))       = D.add (getDepsIty ity, dep)
  (*| getDepsIty (ITYDPP (ity1, ity2))     = D.union (getDepsIty ity1, getDepsIty ity2)*)

and getDepsOrseq (ORSEQ (ityseq, _, _))   = getDepsItyseq ityseq

and getDepsItycon (ITYCONVAR _)             = D.empty
  | getDepsItycon (ITYCONNAM _)             = D.empty
  (*| getDepsItycon (ITYCONFUN (ityseq, ity)) = D.union (getDepsItyseq ityseq, getDepsIty ity)*)
  | getDepsItycon (ITYCONDEP (itycon, dep)) = D.add (getDepsItycon itycon, dep)

(*and getDepsItoken (ITOKVAR _)           = D.empty
  | getDepsItoken (ITOKITY ity)         = getDepsIty ity
  | getDepsItoken (ITOKDEP (itok, dep)) = D.add (getDepsItoken itok, dep)*)

and getDepsItyseq (ITYSEQVAR _) = D.empty
  | getDepsItyseq (ITYSEQSEQ (itys, lab)) =
    foldr (fn (ity, set) => D.union (getDepsIty ity, set))
	  D.empty
	  itys
  | getDepsItyseq (ITYSEQDEP (ityseq, dep)) = D.add (getDepsItyseq ityseq, dep)

(*fun getDepsScheme (bind, (overseq, ity)) = getDepsIty ity*)

(* strip an itycon from its dependencies *)
fun stripItycon (ITYCONDEP (itycon, _)) = stripItycon itycon
  | stripItycon x = x

(* strip an ity from its dependencies *)
fun stripIty (ITYVAR ityvar)            = ITYVAR ityvar
  | stripIty (ITYETV etyvar)            = ITYETV etyvar
  | stripIty (ITYCON (ityseq, itycon))  = ITYCON (stripItyseq ityseq, stripItycon itycon)
  | stripIty (ITYOR  orseq)             = ITYOR (stripOrseq orseq)
  | stripIty (ITYTYP (ity, lab))        = ITYTYP (stripIty ity, lab)
  | stripIty (ITYDEP (ity, _))          = stripIty ity
  (*| stripIty (ITYDPP (ity1, ity2))      = ITYDPP (stripIty ity1, stripIty ity2)*)

and stripOrseq (ORSEQ (ityseq, id, lab)) = ORSEQ (stripItyseq ityseq, id, lab)

(*and stripItoken (ITOKVAR itokvar)   = ITOKVAR itokvar
  | stripItoken (ITOKITY ity)       = ITOKITY (stripIty ity)
  | stripItoken (ITOKDEP (itok, _)) = stripItoken itok*)

and stripItyseq (ITYSEQVAR ityseqvar)     = ITYSEQVAR ityseqvar
  | stripItyseq (ITYSEQSEQ (itys, lab))   = ITYSEQSEQ (map stripIty itys, lab)
  | stripItyseq (ITYSEQDEP (ityseq, dep)) = stripItyseq ityseq

(* Strip a binding from its dependencies *)
fun stripBind (ENVDEP (env, dep)) =
    let val (envbind, deps) = stripBind env
    in (envbind, D.add (deps, dep))
    end
  (*| stripBind (ENVBIN bind) = (ENVBIN bind, D.empty)*)
  | stripBind (ENVAPP (env1, env2)) =
    let val (envbind1, deps1) = stripBind env1
	val (envbind2, deps2) = stripBind env2
    in (ENVAPP (envbind1, envbind2), D.union (deps1, deps2))
    end
  | stripBind (ENVBIN (bind, kind)) = (ENVBIN (bind, kind), D.empty)
  | stripBind ENVNUL = (ENVNUL, D.empty) (* It can actally be a ENVNUL when filtering*)
  | stripBind _ = raise EH.Impossible "The environment should be a dependent binder"

fun stripSub (SUBITY (ity1,    ity2))    = SUBITY (stripIty    ity1,    stripIty    ity2)
  | stripSub (SUBNAM (itycon1, itycon2)) = SUBNAM (stripItycon itycon1, stripItycon itycon2)
  | stripSub (SUBSEQ (ityseq1, ityseq2)) = SUBSEQ (stripItyseq ityseq1, stripItyseq ityseq2)
  | stripSub (SUBIMP (ityseq1, ityseq2)) = SUBIMP (stripItyseq ityseq1, stripItyseq ityseq2)

(* extract the binders of an environment *)
fun get_binders_bind (BINDVID (id, _, _)) = ["BINDVID(" ^ id ^ ")"]
  | get_binders_bind (BINDTYC (id, _, _)) = ["BINDTYC(" ^ id ^ ")"]
  | get_binders_bind (BINDTYV (id, _, _)) = ["BINDTYV(" ^ id ^ ")"]
  | get_binders_bind (BINDATM (id, _, _)) = ["BINDATM(" ^ id ^ ")"]
  | get_binders_bind (BINDVAR (id, _, _)) = ["BINDVAR(" ^ id ^ ")"]

fun get_binders (ENVDEP (env, _))       = get_binders env
  | get_binders (ENVBIN (bind, _))      = get_binders_bind bind
  | get_binders (ENVAPP (env1, env2))   = get_binders env1 @ get_binders env2
  | get_binders (ENVSET envs)           = [] (*List.concat (map get_binders envs)*)
  | get_binders (ENVLOC (env, _))       = [] (*get_binders env*)
  | get_binders (ENVFIL (_, env, _, _)) = [] (*get_binders env*)
  | get_binders (ENVVAR _) = []
  | get_binders (ENVACC _) = []
  | get_binders (ENVCST _) = []
  | get_binders (ENVSUB _) = []
  | get_binders (ENVPOL _) = []
  | get_binders (ENVMRK _) = []
  | get_binders (ENVERR _) = []
  | get_binders (ENVOVL _) = []
  | get_binders (ENVLAB _) = []
  | get_binders (ENVOVE _) = []
  | get_binders (ENVTOF _) = []
  | get_binders ENVNUL     = []

(* Extracts the outer dependencies of a type *)
fun extractDepsIty (ITYDEP (ity, dep)) =
    let val (ity', deps) = extractDepsIty ity
    in (ity', D.add (deps, dep))
    end
  | extractDepsIty ity = (ity, D.empty)

fun extractDepsItyseq (ITYSEQDEP (ityseq, dep)) =
    let val (ityseq', deps) = extractDepsItyseq ityseq
    in (ityseq', D.add (deps, dep))
    end
  | extractDepsItyseq ityseq = (ityseq, D.empty)

fun extractDepsItycon (ITYCONDEP (itycon, dep)) =
    let val (itycon', deps) = extractDepsItycon itycon
    in (itycon', D.add (deps, dep))
    end
  | extractDepsItycon itycon = (itycon, D.empty)


(* ------ TYPE DECORATION ------ *)

(* Annotate with dependencies *)
fun applyDepsIty    ity    deps = D.foldl (fn (dep, ity)    => ITYDEP    (ity,    dep)) ity    deps
fun applyDepsItycon itycon deps = D.foldl (fn (dep, itycon) => ITYCONDEP (itycon, dep)) itycon deps
fun applyDepsItyseq ityseq deps = D.foldl (fn (dep, ityseq) => ITYSEQDEP (ityseq, dep)) ityseq deps
fun applyDepsEnv    env    deps = D.foldl (fn (dep, env)    => ENVDEP    (env,    dep)) env    deps

fun applyDepsOrseq (ORSEQ (ityseq, idor, lab)) deps = ORSEQ (applyDepsItyseq ityseq deps, idor, lab)

fun applyDepsOrseqPair (orseq1, orseq2, deps) deps' =
    (orseq1, orseq2, D.union (deps, deps'))

fun applyDepsOverseq overseq deps =
    map (fn pair => applyDepsOrseqPair pair deps) overseq

fun applyDepsSub (SUBITY (ity1, ity2)) deps = SUBITY (applyDepsIty    ity1 deps, applyDepsIty    ity2 deps)
  | applyDepsSub (SUBNAM (nam1, nam2)) deps = SUBNAM (applyDepsItycon nam1 deps, applyDepsItycon nam2 deps)
  | applyDepsSub (SUBSEQ (seq1, seq2)) deps = SUBSEQ (applyDepsItyseq seq1 deps, applyDepsItyseq seq2 deps)
  | applyDepsSub (SUBIMP (seq1, seq2)) deps = SUBIMP (applyDepsItyseq seq1 deps, applyDepsItyseq seq2 deps)

fun applyDepsCsSub (CSSUB subs) deps =
    CSSUB (map (fn sub => applyDepsSub sub deps) subs)

fun applyDepsCsScheme (CSSCH (overseq, cssub)) deps =
    CSSCH (applyDepsOverseq overseq deps, applyDepsCsSub cssub deps)

fun applyDepsScheme (VMONO (ityvar, deps0, b))    deps = VMONO (ityvar, D.union (deps0, deps), b)
  | applyDepsScheme (VPOLY (bound, (cssch, ity))) deps = VPOLY (bound, (applyDepsCsScheme cssch deps, applyDepsIty ity deps))
  | applyDepsScheme (VODEC (bound, (idor, ity)))  deps = VODEC (bound, (idor, applyDepsIty ity deps))
fun applyDepsItyfun (bound, (seq, ity))           deps = (bound, (applyDepsItyseq seq deps, applyDepsIty ity deps))
fun applyDepsTvbind (ity, kind)                   deps = (applyDepsIty ity deps, kind)


(* ------ SUBSTITUTION ------ *)

fun substituteIty (ity as ITYVAR (ityvar0, eq)) ityvar ity' =
    if ityvar0 = ityvar
    then ity'
    else ity
  | substituteIty (ity as ITYETV _) _ _ = ity
  | substituteIty (ity as ITYCON (ityseq, itycon)) ityvar ity' =
    ITYCON (substituteItyseq ityseq ityvar ity', itycon)
  | substituteIty (ity as ITYOR orseq) _ _ = raise Fail "substituteIty:OR"
  | substituteIty (ity as ITYTYP (ity0, lab)) ityvar ity' =
    ITYTYP (substituteIty ity0 ityvar ity', lab)
  | substituteIty (ity as ITYDEP (ity0, dep)) ityvar ity' =
    ITYDEP (substituteIty ity0 ityvar ity', dep)

and substituteItyseq (ityseq as ITYSEQVAR _) _ _ = ityseq
  | substituteItyseq (ityseq as ITYSEQSEQ (itys, lab)) ityvar ity' =
    ITYSEQSEQ (map (fn ity => substituteIty ity ityvar ity') itys, lab)
  | substituteItyseq (ityseq as ITYSEQDEP (ityseq0, dep)) ityvar ity' =
    ITYSEQDEP (substituteItyseq ityseq0 ityvar ity', dep)

fun substituteEty (ity as ITYVAR _) _ _ = ity
  | substituteEty (ity as ITYETV (etyvar0, lab)) etyvar ity' =
    if etyvar0 = etyvar
    then ity'
    else ity
  | substituteEty (ity as ITYCON (ityseq, itycon)) etyvar ity' =
    ITYCON (substituteEtyseq ityseq etyvar ity', itycon)
  | substituteEty (ity as ITYOR orseq) _ _ = raise Fail "substituteEty:OR"
  | substituteEty (ity as ITYTYP (ity0, lab)) etyvar ity' =
    ITYTYP (substituteEty ity0 etyvar ity', lab)
  | substituteEty (ity as ITYDEP (ity0, dep)) etyvar ity' =
    ITYDEP (substituteEty ity0 etyvar ity', dep)

and substituteEtyseq (ityseq as ITYSEQVAR _) _ _ = ityseq
  | substituteEtyseq (ityseq as ITYSEQSEQ (itys, lab)) etyvar ity' =
    ITYSEQSEQ (map (fn ity => substituteEty ity etyvar ity') itys, lab)
  | substituteEtyseq (ityseq as ITYSEQDEP (ityseq0, dep)) etyvar ity' =
    ITYSEQDEP (substituteEtyseq ityseq0 etyvar ity', dep)


(* ------ VARIABLE EXTRATION ------ *)

(* *** get the implicit type variables of types and type sequences *** *)

(* Extract the type variables from a type *)
fun getItyvarsIty (ITYVAR (ityvar, _)) = ITVS.singleton ityvar
  | getItyvarsIty (ITYETV etyvar)      = ITVS.empty
  | getItyvarsIty (ITYCON (ityseq, _)) = getItyvarsItyseq ityseq
  | getItyvarsIty (ITYOR  orseq)       = getItyvarsOrseq orseq
  | getItyvarsIty (ITYTYP (ity, _))    = getItyvarsIty ity
  | getItyvarsIty (ITYDEP (ity, _))    = getItyvarsIty ity

and getItyvarsOrseq (ORSEQ (ityseq, _, _)) = getItyvarsItyseq ityseq

and getItyvarsItyseq (ITYSEQVAR _)           = ITVS.empty
  | getItyvarsItyseq (ITYSEQSEQ (itys, _))   = List.foldr (fn (ity, set) => ITVS.union (getItyvarsIty ity, set)) ITVS.empty itys
  | getItyvarsItyseq (ITYSEQDEP (ityseq, _)) = getItyvarsItyseq ityseq


(* *** get the type sequence variables *** *)

(* Extract the type sequence variables from a type *)

fun getItyvarseqsIty (ITYVAR _)                = ITVS.empty
  | getItyvarseqsIty (ITYETV _)                = ITVS.empty
  | getItyvarseqsIty (ITYCON (ityseq, itycon)) = getItyvarseqsItyseq ityseq
  | getItyvarseqsIty (ITYOR  orseq)            = getItyvarseqsOrseq orseq
  | getItyvarseqsIty (ITYTYP (ity, _))         = getItyvarseqsIty ity
  | getItyvarseqsIty (ITYDEP (ity, _))         = getItyvarseqsIty ity

and getItyvarseqsOrseq (ORSEQ (ityseq, _, _)) = getItyvarseqsItyseq ityseq

and getItyvarseqsItyseq (ITYSEQVAR sv) = ITVS.singleton sv
  | getItyvarseqsItyseq (ITYSEQSEQ (itys, _)) = List.foldr (fn (ity, set) => ITVS.union (getItyvarseqsIty ity, set)) ITVS.empty itys
  | getItyvarseqsItyseq (ITYSEQDEP (ityseq, _)) = getItyvarseqsItyseq ityseq


(* *** get the implicit and explicit type variables *** *)

(* This is used to build polymorphic type binders at constraint solving. *)

fun getIEtyvarsIty (ITYVAR (ityvar, _)) =
    if ityvar = ityvarFresh
    then (ITVS.empty, ETVS.empty, OTVS.empty)
    else (ITVS.singleton ityvar, ETVS.empty, OTVS.empty)
  | getIEtyvarsIty (ITYETV (etyvar, _)) = (ITVS.empty, ETVS.singleton etyvar, OTVS.empty)
  | getIEtyvarsIty (ITYCON (ityseq, _)) = getIEtyvarsItyseq ityseq
  | getIEtyvarsIty (ITYOR  orseq)       = getIEtyvarsOrseq orseq
  | getIEtyvarsIty (ITYTYP (ity, _))    = getIEtyvarsIty ity
  | getIEtyvarsIty (ITYDEP (ity, _))    = getIEtyvarsIty ity

and getIEtyvarsOrseq (ORSEQ (ityseq, _, _)) = getIEtyvarsItyseq ityseq

and getIEtyvarsItyseq (ITYSEQVAR ityseqvar)   = (ITVS.empty, ETVS.empty, OTVS.empty)
  | getIEtyvarsItyseq (ITYSEQSEQ (itys, _))   =
    List.foldr (fn (ity, (itvs, etvs, otvs)) =>
		   let val (itvs', etvs', otvs') = getIEtyvarsIty ity
		   in (ITVS.union (itvs, itvs'),
		       ETVS.union (etvs, etvs'),
		       OTVS.union (otvs, otvs'))
		   end)
	       (ITVS.empty, ETVS.empty, OTVS.empty)
	       itys
  | getIEtyvarsItyseq (ITYSEQDEP (ityseq, _)) = getIEtyvarsItyseq ityseq


(* *** get the implicit and explicit type variables along with their dependencies *** *)

(* This is the part of our sanity checker that checks whether we can generate
 * equality deciders for all the equalities. *)

fun getDepsIEtyvarsIty (ITYVAR (ityvar, _)) = (ITVM.insert (ITVM.empty, ityvar, D.empty), ETVM.empty)
  | getDepsIEtyvarsIty (ITYETV (etyvar, _)) = (ITVM.empty, ETVM.insert (ETVM.empty, etyvar, D.empty))
  | getDepsIEtyvarsIty (ITYCON (ityseq, _)) = getDepsIEtyvarsItyseq ityseq
  | getDepsIEtyvarsIty (ITYOR  orseq)       = getDepsIEtyvarsOrseq orseq
  | getDepsIEtyvarsIty (ITYTYP (ity, _))    = getDepsIEtyvarsIty ity
  | getDepsIEtyvarsIty (ITYDEP (ity, dep))  =
    let val (itvm, etvm) = getDepsIEtyvarsIty ity
	val itvm' = ITVM.map (fn deps => D.add (deps, dep)) itvm
	val etvm' = ETVM.map (fn deps => D.add (deps, dep)) etvm
    in (itvm', etvm')
    end

and getDepsIEtyvarsOrseq (ORSEQ (ityseq, _, _)) = getDepsIEtyvarsItyseq ityseq

and getDepsIEtyvarsItyseq (ITYSEQVAR _)           = (ITVM.empty, ETVM.empty)
  | getDepsIEtyvarsItyseq (ITYSEQSEQ (itys, _))   =
    List.foldr (fn (ity, (itvm, etvm)) =>
		   let val (itvm1, etvm1) = getDepsIEtyvarsIty ity
		       val itvm2 = ITVM.unionWith (fn (deps1, deps2) => D.union (deps1, deps2)) (itvm, itvm1)
		       val etvm2 = ETVM.unionWith (fn (deps1, deps2) => D.union (deps1, deps2)) (etvm, etvm1)
		   in (itvm2, etvm2)
		   end)
	       (ITVM.empty, ETVM.empty)
	       itys
  | getDepsIEtyvarsItyseq (ITYSEQDEP (ityseq, dep)) =
    let val (itvm, etvm) = getDepsIEtyvarsItyseq ityseq
	val itvm' = ITVM.map (fn deps => D.add (deps, dep)) itvm
	val etvm' = ETVM.map (fn deps => D.add (deps, dep)) etvm
    in (itvm', etvm')
    end


(* *** get the implicit type variables of types and type sequences along with their dependencies *** *)

(* Extract the type variables from a type *)
fun getDepItyvarsIty (ITYVAR (ityvar, _)) = ITVM.insert (ITVM.empty, ityvar, D.empty)
  | getDepItyvarsIty (ITYETV etyvar)      = ITVM.empty
  | getDepItyvarsIty (ITYCON (ityseq, _)) = getDepItyvarsItyseq ityseq
  | getDepItyvarsIty (ITYOR  orseq)       = getDepItyvarsOrseq orseq
  | getDepItyvarsIty (ITYTYP (ity, _))    = getDepItyvarsIty ity
  | getDepItyvarsIty (ITYDEP (ity, dep))  = ITVM.map (fn deps => D.add (deps, dep)) (getDepItyvarsIty ity)

and getDepItyvarsOrseq (ORSEQ (ityseq, _, _)) = getDepItyvarsItyseq ityseq

and getDepItyvarsItyseq (ITYSEQVAR _)             = ITVM.empty
  | getDepItyvarsItyseq (ITYSEQSEQ (itys, _))     =
    List.foldr (fn (ity, set) =>
		   let val set' = getDepItyvarsIty ity
		   in ITVM.unionWith D.union (set', set)
		   end)
	       ITVM.empty
	       itys
  | getDepItyvarsItyseq (ITYSEQDEP (ityseq, dep)) = ITVM.map (fn deps => D.add (deps, dep)) (getDepItyvarsItyseq ityseq)

(*fun getDepItyvarsIty ity = ITVM.listItemsi (getDepItyvarsIty' ity)*)


(* If an implicit type variable is in the map it is mono and we apply the map
 * otherwise remember the type type variable, we're gonna quantify over it.
 * If it is an explicit type variable just remember it. *)
fun applyDepsIEtyvarsIty (ity as ITYVAR (ityvar, eq)) (imap, emap) pol_mrk =
    (case ITVM.find (imap, ityvar) of
	 SOME deps => ((ITVS.empty, ETVS.empty, OTVS.empty), applyDepsIty ity deps)
       | NONE      => ((ITVS.singleton ityvar, ETVS.empty, OTVS.empty), ity))
  | applyDepsIEtyvarsIty (ity as ITYETV (etyvar, _)) (imap, emap) pol_mrk =
    (case ETVM.find (emap, etyvar) of
	 SOME deps => ((ITVS.empty, ETVS.empty, OTVS.empty), applyDepsIty ity deps)
       | NONE      => ((ITVS.empty, ETVS.singleton etyvar, OTVS.empty), ity))
  | applyDepsIEtyvarsIty (ITYCON (ityseq, itycon)) map pol_mrk =
    let val ((iset, eset, oset), ityseq') = applyDepsIEtyvarsItyseq ityseq map pol_mrk
    in ((iset, eset, oset), ITYCON (ityseq', itycon))
    end
  | applyDepsIEtyvarsIty (ITYOR orseq) map pol_mrk =
    let val ((iset, eset, oset), orseq') = applyDepsIEtyvarOrseq orseq map pol_mrk
    in ((iset, eset, oset), ITYOR orseq')
    end
  | applyDepsIEtyvarsIty (ITYTYP (ity, lab)) map pol_mrk =
    let val ((iset, eset, oset), ity') = applyDepsIEtyvarsIty ity map pol_mrk
    in ((iset, eset, oset), ITYTYP (ity', lab))
    end
  | applyDepsIEtyvarsIty (ITYDEP (ity, dep)) map pol_mrk =
    let val ((iset, eset, oset), ity') = applyDepsIEtyvarsIty ity map pol_mrk
    in ((iset, eset, oset), ITYDEP (ity', dep))
    end

and applyDepsIEtyvarOrseq (ORSEQ (ityseq, id, lab)) map pol_mrk =
    let val ((iset, eset, oset), ityseq') = applyDepsIEtyvarsItyseq ityseq map pol_mrk
	val oset' =
	    case pol_mrk of
		TOP => oset
	      | NES => oset (*OTVS.add (oset, id)*)
	      | CON => OTVS.add (oset, id)
    in ((iset, eset, oset'), ORSEQ (ityseq', id, lab))
    end

and applyDepsIEtyvarsItyseq (ityseq as ITYSEQVAR _) map pol_mrk =
    ((ITVS.empty, ETVS.empty, OTVS.empty), ityseq)
  | applyDepsIEtyvarsItyseq (ITYSEQSEQ (itys, lab)) map pol_mrk =
    let val ((iset, eset, oset), itys') =
	    List.foldl (fn (ity, ((iset, eset, oset), itys)) =>
			   let val ((iset0, eset0, oset0), ity') =
				   applyDepsIEtyvarsIty ity map pol_mrk
			       val iset1 = ITVS.union (iset, iset0)
			       val eset1 = ETVS.union (eset, eset0)
			       val oset1 = OTVS.union (oset, oset0)
			   in ((iset1, eset1, oset1), itys @ [ity'])
			   end)
		       ((ITVS.empty, ETVS.empty, OTVS.empty), [])
		       itys
    in ((iset, eset, oset), ITYSEQSEQ (itys', lab))
    end
  | applyDepsIEtyvarsItyseq (ITYSEQDEP (ityseq, dep)) map pol_mrk =
    let val ((iset, eset, oset), ityseq') =
	    applyDepsIEtyvarsItyseq ityseq map pol_mrk
    in ((iset, eset, oset), ITYSEQDEP (ityseq', dep))
    end

end
