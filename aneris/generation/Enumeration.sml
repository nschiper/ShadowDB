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
 *  o File name:   Enumerations.sml
 *  o Description: EventML constraint solver/minimizer/enumerator.
 *)


structure Enum :> ENUM = struct

structure D  = Deps
structure A  = Ast
structure G  = Gen
structure P  = Parser
structure R  = Reg
structure E  = Env
structure T  = ListFormat
structure EH = LibBase


(* ------ SWITCHES ------ *)

(* True if we want to support subtyping. *)
val switchSub = ref false
fun setSwitchSub sub = switchSub := sub
fun getSwitchSub () = !switchSub
fun resetSwitchSub () = switchSub := false


(* ------ EXTENDED REGIONS ------ *)

datatype color = ERR_LOC
	       | END_POINT1
	       | END_POINT2
	       | FREE_LOC
	       | SYNT_LOC

type weight = int

type file = string

datatype treeReg = L of R.region * color * weight
		 | N of R.region * color * weight * treeReg list

type fileReg = file * treeReg list

type regs = fileReg list

fun printColor ERR_LOC    = "R" (* for red    *)
  | printColor END_POINT1 = "B" (* for blue   *)
  | printColor END_POINT2 = "P" (* for purple *)
  | printColor FREE_LOC   = "Y" (* for yellow *)
  | printColor SYNT_LOC   = "Y" (* for yellow *)

fun printLispExtReg (L (r, c, w))     = "(L " ^ R.toStringLispReg r ^ " " ^ printColor c ^ ")"
  | printLispExtReg (N (r, c, w, tl)) = "(N " ^ R.toStringLispReg r ^ " " ^ printColor c ^ " " ^ printLispExtRegList tl ^ ")"

and printLispExtRegList eregs = T.fmt {init = "(", final = ")", sep = " ", fmt = printLispExtReg} eregs

fun printLispExtRegs eregs =
    let fun f (file, regs) = "(\"" ^ file ^ "\" . " ^ printLispExtRegList regs ^ ")"
    in T.fmt {init = "", final = "", sep = " ", fmt = f} eregs
    end

fun toStringColor ERR_LOC    = "ERR_LOC"
  | toStringColor END_POINT1 = "END_POINT1"
  | toStringColor END_POINT2 = "END_POINT2"
  | toStringColor FREE_LOC   = "FREE_LOC"
  | toStringColor SYNT_LOC   = "SYNT_LOC"

fun toStringTreeReg (L (reg, col, weight)) =
    "L(" ^
    R.toStringReg reg     ^ "," ^
    toStringColor col     ^ "," ^
    Int.toString  weight  ^ "," ^
    ")"
  | toStringTreeReg (N (reg, col, weight, regs)) =
    "L(" ^
    R.toStringReg reg     ^ "," ^
    toStringColor col     ^ "," ^
    Int.toString  weight  ^ "," ^
    T.listToString toStringTreeReg regs ^
    ")"

fun toStringExtReg (file, regs) =
    "(" ^ file ^ "," ^ T.listToString toStringTreeReg regs ^ ")"

fun toStringExtRegs eregs = T.listToString toStringExtReg eregs


(* ------ UNIFICATION ------ *)

type etyvarError = E.id        * D.label * R.region list option
type arityError  = int         * D.label * R.region list option
type tyconError  = E.tyconname * D.label * R.region list option
type tyconErrors = tyconError list

datatype errorKind = TYCONSCLASH   of tyconError  * tyconError
		   | OVERLOAD      of tyconError  * tyconErrors
		   | OVERLOADS     of tyconErrors * tyconErrors
		   | ARITYCLASH    of arityError  * arityError
		   | ETYVARCLASH   of etyvarError * tyconError
		   | CTYVARCLASH   of etyvarError * etyvarError
		   | EQTYPE        of D.label     * D.label
		   | EQUALITYDEC   of D.label     * string
		   | UNREBOUNDABLE of string
		   | UNPARSABLE    of string
		   | SYNTAXERROR   of string
		   | FREEATOMS     of string
		   | FREEID        of string
		   | FREEETYVAR    of D.label
		   | FREEITYVAR
		   | TOPOVERLOAD
		   | CIRCULARITY
		   | NOTENOUGHINFO

type error = {id      : int,
	      kind    : errorKind,
	      deps    : D.deps, (* set of dependencies *)
	      regions : regs,
	      slice   : A.term}

exception errorex of error

(* In orpaths:
 *   - int list restricts an overloaded type to some of the types.
 *   - D.deps is the set of dependencies responsible for the restriction.
 *   - E.orseq ref list is the list of overloaded types to which the current one is equal. *)
type orpaths = int list * D.deps

(* Unifier map / substitution *)
structure MAP = SplayMapFn(type ord_key = int val compare = Int.compare)

type uni = {tv : E.ity    MAP.map ref, (* ityvar    -> ity      *)
	    tc : E.itycon MAP.map ref, (* ityconvar -> itycon   *)
	    ev : E.env    MAP.map ref, (* envvar    -> env      *)
	    sv : E.ityseq MAP.map ref, (* ityseqvar -> ityseq   *)
	    or : orpaths  MAP.map ref} (* idor      -> path     *)
(* NOTE: concerning 'or', when unifying it holds the branch of the typeseq
 * to which the type is constrained.  When renaming, we don't want a branch,
 * but we want another orid.
 * NOTE: I've changed that and we now have proper renamings.  *)

(* NOTE: What I'm planning on doing it having ty1 < a < ty2, I will put
 *   a -> ty1 in sub_gt and a -> ty2 in sub_lt.
 * Given a < b, I will put a -> b in sub_lt and b -> a in sub_gt *)
type sub =
     {ltv : E.ity    list MAP.map ref,
      gtv : E.ity    list MAP.map ref,
      ltc : E.itycon list MAP.map ref,
      gtc : E.itycon list MAP.map ref,
      lts : E.ityseq list MAP.map ref,
      gts : E.ityseq list MAP.map ref}

(* Maps labels to type variable.
 * This is used to remember the type generated at specific locations such as
 * the types generated for equal signs so that we can then access these types
 * from an AST and generate equality deciders.
 * LVMAP stands for Label to Variable MAP. *)
structure LVMAP = SplayMapFn(type ord_key = D.label val compare = D.compareLabels)
type loc =
     {eq : E.kityvar LVMAP.map,
      or : (E.idor * E.idor) LVMAP.map}

(* ??? *)
structure IDORMAP = BinaryMapFn(type ord_key = E.idor val compare = Int.compare)
type ove = (E.orseq * E.orseq * D.deps) list IDORMAP.map
type nam = E.ovenames IDORMAP.map

(* ??? *)
(*structure MONMAP = BinaryMapFn(type ord_key = E.idor val compare = Int.compare)*)
type mon = E.marker option * (D.deps E.ITVM.map * D.deps E.ETVM.map)

(* var is used to record the 'variable' declaration.  We could leave them
 * in the environment, but it should be faster to pull them out. *)
structure VARMAP = BinaryMapFn(type ord_key = E.id val compare = String.compare)
type var = (E.ity * D.label) VARMAP.map

(* acc is used to record the types of accessors.  Once we have them all
 * we can decide whether polymorphism is necessary. *)
fun acc_comp ((lab1, id1), (lab2, id2)) =
    case D.compareLabels (lab1, lab2) of
	EQUAL => String.compare (id1, id2)
      | x => x
structure ACCMAP = BinaryMapFn(type ord_key = D.label * E.id val compare = acc_comp)
type acc = E.ity list ACCMAP.map

type unifEnv =
     {uni : uni,   (* unifier - type constraints                           *)
      env : E.env, (* environment                                          *)
      var : var,   (* (global) variable declarations                       *)
      acc : acc,   (* accessors' types                                     *)
      sub : sub,   (* subtype constraints                                  *)
      ove : ove,   (* overloading constraints  - seq1 = seq2               *)
      loc : loc,   (* info associated with specific locations              *)
      nam : nam,   (* names of the different overloading choices           *)
      mon : mon}   (* monomorphic type variables from beg of env to marker *)

datatype solved = SUCCESS of unifEnv * (string * E.env) list
		| ERROR   of error

fun getUni (x : unifEnv) = #uni x
fun getEnv (x : unifEnv) = #env x
fun getVar (x : unifEnv) = #var x
fun getAcc (x : unifEnv) = #acc x
fun getSub (x : unifEnv) = #sub x
fun getOve (x : unifEnv) = #ove x
fun getLoc (x : unifEnv) = #loc x
fun getNam (x : unifEnv) = #nam x
fun getMon (x : unifEnv) = #mon x

fun getLocEq unifenv = #eq (getLoc unifenv)
fun getLocOr unifenv = #or (getLoc unifenv)

val dummy_id_error = 0

val idError = ref 1

fun nextIdError () = let val x = !idError in (idError := x + 1; x) end

fun getErrorId      (err : error) = #id      err
fun getErrorKind    (err : error) = #kind    err
fun getErrorDeps    (err : error) = #deps    err
fun getErrorRegions (err : error) = #regions err
fun getErrorSlice   (err : error) = #slice   err

fun setErrorId      {id = _, kind, deps, regions, slice} id      = {id = id, kind = kind, deps = deps, regions = regions, slice = slice} : error
fun setErrorKind    {id, kind = _, deps, regions, slice} kind    = {id = id, kind = kind, deps = deps, regions = regions, slice = slice}
fun setErrorDeps    {id, kind, deps = _, regions, slice} deps    = {id = id, kind = kind, deps = deps, regions = regions, slice = slice}
fun setErrorRegions {id, kind, deps, regions = _, slice} regions = {id = id, kind = kind, deps = deps, regions = regions, slice = slice}
fun setErrorSlice   {id, kind, deps, regions, slice = _} slice   = {id = id, kind = kind, deps = deps, regions = regions, slice = slice}

fun addErrorDeps err deps = setErrorDeps err (D.union (getErrorDeps err, deps))

fun stripErrorDeps err deps = setErrorDeps err (D.difference (getErrorDeps err, deps))

fun stripErrorDumDep err = stripErrorDeps err (D.singleton D.dummy_dep)

fun mk_pre_error kind deps =
    {id      = dummy_id_error,
     kind    = kind,
     deps    = deps,
     regions = [],
     slice   = A.empty_term}

(* NOTE: Shouldn't we just test the equality of the ids? *)
fun eqError error1 error2 =
    getErrorKind error1 = getErrorKind error2
    andalso
    D.equal (getErrorDeps error1, getErrorDeps error2)

fun isSuccess (SUCCESS _) = true
  | isSuccess (ERROR   _) = false

fun toStringTyconError (tyconname, lab, regsop) =
    "(" ^ E.toStringTyconname tyconname ^ "," ^ D.toStringLabel lab ^ ")"

fun toStringTyconErrors tyconErrors =
    T.listToString toStringTyconError tyconErrors

fun toStringEtyvarError (etyvar, lab, regsop) =
    "(" ^ etyvar ^ "," ^ D.toStringLabel lab ^ ")"

fun toStringArityError (n, lab, regsop) =
    "(" ^ Int.toString n ^ "," ^ D.toStringLabel lab ^ ")"

fun toStringTyconErrorText (tyconname, lab, NONE) = E.toStringTyconname tyconname
  | toStringTyconErrorText (tyconname, lab, SOME regs) =
    E.toStringTyconname tyconname ^ " (at regions: " ^ R.toStringRegList regs ^ ")"

fun toStringTyconErrorsText tyconErrors =
    T.fmt {init = "{", final = "}", sep = ",", fmt = toStringTyconErrorText} tyconErrors

fun toStringArityErrorText n = Int.toString n

fun toStringErrorKind (TYCONSCLASH (tyconError1, tyconError2)) =
    "TYCONSCLASH(" ^ toStringTyconError tyconError1 ^ "," ^ toStringTyconError tyconError2 ^ ")"
  | toStringErrorKind (ETYVARCLASH (etyvarError, tyconError)) =
    "ETYVARCLASH(" ^ toStringEtyvarError etyvarError ^ "," ^ toStringTyconError tyconError ^ ")"
  | toStringErrorKind (CTYVARCLASH (etyvarError1, etyvarError2)) =
    "CTYVARCLASH(" ^ toStringEtyvarError etyvarError1 ^ "," ^ toStringEtyvarError etyvarError2 ^ ")"
  | toStringErrorKind (EQTYPE (lab1, lab2)) =
    "EQTYPE(" ^ D.toStringLabel lab1 ^ "," ^ D.toStringLabel lab2 ^ ")"
  | toStringErrorKind (OVERLOAD (tyconError, tyconErrors)) =
    "OVERLOAD(" ^ toStringTyconError tyconError ^ "," ^ toStringTyconErrors tyconErrors ^ ")"
  | toStringErrorKind (OVERLOADS (tyconErrors1, tyconErrors2)) =
    "OVERLOADS(" ^ toStringTyconErrors tyconErrors1 ^ "," ^ toStringTyconErrors tyconErrors2 ^ ")"
  | toStringErrorKind (ARITYCLASH (arityError1, arityError2)) =
    "ARITYCLASH(" ^ toStringArityError arityError1 ^ "," ^ toStringArityError arityError2 ^ ")"
  | toStringErrorKind (EQUALITYDEC (lab, st)) =
    "EQUALITYDEC(" ^ D.toStringLabel lab ^ "," ^ st ^ ")"
  | toStringErrorKind (UNPARSABLE    st)  = "UNPARSABLE("    ^ st ^ ")"
  | toStringErrorKind (UNREBOUNDABLE st)  = "UNREBOUNDABLE(" ^ st ^ ")"
  | toStringErrorKind (SYNTAXERROR   st)  = "SYNTAXERROR("   ^ st ^ ")"
  | toStringErrorKind (FREEID        st)  = "FREEID("        ^ st ^ ")"
  | toStringErrorKind (FREEATOMS     st)  = "FREEATOMS("     ^ st ^ ")"
  | toStringErrorKind (FREEETYVAR    lab) = "FREEETYVAR("    ^ D.toStringLabel lab ^ ")"
  | toStringErrorKind FREEITYVAR          = "FREEITYVAR"
  | toStringErrorKind TOPOVERLOAD         = "TOPOVERLOAD"
  | toStringErrorKind CIRCULARITY         = "CIRCULARITY"
  | toStringErrorKind NOTENOUGHINFO       = "NOTENOUGHINFO"

fun toStringErrorKindText (TYCONSCLASH ((tyconname1, lab1, regsop1), (tyconname2, lab2, regsop2))) =
    "type constructor clash between " ^ tyconname1 ^ " and " ^ tyconname2
  | toStringErrorKindText (ETYVARCLASH ((etyvar, lab1, regsop1), (tyconname, lab2, regsop2))) =
    "the non-yet-generalised type variable " ^ etyvar ^ " clashes with the type constructor " ^ tyconname
  | toStringErrorKindText (CTYVARCLASH ((etyvar1, lab1, regsop1), (etyvar2, lab2, regsop2))) =
    "clash between the non-yet-generalised type variables " ^ etyvar1 ^ " and " ^ etyvar2
  | toStringErrorKindText (EQTYPE (lab1, lab2)) =
    "equality type is not respected"
  | toStringErrorKindText (OVERLOAD (tyconError, tyconErrors)) =
    "the type constructor " ^ toStringTyconErrorText tyconError ^ " is not any of the following types: " ^ toStringTyconErrorsText tyconErrors
  | toStringErrorKindText (OVERLOADS (tyconErrors1, tyconErrors2)) =
    "the two type constructor lists " ^ toStringTyconErrorsText tyconErrors1 ^ " and " ^ toStringTyconErrorsText tyconErrors2 ^ " are disjoints"
  | toStringErrorKindText (ARITYCLASH ((n1, lab1, regsop1), (n2, lab2, regsop2))) =
    "arity clash between arity " ^ Int.toString n1 ^ " and " ^ Int.toString n2
  | toStringErrorKindText (EQUALITYDEC (lab, st)) = "cannot generate equality decider because " ^ st
  | toStringErrorKindText (UNPARSABLE    st)  = "unparsable: " ^ st
  | toStringErrorKindText (UNREBOUNDABLE st)  = st ^ " cannot be rebound"
  | toStringErrorKindText (SYNTAXERROR   st)  = "syntax error: " ^ st
  | toStringErrorKindText (FREEID        st)  = st ^ " is a free identifier"
  | toStringErrorKindText (FREEATOMS     st)  = "\"" ^ st ^ "\" is not the header of any message"
  | toStringErrorKindText (FREEETYVAR    lab) = "free explicit type variable"
  | toStringErrorKindText FREEITYVAR          = "free implicit type variable"
  | toStringErrorKindText TOPOVERLOAD         = "Overloaded user declaration"
  | toStringErrorKindText CIRCULARITY         = "circularity"
  | toStringErrorKindText NOTENOUGHINFO       = "sorry, we don't have enough type information to resolve the overloading"

fun toStringError (kind, deps) =
    "(" ^ toStringErrorKind kind ^ "," ^ D.toStringDeps deps ^ ")"

fun initUni () =
    {tv = ref MAP.empty,
     tc = ref MAP.empty,
     ev = ref MAP.empty,
     sv = ref MAP.empty,
     or = ref MAP.empty}

fun initSub () =
    {ltv = ref MAP.empty,
     gtv = ref MAP.empty,
     ltc = ref MAP.empty,
     gtc = ref MAP.empty,
     lts = ref MAP.empty,
     gts = ref MAP.empty}

fun initEnv () = E.ENVNUL

fun initVar () = VARMAP.empty

fun initAcc () = ACCMAP.empty

fun initLoc () =
    {eq = LVMAP.empty,
     or = LVMAP.empty}

fun initOve () = IDORMAP.empty

fun initNam () = IDORMAP.empty

fun initIMon  () = (E.emptyITVM : D.deps E.ITVM.map)
fun initEMon  () = (E.emptyETVM : D.deps E.ETVM.map)
fun initIEMon () = (initIMon (), initEMon ())

fun initMon () =
    let val initImonos = E.insertITVM (initIMon (), E.ityvarFresh, D.empty)
    in (NONE, (initImonos, initEMon ()))
    end

fun mkUnifEnv uni env var acc sub ove loc nam mon =
    {uni = uni,
     env = env,
     var = var,
     acc = acc,
     sub = sub,
     ove = ove,
     loc = loc,
     nam = nam,
     mon = mon}

(* Initialise a unifenv *)
fun initUnifEnv () =
    mkUnifEnv (initUni ())
	      (initEnv ())
	      (initVar ())
	      (initAcc ())
	      (initSub ())
	      (initOve ())
	      (initLoc ())
	      (initNam ())
	      (initMon ())

(* Initialise a success state *)
fun initSolved () = SUCCESS (initUnifEnv (), [])

(* Checks whether a constraint solving result is a success or not *)
fun isSuccess (SUCCESS _) = true
  | isSuccess _ = false

fun unionMon (imon1, emon1) (imon2, emon2) =
    (E.unionWithITVM (fn (_, x) => x) (imon1, imon2),
     E.unionWithETVM (fn (_, x) => x) (emon1, emon2))

fun updateLocEq {eq, or} label kityvar = {eq = LVMAP.insert (eq, label, kityvar), or = or}
fun updateLocOr {eq, or} label idors   = {eq = eq, or = LVMAP.insert (or, label, idors)}

(* Replaces the different parts of a unifenv *)
fun replaceUniEnvUni {uni = _, env, var, acc, sub, ove, loc, nam, mon} uni = mkUnifEnv uni env var acc sub ove loc nam mon
fun replaceUniEnvEnv {uni, env = _, var, acc, sub, ove, loc, nam, mon} env = mkUnifEnv uni env var acc sub ove loc nam mon
fun replaceUniEnvVar {uni, env, var = _, acc, sub, ove, loc, nam, mon} var = mkUnifEnv uni env var acc sub ove loc nam mon
fun replaceUniEnvAcc {uni, env, var, acc = _, sub, ove, loc, nam, mon} acc = mkUnifEnv uni env var acc sub ove loc nam mon
fun replaceUniEnvSub {uni, env, var, acc, sub = _, ove, loc, nam, mon} sub = mkUnifEnv uni env var acc sub ove loc nam mon
fun replaceUniEnvOve {uni, env, var, acc, sub, ove = _, loc, nam, mon} ove = mkUnifEnv uni env var acc sub ove loc nam mon
fun replaceUniEnvLoc {uni, env, var, acc, sub, ove, loc = _, nam, mon} loc = mkUnifEnv uni env var acc sub ove loc nam mon
fun replaceUniEnvNam {uni, env, var, acc, sub, ove, loc, nam = _, mon} nam = mkUnifEnv uni env var acc sub ove loc nam mon
fun replaceUniEnvMon {uni, env, var, acc, sub, ove, loc, nam, mon = _} mon = mkUnifEnv uni env var acc sub ove loc nam mon

(* Updates the different parts of a unifenv *)
fun updateUniEnvEnv unifenv env =
    replaceUniEnvEnv unifenv (E.mk_env_app (getEnv unifenv, env))

fun updateUniEnvVar unifenv id ity lab =
    let val vars  = getVar unifenv
	val vars' = VARMAP.insert (vars, id, (ity, lab))
    in replaceUniEnvVar unifenv vars'
    end

fun updateUniEnvAcc unifenv lab id ity =
    let val accs  = getAcc unifenv
	val lst   = case ACCMAP.find (accs, (lab, id)) of
			SOME lst => lst
		      | NONE => []
	val accs' = ACCMAP.insert (accs, (lab, id), ity :: lst)
    in replaceUniEnvAcc unifenv accs'
    end

fun updateUniEnvLocEq unifenv label ityvar =
    let val loc = updateLocEq (getLoc unifenv) label ityvar
    in replaceUniEnvLoc unifenv loc
    end

fun updateUniEnvLocOr unifenv label idors =
    let val loc = updateLocOr (getLoc unifenv) label idors
    in replaceUniEnvLoc unifenv loc
    end

fun updateUniEnvOve unifenv (orseq1, orseq2, deps) =
    let val ove1 = getOve unifenv
	val map1 = IDORMAP.singleton (E.getIdOrseq orseq1, [(orseq1, orseq2, deps)])
	val map2 = IDORMAP.singleton (E.getIdOrseq orseq2, [(orseq2, orseq1, deps)])
	val map  = IDORMAP.unionWith (fn (list1, list2) => list1 @ list2) (map1, map2)
	val ove2 = IDORMAP.unionWith (fn (list1, list2) => list1 @ list2) (ove1, map)
    in replaceUniEnvOve unifenv ove2
    end

fun updateUniEnvNam unifenv idor names =
    let val nam1 = getNam unifenv
	val nam2 = IDORMAP.singleton (idor, names)
	val nam  = IDORMAP.unionWith (fn _ => raise Fail "updateUniEnvNam") (nam1, nam2)
    in replaceUniEnvNam unifenv nam
    end

(* Removes the list associated to id in the ove part of unifenv and returns
 * the list along with the reduced unifenv. *)
fun rmUniEnvOve unifenv id =
    let val ove1 = getOve unifenv
	val (ove2, list) = IDORMAP.remove (ove1, id)
    in (replaceUniEnvOve unifenv ove2, list)
    end handle LibBase.NotFound => (unifenv, [])

(* True if there are entries for id in the ove part of unifenv. *)
fun existsUniEnvOve unifenv id =
    Option.isSome (IDORMAP.find (getOve unifenv, id))

(* True if the ove part of a unifenv is empty *)
fun isEmptyUniEnvOve unifenv = IDORMAP.numItems (getOve unifenv) = 0

(* Lists the elements in the ove part of a unifenv *)
fun listUniEnvOve unifenv = IDORMAP.listItems (getOve unifenv)

(* Resets the different parts of a unifenv *)
fun resetUniEnvOve unifenv = replaceUniEnvOve unifenv (initOve ())

fun resetUniEnvVar unifenv = replaceUniEnvVar unifenv (initVar ())

fun resetUniEnvOveMrk unifenv E.TOP = resetUniEnvOve unifenv
  | resetUniEnvOveMrk unifenv E.NES = unifenv
  | resetUniEnvOveMrk unifenv E.CON = unifenv

(*fun isin _ [] = false
  | isin (x : int) (y :: ys) = x = y orelse isin x ys*)

(*
(* Check if the environment in an unifenv contains an environment variable *)
fun hasEnvVar (E.ENVVAR _) = true
  | hasEnvVar (E.ENVBIN _) = false
  | hasEnvVar (E.ENVAPP (env1, env2)) = hasEnvVar env2 orelse hasEnvVar env1
  | hasEnvVar (E.ENVDEP (env, _)) = hasEnvVar env
  | hasEnvVar (E.ENVMRK _) = false
  | hasEnvVar E.ENVNUL = false
  | hasEnvVar _ = raise EH.Impossible "Unexpected type environment in unification environment"
*)

(* Access the unifier *)
fun getUniTv tv (uni : uni) = MAP.find (!(#tv uni), tv)
fun getUniTc tc (uni : uni) = MAP.find (!(#tc uni), tc)
fun getUniEv ev (uni : uni) = MAP.find (!(#ev uni), ev)
fun getUniSv sv (uni : uni) = MAP.find (!(#sv uni), sv)
fun getUniOr or (uni : uni) = MAP.find (!(#or uni), or)

(* Access the unifier in a unifenv *)
fun getUniEnvTv tv unifenv = getUniTv tv (getUni unifenv)
fun getUniEnvTc tc unifenv = getUniTc tc (getUni unifenv)
fun getUniEnvEv ev unifenv = getUniEv ev (getUni unifenv)
fun getUniEnvSv sv unifenv = getUniSv sv (getUni unifenv)
fun getUniEnvOr or unifenv = getUniOr or (getUni unifenv)

fun applyDepsError {id, kind, deps, regions, slice} deps' =
    {id      = id,
     kind    = kind,
     deps    = D.union (deps, deps'),
     regions = regions,
     slice   = slice}

datatype 'a search_id = FOUND of 'a
		      | NOT_FOUND
		      | SHADOWED

datatype tvbind_kind = EXPL | IMPL

(* Access the environment in a unifenv *)
fun getUniEnvBindVid vid (E.BINDVID (vid', lab, scheme)) kind =
    if vid = vid'
    then FOUND (scheme, lab, kind)
    else NOT_FOUND
  | getUniEnvBindVid _ _ _ = NOT_FOUND

fun getUniEnvBindTyv tyvar (E.BINDTYV (tyvar', lab, ity)) kind =
    if tyvar = tyvar'
    then FOUND (ity, lab, kind)
    else NOT_FOUND
  | getUniEnvBindTyv _ _ _ = NOT_FOUND

fun getUniEnvBindTyc tycon (E.BINDTYC (tycon', lab, ityfun)) kind =
    if tycon = tycon'
    then FOUND (ityfun, lab, kind)
    else NOT_FOUND
  | getUniEnvBindTyc _ _ _ = NOT_FOUND

(*fun getUniEnvBindTok token (E.BINDTOK (token', ity)) =
    if token = token'
    then FOUND ity
    else NOT_FOUND
  | getUniEnvBindTok _ _ = NOT_FOUND*)

fun getUniEnvBindAtm atoms (E.BINDATM (atoms', lab, scheme)) kind =
    if atoms = atoms'
    then FOUND (scheme, lab, kind)
    else NOT_FOUND
  | getUniEnvBindAtm _ _ _ = NOT_FOUND

fun getUniEnvEnv _ (E.ENVVAR _) _ = SHADOWED
  | getUniEnvEnv id (E.ENVBIN (bind, kind)) (_, fapplyBind) = fapplyBind id bind kind
  | getUniEnvEnv id (E.ENVAPP (env1, env2)) f =
    (case getUniEnvEnv id env2 f of
	 SHADOWED  => SHADOWED
       | NOT_FOUND => getUniEnvEnv id env1 f
       | FOUND x   => FOUND x)
  | getUniEnvEnv id (E.ENVDEP (env, dep)) (f as (fapplyDep, _)) =
    (case getUniEnvEnv id env f of
	 SHADOWED  => SHADOWED
       | NOT_FOUND => NOT_FOUND
       | FOUND (x, lab, kind) => FOUND (fapplyDep x (D.singleton dep), lab, kind))
  | getUniEnvEnv _ (E.ENVMRK _) _ = NOT_FOUND
  | getUniEnvEnv _ E.ENVNUL _ = NOT_FOUND
  | getUniEnvEnv _ _ _ = raise Fail "getUniEnvEnv:Unexpected type environment in unification environment"

fun getUniEnvId id unifenv f = getUniEnvEnv id (getEnv unifenv) f

fun getUniEnvVid vid unifenv = getUniEnvId vid unifenv (E.applyDepsScheme, getUniEnvBindVid)
fun getUniEnvTyv tyv unifenv = getUniEnvId tyv unifenv (E.applyDepsTvbind, getUniEnvBindTyv)
fun getUniEnvTyc tyc unifenv = getUniEnvId tyc unifenv (E.applyDepsItyfun, getUniEnvBindTyc)
fun getUniEnvAtm atm unifenv = getUniEnvId atm unifenv (E.applyDepsScheme, getUniEnvBindAtm)
(*fun getUniEnvTok tok unifenv = getUniEnvId tok unifenv (E.applyDepsIty,    getUniEnvBindTok)*)

fun getUniEnvVar id unifenv = VARMAP.find (getVar unifenv, id)

fun getUniEnvAcc lab id unifenv = ACCMAP.find (getAcc unifenv, (lab, id))

fun isAccessed lab id (SUCCESS (unifenv, _)) =
    (case getUniEnvAcc lab id unifenv of
	 NONE    => false
       |	SOME [] => false
       | _       => true)
  | isAccessed _ _ _ = false

fun updUniGen m key value = m := MAP.insert (!m, key, value)

fun updUniGen' key value uni ouni vfresh =
    if key = vfresh
    then uni
    else (updUniGen ouni key value; uni)

fun updUniTv (uni : uni) key value = updUniGen' key value uni (#tv uni) E.ityvarFresh
fun updUniTc (uni : uni) key value = updUniGen' key value uni (#tc uni) E.tyconvarFresh
fun updUniEv (uni : uni) key value = updUniGen' key value uni (#ev uni) E.envvarFresh
fun updUniSv (uni : uni) key value = updUniGen' key value uni (#sv uni) E.ityseqvarFresh
fun updUniOr (uni : uni) key value = updUniGen' key value uni (#or uni) E.idorFresh

fun updateUniEnvUniTv unifenv key value = replaceUniEnvUni unifenv (updUniTv (getUni unifenv) key value)
fun updateUniEnvUniTc unifenv key value = replaceUniEnvUni unifenv (updUniTc (getUni unifenv) key value)
fun updateUniEnvUniEv unifenv key value = replaceUniEnvUni unifenv (updUniEv (getUni unifenv) key value)
fun updateUniEnvUniSv unifenv key value = replaceUniEnvUni unifenv (updUniSv (getUni unifenv) key value)
fun updateUniEnvUniOr unifenv key value = replaceUniEnvUni unifenv (updUniOr (getUni unifenv) key value)

(* Access the subtyping constraints *)
fun getSubLtv tv (sub : sub) = MAP.find (!(#ltv sub), tv)
fun getSubGtv tv (sub : sub) = MAP.find (!(#gtv sub), tv)
fun getSubLtc tc (sub : sub) = MAP.find (!(#ltc sub), tc)
fun getSubGtc tc (sub : sub) = MAP.find (!(#gtc sub), tc)
fun getSubLts sq (sub : sub) = MAP.find (!(#lts sub), sq)
fun getSubGts sq (sub : sub) = MAP.find (!(#gts sub), sq)

(* Access the subtyping constraint in a unifenv *)
fun getUniEnvLtv tv unifenv = getSubLtv tv (getSub unifenv)
fun getUniEnvGtv tv unifenv = getSubGtv tv (getSub unifenv)
fun getUniEnvLtc tc unifenv = getSubLtc tc (getSub unifenv)
fun getUniEnvGtc tc unifenv = getSubGtc tc (getSub unifenv)
fun getUniEnvLts sq unifenv = getSubLts sq (getSub unifenv)
fun getUniEnvGts sq unifenv = getSubGts sq (getSub unifenv)

fun isItyvar ityvar (E.ITYVAR (tv, eq)) = (ityvar = tv)
  | isItyvar ityvar (E.ITYDEP (ity, _)) = isItyvar ityvar ity
  | isItyvar _ _ = false

fun isItyseqvar ityseqvar (E.ITYSEQVAR sv)          = (ityseqvar = sv)
  | isItyseqvar ityseqvar (E.ITYSEQDEP (ityseq, _)) = isItyseqvar ityseqvar ityseq
  | isItyseqvar _ _ = false

fun isItyconvar ityconvar (E.ITYCONVAR tcv)         = (ityconvar = tcv)
  | isItyconvar ityconvar (E.ITYCONDEP (itycon, _)) = isItyconvar ityconvar itycon
  | isItyconvar _ _ = false

fun isUniEnvSubGen sub key1 key2 fvar =
    case MAP.find (!sub, key1) of
	SOME lst => List.exists (fvar key2) lst
      | NONE => false

fun isUniEnvSubLtv subL subG ityvar1    (E.ITYVAR (ityvar2, _))  = isUniEnvSubGen subL ityvar2    ityvar1    isItyvar
								   orelse
								   isUniEnvSubGen subG ityvar1    ityvar2    isItyvar
  | isUniEnvSubLtv subL subG ityvar1    (E.ITYDEP (ity, _))      = isUniEnvSubLtv subL subG ityvar1 ity
  | isUniEnvSubLtv _    _    _          _                        = false

fun isUniEnvSubGtv subG subL ityvar1    (E.ITYVAR (ityvar2, _))  = isUniEnvSubGen subL ityvar1    ityvar2    isItyvar
								   orelse
								   isUniEnvSubGen subG ityvar2    ityvar1    isItyvar
  | isUniEnvSubGtv subG subL ityvar1    (E.ITYDEP (ity, _))      = isUniEnvSubGtv subG subL ityvar1 ity
  | isUniEnvSubGtv _    _    _          _                        = false

fun isUniEnvSubLtc subL subG ityvarcon1 (E.ITYCONVAR ityvarcon2) = isUniEnvSubGen subL ityvarcon2 ityvarcon1 isItyconvar
								   orelse
								   isUniEnvSubGen subG ityvarcon1 ityvarcon2 isItyconvar
  | isUniEnvSubLtc subL subG ityvarcon1 (E.ITYCONDEP (tc, _))    = isUniEnvSubLtc subL subG ityvarcon1 tc
  | isUniEnvSubLtc _    _    _          _                        = false

fun isUniEnvSubGtc subG subL ityvarcon1 (E.ITYCONVAR ityvarcon2) = isUniEnvSubGen subL ityvarcon1 ityvarcon2 isItyconvar
								   orelse
								   isUniEnvSubGen subG ityvarcon2 ityvarcon1 isItyconvar
  | isUniEnvSubGtc subG subL ityvarcon1 (E.ITYCONDEP (tc, _))    = isUniEnvSubGtc subG subL ityvarcon1 tc
  | isUniEnvSubGtc _   _   _          _                          = false

fun isUniEnvSubLts subL subG ityvarseq1 (E.ITYSEQVAR ityvarseq2) = isUniEnvSubGen subL ityvarseq2 ityvarseq1 isItyseqvar
								   orelse
								   isUniEnvSubGen subG ityvarseq1 ityvarseq2 isItyseqvar
  | isUniEnvSubLts subL subG ityvarseq1 (E.ITYSEQDEP (seq, _))   = isUniEnvSubLts subL subG ityvarseq1 seq
  | isUniEnvSubLts _   _   _          _                          = false

fun isUniEnvSubGts subG subL ityvarseq1 (E.ITYSEQVAR ityvarseq2) = isUniEnvSubGen subL ityvarseq1 ityvarseq2 isItyseqvar
								   orelse
								   isUniEnvSubGen subG ityvarseq2 ityvarseq1 isItyseqvar
  | isUniEnvSubGts subG subL ityvarseq1 (E.ITYSEQDEP (seq, _))   = isUniEnvSubGts subG subL ityvarseq1 seq
  | isUniEnvSubGts _   _   _          _                          = false

fun updSubGen key value sub sub1 sub2 vfresh fsub isSub cons =
    if key = vfresh
    then (sub, E.ENVNUL)
    else if isSub sub1 sub2 key value
    then (sub, E.ENVCST (cons (key, value)))
    else let val _ =
		 case MAP.find (!sub1, key) of
		     NONE => sub1 := MAP.insert (!sub1, key, [value])
		   | SOME values => sub1 := MAP.insert (!sub1, key, value :: values)
	     val envs =
		 case MAP.find (!sub2, key) of
		     NONE => []
		   | SOME lst => map (fn elt => E.ENVSUB (fsub (elt, value))) lst
	 in (sub, E.list2env envs)
	 end

fun updSubLtv (sub : sub) key value = updSubGen key value sub (#ltv sub) (#gtv sub) E.ityvarFresh E.SUBITY                       isUniEnvSubLtv (fn (tv,  ity)    => E.CSITY (E.mk_tyvar  tv,  ity))
fun updSubGtv (sub : sub) key value = updSubGen key value sub (#gtv sub) (#ltv sub) E.ityvarFresh (fn (a, b) => E.SUBITY (b, a)) isUniEnvSubGtv (fn (tv,  ity)    => E.CSITY (E.mk_tyvar  tv,  ity))
fun updSubLtc (sub : sub) key value = updSubGen key value sub (#ltc sub) (#gtc sub) E.ityvarFresh E.SUBNAM                       isUniEnvSubLtc (fn (tcv, itycon) => E.CSNAM (E.ITYCONVAR tcv, itycon))
fun updSubGtc (sub : sub) key value = updSubGen key value sub (#gtc sub) (#ltc sub) E.ityvarFresh (fn (a, b) => E.SUBNAM (b, a)) isUniEnvSubGtc (fn (tcv, itycon) => E.CSNAM (E.ITYCONVAR tcv, itycon))
fun updSubLts (sub : sub) key value = updSubGen key value sub (#lts sub) (#gts sub) E.ityvarFresh E.SUBSEQ                       isUniEnvSubLts (fn (sv,  ityseq) => E.CSSEQ (E.ITYSEQVAR sv,  ityseq))
fun updSubGts (sub : sub) key value = updSubGen key value sub (#gts sub) (#lts sub) E.ityvarFresh (fn (a, b) => E.SUBSEQ (b, a)) isUniEnvSubGts (fn (sv,  ityseq) => E.CSSEQ (E.ITYSEQVAR sv,  ityseq))

fun updateUniEnvSubGen unifenv key value fupd =
    let val (sub, env) = fupd (getSub unifenv) key value
	val unifenv' = replaceUniEnvSub unifenv sub
    in (unifenv', env)
    end

fun updateUniEnvSubLtv unifenv key value = updateUniEnvSubGen unifenv key value updSubLtv
fun updateUniEnvSubGtv unifenv key value = updateUniEnvSubGen unifenv key value updSubGtv
fun updateUniEnvSubLtc unifenv key value = updateUniEnvSubGen unifenv key value updSubLtc
fun updateUniEnvSubGtc unifenv key value = updateUniEnvSubGen unifenv key value updSubGtc
fun updateUniEnvSubLts unifenv key value = updateUniEnvSubGen unifenv key value updSubLts
fun updateUniEnvSubGts unifenv key value = updateUniEnvSubGen unifenv key value updSubGts

fun updateUniEnvSubPairTv unifenv (lt, gt) =
    let val sub  = getSub unifenv
	val subL = #ltv sub
	val subG = #gtv sub
	val tv   = E.nextItyvar ()
	val _    = subL := MAP.insert (!subL, tv, [gt])
	val _    = subG := MAP.insert (!subG, tv, [lt])
    in unifenv
    end

fun rmUniEnvSubGen unifenv onesub var =
    let val _ = (onesub := #1 (MAP.remove (!onesub, var)))
	    handle LibBase.NotFound => ()
    in unifenv
    end

fun rmUniEnvSubLtv unifenv ityvar    = rmUniEnvSubGen unifenv (#ltv (getSub unifenv)) ityvar
fun rmUniEnvSubGtv unifenv ityvar    = rmUniEnvSubGen unifenv (#gtv (getSub unifenv)) ityvar
fun rmUniEnvSubLtc unifenv ityconvar = rmUniEnvSubGen unifenv (#ltc (getSub unifenv)) ityconvar
fun rmUniEnvSubGtc unifenv ityconvar = rmUniEnvSubGen unifenv (#gtc (getSub unifenv)) ityconvar
fun rmUniEnvSubLts unifenv ityseqvar = rmUniEnvSubGen unifenv (#lts (getSub unifenv)) ityseqvar
fun rmUniEnvSubGts unifenv ityseqvar = rmUniEnvSubGen unifenv (#gts (getSub unifenv)) ityseqvar

(*
(* Moves the subtyping constraint from one tyvar to another one:
 * from ityvar1 to tyvar2. *)
fun mvUniEnvSubTv unifenv ityvar1 ityvar2 =
    let val subLtv = #ltv (getSub unifenv)
	val subGtv = #gtv (getSub unifenv)
	val unifenv1 =
	    case MAP.find (!subLtv, ityvar1) of
		NONE => unifenv
	      | SOME itys =>
		foldr (fn (ity, unifenv) => updateUniEnvSubLtv unifenv ityvar2 ity)
		      unifenv
		      itys
	val unifenv2 =
	    case MAP.find (!subGtv, ityvar1) of
		NONE => unifenv1
	      | SOME itys =>
		foldr (fn (ity, unifenv) => updateUniEnvSubGtv unifenv ityvar2 ity)
		      unifenv1
		      itys
    in rmUniEnvSubGtv (rmUniEnvSubLtv unifenv2 ityvar1) ityvar1
    end
*)

(* generates subtyping constraints using elt from whatever subtyping constraint
 * unifenv had on var (taking into account that var = elt), and then removes
 * all the recorded subtyping constraints on var (we have the new one on elt). *)
fun toSubUniEnvSubGen unifenv (getSubL, getSubG) toSub (rem1, rem2) var elt =
    let fun getEnvs sub toSub =
	    case MAP.find (!sub, var) of
		NONE => []
	      | SOME lst => map (fn elt => E.ENVSUB (toSub elt)) lst
	fun toSubL elt' = toSub (elt, elt')
	fun toSubG elt' = toSub (elt', elt)
	val envs1 = getEnvs (getSubL (getSub unifenv)) toSubL
	val envs2 = getEnvs (getSubG (getSub unifenv)) toSubG
	val env   = E.list2env (envs1 @ envs2)
    in (rem2 (rem1 unifenv var) var, env)
    end

fun getAllSubs unifenv =
    let val subLtv = !(#ltv (getSub unifenv))
	val subGtv = !(#gtv (getSub unifenv))
	val subLtc = !(#ltc (getSub unifenv))
	val subGtc = !(#gtc (getSub unifenv))
	val subLts = !(#lts (getSub unifenv))
	val subGts = !(#gts (getSub unifenv))
	val subs1  = map (fn (tv,  itys)    => map (fn ity    => E.SUBITY (E.mk_tyvar tv, ity))      itys)    (MAP.listItemsi subLtv)
	val subs2  = map (fn (tv,  itys)    => map (fn ity    => E.SUBITY (ity, E.mk_tyvar tv))      itys)    (MAP.listItemsi subGtv)
	val subs3  = map (fn (tcv, itycons) => map (fn itycon => E.SUBNAM (E.ITYCONVAR tcv, itycon)) itycons) (MAP.listItemsi subLtc)
	val subs4  = map (fn (tcv, itycons) => map (fn itycon => E.SUBNAM (itycon, E.ITYCONVAR tcv)) itycons) (MAP.listItemsi subGtc)
	val subs5  = map (fn (sv,  ityseqs) => map (fn ityseq => E.SUBSEQ (E.ITYSEQVAR sv, ityseq))  ityseqs) (MAP.listItemsi subLts)
	val subs6  = map (fn (sv,  ityseqs) => map (fn ityseq => E.SUBSEQ (ityseq, E.ITYSEQVAR sv))  ityseqs) (MAP.listItemsi subGts)
    in List.concat (subs1 @ subs2 @ subs3 @ subs4 @ subs5 @ subs6)
    end

fun noSubs unifenv =
    let val subLtv = !(#ltv (getSub unifenv))
	val subGtv = !(#gtv (getSub unifenv))
	val subLtc = !(#ltc (getSub unifenv))
	val subGtc = !(#gtc (getSub unifenv))
	val subLts = !(#lts (getSub unifenv))
	val subGts = !(#gts (getSub unifenv))
    in MAP.numItems subLtv = 0
       andalso
       MAP.numItems subGtv = 0
       andalso
       MAP.numItems subLtc = 0
       andalso
       MAP.numItems subGtc = 0
       andalso
       MAP.numItems subLts = 0
       andalso
       MAP.numItems subGts = 0
    end

fun dumpSubs subs =
    let val _ = print ("**************************\n")
	val _ =
	    app (fn sub => print (E.ppSub' sub ^ "\n"))
		subs
	val _ = print ("**************************\n")
    in ()
    end

fun dumpUniEnvSubs unifenv =
    let val _ = (* ltv *)
	    MAP.appi (fn (tv, itys) =>
			 print (Int.toString tv ^ " < " ^ T.listToString E.ppIty' itys ^ "\n"))
		(!(#ltv (getSub unifenv)))
	val _ = (* gtv *)
	    MAP.appi (fn (tv, itys) =>
			 print (Int.toString tv ^ " > " ^ T.listToString E.ppIty' itys ^ "\n"))
		(!(#gtv (getSub unifenv)))
	val _ = (* ltc *)
	    MAP.appi (fn (tc, itycons) =>
			 print (Int.toString tc ^ " < " ^ T.listToString E.ppItycon itycons ^ "\n"))
		(!(#ltc (getSub unifenv)))
	val _ = (* gtc *)
	    MAP.appi (fn (tc, itycons) =>
			 print (Int.toString tc ^ " > " ^ T.listToString E.ppItycon itycons ^ "\n"))
		(!(#gtc (getSub unifenv)))
	val _ = (* lts *)
	    MAP.appi (fn (sv, ityseqs) =>
			 print (Int.toString sv ^ " < " ^ T.listToString E.ppItyseq' ityseqs ^ "\n"))
		(!(#lts (getSub unifenv)))
	val _ = (* gts *)
	    MAP.appi (fn (sv, ityseqs) =>
			 print (Int.toString sv ^ " > " ^ T.listToString E.ppItyseq' ityseqs ^ "\n"))
		(!(#lts (getSub unifenv)))
    in ()
    end

fun dumpUniEnvEqTv unifenv =
    let val _ = print ("**************************\n")
	val _ =
	    MAP.appi (fn (tv, ity) =>
			 print (Int.toString tv ^ " = " ^ E.ppIty' ity ^ "\n"))
		     (!(#tv (getUni unifenv)))
	val _ = print ("**************************\n")
    in ()
    end


(* ------ RENAMING ------ *)

(* for Map on Explicit Variables *)
structure MEV = SplayMapFn(type ord_key = E.id val compare = String.compare)

type etvren = E.ityvar * E.eqTc

type ren = {tv : E.ityvar MAP.map ref,
	    or : E.idor   MAP.map ref,
	    te : etvren   MEV.map ref}

(* initial renamer *)
fun initRen () =
    {tv = ref MAP.empty,
     or = ref MAP.empty,
     te = ref MEV.empty}

(* Access to renamer *)
fun getRenTv tv (ren : ren) = MAP.find (!(#tv ren), tv)
fun getRenOr or (ren : ren) = MAP.find (!(#or ren), or)
fun getRenTe te (ren : ren) = MEV.find (!(#te ren), te)

(* Update a renamer *)
fun updRenTv (ren : ren) key value =
    let val ouni = #tv ren
	val _    = ouni := MAP.insert (!ouni, key, value)
    in ren
    end
fun updRenOr (ren : ren) key value =
    let val ouni = #or ren
	val _    = ouni := MAP.insert (!ouni, key, value)
    in ren
    end
fun updRenTe (ren : ren) key value =
    let val ouni = #te ren
	val _    = ouni := MEV.insert (!ouni, key, value)
    in ren
    end

(* Checks whether a renaming is empty *)
fun isEmptyRen {tv, or, te} =
    MAP.numItems tv = 0
    andalso
    MAP.numItems or = 0
    andalso
    MEV.numItems te = 0


(* ------ TYPE FRESHNING ------ *)

(* Refresh a ity *)
fun refreshIty (E.ITYVAR (tv, eq)) ren =
    (case getRenTv tv ren of
	 NONE        => E.ITYVAR (tv,     eq)
       | SOME ityvar => E.ITYVAR (ityvar, eq))
  | refreshIty (E.ITYETV (etyvar, lab)) ren =
    (case getRenTe etyvar ren of
	 NONE                 => E.ITYETV (etyvar, lab)
       | SOME (ityvar, true)  => E.ITYVAR (ityvar, E.EQ lab)
       | SOME (ityvar, false) => E.ITYVAR (ityvar, E.NEQ))
  | refreshIty (E.ITYCON (ityseq, itycon))  ren = E.ITYCON (refreshItyseq ityseq ren, itycon)
  (*| refreshIty (E.ITYDPP (ity1, ity2))      ren = E.ITYDPP (refreshIty ity1 ren, refreshIty ity2 ren)*)
  | refreshIty (E.ITYOR orseq) ren = E.ITYOR (refreshOrseq orseq ren)
  | refreshIty (E.ITYTYP (ity, lab)) ren = E.ITYTYP (refreshIty ity ren, lab)
  | refreshIty (E.ITYDEP (ity, dep)) ren = E.ITYDEP (refreshIty ity ren, dep)

and refreshOrseq (E.ORSEQ (ityseq, id, lab)) ren =
    case getRenOr id ren of
	NONE      => E.ORSEQ (refreshItyseq ityseq ren, id,   lab)
      | SOME idor => E.ORSEQ (refreshItyseq ityseq ren, idor, lab)

(*and refreshItoken (ITOKVAR kv)            ren = ITOKVAR kv
  | refreshItoken (ITOKITY ity)           ren = ITOKITY (refreshIty ity ren)
  | refreshItoken (ITOKDEP (itoken, dep)) ren = ITOKDEP (refreshItoken itoken ren, dep)*)

and refreshItyseq (E.ITYSEQVAR ityseqvar)     ren = E.ITYSEQVAR ityseqvar
  | refreshItyseq (E.ITYSEQSEQ (itys, lab))   ren = E.ITYSEQSEQ (map (fn x => refreshIty x ren) itys, lab)
  | refreshItyseq (E.ITYSEQDEP (ityseq, dep)) ren = E.ITYSEQDEP (refreshItyseq ityseq ren, dep)

and refreshItycon (itycon as E.ITYCONVAR _)              ren = itycon
  | refreshItycon (itycon as E.ITYCONNAM _)              ren = itycon
  (*| refreshItycon (itycon as E.ITYCONFUN (ityseq, ity))  ren = E.ITYCONFUN (refreshItyseq ityseq ren, refreshIty ity ren)*)
  | refreshItycon (itycon as E.ITYCONDEP (itycon', dep)) ren = E.ITYCONDEP (refreshItycon itycon' ren, dep)

fun refreshSub (E.SUBITY (ity1,    ity2))    ren = E.SUBITY (refreshIty    ity1    ren, refreshIty    ity2    ren)
  | refreshSub (E.SUBNAM (itycon1, itycon2)) ren = E.SUBNAM (refreshItycon itycon1 ren, refreshItycon itycon2 ren)
  | refreshSub (E.SUBSEQ (ityseq1, ityseq2)) ren = E.SUBSEQ (refreshItyseq ityseq1 ren, refreshItyseq ityseq2 ren)
  | refreshSub (E.SUBIMP (ityseq1, ityseq2)) ren = E.SUBIMP (refreshItyseq ityseq1 ren, refreshItyseq ityseq2 ren)

fun isEqEtyvar etyvar = String.isPrefix "''" etyvar

fun instantiateBoundIty (ityvarset, etyvarset, idorset) ity =
    let val init_ren = initRen ()
	val ren1 = E.foldrITVS
		       (fn (itv, ren) =>
			   updRenTv ren itv (E.nextItyvar ()))
		       init_ren
		       ityvarset
	val ren2 = E.foldrETVS
		       (fn (etv, ren) =>
			   updRenTe ren etv (E.nextItyvar (), isEqEtyvar etv))
		       (* TODO: if the explicit type variable is double quoted we have
			* to transform it into an implicit equality type variable. *)
		       ren1
		       etyvarset
	val ren3 = E.foldrOTVS
		       (fn (idor, ren) =>
			   updRenOr ren idor (E.nextIdor ()))
		       ren2
		       idorset
    in (refreshIty ity ren3, ren3)
    end

fun instantiateScheme (E.VPOLY (bound, (E.CSSCH (overseq, E.CSSUB subs), ity))) unifenv id label =
    let val (ity', ren) = instantiateBoundIty bound ity
	val (unifenv', env) =
	    foldr (fn ((orseq1, orseq2, deps), (unifenv, env)) =>
		      let val orseq1' = refreshOrseq orseq1 ren
			  val orseq2' = refreshOrseq orseq2 ren
		      in if existsUniEnvOve unifenv (E.getIdOrseq orseq1)
			 then (* We already have unresolved overloadings for orseq1's id.
			       * That means that we might not know what path to choose.
			       * This is not necessarily true, because the paths for the
			       * recorded unresolved overloadings might be enough to
			       * restrict further our new overloading to single paths. *)
			     let val unifenv' = updateUniEnvOve unifenv (orseq1', orseq2', deps)
			     in (unifenv', env)
			     end
			 else (* We don't have any unresolved overloading for orseq1's id. *)
			     let val cst  = E.CSITY (E.ITYOR orseq1, E.ITYOR orseq2)
				 val env' = E.applyDepsEnv (E.ENVCST cst) deps
			     in (unifenv, E.ENVAPP (env', env))
			     end
		      end)
		  (unifenv, E.ENVNUL)
		  overseq
	val env' =
	    foldr (fn (sub, env) =>
		      let val sub' = refreshSub sub ren
		      in E.ENVAPP (env, E.ENVSUB sub)
		      end)
		  E.ENVNUL
		  subs
    in (ity', unifenv', E.ENVAPP (env, env'))
    end
  | instantiateScheme (scheme as E.VODEC (bound, (idor, ity))) unifenv id label =
    let val (ity', ren) = instantiateBoundIty bound ity
	(* This update is so that we know the accessor at location label
	 * will enventually make a choice between the different choices
	 * offered by the overloading idor. *)
	val unifenv' =
	    case getRenOr idor ren of
		NONE => unifenv (* The idor part has been sliced away. *)
	      | SOME idor' => updateUniEnvLocOr unifenv label (idor', idor)
    in (ity', unifenv', E.ENVNUL)
    end
  | instantiateScheme (E.VMONO (ityvar, deps, b)) unifenv id label =
    let val ity = E.applyDepsIty (E.mk_tyvar ityvar) deps
    in (ity, unifenv, E.ENVNUL)
    end

(*(* We refresh all the type variables of itycon *)
fun freshItycon itycon =
    let val init_ren = initUni ()
	val ren = TVS.foldr (fn (tv, ren) => updUniTv ren tv (E.ITYVAR (E.nextItyvar ())))
			    init_ren
			    (getItyvarsItycon itycon)
    in refreshItycon itycon ren
    end*)

(* We refresh all the type variables of a type function *)
fun instantiateItyfun ((ityvarset, etyvarset, idorset), (ityseq, ity)) =
    let val init_ren = initRen ()
	val ren1 = E.foldrITVS
		       (fn (itv, ren) =>
			   updRenTv ren itv (E.nextItyvar ()))
		       init_ren
		       ityvarset
	val ren2 = E.foldrETVS
		       (fn (etv, ren) =>
			   updRenTe ren etv (E.nextItyvar (), isEqEtyvar etv))
		       ren1
		       etyvarset
	val ren3 = E.foldrOTVS
		       (fn (idor, ren) =>
			   updRenOr ren idor (E.nextIdor ()))
		       ren2
		       idorset
    in (refreshItyseq ityseq ren3, refreshIty ity ren3)
    end


(* ------ TYPE BUILDING ------ *)

(*(* Partially build type constructors by substituting the type cons vars
 * This is used by our solver to check whether a type constructor is an
 * actuall constructor or a function. *)
fun preBuildItycon unifenv (E.ITYCONVAR tcv) =
    (case getUniEnvTc tcv unifenv of
	 NONE => E.ITYCONVAR tcv
       | SOME itycon => preBuildItycon unifenv itycon)
  | preBuildItycon unifenv (E.ITYCONNAM tcn) = E.ITYCONNAM tcn
  | preBuildItycon unifenv (E.ITYCONFUN (ityseq, ity)) = E.ITYCONFUN ()
  | preBuildItycon unifenv (E.ITYCONDEP (itycon, dep)) = E.applyDepsItycon itycon (D.singleton dep)*)

(*(* Partially build type sequences by substituting ityseqvars only.
 * It does more that prebuilding because ity also discard the inside
 * types of the sequence.
 * This is used by our solver to check the arity of a sequence. *)
fun preBuildItyseq unifenv (E.ITYSEQVAR sv) =
    (case getUniEnvSv sv unifenv of
	 NONE => E.ITYSEQVAR sv
       | SOME ityseq => preBuildItyseq unifenv ityseq)
  | preBuildItyseq unifenv (E.ITYSEQSEQ (itys, lab)) = E.ITYSEQSEQ (map (fn _ => E.ITYVAR (E.nextItyvar ())) itys, lab)
  | preBuildItyseq unifenv (E.ITYSEQDEP (ityseq, dep)) = E.applyDepsItyseq ityseq (D.singleton dep)*)


(* Extracts the nth element in a ityseq if it exists *)
fun getItyseqN (E.ITYSEQVAR _) n = NONE
  | getItyseqN (E.ITYSEQSEQ (itys, lab)) n =
    ((SOME (List.nth (itys, n))) handle Subscript => NONE)
  | getItyseqN (E.ITYSEQDEP (ityset, dep)) n =
    (case getItyseqN ityset n of
	 NONE => NONE
       | SOME ity => SOME (E.applyDepsIty ity (D.singleton dep)))

(* Build types, type constructors and environments *)
fun buildItycon unifenv (E.ITYCONVAR tcv) =
    (case getUniEnvTc tcv unifenv of
	 NONE => E.ITYCONVAR tcv
       | SOME itycon => buildItycon unifenv itycon)
  | buildItycon unifenv (E.ITYCONNAM tcn) = E.ITYCONNAM tcn
  (*| buildItycon unifenv (E.ITYCONFUN (ityseq, ity)) = E.ITYCONFUN (buildItyseq unifenv ityseq, buildIty unifenv ity)*)
  | buildItycon unifenv (E.ITYCONDEP (itycon, dep)) =
    E.applyDepsItycon (buildItycon unifenv itycon) (D.singleton dep)

and buildIty unifenv (E.ITYVAR (tv, eq)) =
    (case getUniEnvTv tv unifenv of
	 NONE => E.ITYVAR (tv, eq)
       | SOME ity => buildIty unifenv ity)
  | buildIty unifenv (E.ITYETV etyvar) = E.ITYETV etyvar
  | buildIty unifenv (E.ITYCON (ityseq, itycon)) =
    E.ITYCON (buildItyseq unifenv ityseq, buildItycon unifenv itycon)
  (*| buildIty unifenv (E.ITYDPP (ity1, ity2)) =
   E.ITYDPP (buildIty unifenv ity1, buildIty unifenv ity2)*)
  | buildIty unifenv (E.ITYOR (E.ORSEQ (ityseq, id, lab))) =
    (*E.ITYOR (buildItyseq unifenv ityseq, id, lab)*)
    let val ityseq' = buildItyseq unifenv ityseq
    in case getUniEnvOr id unifenv of
	   NONE => E.ITYOR (E.ORSEQ (ityseq', id, lab))
	 | SOME ([n], deps) =>
	   (case getItyseqN ityseq' n of
		NONE => E.ITYOR (E.ORSEQ (ityseq', id, lab))
	      | SOME ity => E.applyDepsIty ity deps)
	 | _ => E.ITYOR (E.ORSEQ (ityseq', id, lab))
    end
  | buildIty unifenv (E.ITYTYP (ity, lab)) =
    E.ITYTYP (buildIty unifenv ity, lab)
  | buildIty unifenv (E.ITYDEP (ity, dep)) =
    E.applyDepsIty (buildIty unifenv ity) (D.singleton dep)

and buildItyseq unifenv (E.ITYSEQVAR ityseqvar) =
    (case getUniEnvSv ityseqvar unifenv of
	 NONE => E.ITYSEQVAR ityseqvar
       | SOME ityseq => buildItyseq unifenv ityseq)
  | buildItyseq unifenv (E.ITYSEQSEQ (itys, lab)) =
    E.ITYSEQSEQ (map (buildIty unifenv) itys, lab)
  | buildItyseq unifenv (E.ITYSEQDEP (ityseq, dep)) =
    E.applyDepsItyseq (buildItyseq unifenv ityseq) (D.singleton dep)

(* *** the building function below takes into account subtyping constraints *** *)

datatype direction = LT | GT | EQ

fun rev_dir LT = GT
  | rev_dir GT = LT
  | rev_dir EQ = EQ

fun toStringDir LT = "LT"
  | toStringDir GT = "GT"
  | toStringDir EQ = "EQ"

fun toStringOpDir NONE = "-"
  | toStringOpDir (SOME (dir, v)) =
    "(" ^ toStringDir dir ^ "," ^ Int.toString v ^ ")"

fun isItyconvar NONE itycon = false
  | isItyconvar (SOME tcv) (E.ITYCONVAR tcv') = tcv = tcv'
  | isItyconvar _ (E.ITYCONNAM _) = false
  | isItyconvar (opv as SOME _) (E.ITYCONDEP (itycon, dep)) = isItyconvar opv itycon

fun isItyvar NONE ity = false
  | isItyvar (SOME tv) (E.ITYVAR (tv', _)) = tv = tv'
  | isItyvar _ (E.ITYETV _) = false
  | isItyvar _ (E.ITYCON _) = false
  | isItyvar _ (E.ITYOR  _) = false
  | isItyvar _ (E.ITYTYP _) = false
  | isItyvar (opv as SOME _) (E.ITYDEP (ity, dep)) = isItyvar opv ity

fun isItyseqvar NONE ityseq = false
  | isItyseqvar (SOME sv) (E.ITYSEQVAR sv') = sv = sv'
  | isItyseqvar _ (E.ITYSEQSEQ _) = false
  | isItyseqvar (opv as SOME _) (E.ITYSEQDEP (ityseq, dep)) = isItyseqvar opv ityseq


structure STV = BinarySetFn(type ord_key = E.ityvar    val compare = Int.compare)
structure STC = BinarySetFn(type ord_key = E.tyconvar  val compare = Int.compare)
structure STS = BinarySetFn(type ord_key = E.ityseqvar val compare = Int.compare)

type setvar = {stv : STV.set ref, stc : STC.set ref, sts : STS.set ref}

fun initSetVar () = {stv = ref STV.empty, stc = ref STC.empty, sts = ref STS.empty}

fun isStv (setvar : setvar) tv = STV.member (!(#stv setvar), tv)
fun isStc (setvar : setvar) tc = STC.member (!(#stc setvar), tc)
fun isSts (setvar : setvar) sv = STS.member (!(#sts setvar), sv)

fun addStv (setvar : setvar) tv = let val st = #stv setvar in st := STV.add (!st, tv) end
fun addStc (setvar : setvar) tc = let val st = #stc setvar in st := STC.add (!st, tc) end
fun addSts (setvar : setvar) sv = let val st = #sts setvar in st := STS.add (!st, sv) end


fun buildSubItycon unifenv setvar (itycon as E.ITYCONVAR tcv) =
    (case getUniEnvTc tcv unifenv of
	 NONE =>
	 let val b = isStc setvar tcv
	     val _ = addStc setvar tcv
	     val subs1 =
		 map (fn itycon1 =>
			 let val (subs, itycon2) = buildSubItycon unifenv setvar itycon1
			 in E.SUBNAM (itycon, itycon2) :: subs
			 end)
		     (if b then [] else Option.getOpt (getUniEnvLtc tcv unifenv, []))
	     val subs2 =
		 map (fn itycon1 =>
			 let val (subs, itycon2) = buildSubItycon unifenv setvar itycon1
			 in E.SUBNAM (itycon2, itycon) :: subs
			 end)
		     (if b then [] else Option.getOpt (getUniEnvGtc tcv unifenv, []))
	 in (List.concat (subs1 @ subs2), itycon)
	 end
       | SOME itycon => buildSubItycon unifenv setvar itycon)
  | buildSubItycon unifenv setvar (E.ITYCONNAM tcn) = ([], E.ITYCONNAM tcn)
  | buildSubItycon unifenv setvar (E.ITYCONDEP (itycon, dep)) =
    let val (subs, itycon') = buildSubItycon unifenv setvar itycon
    in (subs, E.applyDepsItycon itycon' (D.singleton dep))
    end

and buildSubIty unifenv dir pol_mrk setvar (ity as E.ITYVAR (tv, eq)) =
    (case getUniEnvTv tv unifenv of
	 NONE =>
	 (*(case dir of
	      LT => (* We want the smallest type possible *)
	      (case getUniEnvGtv tv univenv of (* first, we search among the smaller types *)
		   SOME itys => (* We have to get smallest types among the itys *)
		 | NONE => ([], ity))
	      GT => (* We want the biggest type possible *)
	      ([], ity)
	      EQ => ([], ity))*)
	 let val b = isStv setvar tv
	     val _ = addStv setvar tv
	     val subs1 =
		 map (fn ity1 =>
			 let val (subs, ity2) = buildSubIty unifenv dir pol_mrk setvar ity1
			     val sub = E.SUBITY (ity, ity2)
			 in sub :: subs
			 end)
		     (if b then [] else Option.getOpt (getUniEnvLtv tv unifenv, []))
	     (* The itys2 above are either variables, or Int, or Bag *)
	     val subs2 =
		 map (fn ity1 =>
			 let val (subs, ity2) = buildSubIty unifenv dir pol_mrk setvar ity1
			     val sub = E.SUBITY (ity2, ity)
			 in sub :: subs
			 end)
		     (if b then [] else Option.getOpt (getUniEnvGtv tv unifenv, []))
	     (* The itys2 above are either variables, or Nat, or List *)
	 in (List.concat (subs1 @ subs2), ity)
	 end
       | SOME ity' => buildSubIty unifenv dir pol_mrk setvar ity')
  | buildSubIty unifenv dir pol_mrk setvar (E.ITYETV etyvar) = ([], E.ITYETV etyvar)
  | buildSubIty unifenv dir pol_mrk setvar (E.ITYCON (ityseq, itycon)) =
    let val (subs1, itycon') = buildSubItycon unifenv setvar itycon
	val bop  = E.isArrow itycon'
	val dir' = if Option.isSome bop then dir else EQ
	val nfo  = (bop, dir')
	val (subs2, ityseq') = buildSubItyseq unifenv nfo pol_mrk setvar ityseq
    in (subs1 @ subs2, E.ITYCON (ityseq', itycon'))
    end
  | buildSubIty unifenv dir pol_mrk setvar (E.ITYOR (E.ORSEQ (ityseq, id, lab))) =
    let val (subs, ityseq') = buildSubItyseq unifenv (SOME false, dir) pol_mrk setvar ityseq
	fun overloadOrError deps =
	    case pol_mrk of
		E.TOP => raise errorex (mk_pre_error TOPOVERLOAD (D.mk_deps lab))
	      (* TODO: We raise errors here but what we should really do is try
	       * to generate an overloaded declaration. *)
	      | _ => E.ITYOR (E.ORSEQ (ityseq', id, lab))
	val ity =
	    case getUniEnvOr id unifenv of
		NONE => overloadOrError ()
	      | SOME ([n], deps) =>
		(case getItyseqN ityseq' n of
		     NONE => overloadOrError ()
		   | SOME ity => E.applyDepsIty ity deps)
	      | _ => overloadOrError ()
    in (subs, ity)
    end
  | buildSubIty unifenv dir pol_mrk setvar (E.ITYTYP (ity, lab)) =
    let val (subs, ity') = buildSubIty unifenv dir pol_mrk setvar ity
    in (subs, E.ITYTYP (ity', lab))
    end
  | buildSubIty unifenv dir pol_mrk setvar (E.ITYDEP (ity, dep)) =
    let val (subs, ity') = buildSubIty unifenv dir pol_mrk setvar ity
    in (subs, E.applyDepsIty ity' (D.singleton dep))
    end
    handle errorex error => raise errorex (addErrorDeps error (D.singleton dep))

and buildSubItyseq unifenv dir pol_mrk setvar (ityseq as E.ITYSEQVAR sv) =
    (case getUniEnvSv sv unifenv of
	 NONE =>
	 let val b = isSts setvar sv
	     val _ = addSts setvar sv
	     val subs1 =
		 map (fn ityseq1 =>
			 let val (subs, ityseq2) = buildSubItyseq unifenv dir pol_mrk setvar ityseq1
			 in E.SUBSEQ (ityseq, ityseq2) :: subs
			 end)
		     (if b then [] else Option.getOpt (getUniEnvLts sv unifenv, []))
	     val subs2 =
		 map (fn ityseq1 =>
			 let val (subs, ityseq2) = buildSubItyseq unifenv dir pol_mrk setvar ityseq1
			 in E.SUBSEQ (ityseq2, ityseq) :: subs
			 end)
		     (if b then [] else Option.getOpt (getUniEnvGts sv unifenv, []))
	 in (List.concat (subs1 @ subs2), ityseq)
	 end
       | SOME ityseq => buildSubItyseq unifenv dir pol_mrk setvar ityseq)
  | buildSubItyseq unifenv (arr, dir) pol_mrk setvar (E.ITYSEQSEQ (itys, lab)) =
    let val (subsList, itys') = ListPair.unzip (map (buildSubIty unifenv dir pol_mrk setvar) itys)
    in (List.concat subsList, E.ITYSEQSEQ (itys', lab))
    end
  | buildSubItyseq unifenv dir pol_mrk setvar (E.ITYSEQDEP (ityseq, dep)) =
    let val (subs, ityseq') = buildSubItyseq unifenv dir pol_mrk setvar ityseq
    in (subs, E.applyDepsItyseq ityseq' (D.singleton dep))
    end
    handle errorex error => raise errorex (addErrorDeps error (D.singleton dep))

and smallestITycon dir (E.ITYCONVAR _) (E.ITYCONVAR _)   = NONE
  | smallestITycon dir (E.ITYCONDEP (itycon1, _)) itycon2 = smallestITycon dir itycon1 itycon2
  | smallestITycon dir itycon1 (E.ITYCONDEP (itycon2, _)) = smallestITycon dir itycon1 itycon2
  | smallestITycon dir (E.ITYCONNAM (name, _, _)) (E.ITYCONVAR _) =
    (case dir of
	 LT => SOME (SOME name)
       | GT => SOME NONE
       | EQ => NONE)
  | smallestITycon dir (E.ITYCONVAR _) (E.ITYCONNAM (name, _, _)) =
    (case dir of
	 LT => SOME NONE
       | GT => SOME (SOME name)
       | EQ => NONE)
  | smallestITycon dir (E.ITYCONNAM (name1, eq1, lab1)) (E.ITYCONNAM (name2, eq2, lab2)) =
    if name1 = name2
    then SOME (SOME name1)
    else if List.exists (fn x => x = (name1, name2)) E.subList
    then SOME (SOME name1)
    else if List.exists (fn x => x = (name2, name1)) E.subList
    then SOME NONE
    else NONE

and smallestITyseq name dir (E.ITYSEQVAR _) (E.ITYSEQVAR _) = NONE
  | smallestITyseq name dir (E.ITYSEQSEQ _) (E.ITYSEQVAR _) =
    (case dir of
	 LT => SOME true
       | GT => SOME false
       | EQ => NONE)
  | smallestITyseq name dir (E.ITYSEQVAR _) (E.ITYSEQSEQ _) =
    (case dir of
	 LT => SOME false
       | GT => SOME true
       | EQ => NONE)
  | smallestITyseq name dir (E.ITYSEQDEP (ityseq1, _)) ityseq2 = smallestITyseq name dir ityseq1 ityseq2
  | smallestITyseq name dir ityseq1 (E.ITYSEQDEP (ityseq2, _)) = smallestITyseq name dir ityseq1 ityseq2
  | smallestITyseq name dir (E.ITYSEQSEQ (itys1, _)) (E.ITYSEQSEQ (itys2, _)) =
    if name = E.tyconnameArrow
    then case (itys1, itys2) of
	     ([ity1, ity2], [ity3, ity4]) =>
	     (case smallestITy dir ity2 ity4 of
		  SOME b =>
		  (case smallestITy (rev_dir dir) ity1 ity3 of
		       SOME b' => SOME (b = b')
		     | _ => NONE)
		| NONE => NONE)
	   | _ => raise Fail "smallestITyseq:arrow"
    else foldl (fn ((ity1, ity2), SOME b) =>
		   (case smallestITy dir ity1 ity2 of
			SOME b' => SOME (b = b')
		      | _ => NONE)
		 | (_, NONE) => NONE)
	       (SOME true)
	       (ListPair.zip (itys1, itys2))

and smallestITy dir (E.ITYVAR _) (E.ITYVAR _) = NONE
  | smallestITy dir (E.ITYCON _) (E.ITYVAR _) =
    (case dir of
	 LT => SOME true
       | GT => SOME false
       | EQ => NONE)
  | smallestITy dir (E.ITYTYP _) (E.ITYVAR _) =
    (case dir of
	 LT => SOME true
       | GT => SOME false
       | EQ => NONE)
  | smallestITy dir (E.ITYVAR _) (E.ITYCON _) =
    (case dir of
	 LT => SOME false
       | GT => SOME true
       | EQ => NONE)
  | smallestITy dir (E.ITYVAR _) (E.ITYTYP _) =
    (case dir of
	 LT => SOME false
       | GT => SOME true
       | EQ => NONE)
  | smallestITy dir (E.ITYDEP (ity1, _)) ity2 = smallestITy dir ity1 ity2
  | smallestITy dir ity1 (E.ITYDEP (ity2, _)) = smallestITy dir ity1 ity2
  | smallestITy dir (E.ITYCON (ityseq1, itycon1)) (E.ITYCON (ityseq2, itycon2)) =
    (case smallestITycon dir itycon1 itycon2 of
	 SOME (SOME tc) => smallestITyseq tc dir ityseq1 ityseq2
       | SOME NONE => SOME false
       | NONE => NONE)
  | smallestITy dir (ity1 as E.ITYCON (ityseq, itycon)) (E.ITYTYP (ity, lab)) =
    smallestITy dir ity1 (E.mk_type_type lab)
  | smallestITy dir (E.ITYTYP (ity, lab)) (ity2 as E.ITYCON (ityseq, itycon)) =
    smallestITy dir (E.mk_type_type lab) ity2
  | smallestITy dir (E.ITYTYP (ity1, lab1)) (E.ITYTYP (ity2, lab2)) =
    smallestITy dir ity1 ity2
  | smallestITy dir (E.ITYOR _) _ = raise Fail "smallest or"
  | smallestITy dir _ (E.ITYOR _) = raise Fail "smallest or"
  | smallestITy dir _ _ = raise Fail "unfinished"

and findSmallestType unifenv dir pol_mrk setvar tyop [] = tyop
  | findSmallestType unifenv dir pol_mrk setvar NONE (ty :: tys) =
    let val (subs, ity) = buildSubIty unifenv dir pol_mrk setvar ty
    in findSmallestType unifenv dir pol_mrk setvar (SOME ity) tys
    end
  | findSmallestType unifenv dir pol_mrk setvar (SOME ity) (ty :: tys) =
    let val (subs, ity') = buildSubIty unifenv dir pol_mrk setvar ty
    in case smallestITy dir ity ity' of
	   SOME false => SOME ity'
	 | _ => SOME ity
    end

(* *** The building prime function below does not try to build up the variables in the set *** *)

and buildIty' unifenv (ity as E.ITYVAR (tv, eq)) ityvarset =
    if E.isinITVS (ityvarset, tv)
    then ity
    else (case getUniEnvTv tv unifenv of
	      NONE     => ity
	    | SOME ity => buildIty' unifenv ity ityvarset)
  | buildIty' unifenv (E.ITYETV etyvar) ityvarset = E.ITYETV etyvar
  | buildIty' unifenv (E.ITYCON (ityseq, itycon)) ityvarset =
    let val ityseq' = buildItyseq' unifenv ityseq ityvarset
	val itycon' = buildItycon unifenv itycon
    in E.ITYCON (ityseq', itycon')
    end
  | buildIty' unifenv (E.ITYOR (E.ORSEQ (ityseq, id, lab))) ityvarset =
    let val ityseq' = buildItyseq' unifenv ityseq ityvarset
    in case getUniEnvOr id unifenv of
	   NONE => E.ITYOR (E.ORSEQ (ityseq', id, lab))
	 | SOME ([n], deps) =>
	   (case getItyseqN ityseq' n of
		NONE => E.ITYOR (E.ORSEQ (ityseq', id, lab))
	      | SOME ity => E.applyDepsIty ity deps)
	 | _ => E.ITYOR (E.ORSEQ (ityseq', id, lab))
    end
  | buildIty' unifenv (E.ITYTYP (ity, lab)) ityvarset =
    E.ITYTYP (buildIty' unifenv ity ityvarset, lab)
  | buildIty' unifenv (E.ITYDEP (ity, dep)) ityvarset =
    E.applyDepsIty (buildIty' unifenv ity ityvarset) (D.singleton dep)

and buildItyseq' unifenv (E.ITYSEQVAR ityseqvar) ityvarset =
    (case getUniEnvSv ityseqvar unifenv of
	 NONE => E.ITYSEQVAR ityseqvar
       | SOME ityseq => buildItyseq' unifenv ityseq ityvarset)
  | buildItyseq' unifenv (E.ITYSEQSEQ (itys, lab)) ityvarset =
    E.ITYSEQSEQ (map (fn ity => buildIty' unifenv ity ityvarset) itys, lab)
  | buildItyseq' unifenv (E.ITYSEQDEP (ityseq, dep)) ityvarset =
    E.applyDepsItyseq (buildItyseq' unifenv ityseq ityvarset) (D.singleton dep)


(*and buildItoken unifenv (ITOKVAR itokvar)       =
    (case getUniEnvKv itokvar unifenv of
	 NONE => ITOKVAR itokvar
       | SOME itoken => buildItoken unifenv itoken)
  | buildItoken unifenv (ITOKITY ity)           =
    ITOKITY (buildIty unifenv ity)
  | buildItoken unifenv (ITOKDEP (itoken, dep)) =
    applyDepsItoken (buildItoken unifenv itoken) (D.singleton dep)*)

fun buildEnv unifenv (E.ENVVAR ev) =
    (case getUniEnvEv ev unifenv of
	 NONE => E.ENVVAR ev
       | SOME env => buildEnv unifenv env)
  | buildEnv unifenv env = env

fun getItyAtLab (SUCCESS (unifenv, _)) lab =
    (case LVMAP.find (getLocEq unifenv, lab) of
	 NONE                => NONE
       | SOME (ityvar, kind) => SOME (buildIty unifenv (E.mk_tyvar ityvar)))
  | getItyAtLab _ _ = NONE

fun getNameAtLab (SUCCESS (unifenv, _)) lab =
    (case LVMAP.find (getLocOr unifenv, lab) of
	 NONE => NONE
       | SOME (idor1, idor2) =>
	 (case IDORMAP.find (getNam unifenv, idor2) of
	      NONE => NONE
	    | SOME names =>
	      (case getUniEnvOr idor1 unifenv of
		   SOME ([n], _) =>
		   ((List.nth (names, n)) handle Subscript => NONE)
		 | _ => NONE)))
  | getNameAtLab _ _ = NONE

fun foldIty sets (E.ITYVAR (ityvar, _)) ity =
    if E.isConcreteIty ity (* need to build up ity -- done prior to calling the function *)
    then (E.singletonITVM (ityvar, [E.stripIty ity]), E.emptyETVM)
    else (E.emptyITVM, E.emptyETVM)
  | foldIty sets (E.ITYETV (id, _)) ity =
    if E.isConcreteIty ity
    then (E.emptyITVM, E.singletonETVM (id, [E.stripIty ity]))
    else (E.emptyITVM, E.emptyETVM)
  | foldIty sets (E.ITYCON (ityseq1, itycon1)) (E.ITYCON (ityseq2, itycon2)) =
    (case (E.stripItycon itycon1, E.stripItycon itycon2) of
	 (E.ITYCONNAM (name1, _, _), E.ITYCONNAM (name2, _, _)) =>
	 if name1 = name2
	 then foldItyseq sets ityseq1 ityseq2
	 else raise Fail "foldIty:ITYCON-name"
       | _ => raise Fail "foldIty:ITYCON-varcon")
  | foldIty sets (E.ITYCON (ityseq, itycon)) (E.ITYTYP (ity, lab)) = raise Fail "foldIty:ITYCON/TYP"
  | foldIty sets (E.ITYTYP (ity, lab)) (E.ITYCON (ityseq, itycon)) = raise Fail "foldIty:ITYTYP/CON"
  | foldIty sets (E.ITYTYP (ity1, lab1)) (E.ITYTYP (ity2, lab2)) = raise Fail "foldIty:ITYTYP"
  | foldIty sets ity1 (E.ITYDEP (ity2, _)) = foldIty sets ity1 ity2
  | foldIty sets (E.ITYDEP (ity1, _)) ity2 = foldIty sets ity1 ity2
  | foldIty _ (E.ITYCON _) _ = raise Fail "foldIty:ITYCON"
  | foldIty _ (E.ITYTYP _) _ = raise Fail "foldIty:ITYTYP"
  | foldIty _ (E.ITYOR  _) _ = raise Fail "foldIty:ITYOR"

and foldItyseq _ (E.ITYSEQVAR _) _ = raise Fail "foldItyseq:var"
  | foldItyseq _ _ (E.ITYSEQVAR _) = raise Fail "foldItyseq:var"
  | foldItyseq sets (E.ITYSEQSEQ (itys1, _)) (E.ITYSEQSEQ (itys2, _)) =
    foldr
	(fn ((ity1, ity2), (imap, emap)) =>
	    let val (imap', emap') = foldIty sets ity1 ity2
	    in (E.unionWithITVM (fn (lst1, lst2) => lst1 @ lst2) (imap, imap'),
		E.unionWithETVM (fn (lst1, lst2) => lst1 @ lst2) (emap, emap'))
	    end)
	(E.emptyITVM, E.emptyETVM)
	(ListPair.zipEq (itys1, itys2))
  | foldItyseq sets (E.ITYSEQDEP (ityseq1, _)) ityseq2 = foldItyseq sets ityseq1 ityseq2
  | foldItyseq sets ityseq1 (E.ITYSEQDEP (ityseq2, _)) = foldItyseq sets ityseq1 ityseq2

fun findRestrict lab id unifenv (sets as (ityvarset, etyvarset, idorset)) ity =
    if E.isEmptyITVS ityvarset andalso E.isEmptyETVS etyvarset
    then (sets, ity)
    else case getUniEnvAcc lab id unifenv of
	     SOME accs =>
	     let val (imap, emap) =
		     foldr (fn (ity', (imap, emap)) =>
			       let val ity'' = buildIty unifenv ity'
				   val (imap', emap') = foldIty sets ity ity''
				   (*val _ = print (":---------" ^ id ^ "\n" ^ E.ppIty' ity ^ "\n" ^ E.ppIty' ity'' ^ "\n")*)
			       in (E.unionWithITVM (fn (lst1, lst2) => lst1 @ lst2) (imap, imap'),
				   E.unionWithETVM (fn (lst1, lst2) => lst1 @ lst2) (emap, emap'))
			       end)
			   (E.emptyITVM, E.emptyETVM)
			   accs
		 val (ity1, ityvarset') =
		     E.foldriITVM (fn (ityvar, itys, (ity, ityvarset)) =>
				      if E.isinITVS (ityvarset, ityvar)
					 andalso length itys > 0
					 andalso E.allEqItys itys
				      then (E.substituteIty ity ityvar (hd itys),
					    E.removeITVS (ityvarset, ityvar) handle _ => ityvarset)
				      else (ity, ityvarset))
				  (ity, ityvarset)
				  imap
		 val (ity2, etyvarset') =
		     E.foldriETVM (fn (etyvar, itys, (ity, etyvarset)) =>
				      if E.isinETVS (etyvarset, etyvar)
					 andalso length itys > 0
					 andalso E.allEqItys itys
				      then (E.substituteEty ity etyvar (hd itys),
					    E.removeETVS (etyvarset, etyvar) handle _ => etyvarset)
				      else (ity, etyvarset))
				  (ity1, etyvarset)
				  emap
	     in ((ityvarset', etyvarset', idorset), ity2)
	     end
	   | NONE => (sets, ity)

(* Gets the scheme associated to the binder of (id,label) from the beginning
 * of the the environment. *)
fun getScheme' unifenv id label poly env =
    let fun getIdEnv (E.ENVAPP (env1, env2)) =
	    (case getIdEnv env1 of
		 FOUND scheme => FOUND scheme
	       | NOT_FOUND => getIdEnv env2
	       | SHADOWED => SHADOWED)
	  | getIdEnv (E.ENVDEP (env, dep)) = getIdEnv env
	  | getIdEnv (E.ENVBIN (E.BINDVID (vid, lab, scheme), _)) =
	    if id = vid andalso label = lab
	    then case scheme of
		     E.VPOLY (sets as (ityvarset, etyvarset, idorset), (cssch, ity)) =>
		     let val ity' = buildIty' unifenv ity ityvarset
			 val (sets', ity'') =
			     if poly
			     then (sets, ity')
			     else findRestrict lab vid unifenv sets ity'
		     in FOUND (E.VPOLY (sets', (cssch, ity'')))
		     end
		   | E.VODEC ((ityvarset, etyvarset, idorset), (idor, ity)) =>
		     let val ity' = buildIty' unifenv ity ityvarset
		     in FOUND (E.VPOLY ((ityvarset, etyvarset, idorset), (E.mk_new_cs_scheme (), ity')))
		     end
		   | E.VMONO (ityvar, deps, b) =>
		     let val ity1  = buildIty' unifenv (E.mk_tyvar ityvar) E.emptyITVS
			 val ity2  = E.applyDepsIty ity1 deps
			 val bound = (E.emptyITVS, E.emptyETVS, E.emptyOTVS)
		     in FOUND (E.VPOLY (bound, (E.mk_new_cs_scheme (), ity2)))
		     end
	    else NOT_FOUND
	  | getIdEnv (E.ENVBIN _) = NOT_FOUND
	  | getIdEnv (E.ENVVAR _) = SHADOWED
	  | getIdEnv (E.ENVMRK _) = NOT_FOUND
	  | getIdEnv (E.ENVNUL)   = NOT_FOUND
	  | getIdEnv _ = raise Fail "getScheme':Unexpected solving environment"
    in case getIdEnv env of
	   FOUND scheme => SOME scheme
	 | _ => NONE
    end

fun select_env base unifenv envs =
    let (*val _ = print ("[seaching environment for " ^ base ^ "]\n")*)
	val _ = app
		    (fn (b, e) =>
			let (*val _   = print ("[there is an environment for " ^ b ^ "]\n")*)
			    val lst = E.get_binders e
			    (*val _   = print ("  --" ^ T.fmt {init = "{", final = "}", sep = ",", fmt = fn x => x} lst ^ "\n")*)
			in ()
			end)
		    envs
    in case List.find (fn (b,e) => b = base) envs of
	   SOME (b, env) => ((*print ("[found environment for " ^ b ^ "]\n");*) env)
	 | NONE => ((*print ("[didn't find environment for " ^ base ^ "]\n");*) getEnv unifenv)
    end

fun getScheme id label poly file_base (SUCCESS (unifenv, envs)) =
    let val env = select_env file_base unifenv envs
	(*val _   = print ("**********************\n")
	  val _   = print (" -- " ^ id ^ " - " ^ Int.toString nb ^ "\n")
	  val _   = print ("**********************\n")
	  val _   = print ("  --" ^ T.fmt {init = "{", final = "}", sep = ",", fmt = fn x => x} (E.get_binders env) ^ "\n")
	  val _   = print ("**********************\n")*)
	(**)
	(*val _ = print ("\n----------------------------\n" ^
			 ":"  ^ Int.toString nb ^ ":" ^ id  ^
			 (*"\n" ^ E.toStringEnv env ^*)
			 "\n----------------------------\n")*)
	val schop = getScheme' unifenv id label poly env
    (*val _ = print "\n----------------------------\n"*)
    in schop
    end
  | getScheme id label poly file_base (ERROR _) = NONE

(*fun resolveTopAppIty ity =
    let val (ity1, deps1) = extractDepsIty ity
    in case ity1 of
	   E.ITYCON (ityseq, itycon) =>
	   let val (itycon2, deps2) = extractDepsItycon itycon
	   in case itycon2 of
		  E.ITYCONFUN (ityseq2, ity2) =>
		  let val env3 = E.ENVCST (E.CSSEQ (ityseq, ityseq2))
		      val ity3 = E.applyDepsIty (E.applyDepsIty ity2 deps1) deps2
		      val env4 = E.applyDepsEnv (E.applyDepsEnv env3 deps1) deps2
		  in (ity3, env4)
		  end
		| _ => (ity, E.ENVNUL)
	   end
	 | _ => (ity, E.ENVNUL)
    end*)


(* ------ OVERLOADING SOLVING ------ *)

datatype searchOr = OR_FOUND_TYCON of int * D.deps * E.ity
		  | OR_NOT_FOUND   of tyconErrors * D.deps
		  | OR_FOUND_VAR

fun matchItycon tn (E.ITYCONVAR _) c found ity = OR_FOUND_VAR
  | matchItycon tn (E.ITYCONNAM (tyconname, eq, lab)) c found ity =
    if tn = tyconname
    then OR_FOUND_TYCON (c, D.empty, ity)
    else (case found of
	      OR_NOT_FOUND (list, deps) =>
	      OR_NOT_FOUND ((tyconname, lab, NONE) :: list, deps)
	    | x => x)
  (*| matchItycon tn (E.ITYCONFUN _) c found ity = OR_FOUND_VAR*)
   (* if we haven't reduced the function yet it means that we've got a variable somewhere *)
  | matchItycon tn (E.ITYCONDEP (itycon, dep)) c found ity =
    (case matchItycon tn itycon c found ity of
	 OR_FOUND_VAR => OR_FOUND_VAR
       | OR_NOT_FOUND (list, deps) =>
	 (case found of
	      OR_NOT_FOUND _ => OR_NOT_FOUND (list, D.add (deps, dep))
	    | x => x)
       | OR_FOUND_TYCON (n, deps, ity) =>
	 OR_FOUND_TYCON (n, D.add (deps, dep), ity))

(* found can only be a OR_FOUND_VAR or a OR_NOT_FOUND.
 * It cannot be a OR_FOUND_TYCON. *)
fun matchIty tn (E.ITYVAR ityvar) c found =
    (* NOTE: we don't need to go down the variable because the sequence should be fully built up *)
    OR_FOUND_VAR
  | matchIty tn (E.ITYETV _) c found = raise Fail "matchIty:We shouldn't have explicit type variables in overloaded types"
  | matchIty tn (ity as E.ITYCON (ityseq, itycon)) c found =
    matchItycon tn itycon c found ity
  (*| matchIty tn (E.ITYDPP (ity1, ity2)) c found = raise Fail ""*)
  | matchIty tn (E.ITYOR _) c found = raise Fail "matchIty:we shouldn't have several layers of ORs"
  | matchIty tn (E.ITYTYP (ity, lab)) c found =
    matchIty tn (E.mk_type_type lab) c found
  | matchIty tn (E.ITYDEP (ity, dep)) c found =
    (case matchIty tn ity c found of
	 OR_FOUND_VAR => OR_FOUND_VAR
       | OR_NOT_FOUND (list, deps) =>
	 (case found of
	      OR_NOT_FOUND _ => OR_NOT_FOUND (list, D.add (deps, dep))
	    | x => x)
       | OR_FOUND_TYCON (n, deps, ity) =>
	 OR_FOUND_TYCON (n, D.add (deps, dep), ity))

(* Search for tn in a type sequence *)
fun matchOr unifenv tn (E.ITYSEQVAR sv) restriction =
    (* NOTE: We shouldn't need to do that because the sequence should be built up *)
    OR_FOUND_VAR
    (*(case getUniEnvSv sv unifenv of
	 NONE => OR_FOUND_VAR
       | SOME ityseq => matchOr unifenv tn ityseq)*)
  | matchOr unifenv tn (E.ITYSEQSEQ (itys, lab)) restriction =
    let fun isin c NONE = true
	  | isin (c : int) (SOME list) = List.exists (fn n => n = c) list
	fun match (OR_FOUND_TYCON x) ity c = OR_FOUND_TYCON x
	  | match (OR_NOT_FOUND x)   ity c = matchIty tn ity c (OR_NOT_FOUND x)
	  | match OR_FOUND_VAR       ity c = matchIty tn ity c OR_FOUND_VAR
	fun find (ity, (search, c)) =
	    let val search' =
		    if isin c restriction
		    then match search ity c
		    else search
	    in (search', c + 1)
	    end
    in #1 (foldl find (OR_NOT_FOUND ([], D.empty), 0) itys)
    end
  | matchOr unifenv tn (E.ITYSEQDEP (ityseq, dep)) restriction =
    (case matchOr unifenv tn ityseq restriction of
	 OR_FOUND_TYCON (n, deps, ity) => OR_FOUND_TYCON (n, D.add (deps, dep), ity)
       | OR_NOT_FOUND (list, deps) => OR_NOT_FOUND (list, D.add (deps, dep))
       | OR_FOUND_VAR => OR_FOUND_VAR)

fun getToNinSeq (E.ITYSEQVAR _) n = NONE
  | getToNinSeq (E.ITYSEQSEQ (itys, lab)) n =
    (SOME (E.applyDepsIty (List.nth (itys, n)) (D.singleton (D.mk_dep lab)))
     handle Subscript => NONE)
  | getToNinSeq (E.ITYSEQDEP (ityseq, dep)) n =
    (case getToNinSeq ityseq n of
	 SOME ity => SOME (E.applyDepsIty ity (D.singleton dep))
       | NONE => NONE)

(* Check whether a type from a or sequence is full of type constructions *)
structure FULL = SplayMapFn(type ord_key = string val compare = String.compare)

fun fullAddDep tcdeps dep =
    FULL.map (fn (n, lab, ty, deps) => (n, lab, ty, D.add (deps, dep))) tcdeps

fun orFullItycon (E.ITYCONVAR _) _ _ = NONE
  | orFullItycon (E.ITYCONNAM (tyconname, eq, lab)) n ity = SOME (FULL.insert (FULL.empty, tyconname, (n, lab, ity, D.empty)))
  | orFullItycon (E.ITYCONDEP (itycon, dep)) n ity =
    (case orFullItycon itycon n ity of
	 NONE => NONE
       | SOME tcdeps => SOME (fullAddDep tcdeps dep))

fun orFullIty (ity as E.ITYVAR _) _ = NONE
  | orFullIty (ity as E.ITYETV _) _ = raise Fail "orFullIty:We shouldn't have explicit type variables in overloaded types"
  | orFullIty (ity as E.ITYCON (_, itycon)) n = orFullItycon itycon n ity
  | orFullIty (ity as E.ITYOR _) _ = raise Fail "orFullIty:we shouldn't have several layers of ORs"
  | orFullIty (ity as E.ITYTYP _) _ = raise Fail "orFullIty:ITYTYP"
  | orFullIty (ity as E.ITYDEP (ity', dep)) n =
    (case orFullIty ity' n of
	 NONE => NONE
       | SOME tcdeps => SOME (fullAddDep tcdeps dep))

fun orFull (E.ITYSEQVAR sv) = NONE
  | orFull (E.ITYSEQSEQ (itys, lab)) =
    #1 (foldl (fn (ity, (NONE, n)) => (NONE, n + 1)
		| (ity, (SOME tcdeps1, n)) =>
		  (case orFullIty ity n of
		       NONE => (NONE, n + 1)
		     | SOME tcdeps2 =>
		       (SOME (FULL.unionWith (fn _ => raise Fail "orFull") (tcdeps1, tcdeps2)), n + 1)))
	      (SOME FULL.empty, 0)
	      itys)
  | orFull (E.ITYSEQDEP (ityseq, dep)) =
    (case orFull ityseq of
	 NONE => NONE
       | SOME tcdeps => SOME (fullAddDep tcdeps dep))

fun ovEqCompare ((ityvar1_1, ityvar1_2),
		 (ityvar2_1, ityvar2_2)) =
    case Int.compare (ityvar1_1, ityvar2_1) of
	EQUAL => Int.compare (ityvar1_2, ityvar2_2)
      | x => x

structure OVEQMAP = BinaryMapFn(struct type ord_key = E.ityvar * E.ityvar val compare = ovEqCompare end)

(* We're looking for simple overloadings, i.e., 2 sequences of types constrained to
 * be equal and such that if a list of pairs (ityvar1, ityvar2) could be generated
 * for one pair (ity1, ity2) such that ity1 is from ityseq1 and ity2 is from ityseq2,
 * and such that ity1 could potentially be equal to ity2, then the list would also
 * be generated for all the other pairs. *)
fun simpleOverloading ityseq1 ityseq2 paths =
    let fun addDep map dep = OVEQMAP.map (fn deps => D.add (deps, dep)) map
	fun addDeps map deps = OVEQMAP.map (fn deps' => D.union (deps, deps')) map
	fun merge map1 map2 =
	    OVEQMAP.unionWith (fn (deps1, deps2) => D.union (deps1, deps2))
			      (map1, map2)
	fun genPairsIty (E.ITYCON (ityseq1, itycon1)) (E.ITYCON (ityseq2, itycon2)) =
	    let val (itycon1', deps1) = E.extractDepsItycon itycon1
		val (itycon2', deps2) = E.extractDepsItycon itycon2
	    in case (itycon1', itycon2') of
		   (E.ITYCONVAR _, _) => NONE
		 | (_, E.ITYCONVAR _) => NONE
		 | (E.ITYCONNAM (name1, _, _), E.ITYCONNAM (name2, _, _)) =>
		   if name1 = name2
		   then case genPairsItyseq ityseq1 ityseq2 of
			    NONE => NONE
			  | SOME pairs => SOME (addDeps pairs (D.union (deps1, deps2)))
		   else NONE (* here we know that that we can't take that path. *)
		 | _ => raise Fail "genPairsIty:all dependencies have been stripped off"
	    end
	  | genPairsIty (E.ITYVAR (ityvar1, eq1)) (E.ITYVAR (ityvar2, eq2)) =
	    SOME (OVEQMAP.singleton ((ityvar1, ityvar2), D.empty))
	  | genPairsIty ity (E.ITYOR (E.ORSEQ (ityseq, idor, lab))) =
	    let val (ityseq', deps) = E.extractDepsItyseq ityseq
	    in case ityseq' of
		   E.ITYSEQVAR _ => NONE
		 | E.ITYSEQSEQ (itys, dep) => raise Fail "genPairsIty:ITYOR:ORSEQ:ITYSEQSEQ"
		 | E.ITYSEQDEP _ => raise Fail "genPairsIty:ITYOR:ORSEQ:ITYSEQDEP:all dependencies have been stripped off"
	    end
	  | genPairsIty (E.ITYOR (E.ORSEQ (ityseq, idor, lab))) ity =
	    raise Fail "genPairsIty:ITYOR:ORSEQ"
	  | genPairsIty (E.ITYDEP (ity1, dep)) ity2 =
	    (case genPairsIty ity1 ity2 of
		 NONE => NONE
	       | SOME pairs => SOME (addDep pairs dep))
	  | genPairsIty ity1 (E.ITYDEP (ity2, dep)) =
	    (case genPairsIty ity1 ity2 of
		 NONE => NONE
	       | SOME pairs => SOME (addDep pairs dep))
	  | genPairsIty _ _ = NONE
	and genPairsItyseq (E.ITYSEQSEQ (itys1, dep1)) (E.ITYSEQSEQ (itys2, dep2)) =
	    if List.length itys1 = List.length itys2
	    then foldr (fn (_, NONE) => NONE
			 | ((ity1, ity2), SOME set) =>
			   case genPairsIty ity1 ity2 of
			       NONE => NONE
			     | SOME set' => SOME (merge set' set))
		       (SOME OVEQMAP.empty)
		       (ListPair.zip (itys1, itys2))
	    else NONE
	  | genPairsItyseq (E.ITYSEQDEP (ityseq1, dep)) ityseq2 =
	    (case genPairsItyseq ityseq1 ityseq2 of
		 NONE => NONE
	       | SOME pairs => SOME (addDep pairs dep))
	  | genPairsItyseq ityseq1 (E.ITYSEQDEP (ityseq2, dep)) =
	    (case genPairsItyseq ityseq1 ityseq2 of
		 NONE => NONE
	       | SOME pairs => SOME (addDep pairs dep))
	  | genPairsItyseq _ _ = NONE
	fun genPairs (n1, n2) =
	    case (getToNinSeq ityseq1 n1, getToNinSeq ityseq2 n2) of
		(SOME ity1, SOME ity2) => genPairsIty ity1 ity2
	      | _ => NONE
	val pairs = map genPairs paths
    in #1 (foldr (fn (_, (NONE, b)) => (NONE, b)
		   | (NONE, (_, b)) => (NONE, b)
		   | (SOME map, (_, false)) => (SOME map, true)
		   | (SOME map1, (SOME map2, true)) =>
		     let val map = OVEQMAP.intersectWith (fn (deps1, deps2) => D.union (deps1, deps2)) (map1, map2)
		     in if OVEQMAP.numItems map = OVEQMAP.numItems map1
			   andalso
			   OVEQMAP.numItems map = OVEQMAP.numItems map2
			then (SOME map, true)
			else (NONE, false)
		     end)
		 (SOME OVEQMAP.empty, false)
		 pairs)
    end

(*fun matchOrs unifenv (E.ITYSEQVAR _) ityseq2 = OR_FOUND_VAR
  | matchOrs unifenv (E.ITYSEQSEQ (itys, lab)) ityseq2 =
    let fun matchTn (E.ITYCONVAR _) = (unifenv, deps, E.ENVNUL)
	  | matchTn (E.ITYCONNAM (tyconname, eq, lab)) =
	    (case matchOr unifenv tyconname ityseq1 of
		 OR_NOT_FOUND (tns, deps') =>
		 let val kind = OVERLOAD ((tyconname, lab), tns)
		 in raise errorex (mk_pre_error kind (D.union (deps', deps)))
		 end
	       | OR_FOUND_VAR => (unifenv, deps, E.ENVNUL)
	       | OR_FOUND_TYCON (n, deps', ty3) =>
		 let val deps''   = D.union (deps, deps')
		     val path     = (n, deps'')
		     val unifenv' = updateUniEnvUniOr unifenv id path
		     val env      = E.ENVCST (E.CSITY (ty3, ty2))
		 in (unifenv', deps'', env)
		 end)
	  | matchTn (E.ITYCONDEP (itycon, dep)) =
	    (let val (unifenv', deps', env') = matchTn itycon
	     in (unifenv', D.add (deps', dep), env')
	     end handle errorex error =>
			raise errorex (applyDepsError error (D.singleton dep)))

	fun matchIty (E.ITYVAR _) = OR_FOUND_VAR
	  | matchIty (E.ITYETV _) = raise EH.Impossible "explicit type variables shouldn't occur in overloading sequences"
	  | matchIty (E.ITYCON (ityseq, itycon)) = matchTn itycon
	  | matchIty (E.ITYDEP (ity, dep)) =
	    raise Fail ""


	val _ = foldr (fn (ity, ...) =>
			  case matchIty ity of
			      ...)
		      ...
		      itys

	val (unifenv', deps', env) = matchTn itycon
    in solveEnv unifenv' deps' env D.dummy_dep
    end
  | matchOrs unifenv (E.ITYSEQDEP (ityseq1, dep)) ityseq2 =
    let val _ = matchOrs unifenv ityseq1 ityseq2
    in ...
    end*)


(* ------ EQUALITY TYPE SOLVING ------ *)

datatype 'a eqsolv = EQOK  of 'a
		   | EQERR of D.label * D.deps

fun isEqIty (E.ITYVAR (ityvar, eq)) lab =
    EQOK (E.ITYVAR (ityvar, E.mergeEqTv (eq, E.EQ lab)))
  | isEqIty (E.ITYETV (etyvar, lab')) lab =
    if isEqEtyvar etyvar
    then EQOK (E.ITYETV (etyvar, lab'))
    else EQERR (lab', D.empty)
  | isEqIty (E.ITYCON (ityseq, itycon)) lab =
    (case isEqItycon itycon lab of
	 EQOK itycon' =>
	 (case isEqItyseq ityseq lab of
	      EQOK ityseq' => EQOK (E.ITYCON (ityseq', itycon'))
	    | EQERR x => EQERR x)
       | EQERR x => EQERR x)
  | isEqIty (E.ITYOR orseq) lab =
    (case isEqOrseq orseq lab of
	 EQOK orseq' => EQOK (E.ITYOR orseq')
       | EQERR x => EQERR x)
  | isEqIty (E.ITYTYP (ity, lab')) lab = EQERR (lab, D.empty)
  | isEqIty (E.ITYDEP (ity, dep)) lab =
    (case isEqIty ity lab of
	 EQOK ity' => EQOK (E.ITYDEP (ity', dep))
       | EQERR (lab', deps) => EQERR (lab', D.add (deps, dep)))

and isEqOrseq (E.ORSEQ (ityseq, idor, lab')) lab =
    (case isEqItyseq ityseq lab of
	 EQOK ityseq' => EQOK (E.ORSEQ (ityseq', idor, lab'))
       | EQERR x => EQERR x)

and isEqItyseq (E.ITYSEQVAR ityseqvar) lab =
    EQOK (E.ITYSEQVAR ityseqvar)
  | isEqItyseq (E.ITYSEQSEQ (itys, lab')) lab =
    let val x = foldl (fn (ity, EQOK itys') =>
			  (case isEqIty ity lab of
			       EQOK ity' => EQOK (itys' @ [ity'])
			     | EQERR x => EQERR x)
			| (ity, EQERR x) => EQERR x)
		      (EQOK [])
		      itys
    in case x of
	   EQOK itys' => EQOK (E.ITYSEQSEQ (itys', lab'))
	 | EQERR x => EQERR x
    end
  | isEqItyseq (E.ITYSEQDEP (ityseq, dep)) lab =
    (case isEqItyseq ityseq lab of
	 EQOK ityseq' => EQOK (E.ITYSEQDEP (ityseq', dep))
       | EQERR (lab', deps) => EQERR (lab', D.add (deps, dep)))

and isEqItycon (E.ITYCONVAR tyconvar) lab = EQOK (E.ITYCONVAR tyconvar)
  | isEqItycon (E.ITYCONNAM (name, eqtc, lab')) lab =
    if eqtc
    then EQOK (E.ITYCONNAM (name, eqtc, lab'))
    else EQERR (lab', D.empty)
  | isEqItycon (E.ITYCONDEP (itycon, dep)) lab =
    (case isEqItycon itycon lab of
	 EQOK itycon' => EQOK (E.ITYCONDEP (itycon', dep))
       | EQERR (lab', deps) => EQERR (lab', D.add (deps, dep)))


(* ------ SOLVER ------ *)

val first = ref false

val envs = ref []
fun addEnv file env =
    let val file = #file (OS.Path.splitDirFile file)
	val base = #base (OS.Path.splitBaseExt file)
    in envs := (!envs) @ [(base,env)]
    end
fun resetEnvs () = envs := []
fun getEnvs () = !envs

fun setFirst   () = first := true
fun unsetFirst () = first := false
fun isFirst    () = !first

(* Extract a solved environment from the first unifenv and the last one *)
fun solvedEnv (E.ENVAPP (env, E.ENVMRK marker0)) marker =
    if marker = marker0
    then E.ENVNUL
    else E.mk_env_app (solvedEnv env marker, E.ENVMRK marker0)
  | solvedEnv (E.ENVAPP (env1, env2)) marker = E.ENVAPP (solvedEnv env1 marker, env2)
  | solvedEnv (E.ENVMRK marker0) marker =
    if marker = marker0
    then E.ENVNUL
    else E.ENVMRK marker0
  (* impossible cases: *)
  | solvedEnv (E.ENVVAR _) _ = raise Fail "Unexpected type environment in unification environment:ENVVAR(solved)"
  | solvedEnv (E.ENVBIN _) _ = raise Fail "Unexpected type environment in unification environment:ENVBIN(solved)"
  | solvedEnv (E.ENVDEP _) _ = raise Fail "Unexpected type environment in unification environment:ENVDEP(solved)"
  | solvedEnv (E.ENVNUL)   _ = raise Fail "Unexpected type environment in unification environment:ENVNUL(solved)"
  | solvedEnv (E.ENVFIL _) _ = raise Fail "Unexpected type environment in unification environment:ENVFIL(solved)"
  | solvedEnv (E.ENVLOC _) _ = raise Fail "Unexpected type environment in unification environment:ENVLOC(solved)"
  | solvedEnv (E.ENVCST _) _ = raise Fail "Unexpected type environment in unification environment:ENVCST(solved)"
  | solvedEnv (E.ENVSUB _) _ = raise Fail "Unexpected type environment in unification environment:ENVSUB(solved)"
  | solvedEnv (E.ENVPOL _) _ = raise Fail "Unexpected type environment in unification environment:ENVPOL(solved)"
  | solvedEnv (E.ENVSET _) _ = raise Fail "Unexpected type environment in unification environment:ENVSET(solved)"
  | solvedEnv (E.ENVERR _) _ = raise Fail "Unexpected type environment in unification environment:ENVERR(solved)"
  | solvedEnv (E.ENVOVL _) _ = raise Fail "Unexpected type environment in unification environment:ENVOVL(solved)"
  | solvedEnv (E.ENVTOF _) _ = raise Fail "Unexpected type environment in unification environment:ENVTOF(solved)"
  | solvedEnv (E.ENVLAB _) _ = raise Fail "Unexpected type environment in unification environment:ENVLAB(solved)"
  | solvedEnv (E.ENVOVE _) _ = raise Fail "Unexpected type environment in unification environment:ENVOVE(solved)"
  | solvedEnv (E.ENVACC _) _ = raise Fail "Unexpected type environment in unification environment:ENVACC(solved)"

fun solvedUniEnv unifenv marker = solvedEnv (getEnv unifenv) marker

(* isMinMaxSub LT ity is true (SOME ...) if ity is minimal, meaning that it cannot
 * have a subtype other than itself (isMinMaxSub GT ity is similar but for supertypes).*)
fun isMinMaxSub _ (E.ITYVAR _) = NONE
  | isMinMaxSub _ (E.ITYETV x) = SOME (E.ENVNUL, E.ITYETV x)
  | isMinMaxSub dir (ity as E.ITYCON (ityseq, itycon)) =
    (case isMinMaxSubItycon dir itycon of
	 NONE => NONE
       | SOME (name, true) => NONE (* means that name can actually have something smaller *)
       | SOME (name, _) => (* name cannot have anything smaller *)
	 let val sv   = E.nextItyseqvar ()
	     val seq  = E.ITYSEQVAR sv
	     val pair = case dir of
			    LT => (seq, ityseq) (* we're dealing with a type < ity *)
			  | GT => (ityseq, seq) (* we're dealing with a type > ity *)
			  | EQ => raise Fail "isMinMaxSub:ITYCON"
	     val sub  = if name = E.tyconnameArrow
			then E.SUBIMP pair
			else E.SUBSEQ pair
	     val ity = E.ITYCON (seq, itycon)
	 in SOME (E.ENVSUB sub, ity)
	 end)
  | isMinMaxSub dir (E.ITYOR orseq) = NONE
  | isMinMaxSub dir (E.ITYTYP (ity, lab)) = raise Fail "isMinMaxSub:ITYTYP"
  | isMinMaxSub dir (E.ITYDEP (ity, dep)) =
    case isMinMaxSub dir ity of
	NONE => NONE
      | SOME (env, ity') => SOME (env, E.ITYDEP (ity', dep))

and isMinMaxSubItycon _ (E.ITYCONVAR _) = NONE
  | isMinMaxSubItycon dir (itycon as E.ITYCONNAM (name, _, _)) =
    if case dir of
	   LT => List.exists (fn x => x = name) (map (fn (_, x) => x) E.subList)
	 (* is there a smaller type constructor? *)
	 | GT => List.exists (fn x => x = name) (map (fn (x, _) => x) E.subList)
	 (* is there a greater type constructor? *)
	 | EQ => false
    then SOME (name, true)
    else SOME (name, false)
  (* here false means that we the type constructor witch is > or < than itycon
   * is actually = to itycon *)
  | isMinMaxSubItycon dir (E.ITYCONDEP (itycon, _)) = isMinMaxSubItycon dir itycon

and isMinMaxSubItyseq _ (E.ITYSEQVAR _) = NONE
  | isMinMaxSubItyseq dir (E.ITYSEQSEQ (itys, lab)) =
    let val itys_v = map (fn _ => E.mk_new_tyvar ()) itys
	val subs   = map (fn (ity_v, ity) =>
			     case dir of
				 LT => E.SUBITY (ity_v, ity)
			       | GT => E.SUBITY (ity, ity_v)
			       | EQ => raise Fail "isMinMaxSubItyseq:ITYSEQSEQ")
			 (ListPair.zip (itys_v, itys))
	val env    = E.list2env (map E.ENVSUB subs)
    in SOME (env, E.ITYSEQSEQ (itys_v, lab))
    end
  | isMinMaxSubItyseq dir (E.ITYSEQDEP (ityseq, dep)) =
    case isMinMaxSubItyseq dir ityseq of
	NONE => NONE
      | SOME (env, ityseq') => SOME (env, E.ITYSEQDEP (ityseq', dep))

(* Extract the type variables that can be generalised for a type *)
fun getMonoTyvarsBind unifenv (E.BINDVID (vid, lab, E.VMONO (ityvar, deps, b))) =
    let val ity1 = buildIty unifenv (E.mk_tyvar ityvar)
	val ity2 = E.applyDepsIty ity1 deps
    in E.getDepsIEtyvarsIty ity2
    end
  | getMonoTyvarsBind _ _ = initIEMon ()

(* returns: mon map, a bool that's true if env shadows (with a env var), a second
 * bool that's true if we've hit the mon marker *)
fun getMonoTyvarsEnv unifenv (E.ENVVAR _) mrk = (true, false, initIEMon ())
  | getMonoTyvarsEnv unifenv (E.ENVBIN (bind, _)) mrk = (false, false, getMonoTyvarsBind unifenv bind)
  | getMonoTyvarsEnv unifenv (E.ENVAPP (env1, env2)) mrk =
    let val (bvar2, bmrk2, map2) = getMonoTyvarsEnv unifenv env2 mrk
    in if bvar2 orelse bmrk2
       then (bvar2, bmrk2, map2)
       else let val (bvar1, bmrk1, map1) = getMonoTyvarsEnv unifenv env1 mrk
	    in (bvar1, bmrk1, unionMon map1 map2)
	    end
    end
  | getMonoTyvarsEnv unifenv (E.ENVDEP (env, _)) mrk = getMonoTyvarsEnv unifenv env mrk
  (* Don't we need to get the dependency for an E.ENVDEP? *)
  | getMonoTyvarsEnv unifenv E.ENVNUL _ = (false, false, initIEMon ())
  | getMonoTyvarsEnv unifenv (E.ENVMRK marker) (SOME mrk) = (false, marker = mrk, initIEMon ())
  | getMonoTyvarsEnv unifenv (E.ENVMRK marker) NONE = (false, false, initIEMon ())
  | getMonoTyvarsEnv _ (E.ENVFIL _) _ = raise Fail "Unexpected type environment in unification environment:ENVFIL(mono)"
  | getMonoTyvarsEnv _ (E.ENVLOC _) _ = raise Fail "Unexpected type environment in unification environment:ENVLOC(mono)"
  | getMonoTyvarsEnv _ (E.ENVCST _) _ = raise Fail "Unexpected type environment in unification environment:ENVCST(mono)"
  | getMonoTyvarsEnv _ (E.ENVSUB _) _ = raise Fail "Unexpected type environment in unification environment:ENVSUB(mono)"
  | getMonoTyvarsEnv _ (E.ENVPOL _) _ = raise Fail "Unexpected type environment in unification environment:ENVPOL(mono)"
  | getMonoTyvarsEnv _ (E.ENVSET _) _ = raise Fail "Unexpected type environment in unification environment:ENVSET(mono)"
  | getMonoTyvarsEnv _ (E.ENVERR _) _ = raise Fail "Unexpected type environment in unification environment:ENVERR(mono)"
  | getMonoTyvarsEnv _ (E.ENVOVL _) _ = raise Fail "Unexpected type environment in unification environment:ENVOVL(mono)"
  | getMonoTyvarsEnv _ (E.ENVTOF _) _ = raise Fail "Unexpected type environment in unification environment:ENVTOF(mono)"
  | getMonoTyvarsEnv _ (E.ENVLAB _) _ = raise Fail "Unexpected type environment in unification environment:ENVLAB(mono)"
  | getMonoTyvarsEnv _ (E.ENVOVE _) _ = raise Fail "Unexpected type environment in unification environment:ENVOVE(mono)"
  | getMonoTyvarsEnv _ (E.ENVACC _) _ = raise Fail "Unexpected type environment in unification environment:ENVACC(mono)"

fun getOverseqIty unifenv (E.ITYVAR _) = IDORMAP.empty
  | getOverseqIty unifenv (E.ITYETV _) = IDORMAP.empty
  | getOverseqIty unifenv (E.ITYCON (ityseq, _)) = getOverseqItyseq unifenv ityseq
  | getOverseqIty unifenv (E.ITYOR (E.ORSEQ (ityseq, idor, lab))) =
    let val map = getOverseqItyseq unifenv ityseq
    in case getUniEnvOr idor unifenv of
	   NONE => map
	 | SOME (paths, deps) => raise Fail "getOverseqIty"
	   (*let val map1 = IDORMAP.singleton (idor, [(deps, List.map ! orseqs)])
	       val map2 = IDORMAP.unionWith (fn (list1, list2) => list1 @ list2) (map, map1)
	   in map2
	   end*)
    end
  | getOverseqIty unifenv (E.ITYTYP (ity, lab)) = raise Fail "getOverseqIty:ITYTYP"
  | getOverseqIty unifenv (E.ITYDEP (ity, dep)) =
    IDORMAP.map (fn list =>
		    map (fn (deps, x) => (D.add (deps, dep), x)) list)
		(getOverseqIty unifenv ity)

and getOverseqItyseq unifenv (E.ITYSEQVAR _) = IDORMAP.empty
  | getOverseqItyseq unifenv (E.ITYSEQSEQ (itys, lab)) =
    foldr (fn (ity, map) =>
	      IDORMAP.unionWith (fn (list1, list2) => list1 @ list2)
				(map, getOverseqIty unifenv ity))
	  IDORMAP.empty
	  itys
  | getOverseqItyseq unifenv (E.ITYSEQDEP (ityseq, dep)) =
    IDORMAP.map (fn list =>
		    map (fn (deps, x) => (D.add (deps, dep), x)) list)
		(getOverseqItyseq unifenv ityseq)

fun applyDepItyvarsOverseq bound monos pol_mrk overseq =
    foldr (fn ((E.ORSEQ (ityseq1, id1, lab1),
		E.ORSEQ (ityseq2, id2, lab2),
		deps),
	       (bound, overseq)) =>
	      let val (bound1, ityseq1') = E.applyDepsIEtyvarsItyseq ityseq1 monos pol_mrk
		  val (bound2, ityseq2') = E.applyDepsIEtyvarsItyseq ityseq2 monos pol_mrk
		  val bound' = E.mergeBound (E.mergeBound (bound1, bound2), bound)
		  val orseq1 = E.ORSEQ (ityseq1', id1, lab1)
		  val orseq2 = E.ORSEQ (ityseq2', id2, lab2)
	      in (bound', (orseq1, orseq2, deps) :: overseq)
	      end)
	  (bound, [])
	  overseq

fun int2nat (E.ITYVAR _) = NONE
  | int2nat (E.ITYETV _) = NONE
  | int2nat (E.ITYCON (ityseq, itycon)) =
    let fun int2natItycon (E.ITYCONVAR _) = NONE
	  | int2natItycon (E.ITYCONNAM (name, eq, lab)) =
	    if name = E.tyconnameInt
	    then SOME (E.ITYCONNAM (E.tyconnameNat, eq, lab))
	    else NONE
	  | int2natItycon (E.ITYCONDEP (itycon, dep)) =
	    case int2natItycon itycon of
		SOME itycon => SOME (E.ITYCONDEP (itycon, dep))
	      | NONE => NONE
    in case int2natItycon itycon of
	   SOME itycon => SOME (E.ITYCON (ityseq, itycon))
	 | NONE => NONE
    end
  | int2nat (E.ITYOR _) = NONE
  | int2nat (E.ITYTYP (ity, lab)) = raise Fail "int2nat:ITYTYP"
  | int2nat (E.ITYDEP (ity, dep)) =
    case int2nat ity of
	NONE => NONE
      | SOME ity => SOME (E.ITYDEP (ity, dep))

fun applyDepSub bound monos pol_mrk (E.SUBITY (ity1, ity2)) =
    let val (bound1, ity1') = E.applyDepsIEtyvarsIty ity1 monos pol_mrk
	val (bound2, ity2') = E.applyDepsIEtyvarsIty ity2 monos pol_mrk
	val bound' = E.mergeBound (E.mergeBound (bound1, bound2), bound)
    in (bound', E.SUBITY (ity1', ity2'))
    end
  | applyDepSub bound monos pol_mrk (E.SUBNAM (itycon1, itycon2)) =
    (bound, E.SUBNAM (itycon1, itycon2))
  | applyDepSub bound monos pol_mrk (E.SUBSEQ (ityseq1, ityseq2)) =
    let val (bound1, ityseq1') = E.applyDepsIEtyvarsItyseq ityseq1 monos pol_mrk
	val (bound2, ityseq2') = E.applyDepsIEtyvarsItyseq ityseq2 monos pol_mrk
	val bound' = E.mergeBound (E.mergeBound (bound1, bound2), bound)
    in (bound', E.SUBSEQ (ityseq1', ityseq2'))
    end
  | applyDepSub bound monos pol_mrk (E.SUBIMP (ityseq1, ityseq2)) =
    let val (bound1, ityseq1') = E.applyDepsIEtyvarsItyseq ityseq1 monos pol_mrk
	val (bound2, ityseq2') = E.applyDepsIEtyvarsItyseq ityseq2 monos pol_mrk
	val bound' = E.mergeBound (E.mergeBound (bound1, bound2), bound)
    in (bound', E.SUBIMP (ityseq1', ityseq2'))
    end

fun applyDepSubs bound monos pol_mrk subs =
    foldr (fn (sub, (bound, subs)) =>
	      let val (bound', sub') = applyDepSub bound monos pol_mrk sub
	      in (bound', sub' :: subs)
	      end)
	  (bound, [])
	  subs

fun recomputeMonos unifenv (imon, emon) =
    E.foldriITVM (fn (ityvar, deps, monos) =>
		     let val ity1   = buildIty unifenv (E.mk_tyvar ityvar)
			 val ity2   = E.applyDepsIty ity1 deps
			 val monos' = E.getDepsIEtyvarsIty ity2
		     in unionMon monos' monos
		     end)
		 (initIMon (), emon)
		 imon

(*fun simpleSubtypingSimplification monotyvars subs =
    foldr (fn (E.SUBITY (ity1, ity2), subs) =>
	      let val (ity1', deps1) = E.extractDepsIty ity1
		  val (ity2', deps2) = E.extractDepsIty ity2
	      in case (ity1', ity2') of
		     (E.ITYVAR (ityvar1, eq1), E.ITYVAR (ityvar2, eq2)) =>
		     (case )
	      end)
	  []
	  subs*)

fun getGenTyVars vid ity0 unifenv pol_mrk =
    let (*val _ = dumpUniEnvSubs unifenv*)
	(*val _ = dumpUniEnvEqTv unifenv*)
	(*val _ = print (E.toStringIty ity0 ^ "\n")*)
	(*val _ = raise Fail ""*)
	val (subs, ity1)       = buildSubIty unifenv LT pol_mrk (initSetVar ()) ity0
	(* We get the monomorphic type variables that we computed earlier *)
	val (mrk, monos1)      = getMon unifenv
	(* We compute the new set of monomorphic type variables. *)
	val (_, _, monos2)     = getMonoTyvarsEnv unifenv (getEnv unifenv) mrk
	val monos              = unionMon (recomputeMonos unifenv monos1) monos2
	val overseq            = IDORMAP.foldr (op @) [] (getOve unifenv)
	val (bound, ity2)      = E.applyDepsIEtyvarsIty ity1 monos pol_mrk
	val (bound1, overseq') = applyDepItyvarsOverseq bound monos pol_mrk overseq
	val (bound2, subs')    = applyDepSubs bound monos pol_mrk subs
    in (bound2, overseq', subs', ity2, monos)
    end

(*(* extracts the arity of a type constructor *)
fun getArityItycon' (E.ITYCONVAR _) _ = NONE
  | getArityItycon' (E.ITYCONNAM (_, arity, label)) deps = SOME (arity, label, deps)
  | getArityItycon' (E.ITYCONDEP (itycon, dep)) deps = getArityItycon' itycon (D.union (deps, D.singleton dep))

fun getArityItycon itycon = getArityItycon' itycon D.empty*)

fun updMonosTopPoly unifenv monos E.TOP =
    let val mrk      = E.nextMarker ()
	val unifenv1 = replaceUniEnvMon unifenv (SOME mrk, monos)
	val unifenv2 = updateUniEnvEnv unifenv1 (E.ENVMRK mrk)
    in unifenv2
    end
  | updMonosTopPoly unifenv monos E.CON =
    let val mrk      = E.nextMarker ()
	val unifenv1 = replaceUniEnvMon unifenv (SOME mrk, monos)
	val unifenv2 = updateUniEnvEnv unifenv1 (E.ENVMRK mrk)
    in unifenv2
    end
  | updMonosTopPoly unifenv monos E.NES = unifenv

(* Transform a monomorphic environment into a polymorphic one *)
fun toPoly unifenv1 unifenv2 marker pol_mrk =
    let val env1 = getEnv unifenv1
	val env2 = getEnv unifenv2
	val unifenv = replaceUniEnvEnv unifenv2 env1
	(* (2011-08-25) BUG
	 * Arghhh! this stripBind works only for a single binder
	 * because if we have more than one binder we can end up
	 * accumulating the dependencies of all the binders and
	 * then have deps1 comming from bind1 constrain bind2!!
	 *)
	val (envbind, deps) = E.stripBind (solvedEnv env2 marker)
	fun update E.ENVNUL unifenv = unifenv
	  | update (E.ENVAPP (env1, env2)) unifenv =
	    let val unifenv1 = update env1 unifenv
	    in update env2 unifenv1
	    end
	  | update (E.ENVBIN (E.BINDVID (vid, lab, E.VMONO (ityvar, deps0, b)), kind)) unifenv =
	    let val deps1 = D.union (deps, deps0)
		(*val _ = print ("id: " ^ vid ^ " " ^ Int.toString ityvar ^ "\n")*)
		val ity0  = E.mk_tyvar ityvar
		val ity1  = E.applyDepsIty ity0 deps1
		val (bound, overseq, subs, ity2, monos) = getGenTyVars vid ity1 unifenv pol_mrk
		val _ = if List.null subs
			then ()
			else (app (fn s => print (E.toStringSub (E.stripSub s) ^ "\n")) subs;
			      print "--\n";
			      print (E.ppIty' (E.stripIty ity2) ^ "\n");
			      print "--\n";
			      raise Fail "sub")
		val cssch = E.CSSCH (overseq, E.CSSUB subs)
		val bind' = E.BINDVID (vid, lab, E.VPOLY (bound, (cssch, ity2)))
		val unifenv' = updMonosTopPoly unifenv monos pol_mrk
		(*val _ = print (vid ^ " " ^ E.toStringIty ity2 ^ "\n")*)
	    in updateUniEnvEnv unifenv' (E.applyDepsEnv (E.ENVBIN (bind', kind)) deps)
	    end
	  | update (E.ENVBIN (E.BINDVID (vid, lab, E.VODEC (bound, (idor, ity))), kind)) unifenv =
	    let val ity1 = E.applyDepsIty ity deps
		val (bound', _, _, ity2, monos) = getGenTyVars vid ity1 unifenv pol_mrk
		val bind' = E.BINDVID (vid, lab, E.VODEC (bound', (idor, ity2)))
		val unifenv' = updMonosTopPoly unifenv monos pol_mrk
	    in updateUniEnvEnv unifenv' (E.applyDepsEnv (E.ENVBIN (bind', kind)) deps)
	    end
	  | update (env as E.ENVBIN (E.BINDVID (vid, lab, E.VPOLY _), _)) unifenv =
	    updateUniEnvEnv unifenv (E.applyDepsEnv env deps)
	  | update (env as E.ENVBIN (E.BINDTYC _, _)) unifenv =
	    updateUniEnvEnv unifenv (E.applyDepsEnv env deps)
	  | update env unifenv =
	    (print (E.toStringEnv env);
	     raise Fail "The environment should be a constrained binder")
    in update envbind unifenv
    end

(* Checks that there are no unresolved overloading at top level *)
fun noOverloadingAtTopPoly unifenv deps E.TOP =
    app (fn list =>
	    app (fn (orseq1, orseq2, deps0) =>
		    let val deps1 = E.getDepsOrseq orseq1
			val deps2 = E.getDepsOrseq orseq2
			val deps' = D.union (D.union (deps, deps0), D.union (deps1, deps2))
			(*val _     = print "*******************\n"*)
		    in raise errorex (mk_pre_error TOPOVERLOAD deps')
		    end)
		list)
	(listUniEnvOve unifenv)
  | noOverloadingAtTopPoly _ _ _ = ()
(* TODO: try to generate an overloading type. *)

fun isDumIty (E.ITYVAR (ityvar, eq)) = ityvar = E.ityvarFresh
  | isDumIty (E.ITYETV _)            = false
  | isDumIty (E.ITYCON _)            = false
  | isDumIty (E.ITYOR _)             = false
  | isDumIty (E.ITYTYP _)            = false
  | isDumIty (E.ITYDEP (ity, _))     = isDumIty ity

fun isDumItyseq (E.ITYSEQVAR ityseqvar)   = ityseqvar = E.ityseqvarFresh
  | isDumItyseq (E.ITYSEQSEQ _)           = false
  | isDumItyseq (E.ITYSEQDEP (ityseq, _)) = isDumItyseq ityseq

fun decorateIty E.USERCODE label ity = ity
  | decorateIty E.PRELUDE label (ity as E.ITYVAR _) = ity
  | decorateIty E.PRELUDE label (ity as E.ITYETV _) = ity
  | decorateIty E.PRELUDE label (ity as E.ITYCON (ityseq, itycon)) =
    let val ityseq' = decorateItyseq E.PRELUDE label ityseq
	val itycon' = decorateItycon E.PRELUDE label itycon
    in E.ITYCON (ityseq', itycon')
    end
  | decorateIty E.PRELUDE label (ity as E.ITYOR _) = ity
  | decorateIty E.PRELUDE label (ity as E.ITYTYP (ity', lab)) =
    E.ITYTYP (decorateIty E.PRELUDE label ity', lab)
  | decorateIty E.PRELUDE label (ity as E.ITYDEP (ity', dep)) =
    E.ITYDEP (decorateIty E.PRELUDE label ity', dep)

and decorateItyseq E.USERCODE label ityseq = ityseq
  | decorateItyseq E.PRELUDE label (ityseq as E.ITYSEQVAR _) = ityseq
  | decorateItyseq E.PRELUDE label (ityseq as E.ITYSEQSEQ (itys, dep)) =
    E.ITYSEQSEQ (map (decorateIty E.PRELUDE label) itys, label)
  | decorateItyseq E.PRELUDE label (ityseq as E.ITYSEQDEP (ityseq', dep)) =
    E.ITYSEQDEP (decorateItyseq E.PRELUDE label ityseq', dep)

and decorateItycon E.USERCODE label itycon = itycon
  | decorateItycon E.PRELUDE label (itycon as E.ITYCONVAR _) = itycon
  | decorateItycon E.PRELUDE label (itycon as E.ITYCONNAM (name, eq, lab)) =
    E.ITYCONNAM (name, eq, label)
  | decorateItycon E.PRELUDE label (itycon as E.ITYCONDEP (itycon', dep)) =
    E.ITYCONDEP (decorateItycon E.PRELUDE label itycon', dep)

(* Environment solver *)
fun solveEnv unifenv deps (E.ENVVAR ev) _ =
    updateUniEnvEnv unifenv (E.applyDepsEnv (buildEnv unifenv (E.ENVVAR ev)) deps)
  (*(case getUniEnvEv ev unifenv of
	 NONE     => updateUniEnvEnv unifenv (E.ENVVAR ev)
       | SOME env => solveEnv unifenv deps env)*)
  | solveEnv unifenv deps (E.ENVBIN (bind, kind)) _ = solveBind unifenv deps bind kind
  | solveEnv unifenv deps (E.ENVACC acc)  dep = solveAcc unifenv deps acc dep
  | solveEnv unifenv deps (E.ENVCST cst)  _   = solveCst unifenv deps cst
  | solveEnv unifenv deps (E.ENVSUB sub)  _   =
    if getSwitchSub ()
    then let (*val _ = print ("(A)\n")*)
	     (*val _ = print (E.toStringSub (E.stripSub sub) ^ "\n")*)
	     val unifenv = solveSub unifenv deps sub
	     (*val _ = print ("(B)\n")*)
	 in unifenv
	 end
    else solveCst unifenv deps (E.sub2cst sub)
  | solveEnv unifenv deps (E.ENVPOL (env, pol_mrk)) dep =
    let val unifenv1 = resetUniEnvOveMrk unifenv pol_mrk
	val marker   = E.nextMarker ()
	val emarker  = E.ENVMRK marker
	val unifenv2 = updateUniEnvEnv unifenv1 emarker
	(*val _ = print "(A)\n"*)
	(*val _ = print (E.toStringEnv env ^ "\n")*)
	val unifenv3 = solveEnv unifenv2 deps env dep
	(*val _ = print "(B)\n"*)
	val _        = noOverloadingAtTopPoly unifenv3 deps pol_mrk
	val unifenv4 = toPoly unifenv unifenv3 marker pol_mrk
	val unifenv5 = resetUniEnvOveMrk unifenv4 pol_mrk
    in unifenv5
    end
  | solveEnv unifenv deps (E.ENVAPP (env1, env2)) dep =
    let (*val _ = print ("(1) - " ^ E.toStringEnv env1 ^ "\n")*)
	val unifenv1 = solveEnv unifenv  deps env1 dep
	(*val _ = dumpUniEnvSubs unifenv1*)
	(*val _ = dumpUniEnvEqTv unifenv1*)
	(*val _ = print ("(2) - " ^ E.toStringEnv env2 ^ "\n")*)
	val unifenv2 = solveEnv unifenv1 deps env2 dep
	(*val _ = print "(3)\n"*)
    in unifenv2
    end
  | solveEnv unifenv deps (E.ENVSET envs) dep =
    foldl (fn (env, unifenv) => solveEnv unifenv deps env dep)
	  unifenv
	  envs
  | solveEnv unifenv deps (E.ENVLOC (env1, env2)) dep =
    let val unifenv1 = solveEnv unifenv deps env1 dep
	(* NOTE: unifenv1 is obtained from unifenv after sovling env1 *)
	val marker   = E.nextMarker ()
	val emarker  = E.ENVMRK marker
	val unifenv' = updateUniEnvEnv unifenv1 emarker
	(* NOTE: unifenv' is unifenv1 with a marker *)
	val unifenv2 = solveEnv unifenv' deps env2 dep
	(* NOTE: unifenv2 is obtained from unifenv' after solving env2 *)
	val unifenv3 = replaceUniEnvEnv unifenv2 (getEnv unifenv)
	(* NOTE: unifenv3 is unifenv2 (after solving env2) in which we only keep
	 * the original env *)
	val senv     = solvedEnv (getEnv unifenv2) marker
	(*val _        = print ("[++locenv]\n")
	 val _        = print (E.toStringEnv senv ^ "\n")
	 val _        = print ("[--locenv]\n")*)
	val unifenv4 = updateUniEnvEnv unifenv3 senv
    (* NOTE: unifenv4 is unifenv3 in which we add in the environment that has
     * been generated when solving env2 but not env1 (we use a marker for that). *)
    (* NOTE: We get rid of the environments generated while solving env1 *)
    in unifenv4
    end
  | solveEnv unifenv deps (E.ENVDEP (env, dep))   _ = solveEnv unifenv (D.add (deps, dep)) env dep
  | solveEnv unifenv deps (E.ENVFIL (file, env1, env2, label)) dep =
    (let val unifenv1 = solveEnv unifenv (D.add (deps, D.mk_dep label)) env1 dep
	 (* We save the environment associated to file "file" *)
	 val _        = addEnv file (getEnv unifenv1)
	 val marker   = E.nextMarker ()
	 val emarker  = E.ENVMRK marker
	 val unifenv' = updateUniEnvEnv unifenv1 emarker
	 val unifenv2 = solveEnv unifenv' deps env2 dep
	 val unifenv3 = replaceUniEnvEnv unifenv2 (getEnv unifenv)
	 val senv     = solvedEnv (getEnv unifenv2) marker
	 val unifenv4 = updateUniEnvEnv unifenv3 senv
	 (*val envfile  = getEnv unifenv1
	 val envfile' = E.ENVDEP (envfile, D.mk_dep label)*)
     in (*replaceUniEnvEnv unifenv1 envfile'*)  (*unifenv1*) resetUniEnvVar unifenv4
     end handle errorex error =>
		let val deps0 = getErrorDeps error
		    val deps1 = D.add (deps0, D.mk_dep label)
		in raise errorex (setErrorDeps error deps1)
		end)
  | solveEnv unifenv deps (E.ENVERR (st, E.PARSE))   _ = raise errorex (mk_pre_error (UNPARSABLE st)    deps)
  | solveEnv unifenv deps (E.ENVERR (st, E.REBOUND)) _ = raise errorex (mk_pre_error (UNREBOUNDABLE st) deps)
  | solveEnv unifenv deps (E.ENVERR (st, E.SYNTAX))  _ = raise errorex (mk_pre_error (SYNTAXERROR st)   deps)
  | solveEnv unifenv deps (E.ENVOVL tycon) _ =
    (case getUniEnvTyc tycon unifenv of
	 FOUND ((bound, (seq, E.ITYVAR (ityv, eq))), _, _) => unifenv
       | FOUND ((bound, (seq, ity)), _, _) =>
	 let val msg   = "In an overloading statement, a type variable cannot be overloaded to two types with the same type constructor"
	     val deps' = D.union (deps, E.getDepsIty ity)
	 in raise errorex (mk_pre_error (SYNTAXERROR msg) deps')
	 end
       | _ => unifenv)
  | solveEnv unifenv deps (E.ENVTOF vid) _ =
    (if isFirst ()
     then case getUniEnvVid vid unifenv of
	      SHADOWED           => ()
	    | NOT_FOUND          => print ("[" ^ vid ^ " not found]\n")
	    | FOUND (scheme,_,_) => print ("[" ^ vid ^ " has type " ^ E.ppScheme scheme ^ "]\n")
     else ();
     unifenv)
  | solveEnv unifenv deps (E.ENVMRK _) _ =
    raise Fail "marker environment should not be generated at inital constraint generation"
  (* NOTE: here (below) label should be the same as the label in the dep *)
  | solveEnv unifenv deps (E.ENVLAB (lab, ityvar)) dep = updateUniEnvLocEq unifenv lab ityvar
  | solveEnv unifenv deps (E.ENVOVE (idor, names)) dep = updateUniEnvNam unifenv idor names
  | solveEnv unifenv deps E.ENVNUL _ = unifenv

(* Accessor solver *)
and solveAcc unifenv deps (E.ACCVID (vid, ityvar)) (D.DEPL label) =
    (case getUniEnvVid vid unifenv of
	 SHADOWED  => unifenv
       | NOT_FOUND =>
	 let (*val _ = print ("\n***************************\n" ^
			    vid ^
			    "\n" ^
			    E.toStringEnv (getEnv unifenv) ^
			    "\n***************************\n")*)
	 in raise errorex (mk_pre_error (FREEID vid) deps)
	 end
       | FOUND (scheme, lab, kind) =>
	 let val (ity1, unifenv1, env) = instantiateScheme scheme unifenv vid label
	 in if isDumIty ity1
	    then unifenv1
	    else let val ity2 = decorateIty kind label ity1
		     (*val _ = print (vid ^ " " ^ E.toStringIty ity2 ^ "\n")*)
		     (*val _ = print (vid ^ " " ^ E.toStringIty (stripIty ity') ^ "\n")*)
		     val cst  = E.CSITY (E.mk_tyvar ityvar, ity2)
		     val env' = E.ENVAPP (E.ENVCST cst, env)
		     val unifenv2 = updateUniEnvAcc unifenv1 lab vid ity2
		 in solveEnv unifenv2 deps env' D.dummy_dep
		 end
	 end)
  | solveAcc unifenv deps (E.ACCTYC (tycon, (sv, tv))) (D.DEPL label) =
    (case getUniEnvTyc tycon unifenv of
	 SHADOWED  => unifenv
       | NOT_FOUND => raise errorex (mk_pre_error (FREEID tycon) deps)
       | FOUND (ityfun, lab, kind) =>
	 let val (seq, ity) = instantiateItyfun ityfun
	 in if isDumIty ity orelse isDumItyseq seq
	    then unifenv
	    else let val ity' = decorateIty    kind label ity
		     val seq' = decorateItyseq kind label seq
		     val env1 = E.ENVCST (E.CSSEQ (E.ITYSEQVAR sv, seq'))
		     val env2 = E.ENVCST (E.CSITY (E.mk_tyvar tv, ity'))
		 in solveEnv unifenv deps (E.ENVAPP (env1, env2)) D.dummy_dep
		 end
	 end)
  (* NOTE: we've got to refresh the variables in itycon *)
  | solveAcc unifenv deps (E.ACCTYV (tyvar, ityvar)) (D.DEPL label) =
    (case getUniEnvTyv tyvar unifenv of
	 SHADOWED  => unifenv
       | NOT_FOUND => raise errorex (mk_pre_error (FREEID tyvar) deps)
       | FOUND ((ity, tvbind_kind), lab, _) =>
	 let val (ity', deps') = E.extractDepsIty ity
	 in case ity' of
		E.ITYVAR (ityv, eq) =>
		if ityv = E.ityvarFresh (*orelse label = D.dummy_label*)
		then unifenv
		else let val ity0 = case tvbind_kind of
					E.EXPLICIT => E.ITYETV (tyvar, label)
				      | E.IMPLICIT => E.mk_tyvar ityv
			 val cst  = E.CSITY (ity0, E.mk_tyvar ityvar)
		     in solveCst unifenv (D.union (deps, deps')) cst
		     end
	      | _ => raise Fail "explicit type variable binder should bind an implicit type variable"
	 end)
  | solveAcc unifenv deps (E.ACCATM (atoms, ityvar)) (D.DEPL label) =
    (case getUniEnvAtm atoms unifenv of
	 SHADOWED  => unifenv
       | NOT_FOUND => raise errorex (mk_pre_error (FREEATOMS atoms) deps)
       | FOUND (scheme, lab, _) =>
	 let val (ity', unifenv', _) = instantiateScheme scheme unifenv atoms label
	 (* NOTE: here the generated env should be E.ENVNUL because the
	  * atom binders are monomorphic. *)
	 in if isDumIty ity'
	    then unifenv'
	    else solveCst unifenv' deps (E.CSITY (ity', E.mk_tyvar ityvar))
	 end)

(* Binding solver *)
and solveBind unifenv deps (E.BINDTYC (tycon, lab, (bound, (ityseq, ity)))) kind =
    let val ityseq' = buildItyseq unifenv ityseq
	val ity'    = buildIty unifenv ity
	val bound1  = E.getIEtyvarsItyseq ityseq'
	val bound2  = E.getIEtyvarsIty ity'
	(* NOTE: the idor part of the bounds is empty because their
	 * should be no overloading types in ity. *)
	val bound   = E.mergeBound (bound1, bound2)
	val bind    = E.BINDTYC (tycon, lab, (bound, (ityseq', ity')))
	val env     = E.applyDepsEnv (E.ENVBIN (bind, kind)) deps
    (*val _  = print (E.toStringEnv env ^ "\n")*)
    in updateUniEnvEnv unifenv env
    end
  (* NOTE: when accessing a type constructor which is a type function or
   * a partial type constructor (due to slicing), we want to refresh the
   * type variables, such that the type variables of the parameter of the
   * type function if it is a type function. *)
  | solveBind unifenv deps (E.BINDVAR (id, lab, ity)) kind =
    updateUniEnvVar unifenv id (E.applyDepsIty ity deps) lab
  | solveBind unifenv deps (bind as E.BINDVID (vid, lab, E.VMONO (ityvar, deps', b))) kind =
    let val unifenv' = updateUniEnvEnv unifenv (E.applyDepsEnv (E.ENVBIN (bind, kind)) deps)
    in if b
       then case getUniEnvVar vid unifenv of
		SOME (ity,l) =>
		if D.isDummyLab l
		then unifenv'
		else solveCst unifenv' deps (E.CSITY (E.mk_tyvar ityvar, ity))
	      | NONE => unifenv'
       else unifenv'
    end
  | solveBind unifenv deps bind kind =
    updateUniEnvEnv unifenv (E.applyDepsEnv (E.ENVBIN (bind, kind)) deps)

(* Subtyping constraint solver *)
and solveSub unifenv deps (sub as E.SUBITY (E.ITYDEP (ity1, dep), ity2)) = solveSub unifenv (D.add (deps, dep)) (E.SUBITY (ity1, ity2))
  | solveSub unifenv deps (sub as E.SUBITY (ity1, E.ITYDEP (ity2, dep))) = solveSub unifenv (D.add (deps, dep)) (E.SUBITY (ity1, ity2))
  | solveSub unifenv deps (sub as E.SUBNAM (E.ITYCONDEP (itycon1, dep), itycon2)) = solveSub unifenv (D.add (deps, dep)) (E.SUBNAM (itycon1, itycon2))
  | solveSub unifenv deps (sub as E.SUBNAM (itycon1, E.ITYCONDEP (itycon2, dep))) = solveSub unifenv (D.add (deps, dep)) (E.SUBNAM (itycon1, itycon2))
  | solveSub unifenv deps (sub as E.SUBSEQ (E.ITYSEQDEP (ityseq1, dep), ityseq2)) = solveSub unifenv (D.add (deps, dep)) (E.SUBSEQ (ityseq1, ityseq2))
  | solveSub unifenv deps (sub as E.SUBSEQ (ityseq1, E.ITYSEQDEP (ityseq2, dep))) = solveSub unifenv (D.add (deps, dep)) (E.SUBSEQ (ityseq1, ityseq2))
  | solveSub unifenv deps (sub as E.SUBITY (ity1 as E.ITYVAR (tv1, eq1), ity2 as E.ITYVAR (tv2, eq2))) =
    if tv1 = tv2
    then unifenv
    else let val eq = E.mergeEqTv (eq1, eq2)
	 in case (getUniEnvTv tv1 unifenv, getUniEnvTv tv2 unifenv) of
		(NONE, NONE) =>
		let val ity2 = E.applyDepsIty (E.ITYVAR (tv2, eq)) deps
		    val ity1 = E.applyDepsIty (E.ITYVAR (tv1, eq)) deps
		    val (unifenv1, env1) = updateUniEnvSubLtv unifenv tv1 ity2
		    val (unifenv2, env2) = updateUniEnvSubGtv unifenv tv2 ity1
		in solveEnv unifenv2 deps (E.ENVAPP (env1, env2)) D.dummy_dep
		end
	      | (SOME ity, NONE) => solveSub unifenv deps (E.SUBITY (ity, ity2))
	      | (NONE, SOME ity) => solveSub unifenv deps (E.SUBITY (ity1, ity))
	      | (SOME ity1, SOME ity2) => solveSub unifenv deps (E.SUBITY (ity1, ity2))
	 end
  | solveSub unifenv deps (sub as E.SUBITY (E.ITYVAR (tv1, eq1), ity2)) =
    (case getUniEnvTv tv1 unifenv of
	 NONE => (case isMinMaxSub LT ity2 of (* This asks: can there be a type smaller than ity2? *)
		      NONE =>
		      (* Yes there can be such a type. *)
		      let val ity2' = E.applyDepsIty ity2 deps
			  (* TODO: we have to do our equality type contamination! *)
			  val (unifenv', env) = updateUniEnvSubLtv unifenv tv1 ity2'
		      in solveEnv unifenv' deps env D.dummy_dep
		      end
		    | SOME (env, ity') =>
		      (* There might be such a type, but we can constrain tv1 further using the top tycons of ity2. *)
		      let val unifenv' = solveOccIty unifenv deps tv1 eq1 ity'
		      in solveEnv unifenv' deps env D.dummy_dep
		      end)
       | SOME ity1 => solveSub unifenv deps (E.SUBITY (ity1, ity2)))
  | solveSub unifenv deps (sub as E.SUBITY (ity1, E.ITYVAR (tv2, eq2))) =
    (case getUniEnvTv tv2 unifenv of
	 NONE => (case isMinMaxSub GT ity1 of (* This asks: can there be a type bigger than ity1? *)
		      NONE =>
		      (* Yes there can be such a type. *)
		      let val ity1' = E.applyDepsIty ity1 deps
			  val (unifenv', env) = updateUniEnvSubGtv unifenv tv2 ity1'
		      in solveEnv unifenv' deps env D.dummy_dep
		      end
		    | SOME (env, ity') =>
		      (* There might be such a type, but we can constrain tv2 further using the top tycons of ity1. *)
		      let val unifenv' = solveOccIty unifenv deps tv2 eq2 ity'
		      in solveEnv unifenv' deps env D.dummy_dep
		      end)
       | SOME ity2 => solveSub unifenv deps (E.SUBITY (ity1, ity2)))
  | solveSub unifenv deps (sub as E.SUBITY (ity1 as E.ITYCON (ityseq1, itycon1), ity2 as E.ITYCON (ityseq2, itycon2))) =
    let val (itycon1', deps1) = E.extractDepsItycon itycon1
	val (itycon2', deps2) = E.extractDepsItycon itycon2
    in case (itycon1', itycon2') of
	   (E.ITYCONVAR _, _) => unifenv
	 | (_, E.ITYCONVAR _) => unifenv
	 | (ityconname1 as E.ITYCONNAM (name1, _, _), ityconname2 as E.ITYCONNAM (name2, _, _)) =>
	   let val deps' = D.union (D.union (deps1, deps2), deps)
	       val env1  = E.ENVSUB (E.SUBNAM (ityconname1, ityconname2))
	       val env2  = if name1 = E.tyconnameArrow
			      andalso
			      name2 = E.tyconnameArrow
			   then E.ENVSUB (E.SUBIMP (ityseq1, ityseq2))
			   else E.ENVSUB (E.SUBSEQ (ityseq1, ityseq2))
	   in solveEnv unifenv deps' (E.ENVAPP (env1, env2)) D.dummy_dep
	   end
	 | (E.ITYCONDEP _, _) => raise Fail "all the dependencies should have been removed"
	 | (_, E.ITYCONDEP _) => raise Fail "all the dependencies should have been removed"
    end
  | solveSub unifenv deps (sub as E.SUBITY (ity1 as E.ITYETV _, ity2 as E.ITYETV _)) =
    solveCst unifenv deps (E.CSITY (ity1, ity2))
  | solveSub unifenv deps (sub as E.SUBITY (ity1 as E.ITYCON _, ity2 as E.ITYOR _)) =
    (case isMinMaxSub GT ity1 of (* This asks: can there be a type bigger than ity1? *)
	 NONE => (* Yes there can be such a type. *)
	 let val ity1'    = E.applyDepsIty ity1 deps
	     val ity2'    = E.applyDepsIty ity2 deps
	     val unifenv' = updateUniEnvSubPairTv unifenv (ity1', ity2')
	 in unifenv'
	 end
       | SOME (env, ity') => (* There might be such a type, but we can constrain ity2 further using the top tycons of ity1. *)
	 let val unifenv' = solveCst unifenv deps (E.CSITY (ity', ity2))
	 in solveEnv unifenv' deps env D.dummy_dep
	 end)
  | solveSub unifenv deps (sub as E.SUBITY (ity1 as E.ITYOR _, ity2 as E.ITYCON _)) =
    (case isMinMaxSub LT ity2 of
	 NONE =>
	 let val ity1'    = E.applyDepsIty ity1 deps
	     val ity2'    = E.applyDepsIty ity2 deps
	     val unifenv' = updateUniEnvSubPairTv unifenv (ity1', ity2')
	 in unifenv'
	 end
       | SOME (env, ity') =>
	 let val unifenv' = solveCst unifenv deps (E.CSITY (ity1, ity'))
	 in solveEnv unifenv' deps env D.dummy_dep
	 end)
  | solveSub unifenv deps (sub as E.SUBITY (ity1 as E.ITYOR _, ity2 as E.ITYOR _)) =
    let val ity1'    = E.applyDepsIty ity1 deps
	val ity2'    = E.applyDepsIty ity2 deps
	val unifenv' = updateUniEnvSubPairTv unifenv (ity1', ity2')
    in unifenv'
    end
  | solveSub unifenv deps (sub as E.SUBITY (ity1, ity2)) =
    (print (E.toStringIty ity1 ^ "\n" ^
	    E.toStringIty ity2 ^ "\n");
     raise Fail "solveSub:SUBITY")
  | solveSub unifenv deps (sub as E.SUBNAM (itycon1 as E.ITYCONVAR tcv1, itycon2 as E.ITYCONVAR tcv2)) =
    if tcv1 = tcv2
    then unifenv
    else (case (getUniEnvTc tcv1 unifenv, getUniEnvTc tcv2 unifenv) of
	      (NONE, NONE) =>
	      let val itycon2 = E.applyDepsItycon (E.ITYCONVAR tcv2) deps
		  val itycon1 = E.applyDepsItycon (E.ITYCONVAR tcv1) deps
		  val (unifenv1, env1) = updateUniEnvSubLtc unifenv tcv1 itycon2
		  val (unifenv2, env2) = updateUniEnvSubGtc unifenv tcv2 itycon1
	      in solveEnv unifenv2 deps (E.ENVAPP (env1, env2)) D.dummy_dep
	      end
	    | (SOME itycon, NONE) => solveSub unifenv deps (E.SUBNAM (itycon, itycon2))
	    | (NONE, SOME itycon) => solveSub unifenv deps (E.SUBNAM (itycon1, itycon))
	    | (SOME itycon1, SOME itycon2) => solveSub unifenv deps (E.SUBNAM (itycon1, itycon2)))
  | solveSub unifenv deps (sub as E.SUBNAM (E.ITYCONVAR tcv1, itycon2)) =
    (case getUniEnvTc tcv1 unifenv of
	 NONE => let fun updateSub () =
			 let val (unifenv', env) = updateUniEnvSubLtc unifenv tcv1 itycon2
			 in solveEnv unifenv' deps env D.dummy_dep
			 end
		 in case isMinMaxSubItycon LT itycon2 of
			NONE            => updateSub ()
		      | SOME (_, true)  => updateSub ()
		      | SOME (_, false) => solveOccItycon unifenv deps tcv1 itycon2
		 end
       | SOME itycon1 => solveSub unifenv deps (E.SUBNAM (itycon1, itycon2)))
  | solveSub unifenv deps (sub as E.SUBNAM (itycon1, E.ITYCONVAR tcv2)) =
    (case getUniEnvTc tcv2 unifenv of
	 NONE => let fun updateSub () =
			 let val (unifenv', env) = updateUniEnvSubGtc unifenv tcv2 itycon1
			 in solveEnv unifenv' deps env D.dummy_dep
			 end
		 in case isMinMaxSubItycon GT itycon1 of
			NONE            => updateSub ()
		      | SOME (_, true)  => updateSub ()
		      | SOME (_, false) => solveOccItycon unifenv deps tcv2 itycon1
		 end
       | SOME itycon2 => solveSub unifenv deps (E.SUBNAM (itycon1, itycon2)))
  | solveSub unifenv deps (sub as E.SUBNAM (E.ITYCONNAM (name1, eq1, lab1), E.ITYCONNAM (name2, eq2, lab2))) =
    if name1 = name2
       orelse
       List.exists (fn x => x = (name1, name2)) E.subList
       orelse
       (name1 = E.tyconnameUnit andalso name2 = E.tyconnameTuple)
       orelse
       (name2 = E.tyconnameUnit andalso name1 = E.tyconnameTuple)
    then unifenv
    else raise errorex (mk_pre_error (TYCONSCLASH ((name1, lab1, NONE), (name2, lab2, NONE))) deps)
  | solveSub unifenv deps (sub as E.SUBSEQ (ityseq1 as E.ITYSEQVAR sv1, ityseq2 as E.ITYSEQVAR sv2)) =
    if sv1 = sv2
    then unifenv
    else (case (getUniEnvSv sv1 unifenv, getUniEnvSv sv2 unifenv) of
	      (NONE, NONE) =>
	      let val ityseq2 = E.applyDepsItyseq (E.ITYSEQVAR sv2) deps
		  val ityseq1 = E.applyDepsItyseq (E.ITYSEQVAR sv1) deps
		  val (unifenv1, env1) = updateUniEnvSubLts unifenv sv1 ityseq2
		  val (unifenv2, env2) = updateUniEnvSubGts unifenv sv2 ityseq1
	      in solveEnv unifenv2 deps (E.ENVAPP (env1, env2)) D.dummy_dep
	      end
	    | (SOME ityseq, NONE) => solveSub unifenv deps (E.SUBSEQ (ityseq, ityseq2))
	    | (NONE, SOME ityseq) => solveSub unifenv deps (E.SUBSEQ (ityseq1, ityseq))
	    | (SOME ityseq1, SOME ityseq2) => solveSub unifenv deps (E.SUBSEQ (ityseq1, ityseq2)))
  | solveSub unifenv deps (sub as E.SUBSEQ (E.ITYSEQVAR sv1, ityseq2)) =
    (case getUniEnvSv sv1 unifenv of
	 NONE => (case isMinMaxSubItyseq LT ityseq2 of
		      NONE =>
		      let val (unifenv', env) = updateUniEnvSubLts unifenv sv1 ityseq2
		      in solveEnv unifenv' deps env D.dummy_dep
		      end
		    | SOME (env, ityseq') =>
		      let val unifenv' = solveOccItyseq unifenv deps sv1 ityseq'
		      in solveEnv unifenv' deps env D.dummy_dep
		      end)
       | SOME ityseq1 => solveSub unifenv deps (E.SUBSEQ (ityseq1, ityseq2)))
  | solveSub unifenv deps (sub as E.SUBSEQ (ityseq1, E.ITYSEQVAR sv2)) =
    (case getUniEnvSv sv2 unifenv of
	 NONE => (case isMinMaxSubItyseq GT ityseq1 of
		      NONE =>
		      let val (unifenv', env) = updateUniEnvSubGts unifenv sv2 ityseq1
		      in solveEnv unifenv' deps env D.dummy_dep
		      end
		    | SOME (env, ityseq') =>
		      let val unifenv' = solveOccItyseq unifenv deps sv2 ityseq'
		      in solveEnv unifenv' deps env D.dummy_dep
		      end)
       | SOME ityseq2 => solveSub unifenv deps (E.SUBSEQ (ityseq1, ityseq2)))
  | solveSub unifenv deps (sub as E.SUBSEQ (E.ITYSEQSEQ (itys1, lab1), E.ITYSEQSEQ (itys2, lab2))) =
    let val n1 = List.length itys1
	val n2 = List.length itys2
    in if n1 = n2
       then let val envs = E.list2env (map (fn pair => E.ENVSUB (E.SUBITY pair)) (ListPair.zip (itys1, itys2)))
	    in solveEnv unifenv deps envs D.dummy_dep
	    end
       else raise errorex (mk_pre_error (ARITYCLASH ((n1, lab1, NONE), (n2, lab2, NONE))) deps)
    end
  | solveSub unifenv deps (sub as E.SUBIMP (E.ITYSEQDEP (ityseq1, dep), ityseq2)) =
    solveSub unifenv (D.add (deps, dep)) (E.SUBIMP (ityseq1, ityseq2))
  | solveSub unifenv deps (sub as E.SUBIMP (ityseq1, E.ITYSEQDEP (ityseq2, dep))) =
    solveSub unifenv (D.add (deps, dep)) (E.SUBIMP (ityseq1, ityseq2))
  | solveSub unifenv deps (sub as E.SUBIMP (ityseq1 as E.ITYSEQVAR sv1, ityseq2 as E.ITYSEQVAR sv2)) =
    if sv1 = sv2
    then unifenv
    else (case (getUniEnvSv sv1 unifenv, getUniEnvSv sv2 unifenv) of
	      (NONE, NONE) =>
	      let val ityseq2 = E.applyDepsItyseq (E.ITYSEQVAR sv2) deps
		  val ityseq1 = E.applyDepsItyseq (E.ITYSEQVAR sv1) deps
		  val (unifenv1, env1) = updateUniEnvSubLts unifenv sv1 ityseq2
		  val (unifenv2, env2) = updateUniEnvSubGts unifenv sv2 ityseq1
	      in solveEnv unifenv2 deps (E.ENVAPP (env1, env2)) D.dummy_dep
	      end
	    | (SOME ityseq, NONE) => solveSub unifenv deps (E.SUBIMP (ityseq, ityseq2))
	    | (NONE, SOME ityseq) => solveSub unifenv deps (E.SUBIMP (ityseq1, ityseq))
	    | (SOME ityseq1, SOME ityseq2) => solveSub unifenv deps (E.SUBIMP (ityseq1, ityseq2)))
  | solveSub unifenv deps (sub as E.SUBIMP (ityseq1 as E.ITYSEQVAR sv1, ityseq2 as E.ITYSEQSEQ (seq2, lab2))) =
    (case getUniEnvSv sv1 unifenv of
	 NONE => let val ty1  = E.mk_new_tyvar ()
		     val ty2  = E.mk_new_tyvar ()
		     val seq1 = E.ITYSEQSEQ ([ty1, ty2], lab2)
		 in case seq2 of
			[ty1', ty2'] =>
			let val sub1 = E.ENVSUB (E.SUBITY (ty1', ty1))
			    val sub2 = E.ENVSUB (E.SUBITY (ty2, ty2'))
			    val env  = E.ENVCST (E.CSSEQ (ityseq1, seq1))
			in solveEnv unifenv deps (E.list2env [sub1, sub2, env]) D.dummy_dep
			end

		      | _ => raise  Fail "implication should have 2 parameters"
		 (* val (unifenv', env) = updateUniEnvSubLts unifenv sv1 ityseq2 *)
		 (* in solveEnv unifenv' deps env D.dummy_dep*)
		 end
       | SOME ityseq1 => solveSub unifenv deps (E.SUBIMP (ityseq1, ityseq2)))
  | solveSub unifenv deps (sub as E.SUBIMP (ityseq1 as E.ITYSEQSEQ (seq1, lab1), ityseq2 as E.ITYSEQVAR sv2)) =
    (case getUniEnvSv sv2 unifenv of
	 NONE => let val ty1  = E.mk_new_tyvar ()
		     val ty2  = E.mk_new_tyvar ()
		     val seq2 = E.ITYSEQSEQ ([ty1, ty2], lab1)
		 in case seq1 of
			[ty1', ty2'] =>
			let val sub1 = E.ENVSUB (E.SUBITY (ty1, ty1'))
			    val sub2 = E.ENVSUB (E.SUBITY (ty2', ty2))
			    val env  = E.ENVCST (E.CSSEQ (ityseq2, seq2))
			(*val (unifenv', env) = updateUniEnvSubGts unifenv sv2 ityseq1*)
			(*in solveEnv unifenv' deps env D.dummy_dep*)
			in solveEnv unifenv deps (E.list2env [sub1, sub2, env]) D.dummy_dep
			end
		      | _ => raise Fail "implication should have 2 parameters"
		 end
       | SOME ityseq2 => solveSub unifenv deps (E.SUBIMP (ityseq1, ityseq2)))
  | solveSub unifenv deps (sub as E.SUBIMP (E.ITYSEQSEQ (itys1, lab1), E.ITYSEQSEQ (itys2, lab2))) =
    (case (itys1, itys2) of
	 ([ity1, ity2], [ity3, ity4]) =>
	 let val sub1 = E.SUBITY (ity2, ity4)
	     val sub2 = E.SUBITY (ity3, ity1)
	     val env  = E.ENVAPP (E.ENVSUB sub1, E.ENVSUB sub2)
	 in solveEnv unifenv deps env D.dummy_dep
	 end
       | _ => raise Fail "the arrow type should have 2 arguments")

(* MARK/TODO: In here when dealing with implicit type variable, we've got to
 * take care of the equality business. *)

(* Equality constraint solver *)
(* NOTE: The dep ones have to be before the cons one otherwise we'll find false
 * circularity with var(x) = dep(var(x),d). *)
and solveCst unifenv deps (E.CSITY (E.ITYDEP (ity1, dep), ity2)) = solveCst unifenv (D.add (deps, dep)) (E.CSITY (ity1, ity2))
  | solveCst unifenv deps (E.CSITY (ity1, E.ITYDEP (ity2, dep))) = solveCst unifenv (D.add (deps, dep)) (E.CSITY (ity1, ity2))
  | solveCst unifenv deps (E.CSSEQ (E.ITYSEQDEP (ityseq1, dep), ityseq2)) = solveCst unifenv (D.add (deps, dep)) (E.CSSEQ (ityseq1, ityseq2))
  | solveCst unifenv deps (E.CSSEQ (ityseq1, E.ITYSEQDEP (ityseq2, dep))) = solveCst unifenv (D.add (deps, dep)) (E.CSSEQ (ityseq1, ityseq2))
  | solveCst unifenv deps (E.CSNAM (E.ITYCONDEP (itycon1, dep), itycon2)) = solveCst unifenv (D.add (deps, dep)) (E.CSNAM (itycon1, itycon2))
  | solveCst unifenv deps (E.CSNAM (itycon1, E.ITYCONDEP (itycon2, dep))) = solveCst unifenv (D.add (deps, dep)) (E.CSNAM (itycon1, itycon2))
  | solveCst unifenv deps (E.CSENV (ev, E.ENVDEP (env, dep))) = solveCst unifenv (D.add (deps, dep)) (E.CSENV (ev, env))
  | solveCst unifenv deps (E.CSITY (ity1 as E.ITYVAR (tv1, eq1), ity2 as E.ITYVAR (tv2, eq2))) =
    if tv1 = tv2
    then unifenv
    else let val eq = E.mergeEqTv (eq1, eq2) (* NOTE: I'm not sure we actually need to do that... *)
	 (*val _ = print (E.toStringIty ity1 ^ " " ^ E.toStringIty ity2 ^ " " ^ E.toStringIty (E.ITYVAR (tv2, eq)) ^ "\n")*)
	 in case (getUniEnvTv tv1 unifenv, getUniEnvTv tv2 unifenv) of
		(NONE, NONE) => solveOccIty unifenv deps tv1 eq (E.ITYVAR (tv2, eq))
	      (* TODO: we add the equality above but if tv1 is eq and tv2 is neq then
	       * we're not guarented that if tv2 is in unifenv then it is a neq tycon.
	       * We've got to contaminate unifenv with the equality constraint.
	       * NOTE: This is done by the use of isEqIty in solveOccIty. *)
	      | (NONE, SOME ity) => solveCst unifenv deps (E.CSITY (E.ITYVAR (tv1, eq), ity))
	      | (SOME ity, NONE) => solveCst unifenv deps (E.CSITY (E.ITYVAR (tv2, eq), ity))
	      | (SOME ity1, SOME ity2) => solveCst unifenv deps (E.CSITY (ity1, ity2))
	 end
  | solveCst unifenv deps (E.CSNAM (itycon1 as E.ITYCONVAR tcv1, itycon2 as E.ITYCONVAR tcv2)) =
    if tcv1 = tcv2
    then unifenv
    else (case (getUniEnvTc tcv1 unifenv, getUniEnvTc tcv2 unifenv) of
	      (NONE, NONE) => solveOccItycon unifenv deps tcv1 itycon2
	    | (NONE, SOME itycon) => solveCst unifenv deps (E.CSNAM (itycon1, itycon))
	    | (SOME itycon, NONE) => solveCst unifenv deps (E.CSNAM (itycon2, itycon))
	    | (SOME itycon1, SOME itycon2) => solveCst unifenv deps (E.CSNAM (itycon1, itycon2)))
  | solveCst unifenv deps (E.CSSEQ (ityseq1 as E.ITYSEQVAR sv1, ityseq2 as E.ITYSEQVAR sv2)) =
    if sv1 = sv2
    then unifenv
    else (case (getUniEnvSv sv1 unifenv, getUniEnvSv sv2 unifenv) of
	      (NONE, NONE) => solveOccItyseq unifenv deps sv1 ityseq2
	    | (NONE, SOME ityseq) => solveCst unifenv deps (E.CSSEQ (ityseq1, ityseq))
	    | (SOME ityseq, NONE) => solveCst unifenv deps (E.CSSEQ (ityseq2, ityseq))
	    | (SOME ityseq1, SOME ityseq2) => solveCst unifenv deps (E.CSSEQ (ityseq1, ityseq2)))
  | solveCst unifenv deps (E.CSENV (ev1, E.ENVVAR ev2)) =
    if ev1 = ev2
    then unifenv
    else (case getUniEnvEv ev1 unifenv of
	      NONE => solveOccEnv unifenv deps ev1 (E.ENVVAR ev2)
	    | SOME env => solveCst unifenv deps (E.CSENV (ev2, env)))
  (**)
  | solveCst unifenv deps (E.CSNAM (E.ITYCONNAM (tcn1, eq1, lab1), E.ITYCONNAM (tcn2, eq2, lab2))) =
    if tcn1 = tcn2
       orelse (tcn1 = E.tyconnameUnit andalso tcn2 = E.tyconnameTuple)
       orelse (tcn2 = E.tyconnameUnit andalso tcn1 = E.tyconnameTuple)
       orelse (tcn1 = E.tyconnameType andalso tcn2 = E.tyconnameProp)
       orelse (tcn2 = E.tyconnameType andalso tcn1 = E.tyconnameProp)
       orelse (tcn1 = E.tyconnameAtom andalso tcn2 = E.tyconnameToken)
       orelse (tcn2 = E.tyconnameAtom andalso tcn1 = E.tyconnameToken)
    then unifenv
    else raise errorex (mk_pre_error (TYCONSCLASH ((tcn1, lab1, NONE), (tcn2, lab2, NONE))) deps)
  | solveCst unifenv deps (E.CSITY (E.ITYVAR (tv, eq), ity)) =
    (case getUniEnvTv tv unifenv of
	 NONE => solveOccIty unifenv deps tv eq ity
       | SOME ity' => solveCst unifenv deps (E.CSITY (ity', ity)))
  | solveCst unifenv deps (E.CSNAM (E.ITYCONVAR tcv, itycon)) =
    (case getUniEnvTc tcv unifenv of
	 NONE => solveOccItycon unifenv deps tcv itycon
       | SOME itycon' => solveCst unifenv deps (E.CSNAM (itycon', itycon)))
  (*| solveCst unifenv deps (E.CSNAM  (E.ITYCONNAM (tyconname, lab), E.ITYCONFUN (ityseq, ity))) =
   raise Fail ""*)
  (*| solveCst unifenv deps (E.CSNAM  (E.ITYCONFUN (ityseq, ity), E.ITYCONNAM (tyconname, lab))) =
   raise Fail ""*)
  (*| solveCst unifenv deps (E.CSNAM (E.ITYCONFUN (ityseq1, ity1), E.ITYCONFUN (ityseq2, ity2))) =
   raise Fail ""*)
  | solveCst unifenv deps (E.CSITY (ity1 as E.ITYCON (ityseq1, itycon1), ity2 as E.ITYCON (ityseq2, itycon2))) =
    let (*val itycon1' = buildItycon unifenv itycon1*)
	(*val itycon2' = buildItycon unifenv itycon2*)
	val (itycon1'', deps1) = E.extractDepsItycon itycon1(*'*)
	val (itycon2'', deps2) = E.extractDepsItycon itycon2(*'*)
    in case (itycon1'', itycon2'') of
	   (E.ITYCONVAR _, _) => unifenv
	 | (_, E.ITYCONVAR _) => unifenv
	 | (E.ITYCONDEP _, _) => raise Fail "all the dependencies should have been removed"
	 | (_, E.ITYCONDEP _) => raise Fail "all the dependencies should have been removed"
	 | (ityconname1 as E.ITYCONNAM (name1, eq1, l1), ityconname2 as E.ITYCONNAM (name2, eq2, l2)) =>
	   let val deps' = D.union (D.union (deps1, deps2), deps)
	       val env   =
		   (*if name1 = E.tyconnameDeq andalso name2 = E.tyconnameArrow
		   then let val tv   = E.nextItyvar ()
			    val ityv = E.mk_tyvar tv
			    val css1 = E.CSSEQ (ityseq1, E.ITYSEQSEQ ([ityv], l1))
			    val arr  = E.mk_type_arrow (ityv, E.mk_type_bool l1) l1
			    val css2 = E.CSSEQ (ityseq2, E.ITYSEQSEQ ([ityv, arr], l1))
			in E.ENVAPP (E.ENVCST css1, E.ENVCST css2)
			end
		   else if name1 = E.tyconnameArrow andalso name2 = E.tyconnameDeq
		   then let val tv   = E.nextItyvar ()
			    val ityv = E.mk_tyvar tv
			    val css1 = E.CSSEQ (ityseq2, E.ITYSEQSEQ ([ityv], l2))
			    val arr  = E.mk_type_arrow (ityv, E.mk_type_bool l2) l2
			    val css2 = E.CSSEQ (ityseq1, E.ITYSEQSEQ ([ityv, arr], l2))
			in E.ENVAPP (E.ENVCST css1, E.ENVCST css2)
			end
		   else*) let val env1  = E.ENVCST (E.CSNAM (itycon1, itycon2))
			    val env2  = E.ENVCST (E.CSSEQ (ityseq1, ityseq2))
			in E.ENVAPP (env1, env2)
			end
	   in solveEnv unifenv deps' env D.dummy_dep
	   end
    end
  | solveCst unifenv deps (E.CSITY (ity1 as E.ITYCON (ityseq, itycon), ity2 as E.ITYTYP (ity, lab))) =
    solveCst unifenv deps (E.CSITY (ity1, E.mk_type_type lab))
  | solveCst unifenv deps (E.CSITY (ity1 as E.ITYTYP (ity, lab), ity2 as E.ITYCON (ityseq, itycon))) =
    solveCst unifenv deps (E.CSITY (E.mk_type_type lab, ity2))
  | solveCst unifenv deps (E.CSITY (ty1 as E.ITYTYP (ity1, lab1), ty2 as E.ITYTYP (ity2, lab2))) =
    solveCst unifenv deps (E.CSITY (ity1, ity2))
  (*| solveCst unifenv deps (E.CSITY (ty1 as E.ITYCON (ityseq, itycon), ty2 as E.ITYDPP (ity1, ity2))) =
   (* TODO: what if itycon is a type function? *)
   solveCst unifenv deps (E.CSITY (ty1, ity2))*)
  (*| solveCst unifenv deps (E.CSITY (ty1 as E.ITYDPP (ity1, ity2), ty2 as E.ITYCON (ityseq, itycon))) =
   (* TODO: what if itycon is a type function? *)
   solveCst unifenv deps (E.CSITY (ity2, ty2))*)
  (*| solveCst unifenv deps (E.CSITY (ty1 as E.ITYDPP (ity1, ity2), ty2 as E.ITYDPP (ity3, ity4))) =
   let val env1 = E.ENVCST (E.CSITY (ity1, ity3))
       val env2 = E.ENVCST (E.CSITY (ity2, ity4))
   in solveEnv unifenv deps (E.ENVAPP (env1, env2))
   end*)

  | solveCst unifenv deps (E.CSITY (ty1 as E.ITYOR (orseq as E.ORSEQ (ityseq1, id, lab)),
				    ty2 as E.ITYCON (ityseq2, itycon))) =
    let fun match restriction depsor =
	    let val (itycon0, deps0) = E.extractDepsItycon itycon
		val deps1 = D.union (deps, deps0)
		fun matchTn (E.ITYCONVAR _) = (unifenv, deps, E.ENVNUL)
		  | matchTn (E.ITYCONNAM (tyconname, eq, lab)) =
		    (case matchOr unifenv tyconname ityseq1 restriction of
			 OR_NOT_FOUND (tns, deps') =>
			 let val kind  = OVERLOAD ((tyconname, lab, NONE), tns)
			     val deps2 = D.union (deps', deps1)
			     val deps3 = D.union (deps2, depsor)
			 in raise errorex (mk_pre_error kind deps3)
			 end
		       | OR_FOUND_VAR => (unifenv, deps, E.ENVNUL)
		       | OR_FOUND_TYCON (n, deps', ty3) =>
			 let val deps2    = D.union (deps1, deps')
			     (* Below, paths does not have any ids because even if our
			      * overloaded type was constrain because of some other
			      * overloaded types, itycon is the one that narrows down
			      * the possibilities to only one type. *)
			     val paths    = ([n], deps2)
			     val unifenv' = updateUniEnvUniOr unifenv id paths
			     val env      = E.ENVCST (E.CSITY (ty3, ty2))
			 in (unifenv', deps2, env)
			 end)
		  | matchTn (E.ITYCONDEP _) = raise Fail "we striped the internal type constructor"
	    in matchTn itycon0
	    end
    in case getUniEnvOr id unifenv of
	   NONE =>
	   let val (unifenv', deps', env) = match NONE D.empty
	   in solveEnv unifenv' deps' env D.dummy_dep
	   end
	 | SOME ([n], depsor) =>
	   (* NOTE: We have to go into ityseq1 to the nth element in the list. *)
	   (* NOTE: We could pass the singleton to match as a restriction, but it's faster to do that. *)
	   (case getToNinSeq ityseq1 n of
		SOME ity => solveCst unifenv (D.union (deps, depsor)) (E.CSITY (ity, ty2))
	      | NONE => unifenv)
	 | SOME (list, depsor) =>
	   (* We don't need the third parameter anymore because thanks to the itycon
	    * we are going to be able to make a decision as at which type we should
	    * choose in the overloaded type (if indeed we can choose one). *)
	   let val (unifenv1, deps1, env) = match (SOME list) depsor
	       val unifenv2 = solveEnv unifenv1 deps1 env D.dummy_dep
	       val deps'    = D.union (deps, depsor)
	       val (unifenv3, list) = rmUniEnvOve unifenv2 id
	   in foldr (fn ((orseq1, orseq2, deps0), unifenv) =>
			let val cst = E.CSITY (E.ITYOR orseq1, E.ITYOR orseq2)
			in solveCst unifenv (D.union (deps', deps0)) cst
			end)
		    unifenv3
		    list
	   end
    end
  | solveCst unifenv deps (E.CSITY (ty1 as E.ITYOR (orseq as E.ORSEQ (ityseq1, id1, lab1)),
				    ty2 as E.ITYTYP (ity2, lab2))) = raise Fail "solveCst:OR/TYP"

  | solveCst unifenv deps (E.CSITY (ty1 as E.ITYOR (orseq1 as E.ORSEQ (ityseq1, id1, lab1)),
				    ty2 as E.ITYOR (orseq2 as E.ORSEQ (ityseq2, id2, lab2)))) =
    let fun isin c NONE = true
	  | isin (c : int) (SOME list) = List.exists (fn n => n = c) list
	fun restrict restrict1 depsRes1 restrict2 depsRes2 =
	    case (orFull ityseq1, orFull ityseq2) of
		(SOME tcdeps1, SOME tcdeps2) =>
		let val tcdeps1' = FULL.filter (fn (n, _, _, _) => isin n restrict1) tcdeps1
		    val tcdeps2' = FULL.filter (fn (n, _, _, _) => isin n restrict2) tcdeps2
		    val tcdeps   = FULL.intersectWith (fn x => x) (tcdeps1', tcdeps2')
		    val ((list1, list1', list1t, deps1), (list2, list2', list2t, deps2)) =
			FULL.foldri (fn (tyconname,
					 ((n1, lab1, ty1, deps1), (n2, lab2, ty2, deps2')),
					 ((list1, list1', list1t, deps1'), (list2, list2', list2t, deps2))) =>
					(((tyconname, lab1, NONE) :: list1,
					  n1 :: list1',
					  ty1 :: list1t,
					  D.union (deps1, deps1')),
					 ((tyconname, lab2, NONE) :: list2,
					  n2 :: list2',
					  ty2 :: list2t,
					  D.union (deps2, deps2'))))
				    (([], [], [], depsRes1), ([], [], [], depsRes2))
				    tcdeps
		    val num = FULL.numItems tcdeps
		    val deps' = D.union (deps, D.union (deps1, deps2))
		(*val _ = simpleOverloading ityseq1 ityseq2 (ListPair.zip (list1', list2'))*)
		in (* TODO: raising an error here is not the correct thing to do
		    * because we can get such ITYOR equals because some type
		    * constraints have been sliced away.
		    * For example, given :
		    *
		    *     cons map (x : 'a -> 'b; y : 'c) : 'd with ('c, 'd) in {('a List, 'b List), ('a Bag, 'b Bag)} ;;
		    *     let lifting2 f (abag : 'a Bag) bbag =
		    *           concat (map (\f. map f bbag) (map f abag)) ;;
		    *
		    * If we slice away 'a Bag in lifting2 (This can happen by adding
		    * the untypable piece of code: let x1 = map 1 ;;) then we don't
		    * have the constraint anymore on abag and we end up checking that
		    * two ITYOR must be equals.  What can we do then?
		    *)
		    (*let val deps' = D.union (deps, D.union (deps1, deps2))
			  val kind   = NOTENOUGHINFO
		      in raise errorex (mk_pre_error kind deps')
		      end*)
		    if num = 0
		    then let val kind = OVERLOADS (list1, list2)
			 in raise errorex (mk_pre_error kind deps')
			 end
		    else if num = 1
		    then let val cst      = E.CSITY ((fn [pair] => pair | _ => raise Fail "solveCst:ORSEQ") (ListPair.zip (list1t, list2t)))
			     val paths1   = (list1', deps')
			     val paths2   = (list2', deps')
			     val unifenv1 = updateUniEnvUniOr unifenv  id1 paths1
			     val unifenv2 = updateUniEnvUniOr unifenv1 id2 paths2
			     val unifenv3 = solveCst unifenv2 deps' cst
			     val (unifenv4, list1) = rmUniEnvOve unifenv3 id1
			     val (unifenv5, list2) = rmUniEnvOve unifenv4 id2
			 in foldr (fn ((orseq1, orseq2, deps0), unifenv) =>
				      let val cst = E.CSITY (E.ITYOR orseq1, E.ITYOR orseq2)
				      in solveCst unifenv (D.union (deps', deps0)) cst
				      end)
				  unifenv5
				  (list1 @ list2)
			 end
		    else let val paths1   = (list1', deps') (*D.union (deps, deps2)*) (*, (ref orseq2) :: ids1*)
			     val paths2   = (list2', deps') (*D.union (deps, deps1)*) (*, (ref orseq1) :: ids2*)
			     val unifenv1 = updateUniEnvUniOr unifenv  id1 paths1
			     val unifenv2 = updateUniEnvUniOr unifenv1 id2 paths2
			     val unifenv3 = updateUniEnvOve unifenv2 (orseq1, orseq2, deps')
			 in unifenv3
			 end
		end
	      | _ => unifenv
    (*let fun genPath restrict (E.ITYSEQVAR _) = NONE
	    | genPath restrict (E.ITYSEQSEQ (itys, _)) =
	      let val list  = List.tabulate (List.length itys, fn x => x)
		  val list' = List.filter (fn n => isin n restrict) list
	      in SOME (list', deps)
	      end
	    | genPath restrict (E.ITYSEQDEP (ityseq, dep)) =
	      (case genPath restrict ityseq of
		   NONE => NONE
		 | SOME (list, deps) => SOME (list, D.add (deps, dep)))
      in case (genPath restrict1 ityseq1, genPath restrict2 ityseq2) of
	     (SOME (list1, deps1), SOME (list2, deps2)) =>
	     let val paths1   = (list1, D.union (deps, deps2), (ref orseq2) :: ids1)
		 val paths2   = (list2, D.union (deps, deps1), (ref orseq1) :: ids2)
		 val unifenv1 = updateUniEnvUniOr unifenv  id1 paths1
		 val unifenv2 = updateUniEnvUniOr unifenv1 id2 paths2
	     in unifenv2
	     end
	   | _ => unifenv
      end*)
    in case (getUniEnvOr id1 unifenv, getUniEnvOr id2 unifenv) of
	   (SOME ([n], deps'), _) =>
	   (* Here and below we don't care about the ids that forced the overloaded
	    * type to a single type because we now have a single type and will be
	    * able to consequently force the second overloaded type (ty2) to a
	    * single type (if possible) or get an error. *)
	   (case getToNinSeq ityseq1 n of
		SOME ity =>
		let val deps0 = D.union (deps, deps')
		    val cs    = E.CSITY (ity, ty2)
		in solveCst unifenv deps0 cs
		end
	      | NONE => unifenv)
	 | (_, SOME ([n], deps')) =>
	   (case getToNinSeq ityseq2 n of
		SOME ity =>
		let val deps0 = D.union (deps, deps')
		    val cs    = E.CSITY (ity, ty1)
		in solveCst unifenv deps0 cs
		end
	      | NONE => unifenv)
	 | (SOME (list1, deps1), SOME (list2, deps2)) => restrict (SOME list1) deps1 (SOME list2) deps2
	 | (SOME (list1, deps1), NONE) => restrict (SOME list1) deps1 NONE D.empty
	 | (NONE, SOME (list2, deps2)) => restrict NONE D.empty (SOME list2) deps2
	 | (NONE, NONE) => restrict NONE D.empty NONE D.empty
    end

  | solveCst unifenv deps (E.CSITY (E.ITYETV (etyvar, lab), E.ITYCON (ityseq, itycon))) =
    let (*val itycon1 = buildItycon unifenv itycon*)
	val (itycon2, deps1) = E.extractDepsItycon itycon(*1*)
    in case itycon2 of
	   E.ITYCONVAR _ => unifenv
	 | E.ITYCONNAM (tyconname, eq, lab2) =>
	   let val kind  = ETYVARCLASH ((etyvar, lab, NONE), (tyconname, lab2, NONE))
	       val deps2 = D.union (deps, deps1)
	   in raise errorex (mk_pre_error kind deps2)
	   end
	 (*| E.ITYCONFUN _ => raise Fail "type functions should have already been resolved"*)
	 | E.ITYCONDEP _ => raise Fail "all the dependencies should have been removed"
    end
  | solveCst unifenv deps (E.CSITY (E.ITYETV (etyvar, lab1), E.ITYTYP (ity, lab2))) = raise Fail "solveCst:ETV/TYP"
  | solveCst unifenv deps (E.CSITY (E.ITYETV (etyvar1, lab1), E.ITYETV (etyvar2, lab2))) =
    if etyvar1 = etyvar2
    then unifenv
    else let val kind = CTYVARCLASH ((etyvar1, lab1, NONE), (etyvar2, lab2, NONE))
	 in raise errorex (mk_pre_error kind deps)
	 end

  | solveCst unifenv deps (E.CSITY (E.ITYETV (etyvar, lab), E.ITYOR _)) =
    raise Fail "solveCst:CSITY:ITYETV:ITYOR"

  | solveCst unifenv deps (E.CSSEQ (E.ITYSEQVAR sv, ityseq)) =
    (case getUniEnvSv sv unifenv of
	 NONE => solveOccItyseq unifenv deps sv ityseq
       | SOME ityseq' => solveCst unifenv deps (E.CSSEQ (ityseq', ityseq)))
  | solveCst unifenv deps (cst as E.CSSEQ (E.ITYSEQSEQ (itys1, lab1), E.ITYSEQSEQ (itys2, lab2))) =
    let val n1 = List.length itys1
	val n2 = List.length itys2
    in if n1 = n2
       then let val envs = E.list2env (map (fn pair => E.ENVCST (E.CSITY pair)) (ListPair.zip (itys1, itys2)))
	    in solveEnv unifenv deps envs D.dummy_dep
	    end
       else raise errorex (mk_pre_error (ARITYCLASH ((n1, lab1, NONE), (n2, lab2, NONE))) deps)
    end
  (*| solveCst unifenv deps (E.CSARI (E.ITYCONVAR tc, ityseq)) =
   (case getUniEnvTc tc unifenv of
	NONE => unifenv
      | SOME itycon' => solveCst unifenv deps (E.CSARI (itycon', ityseq)))
  | solveCst unifenv deps (E.CSARI (itycon, E.ITYSEQVAR sv)) =
    (case getUniEnvSv sv unifenv of
	 NONE => unifenv
       | SOME ityseq' => solveCst unifenv deps (E.CSARI (itycon, ityseq')))
  | solveCst unifenv deps (E.CSARI (E.ITYCONDEP (itycon, dep), ityseq)) =
    solveCst unifenv (D.add (deps, dep)) (E.CSARI (itycon, ityseq))
  | solveCst unifenv deps (E.CSARI (itycon, E.ITYSEQDEP (ityseq, dep))) =
    solveCst unifenv (D.add (deps, dep)) (E.CSARI (itycon, ityseq))*)
  (*| solveCst unifenv deps (E.CSARI (E.ITYCONNAM (tyconname, lab1), E.ITYSEQSEQ (itys, lab2))) =
   let val n2 = List.length itys
   in if n1 = n2
      then unifenv
      else raise errorex (mk_pre_error (ARITYCLASH ((n1, lab1), (n2, lab2))) (D.add (deps, D.mk_dep lab2))) (* TODO: why do I need to add lab2 here? *)
   end*)
  (*| solveCst unifenv deps (E.CSARI (E.ITYCONFUN (ityseq1, ity1), ityseq2)) =
   let val ityseq1' = preBuildItyseq unifenv ityseq1
   (* We can do that because we suppose that constraints on ityseq1 should have
    * been dealt with already. *)
   in solveCst unifenv deps (E.CSSEQ (ityseq1', ityseq2))
   end*)
  (*| solveCst unifenv deps (E.CSITY (E.ITYCON (_, itycon), ITYIMP _)) =
   solveCst unifenv deps (E.CSNAM (itycon, E.ITYCONNAM (tyconnameArrow, label)))
  (*raise errorex (TYCONSCLASH (itycon, E.ITYCONNAM tyconnameArrow), deps)*)
  | solveCst unifenv deps (E.CSITY (ITYIMP _, E.ITYCON (_, itycon))) =
    solveCst unifenv deps (E.CSNAM (E.ITYCONNAM (tyconnameArrow, label), itycon))
  (*raise errorex (TYCONSCLASH (itycon, E.ITYCONNAM tyconnameArrow), deps)*)
  | solveCst unifenv deps (E.CSITY (ITYIMP (ity1, ity2), ITYIMP (ity3, ity4))) =
    solveEnv unifenv deps (E.ENVAPP (E.ENVCST (E.CSITY (ity1, ity3)), E.ENVCST (E.CSITY (ity2, ity4))))*)
  | solveCst unifenv deps (E.CSENV (ev, env)) =
    if true
    then let val marker   = E.nextMarker ()
	     val emarker  = E.ENVMRK marker
	     val unifenv0 = updateUniEnvEnv unifenv emarker
	     val unifenv' = solveEnv unifenv0 deps env D.dummy_dep
	     val env'     = E.applyDepsEnv (solvedUniEnv unifenv' marker) deps
	     val unifenv1 = replaceUniEnvEnv unifenv' (getEnv unifenv)
	 in updateUniEnvUniEv unifenv1 ev env'
	 end
    else updateUniEnvUniEv unifenv ev (E.applyDepsEnv env deps)
  (* reversing *)
  | solveCst unifenv deps (E.CSITY (x, E.ITYVAR    y)) = solveCst unifenv deps (E.CSITY (E.ITYVAR    y, x))
  | solveCst unifenv deps (E.CSSEQ (x, E.ITYSEQVAR y)) = solveCst unifenv deps (E.CSSEQ (E.ITYSEQVAR y, x))
  | solveCst unifenv deps (E.CSNAM (x, E.ITYCONVAR y)) = solveCst unifenv deps (E.CSNAM (E.ITYCONVAR y, x))
  | solveCst unifenv deps (E.CSITY (x, E.ITYETV    y)) = solveCst unifenv deps (E.CSITY (E.ITYETV    y, x))
  | solveCst unifenv deps (E.CSITY (x, E.ITYOR     y)) = solveCst unifenv deps (E.CSITY (E.ITYOR     y, x))

and toSubUniEnvSubTv unifenv ityvar ity =
    if getSwitchSub ()
    then let val (unifenv', env) = toSubUniEnvSubGen unifenv (#ltv, #gtv) E.SUBITY (rmUniEnvSubLtv, rmUniEnvSubGtv) ityvar ity
	 in solveEnv unifenv' D.empty env D.dummy_dep
	 end
    else unifenv

and toSubUniEnvSubSq unifenv ityseqvar ityseq =
    if getSwitchSub ()
    then let val (unifenv', env) = toSubUniEnvSubGen unifenv (#lts, #gts) E.SUBSEQ (rmUniEnvSubLts, rmUniEnvSubGts) ityseqvar ityseq
	 in solveEnv unifenv' D.empty env D.dummy_dep
	 end
    else unifenv

and toSubUniEnvSubTc unifenv ityconvar itycon =
    if getSwitchSub ()
    then let val (unifenv', env) = toSubUniEnvSubGen unifenv (#ltc, #gtc) E.SUBNAM (rmUniEnvSubLtc, rmUniEnvSubGtc) ityconvar itycon
	 in solveEnv unifenv' D.empty env D.dummy_dep
	 end
    else unifenv

and solveOccIty unifenv deps ityvar eq ity =
    let val ity'    = E.applyDepsIty (buildIty unifenv ity) deps
	val ityvars = E.getItyvarsIty ity'
    in if E.isinITVS (ityvars, ityvar)
       then raise errorex (mk_pre_error CIRCULARITY (E.getDepsIty ity'))
       else case eq of
		E.NEQ =>
		let val unifenv1 = updateUniEnvUniTv unifenv ityvar ity' (* we have ity here in the paper *)
		in toSubUniEnvSubTv unifenv1 ityvar ity'
		end
	      | E.EQ lab1 =>
		case isEqIty ity' lab1 of
		    EQOK ity'' =>
		    let val unifenv1 = updateUniEnvUniTv unifenv ityvar ity''
		    in toSubUniEnvSubTv unifenv1 ityvar ity''
		    end
		  | EQERR (lab2, deps2) =>
		    let val kind = EQTYPE (lab1, lab2)
		    in raise errorex (mk_pre_error kind (D.union (deps, deps2)))
		    end
    end

and solveOccItyseq unifenv deps ityseqvar ityseq =
    let val ityseq' = E.applyDepsItyseq (buildItyseq unifenv ityseq) deps
	val ityvars = E.getItyvarseqsItyseq ityseq'
    in if E.isinITVS (ityvars, ityseqvar)
       then raise errorex (mk_pre_error CIRCULARITY (E.getDepsItyseq ityseq'))
       else let val unifenv' = updateUniEnvUniSv unifenv ityseqvar ityseq'
	    in toSubUniEnvSubSq unifenv' ityseqvar ityseq'
	    end
    end

and solveOccItycon unifenv deps ityconvar itycon =
    let val itycon'  = E.applyDepsItycon itycon deps (* we have itycon here in the paper *)
	val unifenv' = updateUniEnvUniTc unifenv ityconvar itycon'
    in toSubUniEnvSubTc unifenv' ityconvar itycon'
    end

and solveOccEnv unifenv deps envvar env =
    updateUniEnvUniEv unifenv envvar (E.applyDepsEnv env deps)

fun solver env sub =
    let val osub    = getSwitchSub ()
	val _       = setSwitchSub sub
	(*val _       = print ("[++solver]\n")*)
	val _       = resetEnvs ()
	val unifenv = solveEnv (initUnifEnv ()) D.empty env D.dummy_dep
	val envs    = getEnvs ()
	val _       = resetEnvs ()
	(*val _       = print ("[--solver]\n")*)
	val _       = setSwitchSub osub
	val _       = unsetFirst ()
    in SUCCESS (unifenv, envs)
    end handle errorex error =>
	       let val error' = (*stripErrorDumDep*) error
	       in (unsetFirst (); ERROR error')
	       end


(* ------ FILTERING ------ *)

fun isDumBind (E.BINDVID (_, _, E.VMONO (ityvar, _, _))) = (ityvar = E.ityvarFresh)
  | isDumBind (E.BINDVID _) = false
  | isDumBind (E.BINDTYC (_, _, (bound, (E.ITYSEQVAR ityseqvar, E.ITYVAR (ityvar, _))))) =
    (ityseqvar = E.ityseqvarFresh) andalso (ityvar = E.ityvarFresh)
  | isDumBind (E.BINDTYC _) = false
  | isDumBind (E.BINDTYV (_, _, (E.ITYVAR (ityvar, _), _))) = (ityvar = E.ityvarFresh)
  | isDumBind (E.BINDTYV _) = false
  | isDumBind (E.BINDATM (_, _, E.VMONO (ityvar, _, _))) = (ityvar = E.ityvarFresh)
  | isDumBind (E.BINDATM _) = false
  | isDumBind (E.BINDVAR (_, _, E.ITYVAR (ityvar, _))) = (ityvar = E.ityvarFresh)
  | isDumBind (E.BINDVAR _) = false

(* checks if all the binders are dummy binders *)
fun isDumEnv (E.ENVVAR envvar)       = (envvar = E.envvarFresh)
  | isDumEnv (E.ENVBIN (bind, _))    = isDumBind bind
  | isDumEnv (E.ENVACC _)            = false
  | isDumEnv (E.ENVCST _)            = false
  | isDumEnv (E.ENVSUB _)            = false
  | isDumEnv (E.ENVPOL (env, _))     = isDumEnv env
  | isDumEnv (E.ENVAPP (env1, env2)) = isDumEnv env1 andalso isDumEnv env2
  | isDumEnv (E.ENVSET envs)         = List.all isDumEnv envs
  | isDumEnv (E.ENVLOC (env1, env2)) = isDumEnv env2
  | isDumEnv (E.ENVDEP (env, _))     = isDumEnv env
  | isDumEnv (E.ENVFIL _)            = false
  | isDumEnv (E.ENVERR _)            = false
  | isDumEnv (E.ENVOVL _)            = false
  | isDumEnv (E.ENVTOF _)            = false
  | isDumEnv (E.ENVMRK _)            = false
  | isDumEnv (E.ENVLAB _)            = false
  | isDumEnv (E.ENVOVE _)            = false
  | isDumEnv E.ENVNUL                = false

fun dumBind (E.BINDVID (vid,   _, _)) = E.BINDVID (vid,   D.dummy_label, E.mk_new_vscheme E.ityvarFresh)
  | dumBind (E.BINDTYC (tycon, _, _)) = E.BINDTYC (tycon, D.dummy_label, E.mk_new_ityfun (E.ITYSEQVAR E.ityseqvarFresh) (E.mk_tyvar E.ityvarFresh))
  | dumBind (E.BINDTYV (tyvar, _, _)) = E.BINDTYV (tyvar, D.dummy_label, E.mk_new_tvbind E.ityvarFresh)
  | dumBind (E.BINDATM (atoms, _, _)) = E.BINDATM (atoms, D.dummy_label, E.mk_new_vscheme E.ityvarFresh)
  | dumBind (E.BINDVAR (var,   _, _)) = E.BINDVAR (var,   D.dummy_label, E.mk_tyvar E.ityvarFresh)

fun dum (E.ENVVAR envvar)    = E.ENVVAR E.envvarFresh
  | dum (E.ENVBIN (bind, k)) = E.ENVBIN (dumBind bind, k)
  | dum (E.ENVACC _)         = E.ENVNUL
  | dum (E.ENVCST _)         = E.ENVNUL
  | dum (E.ENVSUB _)         = E.ENVNUL
  | dum (E.ENVPOL _)         = raise Fail "pol environment should never be labelled"
  | dum (E.ENVAPP _)         = raise Fail "environment sequences should never be labelled"
  | dum (E.ENVSET _)         = raise Fail "environment sets should never be labelled"
  | dum (E.ENVLOC _)         = raise Fail "local environment should never be labelled"
  | dum (E.ENVDEP _)         = raise Fail "dependent environment should never be labelled"
  | dum (E.ENVFIL _)         = raise Fail "environment file should never be labelled"
  | dum (E.ENVERR _)         = E.ENVNUL
  | dum (E.ENVOVL _)         = E.ENVNUL
  | dum (E.ENVTOF _)         = E.ENVNUL
  | dum (E.ENVMRK _)         = raise Fail "marker environment should never be labelled"
  | dum (E.ENVLAB _)         = E.ENVNUL
  | dum (E.ENVOVE _)         = E.ENVNUL
  | dum E.ENVNUL             = raise Fail "empty environment should never be labelled"

fun collapseEnv (E.ENVDEP (env, dep)) =
    let val (env, deps) = collapseEnv env
    in (env, D.add (deps, dep))
    end
  | collapseEnv env = (env, D.empty)

fun filterEnv (E.ENVVAR _) deps1 deps2 = raise Fail "variable should always be labelled"
  | filterEnv (E.ENVBIN _) deps1 deps2 = raise Fail "binder should always be labelled"
  | filterEnv (E.ENVACC _) deps1 deps2 = raise Fail "accessor should always be labelled"
  | filterEnv (E.ENVCST (E.CSENV (ev, env))) deps1 deps2 = E.ENVCST (E.CSENV (ev, filterEnv env deps1 deps2))
  | filterEnv (E.ENVCST _) deps1 deps2 = raise Fail "equality constraint should always be labelled"
  | filterEnv (E.ENVSUB _) deps1 deps2 = raise Fail "subtyping constraint should always be labelled"
  | filterEnv (E.ENVPOL (env, mrk)) deps1 deps2 = E.ENVPOL (filterEnv env deps1 deps2, mrk)
  | filterEnv (E.ENVAPP (env1, env2)) deps1 deps2 =
    let val env1' = filterEnv env1 deps1 deps2
	val env2' = filterEnv env2 deps1 deps2
    in if E.isNullEnv env1'
       then env2'
       else if E.isNullEnv env2'
       then env1'
       else E.ENVAPP (env1', env2')
    end
  | filterEnv (E.ENVSET envs) deps1 deps2 =
    let val (envs1 (* dummy *), envs2 (* non-dummy *)) =
	    List.foldl (fn (env, (envs1, envs2)) =>
			   let val env' = filterEnv env deps1 deps2
			   in if isDumEnv env'
			      then (envs1 @ [env'], envs2)
			      else (envs1, envs2 @ [env'])
			   end)
		       ([], [])
		       envs
    in E.ENVSET (envs1 @ envs2)
    end
  | filterEnv (E.ENVLOC (env1, env2)) deps1 deps2 = E.ENVLOC (filterEnv env1 deps1 deps2, filterEnv env2 deps1 deps2)
  | filterEnv (E.ENVDEP (env, dep)) deps1 deps2 =
    let val (env', deps) = collapseEnv env
	val deps' = D.add (deps, dep)
    in if D.isSubseteq (deps', D.difference (deps1, deps2))
       then E.ENVDEP (env, dep)
       else if D.isSubseteq (deps', D.union (deps1, deps2))
       then dum env'
       else let val _ = () (*print ("remove " ^ E.toStringEnv env ^ "\n")*)
	    in E.ENVNUL
	    end
    end
  (*if D.member (D.difference (deps1, deps2), dep)
    then E.ENVDEP (env, dep)
    else if D.member (deps2, dep)
    then dum env
    else E.ENVNUL*)
  | filterEnv (E.ENVFIL (file, env1, env2, label)) deps1 deps2 =
    (* deps1 is the dependency set to keep
     * deps2 is the dependency set to try to remove *)
    if D.member (D.difference (deps1, deps2), D.mk_dep label)
    then E.ENVFIL (file, filterEnv env1 deps1 deps2, filterEnv env2 deps1 deps2, label)
    else if D.member (deps2, D.mk_dep label)
    then E.ENVVAR (E.nextEnvvar ())
    else E.ENVNUL
  | filterEnv (E.ENVERR _) _ _ = raise Fail "enverr should always be labelled"
  | filterEnv (E.ENVOVL _) _ _ = raise Fail "envovl should always be labelled"
  | filterEnv (E.ENVTOF _) _ _ = raise Fail "type requests should always be labelled"
  | filterEnv (E.ENVMRK _) _ _ = raise Fail "marker environment should not be generated at initial constraint generation"
  | filterEnv (E.ENVLAB _) _ _ = raise Fail "lab environment should always be labelled"
  | filterEnv (E.ENVOVE _) _ _ = raise Fail "ove environment should always be labelled"
  | filterEnv E.ENVNUL deps1 deps2 = E.ENVNUL

(*fun filter (E.ENVVAR ev) deps1 deps2 deps =
      if D.isSubset (deps, D.union (deps1, deps2))
      then E.ENVVAR ev
      else E.ENVNUL
    | filter (E.ENVBIN bind) deps1 deps2 deps =
      if D.isSubset (deps, D.difference (deps1, deps2))
      then E.ENVBIN bind
      else if D.isSubset (deps, deps2)
      then E.ENVBIN (dumBind bind)
      else E.ENVNUL
    | filter (E.ENVACC acc) deps1 deps2 deps =
      if D.isSubset (deps, D.difference (deps1, deps2))
      then E.ENVACC acc
      else E.ENVNUL
    | filter (E.ENVCST cst)  deps1 deps2 deps =
      if D.isSubset (deps, D.difference (deps1, deps2))
      then E.ENVCST (filterCst cst deps1 deps2 deps)
      else (case cst of
		E.CSENV (_, env2) => E.ENVCST (E.CSENV (E.ENVVAR E.envvarFresh, filter env2 deps1 deps2 deps))
	      | _ => E.ENVNUL)
    | filter (E.ENVPOL env) deps1 deps2 deps = E.ENVPOL (filter env deps1 deps2 deps)
    | filter (E.ENVAPP (env1, env2)) deps1 deps2 deps =
      let val env1' = filter env1 deps1 deps2 deps
	  val env2' = filter env2 deps1 deps2 deps
      in if env1' = E.ENVNUL
	 then env2'
	 else if env2' = E.ENVNUL
	 then env1'
	 else E.ENVAPP (env1', env2')
      end
    | filter (E.ENVDEP (env, dep)) deps1 deps2 deps =
      let val deps' = case dep of DEPL lab => D.singleton dep
				| _ => D.empty
      in if D.isSubset (deps', D.difference (deps1, deps2))
	 then E.ENVDEP (filter env deps1 deps2 deps', dep)
	 else filter env deps1 deps2 deps'
      end
    | filter E.ENVNUL deps1 deps2 deps = E.ENVNUL

 and filterCst (E.CSITY (ity1,    ity2))    deps1 deps2 deps = E.CSITY (ity1,    ity2)
   | filterCst (E.CSNAM (itycon1, itycon2)) deps1 deps2 deps = E.CSNAM (itycon1, itycon2)
   | filterCst (E.CSENV (env1,    env2))    deps1 deps2 deps =
     E.CSENV (env1, filter env2 deps1 deps2 deps)
   | filterCst (E.CSSTA (status1, status2)) deps1 deps2 deps = E.CSSTA (status1, status2)*)


(* ------ SLICING ------ *)

fun slicing prog deps =
    A.slicing prog (D.foldr (fn (D.DEPL lab, labs) => lab :: labs)
			    []
			    deps)


(* ------ ERROR FORMATING ------ *)

fun formatError error prog =
    let val kind   = getErrorKind    error
	val deps   = getErrorDeps    error
	val slice  = getErrorSlice   error
	val regs   = getErrorRegions error
	val slice' = if A.is_empty_term slice
		     then slicing prog deps
		     else slice
    in "kind:  "  ^ toStringErrorKindText kind   ^ "\n" ^
       "slice: "  ^ A.export slice'              ^ "\n" ^
       (*"regions:" ^ toStringExtRegs regs         ^ "\n" ^*)
       (*"slice: "  ^ A.toString slice'            ^ "\n" ^*)
       (*"deps:  "  ^ D.toStringDeps deps          ^ "\n" ^*)
       ""
    end

fun toStringErrorsText' [] _ = ""
  | toStringErrorsText' (error :: errors) prog =
    "[ERROR]\n" ^ formatError error prog ^ toStringErrorsText' errors prog

fun toStringErrorsText [] _ = "[typable]\n\n"
  | toStringErrorsText slices prog = toStringErrorsText' slices prog

fun exportErrorKindToTok (TYCONSCLASH   _) = "TYP"
  | exportErrorKindToTok (ETYVARCLASH   _) = "TYP"
  | exportErrorKindToTok (CTYVARCLASH   _) = "TYP"
  | exportErrorKindToTok (OVERLOAD      _) = "OVE"
  | exportErrorKindToTok (OVERLOADS     _) = "OVS"
  | exportErrorKindToTok (ARITYCLASH    _) = "ARI"
  | exportErrorKindToTok (EQTYPE        _) = "EQT"
  | exportErrorKindToTok (UNPARSABLE    _) = "PAR"
  | exportErrorKindToTok (UNREBOUNDABLE _) = "PAR"
  | exportErrorKindToTok (SYNTAXERROR   _) = "PAR"
  | exportErrorKindToTok (FREEID        _) = "IDE"
  | exportErrorKindToTok (FREEATOMS     _) = "IDE"
  | exportErrorKindToTok (EQUALITYDEC   _) = "EQD"
  | exportErrorKindToTok (FREEETYVAR    _) = "FEV"
  | exportErrorKindToTok FREEITYVAR        = "FIV"
  | exportErrorKindToTok TOPOVERLOAD       = "TOV"
  | exportErrorKindToTok CIRCULARITY       = "CIR"
  | exportErrorKindToTok NOTENOUGHINFO     = "NEI"

fun exportErrorKindToLisp kind =
    "(" ^ exportErrorKindToTok kind ^ " \"" ^ toStringErrorKindText kind ^ "\")"

fun transfun1 #"\""    = "\\\""
  | transfun1 #"\227"  = "\227" (* sequence ldots and rdots *)
  | transfun1 #"\128"  = "\128"
  | transfun1 #"\152"  = "\152"
  | transfun1 #"\153"  = "\153"
  | transfun1 #"\154"  = "\154"
  | transfun1 #"\155"  = "\155"
  | transfun1 #"\226"  = "\226" (* old ldots and rdots *)
  | transfun1 #"\167"  = "\167"
  | transfun1 #"\188"  = "\188"
  | transfun1 #"\189"  = "\189"
  | transfun1 #"\159"  = "\159" (* new ldots and rdots *)
  | transfun1 #"\168"  = "\168"
  | transfun1 #"\169"  = "\169"
  | transfun1 x        = Char.toString x

fun transfun2 st = String.translate transfun1 st

val emacstab = "         "

fun exportErrorToLisp error =
    let val id = "(id . " ^ Int.toString (getErrorId error) ^ ")"
	val cd = "(assumptions . ())"
	val ek = "(kind . " ^ exportErrorKindToLisp (getErrorKind error) ^ ")"
	val rm = "(remove . ())"
	val ss = "(slice . \"" ^ transfun2 (A.export (getErrorSlice error)) ^ "\")"
	val at = "(ast . \"\")"
	val re = "(regions . (" ^ (String.translate (fn #"\\" => "\\\\" | x => str x) (printLispExtRegs (getErrorRegions error))) ^ "))"
	val mn = "(minimal . true)"
    in (id, cd, ek, rm, ss, at, re, mn)
    end

fun exportErrorsToLisp' [] _ = ""
  | exportErrorsToLisp' [err] ind =
    let val ind' = emacstab ^ "      "
	val (id, ap, ek, rm, sl, at, re, mn) = exportErrorToLisp err
	val ap = ind' ^ " " ^ ap
	val ek = ind' ^ " " ^ ek
	val sl = ind' ^ " " ^ sl
	val at = ind' ^ " " ^ at
	val rm = ind' ^ " " ^ rm
	val re = ind' ^ " " ^ re
	val mn = ind' ^ " " ^ mn
    in ind ^ "(" ^ id ^ "\n" ^ re ^ "\n" ^ sl ^ "\n" ^ ap ^ "\n" ^ ek ^ "\n" ^ rm ^ "\n" ^ mn ^ ")"
    end (* at is the ast of the slice *)
  | exportErrorsToLisp' (err :: errs) ind =
    let val ind' = emacstab ^ "      "
	val (id, ap, ek, rm, sl, at, re, mn) = exportErrorToLisp err
	val ap = ind' ^ " " ^ ap
	val ek = ind' ^ " " ^ ek
	val sl = ind' ^ " " ^ sl
	val at = ind' ^ " " ^ at
	val rm = ind' ^ " " ^ rm
	val re = ind' ^ " " ^ re
	val mn = ind' ^ " " ^ mn
	val xs = exportErrorsToLisp' errs ind'
    in ind ^ "(" ^ id ^ "\n" ^ re ^ "\n" ^ sl ^ "\n" ^ at ^ "\n" ^ ek ^ "\n" ^ rm ^ "\n" ^ mn ^ ")\n" ^ xs
    end

fun exportErrorsToLisp errors =
    "(setq sml-tes-slice-data '(" ^ (exportErrorsToLisp' errors "") ^ ")\n  )"


(* ------ EXTENDED REGIONS EXTRACTION ------ *)

fun getColor label regions (TYCONSCLASH ((_, lab1, _), (_, lab2, _))) =
    let val col =
	    if label = lab1
	    then END_POINT1
	    else if label = lab2
	    then END_POINT2
	    else ERR_LOC
    in map (fn reg => L (reg, col, 1)) regions
    end
  | getColor label regions (ETYVARCLASH ((_, lab1, _), (_, lab2, _))) =
    let val col =
	    if label = lab1
	    then END_POINT1
	    else if label = lab2
	    then END_POINT2
	    else ERR_LOC
    in map (fn reg => L (reg, col, 1)) regions
    end
  | getColor label regions (CTYVARCLASH ((_, lab1, _), (_, lab2, _))) =
    let val col =
	    if label = lab1
	    then END_POINT1
	    else if label = lab2
	    then END_POINT2
	    else ERR_LOC
    in map (fn reg => L (reg, col, 1)) regions
    end
  | getColor label regions (OVERLOAD ((_, lab, _), tyconErrors)) =
    let val col =
	    if label = lab
	    then END_POINT1
	    else if List.exists (fn (_, lab, _) => label = lab) tyconErrors
	    then END_POINT2
	    else ERR_LOC
    in map (fn reg => L (reg, col, 1)) regions
    end
  | getColor label regions (OVERLOADS (tyconErrors1, tyconErrors2)) =
    let val col =
	    if List.exists (fn (_, lab, _) => label = lab) tyconErrors1
	    then END_POINT1
	    else if List.exists (fn (_, lab, _) => label = lab) tyconErrors2
	    then END_POINT2
	    else ERR_LOC
    in map (fn reg => L (reg, col, 1)) regions
    end
  | getColor label regions (ARITYCLASH ((_, lab1, _), (_, lab2, _))) =
    let val col =
	    if label = lab1
	    then END_POINT1
	    else if label = lab2
	    then END_POINT2
	    else ERR_LOC
    in map (fn reg => L (reg, col, 1)) regions
    end
  | getColor label regions (EQTYPE (lab1, lab2)) =
    let val col =
	    if label = lab1
	    then END_POINT1
	    else if label = lab2
	    then END_POINT2
	    else ERR_LOC
    in map (fn reg => L (reg, col, 1)) regions
    end
  | getColor label regions (EQUALITYDEC (lab, st)) =
    let val col =
	    if label = lab
	    then END_POINT1
	    else ERR_LOC
    in map (fn reg => L (reg, col, 1)) regions
    end
  | getColor label regions (FREEETYVAR lab) =
    let val col =
	    if label = lab
	    then END_POINT1
	    else ERR_LOC
    in map (fn reg => L (reg, col, 1)) regions
    end
  | getColor label regions FREEITYVAR        = map (fn reg => L (reg, ERR_LOC,  1)) regions
  | getColor label regions TOPOVERLOAD       = map (fn reg => L (reg, ERR_LOC,  1)) regions
  | getColor label regions (UNPARSABLE    _) = map (fn reg => L (reg, SYNT_LOC, 1)) regions
  | getColor label regions (UNREBOUNDABLE _) = map (fn reg => L (reg, SYNT_LOC, 1)) regions
  | getColor label regions (SYNTAXERROR   _) = map (fn reg => L (reg, SYNT_LOC, 1)) regions
  | getColor label regions (FREEID        _) = map (fn reg => L (reg, FREE_LOC, 1)) regions
  | getColor label regions (FREEATOMS     _) = map (fn reg => L (reg, FREE_LOC, 1)) regions
  | getColor label regions CIRCULARITY       = map (fn reg => L (reg, ERR_LOC,  1)) regions
  | getColor label regions NOTENOUGHINFO     = map (fn reg => L (reg, ERR_LOC,  1)) regions

fun get_extregs_decs (A.N {kind, label, value, regions, parents, children}) error paren =
    if D.isLabelInDeps label (getErrorDeps error)
    then let val eregs  = List.concat (map (fn t => get_extregs_decs t error true) children)
	     val eregs1 = getColor label regions (getErrorKind error)
	     val eregs2 =
		 if paren
		 then map (fn reg => L (reg, ERR_LOC,  1)) parents
		 else []
	 in eregs1 @ eregs2 @ eregs
	 end
    else List.concat (map (fn t => get_extregs_decs t error false) children)

fun get_extregs_file (term as A.N {kind, label, value, regions, parents, children}) error =
    if A.getClass term = A.FILE
    then (value, List.concat (map (fn term => get_extregs_decs term error true) children))
    else raise Fail "wrong format"

fun get_extregs (term as A.N {kind, label, value, regions, parents, children}) error =
    if A.getClass term = A.PROG
    then map (fn term => get_extregs_file term error) children
    else raise Fail "wrong format"


(* ------ MINIMISATION ------ *)

fun setError error prog =
    let val slice = slicing prog (getErrorDeps error)
	val eregs = get_extregs prog error
	val id    = nextIdError ()
    in setErrorId (setErrorRegions (setErrorSlice error slice) eregs) id
    end

fun getLabBind (E.ENVVAR _)                        = []
  | getLabBind (E.ENVBIN _)                        = []
  | getLabBind (E.ENVACC _)                        = []
  | getLabBind (E.ENVCST (E.CSENV (ev, env)))      = getLabBind env
  | getLabBind (E.ENVCST _)                        = []
  | getLabBind (E.ENVSUB _)                        = []
  | getLabBind (E.ENVPOL (env, mrk))               = getLabBind env
  | getLabBind (E.ENVAPP (env1, env2))             = (getLabBind env1) @ (getLabBind env2)
  | getLabBind (E.ENVSET envs)                     = foldr (fn (env, set) => (getLabBind env) @ set) [] envs
  | getLabBind (E.ENVLOC (env1, env2))             = (getLabBind env1) @ (getLabBind env2)
  | getLabBind (E.ENVDEP (E.ENVBIN _, D.DEPL lab)) = [D.DEPL lab]
  | getLabBind (E.ENVDEP (env, _))                 = getLabBind env
  | getLabBind (E.ENVFIL (f, env1, env2, _))       = (getLabBind env1) @ (getLabBind env2)
  | getLabBind (E.ENVERR _)                        = []
  | getLabBind (E.ENVOVL _)                        = []
  | getLabBind (E.ENVTOF _)                        = []
  | getLabBind (E.ENVMRK _)                        = []
  | getLabBind (E.ENVLAB _)                        = []
  | getLabBind (E.ENVOVE _)                        = []
  | getLabBind E.ENVNUL                            = []

(*fun unbind env sub [] deps = deps
  | unbind env sub (x :: xs) deps =
    let val env' = filterEnv env deps (D.singleton x) (*D.empty*)
    in case solver env' sub of
	   SUCCESS _ => unbind env sub xs deps
	 | ERROR err =>
	   let val deps2 = D.intersection (deps, getErrorDeps err)
	   in unbind env sub xs deps2
	   end
    end*)

(*fun reduce1 env sub deps [] = deps
  | reduce1 env sub deps (x :: xs) =
    let val deps' = D.addList (deps, xs)
	val env'  = filterEnv env deps' (D.singleton x) (*D.empty*)
    in case solver env' sub of
	   SUCCESS _ => reduce1 env sub (D.add (deps, x)) xs
	 | ERROR err =>
	   let val deps2 = D.intersection (D.addList (D.empty, xs), getErrorDeps err)
	   in reduce1 env sub deps (D.listItems deps2)
	   end
    end*)

fun reduce2 env sub deps [] _ = deps
  | reduce2 env sub deps (x :: xs) prog =
    let val deps' = D.addList (deps, xs)
	val env'  = filterEnv env deps' (D.singleton x) (*D.empty*)
	(*val _ = print "--\n"
	val _ = print (E.toStringEnv env ^ "\n")
	val _ = print "--\n"
	val _ = print (E.toStringEnv env' ^ "\n")
	val _ = print "--\n"
	 *)
	(*val _ = print "--\n"
	val _ = print ("1: "  ^ A.export (slicing prog (D.add (deps', x))) ^ "\n")
	val _ = print ("keep dependencies: " ^ D.toStringDeps deps' ^ "\n")
	val _ = print ("trying to remove dependency: " ^ D.toStringDep x ^ "\n")
	val _ = print ("2: "  ^ A.export (slicing prog deps') ^ "\n")
	val _ = print "--\n"*)
    in if D.isDummyDep x
       then reduce2 env sub (D.add (deps, x)) xs prog
       else case solver env' sub of
		SUCCESS _ => reduce2 env sub (D.add (deps, x)) xs prog
	      | ERROR err =>
		let val deps' = getErrorDeps err
		    val deps2 = D.intersection (deps, deps')
		    val deps3 = D.intersection (D.addList (D.empty, xs), deps')
		    (*val _ = print ("----\n" ^ formatError err prog ^ "----\n")
		    val _ = print (D.toStringDeps deps' ^ "\n")*)
		in reduce2 env sub deps2 (D.listItems deps3) prog
		end
    end

(*fun minimize1 prog err env sub =
    let val deps1 = getErrorDeps err
	val _     = if isSuccess (solver (filterEnv env deps1 D.empty (*D.empty*)) sub)
		    then (print (formatError err prog);
			  print (A.toString (slicing prog deps1));
			  raise Fail "The error should be an error before minimisation")
		    else ()
	val deps2 = unbind env sub (getLabBind env) (D.getLabsDeps deps1)
	val _     = if isSuccess (solver (filterEnv env deps2 D.empty (*D.empty*)) sub)
		    then (print (formatError err prog);
			  print (A.toString (slicing prog deps2));
			  raise Fail "The error should be an error during minimisation")
		    else ()
	val deps3 = reduce1 env sub D.empty (D.listItems deps2)
	val env'  = filterEnv env deps3 D.empty (*D.empty*)
    in case solver env' sub of
	   SUCCESS _ => (print (formatError err prog);
			 print (A.toString (slicing prog deps3));
			 raise Fail "The error should be an error after minimisation")
	 | ERROR error => error
    end*)

fun minimize2 prog err env sub =
    let val deps1 = getErrorDeps err
	(*val _     = print ("\n+++++++++++++++++++++\n" ^ E.toStringEnv env ^ "+++++++++++++++++++++\n\n")*)
	(*val _     = print (D.toStringDeps deps1 ^ "\n")*)
	val _     = print ("\n*********************\n" ^ formatError err prog ^ "*********************\n\n")
	val bind  = getLabBind env
	val labs  = D.intersection (deps1, D.addList (D.empty, bind))
	val labs1 = D.difference (deps1, labs)
	val labs2 = D.listItems labs
	(*val labs  = D.getLabsDeps deps1
	val labs1 = D.difference (deps1, labs) (* This is empty, isn't it? *)
	val labs2 = D.listItems (D.intersection (deps1, labs))*)
	val deps2 = reduce2 env sub labs1 labs2 prog
	val deps3 = reduce2 env sub D.empty (D.listItems deps2) prog
	val env'  = filterEnv env deps3 D.empty (*D.empty*)
	(*val _     = (print (A.toString prog                        ^ "\n");
		     print ("1: "  ^ A.export (slicing prog deps1) ^ "\n");
		     print ("rm: " ^ D.toStringDeps labs           ^ "\n");
		     print ("2: "  ^ A.export (slicing prog deps2) ^ "\n");
		     print ("3: "  ^ A.export (slicing prog deps3) ^ "\n"))*)
    in case solver env' sub of
	   SUCCESS _ => (print (formatError err prog ^ "\n");
			 print (A.toString prog ^ "\n");
			 print (A.export (slicing prog deps1) ^ "\n");
			 print ("1: " ^ A.toString (slicing prog deps1) ^ "\n");
			 print ("2: " ^ A.toString (slicing prog deps2) ^ "\n");
			 print ("3: " ^ A.toString (slicing prog deps3) ^ "\n");
			 print (toStringErrorKind (getErrorKind err) ^ "\n");
			 print (D.toStringDeps deps3 ^ "\n");
			 print (E.toStringEnv env' ^ "\n");
			 raise Fail "The error should be an error after minimisation")
	 | ERROR error => error
    end

(* We finish setting the error in there. *)
fun minimize prog err env sub =
    let val error = minimize2 prog err env sub
    in setError error prog
    end


(* ------ TIMER ------ *)

structure VTimer = struct
type timer = {real : Timer.real_timer,
	      cpu  : Timer.cpu_timer}

fun startTimer () = {real = Timer.startRealTimer (),
		     cpu  = Timer.startCPUTimer ()}

fun getTime (timer : timer) = Timer.checkRealTimer (#real timer)

fun getMilliTime timer = Time.toMilliseconds (getTime timer)

fun stillTime timer limit = getMilliTime timer > limit
end


(* ------ SEARCH SPACE ------ *)

structure SuccDeps = struct type ord_key = D.deps val compare = D.compare end
structure SD = BinarySetFn(SuccDeps) (* Set Labels *)

(* space   is the actual search space and
 * success is the filters for which the solver succeeded.
 * Success is the set of filters for which we haven't found any error,
 * meaning that for each filter in success, the filtering of the constraint
 * set using the filter is solvable. *)
type searchSpace = {space : D.deps list, success : SD.set}

(* Empty searchspace *)
val emSpace = []
val emSuccess = SD.empty
val emSearchSpace = {space = emSpace, success = emSuccess}

(* Initial search space, we start with then empty filter. *)
val initSearchSpace = {space = [D.empty], success = emSuccess}

(* Get the size of a searchspace *)
fun getSizeSearchSpace {space, success} =
    (length space, SD.numItems success)

fun toStringSizeSearchSpace searchspace =
    let val (space, success) = getSizeSearchSpace searchspace
    in "{space = "    ^ Int.toString space   ^
       ", success = " ^ Int.toString success ^ "}"
    end

fun printSizeSearchSpace searchspace =
    print (toStringSizeSearchSpace searchspace ^ "\n")

fun toStringSearchSpace (searchspace as {space, success}) =
    let val (spaceSize, successSize) = getSizeSearchSpace searchspace
    in "{space = "    ^ T.fmt {init = "(", final = ")", sep = ",", fmt = D.toStringDeps} space ^
       ", success = " ^ Int.toString successSize ^ "}"
    end

fun printSearchSpace searchspace =
    print (toStringSearchSpace searchspace ^ "\n")

(* Returns one filter from the searchspace *)
fun getOneFilter {space = [], success} = NONE
  | getOneFilter (searchspace as {space = deps :: sp, success}) =
    SOME (deps, {space = sp, success = success})

(* Adds one filter to a searchspace *)
fun addToSearchSpace deps {space, success} =
    {space   = deps :: (List.filter (fn deps' => not (D.isSubset (deps, deps'))) space),
     success = success}

(* Adds one filter to the end of a searchspace *)
fun addToSearchSpaceEnd deps {space, success} =
    {space   = (List.filter (fn deps' => not (D.isSubset (deps, deps'))) space) @ [deps],
     success = success}

(* Test whether or not a filter is already in a searchspace*)
fun isInSearchSpace deps {space, success} =
    D.exSubseteq deps space
    orelse
    SD.exists (fn deps' => D.isSubseteq (deps', deps)) success

(* Adds new filters to the searchspace, build from the first argument (filter)
 * and the second one (error found). *)
fun buildFilters filter error searchspace b =
    ((*printSizeSearchSpace searchspace;*)
     (* Uncomment the above line to print the size of a searchspace while slicing *)
     D.foldr (fn (dep, searchspace') =>
		 if D.isDummyDep dep
		 then searchspace'
		 else let val deps = D.add (filter, dep)
		      in if isInSearchSpace deps searchspace
			 then searchspace'
			 else if b
			 then addToSearchSpace deps searchspace'
			 else addToSearchSpaceEnd deps searchspace'
		      end)
	     searchspace
	     error)

fun getSuccess {space, success} = SD.listItems success

fun addSuccess deps {space, success} =
    {space = space, success = SD.add (success, deps)}

fun addSuccess' deps {space, success} =
    {space   = List.filter (fn deps' => not (D.isSubseteq (deps, deps'))) space,
     success = SD.add (success, deps)}


(* ------ ENUMERATION ------ *)

fun getLabEnv (E.ENVVAR _)                    = D.empty
  | getLabEnv (E.ENVBIN _)                    = D.empty
  (* We don't need to go down the env of a BINDSTR because
   * it is always a variable at constraint generation. *)
  | getLabEnv (E.ENVACC _)                    = D.empty (* same for accessors *)
  | getLabEnv (E.ENVCST (E.CSENV (ev, env)))  = getLabEnv env
  | getLabEnv (E.ENVCST _)                    = D.empty
  | getLabEnv (E.ENVSUB _)                    = D.empty
  | getLabEnv (E.ENVPOL (env, mrk))           = getLabEnv env
  | getLabEnv (E.ENVAPP (env1, env2))         = D.union (getLabEnv env1, getLabEnv env2)
  | getLabEnv (E.ENVSET envs)                 = foldr (fn (env, set) => D.union (getLabEnv env, set)) D.empty envs
  | getLabEnv (E.ENVLOC (env1, env2))         = D.union (getLabEnv env1, getLabEnv env2)
  | getLabEnv (E.ENVDEP (env, dep))           = D.union (getLabEnv env, D.singleton dep)
  (* The dep is always a label at constaint generation. *)
  | getLabEnv (E.ENVFIL (f, env1, env2, lab)) = D.union (getLabEnv env1, D.union (getLabEnv env2, D.singleton (D.mk_dep lab)))
  | getLabEnv (E.ENVERR _)                    = D.empty
  | getLabEnv (E.ENVOVL _)                    = D.empty
  | getLabEnv (E.ENVTOF _)                    = D.empty
  | getLabEnv (E.ENVMRK _)                    = D.empty
  | getLabEnv (E.ENVLAB _)                    = D.empty
  | getLabEnv (E.ENVOVE _)                    = D.empty
  | getLabEnv E.ENVNUL                        = D.empty

(*fun isinSet _ [] = false
  | isinSet set1 (set2 :: sets) = D.equal (set1, set2) orelse isinSet set1 sets*)

(*fun newFilters filters (*filters still to test*)
	       labs1   (*tested filter*)
	       labs2   (*new error*)
	       errors =
    D.foldr (fn (x, filters') =>
		let val labs = D.add (labs1, x)
		in if isinSet labs filters
		   then filters'
		   else labs :: filters'
		end)
	    filters
	    labs2*)

fun alreadyone filter errors =
    List.find (fn err => D.isEmpty (D.intersection (getErrorDeps err, filter)))
	      errors

(* prt (stands for print) is the switch to print or not print in the strandard
 * output some stuff, b is true iff the timer has not run out. *)
fun printErrorEnd prt prt2 errors prog b time =
    if prt andalso prt2
    then if List.null errors
	 then print ("[typable(" ^ LargeInt.toString time ^ "ms)]\n\n")
	 else if b
	 then print ("[finished in " ^ LargeInt.toString time ^ "ms]\n")
	 else print ("[timer ran off (" ^ LargeInt.toString time ^ "ms)]\n")
    else if prt2
    then print (toStringErrorsText errors prog)
    else ()

fun printErrorMid prt err prog timeop =
    if prt
    then let val t =
		 case timeop of
		     NONE => ""
		   | SOME time => "(" ^ LargeInt.toString time ^ "ms)"
	 in print ("[ERROR" ^ t ^ "]\n" ^ formatError err prog ^ "\n")
	 end
    else ()

fun enumeration timer prog env labs errors searchspace timelimit export prt sub =
    case getOneFilter searchspace of
     NONE => (errors, true)
   | SOME (filter, searchspace') =>
((*printSearchSpace searchspace;*)
     if VTimer.stillTime timer timelimit
     then (errors, false)
     else case alreadyone filter errors of
	      SOME err =>
	      let val ldeps         = D.getLabsDeps (getErrorDeps err)
		  val searchspace'' = buildFilters filter ldeps searchspace' true
	      in enumeration timer prog env labs errors searchspace'' timelimit export prt sub
	      end
	    | NONE =>
	      case solver (filterEnv env labs filter (*D.empty*)) sub of
		  SUCCESS _ =>
		  let val searchspace'' = addSuccess filter searchspace'
		  in enumeration timer prog env labs errors searchspace'' timelimit export prt sub
		  end
		| ERROR err =>
		  let val err'          = minimize prog err env sub
		      val time          = VTimer.getMilliTime timer
		      val _             = printErrorMid prt err' prog (SOME time)
		      val _             = export err'
		      val ldeps         = D.getLabsDeps (getErrorDeps err')
		      val searchspace'' = buildFilters filter ldeps searchspace' true
		  in enumeration timer prog env labs (err' :: errors) searchspace'' timelimit export prt sub
		  end)

(* First is a crude hack to only deal with ENVTOF at the first run of the
 * solver. *)

(* If prt is true, we print the errors as soon as they are found *)
fun enum prog env timelimit export prt sub =
    let val timer  = VTimer.startTimer ()
	val _      = setFirst ()
	val search = initSearchSpace
	(*val _      = print (A.toString prog ^ "\n")*)
	val (error, b) = enumeration timer prog env (getLabEnv env) [] search timelimit export prt sub
	val time   = VTimer.getMilliTime timer
    in (error, b, time)
    end


(* ------ INTERFACE WITH THE SLICER ------ *)

(* TODO: all of that should go in the interface file. *)

val nbOutputFiles = ref (1, "")

(* We removed .el from the output file name *)
fun resetNbOutputFiles output = nbOutputFiles := (1, output)

fun getNbOutputFiles () = !nbOutputFiles

fun nextNbOutputFiles () =
    let val (x, out) = !nbOutputFiles
	val _ = nbOutputFiles := (x + 1, out)
    in (x, out)
    end

fun getBaseLispFile (SOME file) =
    if String.isSuffix ".el" file
    then String.substring (file, 0, (String.size file) - 3)
	 handle Subscript => ""
    else ""
  | getBaseLispFile NONE = ""

fun finishedLispM x = "(setq sml-tes-finished-message \"" ^ x ^ "\")"

fun finishedLispMessage1 msg =
    case msg of
	"" => finishedLispM "slicer worked OK, sorry no debugging message"
      | _  => finishedLispM ("slicer worked OK, " ^ msg)

fun finishedLispMessage2 msg =
    case msg of
	"" => finishedLispM "slicer encountered an internal bug, sorry no debugging message"
      | _  => finishedLispM ("slicer encountered an internal bug, " ^ msg)

fun genFinished msg =
    let val (nb, out) = getNbOutputFiles ()
    in if out = ""
       then ()
       else let val fin   = out ^ "-finished.el"
		val fin'  = fin ^ ".tmp"
		val stout = TextIO.openOut fin'
		val _     = TextIO.output (stout, msg)
		val _     = TextIO.closeOut stout
		val _     = OS.FileSys.rename {old = fin', new = fin}
	    in () end
	    handle IO.Io {name, function, cause} =>
		   (raise Fail ("cannot open or close one of the output file (function:" ^ function ^ ",name:" ^ name ^ ")\n"))
    end

fun exportToLisp error =
    let val (nb, out) = nextNbOutputFiles ()
    in if out = ""
       then ()
       else let val file  = out  ^ "-" ^ Int.toString nb ^ ".el"
		val file' = file ^ ".tmp"
		val stout = TextIO.openOut file'
		val _     = TextIO.output (stout, exportErrorsToLisp [error])
		val _     = TextIO.closeOut stout
		val _     = OS.FileSys.rename {old = file', new = file}
	    in ()
	    end
    end


(* ------ SANITY CHECKER ------ *)

fun getNEQIty (E.ITYVAR _) = []
  | getNEQIty (E.ITYETV _) = []
  | getNEQIty (E.ITYCON (ityseq, itycon)) = getNEQItyseq ityseq @ getNEQItycon itycon
  | getNEQIty (E.ITYOR orseq) = getNEQOrseq orseq
  | getNEQIty (E.ITYTYP (ity, lab)) = getNEQIty ity
  | getNEQIty (E.ITYDEP (ity, dep)) = map (fn (name, deps) => (name, D.add (deps, dep))) (getNEQIty ity)

and getNEQOrseq (E.ORSEQ (ityseq, _, _)) = getNEQItyseq ityseq

and getNEQItyseq (E.ITYSEQVAR _) = []
  | getNEQItyseq (E.ITYSEQSEQ (itys, _)) = List.concat (map getNEQIty itys)
  | getNEQItyseq (E.ITYSEQDEP (ityseq, dep)) = map (fn (name, deps) => (name, D.add (deps, dep))) (getNEQItyseq ityseq)

and getNEQItycon (E.ITYCONVAR _) = []
  | getNEQItycon (E.ITYCONNAM (name, eq, _)) = if eq then [] else [(name, D.empty)]
  | getNEQItycon (E.ITYCONDEP (itycon, dep)) = map (fn (name, deps) => (name, D.add (deps, dep))) (getNEQItycon itycon)

fun getEqualityDecErrors unifenv prog prt =
    LVMAP.foldri (fn (lab, (ityvar, E.EQD), errors) =>
		     let val ity = buildIty unifenv (E.mk_tyvar ityvar)
			 val (ityvars, etyvars) = E.getDepsIEtyvarsIty ity
			 val msgVar  = "type contains type variables"
			 val msgCon  = "type contains non-equality types"
			 val errors1 = E.foldriITVM
					   (fn (ityvar, deps, errors) =>
					       let val deps'  = D.add (deps, D.mk_dep lab)
						   val kind   = EQUALITYDEC (lab, msgVar)
						   val error1 = mk_pre_error kind deps'
						   val error2 = setError error1 prog
					       in error2 :: errors
					       end)
					   []
					   ityvars
			 val errors2 = E.foldriETVM
					   (fn (etyvar, deps, errors) =>
					       let val deps'  = D.add (deps, D.mk_dep lab)
						   val kind   = EQUALITYDEC (lab, msgVar)
						   val error1 = mk_pre_error kind deps'
						   val error2 = setError error1 prog
					       in error2 :: errors
					       end)
					   []
					   etyvars
			 val errors3 = map (fn (name, deps) =>
					       let val deps' = D.add (deps, D.mk_dep lab)
						   (*val _ = print ("\n" ^ name ^ "\n")
						   val _ = print (E.ppIty' ity ^ "\n")*)
						   val kind  = EQUALITYDEC (lab, msgCon)
						   val error = mk_pre_error kind deps'
					       in setError error prog
					       end)
					   (getNEQIty ity)
			 (* NOTE: errors3 should be empty, because these errors
			  * should be detected during slicing. *)
			 (* NOTE: Well, apparently it's not!!!!
			  * For example, given:
			  *
			  *    parameter Op : Type
			  *    import insert
			  *    let foo (p : Op) ps = insert (op =) p ps ;;
			  *
			  * we don't get any error until exporting to Nuprl.
			  * Apparently, isEqIty which is used by solveOccIty
			  * does not get this error...Why?!?!?!
			  *
			  * I had to uncomment '@ errors3' just below and also
			  * for some reason at constraint generation, for =,
			  * I was putting EQD labels on the whole type ('a Deq)
			  * instead that on just 'a (in acc2op and gen_id_bound).
			  *)
			 val errors' = errors1 @ errors2 @ errors3
			 val _       = app (fn error =>
					       (printErrorMid prt error prog NONE;
						exportToLisp error;
						()))
					   errors'
		     in errors @ errors'
		     end
		   | (lab, (ityvar, E.OTH), errors) => errors)
		 []
		 (getLocEq unifenv)

fun noTyvarsInSolvedIty (E.ITYVAR (ityvar, eq)) ityvarset etyvarset deps prog prt =
    if E.isinITVS (ityvarset, ityvar)
    then []
    else let val error1 = mk_pre_error FREEITYVAR deps
	     val error2 = setError error1 prog
	     val _      = printErrorMid prt error2 prog NONE
	     val _      = exportToLisp error2;
	 in [error2]
	 end
  | noTyvarsInSolvedIty (E.ITYETV (etyvar, lab)) ityvarset etyvarset deps prog prt =
    if E.isinETVS (etyvarset, etyvar)
    then []
    else let val error1 = mk_pre_error (FREEETYVAR lab) deps
	     val error2 = setError error1 prog
	     val _      = printErrorMid prt error2 prog NONE
	     val _      = exportToLisp error2;
	 in [error2]
	 end
  | noTyvarsInSolvedIty (E.ITYCON (ityseq, itycon)) ityvarset etyvarset deps prog prt =
    let val (ityseq', deps1) = E.extractDepsItyseq ityseq
	val (itycon', deps2) = E.extractDepsItycon itycon
	val deps' = D.union (deps, deps1)
    in case (ityseq', itycon') of
	   (E.ITYSEQSEQ (itys, _), E.ITYCONNAM _) =>
	   List.concat (map (fn ity => noTyvarsInSolvedIty ity ityvarset etyvarset deps' prog prt) itys)
	 | _ => raise Fail "We shouldn't have sequence of type constructor variables at this stage"
    end
  | noTyvarsInSolvedIty (E.ITYOR (E.ORSEQ (ityseq, _, _))) ityvarset etyvarset deps prog prt =
    let val (ityseq0, deps0) = E.extractDepsItyseq ityseq
	val deps' = D.union (deps, deps0)
    in case ityseq0 of
	   E.ITYSEQSEQ (itys, _) =>
	   List.concat (map (fn ity => noTyvarsInSolvedIty ity ityvarset etyvarset deps' prog prt) itys)
	 | _ => raise Fail "We shouldn't have sequence variables at this stage"
    end
  | noTyvarsInSolvedIty (E.ITYTYP (ity, lab)) ityvarset etyvarset deps prog prt =
    noTyvarsInSolvedIty ity ityvarset etyvarset deps prog prt
  | noTyvarsInSolvedIty (E.ITYDEP (ity, dep)) ityvarset etyvarset deps prog prt =
    noTyvarsInSolvedIty ity ityvarset etyvarset (D.add (deps, dep)) prog prt


fun noTyvarsInSolvedEnv unifenv envs prog prt =
    let fun getErrors (E.ENVVAR _) deps = raise Fail "If program is typable, we shouldn't get any environment variable"
	  | getErrors (E.ENVBIN (E.BINDVID (vid, lab, E.VPOLY ((ityvarset, etyvarset, idorset), (overseq, ity))), _)) deps =
	    let val ity1 = E.stripIty ity
		val ity2 = buildIty' unifenv ity1 ityvarset
	    in noTyvarsInSolvedIty ity2 ityvarset etyvarset deps prog prt
	    end
	  | getErrors (E.ENVBIN (E.BINDVID (vid, lab, E.VODEC ((ityvarset, etyvarset, idorset), (idor, ity))), _)) deps =
	    let val ity1 = E.stripIty ity
		val ity2 = buildIty' unifenv ity1 ityvarset
	    in noTyvarsInSolvedIty ity2 ityvarset etyvarset deps prog prt
	    end
	  | getErrors (E.ENVBIN (E.BINDVID (vid, lab, E.VMONO (ityvar, deps0, b)), _)) deps =
	    let val ity1  = buildIty' unifenv (E.mk_tyvar ityvar) E.emptyITVS
		val deps1 = D.union (deps0, deps)
	    in noTyvarsInSolvedIty ity1 E.emptyITVS E.emptyETVS deps1 prog prt
	    end
	  | getErrors (E.ENVBIN (E.BINDATM (atoms, lab, E.VMONO (ityvar, deps0, b)), _)) deps =
	    let val ity1  = buildIty' unifenv (E.mk_tyvar ityvar) E.emptyITVS
		val deps1 = D.union (deps0, deps)
	    in noTyvarsInSolvedIty ity1 E.emptyITVS E.emptyETVS deps1 prog prt
	    end
	  | getErrors (E.ENVBIN _) deps = []
	  | getErrors (E.ENVAPP (env1, env2)) deps =
	    let val errors1 = getErrors env1 deps
		val errors2 = getErrors env2 deps
	    in errors1 @ errors2
	    end
	  | getErrors (E.ENVDEP (env, dep)) deps = getErrors env (D.add (deps, dep))
	  | getErrors (E.ENVMRK _) _ = []
	  | getErrors E.ENVNUL _ = []
	  | getErrors (E.ENVFIL _) _ = raise Fail "Unexpected type environment in unification environment:ENVFIL(tv)"
	  | getErrors (E.ENVLOC _) _ = raise Fail "Unexpected type environment in unification environment:ENVLOC(tv)"
	  | getErrors (E.ENVCST _) _ = raise Fail "Unexpected type environment in unification environment:ENVCST(tv)"
	  | getErrors (E.ENVSUB _) _ = raise Fail "Unexpected type environment in unification environment:ENVSUB(tv)"
	  | getErrors (E.ENVPOL _) _ = raise Fail "Unexpected type environment in unification environment:ENVPOL(tv)"
	  | getErrors (E.ENVSET _) _ = raise Fail "Unexpected type environment in unification environment:ENVSET(tv)"
	  | getErrors (E.ENVERR _) _ = raise Fail "Unexpected type environment in unification environment:ENVERR(tv)"
	  | getErrors (E.ENVOVL _) _ = raise Fail "Unexpected type environment in unification environment:ENVOVL(tv)"
	  | getErrors (E.ENVTOF _) _ = raise Fail "Unexpected type environment in unification environment:ENVTOF(tv)"
	  | getErrors (E.ENVLAB _) _ = raise Fail "Unexpected type environment in unification environment:ENVLAB(tv)"
	  | getErrors (E.ENVOVE _) _ = raise Fail "Unexpected type environment in unification environment:ENVOVE(tv)"
	  | getErrors (E.ENVACC _) _ = raise Fail "Unexpected type environment in unification environment:ENVACC(tv)"
    in List.concat (map (fn (files, env) => getErrors env D.empty) envs)
    (*getErrors (getEnv unifenv) D.empty*)
    end

fun noOverloadedTopDecIty (E.ITYVAR _) _ _ _ _ = []
  | noOverloadedTopDecIty (E.ITYETV _) _ _ _ _ = []
  | noOverloadedTopDecIty (E.ITYCON (ityseq, _)) idorset deps prog prt =
    let val (ityseq', deps1) = E.extractDepsItyseq ityseq
	val deps' = D.union (deps, deps1)
    in case ityseq' of
	   E.ITYSEQSEQ (itys, _) =>
	   List.concat (map (fn ity => noOverloadedTopDecIty ity idorset deps' prog prt) itys)
	 | _ => raise Fail "We shouldn't have sequence of type constructor variables at this stage"
    end
  | noOverloadedTopDecIty (E.ITYOR orseq) idorset deps prog prt =
    noOverloadedTopDecOrseq orseq idorset deps prog prt
  | noOverloadedTopDecIty (E.ITYTYP (ity, lab)) idorset deps prog prt =
    noOverloadedTopDecIty ity idorset deps prog prt
  | noOverloadedTopDecIty (E.ITYDEP (ity, dep)) idorset deps prog prt =
    noOverloadedTopDecIty ity idorset (D.add (deps, dep)) prog prt

and noOverloadedTopDecOrseq (E.ORSEQ (ityseq, id, lab)) idorset deps prog prt =
    let val (ityseq0, deps0) = E.extractDepsItyseq ityseq
    in case ityseq0 of
	   E.ITYSEQSEQ (itys, _) =>
	   let val deps'   = D.union (deps, deps0)
	       val errors1 =
		   if E.isinOTVS (idorset, id)
		   then []
		   else let val error1 = mk_pre_error TOPOVERLOAD deps
			    val error2 = setError error1 prog
			    val _      = printErrorMid prt error2 prog NONE
			    val _      = exportToLisp error2;
			in [error2]
			end
	       val list    = map (fn ity => noOverloadedTopDecIty ity idorset deps' prog prt) itys
	       val errors2 = List.concat list
	   in errors1 @ errors2
	   end
	 | _ => raise Fail "We shouldn't have sequence variables at this stage"
    end

fun noOverloadedTopDec unifenv prog prt =
    let fun getErrors (E.ENVVAR _) deps = raise Fail "If program is typable, we shouldn't get any environment variable"
	  | getErrors (E.ENVBIN (E.BINDVID (vid, lab, E.VPOLY ((ityvarset, etyvarset, idorset), (overseq, ity))), _)) deps =
	    let val ity1 = E.stripIty ity
		val ity2 = buildIty' unifenv ity1 ityvarset
	    in noOverloadedTopDecIty ity2 idorset deps prog prt
	    end
	  | getErrors (E.ENVBIN (E.BINDVID (vid, lab, E.VODEC ((ityvarset, etyvarset, idorset), (idor, ity))), _)) deps =
	    let val ity1 = E.stripIty ity
		val ity2 = buildIty' unifenv ity1 ityvarset
	    in noOverloadedTopDecIty ity2 idorset deps prog prt
	    end
	  | getErrors (E.ENVBIN (E.BINDVID (vid, lab, E.VMONO (ityvar, deps0, b)), _)) deps =
	    let val ity1  = buildIty' unifenv (E.mk_tyvar ityvar) E.emptyITVS
		val deps1 = D.union (deps0, deps)
	    in noOverloadedTopDecIty ity1 E.emptyOTVS deps1 prog prt
	    end
	  | getErrors (E.ENVBIN _) deps = []
	  | getErrors (E.ENVAPP (env1, env2)) deps =
	    let val errors1 = getErrors env1 deps
		val errors2 = getErrors env2 deps
	    in errors1 @ errors2
	    end
	  | getErrors (E.ENVDEP (env, dep)) deps = getErrors env (D.add (deps, dep))
	  | getErrors (E.ENVMRK _) _ = []
	  | getErrors E.ENVNUL _ = []
	  | getErrors (E.ENVFIL _) _ = raise Fail "Unexpected type environment in unification environment:ENVFIL(ove)"
	  | getErrors (E.ENVLOC _) _ = raise Fail "Unexpected type environment in unification environment:ENVLOC(ove)"
	  | getErrors (E.ENVCST _) _ = raise Fail "Unexpected type environment in unification environment:ENVCST(ove)"
	  | getErrors (E.ENVSUB _) _ = raise Fail "Unexpected type environment in unification environment:ENVSUB(ove)"
	  | getErrors (E.ENVPOL _) _ = raise Fail "Unexpected type environment in unification environment:ENVPOL(ove)"
	  | getErrors (E.ENVSET _) _ = raise Fail "Unexpected type environment in unification environment:ENVSET(ove)"
	  | getErrors (E.ENVERR _) _ = raise Fail "Unexpected type environment in unification environment:ENVERR(ove)"
	  | getErrors (E.ENVOVL _) _ = raise Fail "Unexpected type environment in unification environment:ENVOVL(ove)"
	  | getErrors (E.ENVTOF _) _ = raise Fail "Unexpected type environment in unification environment:ENVTOF(ove)"
	  | getErrors (E.ENVLAB _) _ = raise Fail "Unexpected type environment in unification environment:ENVLAB(ove)"
	  | getErrors (E.ENVOVE _) _ = raise Fail "Unexpected type environment in unification environment:ENVOVE(ove)"
	  | getErrors (E.ENVACC _) _ = raise Fail "Unexpected type environment in unification environment:ENVACC(ove)"
    in getErrors (getEnv unifenv) D.empty
    end

fun sanitizer unifenv envs outputop prog (prt, prt2) =
    let val timer   = VTimer.startTimer ()
	val _       = resetNbOutputFiles (getBaseLispFile outputop)
	val errors1 = getEqualityDecErrors unifenv prog prt
	val errors2 = noTyvarsInSolvedEnv  unifenv envs prog prt
	(* We don't need to check for overloading at top dec, this is detected
	 * at slicing. *)
	(*val errors3 = noOverloadedTopDec   unifenv prog prt*)
	val errors  = errors1 @ errors2
	val time    = VTimer.getMilliTime timer
	val _       = printErrorEnd prt prt2 errors prog true time
    in List.null errors
    end

(* ------ RUN SLICER ------ *)

fun reset () =
    (D.reset ();
     E.reset ();
     ())

fun typecheck file sub =
    let val _    = reset ()
	val prog = P.parse [file]
	val env  = G.gen_prog prog
    in case solver env sub of
	   SUCCESS _ => print "typable\n"
	 | ERROR err => print ("[untypable]\n" ^ formatError err prog)
    end

fun getImports term =
    case A.getKind term of
	(A.DEC, A.DEC_IMPORT)  => map (fn id => A.getIdIdent id) (A.getChildren term)
      | (A.DEC, A.DEC_TIMPORT) => map (fn id => A.getIdIdent id) (A.getChildren term)
      | (A.DEC, _) => []
      | _ => foldr (fn (term, imports) => getImports term @ imports) [] (A.getChildren term)

fun filterImports ids term =
    case A.getKind term of
	(A.DEC, A.DEC_CONS)    =>
	(case A.getChildren term of
	     [ident, args, typ] =>
	     let val id = A.getIdIdent ident
	     in if List.exists (fn x => x = id) ids
		then SOME term
		else NONE
	     end
	   | _ => NONE)
      | (A.DEC, A.DEC_OCONS)   =>
	(case A.getChildren term of
	     [ident, args, typ, tyvar, seq] =>
	     let val id = A.getIdIdent ident
	     in if List.exists (fn x => x = id) ids
		then SOME term
		else NONE
	     end
	   | _ => NONE)
      | (A.DEC, A.DEC_TYCON)   =>
	(case A.getChildren term of
	     [seq, tycon, typ] =>
	     let val id = A.getIdIdent tycon
	     in if List.exists (fn x => x = id) ids
		then ((*print ("[filtering in: " ^ id ^ "]\n");*) SOME term)
		else NONE
	     end
	   | _ => NONE)
      | (A.DEC, A.DEC_EQTYCON) =>
	(case A.getChildren term of
	     [seq, tycon, eqdec, typ] =>
	     let val id = A.getIdIdent tycon
		 val eq = A.getIdIdent eqdec
	     in if List.exists (fn x => x = id orelse x = eq) ids
		then SOME term
		else NONE
	     end
	   | _ => NONE)
      | (A.DEC, _) => NONE
      | _ =>
	let val children' = List.mapPartial (filterImports ids) (A.getChildren term)
	in SOME (A.updChildren term children')
	end

fun merge libop prog =
    let val imports = getImports prog
	(*val _ = print (">>>>" ^ Bool.toString (Option.isSome libop) ^ "\n")*)
	(*val _ = app (fn id => print (">>" ^ id ^ "\n")) imports*)
	val libop'  =
	    if List.null imports
	    then NONE
	    else Option.join (Option.map (filterImports imports) libop)
    in case libop' of
	   (SOME lib) =>
	   (case A.merge lib prog of
		SOME x => x
	      | NONE => prog)
	 | NONE => prog
    end

(* If 'prt' is true then the errors are going to be printed while they are
 * found, if it is false then they are printed at the end of enumeration.
 *
 * 'btyp' has to actually be true to do slicing (by that I mean
 * enumerate the type errors).
 *)
fun slice' inputs outputop libop timelimit (prt, prt2) sub btyp =
    let val _     = reset ()
	val _     = resetNbOutputFiles (getBaseLispFile outputop)
	val lprog = Option.map (fn f => P.parse [f]) libop
	val prog  = P.parse inputs
	val prog' = merge lprog prog
	(*val _     = print (A.toString prog' ^ "\n")*)
	val env   = G.gen_prog prog'
	(*val _     = print ("[++allenv]\n")
	 val _     = print (E.toStringEnv env ^ "\n")
	 val _     = print ("[--allenv]\n")*)
	(*val _     = print ("sub:" ^ Bool.toString sub ^ "\n")*)
	val ok =
	    if btyp
	    then let val (errors, b, time) = enum prog' env timelimit exportToLisp prt sub
		     val _ = printErrorEnd prt prt2 errors prog' b time
		 in null errors
		 end
	    else true
    in (prog', env, ok)
    end

fun slice inputs outputop libop timelimit (prt as (prt1, prt2)) sub san btyp =
    let val (prog, env, b) = slice' inputs outputop libop timelimit prt sub btyp
    in if btyp andalso san andalso b (* b means no errors *)
       then let val solved = solver env sub
	    in case solved of
		   SUCCESS (unifenv, envs) =>
		   (* Then no error has been sent yet, so it's okay to reset
		    * the output info (file name and number of outputs). *)
		   let val b'  = sanitizer unifenv envs outputop prog prt
		       val msg = if b'
				 then "program is typable and sane"
				 else "program is typable but not sane"
		       val _   = genFinished (finishedLispMessage1 msg)
		       val _   = if prt2
				 then if b'
				      then print "[typable and sane]\n"
				      else print "[typable but not sane]\n"
				 else ()
		   in (prog, env, b')
		   end
		 | ERROR error => raise Fail "the piece of code should be typable"
	    end
       else let val msg = if b
			  then "Program is typable"
			  else "Program is not typable and therefore not sane"
		val _   = if prt2 then print ("\n" ^ msg ^ ".\n") else ()
		val _   = genFinished (finishedLispMessage1 msg)
	    in (prog, env, b)
	    end
    end
    handle Fail msg => (genFinished (finishedLispMessage2 msg); raise Fail msg)
	 | error => (genFinished (finishedLispMessage2 ""); raise error)

end
