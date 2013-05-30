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
 *  o File name:   CsGeneration.sml
 *  o Description: Constraint generator.
 *)


structure Gen :> GEN = struct

structure A  = Ast
structure P  = Parser
structure R  = Reg
structure D  = Deps
structure E  = Env
structure PR = Prelude


(* ------ USEFUL FUNCTION USED BY THE CONSTRAINT GENERATOR ------ *)

(*structure TVAR = SplayMapFn(type ord_key = string val compare = String.compare)*)

fun locCst env = E.ENVCST (E.CSENV (E.envvarFresh, env))

fun getExplicitTyVars' (A.N {kind = (A.ID, A.ID_TYVAR), label, value, regions, parents, children}) = [(value, label)]
  | getExplicitTyVars' (A.N {kind = (A.BIND, _), label, value, regions, parents, children}) = []
  | getExplicitTyVars' (A.N {kind, label, value, regions, parents, children}) = foldr (fn (term, list) => (getExplicitTyVars' term) @ list) [] children

fun getExplicitTyVars (A.N {kind, label, value, regions, parents, children}) =
    foldr (fn (term, list) => (getExplicitTyVars' term) @ list) [] children

fun diffExplicitTyVars [] _ = []
  | diffExplicitTyVars ((value, label) :: xs) etyvars =
    if List.exists (fn (v : E.id, l) => v = value) etyvars
    then diffExplicitTyVars xs etyvars
    else (value, label) :: (diffExplicitTyVars xs etyvars)

fun bindExplicitTyVarsGen etyvars kind =
    foldr (fn ((etyvar, label), envs) =>
	      let val bind = E.BINDTYV (etyvar, label, (E.mk_new_tyvar (), kind))
		  val env  = E.mk_env_depbin bind label
	      in env :: envs
	      end)
	  []
	  etyvars

fun bindExplicitTyVars terms kind =
    let val etyvars = List.concat (List.map getExplicitTyVars terms)
	val envs    = bindExplicitTyVarsGen etyvars kind
    in E.ENVSET envs
    end

fun bindExplicitTyVars' terms (tv, term2) kind =
    let val etyvars1 = List.concat (map getExplicitTyVars terms)
	val etyvar   = getExplicitTyVars tv
	val etyvars2 = getExplicitTyVars term2
	val etyvars  = (diffExplicitTyVars etyvars1 etyvar) @ etyvars2
	val envs     = bindExplicitTyVarsGen etyvars kind
    in E.ENVSET envs
    end

fun combineTyvars list1 list2 =
    let fun isin _ [] = (NONE, [])
	  | isin value ((value' : string, labels) :: list) =
	    if value = value'
	    then (SOME labels, list)
	    else let val (opl, list') = isin value list
		 in (opl, (value', labels) :: list')
		 end
    in foldr (fn ((value, labels), list) =>
		 case isin value list of
		     (NONE, _) => (value, labels) :: list
		   | (SOME labels', list') => (value, labels @ labels') :: list)
	     list1
	     list2
    end

(* Extracts all the type variables occurring in a term *)
fun getTyvarsTerm (A.N {kind = (A.ID, A.ID_TYVAR), label, value, regions, parents, children = []}) =
    [(value, [label])]
  | getTyvarsTerm (A.N {kind, label, value, regions, parents, children}) =
    getTyvarsTermList children

and getTyvarsTermList terms =
    foldr (fn (term, list) => combineTyvars (getTyvarsTerm term) list) [] terms

fun getTwoBindTyVars term label =
    let fun getPairs [] = []
	  | getPairs (x :: xs) = (map (fn y => (x, y)) xs) @ (getPairs xs)
	val list = getTyvarsTerm term
	val lists = map (fn (tv, labels) =>
			    let val pairs = getPairs labels
			    in map (fn (lab1, lab2) =>
				       let val env1 = E.ENVERR ("type variable " ^ tv ^ " occurs twice" , E.SYNTAX)
					   val env2 = E.ENVDEP (env1, D.mk_dep lab1)
					   val env3 = E.ENVDEP (env2, D.mk_dep lab2)
				       in E.ENVDEP (env3, D.mk_dep label)
				       end)
				   pairs
			    end)
			list
    in E.list2env (List.concat lists)
    end

(* Returns errors environment for non variables in patterns *)
fun onlyVariables (A.N {kind = (A.ATPAT, A.ATPAT_LIST), label, value, regions, parents, children})      = E.ENVDEP (E.ENVERR ("lists are not allowed in patterns in bindings",        E.SYNTAX), D.mk_dep label)
  | onlyVariables (A.N {kind = (A.PAT,   A.PAT_APP),    label, value, regions, parents, children})      = E.ENVDEP (E.ENVERR ("applications are not allowed in patterns in bindings", E.SYNTAX), D.mk_dep label)
  | onlyVariables (A.N {kind = (A.PAT,   A.PAT_CONS),   label, value, regions, parents, children})      = E.ENVDEP (E.ENVERR ("lists are not allowed in patterns in bindings",        E.SYNTAX), D.mk_dep label)
  | onlyVariables (A.N {kind = (A.SCON,  _),            label, value, regions, parents, children = []}) = E.ENVDEP (E.ENVERR ("constants are not allowed in patterns in bindings",    E.SYNTAX), D.mk_dep label)
  | onlyVariables (A.N {kind = (A.ID,    A.ID_VID),     label, value, regions, parents, children = []}) =
    if List.exists (fn x => x = value) A.constructors
    then E.ENVDEP (E.ENVERR (value ^ " is not allowed in patterns in bindings",  E.SYNTAX), D.mk_dep label)
    else E.ENVNUL
  | onlyVariables (A.N {kind, label, value, regions, parents, children}) = E.list2env (map onlyVariables children)

(* Checks that the bind passed as argument is a function.
 * This is used in the case of recursive definitions. *)
fun isBindFunction (A.N {kind = (A.BIND, A.BIND_DEC),  label, value, regions, parents, children = [f, args, exp]}) _ =
    (case args of
	 A.N {kind = (A.PARAM, A.PARAM_P), label = label', value, regions, parents, children = []} => raise Fail "function does not have parameters"
       | _ => E.ENVNUL)
  | isBindFunction (A.N {kind = (A.BIND, A.BIND_TDEC), label, value, regions, parents, children = [f, args, typ, exp]}) _ =
    (case args of
	 A.N {kind = (A.PARAM, A.PARAM_P), label = label', value, regions, parents, children = []} => raise Fail "function does not have parameters"
       | _ => E.ENVNUL)
  | isBindFunction (A.N {kind = (A.BIND, A.BIND_PAT),  label, value, regions, parents, children = [pat, exp]}) lab =
    let val err  = "recursive definition is neither a function nor a simple recursive combinator"
	val env1 = E.ENVERR (err, E.SYNTAX)
	val env2 = E.ENVDEP (env1, D.mk_dep label)
	val env3 = E.ENVDEP (env2, D.mk_dep lab)

	fun isLambda (A.N {kind = (A.EXP, A.EXP_LAMBDA), label, value, regions, parents, children = [pat, exp]}) _ =
	    E.ENVNUL
	  | isLambda (A.N {kind = (A.EXP,   A.EXP_ATEXP),   label, value, regions, parents, children = [atexp]}) env = isLambda atexp (E.ENVDEP (env, D.mk_dep label))
	  | isLambda (A.N {kind = (A.ATEXP, A.ATEXP_PAREN), label, value, regions, parents, children = [exp]})   env = isLambda exp   (E.ENVDEP (env, D.mk_dep label))
	  | isLambda term env = E.ENVDEP (env, D.mk_dep (A.getLabel term))

	fun isId id (A.N {kind = (A.ATEXP, A.ATEXP_ID), label, value, regions, parents, children = [ident]}) env =
	    if A.getValue ident = id
	    then E.ENVNUL
	    else E.ENVDEP (E.ENVDEP (env, D.mk_dep label), D.mk_dep (A.getLabel ident))
	  | isId id (A.N {kind = (A.EXP,   A.EXP_ATEXP),   label, value, regions, parents, children = [atexp]}) env = isId id atexp (E.ENVDEP (env, D.mk_dep label))
	  | isId id (A.N {kind = (A.ATEXP, A.ATEXP_PAREN), label, value, regions, parents, children = [exp]})   env = isId id exp   (E.ENVDEP (env, D.mk_dep label))
	  | isId id term env = E.ENVDEP (env, D.mk_dep (A.getLabel term))

	fun isLastCompPrior id (A.N {kind = (A.ATEXP, A.ATEXP_PRIOR), label, value, regions, parents, children = [exp]}) env =
	    let val env1 = E.ENVDEP (env, D.mk_dep label)
	    in isId id exp env1
	    end
	  | isLastCompPrior id exp env = E.ENVDEP (env, D.mk_dep (A.getLabel exp))

	fun isCompPrior id (term as A.N {kind = (A.EXP, A.EXP_COMP), label, value, regions, parents, children}) env =
	    let val env1 = E.ENVDEP (env, D.mk_dep label)
	    in if String.isSubstring "P" value
	       then case A.isFreeVid id term of
			NONE => env2
		      | SOME deps => E.applyDepsEnv env1 deps
	       else case rev children of
			(exp :: exps) =>
			let val env2 = isLastCompPrior id exp env1
			    val dexp = A.mk_new_dum_term A.EXP_COMP "" [] exps
			in if E.isNullEnv env2
			   then case A.isFreeVid id dexp of
				    NONE => env2
				  | SOME deps => E.applyDepsEnv env1 deps
			   else env2
			end
		      | [] => env1
	    end
	  | isCompPrior id (A.N {kind = (A.EXP,   A.EXP_ATEXP),   label, value, regions, parents, children = [atexp]}) env = isCompPrior id atexp (E.ENVDEP (env, D.mk_dep label))
	  | isCompPrior id (A.N {kind = (A.ATEXP, A.ATEXP_PAREN), label, value, regions, parents, children = [exp]})   env = isCompPrior id exp   (E.ENVDEP (env, D.mk_dep label))
	  | isCompPrior id (A.N {kind = (A.ATEXP, A.ATEXP_ID),    label, value, regions, parents, children = [ident]}) env = isCompPrior id ident (E.ENVDEP (env, D.mk_dep label))
	  | isCompPrior id (A.N {kind = (A.ATEXP, A.ATEXP_SCON),  label, value, regions, parents, children = [scon]})  env = isCompPrior id scon  (E.ENVDEP (env, D.mk_dep label))
	  | isCompPrior id term env = E.ENVDEP (env, D.mk_dep (A.getLabel term))

	fun isIdPat (A.N {kind = (A.PAT, A.PAT_ATPAT), label, value, regions, parents, children = [atpat]}) env =
	    isIdPat atpat (E.ENVDEP (env, D.mk_dep label))
	  | isIdPat (A.N {kind = (A.ATPAT, A.ATPAT_ID), label, value, regions, parents, children = [id]}) env =
	    let val env1 = E.ENVDEP (env, D.mk_dep label)
		val env2 = E.ENVDEP (env1, D.mk_dep (A.getLabel id))
		val id   = A.getValue id
		val env3 = isLambda exp env2
	    in if E.isNullEnv env3
	       then env3
	       else isCompPrior id exp env3
	    end
	  | isIdPat (A.N {kind = (A.ATPAT, A.ATPAT_PAREN), label, value, regions, parents, children = [pat]}) env =
	    isIdPat pat (E.ENVDEP (env, D.mk_dep label))
	  | isIdPat (A.N {kind, label, value, regions, parents, children}) env = E.ENVDEP (env, D.mk_dep label)

    in isIdPat pat env3
    end
  | isBindFunction _ _ = E.ENVNUL

fun sanityCheckOverloading tyvarseq tyseqset label =
    let val msgSize = "In an overloading statement, there should be as many types in each type sequence as they are type variables in the type variable sequence"
	val msgVar  = "In an overloading statement, in each type sequence, each type should be a type construction"

	exception typevar of D.deps

	(* extracts the top type constructor of a type term *)
	fun getTyCon (A.N {kind = (A.TYPE, A.TYPE_TYCON), label, value, regions, parents, children = [tn, ts]}) =
	    (case tn of
		 A.N {kind = (A.ID, A.ID_TYCON), label = lab, value, regions, parents, children} =>
		 (value, D.add (D.singleton (D.mk_dep lab), D.mk_dep label))
	       | _ => raise Fail "unexpected term")
	  | getTyCon (A.N {kind = (A.TYPE, A.TYPE_ARROW),  label, value, regions, parents, children}) = ("->", D.singleton (D.mk_dep label))
	  | getTyCon (A.N {kind = (A.TYPE, A.TYPE_DARROW), label, value, regions, parents, children}) = ("->", D.singleton (D.mk_dep label))
	  | getTyCon (A.N {kind = (A.TYPE, A.TYPE_DISJU),  label, value, regions, parents, children}) = ("+",  D.singleton (D.mk_dep label))
	  | getTyCon (A.N {kind = (A.TYPE, A.TYPE_TUPLE),  label, value, regions, parents, children}) = ("*",  D.singleton (D.mk_dep label))
	  | getTyCon (A.N {kind = (A.TYPE, A.TYPE_VAR),    label, value, regions, parents, children = [tv]}) =
	    let val lab1 = A.getLabel tv
		val lab2 = case A.getChildren tv of
			       [id] => A.getLabel id
			     | _    => raise Fail "unexpected term"
	    in raise typevar (D.add (D.add (D.singleton (D.mk_dep label), D.mk_dep lab1), D.mk_dep lab2))
	    end
	  | getTyCon (A.N {kind = (A.TYPE, A.TYPE_PAREN), label, value, regions, parents, children = [ty]}) =
	    (let val (tc, deps) = getTyCon ty
	     in (tc, D.add (deps, D.mk_dep label))
	     end handle typevar deps => raise typevar (D.add (deps, D.mk_dep label)))
	  | getTyCon (A.N {kind = (A.TYPE, A.TYPE_DEP),   label, value, regions, parents, children = [id, ty]}) =
	    (let val (tc, deps) = getTyCon ty
	     in (tc, D.add (deps, D.mk_dep label))
	     end handle typevar deps => raise typevar (D.add (deps, D.mk_dep label)))
	  | getTyCon (A.N {kind = (A.TYPE, A.TYPE_SET),   label, value, regions, parents, children = [id, ty, exp]}) =
	    (let val (tc, deps) = getTyCon ty
	     in (tc, D.add (deps, D.mk_dep label))
	     end handle typevar deps => raise typevar (D.add (deps, D.mk_dep label)))
	  | getTyCon _ = raise Fail "unexpected term"

	fun isin _ [] = NONE
	  | isin tycon ((tycon' : string, deps) :: list) =
	    if tycon = tycon'
	    then SOME deps
	    else isin tycon list

	(* The length of tyvarseq *)
	val (size, deps) =
	    case tyvarseq of
		A.N {kind = (A.TYPEVARSEQ, A.TYPEVARSEQ_ONE), label, value, regions, parents, children} =>
		(1, D.singleton (D.mk_dep label))
	      | A.N {kind = (A.TYPEVARSEQ, A.TYPEVARSEQ_SEQ), label, value, regions, parents, children} =>
		(List.length children, D.singleton (D.mk_dep label))
	      | _ => raise Fail "unexpected term"

	(* The lengths of the sequences in tyseqset *)
	val sizes =
	    case tyseqset of
		A.N {kind = (A.TYPESEQSET, A.TYPESEQSET_SET), label = lab0, value, regions, parents, children} =>
		map (let fun check (A.N {kind = (A.TYPESEQ, A.TYPESEQ_ONE), label, value, regions, parents, children}) lab1 =
			     (1, D.addListLab D.empty [label, lab0, lab1])
			   | check (A.N {kind = (A.TYPESEQ, A.TYPESEQ_SEQ), label, value, regions, parents, children}) lab1 =
			     (List.length children, D.addListLab D.empty [label, lab0, lab1])
			   | check _ _ = raise Fail "unexpected term"
		     in fn A.N {kind = (A.OTYPESEQ, A.OTYPESEQ_UNM), label = lab1, value, regions, parents, children = [typeseq]} => check typeseq lab1
			 | A.N {kind = (A.OTYPESEQ, A.OTYPESEQ_NAM), label = lab1, value, regions, parents, children = [typeseq]} => check typeseq lab1
			 | _ => raise Fail "unexpected term"
		     end)
		    children
	      | _ => raise Fail "unexpected term"

	(* NOTE: here we check that the sequences are all the same size *)
	val env1 = foldr (fn ((size1, deps1), env) =>
			     if size1 = size
			     then env
			     else let val deps2  = D.add (D.union (deps1, deps), D.mk_dep label)
				      val enverr = E.ENVERR (msgSize, E.SYNTAX)
				  in E.ENVAPP (E.applyDepsEnv enverr deps2, env)
				 end)
			 E.ENVNUL
			 sizes

	(* We extract the type constructors of the type sequences of the overloading statement *)
	val (tycons, env2) =
	    let val tycons =
		    case tyseqset of
			A.N {kind = (A.TYPESEQSET, A.TYPESEQSET_SET), label = lab0, value, regions, parents, children} =>
			map (let fun check (A.N {kind = (A.TYPESEQ, A.TYPESEQ_ONE), label, value, regions, parents, children = [ty]}) lab1 =
				     (let val (tc, deps) = getTyCon ty
				      in [(tc, D.add (deps, D.mk_dep label))]
				      end handle typevar deps => raise typevar (D.addListLab deps [label, lab0, lab1]))
				   | check (A.N {kind = (A.TYPESEQ, A.TYPESEQ_SEQ), label, value, regions, parents, children}) lab1 =
				     ((map (fn ty =>
					       let val (tc, deps) = getTyCon ty
					       in (tc, D.addListLab deps [label, lab0, lab1])
					       end)
					   children)
				      handle typevar deps => raise typevar (D.addListLab deps [label, lab0, lab1]))
				   | check _ _ = raise Fail "unexpected term"
			     in fn A.N {kind = (A.OTYPESEQ, A.OTYPESEQ_UNM), label = lab1, value, regions, parents, children = [typeseq]} => check typeseq lab1
				 | A.N {kind = (A.OTYPESEQ, A.OTYPESEQ_NAM), label = lab1, value, regions, parents, children = [typeseq]} => check typeseq lab1
				 | _ => raise Fail "unexpected term"
			     end)
			    children
		      | _ => raise Fail "unexpected term"
	    in (tycons, E.ENVNUL)
	    end handle typevar deps =>
		       let val enverr = E.ENVERR (msgVar, E.SYNTAX)
			   val deps0  = D.add (deps, D.mk_dep label)
		       in ([[]], E.applyDepsEnv enverr deps0)
		       end

	val init  = case tycons of
			(list :: _) => List.tabulate (List.length list, fn _ => [])
		      | _ => raise Fail "term is not an overloading type sequence"

	val list1 = foldr (fn (list, lists) =>
			      let val pairs = ListPair.zip (list, lists)
			      in map (fn (x, xs) => x :: xs) pairs
			      end)
			  init
			  tycons

	(* Here we check that each overloaded type variable is not overloded twice to the same type construct.
	 * For that we only check the top type constructor. *)
	val env3 = foldr (fn (tycons, env) =>
			     #2 (foldr (fn ((tycon, deps), (tycons, env)) =>
					   let val env' =
						   case isin tycon tycons of
						       SOME deps' =>
						       let val enverr = E.ENVOVL tycon
							   val deps0  = D.add (D.union (deps, deps'), D.mk_dep label)
						       in E.ENVAPP (E.applyDepsEnv enverr deps0, env)
						       end
						     | NONE => env
					   in ((tycon, deps) :: tycons, env')
					   end)
				       ([], env)
				       tycons))
			 E.ENVNUL
			 list1

    in E.list2env [env1, env2, env3]
    end

fun dSelect _ [] = []
  | dSelect str1 ((str2 : string, deps) :: list) =
    if str1 = str2
    then deps :: (dSelect str1 list)
    else dSelect str1 list

type defs =
     {params  : (string * D.deps) list,
      cons    : (string * D.deps) list,
      headers : (string * D.deps) list,
      decs    : (string * D.deps) list,
      ptyp    : (string * E.envvar) list,
      spec    : string option,
      file    : string option,
      export  : (string * E.envvar * (string * D.deps * E.envvar option) list) list}

val initDefs : defs =
    {params  = [],
     cons    = [],
     headers = [],
     decs    = [],
     ptyp    = [],
     spec    = NONE,
     file    = NONE,
     export  = []}

fun updDefsParams  {params = _, cons, headers, decs, ptyp, spec, file, export} params  = {params = params, cons = cons, headers = headers, decs = decs, ptyp = ptyp, spec = spec, file = file, export = export}
fun updDefsCons    {params, cons = _, headers, decs, ptyp, spec, file, export} cons    = {params = params, cons = cons, headers = headers, decs = decs, ptyp = ptyp, spec = spec, file = file, export = export}
fun updDefsHeaders {params, cons, headers = _, decs, ptyp, spec, file, export} headers = {params = params, cons = cons, headers = headers, decs = decs, ptyp = ptyp, spec = spec, file = file, export = export}
fun updDefsDecs    {params, cons, headers, decs = _, ptyp, spec, file, export} decs    = {params = params, cons = cons, headers = headers, decs = decs, ptyp = ptyp, spec = spec, file = file, export = export}
fun updDefsPtyp    {params, cons, headers, decs, ptyp = _, spec, file, export} ptyp    = {params = params, cons = cons, headers = headers, decs = decs, ptyp = ptyp, spec = spec, file = file, export = export}
fun updDefsSpec    {params, cons, headers, decs, ptyp, spec = _, file, export} spec    = {params = params, cons = cons, headers = headers, decs = decs, ptyp = ptyp, spec = spec, file = file, export = export}
fun updDefsFile    {params, cons, headers, decs, ptyp, spec, file = _, export} file    = {params = params, cons = cons, headers = headers, decs = decs, ptyp = ptyp, spec = spec, file = file, export = export}
fun updDefsExport  {params, cons, headers, decs, ptyp, spec, file, export = _} export  = {params = params, cons = cons, headers = headers, decs = decs, ptyp = ptyp, spec = spec, file = file, export = export}

fun newDefsParams  params  = updDefsParams  initDefs params
fun newDefsCons    cons    = updDefsCons    initDefs cons
fun newDefsHeaders headers = updDefsHeaders initDefs headers
fun newDefsDecs    decs    = updDefsDecs    initDefs decs
fun newDefsPtyp    ptyp    = updDefsPtyp    initDefs ptyp
fun newDefsSpec    spec    = updDefsSpec    initDefs (SOME spec)
fun newDefsFile    file    = updDefsFile    initDefs (SOME file)
fun newDefsExport  export  = updDefsExport  initDefs export

fun mergeDefs {params = params1, cons = cons1, headers = headers1, decs = decs1, ptyp = ptyp1, spec = spec1, file = file1, export = export1}
	      {params = params2, cons = cons2, headers = headers2, decs = decs2, ptyp = ptyp2, spec = spec2, file = file2, export = export2} =
    {params  = params1  @ params2,
     cons    = cons1    @ cons2,
     headers = headers1 @ headers2,
     decs    = decs1    @ decs2,
     ptyp    = ptyp1    @ ptyp2,
     spec    = case spec2 of SOME _ => spec2 | NONE => spec1,
     file    = case file2 of SOME _ => file2 | NONE => file1,
     export  = export1  @ export2}

fun getDefsParams  (defs : defs) = #params  defs
fun getDefsCons    (defs : defs) = #cons    defs
fun getDefsHeaders (defs : defs) = #headers defs
fun getDefsDecs    (defs : defs) = #decs    defs
fun getDefsPtyp    (defs : defs) = #ptyp    defs
fun getDefsSpec    (defs : defs) = #spec    defs
fun getDefsFile    (defs : defs) = #file    defs
fun getDefsExport  (defs : defs) = #export  defs

fun findDefsExport str defs =
    let val exports = getDefsExport defs
	val _ = print ("[exports: " ^ Int.toString (List.length exports) ^ "]\n")
    in List.find
	   (fn (file, envvar, params) => file = str)
	   exports
    end

fun computeParamsExport defs =
    let val params = getDefsParams defs
	val ptyps  = getDefsPtyp defs
    in map (fn (id, deps) =>
	       case List.find (fn (i,_) => id = i) ptyps of
		   SOME (_, ev) => (id, deps, SOME ev)
		 | NONE => (id, deps, NONE))
	   params
    end

fun checkMsgsNotRedefined defs new_msgs =
    let val old_msgs = getDefsHeaders defs
	fun msgHdr hdr = "Header " ^ hdr ^ " declared twice"
	val envs =
	    #1 (foldr (fn ((hdr, deps), (envs, news)) =>
			  let val deps_list = dSelect hdr (old_msgs @ news)
			      val envs' =
				  foldr (fn (deps', envs) =>
					    let val enverr = E.ENVERR (msgHdr hdr, E.SYNTAX)
						val deps0  = D.union (deps, deps')
						val env    = E.applyDepsEnv enverr deps0
					    in env :: envs
					    end)
					envs
					deps_list
			  in (envs', (hdr, deps) :: news)
			  end)
		      ([], [])
		      new_msgs)
    in E.list2env envs
    end

fun checkParameterNotRedefined defs new_params =
    let val old_params = getDefsParams defs
	fun msgParam param = "Parameter " ^ param ^ " declared twice"
	val envs =
	    #1 (foldr (fn ((param, deps), (envs, news)) =>
			  let val deps_list = dSelect param (old_params @ news)
			      val envs' =
				  foldr (fn (deps', envs) =>
					    let val enverr = E.ENVERR (msgParam param, E.SYNTAX)
						val deps0  = D.union (deps, deps')
						val env    = E.applyDepsEnv enverr deps0
					    in env :: envs
					    end)
					envs
					deps_list
			  in (envs', (param, deps) :: news)
			  end)
		      ([], [])
		      new_params)
    in E.list2env envs
    end

fun checkRebindTopDec lab defs term =
    let val params = getDefsParams defs
	val cons   = getDefsCons   defs
	fun treatId i label =
	    let fun isin _ [] = []
		  | isin id0 ((id : string, deps) :: list) =
		    if id0 = id
		    then deps :: (isin id0 list)
		    else isin id0 list
		val id   = A.getIdIdent i
		val lab' = A.getLabel i
		val msg  = "The identifer " ^ id ^ " is already defined as a "
		val cmsg = msg ^ "constant"
		val pmsg = msg ^ "parameter"
		fun genErrors list msg =
		    map (fn deps0 =>
			    let val deps1  = D.addListLab deps0 [lab, label, lab']
				val enverr = E.ENVERR (msg, E.SYNTAX)
			    in E.applyDepsEnv enverr deps1
			    end)
			list
		val deps_list_p = isin id params
		val deps_list_c = isin id cons
		val errs1       = genErrors deps_list_p pmsg
		val errs2       = genErrors deps_list_c cmsg
	    in errs1 @ errs2
	    end
	fun getListIds (term as A.N {kind = (A.ID, A.ID_VID), label, value, regions, parents, children}) = [term]
	  | getListIds (A.N {kind, label, value, regions, parents, children}) = List.concat (map getListIds children)
	val errors =
	    case term of
		A.N {kind = (A.BIND, A.BIND_DEC),  label, value, regions, parents, children = [f, _, _]}       => treatId f label
	      | A.N {kind = (A.BIND, A.BIND_TDEC), label, value, regions, parents, children = [f, _, _, _]}    => treatId f label
	      | A.N {kind = (A.BIND, A.BIND_PAT),  label, value, regions, parents, children = [p, _]}          => List.concat (map (fn id => treatId id label) (getListIds p))
	      | A.N {kind = (A.BIND, A.BIND_IOP),  label, value, regions, parents, children = [i, _, _, _]}    => treatId i label
	      | A.N {kind = (A.BIND, A.BIND_TIOP), label, value, regions, parents, children = [i, _, _, _, _]} => treatId i label
	      | A.N {kind = (A.ID,   A.ID_VID),    label, value, regions, parents, children}                   => treatId term label
	      | _ => raise Fail "term is not a binding"
    in E.list2env errors
    end

datatype match_kind = LIST of bool * bool
		    | INJ  of bool * bool
		    | PAT
		    | WILD
		    | ERR  of string

datatype m_kind = MK_LIST of (m_kind * m_kind) option
		| MK_INJ  of m_kind option * m_kind option
		| MK_TUP  of m_kind list
		| MK_VAR
		| MK_WILD
		| MK_ERR  of string

fun checkCasePatSyntax' (A.N {kind = (A.MATCH, A.MATCH_M), label = lab1, value, regions, parents, children}) =
    let fun structMrules (A.N {kind = (A.MRULE, A.MRULE_M), label = labM, value, regions, parents, children = [casepat, _]}) =
	    let val (mkind, deps) = structCasePat casepat
	    in (mkind, D.add (deps, D.mk_dep labM))
	    end
	  | structMrules _ = raise Fail "term is not matching rule"

	and structCasePat (A.N {kind = (A.CASEPAT, A.CASEPAT_PAT), label, value, regions, parents, children = [pat]}) =
	    let val (mkind, deps) = structPat pat
	    in (mkind, D.addListLab deps [label])
	    end
	  | structCasePat _ = raise Fail "term is not casepat"

	and structPat (A.N {kind = (A.ATPAT, A.ATPAT_ID), label, value, regions, parents, children = [id]}) =
	    (MK_VAR, D.addListLab D.empty [label, A.getLabel id])
	  | structPat (A.N {kind = (A.ATPAT, A.ATPAT_WILD), label, value, regions, parents, children = []}) =
	    (MK_WILD, D.mk_deps label)
	  | structPat (A.N {kind = (A.ATPAT, A.ATPAT_SCON), label, value, regions, parents, children = [sc]}) = raise Fail "checkCasePatSyntax':ATPAT_SCON"
	  | structPat (A.N {kind = (A.ATPAT, A.ATPAT_LIST), label, value, regions, parents, children}) = raise Fail "checkCasePatSyntax':ATPAT_LIST"
	  | structPat (A.N {kind = (A.ATPAT, A.ATPAT_PAREN), label, value, regions, parents, children = [pat]}) =
	    let val (mkind, deps) = structPat pat
	    in (mkind, D.add (deps, D.mk_dep label))
	    end
	  | structPat (A.N {kind = (A.ATPAT, A.ATPAT_STRUC), label, value, regions, parents, children = [id]}) =
	    (MK_VAR, D.addListLab D.empty [label, A.getLabel id])
	  | structPat (A.N {kind = (A.ATPAT, A.ATPAT_TUPLE), label, value, regions, parents, children}) = raise Fail "checkCasePatSyntax':ATPAT_TUPLE"
	  | structPat (A.N {kind = (A.PAT, A.PAT_TYPED), label, value, regions, parents, children = [pat, typ]}) =
	    let val (mkind, deps) = structPat pat
	    in (mkind, D.add (deps, D.mk_dep label))
	    end
	  | structPat (A.N {kind = (A.PAT, A.PAT_AS), label, value, regions, parents, children = [ident, pat]}) =
	    let val (mkind, deps) = structPat pat
	    in (mkind, D.add (deps, D.mk_dep label))
	    end
	  | structPat (A.N {kind = (A.PAT, A.PAT_CONS), label, value, regions, parents, children = [pat1, pat2]}) =
	    let val (mkind1, deps1) = structPat pat1
		val (mkind2, deps2) = structPat pat2
	    in (MK_LIST (SOME (mkind1, mkind2)), D.add (D.union (deps1, deps2), D.mk_dep label))
	    end
	  | structPat (A.N {kind = (A.PAT, A.PAT_ATPAT), label, value, regions, parents, children = [atpat]}) =
	    let val (mkind, deps) = structPat atpat
	    in (mkind, D.add (deps, D.mk_dep label))
	    end
	  | structPat (A.N {kind = (A.PAT, A.PAT_APP), label, value, regions, parents, children = [id, atpat]}) = raise Fail "checkCasePatSyntax':ATPAT_APP"
	  | structPat _ = raise Fail "term is not a pattern"
    in raise Fail "checkCasePatSyntax':structPat"
    end
  | checkCasePatSyntax' _ = raise Fail "term is not matching term"

fun checkCasePatSyntax (A.N {kind = (A.MATCH, A.MATCH_M), label = lab1, value, regions, parents, children}) label tv labE =
    let fun whatKindMrule (A.N {kind = (A.MRULE, A.MRULE_M), label = labM, value, regions, parents, children = [casepat, _]}) =
	    (case casepat of
		 A.N {kind = (A.CASEPAT, A.CASEPAT_PAT),    label = labC, value, regions, parents, children = [pat]} =>
		 let fun whatPat (A.N {kind = (A.PAT, A.PAT_CONS), label = labO, value, regions, parents, children = [pat1, pat2]}) deps =
			 (case (pat1, pat2) of
			      (A.N {kind = (A.PAT, A.PAT_ATPAT), label = labO1, value = _, regions = _, parents = _, children = [atpat1]},
			       A.N {kind = (A.PAT, A.PAT_ATPAT), label = labO2, value = _, regions = _, parents = _, children = [atpat2]}) =>
			      (case (atpat1, atpat2) of
				   (A.N {kind = (A.ATPAT, A.ATPAT_ID), label = labA1, value = _, regions = _, parents = _, children = [id1]},
				    A.N {kind = (A.ATPAT, A.ATPAT_ID), label = labA2, value = _, regions = _, parents = _, children = [id2]}) =>
				   let val labI1 = A.getLabel id1
				       val labI2 = A.getLabel id2
				in (LIST (false, true), D.addListLab deps [labO, labO1, labO2, labA1, labA2, labI1, labI2])
				   end
				 | _ => (PAT, D.addListLab deps [labO, labO1, labO2, A.getLabel atpat1, A.getLabel atpat2]))
			    | _ => (PAT, D.addListLab deps [labO, A.getLabel pat1, A.getLabel pat2]))
		       | whatPat (A.N {kind = (A.PAT, A.PAT_ATPAT), label = labP, value, regions, parents, children = [atpat]}) deps =
			 (case atpat of
			      A.N {kind = (A.ATPAT, A.ATPAT_ID), label = labI, value, regions, parents, children = [id]} =>
			      if A.getValue id = A.id_list_nil
			      then (LIST (true, false), D.addListLab deps [labP, labI, A.getLabel id])
			      else (PAT, D.addListLab deps [labP, labI, A.getLabel id])
			    | A.N {kind = (A.ATPAT, A.ATPAT_LIST), label = labL, value, regions, parents, children = []} =>
			      (LIST (true, false), D.addListLab deps [labP, labL])
			    | A.N {kind = (A.ATPAT, A.ATPAT_WILD), label = labW, value, regions, parents, children = []} =>
			      (WILD, D.addListLab deps [labP, labW])
			    | _ => (PAT, D.addListLab deps [labP, A.getLabel atpat]))
		       | whatPat (A.N {kind = (A.PAT, A.PAT_APP), label = labP, value, regions, parents, children = [id, atpat]}) deps =
			 let val v = A.getValue id
			 in if v = A.id_disju_inl
			    then (INJ (true, false), D.addListLab deps [labP, A.getLabel id])
			    else if v = A.id_disju_inr
			    then (INJ (false, true), D.addListLab deps [labP, A.getLabel id])
			    else (PAT, D.addListLab deps [labP, A.getLabel id])
			 end
		       | whatPat (A.N {kind = (A.ATPAT, A.ATPAT_PAREN), label = labP, value, regions, parents, children = [pat]}) deps =
			 whatPat pat (D.add (deps, D.mk_dep labP))
		       | whatPat pat deps = (PAT, D.addListLab deps [A.getLabel pat])
		 in whatPat pat (D.addListLab D.empty [labM, labC])
		 end
	       | _ => raise Fail "checkCasePatSyntax")
	  | whatKindMrule _ = raise Fail "checkCasePatSyntax"
	fun whatKindMrules mrules =
	    foldl (fn (mrule, SOME (LIST (b1, b2), deps0)) =>
		      let val (kind, deps) = whatKindMrule mrule
		      in case kind of
			     LIST (b1', b2') =>
			     (case ((b1, b2), (b1', b2')) of
				  ((true, _), (true, _)) => SOME (ERR "redundant case", D.union (deps0, deps))
				| ((_, true), (_, true)) => SOME (ERR "redundant case", D.union (deps0, deps))
				| _ => SOME (LIST (b1 orelse b1', b2 orelse b2'), D.union (deps0, deps)))
			   | INJ _ => SOME (ERR "list and injection cases cannot be mixed", D.union (deps0, deps))
			   | PAT  => SOME (PAT, D.union (deps0, deps))
			   | WILD =>
			     if b1 andalso b2
			     then SOME (ERR "redundant case", D.union (deps0, deps))
			     else SOME (WILD, D.union (deps0, deps))
			   | ERR _ => raise Fail "whatKindMrules"
		      end
		    | (mrule, SOME (INJ (b1, b2), deps0)) =>
		      let val (kind, deps) = whatKindMrule mrule
		      in case kind of
			     LIST _ => SOME (ERR "list and injection cases cannot be mixed", D.union (deps0, deps))
			   | INJ (b1', b2') =>
			     (case ((b1, b2), (b1', b2')) of
				  ((true, _), (true, _)) => SOME (ERR "redundant case", D.union (deps0, deps))
				| ((_, true), (_, true)) => SOME (ERR "redundant case", D.union (deps0, deps))
				| _ => SOME (LIST (b1 orelse b1', b2 orelse b2'), D.union (deps0, deps)))
			   | PAT  => SOME (PAT, D.union (deps0, deps))
			   | WILD =>
			     if b1 andalso b2
			     then SOME (ERR "redundant case", D.union (deps0, deps))
			     else SOME (WILD, D.union (deps0, deps))
			   | ERR _ => raise Fail "whatKindMrules"
		      end
		    | (mrule, SOME (PAT, deps0)) =>
		      let val (kind, deps) = whatKindMrule mrule
		      in case kind of
			     LIST _ => SOME (PAT, D.union (deps, deps0))
			   | INJ  _ => SOME (PAT, D.union (deps, deps0))
			   | PAT    => SOME (PAT, D.union (deps, deps0))
			   | WILD   => SOME (WILD, D.union (deps, deps0))
			   | ERR _  => raise Fail "whatKindMrules"
		      end
		    | (mrule, SOME (WILD, deps0)) =>
		      let val (kind, deps) = whatKindMrule mrule
		      in case kind of
			     LIST _ => SOME (ERR "a wild branch can only be the last branch", D.union (deps0, deps))
			   | INJ  _ => SOME (ERR "a wild branch can only be the last branch", D.union (deps0, deps))
			   | PAT    => SOME (ERR "a wild branch can only be the last branch", D.union (deps0, deps))
			   | WILD   => SOME (ERR "a wild branch can only be the last branch", D.union (deps0, deps))
			   | ERR _  => raise Fail "whatKindMrules"
		      end
		    | (_, SOME (ERR st, deps)) => SOME (ERR st, deps)
		    | (mrule, NONE) => SOME (whatKindMrule mrule))
		  NONE
		  mrules
    in case whatKindMrules children of
	   SOME (ERR msg, deps) =>
	   let val enverr = E.ENVERR (msg, E.SYNTAX)
	       val deps'  = D.addListLab deps [label, lab1]
	   in E.applyDepsEnv enverr deps'
	   end
	 | SOME (LIST (true, true), deps) => E.ENVNUL
	 | SOME (LIST _, deps) =>
	   let val msg    = "non-exhaustive matching"
	       val enverr = E.ENVERR (msg, E.SYNTAX)
	       val deps'  = D.addListLab deps [label, lab1]
	   in E.applyDepsEnv enverr deps'
	   end
	 | SOME (INJ (true, true), deps) => E.ENVNUL
	 | SOME (INJ _, deps) =>
	   let val msg    = "non-exhaustive matching"
	       val enverr = E.ENVERR (msg, E.SYNTAX)
	       val deps'  = D.addListLab deps [label, lab1]
	   in E.applyDepsEnv enverr deps'
	   end
	 | SOME (PAT, deps) => E.mk_env_deplab (tv, E.EQD) labE
	 | SOME (WILD, deps) =>
	   if List.length children = 1
	   then let val msg    = "useless case expression"
		    val enverr = E.ENVERR (msg, E.SYNTAX)
		    val deps'  = D.addListLab deps [label, lab1]
		in E.applyDepsEnv enverr deps'
		end
	   else E.ENVNUL
	 | NONE =>
	   let val msg    = "empty case expression"
	       val enverr = E.ENVERR (msg, E.SYNTAX)
	       val deps'  = D.addListLab D.empty [label, lab1]
	   in E.applyDepsEnv enverr deps'
	   end
    end
  | checkCasePatSyntax match lab tv labE = raise Fail "term is not matching term"
(* TODO: The function finds only one error when it should find them all. *)

(* From a list of types, generates pairs. *)
fun tuple2pairs label [] = E.mk_type_unit label
  | tuple2pairs label [x] = raise Fail "a tuples cannot contain a single type"
  | tuple2pairs label [x, y] = E.mk_type_tuple [x, y] label
  | tuple2pairs label (x :: xs) = E.mk_type_tuple [x, tuple2pairs label xs] label

(* We type associated to a binary op is either a binary type 'a -> 'b -> 'c or
 * a 'a Deq. *)
(* NOTE: In case of =, this ENVLAB says that the equality is in tv1. *)
fun acc2op value label tvl tvr tvo tv =
    (* if T.member value [A.id_eq, A.id_eqeq, A.id_diff] then  else E.ENVNUL *)
    if value = A.id_eq
    then let val arr0 = E.mk_type_arrow (E.mk_tyvar tvr, E.mk_tyvar tvo) label
	     val arr  = E.mk_type_arrow (E.mk_tyvar tvl, arr0) label
	     val cst1 = E.CSITY (E.mk_tyvar tvo, E.mk_type_bool label)
	     val cst2 = E.CSITY (E.mk_tyvar tv,  arr)
	     val env1 = E.mk_env_depcst cst1 label
	     val env2 = E.mk_env_depcst cst2 label
	     val env3 = E.mk_env_deplab ((*tv*) tvl, E.EQD) label
	 in E.list2env [env1, env2, env3]
	 end
 (*let val deq  = E.mk_type_deq (E.mk_tyvar tvl) label
	     val cst1 = E.CSITY (E.mk_tyvar tvl, E.mk_tyvar tvr)
	     val cst2 = E.CSITY (E.mk_tyvar tvo, E.mk_type_bool label)
	     val cst3 = E.CSITY (E.mk_tyvar tv,  deq)
	     val env1 = E.mk_env_depcst cst1 label
	     val env2 = E.mk_env_depcst cst2 label
	     val env3 = E.mk_env_depcst cst3 label
	     val env4 = E.mk_env_deplab ((*tv*) tvl, E.EQD) label
	 in E.list2env [env1, env2, env3, env4]
	 end*)
    else let val arr1 = E.mk_type_arrow (E.mk_tyvar tvr, E.mk_tyvar tvo) label
	     val arr2 = E.mk_type_arrow (E.mk_tyvar tvl, arr1) label
	     val cst  = E.CSITY (E.mk_tyvar tv, arr2)
	     val env1 = E.mk_env_depcst cst label
	     val env2 = E.mk_env_deplab (tv, E.OTH) label
	 in E.list2env [env1, env2]
	 end

(* label [tv1, tv2, tv3] tv
 *    --->
 * (tv3 -> (tv2 -> (tv1 -> tv))) *)
fun typevarseq2TypeFun label [] ty f = (ty, E.ENVNUL)
  | typevarseq2TypeFun label [tv] ty f =
    let val tv' = E.nextItyvar ()
	val arr = E.mk_type_arrow (f (E.mk_tyvar tv), ty) label
	val env = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', arr)) label
    in (E.mk_tyvar tv', env)
    end
  | typevarseq2TypeFun label (tv :: tvs) ty f =
    let val tv' = E.nextItyvar ()
	val arr = E.mk_type_arrow (f (E.mk_tyvar tv), ty) label
	val env = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', arr)) label
	val (ty', env') = typevarseq2TypeFun label tvs (E.mk_tyvar tv') f
    in (ty', E.ENVAPP (env, env'))
    end

fun toTyClass tv label =
    let val cl = E.mk_type_class (E.mk_new_tyvar ()) label
    in E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, cl)) label
    end


(* ------ CONSTRAINT GENERATION ------ *)

fun gen_scon (A.N {kind = (A.SCON, A.SCON_INT), label, value, regions, parents, children = []}) =
    let val tv  = E.nextItyvar ()
	val env = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_type_int label)) label
    in (tv, env)
    end
  | gen_scon (A.N {kind = (A.SCON, A.SCON_REAL), label, value, regions, parents, children = []}) =
    let val tv  = E.nextItyvar ()
	val env = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_type_real label)) label
    in (tv, env)
    end
  | gen_scon (A.N {kind = (A.SCON, A.SCON_ATOM), label, value, regions, parents, children = []}) =
    let val tv  = E.nextItyvar ()
	val aty = E.mk_type_atom label
	val env = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, aty)) label
    in (tv, env)
    end
  | gen_scon (A.N {kind = (A.SCON, A.SCON_ATOMS), label, value, regions, parents, children = []}) =
    let val tv  = E.nextItyvar ()
	val aty = E.mk_type_atom label
	val lty = E.mk_type_list aty label
	val env = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, lty)) label
    in (tv, env)
    end
  | gen_scon (A.N {kind = (A.SCON, A.SCON_STRING), label, value, regions, parents, children = []}) =
    let val tv  = E.nextItyvar ()
	val sty = E.mk_type_string label
	val env = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, sty)) label
    in (tv, env)
    end
  | gen_scon _ = raise Fail "term is not a special constant"

and gen_id_bind (A.N {kind = (A.ID, A.ID_VID), label, value, regions, parents, children = []}) =
    let val tv   = E.nextItyvar ()
	val env  = E.mk_env_depbin (E.BINDVID (value, label, E.mk_new_vvscheme tv)) label
	val env' = let val enverr = E.ENVDEP (E.ENVERR (value, E.REBOUND), D.mk_dep label)
		   in if List.exists (fn x => x = value) A.constructors
		      then E.ENVAPP (enverr, env)
		      else env
		   end
    in (tv, env')
    end
  | gen_id_bind (A.N {kind = (A.ID, A.ID_TYVAR), label, value, regions, parents, children = []}) =
    let val tv  = E.nextItyvar ()
	val env = E.mk_env_depbin (E.BINDTYV (value, label, (E.mk_tyvar tv, E.EXPLICIT))) label
    in (tv, env)
    end
  | gen_id_bind term = (print (A.toString term); raise Fail "term is not an identifier")

and gen_ids_bind (A.N {kind = (A.IDENTS, A.IDENTS_LIST), label, value, regions, parents, children}) =
    let val lst = map gen_id_bind children
	val (tvs, envs) = ListPair.unzip lst
	val env = E.list2env envs
    in (tvs, env)
    end
  | gen_ids_bind term = (print (A.toString term); raise Fail "term is not an identifier list")

and gen_id_header (A.N {kind = (A.ID, A.ID_VID), label, value, regions, parents, children = []}) kind =
    let val tv    = E.nextItyvar ()
	val base  = value ^ "'base"
	val send  = value ^ "'send"
	val cast  = value ^ "'broadcast"
	val nsend = value ^ "''send"
	val ncast = value ^ "''broadcast"
	val envB  = (* base class *)
	    if kind = "internal"
	       orelse kind = "input"
	       orelse kind = "output" (* we need the base class to make statements *)
	    then let val tv1  = E.nextItyvar ()
		     val env1 = E.mk_env_depbin (E.BINDVID (base, label, E.mk_new_vscheme tv1)) label
		     val cls  = E.mk_type_class (E.mk_tyvar tv) label
		     val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, cls)) label
		 in [env1, env2]
		 end
	    else if kind = "output"
	    then []
	    else raise Fail "gen_id_header"
	val envS = (* send/broadcast functions *)
	    if kind = "internal" orelse kind = "output"
	    then let (* send *)
		    val envs1 =
			let val tv1  = E.nextItyvar ()
			    val env1 = E.mk_env_depbin (E.BINDVID (send, label, E.mk_new_vscheme tv1)) label
			    val arr1 = E.mk_type_arrow (E.mk_tyvar tv, E.mk_type_instr label) label
			    val arr2 = E.mk_type_arrow (E.mk_type_loc label, arr1) label
			    val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr2)) label
			in [env1, env2]
			end
		    (* nsend *)
		    val envs2 =
			let val tv1  = E.nextItyvar ()
			    val env1 = E.mk_env_depbin (E.BINDVID (nsend, label, E.mk_new_vscheme tv1)) label
			    val arr1 = E.mk_type_arrow (E.mk_tyvar tv, E.mk_type_instr label) label
			    val arr2 = E.mk_type_arrow (E.mk_type_loc label, arr1) label
			    val arr3 = E.mk_type_arrow (E.mk_type_int label, arr2) label
			    val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr3)) label
			in [env1, env2]
			end
		    (* broadcast *)
		    val envs3 =
			let val tv1  = E.nextItyvar ()
			    val env1 = E.mk_env_depbin (E.BINDVID (cast, label, E.mk_new_vscheme tv1)) label
			    val bag  = E.mk_type_bag (E.mk_type_instr label) label
			    val locs = E.mk_type_bag (E.mk_type_loc label) label
			    val arr1 = E.mk_type_arrow (E.mk_tyvar tv, bag) label
			    val arr2 = E.mk_type_arrow (locs, arr1) label
			    val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr2)) label
			in [env1, env2]
			end
		    (* nbroadcast *)
		    val envs4 =
			let val tv1  = E.nextItyvar ()
			    val env1 = E.mk_env_depbin (E.BINDVID (ncast, label, E.mk_new_vscheme tv1)) label
			    val bag  = E.mk_type_bag (E.mk_type_instr label) label
			    val locs = E.mk_type_bag (E.mk_type_loc label) label
			    val arr1 = E.mk_type_arrow (E.mk_tyvar tv, bag) label
			    val arr2 = E.mk_type_arrow (locs, arr1) label
			    val arr3 = E.mk_type_arrow (E.mk_type_int label, arr2) label
			    val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr3)) label
			in [env1, env2]
			end
		in envs1 @ envs2 @ envs3 @ envs4
		end
	    else if kind = "input"
	    then []
	    else raise Fail "gen_id_header"
	val envs = envB @ envS
	val env  = let val enverr = E.ENVDEP (E.ENVERR (value, E.REBOUND), D.mk_dep label)
		   in if List.exists (fn x => x = value) A.constructors
		      then E.list2env (enverr :: envs)
		      else E.list2env envs
		   end
    in (tv, env)
    end
  | gen_id_header term kind = (print (A.toString term); raise Fail "term is not an identifier")

and gen_id_set (A.N {kind = (A.ID, A.ID_VID), label, value, regions, parents, children = []}) lab =
    let val tv        = E.nextItyvar () (* type of an Item *)
	val typ       = value ^ "'set"
	val member    = value ^ "'member"
	val empty     = value ^ "'empty"
	val isempty   = value ^ "'isEmpty"
	val singleton = value ^ "'singleton"
	val add       = value ^ "'add"
	val union     = value ^ "'union"
	val remove    = value ^ "'remove"
	(* set type *)
	val env_typ =
	    let val tv   = E.nextItyvar () (* type Set(Item) *)
		val seq  = E.ITYSEQSEQ ([], label)
		val ity  = E.ITYCON (seq, E.ITYCONNAM (typ, false, label)) (* false means that equality is undecidable *)
		val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, ity)) label
		val env2 = E.mk_env_depbin (E.BINDTYC (typ, lab, E.mk_new_ityfun seq (E.mk_tyvar tv))) lab
	    in E.list2env [env1, env2]
	    end
	fun mk_acc () =
	    let val sv   = E.nextItyseqvar ()
		val tv   = E.nextItyvar ()
		val env1 = E.mk_env_depacc (E.ACCTYC (typ, (sv, tv))) label
		val env2 = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv, E.ITYSEQSEQ ([], label))) label
	    in (tv, E.list2env [env1, env2])
	    end
	(* member *)
	val env_member =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Item -> Set -> Bool *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (member, lab, E.mk_new_vscheme tv1)) lab
		val arr1 = E.mk_type_arrow (E.mk_tyvar tvT, E.mk_type_bool label) label
		val arr2 = E.mk_type_arrow (E.mk_tyvar tv, arr1) label
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr2)) label
	    in E.list2env [envT, env1, env2]
	    end
	(* empty *)
	val env_empty =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Set *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (empty, lab, E.mk_new_vscheme tv1)) lab
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tvT)) label
	    in E.list2env [envT, env1, env2]
	    end
	(* isEmpty *)
	val env_isempty =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Set -> Bool *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (isempty, lab, E.mk_new_vscheme tv1)) lab
		val arr1 = E.mk_type_arrow (E.mk_tyvar tvT, E.mk_type_bool label) label
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr1)) label
	    in E.list2env [envT, env1, env2]
	    end
	(* singleton *)
	val env_singleton =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Item -> Set *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (singleton, lab, E.mk_new_vscheme tv1)) lab
		val arr1 = E.mk_type_arrow (E.mk_tyvar tv, E.mk_tyvar tvT) label
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr1)) label
	    in E.list2env [envT, env1, env2]
	    end
	(* add *)
	val env_add =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Item -> Set -> Set *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (add, lab, E.mk_new_vscheme tv1)) lab
		val arr1 = E.mk_type_arrow (E.mk_tyvar tvT, E.mk_tyvar tvT) label
		val arr2 = E.mk_type_arrow (E.mk_tyvar tv, arr1) label
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr2)) label
	    in E.list2env [envT, env1, env2]
	    end
	(* union *)
	val env_union =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Set -> Set -> Set *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (union, lab, E.mk_new_vscheme tv1)) lab
		val arr1 = E.mk_type_arrow (E.mk_tyvar tvT, E.mk_tyvar tvT) label
		val arr2 = E.mk_type_arrow (E.mk_tyvar tvT, arr1) label
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr2)) label
	    in E.list2env [envT, env1, env2]
	    end
	(* remove *)
	val env_remove =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Item -> Set -> Set *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (remove, lab, E.mk_new_vscheme tv1)) lab
		val arr1 = E.mk_type_arrow (E.mk_tyvar tvT, E.mk_tyvar tvT) label
		val arr2 = E.mk_type_arrow (E.mk_tyvar tv, arr1) label
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr2)) label
	    in E.list2env [envT, env1, env2]
	    end
	val env = E.list2env
		      [env_typ, env_member, env_empty, env_isempty,
		       env_singleton, env_add, env_union, env_remove]
    in (tv, env)
    end
  | gen_id_set term lab = (print (A.toString term); raise Fail "term is not an identifier")

and gen_id_map (A.N {kind = (A.ID, A.ID_VID), label, value, regions, parents, children = []}) lab =
    let val tv        = E.nextItyvar () (* type of a Key *)
	val tv'       = E.nextItyvar () (* type of a Value *)
	val typ       = value ^ "'map"
	val eqkey     = value ^ "'eqKey"
	val find      = value ^ "'find"
	val indom     = value ^ "'inDom"
	val empty     = value ^ "'empty"
	val isempty   = value ^ "'isEmpty"
	val update    = value ^ "'update"
	val add       = value ^ "'add"
	val remove    = value ^ "'remove"
	(* set type *)
	val env_typ =
	    let val tv   = E.nextItyvar () (* type Set(Item) *)
		val seq  = E.ITYSEQSEQ ([], label)
		val ity  = E.ITYCON (seq, E.ITYCONNAM (typ, false, label)) (* false means that equality is undecidable *)
		val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, ity)) label
		val env2 = E.mk_env_depbin (E.BINDTYC (typ, lab, E.mk_new_ityfun seq (E.mk_tyvar tv))) lab
	    in E.list2env [env1, env2]
	    end
	fun mk_acc () =
	    let val sv   = E.nextItyseqvar ()
		val tv   = E.nextItyvar ()
		val env1 = E.mk_env_depacc (E.ACCTYC (typ, (sv, tv))) label
		val env2 = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv, E.ITYSEQSEQ ([], label))) label
	    in (tv, E.list2env [env1, env2])
	    end
	(* eqKey *)
	val env_eqkey =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Key -> Key -> Bool *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (eqkey, lab, E.mk_new_vscheme tv1)) lab
		val arr1 = E.mk_type_arrow (E.mk_tyvar tv, E.mk_type_bool label) label
		val arr2 = E.mk_type_arrow (E.mk_tyvar tv, arr1) label
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr2)) label
	    in E.list2env [envT, env1, env2]
	    end
	(* find *)
	val env_find =
	    let val tv1   = E.nextItyvar ()
		(* tv1 = Key -> Map -> Value + Unit *)
		val (tvT,envT) = mk_acc ()
		val env1  = E.mk_env_depbin (E.BINDVID (find, lab, E.mk_new_vscheme tv1)) lab
		val union = E.mk_type_disju (E.mk_tyvar tv', E.mk_type_unit label) label
		val arr1  = E.mk_type_arrow (E.mk_tyvar tvT, union) label
		val arr2  = E.mk_type_arrow (E.mk_tyvar tv, arr1) label
		val env2  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr2)) label
	    in E.list2env [envT, env1, env2]
	    end
	(* inDom *)
	val env_indom =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Key -> Map -> Bool *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (indom, lab, E.mk_new_vscheme tv1)) lab
		val arr1 = E.mk_type_arrow (E.mk_tyvar tvT, E.mk_type_bool label) label
		val arr2 = E.mk_type_arrow (E.mk_tyvar tv, arr1) label
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr2)) label
	    in E.list2env [envT, env1, env2]
	    end
	(* empty *)
	val env_empty =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Map *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (empty, lab, E.mk_new_vscheme tv1)) lab
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tvT)) label
	    in E.list2env [envT, env1, env2]
	    end
	(* isEmpty *)
	val env_isempty =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Map -> Bool *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (isempty, lab, E.mk_new_vscheme tv1)) lab
		val arr1 = E.mk_type_arrow (E.mk_tyvar tvT, E.mk_type_bool label) label
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr1)) label
	    in E.list2env [envT, env1, env2]
	    end
	(* update *)
	val env_update =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Key -> Value -> Map -> Map *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (update, lab, E.mk_new_vscheme tv1)) lab
		val arr1 = E.mk_type_arrow (E.mk_tyvar tvT, E.mk_tyvar tvT) label
		val arr2 = E.mk_type_arrow (E.mk_tyvar tv', arr1) label
		val arr3 = E.mk_type_arrow (E.mk_tyvar tv, arr2) label
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr3)) label
	    in E.list2env [envT, env1, env2]
	    end
	(* add *)
	val env_add =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Key -> Value -> Map -> Map *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (add, lab, E.mk_new_vscheme tv1)) lab
		val arr1 = E.mk_type_arrow (E.mk_tyvar tvT, E.mk_tyvar tvT) label
		val arr2 = E.mk_type_arrow (E.mk_tyvar tv', arr1) label
		val arr3 = E.mk_type_arrow (E.mk_tyvar tv, arr2) label
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr3)) label
	    in E.list2env [envT, env1, env2]
	    end
	(* remove *)
	val env_remove =
	    let val tv1  = E.nextItyvar ()
		(* tv1 = Key -> Map -> Map *)
		val (tvT,envT) = mk_acc ()
		val env1 = E.mk_env_depbin (E.BINDVID (remove, lab, E.mk_new_vscheme tv1)) lab
		val arr1 = E.mk_type_arrow (E.mk_tyvar tvT, E.mk_tyvar tvT) label
		val arr2 = E.mk_type_arrow (E.mk_tyvar tv, arr1) label
		val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr2)) label
	    in E.list2env [envT, env1, env2]
	    end
	val env = E.list2env
		      [env_typ, env_eqkey, env_find, env_indom,
		       env_empty, env_isempty,
		       env_update, env_add, env_remove]
    in (tv, tv', env)
    end
  | gen_id_map term lab = (print (A.toString term); raise Fail "term is not an identifier")

(* NOTE: p is for patterns as opposed to a new function
 * a pattern can contain 'true' for example, but we cannot rebind 'true'. *)
and gen_id_bind_p (A.N {kind = (A.ID, A.ID_VID), label, value, regions, parents, children = []}) =
    let val tv   = E.nextItyvar ()
	val env  = E.mk_env_depbin (E.BINDVID (value, label, E.mk_new_vvscheme tv)) label
    in (tv, env)
    end
  | gen_id_bind_p term = (print (A.toString term); raise Fail "term is not an identifier")

(* NOTE: i stands for implicit (type variables) as opposed to the explicit markers
 * generated in gen_id_bind. *)
and gen_id_bind_i (A.N {kind = (A.ID, A.ID_TYVAR), label, value, regions, parents, children = []}) =
    let val tv  = E.nextItyvar ()
	val env = E.mk_env_depbin (E.BINDTYV (value, label, (E.mk_tyvar tv, E.IMPLICIT))) label
    in (tv, env)
    end
  | gen_id_bind_i term = (print (A.toString term); raise Fail "term is not an identifier")

(* NOTE: o stands for overloading *)
and gen_id_bind_o (A.N {kind = (A.ID, A.ID_VID), label, value, regions, parents, children = []}) =
    let val tv   = E.nextItyvar ()
	val idor = E.nextIdor ()
	val env  = E.mk_env_depbin (E.BINDVID (value, label, E.mk_new_oscheme tv idor)) label
	val env' = let val enverr = E.ENVDEP (E.ENVERR (value, E.REBOUND), D.mk_dep label)
		   in if List.exists (fn x => x = value) A.constructors
		      then E.ENVAPP (enverr, env)
		      else env
		   end
    in (tv, idor, env')
    end
  | gen_id_bind_o term = (print (A.toString term); raise Fail "term is not an identifier")

(* NOTE: v stands for 'variable'.  For 'variable' declaration, we generate
 * a different kind of binder. *)
and gen_id_bind_v (A.N {kind = (A.ID, A.ID_VID), label, value, regions, parents, children = []}) =
    let val tv   = E.nextItyvar ()
	val env  = E.mk_env_depbin (E.BINDVAR (value, label, E.mk_tyvar tv)) label
    in (tv, env)
    end
  | gen_id_bind_v term = (print (A.toString term); raise Fail "term is not an identifier")

and gen_ids_bind_v (A.N {kind = (A.IDENTS, A.IDENTS_LIST), label, value, regions, parents, children}) =
    let val lst = map gen_id_bind_v children
	val (tvs, envs) = ListPair.unzip lst
	val env = E.list2env envs
    in (tvs, env)
    end
  | gen_ids_bind_v term = (print (A.toString term); raise Fail "term is not an identifier list")

and gen_id_bound (A.N {kind = (A.ID, A.ID_VID), label, value, regions, parents, children = []}) =
    let val tv   = E.nextItyvar ()
	val tv'  = E.nextItyvar ()
	val ity' = E.mk_tyvar tv'
	val env1 = E.mk_env_depacc (E.ACCVID (value, tv)) label
	val env2 =
	    if value = A.id_eq
	    then let val tv1  = E.nextItyvar ()
		     val tv2  = E.nextItyvar ()
		     val bool = E.mk_type_bool label
		     val arr1 = E.mk_type_arrow (E.mk_tyvar tv2, bool) label
		     val arr2 = E.mk_type_arrow (E.mk_tyvar tv1, arr1) label
		     val deq  = E.mk_type_deq   (E.mk_tyvar tv1) label
		     val env2 = E.mk_env_depsub (E.SUBITY (E.mk_tyvar tv, arr2)) label
		     val env3 = E.mk_env_depcst (E.CSITY (ity', deq)) label
		     val env4 = E.mk_env_deplab ((*tv'*) tv1, E.EQD) label
		 in E.list2env [env2, env3, env4]
		 end
	    else let val env2 = E.mk_env_depsub (E.SUBITY (E.mk_tyvar tv, ity')) label
		     val env3 = E.mk_env_deplab (tv', E.OTH) label
		 in E.list2env [env2, env3]
		 end
    in (tv', E.list2env [env1, env2])
    end
 (*let val tv   = E.nextItyvar ()
	val tv'  = E.nextItyvar ()
	val ity' = E.mk_tyvar tv'
	val env1 = E.mk_env_depacc (E.ACCVID (value, tv)) label
	val env2 = E.mk_env_depsub (E.SUBITY (E.mk_tyvar tv, ity')) label
	val env3 =
	    if value = A.id_eq
	    then let val tv0  = E.nextItyvar ()
		     val ity  = E.mk_type_deq (E.mk_tyvar tv0) label
		     val env3 = E.mk_env_depcst (E.CSITY (ity', ity)) label
		     val env4 = E.mk_env_deplab ((*tv'*) tv0, E.EQD) label
		 in E.list2env [env3, env4]
		 end
	    else E.mk_env_deplab (tv', E.OTH) label
    in (tv', E.list2env [env1, env2, env3])
    end*)
  | gen_id_bound (A.N {kind = (A.ID, A.ID_TYVAR), label, value, regions, parents, children = []}) =
    let val tv  = E.nextItyvar ()
	val env = E.mk_env_depacc (E.ACCTYV (value, tv)) label
    in (tv, env)
    end
  | gen_id_bound _ = raise Fail "term is not an identifier"

(* Bound constructor *)
and gen_id_bound_c (A.N {kind = (A.ID, A.ID_VID), label, value, regions, parents, children = []}) lab =
    let val tv  = E.nextItyvar ()
	val env = if List.exists (fn x => x = value) A.constructors
		  then E.mk_env_depacc (E.ACCVID (value, tv)) label
		  else let val msg = value ^ " is not a datatype constructor"
			   val enverr = E.ENVERR (msg, E.SYNTAX)
		       in E.ENVDEP (E.ENVDEP (enverr, D.mk_dep label), D.mk_dep lab)
		       end
    in (tv, env)
    end
  | gen_id_bound_c _ _ = raise Fail "term is not an identifier"

and gen_id_typeof (A.N {kind = (A.ID, A.ID_VID), label, value, regions, parents, children = []}) =
    let val env = E.ENVTOF value
    in env
    end
  | gen_id_typeof _ = raise Fail "term is not an identifier"

and gen_tycon_bound (A.N {kind = (A.ID, A.ID_TYCON), label, value, regions, parents, children = []}) =
    let val sv  = E.nextItyseqvar ()
	val tv  = E.nextItyvar ()
	val env = E.mk_env_depacc (E.ACCTYC (value, (sv, tv))) label
    in (sv, tv, env)
    end
  | gen_tycon_bound _ = raise Fail "term is not a type constructor(bound)"

and gen_tycon_bind (A.N {kind = (A.ID, A.ID_TYCON), label, value, regions, parents, children = []}) =
    let val sv   = E.nextItyseqvar ()
	val tv   = E.nextItyvar ()
	val seq  = E.ITYSEQVAR sv
	val ity  = E.ITYCON (seq, E.ITYCONNAM (value, false, label))
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, ity)) label
	val env2 = E.mk_env_depbin (E.BINDTYC (value, label, E.mk_new_ityfun seq (E.mk_tyvar tv))) label
	val env3 = if List.exists (fn x => x = value) E.tyconnames
		   then E.ENVDEP (E.ENVERR (value ^ " is already an internal EventML type", E.SYNTAX), D.mk_dep label)
		   else E.ENVNUL
    in (value, tv, sv, E.list2env [env1, env2, env3])
    end
  | gen_tycon_bind _ = raise Fail "term is not a type constructor(bind)"

and gen_eqtycon_bind (A.N {kind = (A.ID, A.ID_TYCON), label, value, regions, parents, children = []}) =
    let val sv   = E.nextItyseqvar ()
	val tv   = E.nextItyvar ()
	val seq  = E.ITYSEQVAR sv
	val ity  = E.ITYCON (seq, E.ITYCONNAM (value, true, label))
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, ity)) label
	val env2 = E.mk_env_depbin (E.BINDTYC (value, label, E.mk_new_ityfun seq ity)) label
	val env3 = if List.exists (fn x => x = value) E.tyconnames
		   then E.ENVDEP (E.ENVERR (value ^ " is already an internal E# type", E.SYNTAX), D.mk_dep label)
		   else E.ENVNUL
    in (tv, sv, E.list2env [env1, env2, env3])
    end
  | gen_eqtycon_bind _ = raise Fail "term is not a type constructor(eqbind)"

(*
and gen_eqid_bind (A.N {kind = (A.ID, A.ID_VID), label, value, regions, parents, children = []}) tycon tyvarseq =
    let val tv   = E.nextItyvar ()
	val env  = E.mk_env_depbin (E.BINDVID (value, E.mk_new_vscheme tv)) label
	val env1 = let val enverr = E.ENVDEP (E.ENVERR (value, E.REBOUND), D.mk_dep label)
		   in if T.member value A.constructors
		      then E.ENVAPP (enverr, env)
		      else env
		   end
	val sv   = E.nextItyseqvar ()
	val tc   = A.getValue tycon
	val labT = A.getLabel tycon
	val labS = A.getLabel tyvarseq
	val n    = List.length (A.getChildren tyvarseq)
	val seq  = List.tabulate (n, fn _ => E.mk_new_eqtyvar labT)
	val ity  = E.ITYCON (E.ITYSEQVAR sv, E.ITYCONNAM (tc, true, labT))
	val tv'  = E.nextItyvar ()
	val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', ity)) labT
	val env3 = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv, E.ITYSEQSEQ (seq, labS))) labS
	val arr0 = E.mk_type_arrow (E.mk_tyvar tv', E.mk_type_bool label) label
	val arr  = E.mk_type_arrow (E.mk_tyvar tv', arr0) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, arr)) label
	val env5 = E.list2env [env1, env2, env3, env4]
    in E.ENVPOL (env5, E.CON)
    end
  | gen_eqid_bind term _ _ = (print (A.toString term); raise Fail "term is not an identifier")
*)

and gen_tycon_fun (A.N {kind = (A.ID, A.ID_TYCON), label, value, regions, parents, children = []}) =
    let val sv   = E.nextItyseqvar ()
	val tv   = E.nextItyvar ()
	val tv'  = E.nextItyvar ()
	val seq  = E.ITYSEQVAR sv
	val ity  = E.mk_tyvar tv
	val env0 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
	val env1 = E.mk_env_depbin (E.BINDTYC (value, label, E.mk_new_ityfun seq ity)) label
	val env2 = if List.exists (fn x => x = value) E.tyconnames
		   then E.ENVDEP (E.ENVERR (value ^ " is already an internal E# type", E.SYNTAX), D.mk_dep label)
		   else E.ENVNUL
    in (sv, tv', E.list2env [env0, env1, env2])
    end
  | gen_tycon_fun _ = raise Fail "term is not a type constructor(fun)"

and gen_tyvar_bound (t as A.N {kind = (A.TYPEVAR, A.TYPEVAR_VAR), label, value, regions, parents, children = [tyvar]}) =
    let val (tv, env) = gen_id_bound tyvar
	val tv'  = E.nextItyvar ()
	val env' = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
    in (tv', E.ENVAPP (env, env'))
    end
  | gen_tyvar_bound _ = raise Fail "term is not a type variable"

and gen_tyvar_bind (A.N {kind = (A.TYPEVAR, A.TYPEVAR_VAR), label, value, regions, parents, children = [tyvar]}) =
    let val (tv, env) = gen_id_bind tyvar
	val tv'  = E.nextItyvar ()
	val env' = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
    in (tv', E.ENVAPP (env, env'))
    end
  | gen_tyvar_bind _ = raise Fail "term is not a type variable"

(* NOTE: We generate IMPLICIT markers instead of EXPLICIT ones as in gen_tyvar_bind. *)
and gen_tyvar_bind_i (A.N {kind = (A.TYPEVAR, A.TYPEVAR_VAR), label, value, regions, parents, children = [tyvar]}) =
    let val (tv, env) = gen_id_bind_i tyvar
	val tv'  = E.nextItyvar ()
	val env' = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
    in (tv', E.ENVAPP (env, env'))
    end
  | gen_tyvar_bind_i _ = raise Fail "term is not a type variable"

and gen_type (A.N {kind = (A.TYPE, A.TYPE_ARROW), label, value, regions, parents, children = [ty1, ty2]}) =
    let val (tv1, env1) = gen_type ty1
	val (tv2, env2) = gen_type ty2
	val tv   = E.nextItyvar ()
	val aty  = E.mk_type_arrow (E.mk_tyvar tv1, E.mk_tyvar tv2) label
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, aty)) label
	val env4 = E.ENVAPP (E.ENVAPP (env1, env2), env3)
    in (tv, env4)
    end
  | gen_type (A.N {kind = (A.TYPE, A.TYPE_DARROW), label, value, regions, parents, children = [id, ty1, ty2]}) =
    let val (tv1, env1) = gen_id_bind id
	val (tv2, env2) = gen_type ty1
	val (tv3, env3) = gen_type ty2
	val tv   = E.nextItyvar ()
	val aty  = E.mk_type_arrow (E.mk_tyvar tv2, E.mk_tyvar tv3) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, aty)) label
	val env5 = E.mk_env_loc (E.ENVAPP (env2, env1), env3)
	val env6 = E.list2env [env5, env4]
    in (tv, env6)
    end
  | gen_type (A.N {kind = (A.TYPE, A.TYPE_DISJU), label, value, regions, parents, children = [ty1, ty2]}) =
    let val (tv1, env1) = gen_type ty1
	val (tv2, env2) = gen_type ty2
	val tv   = E.nextItyvar ()
	val aty  = E.mk_type_disju (E.mk_tyvar tv1, E.mk_tyvar tv2) label
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, aty)) label
	val env4 = E.ENVAPP (E.ENVAPP (env1, env2), env3)
    in (tv, env4)
    end
  | gen_type (A.N {kind = (A.TYPE, A.TYPE_TUPLE), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_type children)
	val tv    = E.nextItyvar ()
	val tuple = tuple2pairs label (map E.mk_tyvar tvs)
	val env1  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, tuple)) label
	val env2  = locCst (E.ENVAPP (E.list2env envs, env1))
    in (tv, env2)
    end
  | gen_type (A.N {kind = (A.TYPE, A.TYPE_TYCON), label, value, regions, parents, children = [tycon, tyseq]}) =
    let val (sv1, env1) = gen_typeseq tyseq
	val (sv2, tv2, env2) = gen_tycon_bound tycon
	val tv   = E.nextItyvar ()
	val env3 = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv1, E.ITYSEQVAR sv2)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv2)) label
	val env5 = E.list2env [env1, env2, env3, env4]
    in (tv, env5)
    end
  | gen_type (A.N {kind = (A.TYPE, A.TYPE_VAR), label, value, regions, parents, children = [tyvar]}) =
    let val (tv, env) = gen_tyvar_bound tyvar
	val tv'  = E.nextItyvar ()
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
	val env2 = E.ENVAPP (env, env1)
    in (tv', env2)
    end
  | gen_type (A.N {kind = (A.TYPE, A.TYPE_PAREN), label, value, regions, parents, children = [ty]}) =
    let val (tv, env) = gen_type ty
	val tv'  = E.nextItyvar ()
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
	val env2 = E.ENVAPP (env, env1)
    in (tv', env2)
    end
  | gen_type (A.N {kind = (A.TYPE, A.TYPE_DEP), label, value, regions, parents, children = [id, ty]}) =
    let val (tv1, env1) = gen_id_bind id
	val (tv2, env2) = gen_type ty
    in raise Fail "gen_type:TYPE_DEP"
	(*val tv   = E.nextItyvar ()
	val ity  = E.ITYDPP (E.mk_tyvar tv1, E.mk_tyvar tv2)
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, ity)) label
	val env4 = E.ENVNUL (*E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, E.mk_type_type label)) label*)
	(* NOTE: env1 might have tok binders *)
	val env5 = E.ENVAPP (E.ENVAPP (env2, E.ENVAPP (env3, env4)), env1)
    in (tv, env5)*)
    end
  | gen_type (A.N {kind = (A.TYPE, A.TYPE_SET), label, value, regions, parents, children = [id, ty, prop]}) =
    let val (tv1, env1) = gen_id_bind id
	val (tv2, env2) = gen_type    ty
	val (tv3, env3) = gen_prop    prop
	val envl = E.mk_env_loc (env1, env3)
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_type_prop label)) label
    in (tv2, E.list2env [env2, env4, env5, envl])
    end
  (*| gen_type (A.N {kind = (A.TYPE, A.TYPE_ID), label, value, regions, parents, children = [tok]}) =
    let val (tv, env) = gen_id_bound tok
	val tv'  = E.nextItyvar ()
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_tyvar tv)) label
	val env2 = E.ENVAPP (env, env1)
    in (tv', env2)
    end*)
  | gen_type term = (print (A.toString term); raise Fail "term is not a type")

and gen_typevarseq (A.N {kind = (A.TYPEVARSEQ, A.TYPEVARSEQ_ONE), label, value, regions, parents, children = [tyvar]}) =
    let val (tv, env) = gen_tyvar_bind tyvar
	val sv   = E.nextItyseqvar ()
	val env1 = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv, E.ITYSEQSEQ ([E.mk_tyvar tv], label))) label
	val env2 = E.ENVAPP (env, env1)
	val f    = typevarseq2TypeFun label [tv]
    in (sv, f, env2)
    end
  | gen_typevarseq (A.N {kind = (A.TYPEVARSEQ, A.TYPEVARSEQ_SEQ), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_tyvar_bind children)
	val sv   = E.nextItyseqvar ()
	val env1 = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv, E.ITYSEQSEQ (map E.mk_tyvar tvs, label))) label
	val env2 = E.ENVAPP (E.list2env envs, env1)
	val f    = typevarseq2TypeFun label (rev tvs)
    in (sv, f, env2)
    end
  | gen_typevarseq (A.N {kind = (A.TYPEVARSEQ, A.TYPEVARSEQ_EM), label, value, regions, parents, children = []}) =
    let val sv  = E.nextItyseqvar ()
	val env = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv, E.ITYSEQSEQ ([], label))) label
	val f   = typevarseq2TypeFun label []
    in (sv, f, env)
    end
  | gen_typevarseq _ = raise Fail "term is not a type variable sequence"

and gen_typeseq (A.N {kind = (A.TYPESEQ, A.TYPESEQ_ONE), label, value, regions, parents, children = [ty]}) =
    let val (tv, env) = gen_type ty
	val sv   = E.nextItyseqvar ()
	val env1 = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv, E.ITYSEQSEQ ([E.mk_tyvar tv], label))) label
	val env2 = E.ENVAPP (env, env1)
    in (sv, env2)
    end
  | gen_typeseq (A.N {kind = (A.TYPESEQ, A.TYPESEQ_SEQ), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_type children)
	val sv   = E.nextItyseqvar ()
	val env1 = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv, E.ITYSEQSEQ (map E.mk_tyvar tvs, label))) label
	val env2 = E.ENVAPP (E.list2env envs, env1)
    in (sv, env2)
    end
  | gen_typeseq (A.N {kind = (A.TYPESEQ, A.TYPESEQ_EM), label, value, regions, parents, children = []}) =
    let val sv  = E.nextItyseqvar ()
	val env = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv, E.ITYSEQSEQ ([], label))) label
    in (sv, env)
    end
  | gen_typeseq _ = raise Fail "term is not a type sequence"

and gen_typevarseq_ov (A.N {kind = (A.TYPEVARSEQ, A.TYPEVARSEQ_ONE), label, value, regions, parents, children = [tyvar]}) =
    let val (tv, env) = gen_tyvar_bind_i tyvar
	val tv'  = E.nextItyvar ()
	val env' = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_tyvar tv)) label
    in ([tv'], E.ENVAPP (env, env'))
    end
  | gen_typevarseq_ov (A.N {kind = (A.TYPEVARSEQ, A.TYPEVARSEQ_SEQ), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_tyvar_bind_i children)
	val tvs'  = List.tabulate (List.length tvs, fn _ => E.nextItyvar ())
	val pairs = ListPair.zip (tvs, tvs')
	val envs' = map (fn (tv1, tv2) => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label) pairs
	val env   = E.list2env (envs @ envs')
    in (tvs', env)
    end
  | gen_typevarseq_ov _ = raise Fail "term is not an pverloading type variable sequence"

and gen_typeseq_ov (A.N {kind = (A.TYPESEQ, A.TYPESEQ_ONE), label, value, regions, parents, children = [ty]}) =
    let val (tv, env) = gen_type ty
	val tv'  = E.nextItyvar ()
	val env' = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_tyvar tv)) label
    in ([tv'], E.ENVAPP (env, env'))
    end
  | gen_typeseq_ov (A.N {kind = (A.TYPESEQ, A.TYPESEQ_SEQ), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_type children)
	val tvs'  = map (fn _ => E.nextItyvar ()) tvs
	val pairs = ListPair.zip (tvs, tvs')
	val envs' = map (fn (tv1, tv2) => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label) pairs
	val env   = E.list2env (envs @ envs')
    in (tvs', env)
    end
  | gen_typeseq_ov _ = raise Fail "term is not an overloading type sequence"

and gen_otypeseq (A.N {kind = (A.OTYPESEQ, A.OTYPESEQ_UNM), label, value, regions, parents, children = [typeseq]}) =
    let val (tvs, env) = gen_typeseq_ov typeseq
	val tvs'  = map (fn _ => E.nextItyvar ()) tvs
	val pairs = (ListPair.zip (tvs', tvs))
	val envs  = map (fn (tv1, tv2) => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label) pairs
	val env'  = E.ENVAPP (E.list2env envs, env)
    in ((tvs', env'), NONE)
    end
  | gen_otypeseq (A.N {kind = (A.OTYPESEQ, A.OTYPESEQ_NAM), label, value, regions, parents, children = [typeseq]}) =
    let val (tvs, env) = gen_typeseq_ov typeseq
	val tvs'  = map (fn _ => E.nextItyvar ()) tvs
	val pairs = (ListPair.zip (tvs', tvs))
	val envs  = map (fn (tv1, tv2) => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label) pairs
	val env'  = E.ENVAPP (E.list2env envs, env)
    in ((tvs', env'), SOME value)
    end
  | gen_otypeseq _ = raise Fail "term is not an overloading type sequence"

and gen_typeseqset (A.N {kind = (A.TYPESEQSET, A.TYPESEQSET_SET), label, value, regions, parents, children}) =
    let val (sems, names) = ListPair.unzip (map gen_otypeseq children)
	val (tvss, envs) = ListPair.unzip sems
	val init  = case tvss of
			(list :: _) => List.tabulate (List.length list, fn _ => [])
		      | _ => raise Fail "term is not an overloading type sequence"
	val list1 = foldr (fn (list, lists) =>
			      map (fn (x, xs) => x :: xs)
				  (ListPair.zip (list, lists)))
			  init
			  tvss
	val list2 = map (fn tvs =>
			    let val sv  = E.nextItyseqvar ()
				val seq = E.ITYSEQSEQ (map E.mk_tyvar tvs, label)
				val env = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv, seq)) label
			    in (sv, env)
			    end)
			list1
	val (svs, envs') = ListPair.unzip list2
	val env  = E.list2env (envs @ envs')
    in (svs, env, names)
    end
  | gen_typeseqset _ = raise Fail "term is not a type sequence"

and gen_atpat (A.N {kind = (A.ATPAT, A.ATPAT_ID), label, value, regions, parents, children = [id]}) =
    let val (tv, env) = gen_id_bind_p id
	val tv'  = E.nextItyvar ()
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
	val env2 = E.ENVAPP (env, env1)
    in (tv', env2)
    end
  | gen_atpat (A.N {kind = (A.ATPAT, A.ATPAT_WILD), label, value, regions, parents, children = []}) =
    let val tv = E.nextItyvar ()
    in (tv, E.ENVNUL)
    end
  | gen_atpat (A.N {kind = (A.ATPAT, A.ATPAT_SCON), label, value, regions, parents, children = [sc]}) =
    let val (tv, env) = gen_scon sc
	val tv'  = E.nextItyvar ()
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
	val env2 = E.ENVAPP (env, env1)
    in (tv', env2)
    end
  | gen_atpat (A.N {kind = (A.ATPAT, A.ATPAT_LIST), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_pat children)
	val tv    = E.nextItyvar ()
	val tv'   = E.nextItyvar ()
	val envs' = map (fn tv' => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label) tvs
	val env1  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_type_list (E.mk_tyvar tv) label)) label
	val env2  = E.ENVAPP (E.list2env (envs @ envs'), env1)
    in (tv', env2)
    end
  (*| gen_atpat (A.N {kind = (A.ATPAT, A.ATPAT_BAG), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_pat children)
	val tv    = E.nextItyvar ()
	val tv'   = E.nextItyvar ()
	val envs' = map (fn tv' => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label) tvs
	val env1  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_type_bag (E.mk_tyvar tv) label)) label
	val env2  = E.ENVAPP (E.list2env (envs @ envs'), env1)
    in (tv', env2)
    end*)
  | gen_atpat (A.N {kind = (A.ATPAT, A.ATPAT_PAREN), label, value, regions, parents, children = [pat]}) =
    let val (tv, env) = gen_pat pat
	val tv'  = E.nextItyvar ()
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
	val env2 = E.ENVAPP (env, env1)
    in (tv', env2)
    end
  | gen_atpat (A.N {kind = (A.ATPAT, A.ATPAT_STRUC), label, value, regions, parents, children = [id]}) =
    let val (tv, env) = gen_id_bind_p id
	val tv'  = E.nextItyvar ()
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
	val env2 = E.ENVAPP (env, env1)
    in (tv', env2)
    end
  | gen_atpat (A.N {kind = (A.ATPAT, A.ATPAT_TUPLE), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_pat children)
	val tv   = E.nextItyvar ()
	val list = tuple2pairs label (map E.mk_tyvar tvs)
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, list)) label
	val env2 = E.ENVAPP (E.list2env envs, env1)
    in (tv, env2)
    end
  | gen_atpat _ = raise Fail "term is not an atomic pattern"

and gen_pat (A.N {kind = (A.PAT, A.PAT_TYPED), label, value, regions, parents, children = [pat, typ]}) =
    let val (tv1, env1) = gen_pat pat
	val (tv2, env2) = gen_type typ
	val tv   = E.nextItyvar ()
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv1)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv2)) label
	val env5 = E.list2env [env1, env2, env3, env4]
    in (tv, env5)
    end
  | gen_pat (A.N {kind = (A.PAT, A.PAT_AS), label, value, regions, parents, children = [id, pat]}) =
    let val (tv1, env1) = gen_id_bind_p id
	val (tv2, env2) = gen_pat pat
	val tv   = E.nextItyvar ()
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv1)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv2)) label
	val env5 = E.list2env [env1, env2, env3, env4]
    in (tv, env5)
    end
  | gen_pat (A.N {kind = (A.PAT, A.PAT_CONS), label, value, regions, parents, children = [pat1, pat2]}) =
    let val (tv1, env1) = gen_pat pat1
	val (tv2, env2) = gen_pat pat2
	(*val (ty,  env3) = E.get_type_op value label*)
	val tv   = E.nextItyvar ()
	val tv'  = E.nextItyvar ()
	val arr0 = E.mk_type_arrow (E.mk_tyvar tv2, E.mk_tyvar tv) label
	val ty   = E.mk_type_arrow (E.mk_tyvar tv1, arr0) label
	val env3 = E.mk_env_depacc (E.ACCVID (value, tv')) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', ty)) label
	val env5 = E.list2env [env1, env2, env3, env4]
    in (tv, env5)
    end
  | gen_pat (A.N {kind = (A.PAT, A.PAT_ATPAT), label, value, regions, parents, children = [atpat]}) =
    let val (tv, env) = gen_atpat atpat
	val tv'  = E.nextItyvar ()
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_tyvar tv)) label
	val env2 = E.ENVAPP (env, env1)
    in (tv', env2)
    end
(*  | gen_pat (A.N {kind = (A.PAT, A.PAT_QUOT), label, value, regions, parents, children = [pat]}) =
    let val (tv, env) = gen_pat pat
	val tv'  = E.nextItyvar ()
	val tv'' = E.nextItyvar ()
	val bag  = E.mk_type_bag (E.mk_tyvar tv'') label
	val list = E.mk_type_list (E.mk_tyvar tv'') label
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, list)) label
	val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', bag)) label
	val env3 = E.ENVAPP (env, E.ENVAPP (env1, env2))
    in (tv', env3)
    end*)
  | gen_pat (term as A.N {kind = (A.PAT, A.PAT_APP), label, value, regions, parents, children = [id, atpat]}) =
    let val (tv1, env1) = gen_id_bound_c id label
	val (tv2, env2) = gen_atpat atpat
	val tv   = E.nextItyvar ()
	val arr  = E.mk_type_arrow (E.mk_tyvar tv2, E.mk_tyvar tv) label
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr)) label
	val env4 = E.list2env [env1, env2, env3]
    in (tv, env4)
    end
  (*| gen_pat (term as A.N {kind = (A.PAT, A.PAT_APP), label, value, regions, parents, children = [pat, atpat]}) =
    let val st  = "there shouldn't be an application in a pattern"
	val env = E.ENVDEP (E.ENVERR (st, E.SYNTAX), D.mk_dep label)
	val tv  = E.nextItyvar ()
    in (tv, env)
    end*)
  (*(print (A.toString term); raise Fail )*)
  | gen_pat _ = raise Fail "term is not a pattern"

and gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_ID), label, value, regions, parents, children = [id]}) =
    let val (tv, env) = gen_id_bound id
	val tv'  = E.nextItyvar ()
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_tyvar tv)) label
	val env2 = E.ENVAPP (env, env1)
    in (tv', env2)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_SCON), label, value, regions, parents, children = [scon]}) =
    let val (tv, env) = gen_scon scon
	val tv'  = E.nextItyvar ()
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_tyvar tv)) label
	val env2 = E.ENVAPP (env, env1)
    in (tv', env2)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_TUPLE), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_exp children)
	val tv   = E.nextItyvar ()
	val list = tuple2pairs label (map E.mk_tyvar tvs)
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, list)) label
	val env2 = E.ENVAPP (E.list2env envs, env1)
    in (tv, env2)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_PAREN), label, value, regions, parents, children = [exp]}) =
    let val (tv, env) = gen_exp exp
	val tv'  = E.nextItyvar ()
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_tyvar tv)) label
	val env2 = E.ENVAPP (env, env1)
    in (tv', env2)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_LIST), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_exp children)
	val tv    = E.nextItyvar ()
	val tv'   = E.nextItyvar ()
	val envs' = map (fn tv' => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label) tvs
	val env1  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_type_list (E.mk_tyvar tv) label)) label
	val env2  = E.list2env (envs @ envs' @ [env1])
    in (tv', env2)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_BAG), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_exp children)
	val tv    = E.nextItyvar ()
	val tv'   = E.nextItyvar ()
	val envs' = map (fn tv' => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label) tvs
	val env1  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_type_bag (E.mk_tyvar tv) label)) label
	val env2  = E.list2env (envs @ envs' @ [env1])
    in (tv', env2)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_PRIOR), label, value, regions, parents, children = [exp]}) =
    let val (tv, env) = gen_exp exp
	val tv'  = E.nextItyvar ()
	val tv'' = E.nextItyvar ()
	val cls  = E.mk_type_class (E.mk_tyvar tv'') label
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', cls)) label
	val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv,  cls)) label
	val envl = E.mk_env_deplab (tv'', E.OTH) label
	val env3 = E.list2env [env, env1, env2, envl]
    in (tv', env3)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_ANY), label, value, regions, parents, children = [exp]}) =
    let val (tv, env) = gen_exp exp
	val tv'  = E.nextItyvar ()
    in (tv', env)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_MSG), label, value, regions, parents, children = [atoms, exp]}) =
    let val (tv1, env1) = gen_atoms_bound atoms
	val (tv2, env2) = gen_exp exp
	val tv   = E.nextItyvar ()
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv,  E.mk_type_msg label)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label
	val env5 = E.mk_env_deplab (tv2, E.EQD) label
	val env6 = E.list2env [env1, env2, env3, env4, env5]
    in (tv, env6)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_ONCE), label, value, regions, parents, children = [exp]}) =
    let val (tv1, env1) = gen_exp exp
	val tv   = E.nextItyvar ()
	val tv2  = E.nextItyvar ()
	val cls  = E.mk_type_class (E.mk_tyvar tv) label
	val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, cls)) label
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, cls)) label
	val envl = E.mk_env_deplab (tv, E.OTH) label
	val env4 = E.list2env [env1, env2, env3, envl]
    in (tv2, env4)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_SENDOC), label, value, regions, parents, children = [exp]}) =
    let val (tv1, env1) = gen_exp exp
	val tv   = E.nextItyvar ()
	val tv2  = E.nextItyvar ()
	val loc  = E.mk_type_loc   label
	val bag  = E.mk_type_bag   (E.mk_tyvar tv) label
	val arr  = E.mk_type_arrow (loc, bag)      label
	val cls  = E.mk_type_class (E.mk_tyvar tv) label
	val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr)) label
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, cls)) label
	val envl = E.mk_env_deplab (tv, E.OTH) label
	val env4 = E.list2env [env1, env2, env3, envl]
    in (tv2, env4)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_ONLOC), label, value, regions, parents, children = [exp]}) =
    let val (tv1, env1) = gen_exp exp
	val tv   = E.nextItyvar ()
	val tv2  = E.nextItyvar ()
	val loc  = E.mk_type_loc label
	val cls  = E.mk_type_class (E.mk_tyvar tv) label
	val arr  = E.mk_type_arrow (loc, cls) label
	val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr)) label
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, cls)) label
	val envl = E.mk_env_deplab (tv, E.OTH) label
	val env4 = E.list2env [env1, env2, env3, envl]
    in (tv2, env4)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_SKIP), label, value, regions, parents, children = [exp]}) =
    let val (tv1, env1) = gen_exp exp
	val tv   = E.nextItyvar ()
	val tv'  = E.nextItyvar ()
	val cls  = E.mk_type_class (E.mk_tyvar tv') label
	val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, cls)) label
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv1)) label
	val envl = E.mk_env_deplab (tv, E.OTH) label
	val env4 = E.list2env [env1, env2, env3, envl]
    in (tv, env4)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_STATEC), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_exp children)
	val tv0   = E.nextItyvar () (* output type, will be constrained to Class(B) *)
	val tv    = E.nextItyvar () (* type B in the rest *)
	val cls0  = E.mk_type_class (E.mk_tyvar tv) label
	val env0  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv0, cls0)) label
	val envs0 = envs @ [env0]
	(* the transition function: Loc -> A -> B -> B,
	 * the class:               Class(A),
	 * result: tv0=Class(B), where B=tv *)
	fun aux envs tvs [] = (envs, tvs)
	  | aux envs tvs (tv_tr :: tv_cls :: rest) =
	    let val tv'     = E.nextItyvar ()                                          (* A=tv'                   *)
		val arr1    = E.mk_type_arrow (E.mk_tyvar tv, E.mk_tyvar tv) label     (* B -> B                  *)
		val arr2    = E.mk_type_arrow (E.mk_tyvar tv', arr1) label             (* A -> B -> B             *)
		val tr      = E.mk_type_arrow (E.mk_type_loc label, arr2) label        (* Loc -> A -> B -> B      *)
		val env_tr  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv_tr, tr)) label   (* tr : Loc -> A -> B -> B *)
		val cls     = E.mk_type_class (E.mk_tyvar tv') label                   (* Class(A)                *)
		val env_cls = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv_cls, cls)) label (* cls : Class(A)          *)
	    in aux (envs @ [env_tr, env_cls]) (tvs @ [tv']) rest
	    end
	  | aux _ _ _ = raise Fail "gen_atexp:STATEC"
    in case tvs of
	   (tv_init :: rest) =>
	   let val init = E.mk_type_arrow (E.mk_type_loc label, E.mk_tyvar tv) label (* Loc -> B                *)
	       val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv_init, init)) label (* init : Loc -> B         *)
	       val (envs1, tvs) = aux (envs0 @ [env1]) [] rest
	       val tup  = E.mk_type_tuple (map E.mk_tyvar (tv0 :: tvs)) label
	       val tvL  = E.nextItyvar ()
	       val envL = E.mk_env_depcst (E.CSITY (E.mk_tyvar tvL, tup)) label
	       val envl = E.mk_env_deplab (tvL, E.OTH) label
	   in (tv0, E.list2env (envL :: envl :: envs1))
	   end
	 | _ => raise Fail "gen_atexp:STATEC"
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_MEMORYC), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_exp children)
	val tv0   = E.nextItyvar () (* output type, will be constrained to Class(B) *)
	val tv    = E.nextItyvar () (* type B in the rest *)
	val cls0  = E.mk_type_class (E.mk_tyvar tv) label
	val env0  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv0, cls0)) label
	val envs0 = envs @ [env0]
	(* the transition function: Loc -> A -> B -> B,
	 * the class:               Class(A),
	 * result: tv0=Class(B), where B=tv *)
	fun aux envs tvs [] = (envs, tvs)
	  | aux envs tvs (tv_tr :: tv_cls :: rest) =
	    let val tv'     = E.nextItyvar ()                                          (* A=tv'                   *)
		val arr1    = E.mk_type_arrow (E.mk_tyvar tv, E.mk_tyvar tv) label     (* B -> B                  *)
		val arr2    = E.mk_type_arrow (E.mk_tyvar tv', arr1) label             (* A -> B -> B             *)
		val tr      = E.mk_type_arrow (E.mk_type_loc label, arr2) label        (* Loc -> A -> B -> B      *)
		val env_tr  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv_tr, tr)) label   (* tr : Loc -> A -> B -> B *)
		val cls     = E.mk_type_class (E.mk_tyvar tv') label                   (* Class(A)                *)
		val env_cls = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv_cls, cls)) label (* cls : Class(A)          *)
	    in aux (envs @ [env_tr, env_cls]) (tvs @ [tv']) rest
	    end
	  | aux _ _ _ = raise Fail "gen_atexp:MEMORYC"
    in case tvs of
	   (tv_init :: rest) =>
	   let val init = E.mk_type_arrow (E.mk_type_loc label, E.mk_tyvar tv) label (* Loc -> B                *)
	       val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv_init, init)) label (* init : Loc -> B         *)
	       val (envs1, tvs) = aux (envs0 @ [env1]) [] rest
	       val tup  = E.mk_type_tuple (map E.mk_tyvar (tv0 :: tvs)) label
	       val tvL  = E.nextItyvar ()
	       val envL = E.mk_env_depcst (E.CSITY (E.mk_tyvar tvL, tup)) label
	       val envl = E.mk_env_deplab (tvL, E.OTH) label
	   in (tv0, E.list2env (envL :: envl :: envs1))
	   end
	 | _ => raise Fail "gen_atexp:MEMORYC"
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_MINUS), label, value, regions, parents, children = [atexp]}) =
    let val (tv1, env1) = gen_atexp atexp
	val tv   = E.nextItyvar ()
	val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_type_int label)) label
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv,  E.mk_tyvar tv1)) label
	val env4 = E.list2env [env1, env2, env3]
    in (tv, env4)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_TYPE), label, value, regions, parents, children = [typ]}) =
    let val (tv1, env1) = gen_type typ
	val tv   = E.nextItyvar ()
	val ity  = E.ITYTYP (E.mk_tyvar tv1, label)
	val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, ity)) label
	val env3 = E.mk_env_deplab (tv1, E.OTH) label
	val env4 = E.list2env [env1, env2, env3]
    in (tv, env4)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_WAIT), label, value, regions, parents, children = [time, exp]}) =
    let val (tv1, env1) = gen_exp time
	val (tv2, env2) = gen_exp exp
	val tv   = E.nextItyvar ()
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_type_int label)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv2)) label
	val env5 = E.list2env [env1, env2, env3, env4]
    in (tv, env5)
    end
  | gen_atexp (A.N {kind = (A.ATEXP, A.ATEXP_NULL), label, value, regions, parents, children = []}) =
    let val tv   = E.nextItyvar ()
	val tv'  = E.nextItyvar ()
	val cls  = E.mk_type_class (E.mk_tyvar tv) label
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', cls)) label
	val envl = E.mk_env_deplab (tv, E.OTH) label
	val env2 = E.list2env [env1, envl]
    in (tv', env2)
    end
  | gen_atexp _ = raise Fail "term is not an atomic expression"

and gen_exp (A.N {kind = (A.EXP, A.EXP_OR), label, value, regions, parents, children = [exp1, exp2]}) =
    let val (tv1, env1) = gen_exp exp1
	val (tv2, env2) = gen_exp exp2
	val tv   = E.nextItyvar ()
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv1)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv2)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_type_bool label)) label
	val env6 = E.list2env [env1, env2, env3, env4, env5]
    in (tv, env6)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_AND), label, value, regions, parents, children = [exp1, exp2]}) =
    let val (tv1, env1) = gen_exp exp1
	val (tv2, env2) = gen_exp exp2
	val tv   = E.nextItyvar ()
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv1)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv2)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_type_bool label)) label
	val env6 = E.list2env [env1, env2, env3, env4, env5]
    in (tv, env6)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_TYPED), label, value, regions, parents, children = [exp, typ]}) =
    let val (tv1, env1) = gen_exp exp
	val (tv2, env2) = gen_type typ
	val tv   = E.nextItyvar ()
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv1)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv2)) label
	val env5 = E.list2env [env1, env2, env3, env4]
    in (tv, env5)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_LAMBDA), label, value, regions, parents, children = [pat, exp]}) =
    let val (tv1, env1) = gen_pat pat
	val (tv2, env2) = gen_exp exp
	val tv   = E.nextItyvar ()
	val ev   = E.nextEnvvar ()
	val aty  = E.mk_type_arrow (E.mk_tyvar tv1, E.mk_tyvar tv2) label
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, aty)) label
	val env4 = E.ENVCST (E.CSENV (ev, env1))
	val env5 = E.mk_env_depvar ev label
	val eenv = onlyVariables pat
	val env6 = locCst (E.list2env [eenv, env4, env5, env2, env3])
    in (tv, env6)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_ITE), label, value, regions, parents, children = [exp1, exp2, exp3]}) =
    let val (tv1, env1) = gen_exp exp1
	val (tv2, env2) = gen_exp exp2
	val (tv3, env3) = gen_exp exp3
	val tv   = E.nextItyvar ()
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv2)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv3)) label
	val env6 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_type_bool label)) label
	val env7 = E.list2env [env1, env2, env3, env4, env5, env6]
    in (tv, env7)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_ATEXP), label, value, regions, parents, children = [atexp]}) =
    let val (tv, env) = gen_atexp atexp
	val tv'  = E.nextItyvar ()
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_tyvar tv)) label
	val env2 = E.ENVAPP (env, env1)
    in (tv', env2)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_APP), label, value, regions, parents, children = [exp, atexp]}) =
    let val (tv1, env1) = gen_exp exp
	val (tv2, env2) = gen_atexp atexp
	val tv   = E.nextItyvar ()
	val tv2' = E.nextItyvar ()
	val ity  = E.mk_tyvar tv2'
	val aty  = E.mk_type_arrow (ity, E.mk_tyvar tv) label
	val env3 = E.mk_env_depcst (E.CSITY  (E.mk_tyvar tv1, aty)) label
	val env4 = E.mk_env_depsub (E.SUBITY (E.mk_tyvar tv2, ity)) label
	val env5 = E.list2env [env1, env2, env3, env4]
    in (tv, env5)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_OP), label, value, regions, parents, children = [exp1, exp2]}) =
    let val (tv1, env1) = gen_exp exp1
	val (tv2, env2) = gen_exp exp2
	(*val (ty, env3)  = E.get_type_op value label*)
	val tv   = E.nextItyvar ()
	val tv'  = E.nextItyvar ()
	(* we get something like:
	 *   tv' = tv1 -> tv2 -> tv
	 * except for = where we get:
	 *   tv' = tv1 Deq *)
	val env3 = acc2op value label tv1 tv2 tv tv'
	val env4 = E.mk_env_depacc (E.ACCVID (value, tv')) label
	val env5 = E.list2env [env1, env2, env3, env4]
    in (tv, env5)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_LET), label, value, regions, parents, children = [bind, exp]}) =
    let val (env1, env2) = gen_bind false bind
	val (tv, env) = gen_exp exp
	val ev   = E.nextEnvvar ()
	val tv'  = E.nextItyvar ()
	val enve = bindExplicitTyVars [bind, exp] E.EXPLICIT
	val env3 = E.mk_env_loc (enve, E.ENVPOL (E.ENVAPP (env2, env1), E.NES))
	(*val _  = print (toStringEnv env3 ^ "\n\n")*)
	val env4 = E.mk_env_depvar ev label
	val env5 = E.ENVAPP (E.ENVCST (E.CSENV (ev, env3)), env4)
	val env6 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
	val env7 = locCst (E.list2env [env5, env6, env])
    in (tv', env7)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_CLASS), label, value, regions, parents, children = [bind, exp]}) =
    let val (env1, env2) = gen_bind true bind
	val (tv, env) = gen_exp exp
	val ev   = E.nextEnvvar ()
	val tv'  = E.nextItyvar ()
	val enve = bindExplicitTyVars [bind, exp] E.EXPLICIT
	val env3 = E.mk_env_loc (enve, E.ENVPOL (E.ENVAPP (env2, env1), E.NES))
	(*val _  = print (toStringEnv env3 ^ "\n\n")*)
	val env4 = E.mk_env_depvar ev label
	val env5 = E.ENVAPP (E.ENVCST (E.CSENV (ev, env3)), env4)
	val env6 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
	val env7 = locCst (E.list2env [env5, env6, env])
    in (tv', env7)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_LETR), label, value, regions, parents, children = [bind, exp]}) =
    let val (env1, env2) = gen_bind false bind
	val (tv, env) = gen_exp exp
	val ev   = E.nextEnvvar ()
	val tv'  = E.nextItyvar ()
	val enve = bindExplicitTyVars [bind, exp] E.EXPLICIT
	val env3 = E.mk_env_loc (enve, E.ENVPOL (E.ENVAPP (env1, env2), E.NES))
	val env4 = E.mk_env_depvar ev label
	val env5 = E.ENVAPP (E.ENVCST (E.CSENV (ev, env3)), env4)
	val env6 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
	val eenv = isBindFunction bind label
	val env7 = locCst (E.list2env [eenv, env5, env6, env])
    in (tv', env7)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_WHERE), label, value, regions, parents, children = [atexp, bind]}) =
    let val (env1, env2) = gen_bind false bind
	val (tv, env) = gen_atexp atexp
	val ev   = E.nextEnvvar ()
	val tv'  = E.nextItyvar ()
	val enve = bindExplicitTyVars [bind, atexp] E.EXPLICIT
	val env3 = E.mk_env_loc (enve, E.ENVPOL (E.ENVAPP (env2, env1), E.NES))
	val env4 = E.mk_env_depvar ev label
	val env5 = E.ENVAPP (E.ENVCST (E.CSENV (ev, env3)), env4)
	val env6 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label
	val env7 = locCst (E.ENVAPP (env5, E.ENVAPP (env6, env)))
    in (tv', env7)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_BINDING), label, value, regions, parents, children = [exp1, pat, exp2]}) =
    let val (tv1, env1) = gen_exp exp1
	val (tv2, env2) = gen_pat pat
	val (tv3, env3) = gen_exp exp2
	val tv   = E.nextItyvar ()
	val tv'  = E.nextItyvar ()
	val ev   = E.nextEnvvar ()
	val caty = E.mk_type_class (E.mk_tyvar tv2) label
	val cbty = E.mk_type_class (E.mk_tyvar tv') label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, caty)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv3)) label
	val env6 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, cbty)) label
	val env7 = E.ENVCST (E.CSENV (ev, env2))
	val env8 = E.mk_env_depvar ev label
	val eenv = onlyVariables pat
	(* This last part is to keep track of the type:
	 *   (tv2 -> tv3) which is equal to (tv2 -> (tv' Class)),
	 * so that we can use the generated type info. *)
	val tv'' = E.nextItyvar ()
	val arr  = E.mk_type_arrow (E.mk_tyvar tv2, cbty) label
	val depc = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv'', arr)) label
	val envl = E.mk_env_deplab (tv'', E.OTH) label
	(* And finally we return the following environment: *)
	val env9 = locCst (E.list2env [eenv, env1, env4, env5, env6, env7, env8, env3, envl])
    in (tv, env9)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_MBIND), label, value, regions, parents, children = [exp1, exp2]}) =
    let val (tv1, env1) = gen_exp exp1
	val (tv2, env2) = gen_exp exp2
	val tv   = E.nextItyvar ()
	val tva  = E.nextItyvar ()
	val tvb  = E.nextItyvar ()
	val caty = E.mk_type_class (E.mk_tyvar tva) label
	val cbty = E.mk_type_class (E.mk_tyvar tvb) label
	val tfun = E.mk_type_arrow (E.mk_tyvar tva, cbty) label
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, caty)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, tfun)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv,  cbty)) label
	val envl = E.mk_env_deplab (tv2, E.OTH) label
	val env6 = locCst (E.list2env [env1, env2, env3, env4, env5, envl])
    in (tv, env6)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_COMP), label, value, regions, parents, children = (exp :: exps)}) =
    let val (tv1, env1) = gen_exp exp
	val (tvs, envs) = ListPair.unzip (map gen_exp exps)
	val tv   = E.nextItyvar ()
	val tv'  = E.nextItyvar ()
	val bp   = String.isSubstring "P" value
	val bf   = String.isSubstring "F" value
	val bc   = String.isSubstring "C" value
	val bo   = String.isSubstring "O" value
	val tyv  = E.mk_tyvar tv
	val endT = if bf then tyv else E.mk_type_bag tyv label
	val last = if bc then E.mk_type_bag endT label else endT
	val init = if bp then E.mk_type_arrow (endT, last) label else last
	val cls  = E.mk_type_class (E.mk_tyvar tv) label
	val (tvs', envo) =
	    if bo andalso not (List.null tvs)
	    then let val rev    = List.rev tvs
		     val last   = List.hd rev
		     val last_e = List.last exps
		     val firsts = List.rev (List.tl rev)
		     val bag    = E.mk_type_bag tyv label
		     val typ    =
			 (* We do that because we want to be able to write
			  * Prior(self)?{..}, instead of Prior(self)?(\_.{..})
			  *)
			 if A.isExpBag last_e
			 then bag
			 else E.mk_type_arrow (E.mk_type_loc label, bag) label
		     val env    = E.mk_env_depcst (E.CSITY (E.mk_tyvar last, typ)) label
		 in (firsts, env)
		 end
	    else (tvs, E.ENVNUL)
	val (aty, envs') =
	    foldr (fn (tv, (ty, envs)) =>
		      let val tv'  = E.nextItyvar ()
			  val tyv' = E.mk_tyvar tv'
			  val ty'  = if bf then tyv' else E.mk_type_bag tyv' label
			  val cls  = E.mk_type_class tyv' label
			  val env  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, cls)) label
		      in (E.mk_type_arrow (ty', ty) label, env :: envs)
		      end)
		  (init, [])
		  tvs'
	val env  = E.list2env (envo :: envs' @ envs)
	val aty' = E.mk_type_arrow (E.mk_type_loc label, aty) label
	val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, aty')) label
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', cls)) label
	val envl = E.mk_env_deplab (tv', E.OTH) label
	val env4 = E.list2env [env1, env, env2, env3, envl]
    in (tv', env4)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_CASE), label, value, regions, parents, children = [exp, match]}) =
    let val (tv1, env1) = gen_exp exp
	val tv   = E.nextItyvar () (* type of the expressions in the match *)
	val tv'  = E.nextItyvar () (* type of the patterns in the match    *)
	val cenv = checkCasePatSyntax match label tv1 (A.getLabel exp)
	val env  = foldr (fn ((tvp, tve, enve), env) =>
			     let val enva = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv,  E.mk_tyvar tve)) label
				 val envb = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_tyvar tvp)) label
			     in E.list2env [env, enva, envb, enve]
			     end)
			 E.ENVNUL
			 (gen_match match)
	val env' = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv')) label
	(* NOTE: The following ENVLAB is generated because MATCHs are transformed
	 * and equalities on the exp and the patterns of the pats of the mrules
	 * are generated.  We want to know the type in which the equalities are. *)
	val envs = E.list2env [env1, env, env', cenv]
    in (tv, envs)
    end
  | gen_exp (A.N {kind = (A.EXP, A.EXP_QUOT), label, value, regions, parents, children = [atexp]}) =
    let val (tv, env) = gen_atexp atexp
	val tv'  = E.nextItyvar ()
	val tv'' = E.nextItyvar ()
	val bag  = E.mk_type_bag  (E.mk_tyvar tv'') label
	val list = E.mk_type_list (E.mk_tyvar tv'') label
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, list)) label
	val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', bag)) label
	val env3 = E.ENVAPP (env, E.ENVAPP (env1, env2))
    in (tv', env3)
    end
  | gen_exp (exp as A.N {kind = (A.PROP, _), label, value, regions, parents, children}) = gen_prop exp
  | gen_exp term = (print (A.toString term); raise Fail "term is not an expression")

and gen_casepat (A.N {kind = (A.CASEPAT, A.CASEPAT_PAT), label, value, regions, parents, children = [pat]}) =
    let val (tv, env) = gen_pat pat
	val tv'  = E.nextItyvar ()
	val env' = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_tyvar tv)) label
    in (tv', E.ENVAPP (env', env))
    end
  | gen_casepat term = (print (A.toString term); raise Fail "term is not an case pattern")

(* last is true if the mrule is the last of match *)
and gen_mrule (mrule as A.N {kind = (A.MRULE, A.MRULE_M), label, value, regions, parents, children = [pat, exp]}) =
    let val (tv1, env1) = gen_casepat pat
	val (tv2, env2) = gen_exp exp
	val tv1' = E.nextItyvar ()
	val tv2' = E.nextItyvar ()
	val ev   = E.nextEnvvar ()
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1', E.mk_tyvar tv1)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2', E.mk_tyvar tv2)) label
	val env5 = E.ENVCST (E.CSENV (ev, E.list2env [env3, env1]))
	val env6 = E.ENVAPP (env5, E.mk_env_depvar ev label)
    in (tv1', tv2', E.mk_env_loc (env6, E.ENVAPP (env4, env2)))
    end
  | gen_mrule term = (print (A.toString term); raise Fail "term is not an match rule")

and gen_match (A.N {kind = (A.MATCH, A.MATCH_M), label, value, regions, parents, children}) =
    map gen_mrule children
  | gen_match _ = raise Fail "term is not a matching"
(* TODO: we need to use these labels, otherwise we obtain strange slices
 * where the branches don't seem related to the match/case. *)

(* in gen_bind the first parameter (cls) is to check whether the binding
 * binds a class *)
and gen_bind cls (A.N {kind = (A.BIND, A.BIND_DEC), label, value, regions, parents, children = [f, args, exp]}) =
    let val (tv1, env1) = gen_id_bind f
	val (tvs, envs) = gen_param args
	val (tv2, env2) = gen_exp exp
	(* NOTE: there shoudn't be any accessor in envs so it should be okay
	 * to have it depend on env1. *)
	val ev   = E.nextEnvvar ()
	val ty   = foldr (fn (tv, ty) => E.mk_type_arrow (E.mk_tyvar tv, ty) label) (E.mk_tyvar tv2) tvs
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, ty)) label
	(* NOTE: here we don't want args to contain constructors or lists. *)
	val eenv = onlyVariables args
	val cenv = if cls then [toTyClass tv2 label] else []
	val env4 = E.list2env ([env1, env3, eenv] @ cenv)
	val env5 = E.ENVCST (E.CSENV (ev, E.list2env envs))
	val env6 = E.mk_env_depvar ev label
	val env7 = locCst (E.list2env [env5, env6, env2])
    in (env4, env7)
    end
  | gen_bind cls (term as A.N {kind = (A.BIND, A.BIND_TDEC), label, value, regions, parents, children = [f, args, typ, exp]}) =
    let val (tv1, env1) = gen_id_bind f
	val (tvs, envs) = gen_param args
	val (tvt, envt) = gen_type typ
	val (tv2, env2) = gen_exp exp
	val ev   = E.nextEnvvar ()
	val ty   = foldr (fn (tv, ty) => E.mk_type_arrow (E.mk_tyvar tv, ty) label) (E.mk_tyvar tv2) tvs
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, ty)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, E.mk_tyvar tvt)) label
	(* NOTE: here we don't want args to contain constructors or lists. *)
	val eenv = onlyVariables args
	val cenv = if cls then [toTyClass tv2 label] else []
	val env5 = E.list2env ([env1, env3, env4, eenv] @ cenv)
	val env6 = E.ENVCST (E.CSENV (ev, E.list2env envs))
	val env7 = E.mk_env_depvar ev label
	val env8 = locCst (E.list2env [env6, env7, env5, envt, env2])
    in (env5, env8)
    end
  | gen_bind cls (term as A.N {kind = (A.BIND, A.BIND_PAT), label, value, regions, parents, children = [pat, exp]}) =
    let val (tv1, env1) = gen_pat pat
	val (tv2, env2) = gen_exp exp
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label
	(* NOTE: here we don't want pat to contain constructors or lists. *)
	val env4 = E.mk_env_depbin (E.BINDVID (value, label, E.mk_new_vscheme tv1)) label
	val eenv = onlyVariables pat
	val cenv = if cls then [toTyClass tv2 label] else []
	val env5 = E.list2env ([env1, env3, eenv, env4] @ cenv)
    in (env5, env2)
    end
  | gen_bind cls (A.N {kind = (A.BIND, A.BIND_IOP), label, value, regions, parents, children = [id, arg1, arg2, exp]}) =
    let val (tv1, env1) = gen_id_bind id
	val (tv2, env2) = gen_atpat arg1
	val (tv3, env3) = gen_atpat arg2
	val (tv4, env4) = gen_exp exp
	val ev    = E.nextEnvvar ()
	val arr0  = E.mk_type_arrow (E.mk_tyvar tv3, E.mk_tyvar tv4) label
	val ity   = E.mk_type_arrow (E.mk_tyvar tv2, arr0) label
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, ity)) label
	val env6  = E.ENVAPP (env1, env5)
	val eenv1 = onlyVariables arg1
	val eenv2 = onlyVariables arg2
	val cenv  = if cls then [toTyClass tv4 label] else []
	val env7  = E.ENVCST (E.CSENV (ev, E.list2env ([eenv1, eenv2, env2, env3] @ cenv)))
	val env8  = E.mk_env_depvar ev label
	val env9  = locCst (E.list2env [env7, env8, env4])
    in (env6, env9)
    end
  | gen_bind cls (A.N {kind = (A.BIND, A.BIND_TIOP), label, value, regions, parents, children = [id, arg1, arg2, typ, exp]}) =
    let val (tv1, env1) = gen_id_bind id
	val (tv2, env2) = gen_atpat arg1
	val (tv3, env3) = gen_atpat arg2
	val (tv4, env4) = gen_type typ
	val (tv5, env5) = gen_exp exp
	val ev    = E.nextEnvvar ()
	val arr0  = E.mk_type_arrow (E.mk_tyvar tv3, E.mk_tyvar tv5) label
	val ity   = E.mk_type_arrow (E.mk_tyvar tv2, arr0) label
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv4, E.mk_tyvar tv5)) label
	val env7  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, ity)) label
	val env8  = E.list2env [env6, env7, env4, env1]
	val eenv1 = onlyVariables arg1
	val eenv2 = onlyVariables arg2
	val cenv  = if cls then [toTyClass tv5 label] else []
	val env9  = E.ENVCST (E.CSENV (ev, E.list2env ([eenv1, eenv2, env2, env3] @ cenv)))
	val env10 = E.mk_env_depvar ev label
	val env11 = E.mk_env_loc (E.ENVAPP (env9, env10), env5)
    in (env8, env11)
    end
  | gen_bind _ _ = raise Fail "term is not a binding"

(* NOTE: not using label *)
and gen_param (term as A.N {kind = (A.PARAM, A.PARAM_P), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_atpat children)
	val tvs'  = List.tabulate (List.length tvs, fn _ => E.nextItyvar ())
	val envs' = map (fn (tv, tv') => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv')) label) (ListPair.zip (tvs', tvs))
    in (tvs', envs' @ envs)
    end
  | gen_param _ = raise Fail "term is not a list of parameters"

(* TODO: for that one we want an error env. *)
and gen_parse _ = raise Fail "gen_parse"

and gen_atoms_bind (A.N {kind = (A.ATOMS, A.ATOMS_ATOMS), label, value, regions, parents, children = []}) =
    let val tv  = E.nextItyvar ()
	val env = E.mk_env_depbin (E.BINDATM (value, label, E.mk_new_vscheme tv)) label
	val hdr = (value, D.singleton (D.mk_dep label))
    in (tv, hdr, env)
    end
  | gen_atoms_bind (A.N {kind = (A.ATOMS, A.ATOMS_LIST), label, value, regions, parents, children = []}) =
    let val tv  = E.nextItyvar ()
	val env = E.mk_env_depbin (E.BINDATM (value, label, E.mk_new_vscheme tv)) label
	val hdr = (value, D.singleton (D.mk_dep label))
    in (tv, hdr, env)
    end
  | gen_atoms_bind _ = raise Fail "term is not an atom list"

and gen_atoms_bound (A.N {kind = (A.ATOMS, A.ATOMS_ATOMS), label, value, regions, parents, children}) =
    let val tv  = E.nextItyvar ()
	val env = E.mk_env_depacc (E.ACCATM (value, tv)) label
    in (tv, env)
    end
  | gen_atoms_bound (A.N {kind = (A.ATOMS, A.ATOMS_LIST), label, value, regions, parents, children}) =
    let val tv  = E.nextItyvar ()
	val env = E.mk_env_depacc (E.ACCATM (value, tv)) label
    in (tv, env)
    end
  | gen_atoms_bound (A.N {kind = (A.ATOMS, A.ATOMS_WILD), label, value, regions, parents, children}) =
    let val tv  = E.nextItyvar ()
    in (tv, E.ENVNUL)
    end
  | gen_atoms_bound _ = raise Fail "term is not an atom list"

(* NOTE: right now we don't do anything with id. *)
and gen_arg (A.N {kind = (A.ARG, A.ARG_A), label, value, regions, parents, children = [id, args, ty]}) =
    let val (tv1, tv1', env1) = gen_args args
	val (tv2, env2) = gen_type ty
	val tv   = E.nextItyvar ()
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv,   E.mk_tyvar tv1)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1', E.mk_tyvar tv2)) label
	val env5 = E.list2env [env1, env2, env3, env4]
    in (tv, env5)
    end
  | gen_arg (A.N {kind = (A.ARG, A.ARG_T), label, value, regions, parents, children = [tyvar]}) =
    let val (tv, env) = gen_id_bound tyvar
	val tv'  = E.nextItyvar ()
	val ity  = E.ITYTYP (E.mk_tyvar tv, label)
	val env' = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', ity)) label
    in (tv', E.list2env [env, env'])
    end
  | gen_arg _ = raise Fail "term is not a constant argument"

and gen_args (A.N {kind = (A.ARGS, A.ARGS_EM), label, value, regions, parents, children = []}) =
    let val tv = E.nextItyvar ()
    in (tv, tv, E.ENVNUL)
    end
  | gen_args (A.N {kind = (A.ARGS, A.ARGS_PSEQ), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_arg children)
	val tv  = E.nextItyvar ()
	val tv' = E.nextItyvar ()
	val ity = foldr (fn (tv, ity) => E.mk_type_arrow (E.mk_tyvar tv, ity) label) (E.mk_tyvar tv) tvs
	val env = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', ity)) label
    in (tv', tv, E.list2env (env :: envs))
    end
  | gen_args (A.N {kind = (A.ARGS, A.ARGS_LSEQ), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_arg children)
	val tv  = E.nextItyvar ()
	val tv' = E.nextItyvar ()
	val ity = foldr (fn (tv, ity) => E.mk_type_arrow (E.mk_tyvar tv, ity) label) (E.mk_tyvar tv) tvs
	val env = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', ity)) label
    in (tv', tv, E.list2env (env :: envs))
    end
  | gen_args _ = raise Fail "term is not a constant argument"

and gen_prop (A.N {kind = (A.PROP, A.PROP_EXP), label, value, regions, parents, children = [exp]}) =
    let val (tv, env) = gen_exp exp
	val tv'  = E.nextItyvar ()
	val idor = E.nextIdor ()
	val bty  = E.mk_type_bool label
	val pty  = E.mk_type_prop label
	val ity  = E.mk_type_or (E.ITYSEQSEQ ([bty, pty], label)) idor label
	(*val ity  = bty*)
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv,  ity)) label
	val env2 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', pty)) label
	val env3 = E.mk_env_deplab (tv, E.OTH) label
    in (tv', E.list2env [env, env1, env2, env3])
    end
  | gen_prop (A.N {kind = (A.PROP, A.PROP_OR), label, value, regions, parents, children = [prop1, prop2]}) =
    let val (tv1, env1) = gen_prop prop1
	val (tv2, env2) = gen_prop prop2
	val tv   = E.nextItyvar ()
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv1)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv2)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_type_prop label)) label
	val env6 = E.list2env [env1, env2, env3, env4, env5]
    in (tv, env6)
    end
  | gen_prop (A.N {kind = (A.PROP, A.PROP_AND), label, value, regions, parents, children = [prop1, prop2]}) =
    let val (tv1, env1) = gen_prop prop1
	val (tv2, env2) = gen_prop prop2
	val tv   = E.nextItyvar ()
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv1)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv2)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_type_prop label)) label
	val env6 = E.list2env [env1, env2, env3, env4, env5]
    in (tv, env6)
    end
  | gen_prop (A.N {kind = (A.PROP, A.PROP_IMP), label, value, regions, parents, children = [prop1, prop2]}) =
    let val (tv1, env1) = gen_prop prop1
	val (tv2, env2) = gen_prop prop2
	val tv   = E.nextItyvar ()
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv1)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv2)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_type_prop label)) label
	val env6 = E.list2env [env1, env2, env3, env4, env5]
    in (tv, env6)
    end
  | gen_prop (A.N {kind = (A.PROP, A.PROP_IFF), label, value, regions, parents, children = [prop1, prop2]}) =
    let val (tv1, env1) = gen_prop prop1
	val (tv2, env2) = gen_prop prop2
	val tv   = E.nextItyvar ()
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv1)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv2)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_type_prop label)) label
	val env6 = E.list2env [env1, env2, env3, env4, env5]
    in (tv, env6)
    end
  | gen_prop (A.N {kind = (A.PROP, A.PROP_ALL), label, value, regions, parents, children = [id, typ, prop]}) =
    let val (tvs, env1) = gen_ids_bind id
	val (tv2, env2) = gen_type typ
	val (tv3, env3) = gen_prop prop
	val tv    = E.nextItyvar ()
	val ev    = E.nextEnvvar ()
	val ty2   = E.mk_tyvar tv2
	val envs  = map (fn v => E.mk_env_depcst (E.CSITY (E.mk_tyvar v, ty2)) label) tvs
	val env4  = E.list2env envs
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv,  E.mk_type_prop label)) label
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_type_prop label)) label
	val env7  = E.ENVCST (E.CSENV (ev, env1))
	val env8  = E.mk_env_depvar ev label
	val env9  = E.mk_env_deplab (tv2, E.OTH) label
	val env10 = locCst (E.list2env [env9, env2, env4, env5, env6, env7, env8, env3])
    in (tv, env10)
    end
  | gen_prop (A.N {kind = (A.PROP, A.PROP_EX), label, value, regions, parents, children = [id, typ, prop]}) =
    let val (tvs, env1) = gen_ids_bind id
	val (tv2, env2) = gen_type typ
	val (tv3, env3) = gen_prop prop
	val tv    = E.nextItyvar ()
	val ev    = E.nextEnvvar ()
	val ty2   = E.mk_tyvar tv2
	val envs  = map (fn v => E.mk_env_depcst (E.CSITY (E.mk_tyvar v, ty2)) label) tvs
	val env4  = E.list2env envs
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv,  E.mk_type_prop label)) label
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_type_prop label)) label
	val env7  = E.ENVCST (E.CSENV (ev, env1))
	val env8  = E.mk_env_depvar ev label
	val env9  = E.mk_env_deplab (tv2, E.OTH) label
	val env10 = locCst (E.list2env [env9, env2, env4, env5, env6, env7, env8, env3])
    in (tv, env10)
    end
  | gen_prop (A.N {kind = (A.PROP, A.PROP_OBS), label, value, regions, parents, children = [class, v, event]}) =
    let val (tv1, env1) = gen_atexp class
	val (tv2, env2) = gen_atexp v
	val (tv3, env3) = gen_atexp event
	val tv   = E.nextItyvar ()
	val cls  = E.mk_type_class (E.mk_tyvar tv2) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, cls))                   label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_type_event label)) label
	val env6 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv,  E.mk_type_prop label))  label
	val env7 = E.mk_env_deplab (tv2, E.OTH) label
	val env8 = E.list2env [env1, env2, env3, env4, env5, env6, env7]
    in (tv, env8)
    end
  | gen_prop (A.N {kind = (A.PROP, A.PROP_PAREN), label, value, regions, parents, children = [prop]}) =
    let val (tv, env) = gen_prop prop
	val tv'  = E.nextItyvar ()
	val env1 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv', E.mk_tyvar tv)) label
	val env2 = E.ENVAPP (env, env1)
    in (tv', env2)
    end
  | gen_prop term = (print (A.toString term); raise Fail "term is not a proposition")

and gen_data (A.N {kind = (A.DATA, A.DATA_CONS), label, value, regions, parents, children = [id, typ]}) =
    let val name   = A.getIdIdent id
	val id_is  = A.mk_term A.ID_VID label ("is_"  ^ name) [] [] []
	val id_get = A.mk_term A.ID_VID label ("get_" ^ name) [] [] []
	val (tv1, env1) = gen_id_bind id
	val (tv2, env2) = gen_type typ
	val (tv3, env3) = gen_id_bind id_is
	val (tv4, env4) = gen_id_bind id_get
	val tv   = E.nextItyvar ()
	val arr1 = E.mk_type_arrow (E.mk_tyvar tv2, E.mk_tyvar tv) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr1)) label
	val arr2 = E.mk_type_arrow (E.mk_tyvar tv, E.mk_type_bool label) label
	val env6 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, arr2)) label
	val arr3 = E.mk_type_arrow (E.mk_tyvar tv, E.mk_tyvar tv2) label
	val env7 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv4, arr3)) label
	val env8 = E.mk_env_deplab (tv2, E.OTH) label
    in (tv, E.list2env [env5, env6, env7, env8, env2, env1, env3, env4])
    end
  | gen_data _ = raise Fail "term is not a data"

and gen_binds defs (A.N {kind = (A.BINDS, A.BINDS_LIST), label, value, regions, parents, children}) =
    foldr (fn (bind, env) =>
	      let val (envpat, envexp) = gen_bind false bind
		  val ev   = E.nextEnvvar ()
		  val enve = bindExplicitTyVars [bind] E.EXPLICIT
		  val env1 = E.mk_env_loc (enve, E.ENVPOL (E.ENVAPP (envexp, envpat), E.NES))
		  val env2 = E.mk_env_depvar ev label
		  val envc = checkRebindTopDec label defs bind
		  val env3 = E.ENVCST (E.CSENV (ev, env1))
	      in E.list2env [envc, env3, env2, env]
	      end)
	  E.ENVNUL
	  children
  | gen_binds _ _ = raise Fail "term is not a binding sequence"

and gen_incparm (A.N {kind = (A.INCPARM, A.INCPARM_EXP), label, value, regions, parents, children = [id, exp]}) =
    let val (tv1, env1) = gen_atexp exp
	val (tv2, env2) = gen_id_bound id
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label
	val deps = D.add (D.singleton (D.mk_dep (A.getLabel id)), D.mk_dep label)
    in (0, A.getIdIdent id, deps, E.ENVNUL (*E.list2env [env1, env2, env3]*))
    end
  | gen_incparm (A.N {kind = (A.INCPARM, A.INCPARM_TYP), label, value, regions, parents, children = [id, typ]}) =
    let val (tv1, env1) = gen_type typ
	val deps = D.add (D.singleton (D.mk_dep (A.getLabel id)), D.mk_dep label)
	val envl = E.mk_env_deplab (tv1, E.OTH) label
    in (1, A.getIdIdent id, deps, E.list2env [env1, envl])
    end
  | gen_incparm (A.N {kind = (A.INCPARM, A.INCPARM_INT), label, value, regions, parents, children = [id, atoms]}) =
    let val deps = D.add (D.singleton (D.mk_dep (A.getLabel id)), D.mk_dep label)
    in (3, A.getIdIdent id, deps, E.ENVNUL)
    end
  | gen_incparm _ = raise Fail "term is not a include parameter"

and gen_incparms (A.N {kind = (A.INCPARMS, A.INCPARMS_P), label, value, regions, parents, children}) =
    map gen_incparm children
  | gen_incparms _ = raise Fail "term is not a include parameter list"

(* NOTE: In gen_dec, the extra parameter is to check whether a header is
 * mapped to 2 different types.  Also to ensure that the program does not
 * rebind parameters and constants. *)
and gen_dec defs (A.N {kind = (A.DEC, A.DEC_LET), label, value, regions, parents, children = [bind]}) =
    let val (env1, env2) = gen_bind false bind
	val ev   = E.nextEnvvar ()
	val enve = bindExplicitTyVars [bind] E.EXPLICIT
	val env3 = E.mk_env_loc (enve, E.ENVPOL (E.ENVAPP (env2, env1), E.TOP))
	val env4 = E.mk_env_depvar ev label
	val envc = checkRebindTopDec label defs bind
	val env5 = E.ENVCST (E.CSENV (ev, env3))
	val env6 = E.list2env [envc, env5, env4]
    in (env6, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_CLASS), label, value, regions, parents, children = [bind]}) =
    let val (env1, env2) = gen_bind true bind
	val ev   = E.nextEnvvar ()
	val enve = bindExplicitTyVars [bind] E.EXPLICIT
	val env3 = E.mk_env_loc (enve, E.ENVPOL (E.ENVAPP (env2, env1), E.TOP))
	val env4 = E.mk_env_depvar ev label
	val env5 = E.ENVCST (E.CSENV (ev, env3))
	val envc = checkRebindTopDec label defs bind
	val env6 = E.list2env [envc, env5, env4]
    in (env6, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_CLASSREC), label, value, regions, parents, children = [f, arg, X, Y]}) =
    let val (tv1, env1) = gen_id_bind f
	val (tv2, env2) = gen_atpat arg
	val (tv3, env3) = gen_exp X
	val (tv4, env4) = gen_exp Y
	val ev    = E.nextEnvvar ()
	val ev'   = E.nextEnvvar ()
	val tv    = E.nextItyvar ()
	val tv'   = E.nextItyvar ()
	val enve  = bindExplicitTyVars [arg, X, Y] E.EXPLICIT
	val eenv  = onlyVariables arg
	val envc  = checkRebindTopDec label defs f
	val clsX  = E.mk_type_class (E.mk_tyvar tv)  label
	val clsY  = E.mk_type_class (E.mk_tyvar tv') label
	val arr   = E.mk_type_arrow (E.mk_tyvar tv', clsX) label
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, clsX)) label
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv4, clsY)) label
	val env7  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr)) label
	val env8  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, E.mk_tyvar tv')) label
	val env9  = E.ENVCST (E.CSENV (ev, env2))
	val env10 = E.mk_env_depvar ev label
	val env11 = locCst (E.list2env [env9, env10, env3, env4, env5, env6, env7, env8, eenv])
	val env12 = E.mk_env_loc (enve, E.ENVPOL (E.ENVAPP (env1, env11), E.TOP))
	val env13 = E.mk_env_depvar ev' label
	val env14 = E.ENVCST (E.CSENV (ev', env12))
	val env15 = E.mk_env_deplab (tv1, E.OTH) label
	val env16 = E.list2env [envc, env14, env13, env15]
    in (env16, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_LETR), label, value, regions, parents, children = [bind]}) =
    let val (env1, env2) = gen_bind false bind
	val ev   = E.nextEnvvar ()
	val enve = bindExplicitTyVars [bind] E.EXPLICIT
	val env3 = E.mk_env_loc (enve, E.ENVPOL (E.ENVAPP (env1, env2), E.TOP))
	val env4 = E.mk_env_depvar ev label
	val eenv = isBindFunction bind label
	val envc = checkRebindTopDec label defs bind
	val env5 = E.ENVCST (E.CSENV (ev, env3))
	val env6 = E.list2env [envc, eenv, env5, env4]
    in (env6, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_CONS), label, value, regions, parents, children = [id, args, typ]}) =
    let val (tv1, env1) = gen_id_bind id
	val (tv2, tv2', env2) = gen_args args
	val (tv3, env3) = gen_type typ
	val enve  = bindExplicitTyVars [args, typ] E.EXPLICIT
	val env3' = E.mk_env_loc (enve, E.ENVAPP (env2, env3))
	val env4  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1,  E.mk_tyvar tv2)) label
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2', E.mk_tyvar tv3)) label
	val env6  = E.list2env [env3', env4, env5, E.ENVPOL (env1, E.CON)]
	val cons  = (A.getIdIdent id, D.addListLab D.empty [label, A.getLabel id])
	val env7  =
	    let val (tv1, env1) = gen_id_bind id
		val (tv2, env2) = gen_id_bound id
		val cst  = E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)
		val env3 = E.mk_env_depcst cst label
		val env4 = E.ENVPOL (env1, E.TOP)
	    in E.list2env [env3, env2, env4]
	    end
    (* NOTE: right now constants are globally exported *)
    in (env6, env7, newDefsCons [cons])
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_OCONS), label, value, regions, parents, children = [id, args, typ, tyvarseq, tyseqset]}) =
    let val (tv1, idor, env1) = gen_id_bind_o id
	val (tv2, tv2', env2) = gen_args args
	val (tv3, env3) = gen_type typ
	val (tvs, env4) = gen_typevarseq_ov tyvarseq
	val (svs, env5, names) = gen_typeseqset tyseqset
	(* NOTE: we add the typevars in typeseq and minus tyvar on typ: *)
	val enve  = bindExplicitTyVars' [args, typ] (tyvarseq, tyseqset) E.EXPLICIT
	val env3' = E.mk_env_loc (enve, E.ENVAPP (env5, E.mk_env_loc (env4, E.ENVAPP (env2, env3))))
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1,  E.mk_tyvar tv2)) label
	val env7  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2', E.mk_tyvar tv3)) label
	(* NOTE: we build the overloading structure *)
	val pairs = ListPair.zip (tvs, svs)
	val envs8 = map (fn (tv, sv) => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_type_or (E.ITYSEQVAR sv) idor label)) label) pairs
	val env8  = E.list2env envs8
	(*val env7  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_type_or (E.ITYSEQVAR sv) label)) label*)
	val env9  = E.list2env [env3', env6, env7, env8, E.ENVPOL (env1, E.CON)]
	val envo  = sanityCheckOverloading tyvarseq tyseqset label
	val envn  = E.mk_env_depove idor names label
	val env10 = E.list2env [envn, envo, env9]
	val cons  = (A.getIdIdent id, D.addListLab D.empty [label, A.getLabel id])
    in (env10, E.ENVNUL, newDefsCons [cons])
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_PSET), label, value, regions, parents, children = [id, typ]}) =
    let val (tv1, env1) = gen_id_set id label
	val (tv2, env2) = gen_type typ
	val enve  = bindExplicitTyVars [typ] E.EXPLICIT
	val env2' = E.mk_env_loc (enve, env2)
	val env3  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label
	val env4  = E.list2env [env2', env3, E.ENVPOL (env1, E.CON)]
	val deps  = D.addListLab D.empty [label, A.getLabel id]
	val ev    = E.nextEnvvar ()
	val env5  = E.ENVCST (E.CSENV (ev, env4))
	val env6  = E.mk_env_depvar ev label
	val ident = A.getIdIdent id
	val param = (ident, deps)
	val ptyp  = (ident, ev)
	val envf  = checkParameterNotRedefined defs [param]
	val defs' = updDefsPtyp (newDefsParams [param]) [ptyp]
	val tv    = E.nextItyvar ()
	val tset  = E.mk_type_set (E.mk_tyvar tv2) label
	val envc  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, tset)) label
	val envl  = E.mk_env_deplab (tv, E.OTH) label
    in (E.list2env [(*env4,*) envc, envl, envf, env5, env6], E.ENVNUL, defs')
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_PMAP), label, value, regions, parents, children = [id, typ1, typ2]}) =
    let val (tv1, tv1', env1) = gen_id_map id label
	val (tv2, env2) = gen_type typ1
	val (tv3, env3) = gen_type typ2
	val enve  = bindExplicitTyVars [typ1, typ2] E.EXPLICIT
	val env4  = E.mk_env_loc (enve, E.list2env [env2, env3])
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1', E.mk_tyvar tv3)) label
	val env7  = E.list2env [env4, env5, env6, E.ENVPOL (env1, E.CON)]
	val deps  = D.addListLab D.empty [label, A.getLabel id]
	val ev    = E.nextEnvvar ()
	val env8  = E.ENVCST (E.CSENV (ev, env7))
	val env9  = E.mk_env_depvar ev label
	val ident = A.getIdIdent id
	val param = (ident, deps)
	val ptyp  = (ident, ev)
	val envf  = checkParameterNotRedefined defs [param]
	val defs' = updDefsPtyp (newDefsParams [param]) [ptyp]
	val tv    = E.nextItyvar ()
	val tmap  = E.mk_type_map (E.mk_tyvar tv2, E.mk_tyvar tv3) label
	val envc  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, tmap)) label
	val envl  = E.mk_env_deplab (tv, E.OTH) label
    in (E.list2env [(*env4,*) envc, envl, envf, env8, env9], E.ENVNUL, defs')
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_PARAM), label, value, regions, parents, children = [id, typ]}) =
    let val (tv1, env1) = gen_id_bind id
	val (tv2, env2) = gen_type typ
	val enve  = bindExplicitTyVars [typ] E.EXPLICIT
	val env2' = E.mk_env_loc (enve, env2)
	val env3  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label
	val env4  = E.list2env [env2', env3, E.ENVPOL (env1, E.CON)]
	val deps  = D.addListLab D.empty [label, A.getLabel id]
	val ev    = E.nextEnvvar ()
	val env5  = E.ENVCST (E.CSENV (ev, env4))
	val env6  = E.mk_env_depvar ev label
	val ident = A.getIdIdent id
	val param = (ident, deps)
	val ptyp  = (ident, ev)
	val envf  = checkParameterNotRedefined defs [param]
	val defs' = updDefsPtyp (newDefsParams [param]) [ptyp]
    in (E.list2env [(*env4,*) envf, env5, env6], E.ENVNUL, defs')
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_PARAMP), label, value, regions, parents, children = [id, typ, prop]}) =
    let val (tv1, env1) = gen_id_bind id
	val (tv2, env2) = gen_type typ
	val (tv3, env3) = gen_atexp prop
	val enve  = bindExplicitTyVars [typ] E.EXPLICIT
	val env2' = E.mk_env_loc (enve, env2)
	val env4  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_type_prop label)) label
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label
	val env3' = locCst (E.list2env [env4, env3])
	val env6  = E.list2env [env2', env5, E.ENVPOL (env1, E.CON), env3']
	val deps  = D.addListLab D.empty [label, A.getLabel id]
	val ev    = E.nextEnvvar ()
	val env7  = E.ENVCST (E.CSENV (ev, env6))
	val env8  = E.mk_env_depvar ev label
	val ident = A.getIdIdent id
	val param = (ident, deps)
	val ptyp  = (ident, ev)
	val envf  = checkParameterNotRedefined defs [param]
	val defs' = updDefsPtyp (newDefsParams [param]) [ptyp]
    in (E.list2env [(*env6,*) envf, env7, env8], E.ENVNUL, defs')
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_TYPARAM), label, value, regions, parents, children = [tyvarseq, tycon, typ]}) =
    let val (sv1, f, env1) = gen_typevarseq tyvarseq
	val (_, _(*tv2*), sv2, env2) = gen_tycon_bind tycon
	val (tv3, env3) = gen_type typ
	(*val ity2  = E.mk_tyvar tv2*)
	val env4  = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv1, E.ITYSEQVAR sv2)) label
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_type_type label)) label
	val env6  = E.list2env [env3, env4, env5, E.mk_env_loc (env1, env2)]
	val env7  = getTwoBindTyVars tyvarseq label
	val deps  = D.addListLab D.empty [label, A.getLabel tycon]
	val tcons = (A.getIdIdent tycon, deps)
	val enve  = checkParameterNotRedefined defs [tcons]
	val env8  = E.list2env [env6, env7, enve]
    in (env8, E.ENVNUL, newDefsParams [tcons])
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_ETPARAM), label, value, regions, parents, children = [tyvarseq, tycon, id, typ]}) =
    let val (sv1, f, env1) = gen_typevarseq tyvarseq
	val (_(*tv2*), sv2, env2) = gen_eqtycon_bind tycon
	val (tv3, env3) = gen_id_bind id
	val (tv4, env4) = gen_type typ
	(*val env3  = gen_eqid_bind id tycon tyvarseq*)
	val tv    = E.nextItyvar ()
	val tnv   = E.nextTyconvar ()
	val tc    = A.getValue tycon
	val labC  = A.getLabel tycon
	val ptype = E.mk_type_type label
	val ity   = E.ITYCON (E.ITYSEQVAR sv2, E.ITYCONNAM (tc, true, labC))
	val pdeq  = E.mk_type_deq (E.mk_tyvar tv) label
	val (tyd, envd) = f pdeq (fn t => E.mk_type_deq t label)
	val pair  = E.mk_type_tuple [ptype, E.mk_tyvar tv3] label
	val env5  = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv1, E.ITYSEQVAR sv2)) label
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv4, pair)) label
	val env7  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv,  ity))  labC
	val env8  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, tyd))  label
	val env9  = E.list2env [env5, E.mk_env_loc (env1, E.ENVAPP (env2, env4))]
	val env10 = E.list2env [env6, env7, envd, env8, env9, E.ENVPOL (env3, E.CON)]
	val env11 = getTwoBindTyVars tyvarseq label
	val depst = D.addListLab D.empty [label, A.getLabel tycon]
	val depsc = D.addListLab D.empty [label, A.getLabel id]
	val ident = A.getIdIdent id
	val ev    = E.nextEnvvar ()
	val env12 = E.ENVCST (E.CSENV (ev, env10))
	val tcons = (A.getIdIdent tycon, depst)
	val cons  = (ident, depsc)
	val ptyp  = (ident, ev)
	val enve  = checkParameterNotRedefined defs [tcons, cons]
	val defs' = updDefsPtyp (newDefsParams [tcons, cons]) [ptyp]
    in (E.list2env [env10, env11, env12, enve], E.ENVNUL, defs')
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_TYCON), label, value, regions, parents, children = [tyvarseq, tycon, ty]}) =
    let val (sv1, f, env1) = gen_typevarseq tyvarseq
	val (_, _(*tv2*), sv2, env2) = gen_tycon_bind tycon
	val (tv3, env3) = gen_type ty
	(*val ity2 = E.mk_tyvar tv2*)
	val env4 = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv1, E.ITYSEQVAR sv2)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_type_type label)) label
	val env6 = E.list2env [env3, env4, env5, E.mk_env_loc (env1, env2)]
	val env7 = getTwoBindTyVars tyvarseq label
	val env8 = E.list2env [env6, env7]
	val env9  =
	    let val (v1, tv1, sv1, env1) = gen_tycon_bind tycon
		val (sv2, tv2, env2) = gen_tycon_bound tycon
		val cst1 = E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)
		val cst2 = E.CSSEQ (E.ITYSEQVAR sv1, E.ITYSEQVAR sv2)
		val env3 = E.mk_env_depcst cst1 label
		val env4 = E.mk_env_depcst cst2 label
		val ev   = E.nextEnvvar ()
		val envs = E.list2env [env3, env4, env2, env1]
		val env5 = E.ENVCST (E.CSENV (ev, envs))
		val env6 = E.mk_env_depvar ev label
	    in E.list2env [env5, env6]
	    end
    in (env8, env9 (*E.ENVNUL*), initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_EQTYCON), label, value, regions, parents, children = [tyvarseq, tycon, id, ty]}) =
    let val (sv1, f, env1) = gen_typevarseq tyvarseq
	val (tv2, sv2, env2) = gen_eqtycon_bind tycon
	val (tv3, env3) = gen_id_bind id
	val (tv4, env4) = gen_type ty
	val tv    = E.nextItyvar ()
	val tnv   = E.nextTyconvar ()
	val tc    = A.getValue tycon
	val labC  = A.getLabel tycon
	val ptype = E.mk_type_type label
	val ity   = E.ITYCON (E.ITYSEQVAR sv2, E.ITYCONNAM (tc, true, labC))
	val pdeq  = E.mk_type_deq (E.mk_tyvar tv) label
	val (tyd, envd) = f pdeq (fn t => E.mk_type_deq t label)
	val pair  = E.mk_type_tuple [ptype, E.mk_tyvar tv3] label
	val env5  = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv1, E.ITYSEQVAR sv2)) label
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv4, pair)) label
	val env7  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv,  ity))  labC
	val env8  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, tyd))  label
	val env9  = E.list2env [env5, E.mk_env_loc (env1, E.ENVAPP (env2, env4))]
	val env10 = E.list2env [env6, env7, envd, env8, env9, E.ENVPOL (env3, E.CON)]
	val env11 = getTwoBindTyVars tyvarseq label
	val env12 = E.list2env [env10, env11]
	val cons  = (A.getIdIdent id, D.addListLab D.empty [label, A.getLabel id])
    in (env12, E.ENVNUL, newDefsCons [cons])
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_TYFUN), label, value, regions, parents, children = [tyvarseq, tycon, typ]}) =
    let val (sv1, f, env1) = gen_typevarseq tyvarseq
	val (sv2, tv2, env2) = gen_tycon_fun tycon
	val (tv3, env3) = gen_type typ
	val env4 = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv1, E.ITYSEQVAR sv2)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, E.mk_tyvar tv3)) label
	val env6 = E.ENVAPP (env4, E.ENVAPP (env5, E.mk_env_loc (env1, E.ENVAPP (env3, env2))))
	val env7 = getTwoBindTyVars tyvarseq label
	val env8 = E.list2env [env6, env7]
    in (env8, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_QMSG), label, value, regions, parents, children = [id, hdr, typ]}) =
    let val (tv1,    env1) = gen_id_header id value
	val (tv2, v, env2) = gen_atoms_bind hdr
	val (tv3,    env3) = gen_type typ
	val ev   = E.nextEnvvar ()
	val enve = bindExplicitTyVars [typ] E.IMPLICIT
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv3)) label
	val env6 = E.mk_env_loc (enve, E.list2env [env2, env3, env4, env5, env1])
	val env7 = E.mk_env_depvar ev label
	val v'   = let val (hdr, deps) = v in (hdr, D.add (deps, D.mk_dep label)) end
	val envm = checkMsgsNotRedefined defs [v']
	val env8 = E.ENVCST (E.CSENV (ev, env6))
	val envl = E.mk_env_deplab (tv3, E.OTH) label
	val env9 = E.list2env [envm, envl, env8, env7]
    in (env9, E.ENVNUL, newDefsHeaders [v'])
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_EQMSG), label, value, regions, parents, children = [id, exp, typ]}) =
    let val (tv1, env1) = gen_id_header id value
	val (tv2, env2) = gen_atexp exp
	val (tv3, env3) = gen_type typ
	val ev   = E.nextEnvvar ()
	val aty  = E.mk_type_atom label
	val lty  = E.mk_type_list aty label
	val enve = bindExplicitTyVars [typ] E.IMPLICIT
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, lty)) label
	val env5 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv3)) label
	val env6 = E.mk_env_loc (enve, E.list2env [env2, env3, env4, env5, env1])
	val env7 = E.mk_env_depvar ev label
	val env8 = E.ENVCST (E.CSENV (ev, env6))
	val envl = E.mk_env_deplab (tv3, E.OTH) label
	val env9 = E.list2env [envl, env8, env7]
    in (env9, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_PARSE), label, value, regions, parents, children = []}) =
    let val env = E.ENVDEP (E.ENVERR (value, E.PARSE), D.mk_dep label)
    in (env, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_TYPEOF), label, value, regions, parents, children = [id]}) =
    let val env = E.ENVDEP (gen_id_typeof id, D.mk_dep label)
    in (env, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_INFIX), label, value, regions, parents, children = [id]}) =
    let val (tv1, env1) = gen_id_bind id
	val (tv2, env2) = gen_id_bound id
	val arr1 = E.mk_type_arrow (E.mk_new_tyvar (), E.mk_new_tyvar ()) label
	val arr2 = E.mk_type_arrow (E.mk_new_tyvar (), arr1) label
	val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label
	val env4 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, arr2)) label
	val env5 = E.ENVPOL (E.list2env [env2, env3, (*env4,*) env1], E.TOP)
    in (env5, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_MAIN), label, value, regions, parents, children = [exp]}) =
    let val (tv, env)   = gen_exp exp
	val cls         = E.mk_type_class (E.mk_type_instr label) label
	val env'        = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, cls)) label
	val id_main     = A.mk_term A.ID_VID label "main" [] [] []
	val (tv1, env1) = gen_id_bind id_main
	val env2        = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv)) label
	val env3        = E.list2env [env, env', env2, env1]
	val env4        = E.ENVPOL (env3, E.TOP)
	val ev          = E.nextEnvvar ()
	val env5        = E.mk_env_depvar ev label
	val env6        = E.ENVCST (E.CSENV (ev, env4))
	val (ev', env7) =
	    let val spec        = getDefsSpec defs
		val id_ex_main  = case spec of SOME x => x ^ "_main" | NONE => "main"
		val ex_main     = A.mk_term A.ID_VID label id_ex_main [] [] []
		val (tv1, env1) = gen_id_bind ex_main
		val (tv2, env2) = gen_id_bound id_main
		val cst         = E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)
		val env3        = E.mk_env_depcst cst label
		val env4        = E.ENVPOL (env1, E.TOP)
		val env5        = E.list2env [env3, env2, env4]
		val ev          = E.nextEnvvar ()
		val env6        = E.ENVCST (E.CSENV (ev, env5))
	    in (ev, env6)
	    end
	val env8        = E.list2env [env6, env5, env7]
	val defs'       =
	    case getDefsFile defs of
		SOME file =>
		let val {dir, file} = OS.Path.splitDirFile file
		    val {base, ext} = OS.Path.splitBaseExt file
		(*val _  = print ("[adding export env for: " ^ base ^ "]\n")*)
		(*getDefsParams defs*)
		in newDefsExport [(base, ev', computeParamsExport defs)]
		end
	      | NONE => initDefs
    in (env8, E.ENVNUL, defs')
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_SPEC), label, value, regions, parents, children = [id]}) =
    (E.ENVNUL, E.ENVNUL, newDefsSpec (A.getIdIdent id))
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_IMPORT), label, value, regions, parents, children}) =
    let val lst = map (fn x => (gen_id_bind x, gen_id_bound x)) children
	val (envscs, envsbound, envsbind) =
	    foldr (fn (((tv1, env1), (tv2, env2)),
		       (envscs, envsbound, envsbind)) =>
		      let val cst  = E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)
			  val env' = E.mk_env_depcst cst label
		      in (env' :: envscs,
			  env2 :: envsbound,
			  env1 :: envsbind)
		      end)
		  ([], [], [])
		  lst
	val envs = map (fn e => E.ENVPOL (e, E.TOP)) envsbind
	val env  = E.list2env (envscs @ envsbound @ envs)
	val cons = map (fn id => (A.getIdIdent id, D.addListLab D.empty [label, A.getLabel id])) children
    in (env, E.ENVNUL, newDefsCons cons)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_TIMPORT), label, value, regions, parents, children}) =
    let val lst = map (fn x => (gen_tycon_fun(*bind*) x, gen_tycon_bound x)) children
	val (envscs, envsbound, envsbind) =
	    foldr (fn (((*(v1, tv1, sv1, env1),*)
			(sv1, tv1, env1),
			(sv2, tv2, env2)),
		       (envscs, envsbound, envsbind)) =>
		      let val env3 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, E.mk_tyvar tv2)) label
			  val env4 = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv1, E.ITYSEQVAR sv2)) label
		      in (env3 :: env4 :: envscs,
			  env2 :: envsbound,
			  env1 :: envsbind)
		      end)
		  ([], [], [])
		  lst
	val env  = E.list2env (envscs @ envsbound @ envsbind)
	val cons = map (fn id => (A.getIdIdent id, D.addListLab D.empty [label, A.getLabel id])) children
    in (env, E.ENVNUL, newDefsCons cons)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_EXPORT), label, value, regions, parents, children}) =
    (E.list2env (#2 (ListPair.unzip (map gen_id_bound children))), E.ENVNUL, initDefs)
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_ASSUME), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_id_bound children)
	val ty    = E.mk_type_prop label
	val envs' = map (fn tv => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, ty)) label) tvs
    in (E.list2env (envs @ envs'), E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_GUARANT), label, value, regions, parents, children}) =
    let val (tvs, envs) = ListPair.unzip (map gen_id_bound children)
	val ty    = E.mk_type_prop label
	val envs' = map (fn tv => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, ty)) label) tvs
    in (E.list2env (envs @ envs'), E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_DOC), label, value, regions, parents, children}) =
    (E.ENVNUL, E.ENVNUL, initDefs)
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_INV), label, value, regions, parents, children = [id, cls, params, args, prop]}) =
    let val (tv1, env1) = gen_id_bound cls
	val (tvs, envs) = gen_param params
	val (tv2, env2) = gen_pat args
	val (tv3, env3) = gen_prop prop
	val tv    = E.nextItyvar ()
	val ev    = E.nextEnvvar ()
	val cls   = E.mk_type_class (E.mk_tyvar tv) label
	val ty    = foldr (fn (tv, ty) => E.mk_type_arrow (E.mk_tyvar tv, ty) label) cls tvs
	val env4  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, ty))  label
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, E.mk_tyvar tv)) label
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_type_prop label)) label
	val env7  = E.mk_env_deplab (tv1, E.OTH) label
	val env8  = E.ENVCST (E.CSENV (ev, E.list2env (envs @ [env2])))
	val env9  = E.mk_env_depvar ev label
	val env10 = locCst (E.list2env [env1, env4, env5, env6, env7, env8, env9, env3])
    in (env10, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_ORDER), label, value, regions, parents, children = [id, cls, params, args1, args2, prop]}) =
    let val (tv1, env1) = gen_id_bound cls
	val (tvs, envs) = gen_param params
	val (tv2, env2) = gen_pat args1
	val (tv3, env3) = gen_pat args2
	val (tv4, env4) = gen_prop prop
	val tv    = E.nextItyvar ()
	val ev    = E.nextEnvvar ()
	val cls   = E.mk_type_class (E.mk_tyvar tv) label
	val ty    = foldr (fn (tv, ty) => E.mk_type_arrow (E.mk_tyvar tv, ty) label) cls tvs
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, ty))  label
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, E.mk_tyvar tv)) label
	val env7  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_tyvar tv)) label
	val env8  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv4, E.mk_type_prop label)) label
	val env9  = E.mk_env_deplab (tv1, E.OTH) label
	val env10  = E.ENVCST (E.CSENV (ev, E.list2env (envs @ [env2, env3])))
	val env11  = E.mk_env_depvar ev label
	val env12 = locCst (E.list2env [env1, env5, env6, env7, env8, env9, env10, env11, env4])
    in (env12, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_PROGRESS), label, value, regions, parents, children = [id, cls, params, args1, args2, wcls, prop]}) =
    let val (tv1, env1) = gen_id_bound cls
	val (tvs, envs) = gen_param params
	val (tv2, env2) = gen_pat args1
	val (tv3, env3) = gen_pat args2
	val (tv4, env4) = gen_prop prop
	val (tvW, envW) = gen_id_bound wcls
	val tv    = E.nextItyvar ()
	val tv'   = E.nextItyvar ()
	val ev    = E.nextEnvvar ()
	val cls   = E.mk_type_class (E.mk_tyvar tv)  label
	val cls'  = E.mk_type_class (E.mk_tyvar tv') label
	val ty    = foldr (fn (tv, ty) => E.mk_type_arrow (E.mk_tyvar tv, ty) label) cls tvs
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, ty))  label
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, E.mk_tyvar tv)) label
	val env7  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_tyvar tv)) label
	val env8  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv4, E.mk_type_prop label)) label
	val env9  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tvW, cls')) label
	val env10 = E.mk_env_deplab (tv1, E.OTH) label
	val env11  = E.ENVCST (E.CSENV (ev, E.list2env (envs @ [envW, env2, env3])))
	val env12  = E.mk_env_depvar ev label
	val env13 = locCst (E.list2env [env1, env5, env6, env7, env8, env9, env10, env11, env12, env4])
    in (env13, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_STRICT), label, value, regions, parents, children = [id, cls, params, args1, args2, v, wcls, obs, prop, rel]}) =
    let val (tv1, env1) = gen_id_bound cls
	val (tvs, envs) = gen_param params
	val (tv2, env2) = gen_pat args1
	val (tv3, env3) = gen_pat args2
	val (tvV, envV) = gen_pat v
	val (tvW, envW) = gen_id_bound wcls
	val (tvO, envO) = gen_pat obs
	val (tvP, envP) = gen_prop prop
	val (tvR, envR) = gen_prop rel
	val tv    = E.nextItyvar ()
	val tv'   = E.nextItyvar ()
	val ev    = E.nextEnvvar ()
	val ev'   = E.nextEnvvar ()
	val cls   = E.mk_type_class (E.mk_tyvar tv)  label
	val cls'  = E.mk_type_class (E.mk_tyvar tv') label
	val ty    = foldr (fn (tv, ty) => E.mk_type_arrow (E.mk_tyvar tv, ty) label) cls tvs
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, ty))  label
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, E.mk_tyvar tv)) label
	val env7  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_tyvar tv)) label
	val env8  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tvR, E.mk_type_prop label)) label
	val env9  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tvW, cls')) label
	val env10 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tvV, E.mk_tyvar tv')) label
	val env11 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tvO, E.mk_tyvar tv))  label
	val env12 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tvP, E.mk_type_prop label)) label
	val envA  = E.ENVCST (E.CSENV (ev', E.list2env [envV, envO]))
	val envB  = E.mk_env_depvar ev' label
	val env13 = locCst (E.list2env [envA, envB, envP])
	val env14 = E.mk_env_deplab (tv1, E.OTH) label
	val env15 = E.ENVCST (E.CSENV (ev, E.list2env (envs @ [envW, env2, env3])))
	val env16 = E.mk_env_depvar ev label
	val lst   = [env1, env5, env6, env7, env8, env9, env10, env11, env12, env14, env15, env16, env13, envR]
	val env17 = locCst (E.list2env lst)
    in (env17, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_CONSIST), label, value, regions, parents, children = [id, cls, params, args1, args2, prop]}) =
    let val (tv1, env1) = gen_id_bound cls
	val (tvs, envs) = gen_param params
	val (tv2, env2) = gen_pat args1
	val (tv3, env3) = gen_pat args2
	val (tv4, env4) = gen_prop prop
	val tv    = E.nextItyvar ()
	val ev    = E.nextEnvvar ()
	val cls   = E.mk_type_class (E.mk_tyvar tv) label
	val ty    = foldr (fn (tv, ty) => E.mk_type_arrow (E.mk_tyvar tv, ty) label) cls tvs
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, ty))  label
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, E.mk_tyvar tv)) label
	val env7  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_tyvar tv)) label
	val env8  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv4, E.mk_type_prop label)) label
	val env9  = E.mk_env_deplab (tv1, E.OTH) label
	val env10  = E.ENVCST (E.CSENV (ev, E.list2env (envs @ [env2, env3])))
	val env11  = E.mk_env_depvar ev label
	val env12 = locCst (E.list2env [env1, env5, env6, env7, env8, env9, env10, env11, env4])
    in (env12, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_MEMORY), label, value, regions, parents, children = [id, cls, params, args1, args2, v, wcls, rel]}) =
    let val (tv1, env1) = gen_id_bound cls
	val (tvs, envs) = gen_param params
	val (tv2, env2) = gen_pat args1
	val (tv3, env3) = gen_pat args2
	val (tvV, envV) = gen_pat v
	val (tvW, envW) = gen_id_bound wcls
	val (tvR, envR) = gen_prop rel
	val tv    = E.nextItyvar ()
	val tv'   = E.nextItyvar ()
	val ev    = E.nextEnvvar ()
	val cls   = E.mk_type_class (E.mk_tyvar tv)  label
	val cls'  = E.mk_type_class (E.mk_tyvar tv') label
	val ty    = foldr (fn (tv, ty) => E.mk_type_arrow (E.mk_tyvar tv, ty) label) cls tvs
	val env5  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv1, ty))  label
	val env6  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv2, E.mk_tyvar tv)) label
	val env7  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, E.mk_tyvar tv)) label
	val env8  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tvR, E.mk_type_prop label)) label
	val env9  = E.mk_env_depcst (E.CSITY (E.mk_tyvar tvW, cls')) label
	val env10 = E.mk_env_depcst (E.CSITY (E.mk_tyvar tvV, E.mk_tyvar tv')) label
	val env11 = E.mk_env_deplab (tv1, E.OTH) label
	val env12 = E.ENVCST (E.CSENV (ev, E.list2env (envs @ [envW, env2, env3, envV])))
	val env13 = E.mk_env_depvar ev label
	val lst   = [env1, env5, env6, env7, env8, env9, env10, env11, env12, env13, envR]
	val env14 = locCst (E.list2env lst)
    in (env14, E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_DATA), label, value, regions, parents, children = (id :: cons)}) =
    let val (_, tv1, sv1, env1) = gen_tycon_bind id
	val (tvs, envs) = ListPair.unzip (map gen_data cons)
	val seq   = E.ITYSEQSEQ ([], label)
	val env2  = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv1, seq)) label
	val envs' = map (fn tv => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, E.mk_tyvar tv1)) label) tvs
    in (E.ENVPOL (E.list2env (env1 :: env2 :: envs' @ envs), E.TOP), E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_ABSTYPE), label, value, regions, parents, children = [tycon, typ, binds]}) =
    let val (v, tv1, sv1, env1) = gen_tycon_bind tycon
	val (tv2, env2) = gen_type typ
	val id_abs = A.mk_term A.ID_VID label ("abs_" ^ v) [] [] []
	val id_rep = A.mk_term A.ID_VID label ("rep_" ^ v) [] [] []
	val (tv3, env3) = gen_id_bind id_abs
	val (tv4, env4) = gen_id_bind id_rep
	val env5   = gen_binds defs binds
	val seq    = E.ITYSEQSEQ ([], label)
	val env6   = E.mk_env_depcst (E.CSSEQ (E.ITYSEQVAR sv1, seq)) label
	val arr3   = E.mk_type_arrow (E.mk_tyvar tv2, E.mk_tyvar tv1) label
	val env7   = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv3, arr3)) label
	val arr4   = E.mk_type_arrow (E.mk_tyvar tv1, E.mk_tyvar tv2) label
	val env8   = E.mk_env_depcst (E.CSITY (E.mk_tyvar tv4, arr4)) label
	val env9   = E.mk_env_loc (E.list2env [env7, env8, env3, env4], env5)
	val envl   = E.mk_env_deplab (tv2, E.OTH) label
    (**)
    in (E.list2env [env2, env6, env1, env9, envl], E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_VAR), label, value, regions, parents, children = [ids, typ]}) =
    let val (tvs, env1) = gen_ids_bind_v ids
	val (tv2, env2) = gen_type typ
	val ty2  = E.mk_tyvar tv2
	val envs = map (fn tv => E.mk_env_depcst (E.CSITY (E.mk_tyvar tv, ty2)) label) tvs
    in (E.list2env (envs @ [env2, env1]), E.ENVNUL, initDefs)
    end
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_OPTIONS), label, value, regions, parents, children}) =
    (E.ENVNUL, E.ENVNUL, initDefs)
  | gen_dec defs (A.N {kind = (A.DEC, A.DEC_INCLUDE), label, value, regions, parents, children = [id, parms]}) =
    let val file = A.getIdIdent id
	val lst  = gen_incparms parms
	val env  = E.list2env (map (fn (n, id, deps, env) => env) lst)
    in case findDefsExport file defs of
	   SOME (f, ev, params) =>
	   (E.mk_env_app (env, E.mk_env_depvar ev label) , E.ENVNUL, initDefs)
	 | NONE => (env, E.ENVNUL, initDefs)
    end
  | gen_dec _ _ = raise Fail "term is not a declaration"

and gen_file export (A.N {kind = (A.FILE, A.FILE_F), label, value, regions, parents, children}) =
    let val (envs, envs', defs') =
	    foldl (fn (env, (envs, envs', defs1)) =>
		      let val (env', env'', defs2) = gen_dec defs1 env
		      in (envs @ [env'], envs' @ [env''], mergeDefs defs1 defs2)
		      end)
		  ([], [], updDefsExport (newDefsFile value) export)
		  children
	(*val _ = print ("[file:" ^ value ^ "]\n")*)
	val env1 = E.list2env envs
	val env2 = E.list2env envs'
    (*val _   = print ("(0)--" ^ toStringEnv env ^ "\n")*)
    in (E.ENVFIL (value, env1, env2, label), getDefsExport defs')
    end
  | gen_file _ _ = raise Fail "term is not a declaration list"

and gen_prog (prog as A.N {kind = (A.PROG, A.PROG_P), label, value, regions, parents, children}) =
    let val (envs, _) =
	    foldl (fn (file, (envs, export)) =>
		      let val (env, export') = gen_file export file
		      in (envs @ [env], export')
		      end)
		  ([], [])
		  children
    in E.mk_env_app (PR.init_env, E.list2env envs)
    end
  | gen_prog _ = raise Fail "term is not a program"


end
