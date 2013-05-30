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
 *  o File name:   Dependencies.sml
 *  o Description: Program dependencies.
 *)


structure Deps :> DEPS = struct

type label = int

datatype dep = DEPL of label

val compareLabels = Int.compare

fun compareDeps (DEPL lab1, DEPL lab2) = compareLabels (lab1, lab2)

structure SET = BinarySetFn(type ord_key = dep val compare = compareDeps)

type deps = SET.set


(* ------ TO STRING ------ *)

fun toStringLabel label = Int.toString label

fun toStringDep (DEPL lab) = "DEPL(" ^ toStringLabel lab ^ ")"

fun toStringDepText (DEPL lab) = "l" ^ toStringLabel lab

fun toStringDeps deps =
    "[" ^ #1 (SET.foldr (fn (dep, (st, del)) => (toStringDep dep ^ del ^ st, ",")) ("]", "") deps)

fun toStringDepsText deps =
    #1 (SET.foldr (fn (dep, (st, del)) => (toStringDepText dep ^ del ^ st, ", ")) ("", "") deps)


(* ------ LABELS ------ *)

val dummy_label = 0

val label = ref 1

fun newLabel () = let val x = !label in (label := x + 1; x) end
fun resetLabel () = label := 1

fun reset () = (resetLabel (); ())


(* ------ DEPENDENCIES ------ *)

fun mk_dep lab = DEPL lab
fun mk_deps lab = SET.singleton (DEPL lab)

val dummy_dep = mk_dep dummy_label

fun isDummyLab lab = (lab = dummy_label)
fun isDummyDep (DEPL lab) = isDummyLab lab

fun getLabsDeps deps = deps

fun isLabelInDeps label deps = SET.exists (fn (DEPL lab) => lab = label) deps

val empty        = SET.empty
val filter       = SET.filter
val exists       = SET.exists
val equal        = SET.equal
val foldr        = SET.foldr
val foldl        = SET.foldl
val singleton    = SET.singleton
val add          = SET.add
val union        = SET.union
val intersection = SET.intersection
val listItems    = SET.listItems
val addList      = SET.addList
val member       = SET.member
val difference   = SET.difference
val isEmpty      = SET.isEmpty
val compare      = SET.compare

val isSubseteq = SET.isSubset

fun addListLab deps labs = SET.addList (deps, map (fn lab => mk_dep lab) labs)

fun isSubset (set1, set2) =
    SET.isSubset (set1, set2)
    andalso
    SET.numItems set1 < SET.numItems set2

fun exSubseteq deps depsset =
    List.exists (fn deps' => isSubseteq (deps', deps))
		depsset

end
