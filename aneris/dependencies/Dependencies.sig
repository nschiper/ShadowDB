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
 *  o File name:   Dependencies.sig
 *  o Description: Program dependencies.
 *)


signature DEPS = sig

    type label = int

    datatype dep = DEPL of label

    type deps

    val reset : unit -> unit

    val dummy_label    : label
    val compareLabels  : label * label -> order
    val newLabel       : unit -> label

    val dummy_dep    : dep
    val mk_dep       : label -> dep
    val mk_deps      : label -> deps

    val isDummyLab   : label -> bool
    val isDummyDep   : dep   -> bool

    val empty        : deps
    val filter       : (dep -> bool) -> deps -> deps
    val exists       : (dep -> bool) -> deps -> bool
    val equal        : deps * deps -> bool
    val foldr        : (dep * 'a -> 'a) -> 'a -> deps -> 'a
    val foldl        : (dep * 'a -> 'a) -> 'a -> deps -> 'a
    val singleton    : dep -> deps
    val add          : deps * dep -> deps
    val union        : deps * deps -> deps
    val intersection : deps * deps -> deps
    val listItems    : deps -> dep list
    val addList      : deps * dep list -> deps
    val member       : deps * dep -> bool
    val difference   : deps * deps -> deps
    val isEmpty      : deps -> bool
    val compare      : deps * deps -> order
    val isSubset     : deps * deps -> bool
    val isSubseteq   : deps * deps -> bool
    val exSubseteq   : deps -> deps list -> bool
    val addListLab   : deps -> label list -> deps

    val getLabsDeps      : deps -> deps
    val isLabelInDeps    : label -> deps -> bool

    val toStringLabel    : label -> string
    val toStringDep      : dep  -> string
    val toStringDeps     : deps -> string
    val toStringDepsText : deps -> string

end
