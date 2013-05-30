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
 *  o File name:   Infix.sig
 *  o Description: Handling of infix operators.
 *)


signature INFIX = sig

    type label
    type 'a pack = 'a * Reg.region
    type packstr = string pack
    datatype 'a tree = L of 'a pack
		     | O of packstr
		     | N of packstr * 'a tree * 'a tree

    val isInfix    : string -> bool

    val convert    : 'a tree list -> 'a tree

    val getLeft    : 'a tree -> Reg.region
    val getRight   : 'a tree -> Reg.region

    val newScope   : unit  -> label
    val rmScope    : label -> unit

    val addInfixL  : string -> int -> unit
    val addInfixR  : string -> int -> unit
    val rmInfix    : string -> unit

    val reset      : unit -> unit

end
