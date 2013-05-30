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
 *  o File name:   Tools.sig
 *  o Description: Some useful functions.
 *)


signature TOOLS = sig

    type timer = {real : Timer.real_timer,
		  cpu  : Timer.cpu_timer}

    val can : ('a -> 'b) -> 'a -> bool

    val fst : 'a * 'b -> 'a
    val snd : 'a * 'b -> 'b

    val decr_steps : int -> int

    val split : int -> 'a list -> 'a list * 'a list

    val accumulate2 : ('a -> 'b -> 'c -> 'a)
		      -> 'a
		      -> 'b list
		      -> 'c list
		      -> 'a

    val all2 : ('a -> 'b -> bool)
	       -> 'a list
	       -> 'b list
	       -> bool

    val get1 : 'a list -> 'a
    val get2 : 'a list -> 'a * 'a
    val get3 : 'a list -> 'a * 'a * 'a
    val get4 : 'a list -> 'a * 'a  *'a * 'a

    val get1_0bound   : ('a list * 'b) list -> 'b
    val get1_1bound   : ('a list * 'b) list -> 'a * 'b
    val get1_2bound   : ('a list * 'b) list -> 'a * 'a * 'b
    val get2_0bound   : ('a list * 'b) list -> 'b * 'b
    val get2_1bound   : ('a list * 'b) list -> 'a * 'b * 'a * 'b
    val get2_02bound  : ('a list * 'b) list -> 'b * 'a * 'a * 'b
    val get2_03bound  : ('a list * 'b) list -> 'b * 'a * 'a * 'a * 'b
    val get3_202bound : ('a list * 'b) list -> 'a * 'a * 'b * 'b * 'a * 'a * 'b

    val startTimer   : unit -> timer
    val getMilliTime : timer -> IntInf.int

end
