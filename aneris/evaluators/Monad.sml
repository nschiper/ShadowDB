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
 *  o File name:   Monad.sml
 *  o Description: State monad.
 *)


structure Monad :> MONAD = struct

structure B = Tools
structure T = NuprlTerms

type 's state = int * 's
type ('a, 's) M = 's state -> 'a * 's state

fun decr x (n,s) = (x, (B.decr_steps n,s))
fun unit x s = (x,s)
fun exec f (t,e) s = (f s) handle _ => (print ("++failed(" ^ T.opid_of_term t ^ ")\n"); ((t,e), s))
fun bind (m, k) =
 fn s =>
    let val (x,s') = m s
	(*val _ = (fn (steps,_) => print (">" ^ Int.toString steps ^ "\n")) s'*)
    in k x s'
    end

end
