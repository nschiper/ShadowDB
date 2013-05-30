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
 *  o File name:   Region.sig
 *  o Description: Signature to handle regions in files.
 *)


signature REG = sig

    (* A position is a pair line number/column number *)
    type pos    = int * int
    (* A region is defined by a starting point (from) and an end point (to). *)
    type region = {from : pos, to : pos}

    (* ------ CONSTRUCTORS ------ *)

    (* Region constructor for a region on a same line. *)
    val consReg         : pos -> pos -> region
    (* Region constructor for a region spanning over multiple lines. *)
    val getRegionList   : pos -> pos -> region list

    (* ------ MODIFIERS ------ *)

    (* Next position. *)
    val upPos           : pos -> pos
    (* Previous position. *)
    val downPos         : pos -> pos

    (* Moves the position on line coordinate w.r.t. the length of the string. *)
    val addString       : pos -> string -> pos

    (* ------ ACCESSORS ------ *)

    val getFrom         : region -> pos
    val getTo           : region -> pos

    (* ------ TOSTRING ------  *)

    val toStringPos     : pos -> string
    val toStringReg     : region -> string
    val toStringLispReg : region -> string
    val toStringRegList : region list -> string

end
