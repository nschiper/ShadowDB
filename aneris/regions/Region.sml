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
 *  o File name:   Region.sml
 *  o Description: Structure to handle regions in files.
 *)


structure Reg :> REG = struct

structure EH = LibBase
structure T  = ListFormat

type pos    = int * int
type region = {from : pos, to : pos}

fun addString (line, column) s = (line, column + String.size s)

fun consReg from to = {from = from, to = to}

fun toStringPos (line, column) = Int.toString line ^ "." ^ Int.toString column

fun toStringReg {from, to} = "(" ^ toStringPos from ^ "," ^ toStringPos to ^ ")"

fun toStringLispPos (line, column) = Int.toString line ^ " " ^ Int.toString column

fun toStringLispReg {from, to} = "(" ^ toStringLispPos from ^ " " ^ toStringLispPos to ^ ")"

fun toStringRegList xs = T.listToString toStringReg xs

fun upPos   (line, column) = (line, column + 1)
fun downPos (line, column) =
    if column = 0
    then raise EH.Impossible "downPos: the current column coordinate is already 0"
    else (line, column - 1)

fun infPos (line1, column1) (line2, column2) =
    line1 < line2
    orelse
    (line1 = line2 andalso column1 <= column2)

fun getRegionList (left as (l1, c1)) (right as (l2, c2)) =
    let fun consListRegs left right =
	    if infPos left right
	    then [consReg left right]
	    else []
	fun endlines n c =
	    if l2 < n
	    then raise EH.Impossible ""
	    else if n = l2
	    then consListRegs (l2, 1) (downPos right)
	    else let val newpos = upPos (n, c)
		 in (consReg newpos newpos) :: (endlines (n + 1) 0)
		 end
    in if l1 < l2
       then endlines l1 c1
       else consListRegs (upPos left) (downPos right)
    end

fun getFrom {from, to} = from
fun getTo   {from, to} = to

end
