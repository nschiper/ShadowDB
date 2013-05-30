(* Copyright 2011 Cornell University
 * Copyright 2012 Cornell University
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
 *  o File name:   ParseNuprlAscii.sig
 *  o Description: Parser for Nuprl ascii terms.
 *)


signature PARSER_NUPRL_ASCII = sig

    val parse : bool           (* true to print debug info *)
		-> bool        (* true to get light terms *)
		-> string list (* theories to filter out *)
		-> string      (* file to parse *)
		-> bool        (* true if ones wants to split the file into smaller files *)
		-> NuprlTerms.nuprl_term list

    val parseString : bool           (* true to print debug info *)
		      -> bool        (* true to get light terms *)
		      -> string list (* theories to filter out *)
		      -> string      (* string to parse *)
		      -> NuprlTerms.nuprl_term list

end
