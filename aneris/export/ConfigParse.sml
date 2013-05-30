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
 *  o Date:        30 Sep 2011
 *  o File name:   ConfigParse.sml
 *  o Description: Configuration files parser.
 *)


structure ConfigParse :> CONFIG_PARSE = struct

structure T = NuprlTerms

structure ConfigLrVals = ConfigLrValsFun(structure Token  = LrParser.Token)
structure ConfigLex    = ConfigLexFun   (structure Tokens = ConfigLrVals.Tokens)
structure ConfigParser = JoinWithArg(structure ParserData = ConfigLrVals.ParserData
                                     structure Lex        = ConfigLex
                                     structure LrParser   = LrParser)

type location    = string
type host        = string
type port        = int
type address     = location * host * port
type group_name  = string
type member      = bool
type group       = group_name * member * address list
type conn        = group_name * group_name
type parameter   = string * T.nuprl_term
type message     = T.nuprl_term * T.nuprl_term * T.nuprl_term
type loc_message = location * message

fun toStringPos (l, c) = Int.toString l ^ "." ^ Int.toString c
fun toStringReg (f, t) = toStringPos  f ^ "-" ^ toStringPos  t

fun parseStream file inputStream =
    let val error = ref false
	(* lexer argument: file name and start position *)
	val lexarg = (file, ref (1, 1))
	(* create a stream of lexical tokens *)
	val lexstream =
	    ConfigParser.makeLexer (fn n => TextIO.inputN (inputStream, n))
				   lexarg
	(* a function for reporting syntax errors *)
	fun syntaxError (msg, from, to) =
	    print (file ^ ":"  ^ toStringReg (from, to) ^ ": " ^  msg ^ "\n")
	(* build the AST, parameterized by its lowest node label *)
	val (astFunction, _) = ConfigParser.parse (15, lexstream, syntaxError, ())
    in astFunction ()
    end

fun parse input =
    let val instr = TextIO.openIn input
	val out   = parseStream input instr
	val _     = TextIO.closeIn instr
    in out
    end
    handle IO.Io {name, function, cause} =>
	   raise Fail ("cannot access file: " ^ input)

fun parseString str =
    let val input = "string"
    in let val instr = TextIO.openString str
	   val out   = parseStream input instr
	   val _     = TextIO.closeIn instr
       in out
       end
       handle IO.Io {name, function, cause} =>
	      raise Fail ("cannot access file: " ^ input)
    end

end
