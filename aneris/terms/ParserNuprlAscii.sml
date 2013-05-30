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
 *  o File name:   ParseNuprlAscii.sml
 *  o Description: Parser for Nuprl ascii terms.
 *)


structure ParserNuprlAscii :> PARSER_NUPRL_ASCII = struct

structure EH = LibBase

structure NuprlAsciiLrVals = NuprlAsciiLrValsFun(structure Token  = LrParser.Token)
structure NuprlAsciiLex    = NuprlAsciiLexFun   (structure Tokens = NuprlAsciiLrVals.Tokens)
structure NuprlAsciiParser = JoinWithArg(structure ParserData = NuprlAsciiLrVals.ParserData
                                         structure Lex        = NuprlAsciiLex
	       			         structure LrParser   = LrParser)

type timer = {real : Timer.real_timer,
	      cpu  : Timer.cpu_timer}

fun startTimer () = {real = Timer.startRealTimer (),
		     cpu  = Timer.startCPUTimer ()}

fun getTime (timer : timer) = Timer.checkRealTimer (#real timer)

fun getMilliTime timer = Time.toMilliseconds (getTime timer)
fun getSecTime   timer = Time.toSeconds      (getTime timer)

fun cond_print true str = print str
  | cond_print false _ = ()

fun parseStream prt light lst file inputStream =
    let val error     = ref false
	val lexarg    = (file, ref (1, 1))
	val lexstream = NuprlAsciiParser.makeLexer
			    (fn n => TextIO.inputN(inputStream, n)) lexarg
	fun syntaxError (msg, from, to) = (error := true; print (msg ^ "\n"))
	val _     = cond_print prt ("[start parsing " ^ file ^ "]\n")
	val timer = startTimer ()
	val (astFunction, _) = NuprlAsciiParser.parse (15, lexstream, syntaxError, ())
	val ast   = astFunction (light,lst)
	val time  = getSecTime timer
	val _     = cond_print prt ("[end parsing " ^ file ^ ": " ^ LargeInt.toString time ^ "s]\n")
    in if !error
       then raise Fail "unparsable"
       else ast
    end

fun getFiles prt file false = [file]
  | getFiles prt file true =
    let val _      = cond_print prt ("[start splitting file " ^ file ^ "]\n")
	val dir    = file ^ "-split"
	val status = OS.Process.system ("splitAscii " ^ file ^ " " ^ dir)
	val _      = cond_print prt ("[end splitting file " ^ file ^ "]\n")
	val strdir = OS.FileSys.openDir dir
	fun get NONE = []
	  | get (SOME file) = (dir ^ "/" ^ file) :: (get (OS.FileSys.readDir strdir))
	val files  = get (OS.FileSys.readDir strdir)
	val _      = OS.FileSys.closeDir strdir
    in files
    end

fun parseFiles _ _ _ [] = []
  | parseFiles prt light lst (file :: files) =
    let val instr = TextIO.openIn file
	val progs = parseStream prt light lst file instr
	val _     = TextIO.closeIn instr
    in progs @ (parseFiles prt light lst files)
    end

fun parse prt light lst file split =
    let val _      = cond_print prt ("[start parsing]\n")
	val timer  = startTimer ()
	val files  = getFiles prt file split
	val terms  = parseFiles prt light lst files
	val time   = getSecTime timer
	val _      = cond_print prt ("[end parsing: " ^ LargeInt.toString time ^ "s]\n")
    in terms
    end
    handle IO.Io {name, function, cause} =>
	   let val message = "cannot access file: " ^ file
	   in print (message ^ "\n");
	      raise EH.Impossible message
	   end
	 | OS.SysErr (st, syserrop) =>
	   let val message = "cannot run command: " ^ st
	   in print (message ^ "\n");
	      raise EH.Impossible message
	   end

fun parseString prt light lst string =
    let val file  = "string"
    in let val _     = cond_print prt ("[start parsing]\n")
	   val timer = startTimer ()
	   val instr = TextIO.openString string
	   val terms = parseStream prt light lst file instr
	   val _     = TextIO.closeIn instr
	   val time  = getSecTime timer
	   val _     = cond_print prt ("[end parsing: " ^ LargeInt.toString time ^ "s]\n")
       in terms
       end handle IO.Io {name, function, cause} =>
		  let val message = "cannot access file: " ^ file
		  in print (message ^ "\n");
		     raise EH.Impossible message
		  end
		| OS.SysErr (st, syserrop) =>
		  let val message = "cannot run command: " ^ st
		  in print (message ^ "\n");
		     raise EH.Impossible message
		  end
    end

end

