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
 *  o File name:   Parser.sml
 *  o Description: EventML parser.
 *)


structure Parser :> PARSER = struct

structure A  = Ast
structure R  = Reg
structure F  = Infix
structure PD = ParseDefs
structure LD = LexDefs
structure EH = LibBase

(* currently used to hold information about syntax errors *)
type messages = (string * string * R.region list) list

(*exception ParsingError of int * messages*)

structure EMLLrVals = EMLLrValsFun(structure Token  = LrParser.Token)
structure EMLLex    = EMLLexFun   (structure Tokens = EMLLrVals.Tokens)
structure EMLParser = JoinWithArg(structure ParserData = EMLLrVals.ParserData
                                  structure Lex        = EMLLex
                                  structure LrParser   = LrParser)

(* sets a val for holding information about syntax errors *)
val messages : messages ref = ref []

(* add and print functions for the messages list *)
fun addmessage msg = messages := (!messages) @ [msg]
fun printmessages _ = app (fn (x, _, _) => print (x ^ "\n")) (!messages)

fun newLineCol (line, col) =
    case LD.getLength line of
	NONE =>
	(case LD.getLength (line - 1) of
	     NONE => raise Fail "The parser failed somewhere outside the text of the file(1)"
	   | SOME c => (line - 1, c))
      | SOME c =>
	if c = 0
	then (case LD.getLength (line - 1) of
		  NONE => raise Fail "The parser failed somewhere outside the text of the file(2)"
		| SOME c => (line - 1, c))
	else if col <= c
	then (line, col)
	else (line, c)

fun dummyparsing file messages =
    let val errs = map (fn (msg, _, regs) =>
			   let val regs' =
				   map (fn reg =>
					   let (*val _ = print (R.toStringReg reg ^ "\n")*)
					       val pos1 = newLineCol (R.getFrom reg)
					       val pos2 = newLineCol (R.getTo reg)
					   in R.consReg pos1 pos2
					   end)
				       regs
			   in A.mk_new_term A.DEC_PARSE msg regs' []
			   end)
		       messages
	val pfile = A.mk_new_term A.FILE_F file [] errs
    in A.mk_new_term A.PROG_P "" [] [pfile]
    end

(* returns a string reporting that there was an unexpected parsing error *)
fun failureParse file = "an unexpected parsing error occurred in " ^ file

val debug = true

fun print_debug str =
    if debug
    then print str
    else ()

fun parseStream file inputStream =
    let val error = ref false
	(* lexer argument: file name and start position *)
	val lexarg = (file, ref (1, 1))
	(* create a stream of lexical tokens *)
	val lexstream = EMLParser.makeLexer
			    (fn n => TextIO.inputN(inputStream, n)) lexarg
	(* initial parsing error messages *)
	val _ = messages := []
	(* a function for reporting syntax errors *)
	fun syntaxError (msg, from, to) =
	    (fn extmsg => (error := true; addmessage extmsg))
		(file ^ ":"  ^ R.toStringPos from ^ "-" ^ R.toStringPos to ^ ": " ^  msg,
		 msg,
		 [R.consReg from to])
	(* build the AST, parameterized by its lowest node label *)
	val (astFunction, _) =
	    LD.handleLex EMLParser.parse (15, lexstream, syntaxError, ())
	val ast = astFunction ()
    in if !error
       then ((*print_debug "syntax error(s)";*)
	     dummyparsing file (!messages))
       (*raise ParsingError (n, (!messages))*)
       else case PD.get_parsing_error () of
		SOME (msg, regs) => raise PD.ParseError (msg, regs)
	      | NONE => ast
    end
    handle LD.LexError x            => (print_debug "Lex errors\n";        dummyparsing file [x])
	 | EMLParser.ParseError     => (print_debug "ParseError(ML)\n";    dummyparsing file (!messages))
	 | PD.ParseError (msg, reg) => (print_debug "ParseError(PD)\n";    dummyparsing file [(msg, msg, reg)])
	 | EH.Impossible st         => (print_debug "DeadBranch error\n";  dummyparsing file [(st, st, [])])
	 | Fail st                  => (print_debug "Failure\n";           dummyparsing file [(st, st, [])])
	 | _                        => (print_debug "Unknown error\n";     dummyparsing file [(failureParse file, failureParse file, [])])

fun parseGen b input =
    let val instr = if b then TextIO.openIn input else TextIO.openString input
	val _     = PD.setFile input
	val _     = PD.reset_parsing_error ()
	val _     = A.reset ()
	val _     = LD.resetLengths ()
	val _     = LD.setFile input
	val prog  = parseStream input instr
	val _     = PD.setFile ""
	val _     = TextIO.closeIn instr
    in prog
    end
    handle IO.Io {name, function, cause} =>
	   let val message = "cannot access file: " ^ input
	   in print (message ^ "\n");
	      raise Fail message
	   end

fun merge [] = A.mk_new_term A.PROG_P "" [] []
  | merge [prog] = prog
  | merge (prog :: progs) =
    let val prog' = merge progs
    in if A.getKind prog = (A.PROG, A.PROG_P)
	  andalso
	  A.getKind prog' = (A.PROG, A.PROG_P)
       then let val lst1 = A.getChildren prog
		val lst2 = A.getChildren prog'
	    in A.mk_new_term A.PROG_P "" [] (lst1 @ lst2)
	    end
       else raise Fail "merge programs"
    end

fun close_inputs b [] = []
  | close_inputs b (input :: inputs) =
    let val prog  = parseGen b input
	val {dir,file} = OS.Path.splitDirFile input
	val files = A.get_includes prog
	val progs =
	    map (fn file =>
		    let val file' = OS.Path.joinDirFile {dir = dir, file = file}
		    in parseGen b (file' ^ ".esh")
		    end)
		files
    in progs @ [prog] @ (close_inputs b inputs)
    end

fun parseGens b inputs = merge (close_inputs b inputs)
(*fun parseGens b inputs = merge (map (parseGen b) inputs)*)

val parse       = parseGens true
val parseString = parseGens false

end
