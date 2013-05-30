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
 *  o File name:   Interface.sig
 *  o Description: EventML interface.
 *)


signature INTERFACE = sig

    datatype arg = I       of string        (* input file                                    *)
		 | O       of string        (* output file                                   *)
		 | L       of string        (* EventML library file                          *)
		 | T       of LargeInt.int  (* timelimit                                     *)
		 | OBID    of string        (* Export EventML to NuPrl - obid                *)
		 | EVAL    of string        (* EventML expression to evaluate                *)
		 | DEF     of string        (* Alldef file - Nuprl library file              *)
		 | TEST    of int           (* To test stuff                                 *)
		 | STEP    of int           (* To run a step of the program stepper          *)
		 | HOST    of string        (* Host name                                     *)
		 | PORT    of int           (* Port number                                   *)
		 | CONF    of string        (* Runs an EML program in a distributed setting  *)
		 | EV      of string        (* One of the Nuprl evaluator                    *)
		 | ID      of string        (* Machine identifier                            *)
		 | EXTRA   of string        (* Extra arguments                               *)
		 | FILE1   of string        (* Extra file argument                           *)
		 | FILE2   of string        (* Extra file argument                           *)
		 | SIMUL                    (* Configuration file to run the prog of a spec  *)
		 | CLIENT                   (* Starts a dummy client                         *)
		 | SEND                     (* Sends initial messages in transit             *)
		 | OTHER                    (* Runs a forward machine                        *)
		 | ALL                      (* Simulates all the machines                    *)
		 | TONUPRL                  (* Export EventML to NuPrl                       *)
		 | SUBTYPING                (* Subtyping constraints                         *)
		 | SANITY                   (* Sanity checker of the generated environemnt   *)
		 | FROMASCII                (* Imports NuPrl terms                           *)
		 | TYPECHECK                (* Type checks an input EventML program          *)
		 | PARSE                    (* Parses an input EventML# program              *)
		 | SPLIT                    (* When importing NuPrl, one can split the input *)
		 | PRINT                    (* Prints the export                             *)
		 | SESSION                  (* To start an interactive session               *)
		 | MONO                     (* To generate types as monomorphic as we can    *)
		 | GC                       (* To turn on explicit garbage collection        *)
		 | MONITOR                  (* To monitor the other processes                *)
		 | TOLISP                   (* To generate lisp code from nuprl terms        *)

    val run : arg list -> unit

end
