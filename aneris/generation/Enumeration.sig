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
 *  o File name:   Enumerations.sml
 *  o Description: Signature of the EventML constraint solver/minimizer/enumerator.
 *)


signature ENUM = sig

    (* Type of a constraint solving result *)
    type solved

    val initSolved  : unit -> solved

    val isSuccess   : solved -> bool

    (* The Boolean has to be true to get schemes as polymorphic as possible,
     * and false to try to restrict schemes to the identifiers' uses,
     * the last string is the file name (its base). *)
    val getScheme   : string -> Deps.label -> bool -> string -> solved -> Env.scheme option

    (* checks whether the accessor corresponding to the string is actually
     * accessed in the program (its constraint form, i.e., solved).  *)
    val isAccessed : Deps.label -> string -> solved -> bool

    (* Given a 'solved' and a label (location), tries (that's why we have option)
     * to return the type generated for the piece of code at the given location. *)
    val getItyAtLab : solved -> Deps.label -> Env.ity option

    (* Given a 'solved' and a label (location) corresponding to a bound id,
     * this function tries to return the id name that we should use, because
     * in some overloading cases, a bound id is in fact another id as specified
     * in the overloading dec. *)
    val getNameAtLab : solved -> Deps.label -> string option

    (* Type checks a file *)
    val typecheck   : string -> bool -> unit

    (* Solves the environment given as input *)
    val solver      : Env.env -> bool -> solved

    (* Slice a file *)
    val slice       : string list       (* input files  *)
		      -> string option  (* output file  *)
		      -> string option  (* library file *)
		      -> LargeInt.int   (* timelimit    *)
		      -> bool * bool    (* fst: true to display the errors on the fly
					 * snd: true to get more debug messages           *)
		      -> bool           (* true to use subtyping                          *)
		      -> bool           (* true to run the sanitizer after the inferencer *)
		      -> bool           (* true to run the type checker                   *)
		      -> Ast.term   (* The AST generated for the input file         *)
			 * Env.env  (* The constraint problem generated for the AST *)
			 * bool     (* True if the environment is solvable          *)


end
