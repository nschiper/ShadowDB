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
 *  o File name:   ToNuprl.sig
 *  o Description: Interprets EventML to Nuprl.
 *)


signature TONUPRL = sig

    val toNuprl : NuprlTerms.lib option (* Nuprl context *)
		  -> string   (* Input file name                              *)
		  -> string   (* Object id of the esarph file within Nuprl    *)
		  -> Ast.term (* AST generated for the input file             *)
		  -> Env.env  (* Environment generated for the term           *)
		  -> bool     (* Subtyping                                    *)
		  -> bool     (* True to add a prefix to terms                *)
		  -> bool     (* True to get as much polymorphism as possible *)
		  -> bool     (* True to get stdma and stuff                  *)
		  -> bool     (* True to use type info                        *)
		  -> bool     (* True to generate callbyvalue's for let's     *)
		  -> bool     (* True to generate the new processes           *)
		  -> (NuprlTerms.nuprl_term list * (* list of declarations from the spec *)
		      (string list * NuprlTerms.nuprl_term) option) (* program along with params *)

end
