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
 *  o File name:   Evaluators.sig
 *  o Description: Interface to our various SML evaluators.
 *)


signature EVALUATOR = sig

    type eval_sig = int
                    -> NuprlTerms.nuprl_term
		    -> NuprlTerms.nuprl_term * int

    (*val a_bunch_of_terms    : (string * NuprlTerms.nuprl_term) list*)

    val Evaluator1        : int
			    -> NuprlTerms.nuprl_term
			    -> NuprlTerms.nuprl_term * int

    val run_ev1           : bool
			    -> string option
			    -> eval_sig

    val run_ev2b           : bool
			    -> string option
			    -> eval_sig

    val run_ev1_map       : eval_sig
    val run_ev2_map       : eval_sig
    val run_ev2e_map      : eval_sig
    val run_ev2b_map      : eval_sig
    val run_ev2d_map      : eval_sig
    val run_ev3_map       : eval_sig
    val run_ev3b_map      : eval_sig
    val run_ev3c_map      : eval_sig
    val run_ev3d_map      : eval_sig
    val run_ev4_map       : eval_sig
    val run_ev4b_map      : eval_sig
    val run_ev5_map       : eval_sig
    val run_ev5b_map      : eval_sig

    val start_session     : bool (*true to get debug info*)
			    -> string option
			    -> unit

    val add_to_session    : NuprlTerms.nuprl_term list -> unit

    val start_session_lib : NuprlTerms.lib -> unit

    val end_session       : unit -> unit

    val get_lib           : unit -> NuprlTerms.lib
    val reset_lib         : unit -> unit

    val test              : int -> unit

end
