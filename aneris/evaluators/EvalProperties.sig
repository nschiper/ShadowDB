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
 *  o Date:        29 July 2011
 *  o File name:   EvalProperties.sig
 *  o Description: Some properties of terms.
 *)


signature EVAL_PROPERTIES = sig

    val is_eval_all : string -> bool

    val num_principals    : string -> int
    val num_principal_all : string -> int

    val is_termof_term : NuprlTerms.lib
			 -> NuprlTerms.nuprl_term
			 -> bool

    val is_abstraction_term : NuprlTerms.lib
			      -> NuprlTerms.nuprl_term
			      -> bool

    val unfold_tof : NuprlTerms.nuprl_term
		     -> NuprlTerms.nuprl_term

    val unfold_abs : NuprlTerms.nuprl_term
		     -> NuprlTerms.nuprl_term

    val ct_unfold_abs : NuprlTerms.env option
			-> NuprlTerms.nuprl_term
			-> NuprlTerms.nuprl_term

end
