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
 *  o File name:   ConfigParse.sig
 *  o Description: Signature of the configuration files parser.
 *)


signature CONFIG_PARSE = sig

    type location    = string (* location name *)
    type host        = string
    type port        = int
    type address     = location * host * port
    type group_name  = string (* name of a group *)
    type member      = bool   (* true if group is a member of the system *)
    type group       = group_name * member * address list
    type conn        = group_name * group_name
    type parameter   = string * NuprlTerms.nuprl_term
    type message     = NuprlTerms.nuprl_term * NuprlTerms.nuprl_term * NuprlTerms.nuprl_term
    type loc_message = location * message

    val parse : string ->
		group       list *
		conn        list *
		parameter   list *
		loc_message list

    val parseString : string ->
  		      group       list *
		      conn        list *
		      parameter   list *
		      loc_message list

end
