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
 *  o File name:   Interface.sml
 *  o Description: EventML interface.
 *)


(* --------- IN TRANSIT MESSAGES --------- *)

signature INTRANSIT_MESSAGE = sig
    type message
    val apply2msg : (NuprlTerms.nuprl_term -> NuprlTerms.nuprl_term)
		    -> message
		    -> message
    val mk_message   : int -> string -> NuprlTerms.nuprl_term -> message
    val dest_message : message -> (int * string * NuprlTerms.nuprl_term)
    val message2term : message -> NuprlTerms.nuprl_term
    val term2message : NuprlTerms.nuprl_term -> message
end

structure Intransit_Message :> INTRANSIT_MESSAGE = struct

structure NT = NuprlTerms

type message =
     {delay : int,
      id    : string,
      msg   : NT.nuprl_term}

fun mk_message delay id msg = {delay = delay, id = id, msg = msg}

fun dest_message {delay,id,msg} = (delay,id,msg)

fun get_delay (msg : message) = #delay msg
fun get_id    (msg : message) = #id    msg
fun get_msg   (msg : message) = #msg   msg

fun apply2msg f {delay,id,msg} = mk_message delay id (f msg)

fun message2term {delay,id,msg} =
    let val delay_term = NT.mk_nuprl_small_natural_number_term delay
	val loc_term   = NT.mk_mkid_term id
    in NT.mk_pair_term delay_term (NT.mk_pair_term loc_term msg)
    end

fun term2message msg =
    let val (delay,rest) = NT.dest_pair 5 msg
	val (id,m) = NT.dest_pair 5 rest
    in mk_message (NT.dest_small_integer delay) (NT.dest_id 1 id) m
    end

end


(* --------- INTERFACE --------- *)

structure Interface :> INTERFACE = struct

structure P  = Parser
structure C  = ConfigParse
structure A  = Ast
structure E  = Enum
structure N  = ToNuprl
structure D  = Deps
structure G  = Gen
structure EN = Env
structure GL = GenLib
structure NT = NuprlTerms
structure EH = LibBase
structure EV = Evaluators
structure PN = ParserNuprlAscii
structure IM = Intransit_Message


(* ------ ARGUMENTS ------ *)

val default_el_output = "/tmp/eventml-output.el"
val default_output    = NONE
val default_lib       = NONE
val default_input     = "test.esh"
val default_time      = LargeInt.fromInt 1000 (* 1sec by default *)
val default_sub       = false
val default_sanity    = false
val default_nuprl     = false
val default_obid      = ""
val default_ascii     = false
val default_tcheck    = false
val default_parse     = false
val default_split     = false
val default_prt       = false
val default_eval      = NONE
val default_alldef    = NONE
val default_test      = NONE
val default_session   = false
val default_simul     = false
val default_step      = NONE
val default_mono      = false
val default_host      = "127.0.0.0"
val default_port      = 14567
val default_conf      = NONE
val default_client    = false
val default_send      = false
val default_other     = false
val default_all       = false
val default_ev        = "ev2b" (* ev1 *)
val default_id        = ""
val default_extra     = ""
val default_gc        = false
val default_file1     = NONE
val default_file2     = NONE
val default_monitor   = false
val default_tolisp    = false

val initArgs =
    {input   = default_input,
     output  = default_output,
     lib     = default_lib,
     time    = default_time,
     sub     = default_sub,
     sanity  = default_sanity,
     nuprl   = default_nuprl,
     obid    = default_obid,
     ascii   = default_ascii,
     tcheck  = default_tcheck,
     parse   = default_parse,
     split   = default_split,
     prt     = default_prt,
     eval    = default_eval,
     alldef  = default_alldef,
     test    = default_test,
     session = default_session,
     simul   = default_simul,
     step    = default_step,
     mono    = default_mono,
     host    = default_host,
     port    = default_port,
     conf    = default_conf,
     client  = default_client,
     send    = default_send,
     other   = default_other,
     all     = default_all,
     ev      = default_ev,
     id      = default_id,
     extra   = default_extra,
     gc      = default_gc,
     file1   = default_file1,
     file2   = default_file2,
     monitor = default_monitor,
     tolisp  = default_tolisp}

fun getElOutput (SOME output) = output
  | getElOutput NONE = default_el_output

fun updInput   {input = _, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} input   = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updOutput  {input, output = _, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} output  = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updLib     {input, output, lib = _, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} lib     = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updTime    {input, output, lib, time = _, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} time    = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updSub     {input, output, lib, time, sub = _, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} sub     = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updSanity  {input, output, lib, time, sub, sanity = _, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} sanity  = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updNuprl   {input, output, lib, time, sub, sanity, nuprl = _, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} nuprl   = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updObid    {input, output, lib, time, sub, sanity, nuprl, obid = _, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} obid    = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updAscii   {input, output, lib, time, sub, sanity, nuprl, obid, ascii = _, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} ascii   = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updTcheck  {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck = _, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} tcheck  = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updParse   {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse = _, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} parse   = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updSplit   {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split = _, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} split   = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updPrint   {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt = _, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} prt     = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updEval    {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval = _, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} eval    = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updAlldef  {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef = _, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} alldef  = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updTest    {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test = _, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} test    = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updSession {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session = _, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} session = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updSimul   {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul = _, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} simul   = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updStep    {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step = _, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} step    = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updMono    {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono = _, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} mono    = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updHost    {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host = _, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} host    = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updPort    {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port = _, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} port    = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updConf    {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf = _, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} conf    = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updClient  {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client = _, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} client  = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updSend    {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send = _, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp} send    = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updOther   {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other = _, all, ev, id, extra, gc, file1, file2, monitor, tolisp} other   = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updAll     {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all = _, ev, id, extra, gc, file1, file2, monitor, tolisp} all     = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updEv      {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev = _, id, extra, gc, file1, file2, monitor, tolisp} ev      = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updId      {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id = _, extra, gc, file1, file2, monitor, tolisp} id      = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updExtra   {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra = _, gc, file1, file2, monitor, tolisp} extra   = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updGc      {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc = _, file1, file2, monitor, tolisp} gc      = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updFile1   {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1 = _, file2, monitor, tolisp} file1   = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updFile2   {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2 = _, monitor, tolisp} file2   = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updMonitor {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor = _, tolisp} monitor = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}
fun updTolisp  {input, output, lib, time, sub, sanity, nuprl, obid, ascii, tcheck, parse, split, prt, eval, alldef, test, session, simul, step, mono, host, port, conf, client, send, other, all, ev, id, extra, gc, file1, file2, monitor, tolisp = _} tolisp  = {input = input, output = output, lib = lib, time = time, sub = sub, sanity = sanity, nuprl = nuprl, obid = obid, ascii = ascii, tcheck = tcheck, parse = parse, split = split, prt = prt, eval = eval, alldef = alldef, test = test, session = session, simul = simul, step = step, mono = mono, host = host, port = port, conf = conf, client = client, send = send, other = other, all = all, ev = ev, id = id, extra = extra, gc = gc, file1 = file1, file2 = file2, monitor = monitor, tolisp = tolisp}

datatype arg = I       of string        (* input     *)
	     | O       of string        (* ouput     *)
	     | L       of string        (* library   *)
	     | T       of LargeInt.int  (* timelimit *)
	     | OBID    of string        (* object_id to use when generating the nuprl code *)
	     | EVAL    of string        (* term to evaluate *)
	     | DEF     of string        (* the alldef file  *)
	     | TEST    of int
	     | STEP    of int           (* to step through a protocl, by specifying (int) the message to send next *)
	     | HOST    of string        (* host name *)
	     | PORT    of int           (* port number *)
	     | CONF    of string        (* run an EML program in a distributed environment using a conf file *)
	     | EV      of string        (* picks one of our Nuprl evaluator *)
	     | ID      of string        (* machine identifier   *)
	     | EXTRA   of string        (* extra arguments      *)
	     | FILE1   of string        (* Extra file argument  *)
	     | FILE2   of string        (* Extra file argument  *)
	     | SIMUL                    (* simulate an EML program with a configuration file *)
	     | CLIENT                   (* start a dummy client *)
	     | SEND                     (* send the initial messages in transit *)
	     | OTHER                    (* run a `forward` machine *)
	     | ALL                      (* simulates all the machines *)
	     | SESSION
	     | TONUPRL
	     | SUBTYPING
	     | SANITY
	     | FROMASCII
	     | TYPECHECK
	     | PARSE
	     | SPLIT
	     | PRINT
	     | MONO
	     | GC
	     | MONITOR
	     | TOLISP

fun format args =
    let fun gen [] r = r
	  | gen (SUBTYPING      :: list) r = gen list (updSub     r true)
	  | gen (SANITY         :: list) r = gen list (updSanity  r true)
	  | gen (FROMASCII      :: list) r = gen list (updAscii   r true)
	  | gen (TYPECHECK      :: list) r = gen list (updTcheck  r true)
	  | gen (PARSE          :: list) r = gen list (updParse   r true)
	  | gen (SPLIT          :: list) r = gen list (updSplit   r true)
	  | gen (PRINT          :: list) r = gen list (updPrint   r true)
	  | gen (TONUPRL        :: list) r = gen list (updNuprl   r true)
	  | gen (SESSION        :: list) r = gen list (updSession r true)
	  | gen (MONO           :: list) r = gen list (updMono    r true)
	  | gen (SEND           :: list) r = gen list (updSend    r true)
	  | gen (OTHER          :: list) r = gen list (updOther   r true)
	  | gen (CLIENT         :: list) r = gen list (updClient  r true)
	  | gen (ALL            :: list) r = gen list (updAll     r true)
	  | gen (GC             :: list) r = gen list (updGc      r true)
	  | gen (MONITOR        :: list) r = gen list (updMonitor r true)
	  | gen (TOLISP         :: list) r = gen list (updTolisp  r true)
	  | gen (SIMUL          :: list) r = gen list (updSimul   r true)
	  | gen ((EXTRA   str)  :: list) r = gen list (updExtra   r str)
	  | gen ((HOST    host) :: list) r = gen list (updHost    r host)
	  | gen ((PORT    port) :: list) r = gen list (updPort    r port)
	  | gen ((OBID    obid) :: list) r = gen list (updObid    r obid)
	  | gen ((T       time) :: list) r = gen list (updTime    r time)
	  | gen ((I       file) :: list) r = gen list (updInput   r file)
	  | gen ((EV      ev)   :: list) r = gen list (updEv      r ev)
	  | gen ((ID      id)   :: list) r = gen list (updId      r id)
	  | gen ((STEP    n)    :: list) r = gen list (updStep    r (SOME n))
	  | gen ((TEST    n)    :: list) r = gen list (updTest    r (SOME n))
	  | gen ((EVAL    str)  :: list) r = gen list (updEval    r (SOME str))
	  | gen ((FILE1   file) :: list) r = gen list (updFile1   r (SOME file))
	  | gen ((FILE2   file) :: list) r = gen list (updFile2   r (SOME file))
	  | gen ((CONF    file) :: list) r = gen list (updConf    r (SOME file))
	  | gen ((O       file) :: list) r = gen list (updOutput  r (SOME file))
	  | gen ((L       file) :: list) r = gen list (updLib     r (SOME file))
	  | gen ((DEF     file) :: list) r = gen list (updAlldef  r (SOME file))
    in gen args initArgs
    end

datatype extra = EX_INT  of int
	       | EX_BOOL of bool
	       | EX_DUMP
	       | EX_2HASKELL
	       | EX_2SML
	       | EX_TAGLESS
	       | EX_2LISP
	       | EX_NSOCK
	       | EX_NONE
	       | EX_CBVA
	       | EX_FOLD
	       | EX_UNFOLD
	       | EX_SILENT

fun getExtra str =
    let val lst = String.tokens (fn #" " => true
				  | #"," => true
				  | #":" => true
				  | #";" => true
				  | #"-" => true
				  | #"+" => true
				  | _    => false)
				str
	(*val str = ListFormat.fmt {init  = "",
				  final = "",
				  sep   = ":",
				  fmt   = fn x => x}
				 lst
	val _ = print ("[extras:" ^ str ^ "]\n")*)
    in map (fn elt =>
	       case Int.fromString elt of
		   SOME n => EX_INT n
		 | NONE =>
		   case elt of
		       "true"     => EX_BOOL true
		     | "false"    => EX_BOOL false
		     | "True"     => EX_BOOL true
		     | "False"    => EX_BOOL false
		     | "T"        => EX_BOOL true
		     | "F"        => EX_BOOL false
		     | "newsock"  => EX_NSOCK
		     | "dump"     => EX_DUMP
		     | "haskell"  => EX_2HASKELL
		     | "sml"      => EX_2SML
		     | "tagless"  => EX_TAGLESS
		     | "lisp"     => EX_2LISP
		     | "cbva"     => EX_CBVA
		     | "fold"     => EX_FOLD
		     | "unfold"   => EX_UNFOLD
		     | "silent"   => EX_SILENT
		     | _          => EX_NONE)
	   lst
    end

fun is_true_extra    str = List.exists (fn (EX_BOOL b) => b     | _ => false) (getExtra str)
fun is_false_extra   str = List.exists (fn (EX_BOOL b) => not b | _ => false) (getExtra str)
fun get_int_extra    str = List.find   (fn (EX_INT n)  => true  | _ => false) (getExtra str)
fun is_nsock_extra   str = List.exists (fn EX_NSOCK    => true  | _ => false) (getExtra str)
fun is_dump_extra    str = List.exists (fn EX_DUMP     => true  | _ => false) (getExtra str)
fun is_haskell_extra str = List.exists (fn EX_2HASKELL => true  | _ => false) (getExtra str)
fun is_sml_extra     str = List.exists (fn EX_2SML     => true  | _ => false) (getExtra str)
fun is_tagless_extra str = List.exists (fn EX_TAGLESS  => true  | _ => false) (getExtra str)
fun is_lisp_extra    str = List.exists (fn EX_2LISP    => true  | _ => false) (getExtra str)
fun is_cbva_extra    str = List.exists (fn EX_CBVA     => true  | _ => false) (getExtra str)
fun is_fold_extra    str = List.exists (fn EX_FOLD     => true  | _ => false) (getExtra str)
fun is_unfold_extra  str = List.exists (fn EX_UNFOLD   => true  | _ => false) (getExtra str)
fun is_silent_extra  str = List.exists (fn EX_SILENT   => true  | _ => false) (getExtra str)

fun spget_int_extra extra =
    case get_int_extra extra of
	SOME (EX_INT n) => n
      | _ => 0

datatype ident = Loc of string
	       | Mac of string * int

fun ident_to_string (Mac (host, port)) = "(" ^ host ^ "," ^ Int.toString port ^ ")"
  | ident_to_string (Loc id) = id


(* ------ A FEW GLOBAL VARIABLES ------ *)

val program = ref NONE : (NT.variable list * NT.nuprl_term) option ref

fun set_program_op pop = program := pop

val default_alldefs = "/usr/fdl/lib/alldefs"

val loaded     = ref false
val configured = ref false

val components = ref [] : (string * NT.nuprl_term) list ref
val intransit  = ref [] : IM.message list ref

val process : NT.nuprl_term option ref = ref NONE

fun reset_all () =
    (program    := NONE;
     components := [];
     intransit  := [];
     process    := NONE;
    ())

(* ------ TIMER ------ *)

type timer = {real : Timer.real_timer,
	      cpu  : Timer.cpu_timer}

fun startTimer () = {real = Timer.startRealTimer (),
		     cpu  = Timer.startCPUTimer  ()}
fun getTime (timer : timer) = Timer.checkRealTimer (#real timer)
fun getMilliTime   timer = Time.toMilliseconds (getTime timer)
fun getMicroTime   timer = Time.toMicroseconds (getTime timer)
fun getSecondsTime timer = Time.toSeconds      (getTime timer)


(* ------ EVALUATORS ------ *)

exception EVAL_ERROR of string

val print_ref = ref true
fun no_printing () = print_ref := false
fun if_print x = if !print_ref then x else ()

fun print_eml str = if_print (print ("[" ^ str ^ "]\n"))

fun print_eml_id id str = if_print (print_eml (id ^ ":" ^ str))

fun eval_wrap ev n t =
    let val timer  = startTimer ()
	val (r,s)  = ev n t
	    handle Fail str => raise EVAL_ERROR str
		 | _ => raise EVAL_ERROR ""
	val time   = getTime timer
	val mstime = Time.toMilliseconds time
	val nstime = Time.toMicroseconds time
	val _      = print_eml (LargeInt.toString mstime ^ "ms"
				^ "-"
				^ LargeInt.toString nstime ^ "ns")
    in (r,s)
    end

val ev1  = EV.run_ev1_map
val ev2  = EV.run_ev2_map
val ev2e = EV.run_ev2e_map
val ev2b = EV.run_ev2b_map
val ev2d = EV.run_ev2d_map
val ev3  = EV.run_ev3_map
val ev3b = EV.run_ev3b_map
val ev3c = EV.run_ev3c_map
val ev3d = EV.run_ev3d_map
val ev4  = EV.run_ev4_map
val ev4b = EV.run_ev4b_map
val ev5  = EV.run_ev5_map
val ev5b = EV.run_ev5b_map

val ev1  = eval_wrap ev1
val ev2  = eval_wrap ev2
val ev2e = eval_wrap ev2e
val ev2b = eval_wrap ev2b
val ev2d = eval_wrap ev2d
val ev3  = eval_wrap ev3
val ev3b = eval_wrap ev3b
val ev3c = eval_wrap ev3c
val ev3d = eval_wrap ev3d
val ev4  = eval_wrap ev4
val ev4b = eval_wrap ev4b
val ev5  = eval_wrap ev5
val ev5b = eval_wrap ev5b

fun get_ev "ev1"  = ("1",  ev1)
  (* 2nd evalutator *)
  | get_ev "ev2"  = ("2",  ev2)
  | get_ev "ev2b" = ("2b", ev2b)
  | get_ev "2b"   = ("2b", ev2b)
  | get_ev "ev2e" = ("2e", ev2e)
  | get_ev "2e"   = ("2e", ev2e)
  | get_ev "ev2d" = ("2d", ev2d)
  | get_ev "2d"   = ("2d", ev2d)
  (* 3rd evalutator *)
  | get_ev "ev3"  = ("3",  ev3)
  | get_ev "ev3b" = ("3b", ev3b)
  | get_ev "3b"   = ("3b", ev3b)
  | get_ev "ev3c" = ("3c", ev3c)
  | get_ev "3c"   = ("3c", ev3c)
  | get_ev "ev3d" = ("3d", ev3d)
  | get_ev "3d"   = ("3d", ev3d)
  (* 4th evalutator *)
  | get_ev "ev4"  = ("4",  ev4)
  | get_ev "ev4b" = ("4b", ev4b)
  | get_ev "4b"   = ("4b", ev4b)
  (* 5th evalutator *)
  | get_ev "ev5"  = ("5",  ev5)
  | get_ev "ev5b" = ("5b", ev5b)
  | get_ev "5b"   = ("5b", ev5b)
  (* failure *)
  | get_ev _ = raise Fail "get_ev"


(* ------ PARSING ------ *)

fun parseEML input =
    let val _    = D.reset ()
	val term = P.parse [input]
	val st1  = A.export term
	val st2  = A.toString term
	val _    = print (st1 ^ "\n")
	val _    = print (st2 ^ "\n")
    in ()
    end


(* ------ EVALUATION ------ *)

fun getLastAbstraction [] = (NONE, [])
  | getLastAbstraction lst =
    let val rev    = List.rev lst
	val last   = List.hd rev
	val firsts = List.rev (List.tl rev)
    in if NT.is_nuprl_iabstraction_term last
       then (SOME (NT.dest_iabstraction last), [])
       else let val (termop, terms) = getLastAbstraction firsts
	    in (termop, terms @ [last])
	    end
    end

fun keepAbsWf [] = []
  | keepAbsWf (term :: terms) =
    if NT.is_nuprl_iabstraction_term term
       orelse
       NT.is_nuprl_iwftheorem_term term
    then term :: keepAbsWf terms
    else keepAbsWf terms

fun pairAbsWf [] = []
  | pairAbsWf [x] = raise EH.Impossible "pairAbsWf"
  | pairAbsWf (abs :: wf :: terms) =
    if NT.is_nuprl_iabstraction_term abs
       andalso
       NT.is_nuprl_iwftheorem_term wf
    then (abs, wf) :: pairAbsWf terms
    else raise EH.Impossible "pairAbsWf"

fun splitLastAbs terms =
    let val terms' = keepAbsWf terms
	val pairs  = pairAbsWf terms'
	val rev    = List.rev pairs
    in if List.length rev > 0
       then let val (abs, wf) = List.hd rev
		val firsts = List.rev (List.tl rev)
	    in (SOME (NT.dest_iabstraction abs, wf), firsts)
	    end
       else (NONE, [])
    end

fun terms2sub terms =
    map (fn (abs,wf) =>
	    let val (cond,lhs,rhs)  = NT.dest_iabstraction abs
		val (opid,subterms) = NT.dest_simple_term (NT.rterm2term lhs)
	    in case subterms of
		   [] => (opid,NT.rterm2term rhs)
		 | _  => raise Fail "terms2sub:term-has-subterms"
	    end)
	terms

fun copy_file filein fileout =
    let val instream  = TextIO.openIn filein
	val outstream = TextIO.openOut fileout
	fun aux () =
	    case TextIO.inputLine instream of
		SOME line => (TextIO.output (outstream, line); aux ())
	      | NONE => ()
	val _ = aux ()
	val _ = TextIO.closeIn instream
	val _ = TextIO.closeOut outstream
    in ()
    end

fun evaluate_gen ev input str libop obid sub prt btyp extra =
    let val _        = D.reset ()
	val (n,str)  = case String.tokens (fn #"%" => true | _ => false) str of
			   [n,x] => (case Int.fromString n of
					 SOME m => (m,x)
				       | NONE => (~1,str))
			 | _ => (~1,str)
	(*val n = ~1*)
	(*val _        = print ("[" ^ str ^ " will be evaluated in " ^ Int.toString n ^ " steps]\n")*)
	val tmp      = OS.FileSys.tmpName ()
	val _        = copy_file input tmp
	val stout    = TextIO.openAppend tmp
	val _        = TextIO.output (stout, "let it = " ^ str ^ "\n")
	val _        = TextIO.closeOut stout
	val san      = true
    in let val (term, env, b) = E.slice [tmp] NONE libop 1 prt sub san btyp
       in if b
	  then let val s_term        = A.simplify term
		   val lib           = NONE
		   val pref          = false
		   val poly          = true
		   val stdma         = true
		   val cbva          = is_cbva_extra extra
		   val newprog       = true
		   val (n_terms1, _) = N.toNuprl lib tmp obid s_term env sub pref poly stdma btyp cbva newprog
		   val _             = OS.FileSys.remove tmp
	       in case splitLastAbs n_terms1 of
		      (SOME ((r_cond, r_left, r_right), wf), terms) =>
		      let val left   = NT.rterm2term r_left
			  val right  = NT.rterm2term r_right
			  (*val _      = print (NT.toStringTerm left  ^ "\n***********\n")*)
			  (*val _      = print (NT.toStringTerm right ^ "\n***********\n")*)
			  (*val _      = print (NT.toStringTerm wf    ^ "\n***********\n")*)
			  (*val _      = app (fn (abs,wf) => print (NT.toStringTerm abs ^ "\n***********\n")) terms*)
			  val right1 = NT.replace_terms (terms2sub terms) right
			  (*val _      = print (NT.toStringTerm right1 ^ "\n***********\n")*)
			  (*val _      = print ("[evaluating rhs]\n")*)
			  (*val right2 = ev (NT.mk_nuprl_evalall_term right) terms n*)
			  val right2 = ev (NT.mk_nuprl_evalall_term right1) n
			  (*val _      = print ("[evaluation done]\n")*)
			  val abs    = NT.mk_nuprl_iabstraction_term left right2
			  val thm    = NT.dest_iwftheorem wf
			  val sta    = NT.nuprl2eml_abs "it" abs
			  val stw    = NT.nuprl2eml_wf  "it" wf
			  (*val _ = print (NT.toStringTerm wf)*)
			  val _ =
			      if sta = "-"
				 andalso
				 n >= 0
			      then print (NT.ppTerm right2 ^ "\n")
			      else if sta = "`???`"
			      then print ("evaluation failed.\n")
			      else print (sta ^ " : " ^ stw ^ "\n")
		      (*app (fn t => print (NT.nuprl2eml t ^ "\n")) [abs, wf]*)
		      in ()
		      end
		    | _ => ()
	       end
	  else print "Untypable.\n"
       end handle e => ((OS.FileSys.remove tmp; raise e) handle _ => raise e)
    end

fun evaluate input str libop alldefop obid sub extra =
    evaluate_gen (fn t => fn s => #1 (EV.run_ev2b true alldefop s t))
		 input str libop obid sub (true, true) true extra

fun evaluate_map ev input str libop obid sub btyp extra =
    evaluate_gen (fn t => fn s => let val (u, n) = ev s t in u end)
		 input str libop obid sub (true, false) btyp extra

fun run_gen_session ev strm libop input obid sub btyp extra =
    let val _ = print "EventML# "
    in run_session ev strm libop input obid sub btyp extra
    end

and run_ev_session ev str strm libop input obid sub btyp extra =
    let val _ = print ("\nswitched to evaluator" ^ str ^ "\n")
    in run_gen_session ev strm libop input obid sub btyp extra
    end

and run_help_session ev strm libop input obid sub btyp extra =
    let val _ = print ("\n"
		       ^ "  - To evaluate an expression, type an expresion followed by ;;."
		       ^ "\n"
		       ^ "  - To quit, type 'quit'."
		       ^ "\n"
		       ^ "  - To disable the type inferencer, type 'notype'."
		       ^ "\n"
		       ^ "  - To enable the type inferencer, type 'type'."
		       ^ "\n"
		       ^ "  - To change the evaluator, type one of these: ev1, ev2, ev2b, ev2e, ev2d, ev3, ev3b, ev3c, ev3d, ev4, ev4b, ev5, ev5b"
		       ^ "\n\n")
    in run_gen_session ev strm libop input obid sub btyp extra
    end

and run_session ev strm libop input obid sub btyp extra =
    case TextIO.inputLine strm of
	NONE => run_session ev strm libop input obid sub btyp extra
      | SOME "quit\n"     => (print "\nend of session\n"; EV.end_session ())
      | SOME "exit\n"     => (print "\nend of session\n"; EV.end_session ())
      | SOME "aurevoir\n" => (print "\nend of session\n"; EV.end_session ())
      | SOME "type\n"     => (print "\nenabled type inferencer\n";  print "EventML# "; run_session ev strm libop input obid sub true  extra)
      | SOME "notype\n"   => (print "\ndisabled type inferencer\n"; print "EventML# "; run_session ev strm libop input obid sub false extra)
      | SOME "ev1\n"      => run_ev_session ev1  "1"  strm libop input obid sub btyp extra
      | SOME "ev2\n"      => run_ev_session ev2  "2"  strm libop input obid sub btyp extra
      | SOME "ev2b\n"     => run_ev_session ev2b "2b" strm libop input obid sub btyp extra
      | SOME "2b\n"       => run_ev_session ev2b "2b" strm libop input obid sub btyp extra
      | SOME "ev2e\n"     => run_ev_session ev2e "2e" strm libop input obid sub btyp extra
      | SOME "2e\n"       => run_ev_session ev2e "2e" strm libop input obid sub btyp extra
      | SOME "ev2d\n"     => run_ev_session ev2d "2d" strm libop input obid sub btyp extra
      | SOME "2d\n"       => run_ev_session ev2d "2d" strm libop input obid sub btyp extra
      | SOME "ev3\n"      => run_ev_session ev3  "3"  strm libop input obid sub btyp extra
      | SOME "ev3b\n"     => run_ev_session ev3b "3b" strm libop input obid sub btyp extra
      | SOME "3b\n"       => run_ev_session ev3b "3b" strm libop input obid sub btyp extra
      | SOME "ev3c\n"     => run_ev_session ev3c "3c" strm libop input obid sub btyp extra
      | SOME "3c\n"       => run_ev_session ev3c "3c" strm libop input obid sub btyp extra
      | SOME "ev3d\n"     => run_ev_session ev3d "3d" strm libop input obid sub btyp extra
      | SOME "3d\n"       => run_ev_session ev3d "3d" strm libop input obid sub btyp extra
      | SOME "ev4\n"      => run_ev_session ev4  "4"  strm libop input obid sub btyp extra
      | SOME "ev4b\n"     => run_ev_session ev4b "4b" strm libop input obid sub btyp extra
      | SOME "4b\n"       => run_ev_session ev4b "4b" strm libop input obid sub btyp extra
      | SOME "ev5\n"      => run_ev_session ev5  "5"  strm libop input obid sub btyp extra
      | SOME "ev5b\n"     => run_ev_session ev5b "5b" strm libop input obid sub btyp extra
      | SOME "5b\n"       => run_ev_session ev5b "5b" strm libop input obid sub btyp extra
      | SOME "help\n"     => run_help_session ev strm libop input obid sub btyp extra
      | SOME line         =>
	let val _ = evaluate_map ev input line libop obid sub btyp extra
	    val _ = print "EventML# "
	in run_session ev strm libop input obid sub btyp extra
	end

fun start_session ev input libop alldefop obid sub extra =
    let val _    = print "Loading library...\n"
	val _    = EV.start_session false alldefop
	val strm = TextIO.stdIn
	val _    = print "EventML# "
	val btyp = true
    in run_session ev strm libop input obid sub btyp extra
    end


(* ------ TYPE CHECKER ------ *)

val eval_test = EV.test

val typecheck = E.typecheck

fun slice input outputop libop timelimit sub =
    let val san  = false
	val btyp = true
	val (prog, env, b) = E.slice [input] outputop libop timelimit (true, true) sub san btyp
    in ()
    end

fun dump_prog_stream n_terms prog file =
    let val _      = print_eml "dumping programn"
	val time   = #stime (Posix.ProcEnv.times ())
	val output = file ^ "-" ^ Time.toString time
	val _      = NT.toStringTermStream prog output
	val _      = print_eml "program dumped"
    in ()
    end

fun dump_prog n_terms prog fdump =
    let	val time   = #stime (Posix.ProcEnv.times ())
	val output = "output-term-" ^ Time.toString time
	val _      = print_eml ("dumping program in " ^ output)
	val stout  = TextIO.openOut output
	val _      = TextIO.output (stout, fdump prog ^ "\004\n\n")
	val _      = map (fn t => TextIO.output (stout, fdump t ^ "\004\n")) n_terms
	val _      = TextIO.closeOut stout
    in ()
    end

fun pp_dump_prog_op nterms (SOME (_, prog)) = dump_prog nterms prog NT.ppTerm
  | pp_dump_prog_op _ _ = ()

fun dump_prog_op nterms (SOME (_, prog)) = dump_prog nterms prog NT.toStringTerm
  | dump_prog_op _ _ = ()

fun dump_hs prog =
    let val lib     = EV.get_lib ()
	val time    = #stime (Posix.ProcEnv.times ())
	val date    = Date.fmt "%j-%m-%d" (Date.fromTimeLocal time)
	val output  = "program-" ^ date ^ "-" ^ Time.toString time
	val output1 = output ^ ".unfolded"
	val output2 = output ^ ".hs"
	val _       = print_eml ("dumping unfolded program in " ^ output1)
	val _       = NT.print_lib_stats lib
	val prog'   = NT.unfold_all lib prog
	val stout1  = TextIO.openOut output1
	val _       = TextIO.output (stout1, NT.toStringTerm prog')
	val _       = TextIO.closeOut stout1
	val _       = print_eml ("dumping haskell program in " ^ output2)
	val stout2  = TextIO.openOut output2
	val _       = TextIO.output (stout2, NT.nuprl2haskell prog')
	val _       = TextIO.closeOut stout2
    in ()
    end

fun dump_hs_op () =
    case !program of
	SOME (_, prog) => dump_hs prog
      | NONE => ()

fun dump_sml prog =
    let (*val lib     = EV.get_lib ()*)
	val time    = #stime (Posix.ProcEnv.times ())
	val date    = Date.fmt "%j-%m-%d" (Date.fromTimeLocal time)
	val output  = "program-" ^ date ^ "-" ^ Time.toString time
	(*val output1 = output ^ ".unfolded"*)
	val output2 = output ^ ".sml"
	(*val _       = print_eml ("dumping unfolded program in " ^ output1)*)
	(*val _       = NT.print_lib_stats lib*)
	(*val prog'   = NT.unfold_all lib prog*)
	(*val stout1  = TextIO.openOut output1*)
	(*val _       = TextIO.output (stout1, NT.toStringTerm prog')*)
	(*val _       = TextIO.closeOut stout1*)
	(*val _       = print_eml ("term stats: " ^ NT.stats_term_to_string prog)*)
	val _       = print_eml ("dumping sml program in " ^ output2)
	val stout2  = TextIO.openOut output2
	val _       = TextIO.output (stout2, NT.nuprl2sml prog(*'*))
	val _       = TextIO.closeOut stout2
    in ()
    end

fun dump_sml_op () =
    case !program of
	SOME (_, prog) => dump_sml prog
      | NONE => ()

fun dump_tagless prog =
    let (*val lib     = EV.get_lib ()*)
	val time    = #stime (Posix.ProcEnv.times ())
	val date    = Date.fmt "%j-%m-%d" (Date.fromTimeLocal time)
	val output  = "program-" ^ date ^ "-" ^ Time.toString time
	(*val output1 = output ^ ".unfolded"*)
	val output2 = output ^ ".sml"
	(*val _       = print_eml ("dumping unfolded program in " ^ output1)*)
	(*val _       = NT.print_lib_stats lib*)
	(*val prog'   = NT.unfold_all lib prog*)
	(*val stout1  = TextIO.openOut output1*)
	(*val _       = TextIO.output (stout1, NT.toStringTerm prog')*)
	(*val _       = TextIO.closeOut stout1*)
	(*val _       = print_eml ("term stats: " ^ NT.stats_term_to_string prog)*)
	val _       = print_eml ("dumping sml program in " ^ output2)
	val stout2  = TextIO.openOut output2
	val _       = TextIO.output (stout2, NT.tagless prog(*'*))
	val _       = TextIO.closeOut stout2
    in ()
    end

fun dump_tagless_op () =
    case !program of
	SOME (_, prog) => dump_tagless prog
      | NONE => ()

fun dump_lisp extra prog =
    let val lib     = EV.get_lib ()
	val time    = #stime (Posix.ProcEnv.times ())
	val date    = Date.fmt "%j-%m-%d" (Date.fromTimeLocal time)
	val output  = "program-" ^ date ^ "-" ^ Time.toString time
	val output1 = output ^ ".unfolded"
	val output2 = output ^ ".lisp"
	(*val _       = print_eml ("dumping unfolded program in " ^ output1)*)
	(*val _       = NT.print_lib_stats lib*)
	(*val prog'   = NT.unfold_all lib prog*)
	(*val stout1  = TextIO.openOut output1*)
	(*val _       = TextIO.output (stout1, NT.toStringTerm prog')*)
	(*val _       = TextIO.closeOut stout1*)
	(*val _       = print_eml ("term stats: " ^ NT.stats_term_to_string prog)*)
	val _       = print_eml ("dumping lisp program in " ^ output2)
	val stout2  = TextIO.openOut output2
	val unfold  = if is_false_extra extra then false else true
	val _       = TextIO.output (stout2, NT.nuprl2lisp lib unfold prog(*'*))
	val _       = TextIO.closeOut stout2
    in ()
    end

fun dump_lisp_op extra =
    case !program of
	SOME (_, prog) => dump_lisp extra prog
      | NONE => ()

fun set_light_terms () = (print_eml "setting dummy tag"; NT.set_dummy_dtag ())

fun set_heavy_terms () = NT.reset_dtag ()

fun prog2lisp prt extra input (SOME output) alldefs =
    if String.isSuffix ".prog" input
    then let val _     = set_light_terms ()
	     val light = true
	     val split = false
	     val _     = print_eml "parsing program"
	     val terms = PN.parse prt light NT.to_filter_out input split
	 in case terms of
		(prog :: rest) =>
		let val (vars, _) = NT.dest_lambdas 6 prog
		    val vars'     = map NT.dest_nuprl_var vars
		    val _         = EV.start_session false alldefs
		    val _         = EV.add_to_session rest
		    val lib       = EV.get_lib ()
		    val stout     = TextIO.openOut output
		    val unfold    = false (*if is_false_extra extra then false else true*)
		    val _         = print_eml "exporting to lisp"
		    val conv      = if spget_int_extra extra = 1
				    then NT.nuprl2lisp
				    else NT.nuprl2lisp2
		    val _         = TextIO.output (stout, conv lib unfold prog)
		    val _         = TextIO.closeOut stout
		in print_eml ("lisp code generated in " ^ output)
		end
	      | _ => print_eml "file does not contain a program"
	 end
    else print_eml "file is not a .prog file"
  | prog2lisp prt extra input NONE libop = print_eml "no output file specified"

fun toNuprl input outputop libop alldefop obid timelimit sub prt sprt poly btyp extra =
    let val san    = false
	(* export is true if we actually want to export the terms in a file *)
	val _      = if Option.isSome outputop
		     then set_heavy_terms ()
		     else set_light_terms ()
	val (term, env, b) = E.slice [input] NONE libop timelimit sprt sub san btyp
    in if b
       then let val s_term = A.simplify term
		val mapop =
		    case alldefop of
			SOME f =>
			let val _     = print_eml "parsing Nuprl library"
			    val light = false
			    val terms = PN.parse prt light NT.to_filter_out f false
			    val _     = print_eml "generating EML library"
			    val lib   = NT.terms2map 1 terms
			    val _     = NT.print_lib_stats lib
			    val _     = EV.start_session_lib lib
			    val _     = print_eml "library loaded"
			    val _     = loaded := true
			in SOME lib
			end
		      | NONE => NONE
		val pref    = true
		val stdma   = true
		val cbva    = is_cbva_extra extra
		val newprog = true
		val (n_terms, prog) = N.toNuprl mapop input obid s_term env sub pref poly stdma btyp cbva newprog
		(* -- Add programs to library -- *)
		val _  = EV.add_to_session n_terms
		val _  = set_program_op prog
		val _  =
		    if is_dump_extra extra
		    then dump_prog_op n_terms prog
		    else ()
		val st =
		    if prt
		    then app (fn n_term => print (NT.toStringTerm n_term ^ "\n")) n_terms
		    else ()
		val _  =
		    case outputop of
			NONE => ()
		      | SOME file =>
			let val file' = file ^ ".tmp"
			    val stout = TextIO.openOut file'
			    val _     = map (fn n_term => TextIO.output (stout, NT.toStringTerm n_term ^ "\004\n")) n_terms
			    val _     = TextIO.closeOut stout
			    val _     = OS.FileSys.rename {old = file', new = file}
			in () end
			handle IO.Io {name, function, cause} =>
			       (raise EH.Impossible ("cannot open or close one of the output file (function:" ^ function ^ ",name:" ^ name ^ ")\n"))
	    in n_terms
	    end
       else (print "We won't convert the program into a Nuprl term because it contains errors.\n"; [])
    end

fun parseNuprl input outputop prt split extra =
    let val force = is_true_extra extra
	val light = false
    in GL.exportTerms input outputop prt light split force
    end

fun sanitizer input outputop libop timelimit sub =
    let val san  = true
	val btyp = true
	val (prog, env, b) = E.slice [input] outputop libop timelimit (true, true) sub san btyp
    in ()
    end


(* ------ PROTOCOL SIMULATION ------ *)

(*
fun getComponents term =
    if NT.is_nuprl_cons_term term
    then let val (head, tail) = NT.dest_cons term
	 in if NT.is_nuprl_pair_term head
	    then (NT.dest_pair 1 head) :: (getComponents tail)
	    else raise EH.Impossible "getComponents"
	 end
    else if NT.is_nuprl_nil_term term
    then []
    else raise EH.Impossible "getComponents"
*)

fun how_many_components term =
    if NT.is_nuprl_pair_term term
    then 1 + how_many_components (#2 (NT.dest_pair 2 term))
    else 1

fun nuprl_all t   =
    if NT.is_ct t
    then let val (a,e) = NT.dest_ct t
	 in NT.mk_rct (nuprl_all a, e)
	 end
    else NT.mk_set_cbva_term t

fun nuprl_app p t =
    if NT.is_ct p
    then let val (a,e) = NT.dest_ct p
	 in NT.mk_rct (nuprl_app a t, e)
	 end
    else NT.mk_apply_term p t

(*
fun testing 0 = EV.start_session true (SOME default_alldefs)
  | testing 1 = EV.end_session ()
  | testing n =
    let val terms   = GL.parseTerms "output" false false
	(* list of locations: *)
	val pid     = NT.mk_mkid_term "p"
	val l1id    = NT.mk_mkid_term "l1"
	val l2id    = NT.mk_mkid_term "l2"
	val ids     = [pid, l1id, l2id]
	val lstls   = NT.mk_nuprl_finite_list_term [l1id, l2id]
    in case terms of
	   [prog] =>
	   let (* program: *)
	       val prog1 = NT.mk_apply_term prog lstls
	       val prog2 = NT.mk_apply_term prog1 pid
	       val prog3 = #1 (ev1 [] (~1) prog2)
	       fun build id = NT.mk_nuprl_df_program_meaning_term (nuprl_app prog3 id)
	       (*val xxx =
		   let val x = ev n (build pid)
		       val _ = print ("\n-+1+-----------\n" ^ NT.toStringTerm prog  ^ "\n-+1+-----------\n")
		       val _ = print ("\n-+2+-----------\n" ^ NT.toStringTerm prog3 ^ "\n-+2+-----------\n")
		       val _ = print ("\n-+3+-----------\n" ^ NT.toStringTerm x     ^ "\n-+3+-----------\n")
		       val _ = raise Fail ""
		   in ()
		   end*)
	       val lst = map (fn id => (id, #1 (ev1 [] (~1) (build id)))) ids
	       (* inital message: *)
	       val client = NT.mk_mkid_term "client"
	       val start  = NT.mk_nuprl_token_term "start"
	       val hdr    = NT.mk_nuprl_finite_list_term [start]
	       val loc    = NT.mk_nuprl_loc_term
	       val msg    = NT.mk_nuprl_make_msg_term hdr loc client
	   (* now we select the program associated with "p" *)
	   in case List.find (fn (id, p) => NT.alpha_equal_terms id pid) lst of
		  SOME (i, p) =>
		  let (*val _ = print ("\n-+a+-----------\n" ^ NT.toStringTerm p  ^ "\n-+a+-----------\n")*)
		      val p' = #1 (ev1 [] (~1) (nuprl_all (nuprl_app p msg)))
		  in print ("\n-+b+-----------\n" ^ NT.toStringTerm p' ^ "\n-+b+-----------\n")
		  end
		| NONE => ()
	   end
	 | _ => ()
    end
*)

(*
fun extractInfoParam list =
    let val locs =
	    map (fn loc =>
		    if String.isPrefix "LOC(" loc
		       andalso
		       String.isSuffix ")" loc
		    then let val to = String.size loc - 5
			     val id = String.substring (loc, 4, to)
			 in NT.mk_mkid_term id
			 end
		    else raise Fail ("extract_info_param - " ^ loc))
		list
    in case locs of
	   []  => raise Fail ("extract_info_param - " ^ Int.toString (List.length locs))
	 | [l] => l
	 | _   => NT.mk_nuprl_finite_list_term locs
    end

(* Write a proper parser for that! *)
fun load_config config =
    let val strm = TextIO.openIn config
	val fsep = fn #" " => true | _ => false
	val locations =
	    case TextIO.inputLine strm of
		SOME line =>
		let val line = String.substring (line, 0, String.size line - 1)
		    val elts = String.tokens fsep line
		in case elts of
		       x :: xs =>
		       if x = "locations:"
		       then map NT.mk_mkid_term xs
		       else raise Fail "load_config"
		     | [] => raise Fail "load_config"
		end
	      | NONE => raise Fail "load_config"
	fun getParams () =
	    case TextIO.inputLine strm of
		SOME line =>
		let val line = String.substring (line, 0, String.size line - 1)
		    val elts = String.fields fsep line
		in case elts of
		       (name :: vs) =>
		       if name = ""
		       then getParams ()
		       else (case String.fields (fn #":" => true | _ => false) name of
				 [name, ""] => (name, extractInfoParam vs) :: getParams ()
			       | _ => raise Fail "load_config - name")
		     | _ => raise Fail "load_config - elts"
		end
	      | NONE => []
	val params = getParams ()
	val _ = TextIO.closeIn strm
    in (locations, params, [])
    end
*)


(* ------ operation on configation file ------ *)

fun get_locs_from_groups groups =
    foldr (fn ((name,mem,locs),lst) =>
	      (map (fn (i,h,p) => (i,h,p,name,mem)) locs) @ lst)
	  []
	  groups

fun get_sp_locs_from_groups groups =
    foldr (fn ((name,mem,locs),lst) => locs @ lst)
	  []
	  groups

fun get_internal_locs_from_groups groups =
    foldr (fn ((name,mem,locs),lst) =>
	      if mem
	      then (map (fn (i,h,p) => (i,h,p,name,mem)) locs) @ lst
	      else lst)
	  []
	  groups

fun get_sp_internal_locs_from_groups groups =
    foldr (fn ((name,mem,locs),lst) =>
	      if mem
	      then locs @ lst
	      else lst)
	  []
	  groups

fun get_external_locs_from_groups groups =
    foldr (fn ((name,mem,locs),lst) =>
	      if mem
	      then lst
	      else (map (fn (i,h,p) => (i,h,p,name,mem)) locs) @ lst)
	  []
	  groups

fun get_sp_external_locs_from_groups groups =
    foldr (fn ((name,mem,locs),lst) =>
	      if mem
	      then lst
	      else locs @ lst)
	  []
	  groups

(* find the group from which (ip, port) belongs to *)
fun find_group (ip, port) groups =
    List.find
	(fn (name,mem,locs) =>
	    List.exists (fn (i,h,p) => ip = h andalso port = p) locs)
	groups

fun find_lt_groups name conns =
    List.foldr
	(fn ((from,to),names) =>
	    if name = to
	    then from :: names
	    else names)
	[]
	conns

fun find_gt_groups name conns =
    List.foldr
	(fn ((from,to),names) =>
	    if name = from
	    then to :: names
	    else names)
	[]
	conns

fun filter_groups groups names =
    List.filter
	(fn (name,mem,locs) => List.exists (fn x => x = name) names)
	groups

fun get_group_op groups name = List.find (fn (n,mem,locs) => n = name) groups
fun get_group groups name =
    case get_group_op groups name of
	SOME (name,mem,locs) => locs
      | NONE => []

(*
fun load_config_parse_locations locs =
    map (fn (loc, host, port) =>
	    (NT.mk_mkid_term loc,
	     NT.mk_nuprl_ihost_term host,
	     NT.mk_nuprl_iport_term port))
	locs

fun load_config_parse_groups groups =
    map (fn (name, mem, locs) => (name, mem, load_config_parse_locations locs))
	groups
*)

fun old_mk_nuprl_message hdr typ body =
    let val pair = NT.mk_pair_term typ body
    in NT.mk_pair_term hdr pair
    end

fun mk_nuprl_message hdr typ body = NT.mk_pair_term hdr body

fun load_config_parse_messages msgs =
    map (fn (id, (hdr, typ, body)) =>
	    IM.mk_message 0 id (mk_nuprl_message hdr typ body))
	msgs

fun load_config_parse config =
    let val (grps, conns, params, msgs) = C.parse config
	val groups   = (*load_config_parse_groups*)   grps
	val messages = load_config_parse_messages msgs
    in (groups, conns, params, messages)
    end

fun load_config_parse_str str = load_config_parse_messages (#4 (C.parseString str))


(* ------ ... ------ *)

fun testing _ = (load_config_parse "new_conf"; ())

fun testing n =
    let val _ = EV.start_session true (SOME default_alldefs)
	val _ = print_eml "loaded library"
    in eval_test n
    end

fun getParams params map_params =
    case params of
	[] => []
      | _  =>
	let val last_param = List.last params
	    val params' =
		(* sometimes when we unfold the abstractions such as the LoE
		 * combinators, we get the parameters followed by a \i. which
		 * is fact not a parameter but the location of the program. *)
		if last_param = "i"
		then List.take (params, List.length params - 1)
		else params
	in map (fn param =>
		   case List.find (fn (id : string, _) => id = param) map_params of
		       SOME (id, v) => v
		     | NONE => raise Fail ("getParams(" ^ param ^ ")"))
	       params'
	end

fun update_component (i,s) cmps =
    map (fn (j,p) => if i = j then (i,s) else (j,p)) cmps

fun print_message_list _ [] = ()
  | print_message_list n (msg :: msgs) =
    (print (Int.toString n ^ ": " ^ NT.nuprlTerm2eml msg ^ "\n--\n");
     print_message_list (n + 1) msgs)

fun unfold_intransit_message lib = IM.apply2msg (NT.unfold_all lib)

fun unfold_intransit_messages lib = map (unfold_intransit_message lib)

fun print_messages_intransit () =
    (print ("\n-------IN-TRANSIT-------\n");
     print_message_list 1 (map IM.message2term (!intransit)))

fun debug_eval previous_result term n =
    let val _ = print ("-" ^ Int.toString n ^ "\n")
	val (p1,n1) = ev1 n term
	val (p2,n2) = ev2 n term
    in if n1 = n2 andalso NT.alpha_equal_terms p1 p2
       then if n1 = n
	    then debug_eval p1 term (n + 1)
	    else print ("equal[1:" ^ Int.toString n1 ^ ",2:" ^ Int.toString n2 ^ "]\n")
       else let val stout = TextIO.openOut "output-old"
		val _     = TextIO.output (stout, NT.toStringTerm previous_result ^ "\004\n")
		val _     = TextIO.closeOut stout
		val stout = TextIO.openOut "output-1"
		val _     = TextIO.output (stout, NT.toStringTerm p1 ^ "\004\n")
		val _     = TextIO.closeOut stout
		val stout = TextIO.openOut "output-2"
		val _     = TextIO.output (stout, NT.toStringTerm p2 ^ "\004\n")
		val _     = TextIO.closeOut stout
	    in print ("diff[1:" ^ Int.toString n1 ^ ",2:" ^ Int.toString n2 ^ "]\n")
	    end
    end

fun run_stepper n ev gc debug remove config alldefs extra =
    if n < 0
    then let val _ = configured := false
	     val _ = components := []
	     val _ = intransit  := []
	 in ()
	 end
    else if !loaded
    then if !configured
	 then case !intransit of
		  [] => print_eml "no message in transit"
		| lst =>
		  if n > List.length lst orelse n < 1
		  then print_eml "out of bound"
		  else if remove
		  then let val new_msgs = (List.take (lst, n - 1)) @ (List.drop (lst, n))
			   val _ = intransit := new_msgs
		       in ()
		       end
		  else let val (delay,id,msg) = IM.dest_message (List.nth (lst, n - 1))
			   (*val _ = print ("-+-"
					  ^ id
					  ^ "-+-"
					  ^ NT.toStringTerm msg
					  ^ "\n")*)
		       in case List.find (fn (i,_) => i = id) (!components) of
			      SOME (i,ip) =>
			      let (*val _ = print ("\n-+a+-----------\n" ^ NT.toStringTerm p  ^ "\n-+a+-----------\n")*)
				  (*val _  = print ("[size: " ^ IntInf.toString (NT.large_size p) ^ "]\n")*)
				  val _ = print_eml ("size(1): " ^ Int.toString (NT.size ip))
				  val pop =
				      let val p = NT.compute_to_inj ip
				      in if NT.is_nuprl_inl_term p
					 then SOME (NT.dest_inl p)
					 else if NT.is_nuprl_inr_term p
					 then NONE
					 else raise Fail ""
				      end handle _ =>
						 (dump_prog [] ip NT.toStringTerm;
						  raise Fail ("run_stepper:newprog:not_inl_or_inr("
							      ^ NT.opid_of_term ip
							      ^ ")"))
			      in case pop of
				     SOME p =>
				     let val toeval = nuprl_all (nuprl_app p msg)
					 val (p',stps) = ev (~1) toeval
					     handle EVAL_ERROR str =>
						    (dump_prog [] ip NT.toStringTerm;
						     raise Fail ("run_stepper:evaluation_error("
								 ^ str
								 ^ "):cannot_recover"))
					 val _ = if debug
						 then debug_eval toeval toeval 2025
						 else () (**)
					 val _ = print_eml ("steps: " ^ Int.toString stps)
					 (*val _ = raise Fail "--"*)
					 val (s,msgs) = NT.dest_pair 4 p'
					 val _  = print_eml ("size(2): " ^ IntInf.toString (NT.large_size s))
					 (*val _  = print ("[depth: " ^ Int.toString (NT.env_depth s) ^ "]\n")*)
					 (*val _  = print ("[env: " ^ Int.toString (NT.env_size s) ^ "]\n")*)
					 (*val _ = print ("\n-+b+-----------\n" ^ NT.toStringTerm p' ^ "\n-+b+-----------\n")*)
					 (*val _ = print ("\n-------MSGS------\n" ^ NT.toStringTerm msgs ^ "\n------------\n")*)
					 val lst_msgs = map IM.term2message (NT.dest_list msgs)
					 val new_cmps = update_component (i,s) (!components)
					 val new_msgs = (List.take (lst, n - 1)) @ (List.drop (lst, n)) @ lst_msgs
					 val _ = components := new_cmps
					 val _ = intransit := new_msgs
				     in ()
				     end
				   | NONE => print_eml "program finished"
			      end
			    | NONE => print_eml "no recipient"
		       end
	 else let val (groups, conns, params, messages) = load_config_parse config
	      (* gets the locations and parameters from the input config file *)
	      (*val (locations, params, messages) = load_config config*)
	      (* generates the list of components *)
	      in case !program of
		     SOME (prms, p) =>
		     let val args = getParams prms params
			 val _    = print_eml ("size initial component: " ^ Int.toString (NT.size p))
			 val lib  = EV.get_lib ()
			 val _    = NT.print_lib_stats lib
			 val (prog0,messages,args) =
			     if is_fold_extra extra orelse not (is_unfold_extra extra)
			     then let val _ = print_eml "trimming library"
				      val _ = NT.trim_lib_terms lib (p :: args @ (map IM.message2term messages))
				      val _ = NT.print_lib_stats lib
				      val _ = if gc then MLton.GC.collect() else ()
				  in (p, messages, args)
				  end
			     else let val _    = print_eml "unfolding"
				      val prog = NT.unfold_all lib p
				      val msgs = unfold_intransit_messages lib messages
				      val args = map (NT.unfold_all lib) args
				      val _    = print_eml ("size after unfolding: " ^ Int.toString (NT.size prog))
				      val _    = print_eml ("term stats: " ^ NT.stats_term_to_string prog)
				      val _    = print_eml "GC library"
				      val _    = EV.reset_lib ()
				      val _    = if gc then MLton.GC.collect() else ()
				  in (prog, msgs, args)
				  end
			 val prog1 =
			     (*if newprog_extra extra
			     then let val p = NT.partial_ev_opt prog0
				      val _ = print_eml ("size after partial evaluation: " ^ Int.toString (NT.size p))
				      val _ = if is_dump_extra extra then dump_prog [] p NT.toStringTerm else ()
				  in p
				  end
			     else*) prog0
			 val prog2 = NT.mk_nuprl_applies_term prog1 args
			 (* --- TEST --- *)
			 val prog2 =
			     if is_fold_extra extra orelse not (is_unfold_extra extra)
			     then prog2
			     else let val prog2 = NT.TERM2term prog2
				      val _ = NT.test_full_coverage prog2
				  in prog2
				  end
			 (* ------------ *)
			 fun build id =
			     let val prog2 =
				     if is_fold_extra extra orelse not (is_unfold_extra extra)
				     then prog2
				     else NT.set_all_nuprl_var prog2 (* TEST *)
				 val mkid  = NT.mk_mkid_term id
			     in nuprl_app prog2 mkid
			     end
			 val cmps =
			     map (fn (id,_,_) =>
				     let val _ = print_eml ("generating program for " ^ id)
					 val b = build id
					 val (p,stps) = ev (~1) b
					     handle EVAL_ERROR str =>
						    (dump_prog [] b NT.toStringTerm;
						     raise Fail ("run_stepper:evaluation_error("
								 ^ str
								 ^ "):cannot_recover"))
					 val _ = print_eml ("size component: " ^ Int.toString (NT.size p))
					 (*val _ = print_eml ("size_var component: " ^ Int.toString (NT.size_var p))*)
					 val p' = NT.partial_ev_opt p
					 val _  = print_eml ("size after partial evaluation: " ^ Int.toString (NT.size p'))
				     in (id, p')
				     end)
				 (get_sp_internal_locs_from_groups groups)
			 val _ = components := cmps
			 val _ = intransit  := messages
			 val _ = configured := true
			 val _ = print_eml "components loaded"
		     in ()
		     end
		   | NONE => raise Fail "no program loaded in memory"
	      end
    else let (* loads the alldefs file *)
	    val _ = EV.start_session true (SOME default_alldefs)
	    val _ = loaded := true
	    val _ = print_eml "library loaded"
	in ()
	end

fun loop_gen_stepper ev gc strm conf alldefs extra =
    let val _ = print "message? "
    in loop_stepper ev gc strm conf alldefs extra
    end

and loop_ev_stepper ev str gc strm conf alldefs extra =
    let val _ = print ("\nswitched to evaluator" ^ str ^ "\n")
    in loop_gen_stepper ev gc strm conf alldefs extra
    end

and loop_help_stepper ev gc strm conf alldefs extra =
    let val _ = print ("\n"
		       ^ "  - To send a message enter an integer corresponding to a message in transit."
		       ^ "\n"
		       ^ "  - To quit, type quit."
		       ^ "\n"
		       ^ "  - To change the evaluator, type one of these: ev1, ev2, ev2b, ev2e, ev2d, ev3, ev3b, ev3c, ev3d, ev4, ev4b, ev5, ev5b"
		       ^ "\n\n")
    in loop_gen_stepper ev gc strm conf alldefs extra
    end

and loop_stepper ev gc strm conf alldefs extra =
    case TextIO.inputLine strm of
	NONE => loop_stepper ev gc strm conf alldefs extra
      | SOME "quit\n"     => (print "\nend of session\n"; run_stepper (~1) ev gc false false conf alldefs extra)
      | SOME "exit\n"     => (print "\nend of session\n"; run_stepper (~1) ev gc false false conf alldefs extra)
      | SOME "aurevoir\n" => (print "\nend of session\n"; run_stepper (~1) ev gc false false conf alldefs extra)
      | SOME "ev1\n"      => loop_ev_stepper ev1  "1"  gc strm conf alldefs extra
      | SOME "ev2\n"      => loop_ev_stepper ev2  "2"  gc strm conf alldefs extra
      | SOME "ev2b\n"     => loop_ev_stepper ev2b "2b" gc strm conf alldefs extra
      | SOME "2b\n"       => loop_ev_stepper ev2b "2b" gc strm conf alldefs extra
      | SOME "ev2e\n"     => loop_ev_stepper ev2e "2e" gc strm conf alldefs extra
      | SOME "2e\n"       => loop_ev_stepper ev2e "2e" gc strm conf alldefs extra
      | SOME "ev2d\n"     => loop_ev_stepper ev2d "2d" gc strm conf alldefs extra
      | SOME "2d\n"       => loop_ev_stepper ev2d "2d" gc strm conf alldefs extra
      | SOME "ev3\n"      => loop_ev_stepper ev3  "3"  gc strm conf alldefs extra
      | SOME "ev3b\n"     => loop_ev_stepper ev3b "3b" gc strm conf alldefs extra
      | SOME "3b\n"       => loop_ev_stepper ev3b "3b" gc strm conf alldefs extra
      | SOME "ev3c\n"     => loop_ev_stepper ev3c "3c" gc strm conf alldefs extra
      | SOME "3c\n"       => loop_ev_stepper ev3c "3c" gc strm conf alldefs extra
      | SOME "ev3d\n"     => loop_ev_stepper ev3d "3d" gc strm conf alldefs extra
      | SOME "3d\n"       => loop_ev_stepper ev3d "3d" gc strm conf alldefs extra
      | SOME "ev4\n"      => loop_ev_stepper ev4  "4"  gc strm conf alldefs extra
      | SOME "ev4b\n"     => loop_ev_stepper ev4b "4b" gc strm conf alldefs extra
      | SOME "4b\n"       => loop_ev_stepper ev4b "4b" gc strm conf alldefs extra
      | SOME "ev5\n"      => loop_ev_stepper ev5  "5"  gc strm conf alldefs extra
      | SOME "ev5b\n"     => loop_ev_stepper ev5b "5b" gc strm conf alldefs extra
      | SOME "5b\n"       => loop_ev_stepper ev5b "5b" gc strm conf alldefs extra
      | SOME "help\n"     => loop_help_stepper ev gc strm conf alldefs extra
      | SOME line         =>
	let fun aux [] =
		(print_messages_intransit ();
		 print "message? ";
		 loop_stepper ev gc strm conf alldefs extra)
	      | aux (input :: inputs) =
		let val (input',debug,remove) =
			if String.isPrefix "d" input
			then (String.substring (input, 1, (String.size input) - 1), true, false)
			else if String.isPrefix "r" input
			then (String.substring (input, 1, (String.size input) - 1), false, true)
			else (input, false, false)
		in case Int.fromString input' of
		       SOME n => (run_stepper n ev gc debug remove conf alldefs extra;
				  if gc then MLton.GC.collect() else ();
				  aux inputs)
		     | NONE => (print "please, enter a nat\n";
				print "message? ";
				loop_stepper ev gc strm conf alldefs extra)
		end
	in aux (String.tokens (fn #" " => true | #"\n" => true | _ => false) line)
	end (*handle _ => print "empty line"*)

fun load_program_from_file input outputop lib alldefs obid time sub prt sprt poly btyp extra loc =
    let val _ =
	    if String.isSuffix ".prog" input
	    then let val _ = if Option.isSome outputop
			     then set_heavy_terms ()
			     else set_light_terms ()
		     (*val _ = print_eml ("loading program from Nuprl ASCII file: " ^ input)*)
		     val light = true
		     val terms = PN.parse prt light NT.to_filter_out input false
		 in case terms of
			(prog :: rest) =>
			let val (vars, _) = NT.dest_lambdas 6 prog
			    val vars' = map NT.dest_nuprl_var vars
			    val _ = set_program_op (SOME (vars', prog))
			    (*val _ = NT.test_full_coverage prog*)
			    (*val _ = print_eml ("term stats: " ^ NT.stats_term_to_string prog)*)
			    val _ = print_eml_id loc "loading Nuprl library"
			    val _ = EV.start_session false alldefs
			    val _ = loaded := true
			    val _ = print_eml_id loc "library loaded"
			    val _ = EV.add_to_session rest
			in ()
			end
		      | _ => raise Fail "error, no program"
		 end
	    else let val _ = print_eml_id loc "loading program from EML spec"
		     val _ = toNuprl input outputop lib alldefs obid time sub prt sprt poly btyp extra
		 in ()
		 end
	val _  =
	    if is_haskell_extra extra
	    then (print_eml_id loc "dumping haskell"; dump_hs_op ())
	    else print_eml_id loc "not dumping haskell"
	val _  =
	    if is_sml_extra extra
	    then (print_eml_id loc "dumping sml"; dump_sml_op ())
	    else print_eml_id loc "not dumping sml"
	val _  =
	    if is_lisp_extra extra
	    then (print_eml_id loc "dumping lisp"; dump_lisp_op extra)
	    else print_eml_id loc "not dumping lisp"
	val _  =
	    if is_tagless_extra extra
	    then (print_eml_id loc "dumping tagless"; dump_tagless_op ())
	    else print_eml_id loc "not dumping tagless"
    in ()
    end

fun start_stepper spec conf ev gc lib alldefs obid time sub prt extra =
    let (* loads library and generates program *)
	val _     = print_eml "type checking and loading library"
	val poly  = true
	val btyp  = true
	val sprt  = (true, false)
	val outop = NONE
	val _     = load_program_from_file spec outop lib alldefs obid time sub prt sprt poly btyp extra "stepper"
	(* evaluators *)
	val _     = print_eml "available evaluators: 1/2/2b/3/3b/3c/3d/4/4b/5/5b"
	(* loads the components *)
	val _     = print_eml "loading components"
	val _     = run_stepper 0 ev gc false false conf alldefs extra
	(* print the list of messages initially in transit *)
	val _     = print_eml "running simulator"
	val _     = print_messages_intransit ()
	val _     = print "message? "
	(* generates stream on the standard input *)
	val strm  = TextIO.stdIn
    (* starts simulator *)
    in loop_stepper ev gc strm conf alldefs extra
    end

fun alpha_equal_programs prog1 prog2 =
    let val _      = print_eml "checking whether programs are alpha equals"
	val _      = NT.set_dummy_dtag ()
	val prt    = false
	val light  = true
	val split  = false
	val terms1 = PN.parse prt light NT.to_filter_out prog1 split
	val terms2 = PN.parse prt light NT.to_filter_out prog2 split
	val p1     = NT.partial_ev_opt (List.hd terms1)
	val p2     = NT.partial_ev_opt (List.hd terms2)
    in NT.alpha_eq_terms p1 p2
    end

(* ------ SOCKET STUFF ------ *)

val ip_server = "127.0.0.1"
val port_server = 8987

fun addr_to_string addr =
    let val (a,p) = INetSock.fromAddr addr
    in "(" ^ NetHostDB.toString a ^ "," ^ Int.toString p ^ ")"
    end

val word_to_char     = Char.chr o Word8.toInt
val word_to_str      = String.str o word_to_char
val vector_to_string = Word8Vector.foldl (fn (elt, str) => str ^ word_to_str elt) ""
val slice_to_string  = vector_to_string o Word8VectorSlice.vector

val a_sock_ref = ref ([] : (INetSock.inet, Socket.active  Socket.stream) Socket.sock list)
val p_sock_ref = ref ([] : (INetSock.inet, Socket.passive Socket.stream) Socket.sock list)

fun add_a_sock_ref sock = a_sock_ref := sock :: !a_sock_ref
fun add_p_sock_ref sock = p_sock_ref := sock :: !p_sock_ref

fun get_addr_of_ip ip =
    Option.valOf (NetHostDB.fromString ip)
    handle Option => NetHostDB.addr (Option.valOf (NetHostDB.getByName ip))

fun create_server_socket loc (ip, port) =
    let val sock      = INetSock.TCP.socket ()
	val _         = INetSock.TCP.setNODELAY (sock, true)
	val addr      = get_addr_of_ip ip
	val sock_addr = INetSock.toAddr (addr, port)
	val saddr     = addr_to_string sock_addr
	val _         = print_eml_id loc ("created server socket at " ^ saddr)
	val _         = Socket.Ctl.setREUSEADDR (sock, true)
	val _         = Socket.bind (sock, sock_addr)
	val _         = print_eml_id loc "binding"
	val _         = Socket.listen (sock, 100)
	val _         = print_eml_id loc "listening"
	val _         = add_p_sock_ref sock
    in (sock, sock_addr)
    end

fun print_syserror_msg (str, NONE) = "(" ^ str ^ ")"
  | print_syserror_msg (str, SOME syserror) =
    "(type:" ^ str ^ ",msg:" ^ OS.errorMsg syserror ^ ",name:" ^ OS.errorName syserror ^ ")"

val _ = MLton.Signal.setHandler
	    (Posix.Signal.int,
	     MLton.Signal.Handler.simple
		 (fn () =>
		     let val _ = app (fn sock =>
					 let val _ = print_eml "cleaning active socket"
					     val _ = Socket.shutdown (sock, Socket.NO_RECVS_OR_SENDS)
					 in Socket.close sock
					    handle OS.SysErr err =>
						   print_eml ("--ignoring close socket failure:" ^ print_syserror_msg err)
					 end)
				     (!a_sock_ref)
			 val _ = app (fn sock =>
					 let val _ = print_eml "cleaning passive socket"
					     val _ = Socket.shutdown (sock, Socket.NO_RECVS_OR_SENDS)
					 in Socket.close sock
					    handle OS.SysErr err =>
						   print_eml ("--ignoring close socket failure:" ^ print_syserror_msg err)
					 end)
				     (!p_sock_ref)
			 val _ = Posix.Process.exit 0w0
		     in ()
		     end))

(*
fun get_server_info () =
    let val addr   = Option.valOf (NetHostDB.fromString ip_server)
	val server = INetSock.toAddr (addr, port_server)
    in (server)
    end

fun run_client n =
    let val sock   = INetSock.TCP.socket ()
	(* -- server socket -- *)
	val server = get_server_info ()
	(* -- stuff to send -- *)
	val vect  = Word8Vector.fromList [Word8.fromInt n]
	val slice = Word8VectorSlice.full vect
	(*----*)
	val _     = print ("connecting to server\n")
	val _     = Socket.connect (sock, server)
	val _     = print ("connected\n")
	fun loop () =
	    let val _ = print ("sending data\n")
		val _ = Socket.sendVec (sock, slice)
		val _ = print ("data sent\n")
	    in loop ()
	    end
    in loop ()
    end
*)

fun get_id_in_locs (Mac (ip, port)) locations =
    List.find (fn (i,h,p) => ip = h andalso port = p) locations
  | get_id_in_locs (Loc loc) locations =
    List.find (fn (i,h,p) => loc = i) locations

fun get_id_in_groups ident groups =
    get_id_in_locs ident (get_sp_internal_locs_from_groups groups)

fun get_ip_port_in_locs loc locations =
    get_id_in_locs (Loc loc) locations

fun load_program ev gc ident params loc extra =
    case !program of
	SOME (prms, p) =>
	let val args  = getParams prms params
	    val prog1 = NT.mk_nuprl_applies_term p args
	    val id    = NT.mk_mkid_term loc
	    val prog2 = nuprl_app prog1 id
	    val prog3 =
		if is_fold_extra extra orelse not (is_unfold_extra extra)
		then prog2
		else let val _     = print_eml_id loc "unfolding"
			 val lib   = EV.get_lib ()
			 val _     = NT.print_lib_stats lib
			 val prog3 = NT.unfold_all lib prog2
			 val _     = EV.reset_lib ()
			 val _     = if gc then MLton.GC.collect() else ()
			 val _     = print_eml_id loc ("size(after unfolding): " ^ Int.toString (NT.size prog3))
		     in prog3
		     end
	    val prog4 = #1 (ev (~1) prog3)
			   handle EVAL_ERROR str =>
				  raise Fail ("loading_program:evaluation_error("
					      ^ str
					      ^ "):cannot_recover")
	    val _     = print_eml_id loc "component loaded"
	    val _     = print_eml_id loc ("size: " ^ Int.toString (NT.size prog4))
	    (* -- *)
	    val prog5 = NT.partial_ev_opt prog4
	    val size  = Int.toString (NT.size prog5)
	    val _     = print_eml_id loc ("size(after partial evaluation): " ^ size)
	in prog5
	end
      | NONE => raise Fail "[no program]"

(*
fun testing n =
    let val sock  = INetSock.TCP.socket ()
	(*----*)
	val addr   = get_addr_of_ip ip_server
	val server = INetSock.toAddr (addr, port_server)
	(*----*)
	val _     = print ("connecting to server\n")
	val _     = Socket.connect (sock, server)
	val _     = print ("connected\n")
	(*----*)
	val vect  = Word8Vector.fromList [Word8.fromInt n]
	val slice = Word8VectorSlice.full vect
	fun loop () =
	    let val _ = print ("sending data\n")
		val _ = Socket.sendVec (sock, slice)
		val _ = print ("data sent\n")
	    in loop ()
	    end
    in loop ()
    end
*)

fun split_locations_in_group (ip, port) name mem [] = ([], [])
  | split_locations_in_group (ip, port) name mem ((i,h,p) :: lst) =
    let val (less, greater) = split_locations_in_group (ip, port) name mem lst
    in case (String.compare (ip, h), Int.compare (port, p)) of
	   (LESS, _)      => (less, (i,h,p,name,mem) :: greater)
	 | (EQUAL, LESS)  => (less, (i,h,p,name,mem) :: greater)
	 | (EQUAL, EQUAL) => (less, greater)
	 | _              => ((i,h,p,name,mem) :: less, greater)
    end

fun split_locations (ip, port) groups conns =
    case find_group (ip, port) groups of
	SOME (name, mem, locs) =>
	let val (lt1, gt1) = split_locations_in_group (ip, port) name mem locs
	    val lt_groups = find_lt_groups name conns
	    val gt_groups = find_gt_groups name conns
	    val lt2 = get_locs_from_groups (filter_groups groups lt_groups)
	    val gt2 = get_locs_from_groups (filter_groups groups gt_groups)
	in (lt1 @ lt2, gt1 @ gt2)
	end
      | NONE => ([], [])

datatype 'mode sock_kind =
	 KIND_SOCK of (INetSock.inet, 'mode Socket.stream) Socket.sock
       | KIND_FILE of Posix.IO.file_desc

type 'mode sock_nfo = (* mode is Socket.active or Socket.passive *)
     {id    : string option,
      host  : string option,
      port  : int option,
      sock  : 'mode sock_kind,
      desc  : Socket.sock_desc}

fun select_id        (nfo : 'a sock_nfo) = #id   nfo
fun select_sock      (nfo : 'a sock_nfo) = #sock nfo
fun select_desc      (nfo : 'a sock_nfo) = #desc nfo
fun select_host      (nfo : 'a sock_nfo) = #host nfo
fun select_port      (nfo : 'a sock_nfo) = #port nfo
fun select_sock_desc (nfo : 'a sock_nfo) = (#sock nfo, #desc nfo)

fun idNfo2string nfo =
    case select_id nfo of
	SOME id => id
      | NONE => "-"

fun add_socket_desc sock desc (id,host,port) =
    {id   = SOME id,
     host = SOME host,
     port = SOME port,
     sock = KIND_SOCK sock,
     desc = desc}

(* name is name of the group of the location,
 * mem is wether or not the group is part of the system. *)
fun add_socket_nfo sock desc (id,host,port,name,mem) =
    add_socket_desc sock desc (id,host,port)

fun add_file_desc fd =
    {id   = NONE,
     host = NONE,
     port = NONE,
     sock = KIND_FILE fd,
     desc = Socket.sockDesc (MLton.Socket.fdToSock fd)}

fun add_socket sock nfo = add_socket_desc sock (Socket.sockDesc sock) nfo

fun filter_socks id nfo =
    case select_id nfo of
	SOME i => id = i
      | NONE => false

fun filter_socks_op i nfo =
    if filter_socks i nfo
    then SOME (select_sock_desc nfo)
    else NONE

fun sock_nfo_to_string nfo =
    "("   ^ (case select_id   nfo of SOME s => s | NONE => "-")
    ^ "," ^ (case select_host nfo of SOME s => s | NONE => "-")
    ^ "," ^ (case select_port nfo of SOME p => Int.toString p | NONE => "-")
    ^ ")"

fun sockets_to_string sockets =
    ListFormat.fmt {init  = "[",
		    final = "]",
		    sep   = ", ",
		    fmt   = sock_nfo_to_string}
		   sockets

fun get_descs sockets = map select_desc sockets

fun get_sock_from_desc _ [] = NONE
  | get_sock_from_desc d (nfo :: lst) =
    if Socket.sameDesc (d, select_desc nfo)
    then SOME nfo
    else get_sock_from_desc d lst

fun string_to_slice str =
    let (* -- we transfrom the string into a character list -- *)
	val chars = String.explode str
	(* -- we transform the char list -> int list -> word list -- *)
	val words = map (Word8.fromInt o Char.ord) chars
	(* -- we get a vector from the work list -- *)
	val vec   = Word8Vector.fromList words
	(* -- we get a slice from the vector -- *)
	val slice = Word8VectorSlice.full vec
    in slice
    end

fun pack_message str =
    let val slice = string_to_slice str
	val len   = Word8VectorSlice.length slice
	val slen  = string_to_slice (Int.toString len ^ "!")
    in (slen, slice)
    end

fun pack_nuprl_message loc msg =
    (pack_message (NT.spToStringTerm msg))
    handle Fail str =>
	   (print_eml_id loc ("pack_nuprl_message failed (" ^ str ^ ")");
	    raise Fail str)
	 | err =>
	   (print_eml_id loc ("pack_nuprl_message failed with unknown error");
	    raise err)

fun get_socket_info loc (KIND_SOCK sock) =
    let val name = Socket.Ctl.getSockName sock
	    handle exc =>
		   (print_eml_id loc "--cannot get socket name";
		    raise exc)
	val peer = Socket.Ctl.getPeerName sock
	    handle exc =>
		   (print_eml_id loc "--cannot get peer name";
		    raise exc)
	val nstr = addr_to_string name
	val pstr = addr_to_string peer
	val str  = "(iodesc:socket,name:" ^ nstr ^ ",peer:" ^ pstr ^ ")"
	val send = fn slice => Socket.sendVecNB (sock, slice)
	val rcv  = fn n => Socket.recvVec (sock, n)
    in (send, rcv, str)
    end
  | get_socket_info loc (KIND_FILE fd) =
    let val send = fn slice => SOME (Posix.IO.writeVec (fd, slice))
	val rcv  = fn n => Posix.IO.readVec (fd, n)
	val str  = "(iodesc:file)"
    in (send, rcv, str)
    end

fun send_something loc sock desc slice =
    (case #wrs (Socket.select {rds = [], wrs = [desc], exs = [], timeout = NONE}) of
	 [_] =>
	 let val (send,_,_) = get_socket_info loc sock
	     val n   = Word8VectorSlice.length slice
	     (*val _    = print ("[--socket is ready, sending slice of length " ^ Int.toString n ^ "]\n")*)
	     val mop = send slice
	     (*val _    = print ("[--checking that slice has been correctly sent]\n")*)
	 in case mop of
		SOME m =>
		if n = m
		then print_eml_id loc ("--successfully sent " ^ Int.toString m ^ "bytes")
		else print_eml_id loc ("--send_message:bag length(sent " ^ Int.toString m ^ ", should have sent " ^ Int.toString n ^ ")")
	      | NONE => print_eml_id loc ("--send has to wait (blocked)")
	 end
       | lst => (print_eml_id loc ("--send_something:error: " ^ Int.toString (List.length lst) ^ " sockets writable");
		 raise Fail "send_something"))
    handle OS.SysErr err => print_eml_id loc ("--error, cannot send message:" ^ print_syserror_msg err)
	 | _ => print_eml_id loc ("--cannot send message: unknown error")

fun wait_before_sending loc delay =
    let val time = Time.fromSeconds (LargeInt.fromInt delay)
    in if delay > 0
       then let val _ = print_eml_id loc ("sending message in " ^ Int.toString delay ^ "s")
		val _ = Posix.Process.sleep time
	    in ()
	    end
       else ()
    end

fun send_message id sock desc loc delay msg =
    let val _ = print_eml_id id ("sending message to " ^ loc)
	val (slen,slice) = pack_nuprl_message id msg
    in if 0 < delay
       then case Posix.Process.fork () of
		SOME child => (* parent *)
		print_eml_id id "forked, child will send the message"
	      | NONE => (* child *)
		let val _ = wait_before_sending id delay
		    (*val _ = print ("[--sending length]\n")*)
		    val _ = send_something id sock desc slen
		    (*val _ = print ("[--length sent]\n")*)
		    (*val _ = print ("[--sending slice]\n")*)
		    val _ = send_something id sock desc slice
		(*val _ = print ("[--slice sent]\n")*)
		in OS.Process.exit OS.Process.success
		end
       else let val _ = send_something id sock desc slen
		val _ = send_something id sock desc slice
	    in ()
	    end
    end
    handle _ =>
	   print_eml_id id "ignoring external send request because an unknown error occured"

fun send_nuprl_external_message id sockets loc delay msg =
    case List.find (filter_socks loc) sockets of
	SOME nfo =>
	let val (sock,desc) = select_sock_desc nfo
	in send_message id sock desc loc delay msg
	end
      | NONE =>
	(*(print (sockets_to_string sockets); raise Fail "send_message")*)
	print_eml_id id "recipient does not exist anymore, ignoring send request"

fun send_nuprl_internal_message loc outfd delay msg =
    let val _            = print_eml_id loc ("sending internal message (" ^ loc ^ ")")
	val (slen,slice) = pack_nuprl_message loc msg
	val (send,_,_)   = get_socket_info loc (KIND_FILE outfd)
	    handle err => (print_eml_id loc "cannot get socket info"; raise err)
    in if 0 < delay
       then case Posix.Process.fork() of
		SOME child => (* parent *) print_eml_id loc "forked, child will send the message"
	      | NONE =>
		let val _ = wait_before_sending loc delay
		    val _ = send slen
			handle err => (print_eml_id loc ("sending of message size failed (" ^ slice_to_string slen ^ ")");
				       raise err)
		    val _ = send slice
			handle err => (print_eml_id loc ("sending of message failed (" ^ slice_to_string slice ^ ")");
				       raise err)
		in OS.Process.exit OS.Process.success
		end
		handle _ =>
		       (print_eml_id loc "ignoring internal send request because an unknown error occured; terminating handler";
			OS.Process.exit OS.Process.success)
       else let val _ = send slen
		    handle err => (print_eml_id loc ("sending of message size failed (" ^ slice_to_string slen ^ ")");
				   raise err)
		val _ = send slice
		    handle err => (print_eml_id loc ("sending of message failed (" ^ slice_to_string slice ^ ")");
				   raise err)
	    in ()
	    end
    end
    handle OS.SysErr err =>
	   let val s = print_syserror_msg err
	   in print_eml_id loc ("sending of internal message failed with: " ^ s ^ ", ignoring error")
	   end
	 | _ => print_eml_id loc "ignoring internal send request because an unknown error occured"

fun send_nuprl_messages' loc outfd sockets [] = []
  | send_nuprl_messages' loc outfd sockets (term :: terms) =
    let val (delay,id,msg) = IM.dest_message (IM.term2message term)
    in if loc = id
       then let val _ = send_nuprl_internal_message loc outfd delay msg
	    in send_nuprl_messages' loc outfd sockets terms
	    end
       else let val _ = send_nuprl_external_message loc sockets id delay msg
	    in send_nuprl_messages' loc outfd sockets terms
	    end
    end

fun send_nuprl_messages loc outfd sockets [] =
    print_eml_id loc ("no message to send from " ^ loc)
  | send_nuprl_messages loc outfd sockets lstmsgs =
    let val _ = print_eml_id loc ("sending messages from " ^ loc)
	val _ = print_message_list 1 lstmsgs
	val _ = send_nuprl_messages' loc outfd sockets lstmsgs
	val _ = print_eml_id loc "messages sent"
    in ()
    end

fun send_nuprl_messages_to_itself loc outfd delay [] =
    print_eml_id loc ("sent internal messages (" ^ loc ^ ")")
  | send_nuprl_messages_to_itself loc outfd delay (msg :: msgs) =
    (send_nuprl_internal_message loc outfd delay msg;
     send_nuprl_messages_to_itself loc outfd delay msgs)

exception RCV_INT of Socket.sock_desc option
exception RCV_SYS of Socket.sock_desc option * (string * OS.syserror option)
exception SEL_SYS of string * OS.syserror option

fun receive_integer_gen loc n pref debug_str descop rcv =
    let fun loop b lst =
	    let val vec  = rcv 1
		val len  = Word8Vector.length vec
		val nstr = Int.toString n
	    in if b andalso len = 0
	       then (print_eml_id loc ("--error(@" ^ loc ^ "), received empty vector(" ^ nstr ^ ")");
		     raise RCV_INT descop)
	       else if len = 1
	       then let val word = Word8Vector.sub (vec, 0)
			val char = word_to_char word
		    in if char = #"!"
		       then lst
		       else loop false (lst @ [char])
		    end
	       else raise Fail ("receive_integer(" ^ nstr ^ ")"
				^ ":bad_length"
				^ ":expecting_message_length_1_received_" ^ Int.toString len
				^ ":received_so_far_" ^ String.implode lst
				^ ":" ^ debug_str
				^ "\n")
	    end
	val chars = loop true []
	val str   = pref ^ String.implode chars
	val _     = print_eml_id loc ("--received integer " ^ str ^ " (" ^ debug_str ^ ")")
    in Int.fromString str
    end handle OS.SysErr err =>
	       let val s = print_syserror_msg err
		   val _ = print_eml_id loc ("--error, cannot receive integer:" ^ s ^ ", trying to recover")
	       in raise RCV_SYS (descop, err)
	       end

fun receive_integer loc rcv debug_str sock desc =
    receive_integer_gen
	loc
	1
	""
	debug_str
	(SOME desc)
	rcv

fun clean_sockets loc NONE sockets = sockets
  | clean_sockets loc (SOME desc) [] = []
  | clean_sockets loc (SOME desc) (entry :: sockets) =
    let val (sock,desc') = select_sock_desc entry
	val id = idNfo2string entry
    in if Socket.sameDesc (desc', desc)
       then let val _ = print_eml_id loc ("--removing " ^ id ^ " from socket list")
	    in clean_sockets loc (SOME desc) sockets
	    end
       else entry :: clean_sockets loc (SOME desc) sockets
    end

fun try_clean_sockets loc [] = (false,[])
  | try_clean_sockets loc (entry :: socks) =
    let val (sock,desc) = select_sock_desc entry
	val id = idNfo2string entry
    in let val timeout    = SOME (Time.fromMilliseconds 1)
	   val _          = get_socket_info loc sock
	   val _          = Socket.select {rds = [desc], wrs = [], exs = [], timeout = timeout}
	   val (b,socks') = try_clean_sockets loc socks
       in (b, entry :: socks')
       end handle _ => (print_eml_id loc ("--socket for "
					  ^ id
					  ^ " seems dead "
					  ^ sock_nfo_to_string entry
					  ^ ", removing it from list");
			(true, socks))
    end

fun receive_one_message sockets loc desc =
    case get_sock_from_desc desc sockets of
	SOME nfo =>
	let val (sock,desc)   = select_sock_desc nfo
	    val (_,rcv,debug) = get_socket_info loc sock
	in case receive_integer loc rcv debug sock desc of
	       NONE => raise Fail "receive_one_message:not_an_int"
	     | SOME n =>
	       let fun aux n s =
		       if n = 0
		       then s
		       else if n < 0
		       then raise Fail ("receive_one_message_bad_length(negative)")
		       else let val vec = rcv n
				val len = Word8Vector.length vec
				val _ = print_eml_id loc ("--received " ^ Int.toString len ^ " out of " ^ Int.toString n)
				val str = vector_to_string vec
			    in aux (n - len) (s ^ str)
			    end
		   val _ = print_eml_id loc ("--ready to receive message of length " ^ Int.toString n)
	       in aux n ""
	       end
	       handle OS.SysErr err =>
		      let val s = print_syserror_msg err
			  val _ = print_eml_id loc ("--error while receiving vector:" ^ s ^ ", trying to recover")
		      in raise RCV_SYS (SOME desc, err)
		      end
	end
      | NONE => raise Fail "receive_one_message:no_corresponding_socket"

fun receive_messages loc [] =
    let val str = "no more sockets to read from"
    in (print_eml_id loc str; raise Fail str)
    end
  | receive_messages loc sockets =
    (case #rds (Socket.select {rds = get_descs sockets, wrs = [], exs = [], timeout = NONE}) of
	 [] => raise Fail "receive_message:select:nil"
       | descs =>
	 let val _ = print_eml_id loc ("--ready to receive from " ^ Int.toString (length descs) ^ " sockets")
	     val str = String.concat (map (receive_one_message sockets loc) descs)
	 in (str, sockets)
	 end)
    handle OS.SysErr err =>
	   let val s = print_syserror_msg err
	       val _ = print_eml_id loc ("--error, cannot receive message (sockets):" ^ s)
	       val _ = print_eml_id loc ("--trying to find dead sockets")
	       val (b,sockets') = try_clean_sockets loc sockets
	   in if b
	      then (print_eml_id loc ("--retrying receive with new socket list");
		    receive_messages loc sockets')
	      else (print_eml_id loc ("--system error, but sockets seem fine, failing");
		    raise OS.SysErr err)
	   end
	 | RCV_INT descop =>
	   let val _        = print_eml_id loc ("--cleaning socket list")
	       val sockets' = clean_sockets loc descop sockets
	       val _        = print_eml_id loc ("--retrying receive with new socket list")
	   in receive_messages loc sockets'
	   end
	 | RCV_SYS (descop, err) =>
	   let val _        = print_eml_id loc ("--cleaning socket list")
	       val sockets' = clean_sockets loc descop sockets
	       val _        = print_eml_id loc ("--retrying receive with new socket list")
	   in receive_messages loc sockets'
	   end
	 | exc =>
	   (print_eml_id loc ("--unknow error while receiving message from sockets");
	    raise exc)

(*
(* How can we read from either sockets of infd? *)
and receive_message loc sockets =
    (case infd_op of
	SOME infd =>
	let (*val _   = print ("[receive_message: trying to receive message from internal connection]\n");*)
	    fun rcv n = Posix.IO.readVec (infd, n)
	    val vec = rcv 1
	    val str = vector_to_string vec
     	in case receive_integer_gen loc 2 str "" NONE rcv of
	       SOME n =>
	       let val vec = rcv n
		   val len = Word8Vector.length vec
	       in if len = n
		  then (vector_to_string vec, true, sockets)
		  else raise Fail "receive_message:bad_length"
	       end
	     | NONE => raise Fail "receive_message:fd"
	end
      | NONE => (print_eml ("receive_message: no internal connection");
		 raise NOT_FD))
    handle Fail str => (print_eml ("receive_message: Fail error occured(" ^ str ^ ")");
			raise Fail str)
	 | _ => ((*print ("[receive_message: trying to receive message from sockets]\n");*)
		 receive_message_sockets loc infd_op sockets)
*)

fun receive_message' loc sockets =
    let val (msgs,_) = receive_messages loc sockets
    in msgs
    end

(* connect to server using socket.
 * It differs from connect by the fact that if it fails to connect,
 * it creates a new socket. *)
fun connect' loc socket desc id server timer =
    if getSecondsTime timer > 40
    then (print_eml_id
	      loc
	      ("--aborting connection to "
	       ^ id
	       ^ ", already tried for 1 minute"); NONE)
    else let val s = addr_to_string server
	     val _ = print_eml_id loc ("--n-connecting to " ^ s ^ "(" ^ id ^ ")")
	     val _ = Socket.connect (socket, server)
	 in SOME (socket,desc)
	 end handle OS.SysErr err =>
		    let val retry_limit = 2
			val st   = IntInf.toString retry_limit
			val _    = print_eml_id loc ("--n-cannot connect" ^ print_syserror_msg err ^ ", will retry in " ^ st ^ "s")
			val _    = Posix.Process.sleep (Time.fromSeconds retry_limit)
			val _    = print_eml_id loc ("--n-ready to retry connect, creating a new socket")
			val sock = INetSock.TCP.socket ()
			val _    = INetSock.TCP.setNODELAY (sock, true)
			val desc = Socket.sockDesc sock
		    in connect' loc sock desc id server timer
		    end
		  | exc =>
		    (print_eml_id loc ("--n-cannot connect, unknown error");
		     raise exc)

(* connect to server using socket *)
fun connect loc socket desc id server timer =
    if getSecondsTime timer > 40
    then (print_eml_id
	      loc
	      ("--aborting connection to "
	       ^ id
	       ^ ", already tried for 1 minute"); NONE)
    else let val s = addr_to_string server
	     val _ = print_eml_id loc ("--connecting to " ^ s)
	     val _ = Socket.connect (socket, server)
	 in SOME (socket,desc)
	 end handle OS.SysErr err =>
		    let val retry_limit = 1
			val st = IntInf.toString retry_limit
			val _  = print_eml_id loc ("--cannot connect" ^ print_syserror_msg err ^ ", will retry in " ^ st ^ "s")
			val _  = Posix.Process.sleep (Time.fromSeconds retry_limit)
			val _  = print_eml_id loc ("--ready to retry")
		    in connect loc socket desc id server timer
		    end
		  | exc =>
		    (print_eml_id loc ("--cannot connect, unknown error");
		     raise exc)

fun send_port_number loc (SOME port) sock desc =
    let val pstr   = Int.toString port
	val pslice = string_to_slice (pstr ^ "!")
	val _      = print_eml_id loc ("--sending port number " ^ pstr)
	val _      = send_something loc (KIND_SOCK sock) desc pslice
    in ()
    end
  | send_port_number loc NONE sock desc = ()

fun connect_to loc port nsock [] =
    let val _ = print_eml_id loc "--connect to done"
    in []
    end
  | connect_to loc port nsock ((nfo as (i,h,p,name,mem)) :: lst) =
    let val n      = Int.toString (List.length lst + 1)
	val _      = print_eml_id loc ("--still " ^ n ^ " machine(s) to connect to")
	(* -- generates sock (a new socket) and desc (description of the socket) *)
	val sock   = INetSock.TCP.socket ()
	val _      = INetSock.TCP.setNODELAY (sock, true)
	val desc   = Socket.sockDesc sock
	(* -- server to which we have to connect -- *)
	val addr   = get_addr_of_ip h
	val server = INetSock.toAddr (addr, p)
	(* -- tries to connect to server using socket sock -- *)
	val timer  = startTimer ()
	val sdop   =
	    if nsock
	    then connect' loc sock desc i server timer
	    else connect  loc sock desc i server timer
    in case sdop of
	   SOME (sock,desc) =>
	   let (* -- sends port number to server -- *)
	       val _      = send_port_number loc port sock desc
	   in (add_socket_nfo sock desc nfo) :: (connect_to loc port nsock lst)
	   end
	 | NONE => connect_to loc port nsock lst
    end

fun print_location_list lst =
    ListFormat.fmt {init  = "[",
		    final = "]",
		    sep   = ", ",
		    fmt   = fn (i,h,p,name,mem) => i}
		   lst

fun remove_address_from_list addr port lst =
    let val (ip,_) = INetSock.fromAddr addr
    in List.partition
	   (fn (i,h,p,name,mem) => h = NetHostDB.toString ip andalso p = port)
	   lst
    end

fun dest_pair_debug n pair =
    (NT.dest_pair n pair)
    handle exc =>
	   (print (NT.toStringTerm pair ^ "\n"); raise exc)

fun parse_message_debug n string prt =
    let val str   = string
	val light = true
    in PN.parseString prt light [] string
    end handle exn =>
	       (print ("\n-EXC("
		       ^ Int.toString n
		       ^ ")----------------------\n"
		       ^ string
		       ^ "\n--------------------------\n");
		raise exn)

fun mk_client_sock_info sock addr =
    let val (host,port) = INetSock.fromAddr addr
	val shost       = NetHostDB.toString host
    in add_socket sock ("client",shost,port)
    end

fun print_messages msgs =
    app (fn msg =>
	    ((*print (NT.toStringTerm msg ^ "msg" ^ "\n");*)
	     print (NT.nuprlTerm2eml msg ^ "\n")))
	msgs

fun send_dummy_info loc socket =
    let val desc  = Socket.sockDesc socket
	val slice = string_to_slice ""
	val _     = print_eml_id loc "sending dummy information"
    in send_something loc (KIND_SOCK socket) desc slice
    end

fun handle_new_connection loc gc outfd sock addr =
    let val saddr = addr_to_string addr
	val _     = print_eml_id loc ("--received new connection request from " ^ saddr)
	val nfo   = mk_client_sock_info sock addr
	fun loop () =
	    let val msgstr = receive_message' loc [nfo]
		    (* we might want to do that in receive_integer_gen *)
		    (*handle exc => (send_dummy_info sock; raise exc)*)
		val msgs   = parse_message_debug 1 msgstr false
		val _      = print_eml_id loc ("--received input messages from " ^ saddr)
		val _      = print_messages msgs
		(*val _      = print "[--closing socket]\n"
		val _      = Socket.close sock*)
		val _      = print_eml_id loc "--sending message to myself"
		val delay  = 0 (* no delay *)
		val _      = send_nuprl_messages_to_itself loc outfd 0 msgs
	    in loop ()
	    end
    in loop ()
    end handle _ =>
	       let val _ = print_eml_id loc "connection handler died"
	       in OS.Process.exit OS.Process.success
	       end

fun keep_listening_for_inputs_aux loc gc outfd server =
    let val (sock,addr) = Socket.accept server
    in case Posix.Process.fork () of
		SOME child => keep_listening_for_inputs_aux loc gc outfd server
	      | NONE => handle_new_connection loc gc outfd sock addr
    end handle _ =>
	       let val _ = print_eml_id loc "listner died"
	       in OS.Process.exit OS.Process.success
	       end

fun keep_listening_for_inputs loc gc outfd server =
    let val _ = reset_all ()
	val _ = if gc then MLton.GC.collect() else ()
    in keep_listening_for_inputs_aux loc gc outfd server
    end

fun split_in_and_out_locs [] = ([],[])
  | split_in_and_out_locs ((x as (id,host,port,name,mem)) :: lst) =
    let val (inlocs,outlocs) = split_in_and_out_locs lst
    in if mem
       then (x :: inlocs, outlocs)
       else (inlocs, x :: outlocs)
    end

fun waiting_on_connection loc socket timer accept =
    if getSecondsTime timer > 60
    then NONE
    else case accept socket of
	     SOME (sock,addr) =>
	     let val _             = add_a_sock_ref sock
		 val desc          = Socket.sockDesc sock
		 val _             = print_eml_id loc ("--received connection request from " ^ addr_to_string addr)
		 val (_,rcv,debug) = get_socket_info loc (KIND_SOCK sock)
		 val client_port = (* wait to receive port number *)
		     case receive_integer loc rcv debug sock desc of
			 SOME p => p
		       | NONE => raise Fail "waiting_on_connection:received_non_int_data"
		 val _           = print_eml_id loc ("--received port number " ^ Int.toString client_port)
	     in SOME (sock,desc,addr,client_port)
	     end
	   | NONE =>
	     let val retry_limit = 2
		 val st   = IntInf.toString retry_limit
		 val _    = print_eml_id loc ("--no pending connection request, will retry in " ^ st ^ "s")
		 val _    = Posix.Process.sleep (Time.fromSeconds retry_limit)
		 val _    = print_eml_id loc ("--ready to retry accept")
	     in waiting_on_connection loc socket timer accept
	     end

fun wait_for_connections' loc server [] accept =
    let val _ = print_eml_id loc "--waiting for connections done"
    in []
    end
  | wait_for_connections' loc server lst accept =
    let val n                     = Int.toString (List.length lst)
	val _                     = print_eml_id loc ("--waiting on " ^ n ^ " connection(s)")
	val timer                 = startTimer ()
    in case waiting_on_connection loc server timer accept of
	   SOME (sock,desc,addr,port) =>
	   let val (connected,lst') = remove_address_from_list addr port lst
	       val sockets          = map (add_socket_nfo sock desc) connected
	   in sockets @ (wait_for_connections' loc server lst' accept)
	   end
	 | NONE =>
	   let val (inlocs,outlocs) = split_in_and_out_locs lst
	       val _ =
		   print_eml_id
		       loc
		       ("--waiting on connection timed out (1 minute)"
			^ ", did not connect to:"
			^ print_location_list inlocs
			^ ", still waiting for external connections")
	       fun accept sock = SOME (Socket.accept sock)
	   in wait_for_connections' loc server outlocs accept
	   end
    end

fun wait_for_connections locs server locations =
    wait_for_connections' locs server locations Socket.acceptNB

fun boot_up_prog ev gc ident spec lib alldefs loc params extra =
    let (* -- load library and generates program -- *)
	val _     = print_eml_id loc ("type checking and loading library")
	(* -- *)
	val poly  = true
	val btyp  = true
	val prt   = default_prt
	val sprt  = (true, true)
	val sub   = default_sub
	val obid  = default_obid
	val time  = default_time
	val outop = NONE
	(* -- *)
	(* -- Generate program from specification -- *)
	(*val terms = toNuprl spec NONE lib alldefs obid time sub prt (true, true) poly btyp extra*)
	val _     = load_program_from_file spec outop lib alldefs obid time sub prt sprt poly btyp extra loc
	(* -- Load component -- *)
	val _     = print_eml_id loc "loading component"
	val prog  = load_program ev gc ident params loc extra
    in prog
    end

fun boot_up_conn loc nsock ip port groups conns =
    let (* -- Split the locations into those with lower ip/port and those with greater -- *)
	val (lt, gt) = split_locations (ip, port) groups conns
	val lt_size  = Int.toString (List.length lt)
	val gt_size  = Int.toString (List.length gt)
	val _        = print_eml_id loc (lt_size ^ " lower machine(s), " ^ gt_size ^ " higher machine(s)")
	(* -- create a server socket -- *)
	val _             = print_eml_id loc "starting server socket"
	val (server,addr) = create_server_socket loc (ip, port)
	(* -- connect to machines with higher ip/port -- *)
	val _       = print_eml_id loc "connecting to higher machines"
	val lst_gt  = connect_to loc (SOME port) nsock gt
	(* -- waits for connections from machines with lower ip/port -- *)
	val _       = print_eml_id loc "waiting on connections from lower machines"
	val lst_lt  = wait_for_connections loc server lt
	(*(* -- waits for connections from other machines -- *)
	val _       = print ("[connecting to other machines]\n")
	val lst_ot  = wait_for_connections server locs'*)
    in (server, lst_lt @ lst_gt (*@ lst_ot*))
    end

fun boot_up ev gc ident config spec lib alldefs extra =
    let val _     = set_light_terms ()
	(* ------ reading configuration file ------ *)
	val idstr = ident_to_string ident
	val _     = print_eml_id idstr "reading configuration file"
	val (groups,conns,params,msgs) = load_config_parse config
	val (loc,ip,port) =
	    case get_id_in_groups ident groups (* extract own location *) of
		SOME (id,host,port) => (id,host,port)
	      | NONE => raise Fail "[unknown location]\n"
	(* -- *)
	(* ------ generating connections ------ *)
	val _     = print_eml_id loc "connecting"
	val nsock = is_nsock_extra extra
	val (server,sockets) = boot_up_conn loc nsock ip port groups conns
	val {infd,outfd} = Posix.IO.pipe ()
	(*val _     = Posix.IO.setfl (infd, Posix.IO.O.nonblock)*)
	val _     =
	    case Posix.Process.fork () of
		SOME child => (* parent *)
		let val word = Posix.Process.pidToWord child
		in print_eml_id loc ("forking listner: " ^ SysWord.toString word)
		end
	      (* The parent keep on doing the rest of the program while the child
	       * listens for new connections. *)
	      | NONE => keep_listening_for_inputs loc gc outfd server (* child *)
	(* -- *)
	(* ------ loading program ------ *)
	val _     = print_eml_id loc "loading program"
	val prog  = boot_up_prog ev gc ident spec lib alldefs loc params extra
    in (loc, prog, server, (add_file_desc infd) :: sockets)
    end

(*
fun get_internal_ready_message [] = NONE
  | get_internal_ready_message ((delay,timer,msg) :: msgs) =

*)

fun get_non_halted_prog extra prog =
    if NT.is_nuprl_inl_term prog
    then SOME (NT.dest_inl prog)
    else if NT.is_nuprl_inr_term prog
    then NONE
    else (dump_prog [] prog NT.toStringTerm;
	  raise Fail ("run_on_messages:newprog:not_inl_or_inr("
		      ^ NT.opid_of_term prog
		      ^ ")"))

fun run_on_messages extra loc outfd ev progop sockets [] = progop
  | run_on_messages extra loc outfd ev NONE sockets _ = NONE
  | run_on_messages extra loc outfd ev (SOME prog) sockets (msg :: msgs) =
    case get_non_halted_prog extra prog of
	NONE => NONE
      | SOME prog =>
	let (*val _             = print ("[size: " ^ Int.toString (NT.size prog) ^ "]\n")*)
	    (*val _             = print ("[size: " ^ Int.toString (NT.size prog) ^ "," ^ Int.toString (NT.env_size prog) ^ "]\n")*)
	    (*val _             = dump_prog_stream prog ("output-term-" ^ loc)*)
	    val _             = print_eml_id loc "received message"
	    val _             = print (NT.nuprlTerm2eml msg ^ "\n")
	    (*val _             = print (NT.toStringTerm msg ^ "\n")*)
	    val toeval        = nuprl_all (nuprl_app prog msg)
	    val (prog1,steps) = ev (~1) toeval
	    val _             = print_eml_id loc (Int.toString steps ^ " steps")
	    val (prog2,out)   = NT.dest_pair 7 prog1
	    val lstmsgs       = NT.dest_list out
	    val _             = send_nuprl_messages loc outfd sockets lstmsgs
	in run_on_messages extra loc outfd ev (SOME prog2) sockets msgs
	end
	handle EVAL_ERROR str =>
	       (print_eml_id loc ("evaluation failed ("
				  ^ str
				  ^ "), rolling back to input term");
		run_on_messages extra loc outfd ev (SOME prog) sockets msgs)

fun run_distributed_program2 ev gc ident config spec lib alldefs prt extra =
    let val idstr = ident_to_string ident
	val _ = print_eml_id idstr "booting up"
	val (loc,process,server,sockets) =
	    boot_up ev gc ident config spec lib alldefs extra
	val {infd,outfd} = Posix.IO.pipe ()
	val _ = print_eml_id loc "running process"
	fun loop sockets (SOME prog) (*n*) =
	    let val _ = if gc then MLton.GC.collect() else ()
		val _ = print_eml_id loc "waiting for a new message"
		val (msgs_str,sockets') = receive_messages loc sockets
		val msgs = parse_message_debug 3 msgs_str prt
		val prog' = run_on_messages extra loc outfd ev (SOME prog) sockets' msgs
	    in (*if n > k
		 then print ("[DONE]\n")
		 else*) loop sockets' prog' (*(n+1)*)
	    end
	  | loop sockets NONE = print_eml_id loc "program finished"
    in loop ((add_file_desc infd) :: sockets) (SOME process) (*0*)
    end

fun run_distributed_program extra ev gc ident config spec lib alldefs prt =
    let val idstr = ident_to_string ident
    in (run_distributed_program2 ev gc ident config spec lib alldefs prt extra;
	print_eml_id idstr "process terminated without error")
    end

(*
fun monitor_process socket =
    let val _       = send_dummy_info socket
	val desc    = Socket.sockDesc socket
	val timeout = Time.fromMilliseconds 1000
    in case #rds (Socket.select {rds = [desc], wrs = [], exs = [], timeout = SOME timeout}) of
	   [_] =>
	 | _   =>
    end
*)

fun send_intransit_messages locs [] = ()
  | send_intransit_messages locs (message :: msgs) =
    let val (delay,id,msg) = IM.dest_message message
    in case get_ip_port_in_locs id locs of
	   SOME (nfo as (id,host,port)) =>
	   let val sock    = INetSock.TCP.socket ()
	       val _       = INetSock.TCP.setNODELAY (sock, true)
	       val addr    = get_addr_of_ip host
	       val server  = INetSock.toAddr (addr, port)
	       val s       = addr_to_string server
	       val _       = print_eml ("connecting to " ^ s)
	       val _       = Socket.connect (sock, server)
	       val sockets = [add_socket sock nfo]
	       val _       = print_eml ("sending message to " ^ s)
	       val _       = send_nuprl_external_message "intransit" sockets id delay msg
	       val _       = print_eml "message sent"
	       val _       = print_eml "closing socket"
	       val _       = Socket.close sock
	       val _       = print_eml "socket closed"
	   in send_intransit_messages locs msgs
	   end
	 | NONE =>
	   let val _ = print_eml "unknown location"
	   in send_intransit_messages locs msgs
	   end
    end

fun monitor_process server locs msgs =
    (* It would be good to leave the socket open *)
    let val sock = INetSock.TCP.socket ()
	val _    = INetSock.TCP.setNODELAY (sock, true)
	val _    = Socket.connect (sock, server)
	val _    = send_dummy_info "monitor" sock
	val time = Time.fromSeconds 2
	val _    = Posix.Process.sleep time
    in monitor_process server locs msgs
    end handle _ =>
	       (print_eml "Crash suspected, sending swap messages";
		send_intransit_messages locs msgs)

fun monitor_processes gc config alldefop prt =
    let val (groups,conns,params,msgs) = load_config_parse config
	val locs  = get_sp_locs_from_groups groups
	val _     = print_eml "generating Nuprl library"
	val light = false
	val split = false
	val lib   =
	    case alldefop of
		SOME f => NT.terms2map 2 (PN.parse prt light NT.to_filter_out f split)
	      | NONE => NT.emlib ()
	val _     = print_eml "Preparing messages"
	val reps  = get_group groups "replicas"
	val cstr  = Int.toString 1000
	val msgs1 = map (fn (id,_,_) => id ^ " : (``swap``, (Int * Tok List), (" ^ cstr ^ ",``paxos``))") reps
	val msgs2 = List.concat (map load_config_parse_str msgs1)
	val msgs3 = unfold_intransit_messages lib msgs2
	val _     = EV.reset_lib ()
	val _     = if gc then MLton.GC.collect () else ()
	val _     = print_eml "Launching monitors"
    in List.app
	   (fn (id,host,port) =>
	       let val addr   = get_addr_of_ip host
		   val server = INetSock.toAddr (addr, port)
	       in case Posix.Process.fork () of
		      SOME child => ()
		    | NONE => monitor_process server locs msgs3
	       end)
	   (get_sp_internal_locs_from_groups groups)
    end


(* This is just for testing purposes, this is never going to be used. *)
(*fun simulate_clients_server config =
    let val (locations, locations', params, messages) = load_config_parse config
	fun listen_and_print sock =
	    let val sockets = wait_for_connections sock locations
		val descs   = map select_desc sockets
		fun loop () =
		    let val msgstr = receive_message NONE sockets descs
		    in case parse_message_debug 6 msgstr false of
			   [msg] =>
			   let val _ =  print ("\n-------------\n"
					       ^ "received: "
					       ^ NT.nuprlTerm2eml msg
					       ^ "\n-------------\n")
			   in loop ()
			   end
			 | _ => raise Fail "simulate_clients:listen_and_print"
		    end
	    in loop ()
	    end
	fun start_listening_sockets [] = []
	  | start_listening_sockets ((id, thost, tport) :: lst) =
	    let val h = NT.dest_ihost thost
		val p = NT.dest_iport tport
		val (sock, addr) = create_server_socket (h,p)
		val _ =
		    case Posix.Process.fork () of
			SOME pid => () (* parent *)
		      | NONE => listen_and_print sock (* child *)
	    in start_listening_sockets lst
	    end
	val _ = print "[waiting on connections]\n"
	val _ = start_listening_sockets locations'
    in ()
    end*)

fun simulate_clients_server config prt =
    let val (groups,_,_,_) = load_config_parse config
	val _       = print_eml "--connecting to other machines"
	val sockets = connect_to "clients" NONE false (get_external_locs_from_groups groups)
	val _       = print_eml "--ready to receive messages from other machines"
	fun loop sockets =
	    let val (msgs_str,sockets') = receive_messages "client" sockets
		val msgs = parse_message_debug 6 msgs_str prt
		val _ = print ("\n-------------\n"
			       ^ "received: "
			       ^ (ListFormat.fmt {init  = "",
						  final = "",
						  sep   = "\n--\n",
						  fmt   = NT.nuprlTerm2eml}
						 msgs)
			       ^ "\n-------------\n")
	    in loop sockets'
	    end
    in loop sockets
    end

fun simulate_clients_send config alldefop prt gc =
    let	val lib =
	    case alldefop of
		SOME f =>
		let val _     = print_eml "parsing Nuprl library"
		    val light = false
		    val split = false
		    val terms = PN.parse prt light NT.to_filter_out f split
		    val _     = print_eml "generating EML library"
		    val lib   = NT.terms2map 3 terms
		    val _     = NT.print_lib_stats lib
		in lib
		end
	      | NONE => NT.emlib ()
	val strm = TextIO.stdIn
	val cid  = ref 1
	fun aux msgop =
	    let val (groups,_,_,msgs) = load_config_parse config
		val locs = get_sp_internal_locs_from_groups groups
		val msgs =
		    case msgop of
			NONE => msgs
		      | SOME msg => load_config_parse_str msg
		val time = Time.toMilliseconds (Time.now ())
		val _    = print_eml ("unfolding messages(" ^ LargeInt.toString time ^ "ms)")
		val msgs = unfold_intransit_messages lib msgs
		val _    = if gc then MLton.GC.collect () else ()
		val time = Time.toMilliseconds (Time.now ())
		val _    = print_eml ("sending intransit messages(" ^ LargeInt.toString time ^ "ms)")
		val _    = send_intransit_messages locs msgs
		val _    = print "send? "
	    in case TextIO.inputLine strm of
		   NONE       => aux NONE
		 | SOME ""    => aux NONE
		 | SOME "\n"  => aux NONE
		 | SOME "sp\n" =>
		   let val c = Int.toString (!cid)
		       val _ = cid := !cid + 1
		   in aux (SOME ("rep1 : (``swap``, (Int * Tok List), (" ^ c ^ ",``paxos``))"))
		   end
		 | SOME "st\n" =>
		   let val c = Int.toString (!cid)
		       val _ = cid := !cid + 1
		   in aux (SOME ("rep1 : (``swap``, (Int * Tok List), (" ^ c ^ ",``2/3``))"))
		   end
		 | SOME "c\n" =>
		   let val c = Int.toString (!cid)
		       val _ = cid := !cid + 1
		   in aux (SOME ("rep1 : (``bcast``, (Int * Tok List), (" ^ c ^ ",``" ^ c ^ "``))"))
		   end
		 | SOME "p\n" =>
		   let val c = Int.toString (!cid)
		       val _ = cid := !cid + 1
		   in aux (SOME ("ldr1 : (``propose``, (Int * Tok List), (" ^ c ^ ",``" ^ c ^ "``))"))
		   end
		 | SOME msg   => aux (SOME msg)
	    end
	val _ = print "send? "
    in case TextIO.inputLine strm of
	   NONE       => aux NONE
	 | SOME ""    => aux NONE
	 | SOME "\n"  => aux NONE
	 | SOME "sp\n" =>
	   let val c = Int.toString (!cid)
	       val _ = cid := !cid + 1
	   in aux (SOME ("rep1 : (``swap``, (Int * Tok List), (" ^ c ^ ",``paxos``))"))
	   end
	 | SOME "st\n" =>
	   let val c = Int.toString (!cid)
	       val _ = cid := !cid + 1
	   in aux (SOME ("rep1 : (``swap``, (Int * Tok List), (" ^ c ^ ",``2/3``))"))
	   end
	 | SOME "c\n" =>
	   let val c = Int.toString (!cid)
	       val _ = cid := !cid + 1
	   in aux (SOME ("rep1 : (``bcast``, (Int * Tok List), (" ^ c ^ ",``" ^ c ^ "``))"))
	   end
	 | SOME "p\n" =>
	   let val c = Int.toString (!cid)
	       val _ = cid := !cid + 1
	   in aux (SOME ("ldr1 : (``propose``, (Int * Tok List), (" ^ c ^ ",``" ^ c ^ "``))"))
	   end
	 | SOME msg   => aux (SOME msg)
    end

fun run_other_machine extra ident config prt =
    let val (groups,_,_,_) = load_config_parse config
	val locs  = get_internal_locs_from_groups groups
	val locs' = get_sp_external_locs_from_groups groups
	(* -- checks whethere ip/port is in the configuration file -- *)
	val (loc,host,port) =
	    case get_id_in_locs ident locs' of
		SOME (id,host,port) => (id,host,port)
	      | NONE => raise Fail ("unknown location" ^ ident_to_string ident ^ "\n")
	(* -- connects to the other machines -- *)
	val _              = print_eml_id loc "connecting to machines"
	val sockets        = connect_to loc (SOME port) (is_nsock_extra extra) locs
	(* -- waits on a connection request from some client -- *)
	val _              = print_eml_id loc "starting server socket"
	val (sock,addr)    = create_server_socket loc (host, port)
	val (client,caddr) = Socket.accept sock
	val scaddr         = addr_to_string caddr
	val _              = print_eml_id loc ("--received connection request from " ^ scaddr)
	val cdesc          = Socket.sockDesc client
	val _              = print_eml_id loc ("--ready to receive messages and forward to " ^ scaddr)
	(* -- forwards to client messages received from internal locations --  *)
	fun internal sockets =
	    let val (msgs_str,sockets') = receive_messages loc sockets
		val time = Time.toMilliseconds (Time.now ())
		val _    = print_eml_id loc ("received message at " ^ LargeInt.toString time ^ "ms")
		val msgs = parse_message_debug 7 msgs_str prt
		val _    = print ("\n-------------\nreceived:\n"
				  ^ (ListFormat.fmt {init  = "",
						     final = "",
						     sep   = "\n--\n",
						     fmt   = NT.nuprlTerm2eml}
						    msgs)
				  ^ "\n-------------\n")
		val delay = 0 (* just forwarding, no delay *)
		val _ = List.app (send_message loc (KIND_SOCK client) cdesc "client" delay) msgs
	    in internal sockets'
	    end
    in internal sockets
    end

exception DONE

fun start_all_machines ev gc config spec lib alldefs prt extra =
    let val (groups,_,_,_) = load_config_parse config
	val locs  = get_sp_internal_locs_from_groups groups
	val locs' = get_sp_external_locs_from_groups groups
	val _ =
	    List.app (fn (id,host,port) =>
			 case Posix.Process.fork () of
			     SOME child => ()
			   | NONE =>
			     let val _ = run_other_machine extra (Loc id) config prt
			     in raise DONE
			     end)
		     locs'
	val _ =
	    List.app (fn (id,host,port) =>
			 case Posix.Process.fork () of
			     SOME child => ()
			   | NONE =>
			     let val _ = run_distributed_program extra ev gc (Loc id) config spec lib alldefs prt
			     in raise DONE
			     end)
		     locs
    in ()
    end handle DONE => ()

(* ------ TESTING ------ *)

fun testing n =
    let val str = "{make-Msg:OPID}\n({cons:OPID}\n ({token:OPID,propose:t}();\n  {nil:OPID}());\n {product:OPID}\n ({int:OPID}();\n  {bound_id:OPID,:v}\n  ({int:OPID}()));\n {pair:OPID}\n ({natural_number:OPID,12:n}();\n  {natural_number:OPID,4:n}()))"
	val _ = parse_message_debug 8 str true
    in ()
    end

fun testing n =
    let val str = "{make-Msg:OPID}\n({cons:OPID}\n ({token:OPID,propose:t}();\n  {nil:OPID}());\n {product:OPID}\n ({int:OPID}();\n  {bound_id:OPID,:v}\n  ({int:OPID}()));\n {pair:OPID}\n ({natural_number:OPID,12:n}();\n  {natural_number:OPID,4:n}()))"
    in case parse_message_debug 9 str true of
	   [term] =>
	   (print (NT.toStringTerm term ^ "\n");
	    dump_prog_stream [] term "output-debug-blahblah")
	 | _ => raise Fail "run_program"
    end


(* ------ MAIN ------ *)

fun run args =
    let val {input,  output,  lib,     time,    sub,      sanity,
	     nuprl,  obid,    ascii,   tcheck,  parse,    split,
	     prt,    eval,    alldef,  test,    session,  simul,
	     step,   mono,    host,    port,    conf,     client,
	     send,   other,   all,     ev,      id,       extra,
	     gc,     file1,   file2,   monitor, tolisp} = format args
	val _ = if is_silent_extra extra then no_printing () else ()
	val poly = not mono
	val btyp = true
	val (evstr, ev) = (get_ev ev) handle _ => get_ev default_ev
	val _     = print_eml ("using evalutor " ^ evstr)
	val ident = if id = "" then Mac (host, port) else Loc id
	(*val _ = print ("[extra:"
		       ^ extra
		       ^ "-"
		       ^ Bool.toString (is_dump_extra extra)
		       ^ "-"
		       ^ Bool.toString (is_haskell_extra extra)
		       ^ "]\n")*)
    in if tolisp
       then prog2lisp prt extra input output alldef
       else if Option.isSome test
       then testing (Option.valOf test)
       else if parse
       then parseEML input
       else if tcheck
       then typecheck input sub
       else if sanity
       then sanitizer input output lib time sub
       else if nuprl
       then (toNuprl input output lib alldef obid time sub prt (true, true) poly btyp extra; ())
       else if ascii
       then parseNuprl input output prt split extra
       else if Option.isSome eval
       then evaluate input (Option.valOf eval) lib alldef obid sub extra
       else if session
       then start_session ev input lib alldef obid sub extra
       else if Option.isSome step
       then run_stepper (Option.valOf step) ev gc false false input alldef extra
       else if Option.isSome conf
       then let val config = Option.valOf conf
	    in if simul
	       then start_stepper input config ev gc lib alldef obid time sub prt extra
	       else if client
	       then simulate_clients_server config prt
	       else if send
	       then simulate_clients_send config alldef prt gc
	       else if other
	       then run_other_machine extra ident config prt
	       else if all
	       then start_all_machines ev gc config input lib alldef prt extra
	       else if monitor
	       then monitor_processes gc config alldef prt
	       else run_distributed_program extra ev gc ident config input lib alldef prt
	    end
       else if Option.isSome file1 andalso Option.isSome file2
       then alpha_equal_programs (Option.valOf file1) (Option.valOf file2)
       else slice input output lib time sub
    end

end
