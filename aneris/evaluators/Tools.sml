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
 *  o File name:   Tools.sml
 *  o Description: Some useful functions.
 *)


structure Tools :> TOOLS = struct

fun member (element : string) list =
    List.exists (fn v => v = element) list

fun can f x = (f x; true) handle _ => false

fun fst (x, y) = x
fun snd (x, y) = y

fun mapfilter f list =
    List.mapPartial (fn x => ((SOME (f x)) handle _ => NONE))
		    list

fun decr_steps steps =
    if steps = 0
    then raise Fail "decr_steps"
    else steps - 1

fun split n list = (List.take (list, n), List.drop (list, n))

fun accumulate2 f i [] [] = i
  | accumulate2 f i [] _  = raise Fail "accumulate2"
  | accumulate2 f i _  [] = raise Fail "accumulate2"
  | accumulate2 f i (x :: xs) (y :: ys) = accumulate2 f (f i x y) xs ys

fun all2 f [] [] = true
  | all2 f [] _  = false
  | all2 f _  [] = false
  | all2 f (x :: xs) (y :: ys) = (f x y) andalso (all2 f xs ys)

fun get1 [x1] = x1
  | get1 lst  = raise Fail ("get1:" ^ Int.toString (List.length lst))

fun get2 [x1,x2] = (x1,x2)
  | get2 _ = raise Fail "get2"

fun get3 [x1,x2,x3] = (x1,x2,x3)
  | get3 _ = raise Fail "get3"

fun get4 [x1,x2,x3,x4] = (x1,x2,x3,x4)
  | get4 _ = raise Fail "get4"

fun get1_0bound x =
    (case get1 x of
	 ([],z) => z
       | _ => raise Fail "get1_0bound")
    handle _ => raise Fail "get1_0bound"

fun get1_1bound x =
    (case get1 x of
	 ([a],z) => (a,z)
       | _ => raise Fail "get1_1bound")
    handle _ => raise Fail "get1_1bound"

fun get1_2bound x =
    (case get1 x of
	 ([a,b],z) => (a,b,z)
       | _ => raise Fail "get1_2bound")
    handle _ => raise Fail "get1_2bound"

fun get2_0bound x =
    (case get2 x of
	 (([],z),([],w)) => (z,w)
       | _ => raise Fail "get2_0bound")
    handle _ => raise Fail "get2_0bound"

fun get2_1bound x =
    (case get2 x of
	 (([a],z),([b],w)) => (a,z,b,w)
       | _ => raise Fail "get2_1bound")
    handle _ => raise Fail "get2_1bound"

fun get2_02bound x =
    (case get2 x of
	 (([],z),([a,b],w)) => (z,a,b,w)
       | _ => raise Fail "get2_02bound")
    handle _ => raise Fail "get2_02bound"

fun get2_03bound x =
    (case get2 x of
	 (([],z),([a,b,c],w)) => (z,a,b,c,w)
       | _ => raise Fail "get2_03bound")
    handle _ => raise Fail "get2_03bound"

fun get3_202bound x =
    (case get3 x of
	 (([a,b],z),([],w),([c,d],y)) => (a,b,z,w,c,d,y)
       | _ => raise Fail "get3_202bound")
    handle _ => raise Fail "get3_202bound"

type timer = {real : Timer.real_timer,
	      cpu  : Timer.cpu_timer}

fun startTimer () = {real = Timer.startRealTimer (),
		     cpu  = Timer.startCPUTimer ()}

fun getTime (timer : timer) = Timer.checkRealTimer (#real timer)

fun getMilliTime timer = Time.toMilliseconds (getTime timer)

end
