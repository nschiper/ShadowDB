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
 *  o File name:   Infix.sml
 *  o Description: Handling of infix operators.
 *)


structure Infix :> INFIX = struct

structure OrdStr : ORD_KEY = struct
type ord_key = string
val compare  = String.compare
end

structure A  = Ast
structure R  = Reg
structure EH = LibBase
structure MS = SplayMapFn(OrdStr)

type 'a pack = 'a * R.region

type packstr = string pack

datatype 'a tree = L of 'a pack
		 | O of packstr
		 | N of packstr * 'a tree * 'a tree

datatype assoc = LEFT | RIGHT

type prec = int

type fixity = assoc * prec

type label = int

type operators = (label * fixity MS.map) list ref


(* For now, we don't associate associativity with the operators,
 * it depends on the precedence.
 * The label that goes with the id is the label corresponding to the
 * scope of the fixity declaration.  For example, if an id x is
 * declared an infix in a let expression labelled l, then we will
 * add (x, l) in one of the list. *)

val initOperators7 = [(A.id_int_mult,    LEFT,  7),
		      (A.id_int_div,     LEFT,  7)]
val initOperators6 = [(A.id_int_plus,    LEFT,  6),
		      (A.id_int_minus,   LEFT,  6)]
val initOperators5 = [(A.id_list_cons,   RIGHT, 5),
		      (A.id_list_concat, RIGHT, 5),
		      (A.id_class_bind,  RIGHT, 5)]
val initOperators4 = [(A.id_eq,          LEFT,  4),
		      (A.id_eqeq,        LEFT,  4),
		      (A.id_diff,        LEFT,  4),
		      (A.id_int_gt,      LEFT,  4),
		      (A.id_int_lt,      LEFT,  4),
		      (A.id_int_geq,     LEFT,  4),
		      (A.id_int_leq,     LEFT,  4),
		      (A.id_class_at,    RIGHT, 4),
		      (A.id_es_causl,    RIGHT, 4),
		      (A.id_es_locl,     RIGHT, 4)]
val initOperators3 = [(A.id_class_opt,   LEFT,  3),
		      (A.id_class_opt_c, LEFT,  3)]
val initOperators2 = [(A.id_class_until, LEFT,  2)]
val initOperators1 = [(A.id_class_par,   LEFT,  1)]
val initOperators0 = []

val initOperators = initOperators7 @
		    initOperators6 @
		    initOperators5 @
		    initOperators4 @
		    initOperators3 @
		    initOperators2 @
		    initOperators1 @
		    initOperators0

val initOperatorsMap =
    foldr (fn ((st, assoc, prec), operators) =>
	      MS.insert (operators, st, (assoc, prec)))
	  MS.empty
	  initOperators

val dummyLabel = 0
val label = ref 1
fun nextLabel () = let val x = !label in (label := x + 1; x) end

val operators = ref [(dummyLabel, initOperatorsMap)]

fun reset () =
    (label     := 1;
     operators := [(dummyLabel, initOperatorsMap)])

fun isInfix st = List.exists (fn (lab, map) => Option.isSome (MS.find (map, st))) (!operators)

(* We use foldl because we start with the tighter binding *)
fun getOperator st = List.foldl (fn ((lab, map), SOME x) => SOME x
				  | ((lab, map), NONE) => MS.find (map, st))
				NONE
				(!operators)

fun newScope () =
    let val lab = nextLabel ()
	val _   = operators := (lab, MS.empty) :: (!operators)
    in lab
    end

fun rmScope lab =
    case !operators of
	[] => raise EH.Impossible "leaving an empty scope?!"
      | ((lab', map) :: operators') =>
	if lab = lab'
	then operators := operators'
	else raise EH.Impossible "leaving another scope?!"

fun addInfix st assoc prec =
    case !operators of
	[] => raise EH.Impossible "cannot add operator to empty scope"
      | ((lab, map) :: operators') =>
	operators := (lab, MS.insert (map, st, (assoc, prec))) :: operators'

fun addInfixL st prec = addInfix st LEFT  prec
fun addInfixR st prec = addInfix st RIGHT prec

fun rmInfix st =
    case !operators of
	[] => raise EH.Impossible "cannot remove operator from empty scope"
      | ((lab, map) :: operators') =>
	(operators := (lab, #1 (MS.remove (map, st))) :: operators')
	handle LibBase.NotFound => ()


(* Extract region information from nodes *)

fun getLeft (L (_, r))    = r
  | getLeft (O (_, r))    = r
  | getLeft (N (_, t, _)) = getLeft t

fun getRight (L (_, r))    = r
  | getRight (O (_, r))    = r
  | getRight (N (_, t, _)) = getRight t


(*(* convert a list of string to a list of leaves *)
fun preConvert xs = map (fn x => L x) xs*)

fun convertGen []     _ _ = raise EH.Impossible ""
  | convertGen [x]    _ _ = [x]
  | convertGen [_, _] _ _ = raise EH.Impossible ""
  | convertGen (x :: (z as O (str, r)) :: y :: xs) assoc prec =
    (case getOperator str of
	 NONE => raise EH.Impossible (str ^ " should be an infix operator")
       | SOME (assoc', prec') =>
	 if assoc = assoc' andalso prec = prec'
	 then let val node = case assoc of
				 LEFT  => N ((str, r), x, y)
			       | RIGHT => N ((str, r), y, x)
	      in convertGen (node :: xs) assoc prec
	      end
	 else x :: z :: (convertGen (y :: xs) assoc prec))
  | convertGen (x :: (N _) :: y :: xs) _ _ = raise EH.Impossible ""
  | convertGen (x :: (L _) :: y :: xs) _ _ = raise EH.Impossible ""

fun convertGen' tokens prec =
    let val tokens1 = convertGen tokens LEFT prec
	val tokens2 = rev (convertGen (rev tokens1) RIGHT prec)
    in tokens2
    end

fun convert' x =
    foldr (fn (prec, x) => convertGen' x prec)
	  x
	  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

fun convert xs =
    case convert' xs of
	[x] => x
      | _   => raise EH.Impossible ""


end
