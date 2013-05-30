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
 *  o File name:   NuprlAscii.lex
 *  o Description: Lexer for Nuprl terms in ascii forms.
 *)


structure T = Tokens

type pos           = int * int
type svalue        = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult     = (svalue, pos) token
type arg           = string * pos ref

fun eof (file, posref) = T.EOF (!posref, !posref)

fun toStringPos (line, column) = Int.toString line ^ "." ^ Int.toString column

fun addString (line, column) s = (line, column + String.size s)

fun error ((file, posref), msg) =
    TextIO.print (file ^ ":" ^ toStringPos (!posref) ^ ": " ^  msg ^ "\n")

fun token c s (_, posref) =
    let val (from as (row, col)) = !posref
	val next = col + String.size s
	val to = next - 1
    in (posref := (row, next); c (from, (row, to))) end

fun tokenWithString c s (_, posref) =
    let val (from as (row, col)) = !posref
	val next = col + String.size s
	val to = next - 1
    in (posref := (row, next); c (s, from, (row, to))) end

fun skip s (_, posref) = posref := addString (!posref) s

fun newline (_, posref) =
    let val (row, col) = !posref
    in posref := (row + 1, 1)
    end


%%
%header (functor NuprlAsciiLexFun(structure Tokens : NuprlAscii_TOKENS));
%arg (arg);


escape     = [\t\ ]+;
digitDec   = [0-9];
letter     = [a-zA-Z];
symbol     = [! % & \$ # \+ \/ \< \= \> \? @ \\ ~ ` \^ \| \* -];
prime      = "'";
alphadigit = {letter} | {digitDec} | _ | {prime} | {symbol}
           | "\\{" | "\\}" | "\\:" | "\\," | "\\ " | "\\["
	   | "\\]" | "\\." | "\\(" | "\\)";
ident      = {alphadigit}*;


%%


"\n"     => (newline arg;
	     continue());

{escape} => (skip yytext arg;
	     continue());
"\000"   => (skip yytext arg;
	     continue());

"\004"   => (token T.SEP          yytext arg);
"("      => (token T.LPAREN       yytext arg);
")"      => (token T.RPAREN       yytext arg);
"{"      => (token T.LBRACE       yytext arg);
"}"      => (token T.RBRACE       yytext arg);
":"      => (token T.COLON        yytext arg);
";"      => (token T.SEMICOLON    yytext arg);
","      => (token T.COMMA        yytext arg);
{ident}  => (tokenWithString T.ID yytext arg);
.        => (error (arg, "ignoring bad character(" ^ yytext ^ ")");
	     continue());
