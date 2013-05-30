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
 *  o File name:   Config.lex
 *  o Description: Lexer for config files
 *)


structure T = Tokens

type pos           = int * int
type svalue        = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult     = (svalue, pos) token
type arg           = string * pos ref

fun eof (file, posref) = T.EOF (!posref, !posref)

fun toStringPos (l, c) = Int.toString l ^ "." ^ Int.toString c

fun error ((file, posref), msg) =
    TextIO.print (file ^ ":" ^ toStringPos (!posref) ^ ": " ^  msg ^ "\n")

fun badchar (file, posref) =
    let val msg = "bad character in " ^ file ^ " at: " ^ toStringPos (!posref)
        val _   = raise Fail msg
    in ()
    end

fun token c s (_, posref) =
    let val (from as (row, col)) = !posref
	val next = col + String.size s
	val to = next - 1
    in (posref := (row, next); c (from, (row, to)))
    end

fun tokenWithString c s (_, posref) =
    let val (from as (row, col)) = !posref
	val next = col + String.size s
	val to = next - 1
    in (posref := (row, next); c (s, from, (row, to)))
    end

fun addString (line, column) s = (line, column + String.size s)

fun skip s (_, posref) = posref := addString (!posref) s

fun newline (_, posref) =
    let val (row, col) = !posref
    in posref := (row + 1, 1)
    end


%%
%header (functor ConfigLexFun(structure Tokens : Config_TOKENS));
%arg (arg);
%s ATOMS;


digitDec     = [0-9];
boolean      = "true" | "false";
numeral      = [1-9] {digitDec}*;
digitDecSeq  = {digitDec}+;
integerDec   = ~ {digitDecSeq} | {digitDecSeq};
natDec       = {digitDecSeq};
esc          = [\t\ \r]+;
letter       = [a-zA-Z];
colon        = ":";
star         = "*";
plus         = "+";
prime        = "'";
minus        = "-";
at           = "@";
append       = "++";
presymb      = [! % & \$ # \/ \< \= \? \\ ~ \^ \| \> -];
symbol       = {colon} | {plus} | {at} | {presymb} | {star};
symboltc     = {colon} | {plus} | {at} | {presymb};
symbolstc    = {symboltc} | {symbol} {symbol}+;
alphadigit   = {letter} | {digitDec} | _ | {prime} | {minus};
tycon        = {letter} {alphadigit}* | {symbolstc};
ident        = {letter} {alphadigit}* | {symbol}+;
stident      = ({alphadigit} | {plus} | {at} | {star} | {presymb})+;
comment      = "%%" [^\n]* "\n";


%%


<INITIAL,ATOMS> "\n"         => (newline arg; continue());

<INITIAL,ATOMS> {esc}        => (skip yytext arg; continue());

<INITIAL> {comment}          => (skip yytext arg; newline arg; continue());

<INITIAL> "``"               => (YYBEGIN ATOMS; token T.LATOMS yytext arg);

<INITIAL> "["                => (token T.LLIST       yytext arg);
<INITIAL> "]"                => (token T.RLIST       yytext arg);
<INITIAL> "{"                => (token T.LBRACE      yytext arg);
<INITIAL> "}"                => (token T.RBRACE      yytext arg);
<INITIAL> ","                => (token T.COMMA       yytext arg);
<INITIAL> ";"                => (token T.SEMICOLON   yytext arg);
<INITIAL> ":"                => (token T.COLON       yytext arg);
<INITIAL> "("                => (token T.LPAREN      yytext arg);
<INITIAL> ")"                => (token T.RPAREN      yytext arg);
<INITIAL> {star}             => (token T.STAR        yytext arg);
<INITIAL> "="                => (token T.EQUAL       yytext arg);
<INITIAL> "->"               => (token T.ARROW       yytext arg);
<INITIAL> "\\"               => (token T.LAMBDA      yytext arg);
<INITIAL> "."                => (token T.DOT         yytext arg);
<INITIAL> "and"              => (token T.AND         yytext arg);
<INITIAL> "if"               => (token T.IF          yytext arg);
<INITIAL> "then"             => (token T.THEN        yytext arg);
<INITIAL> "else"             => (token T.ELSE        yytext arg);
<INITIAL> "LOC"              => (token T.LOC         yytext arg);
<INITIAL> "DEQ"              => (token T.DEQ         yytext arg);
<INITIAL> "TYPE"             => (token T.TYPE        yytext arg);
<INITIAL> "external"         => (token T.EXTERNAL    yytext arg);
<INITIAL> "%locations"       => (token T.LOCATIONS   yytext arg);
<INITIAL> "%connections"     => (token T.CONNECTIONS yytext arg);
<INITIAL> "%parameters"      => (token T.PARAMETERS  yytext arg);
<INITIAL> "%messages"        => (token T.MESSAGES    yytext arg);
<INITIAL> "%databases"       => (token T.EOF         yytext arg);
<INITIAL> "%tobroadcast"     => (token T.EOF         yytext arg);
<INITIAL> {numeral}          => (tokenWithString T.NUM yytext arg);
<INITIAL> {natDec}           => (tokenWithString T.INT yytext arg);
<INITIAL> {ident}            => (tokenWithString T.ID  yytext arg);
<INITIAL> .                  => (badchar arg; continue ());

<ATOMS> {stident}            => (tokenWithString T.ID yytext arg);
<ATOMS> "``"                 => (YYBEGIN INITIAL; token T.RATOMS yytext arg);
<ATOMS> .                    => (badchar arg; continue ());
