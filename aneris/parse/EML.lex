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
 *  o File name:   EML.lex
 *  o Description: EventML lexer.
 *)


structure L = LexDefs
structure C = Comment
structure T = Tokens
structure R = Reg

type pos           = int * int
type svalue        = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult     = (svalue, pos) token
type arg           = string * pos ref

datatype str_kind = SK_STRING | SK_TOKENS | SK_TOKEN

(* Position:    position of the begining of the string being read
 * First bool:  true if reading a string
 * str_kind;    kind of the read stuff
 * String:      string being read *)
val initQuotedString : (pos * bool * str_kind * string) =
  ((0, 0), false, SK_STRING, "")

val quotedString = ref initQuotedString

fun consCommentRegions pos =
    [R.consReg pos (R.upPos pos)]

fun consRegions pos = [R.consReg pos pos]

fun reset () =
    let val _ = quotedString := initQuotedString
    in ()
    end

fun raiseError error = (reset (); raise error)

fun eof (file, posref) =
    if not (Comment.isClosed())
    then raiseError (L.UnclosedComment (file, consCommentRegions (Option.valOf (Comment.getTop ())) handle Option => []))
    else let val (pos, read, b, str) = !quotedString
         in if read
            then raiseError (L.UnclosedString (file, consRegions pos))
            else T.EOF (!posref, !posref)
	 end

fun error ((file, posref), msg) =
    TextIO.print (file ^ ":" ^ R.toStringPos (!posref) ^ ": " ^  msg ^ "\n")

fun badchar s (file, posref) =
    let val msg = "ignoring bad character"
	(*val _   = TextIO.print (file ^ ":" ^ R.toStringPos (!posref) ^ ": " ^  msg ^ "\n")*)
		   val _   = raise L.BadCharacter (file, s, consRegions (!posref))
    in ()
    end

fun unclosedString s (file, posref) =
    let val (pos, read, b, str) = !quotedString
    in if read
       then raise L.UnclosedString (file, consRegions pos)
       else badchar s (file, posref) (* Should never happen *)
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

fun skip s (_, posref) = posref := R.addString (!posref) s

fun newline (_, posref) =
    let val (row, col) = !posref
	val _ = L.addLength (row, col)
    in posref := (row + 1, 1)
    end

fun closedComment (file, posref) =
    raise L.ClosedComment (file, consCommentRegions (!posref))

fun addString (charlist, s:string, (_, posref)) =
    let val (row, col) = !posref
        val next = col + String.size s
        val _ = posref := (row, next)
        val _ = charlist := s :: (!charlist)
    in ()
    end

fun addChar (charlist, c:char, arg) = addString (charlist, String.str c, arg)

fun makeString charlist = (concat(rev(!charlist)) before charlist := nil)

fun quotedStringIsToken () = #3 (!quotedString) = SK_TOKEN

fun startQuotedString (_, posref) b = quotedString := (!posref, true, b, "")

fun addQuotedString (strR, yytext, (_, posref)) =
    let val (row, col) = !posref
        val next = col + String.size yytext
	val (pos, read, b, strL) = !quotedString
        val _ = posref := (row, next)
	val _ = quotedString := (pos, read, b, strL ^ strR)
    in ()
    end

fun addButFirstQuotedString (yytext, (_, posref)) =
    let val (row, col) = !posref
        val next = col + String.size yytext
	val (pos, read, b, strL) = !quotedString
	val strR = String.extract (yytext, 1, NONE)
        val _ = posref := (row, next)
	val _ = quotedString := (pos, read, b, strL ^ strR)
    in ()
    end

fun tokenWithStringSTRING (file, posref) b2 =
    let val (row, col) = !posref
	val (pos, read, b, str) = !quotedString
	val _ = if b = b2 then () else raise  L.BadAtomsClosing (file, consRegions (!posref))
        val str = str
        val (tok,sk) =
            case b of
	       SK_STRING => (T.STRING, 1)
	     | SK_TOKENS => (T.ATOMS,  2)
	     | SK_TOKEN  => (T.ATOM,   1)
	val _ = quotedString := initQuotedString
    in (posref := (row, col + sk); tok (str, pos, (row, col)))
    end

fun stringToChar str =
    let val sub = String.substring (str, 1, 3)
        val num = Option.valOf (Int.fromString sub)
    in if num > 255
       then "" (* raise an error here *)
       else Char.toString (Char.chr num)
    end

val doc = ref ([] : string list)

fun startDoc (_, posref) =
    let val _ = doc := []
	val _ = C.ope (!posref)
    in ()
    end

fun addDoc s (_, posref) =
    let val _ =  posref := R.addString (!posref) s
    in case !doc of
	   [] => doc := [s]
	 | lst => let val r = List.rev lst
                      val last = hd r
            	      val firsts = List.rev (tl r)
	          in doc:= firsts @ [last ^ s]
	          end
    end

fun newlineDoc (_, posref) =
    let val (row, col) = !posref
	val _ = L.addLength (row, col)
        val _ = posref := (row + 1, 1)
	val _ = doc := !doc @ [""]
    in ()
    end

fun tokenWithDoc s (_, posref) =
    let val (from as (row, col)) = !posref
	val next = col + String.size s
	val to = next - 1
        val lst = !doc
        val _ = doc := []
    in (posref := (row, next); T.DOC (lst, from, (row, to)))
    end


%%
%header (functor EMLLexFun(structure Tokens : EML_TOKENS));
%arg (arg);
%s COMMENT DOC STRI ESCSTRI;
%reject


digitDec     = [0-9];
boolean      = "true" | "false";
numeral      = [1-9] {digitDec}*;
digitDecSeq  = {digitDec}+;
digitHexLow  = [a-f];
digitHexUp   = [A-F];
digitHex     = {digitHexLow} | {digitHexUp};
hex          = {digitHex} | {digitDec};
digitHexSeq  = {hex}+;
integerDec   = ~ {digitDecSeq} | {digitDecSeq};
integerHex   = ~ 0 x {digitHexSeq} | 0 x {digitHexSeq};
natDec       = {digitDecSeq};
natHex       = 0 x {digitHexSeq};
wordDec      = 0 w {digitDecSeq};
wordHex      = 0 w x {digitHexSeq};
realE        = {integerDec} | {integerDec} "." {digitDecSeq};
real         = {realE} | {realE} [eE] {integerDec};
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
tyvar        = {prime} {alphadigit}*;


%%


<INITIAL,COMMENT> "\n"       => (newline arg;
				 continue());

<INITIAL,COMMENT> {esc}      => (skip yytext arg;
				 continue());

<DOC> "\n"                   => (newlineDoc arg;
				 continue());

<DOC> {esc}                  => (addDoc yytext arg;
				 continue());

<INITIAL> "(**"              => (startDoc arg;
				 skip yytext arg;
				 YYBEGIN DOC;
				 continue());

<INITIAL> "(*"               => (C.ope (!(#2 arg));
				 skip yytext arg;
				 YYBEGIN COMMENT;
				 continue());

<INITIAL> "*)"               => (closedComment arg;
				 continue ());

<INITIAL> "\""               => (YYBEGIN STRI;
				 startQuotedString arg SK_STRING;
				 skip yytext arg;
				 continue ());

<INITIAL> "``"               => (YYBEGIN STRI;
				 startQuotedString arg SK_TOKENS;
				 skip yytext arg;
				 continue ());

<INITIAL> "`".               => (YYBEGIN STRI;
				 startQuotedString arg SK_TOKEN;
				 addButFirstQuotedString (yytext, arg);
				 continue ());

<INITIAL> "o"                => (token T.COMP       yytext arg);
<INITIAL> "oo"               => (token T.CLSCOMP    yytext arg);
<INITIAL> "O"                => (token T.BIGCOMP    yytext arg);
<INITIAL> ">>"               => (token T.SBIND      yytext arg);
<INITIAL> "{"                => (token T.LBRACE     yytext arg);
<INITIAL> "}"                => (token T.RBRACE     yytext arg);
<INITIAL> "["                => (token T.LLIST      yytext arg);
<INITIAL> "]"                => (token T.RLIST      yytext arg);
<INITIAL> ","                => (token T.COMMA      yytext arg);
<INITIAL> "("                => (token T.LPAREN     yytext arg);
<INITIAL> ")"                => (token T.RPAREN     yytext arg);
<INITIAL> "->"               => (token T.TYPEARROW  yytext arg);
<INITIAL> "<-"               => (token T.CBVA       yytext arg);
<INITIAL> "<=>"              => (token T.IFF        yytext arg);
<INITIAL> "=>"               => (token T.FUNARROW   yytext arg);
<INITIAL> {star}             => (token T.STAR       yytext arg);
<INITIAL> {at}               => (token T.AT         yytext arg);
<INITIAL> {append}           => (token T.APPEND     yytext arg);
<INITIAL> "|"                => (token T.PIPE       yytext arg);
<INITIAL> "=="               => (token T.EQEQUALOP  yytext arg);
<INITIAL> "="                => (token T.EQUALOP    yytext arg);
<INITIAL> "or"               => (token T.BOR        yytext arg);
<INITIAL> "&"                => (token T.BAND       yytext arg);
<INITIAL> "\\/"              => (token T.POR        yytext arg);
<INITIAL> "/\\"              => (token T.PAND       yytext arg);
<INITIAL> "and"              => (token T.TAND       yytext arg);
<INITIAL> "on"               => (token T.ON         yytext arg);
<INITIAL> "forall"           => (token T.FORALL     yytext arg);
<INITIAL> "exists"           => (token T.EXISTS     yytext arg);
<INITIAL> "let"              => (token T.LET        yytext arg);
<INITIAL> "letrec"           => (token T.LETREC     yytext arg);
<INITIAL> "class"            => (token T.CLASS      yytext arg);
<INITIAL> "classrec"         => (token T.CLASSREC   yytext arg);
<INITIAL> "in"               => (token T.IN         yytext arg);
<INITIAL> "fn"               => (token T.FN         yytext arg);
<INITIAL> "if"               => (token T.IF         yytext arg);
<INITIAL> "then"             => (token T.THEN       yytext arg);
<INITIAL> "else"             => (token T.ELSE       yytext arg);
<INITIAL> "_"                => (token T.WILD       yytext arg);
<INITIAL> "::"               => (token T.DCOLON     yytext arg);
<INITIAL> {colon}            => (token T.COLON      yytext arg);
<INITIAL> ";"                => (token T.SEMICOLON  yytext arg);
<INITIAL> "overload"         => (token T.OVERLOAD   yytext arg);
<INITIAL> "."                => (token T.DOT        yytext arg);
<INITIAL> "\\"               => (token T.BACKSLASH  yytext arg);
<INITIAL> "/~"               => (token T.QUOTIENT   yytext arg);
<INITIAL> "~"                => (token T.SQMINUS    yytext arg);
<INITIAL> "Prior"            => (token T.PRIOR      yytext arg);
<INITIAL> "Once"             => (token T.ONCE       yytext arg);
<INITIAL> "Output"           => (token T.SENDOC     yytext arg);
<INITIAL> "OnLoc"            => (token T.ONLOC      yytext arg);
<INITIAL> "State"            => (token T.STATEC     yytext arg);
<INITIAL> "Memory"           => (token T.MEMORYC    yytext arg);
<INITIAL> "Skip"             => (token T.SKIP       yytext arg);
<INITIAL> "wait"             => (token T.WAIT       yytext arg);
<INITIAL> "Null"             => (token T.NULLCLS    yytext arg);
<INITIAL> "constant"         => (token T.CONS       yytext arg);
<INITIAL> "import"           => (token T.IMPORT     yytext arg);
<INITIAL> "export"           => (token T.EXPORT     yytext arg);
<INITIAL> "include"          => (token T.INCLUDE    yytext arg);
<INITIAL> "interface"        => (token T.INTERFACE  yytext arg);
<INITIAL> "parameter"        => (token T.PARAM      yytext arg);
<INITIAL> "assume"           => (token T.ASSUME     yytext arg);
<INITIAL> "guarantee"        => (token T.GUARANTEE  yytext arg);
<INITIAL> "invariant"        => (token T.INVARIANT  yytext arg);
<INITIAL> "ordering"         => (token T.ORDERING   yytext arg);
<INITIAL> "progress"         => (token T.PROGRESS   yytext arg);
<INITIAL> "consistency"      => (token T.CONSIST    yytext arg);
<INITIAL> "memory"           => (token T.MEMORY     yytext arg);
<INITIAL> "variable"         => (token T.VARIABLE   yytext arg);
<INITIAL> "options"          => (token T.OPTIONS    yytext arg);
<INITIAL> "type"             => (token T.TYPE       yytext arg);
<INITIAL> "any"              => (token T.ANY        yytext arg);
<INITIAL> "observes"         => (token T.OBSERVES   yytext arg);
<INITIAL> "set"              => (token T.SET        yytext arg);
<INITIAL> "map"              => (token T.MAP        yytext arg);
<INITIAL> {plus}             => (token T.PLUS       yytext arg);
<INITIAL> "self"             => (token T.SELF       yytext arg);
<INITIAL> "with"             => (token T.WITH       yytext arg);
<INITIAL> "where"            => (token T.WHERE      yytext arg);
<INITIAL> "MSG"              => (token T.MSG        yytext arg);
<INITIAL> "case"             => (token T.CASE       yytext arg);
<INITIAL> "of"               => (token T.OF         yytext arg);
<INITIAL> "??"               => (token T.CLASSOPTC  yytext arg);
<INITIAL> "?"                => (token T.CLASSOPT   yytext arg);
<INITIAL> "typeof"           => (token T.TYPEOF     yytext arg);
<INITIAL> "infix"            => (token T.INFIX      yytext arg);
<INITIAL> "infixr"           => (token T.INFIXR     yytext arg);
<INITIAL> "op"               => (token T.OP         yytext arg);
<INITIAL> "struct"           => (token T.STRUCT     yytext arg);
<INITIAL> "specification"    => (token T.SPEC       yytext arg);
<INITIAL> "main"             => (token T.MAIN       yytext arg);
<INITIAL> "internal"         => (token T.INTERNAL   yytext arg);
<INITIAL> "output"           => (token T.OUTPUT     yytext arg);
<INITIAL> "input"            => (token T.INPUT      yytext arg);
<INITIAL> "data"             => (token T.DATA       yytext arg);
<INITIAL> "abstype"          => (token T.ABSTYPE    yytext arg);
<INITIAL> {numeral}          => (tokenWithString T.NUM     yytext arg);
<INITIAL> {natDec}           => (tokenWithString T.INT     yytext arg);
<INITIAL> {natHex}           => (tokenWithString T.INT     yytext arg);
<INITIAL> {wordDec}          => (tokenWithString T.WORD    yytext arg);
<INITIAL> {wordHex}          => (tokenWithString T.WORD    yytext arg);
<INITIAL> {real}             => (tokenWithString T.REAL    yytext arg);
<INITIAL> {tyvar}            => (tokenWithString T.TYPEVAR yytext arg);
<INITIAL> {ident}            => (tokenWithString T.ID      yytext arg);
<INITIAL> .                  => (badchar yytext arg;
				 continue ());

<COMMENT> "(*"               => (C.ope (!(#2 arg));
				 skip yytext arg;
				 continue());
<COMMENT> "*)"               => (skip yytext arg;
				 C.close ();
				 if C.isClosed () then YYBEGIN INITIAL else ();
				 continue());
<COMMENT> .                  => (skip yytext arg;
				 continue());

<DOC> "(*"                   => (C.ope (!(#2 arg));
				 addDoc yytext arg;
				 continue());
<DOC> "*)"                   => (addDoc yytext arg;
				 C.close ();
				 if C.isClosed ()
                                 then (YYBEGIN INITIAL;
                                       tokenWithDoc yytext arg)
				 else continue());
<DOC> .                      => (addDoc yytext arg;
				 continue());

<STRI> "\\"                  => (skip yytext arg;
				 YYBEGIN ESCSTRI;
				 continue ());
<STRI> "\\a"                 => (addQuotedString ("\a", yytext, arg);
				 continue ());
<STRI> "\\b"                 => (addQuotedString ("\b", yytext, arg);
				 continue ());
<STRI> "\\t"                 => (addQuotedString ("\t", yytext, arg);
				 continue ());
<STRI> "\\n"                 => (addQuotedString ("\n", yytext, arg);
				 continue ());
<STRI> "\\v"                 => (addQuotedString ("\v", yytext, arg);
				 continue ());
<STRI> "\\f"                 => (addQuotedString ("\f", yytext, arg);
				 continue ());
<STRI> "\\r"                 => (addQuotedString ("\r", yytext, arg);
				 continue ());
<STRI> "\\\\"                => (addQuotedString ("\\", yytext, arg);
				 continue ());
<STRI> "\\\""                => (addQuotedString ("\"", yytext, arg);
				 continue ());
<STRI> "\\`"                 => (addQuotedString ("`", yytext, arg);
				 continue ());
<STRI> "\\"[0-9]{3}          => (addQuotedString (stringToChar yytext, yytext, arg);
				 continue ());
<STRI> "\\u"{hex}{4}         => (addQuotedString (yytext, yytext, arg);
				 continue ());
<STRI> "\""                  => (YYBEGIN INITIAL;
				 tokenWithStringSTRING arg SK_STRING);
<STRI> "``"                  => (if quotedStringIsToken ()
                                 then REJECT ()
                                 else (YYBEGIN INITIAL;
				       tokenWithStringSTRING arg SK_TOKENS));
<STRI> "`"                   => (YYBEGIN INITIAL;
				 tokenWithStringSTRING arg SK_TOKEN);
<STRI> "\n"                  => (unclosedString yytext arg;
				 continue ());
<STRI> [^\"\n]               => (addQuotedString (yytext, yytext, arg);
				 continue());

<ESCSTRI> "\n"               => (newline arg;
				 continue());
<ESCSTRI> {esc}              => (skip yytext arg;
				 continue());
<ESCSTRI> "\\"               => (skip yytext arg;
				 YYBEGIN STRI;
				 continue());
<ESCSTRI> .                  => (badchar yytext arg;
				 continue ());
