{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* part 1 *)
let int =  '-'? ['0'-'9'] ['0'-'9']*

(* part 2 *)
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

(* part 3 *)
let white = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n" 
let upper_id = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let lower_id = ['a'-'z' ] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let op = ['+' '\\' '-' '/' '*' '=' '.' '$' '<' '>' ':' '&''|''^''?''%''#''@''~''!''+''|']+


rule token = parse
| white    { token lexbuf }
| newline  { NEWLINE 
(*
(next_line lexbuf; token lexbuf) 
*)
}

| "TRUE" { TRUEToken }
| "FALSE" { FALSEToken }
| "true" { TRUEE (bool_of_string (Lexing.lexeme lexbuf))}
| "false" { FALSEE (bool_of_string (Lexing.lexeme lexbuf))}

| ">=" {GTEQ}
| "<=" {LTEQ}
| '>' {GT}
| '<' {LT}
| '=' {EQ}
| "/\\" {CONJ}
| '|' { CHOICE }
| ">=" {GTEQ}
| "<=" {LTEQ}

| '(' { LPAR }
| ')' { RPAR }
| '{' { LBRACK  }
| '}' { RBRACK }
| '[' { LBrackets }
| ']' { RBrackets }
| '/' {DIV}
| '\\' {LAMDA}
| "|>" {THEN_}
| int      { INTE (int_of_string (Lexing.lexeme lexbuf)) }
| '.' { CONCAT }

| 'X' {NEXT}
| 'U' {UNTIL}
| "import" {IMPORT}
| "exposing" {EXPOSING}
| "case" {CASE}
| "of" {OF}

| "type" {TYPE}
| "alias" {ALIAS}


| "as" {AS}
| "module" {MODULE}
| ".." {ALLEX}
| upper_id as str { UVAR str }
| lower_id as str { LVAR str }
| "|-" {ENTIL}
| "\\/" {DISJ}
| '+' { PLUS }
| '-' { MINUS }
| '~' {NEGATION}



| 'w' {OMEGA}
| '_' {UNDERLINE}

| '^' { POWER }
| '*' {KLEENE}

| "<>" {FUTURE}  
| "->" {IMPLY}
| '!' {LTLNOT}
| ',' { COMMA }
| ':' { COLON }
| "&&" {LILAND}
| "||" {LILOR}
| "--" { read_single_line_comment lexbuf }
| "{-" { read_multi_line_comment lexbuf }
| '"'      { read_string (Buffer.create 17) lexbuf }


| op as str {COP str}
| eof { EOF }

(*

| "/*@" {LSPEC}
| "@*/" {RSPEC}

| '?' {QUESTION}
| '#' { SHARP }

| ';' { SIMI }
| "||" { PAR }
| "require" {REQUIRE}
| "ensure" {ENSURE}

| "hiphop" {HIPHOP}
| "inout" {INOUT}
| "out" {OUT}
| "end" {END}
| "in" {IN}
| "then" {THEN}
| "when" {WHEN}
| "if" {IF}
| "else" {ELSE}
| "count" { COUNT }
| "abort" {ABORT} 
| "halt" { NOTHING }
| "yield" {PAUSE}  
| "loop" {LOOP}
| "signal" {SIGNAL}
| "emit" {EMIT}
| "await" {AWAIT}
| "async" {ASYNC}

| "assert" {ASSERT}

| "present" {PRESENT}
| "run" {RUN}
| "trap" {TRAP}
| "exit" {EXIT}
| "emp" { EMPTY }

| "else" {ELSE}
| "[]" {GLOBAL}
| "include" {INCLUDE}




| '"' { read_string (Buffer.create 17) lexbuf }

| '[' { LBrackets }
| ']' { RBrackets }
| '{' { LBRACK  }
| '}' { RBRACK }










*)
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }


and read_single_line_comment = parse
  | newline { next_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { read_single_line_comment lexbuf }
  
and read_multi_line_comment = parse
  | "-}" { token lexbuf }
  | newline { next_line lexbuf; read_multi_line_comment lexbuf }
  | eof { raise (SyntaxError ("Lexer - Unexpected EOF - please terminate your comment.")) }
  | _ { read_multi_line_comment lexbuf }


(* part 5   *)
and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

