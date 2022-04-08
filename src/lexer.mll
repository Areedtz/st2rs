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
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let num = ['0'-'9']+

(* part 2 *)
rule read =
  parse
  | white    { read lexbuf } (* skip blanks *)
  | newline  { next_line lexbuf; read lexbuf }
  | "bitstring"  { raise (SyntaxError ("Type 'bitstring' is not allowed")) }
  | ','      { COMMA }
  | '('      { LEFT_PAR }
  | ')'      { RIGHT_PAR }
  | '{'      { LEFT_BRACE }
  | '}'      { RIGHT_BRACE }
  | '['      { LEFT_BRACK }
  | ']'      { RIGHT_BRACK }
  | '<'      { LEFT_ANGLE }
  | '>'      { RIGHT_ANGLE }
  | '='      { EQ }
  | '&'      { AND }
  | '|'      { OR }
  | '~'      { NOT }
  | ':'      { COLON }
  | ';'      { SEMI }
  | '@'      { AT }
  | '%'      { PCT }
  | '#' [^ '\n']*      { read lexbuf }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | "new"    { NEW }
  | "let"    { LET }
  | "event"  { EVENT }
  | "inj-event" { INJ_EVENT }
  | "in"     { IN }
  | "end"    { END }
  | "if"     { IF }
  | "else"   { ELSE }
  | "dishonest"  { DISHONEST }
  | "Problem"    { PROBLEM }
  | "Principals" { PRINCIPALS }
  | "Knowledge"  { KNOWLEDGE }
  | "Types"      { TYPES }
  | "Functions"  { FUNCTIONS }
  | "Equations"  { EQUATIONS }
  | "Formats"    { FORMATS }
  | "Events"    { EVENTS }
  | "Queries"    { QUERIES }
  | "Protocol"   { PROTOCOL }
  | "->"         { ARROW }
  | "=>"         { BIGARROW }
  | "*->*"       { AUTHCONF }
  | id       { let s = Lexing.lexeme lexbuf in ID(s) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

and read_string buf =
  parse
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
