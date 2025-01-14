open Lexer
open Lexing
open Rusttypes
open Types
open Proverif

let fprintf = Printf.fprintf

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum pos.pos_bol

let parse_with_error lexbuf =
  try Parser.program Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let main =
  let filename = Sys.argv.(1) in
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let action = if Array.length Sys.argv > 2 then Sys.argv.(2) else "rust" in
  let f = match action with
  | "rust" -> rust_output
  | "proverif" -> proverif
  | _ -> fun x -> ()
  in
  match parse_with_error lexbuf with
  | Some p -> f p
  | None -> ();
  close_in inx
