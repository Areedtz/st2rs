open Types
open Localtypes
open Rusttranslator

let abstract_type = "T"
let concrete_type = "/* unimplemented */"
let functions_variable = "f"
let pair = "pair"
let pair_function = functions_variable ^ "." ^ pair
let indent = "    "
let enum = "I"
let enum_func =  "::"^enum

let rec show_term_pair = function
    [] -> ""
  | [x] -> "" ^ show_term x (* In all cases we want to lend the value *)
  | (x::xs) ->  pair_function ^ "("  ^  "&" ^show_term x ^ ", &" ^ show_term_pair xs ^ ")"

and show_term = function
    Var(x) -> x
  | Func(name, args) -> functions_variable ^ "." ^ name ^ "(" ^ show_term_list args ^ ")"
  | Form(name, args) -> show_format name ^ "(" ^ show_term_list_without_lending args ^ ")"
  | Tuple(args) -> show_term_pair args
  | Eq(t1, t2) -> show_term t1 ^ " = " ^ show_term t2
  | And(t1, t2) -> show_term t1 ^ " & " ^ show_term t2
  | Or(t1, t2) -> show_term t1 ^ " | " ^ show_term t2
  | Not(t) -> "~" ^ show_term t

and show_format name = name ^ enum_func

(* List options: empty, single item, list *)
and show_term_list_without_lending = function
    [] -> ""
  | [x] -> "" ^ show_term x (* In all cases we want to lend the value *)
  | (x::xs) ->  "" ^ show_term x ^ ", " ^ show_term_list xs

and show_term_list = function
    [] -> ""
  | [x] -> "&" ^ show_term x (* In all cases we want to lend the value *)
  | (x::xs) ->  "&" ^ show_term x ^ ", " ^ show_term_list xs

let rec term_as_type = function
    Var(x) -> abstract_type
  | Func(name, args) -> abstract_type (*name ^ "(" ^ get_term_as_type_channel_list args ^ ")"*)
  | Form(name, args) -> name
  | Tuple(args) -> "(" ^ term_as_type_list args ^ ")"

and term_as_type_list = function
    [] -> ""
  | [x] -> term_as_type x
  | (x::xs) -> abstract_type ^ ", " ^ term_as_type_list xs

and pattern = function
    PVar(x, _) -> abstract_type
  | PTuple(args) -> "pair(" ^ pattern_list args ^ ")"
  | PMatch(t) -> "=" ^ term_as_type t

and pattern_list = function
    [] -> ""
  | [x] -> pattern x
  | (x::xs) -> pattern x ^ ", " ^ pattern_list xs

and show_pattern = function
    PVar(x, _) -> x
  | PForm(fname, args) -> show_format fname ^ "(" ^ show_pattern_list args ^ ")"
  | PTuple(args) -> "<" ^ show_pattern_list args ^ ">"
  | PMatch(t) -> "=" ^ show_term t

and show_pattern_list = function
    [] -> ""
  | [x] -> show_pattern x
  | (x::xs) -> show_pattern x ^ ", " ^ show_pattern_list xs

and channels = function
    LSend(_, opt, t, local_type) -> "Send<Repr<" ^ term_as_type t ^ ">, " ^ channels local_type ^ ">"
  | LRecv (_, opt, pattern, term, local_type) -> "Recv<Repr<" ^ term_as_type term ^">, "^ channels local_type ^ ">"
  | LNew (ident, data_type, local_type) -> channels local_type
  | LLet (ident, term, local_type) -> channels local_type
  | LEvent (ident, term, local_type) -> channels local_type
  | _ -> "Eps"

and createArguments (t:data_type list) =
  let rec inner dt i =
    match dt with
      [] -> []
    | x::xs -> ("a" ^(string_of_int i)^ ": " ^ "&" ^ show_dtype x)::(inner xs (i+1)) in
  inner t 1

and functions (f : (ident * (data_type list * data_type * bool)) list) =
    match f with
  | [] -> []
  | (name,(args,ret,bool)) :: tail -> ("fn " ^ name ^ "(" ^ (String.concat ", " ("&self"::createArguments args)) ^ ") -> " ^ show_dtype ret) :: functions tail

and print ident term =
  "println!(\"" ^ ident ^ ": " ^ String.concat " " (List.map (fun t -> "{}") term) ^ "\", " ^ show_term_list term ^ ");\n"

and print_type t =
  match t with
  | DType dtype -> dtype
  | DAType (dtype,_) -> dtype

and rust_types type_list =
  let types = List.map (fun t -> "type "^ print_type t ^ " = " ^ concrete_type ^ ";") type_list in
  String.concat "\n" (types)

and rust_a_types type_list =
  let types = List.map (function DAType(s1,s2) -> "#[derive(Serialize, Deserialize)]\npub struct " ^ s1 ^ "<" ^ s2 ^">(Vec<u8>, PhantomData<T>);") type_list in
  String.concat "\n" (types)

and rust_channel p t =
  "type " ^ p ^ " = " ^ channels t ^ ";"


let rust_output (pr:problem) : unit =
  let knowledge = List.map (fun (p, _) -> p, initial_knowledge p [] pr.knowledge) pr.principals in
  let env = List.map (fun (p, e) -> (p, List.map (fun (i, d, _) -> (i, d)) e)) knowledge in
  Printf.printf "%s\n" (rust_handwritten);
  List.iter (fun (p, b) ->
      Printf.printf "%s\n" (rust_channel p (to_local_type pr.protocol p))) pr.principals;
  let abstract_types = List.filter_map (function DAType(s1,s2) -> Some(DAType(s1,s2)) | _ -> None) pr.types in
  let concrete_types = List.filter_map (function DType(s1) -> Some(DType(s1)) | _ -> None) pr.types in
  Printf.printf "\n%s\n" (rust_a_types abstract_types);
  Printf.printf "\n%s\n" (rust_types concrete_types);
  Printf.printf "\n%s\n" (rust_formats pr.formats);
  Printf.printf "\n%s\n" (rust_functions pr.functions concrete_types);
  List.iter (fun (p, b) -> Printf.printf "\n%s\n" (rust_process pr.knowledge p (compile env pr.formats pr.functions p pr.protocol))) pr.principals;
