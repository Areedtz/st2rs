open Types
open Localtypes
open Rusttranslator

let abstract_type = "T"
let concrete_type = "/* unimplemented */"
let pair = "pair"
let pair_function = pair
let indent = "    "
let interface_impl_name = "Functions"
let enum = "I"
let enum_func =  "::"^enum

let rec show_term_pair = function
    [] -> ""
  | [x] -> "" ^ show_term x (* In all cases we want to lend the value *)
  | (x::xs) ->  pair_function ^ "("  ^  "&" ^ show_term x ^ ", &" ^ show_term_pair xs ^ ")"

and show_term = function
    Var(x) -> x
  | Func(name, args) -> name ^ "(" ^ show_term_list_without_lending args ^ ")"
  | Form(name, args) -> show_format name ^ "(" ^ show_term_list_without_lending args ^ ")"
  | Tuple(args) -> show_term_pair args
  | Eq(t1, t2) -> show_term t1 ^ " = " ^ show_term t2
  | And(t1, t2) -> show_term t1 ^ " & " ^ show_term t2
  | Or(t1, t2) -> show_term t1 ^ " | " ^ show_term t2
  | Not(t) -> "~" ^ show_term t

and show_format name = name ^enum_func
(* List options: empty, single item, list *)
and show_term_list_without_lending = function
    [] -> ""
  | [x] -> "" ^ show_term x (* In all cases we want to lend the value *)
  | (x::xs) ->  "" ^ show_term x ^ ", " ^ show_term_list_without_lending xs

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

and fresh t =
  "fresh_" ^ show_dtype t

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


and rust_interface (f : (ident * (data_type list * data_type * bool)) list) t =
  let freshTypeFunctions = List.map (fun (typ) -> (fresh typ, ([], typ, false))) t in
  "trait Interface {\n" ^ indent ^ String.concat (";\n" ^indent) (functions (f @ freshTypeFunctions)) ^ ";\n}"

and rust_impl_interface (f : (ident * (data_type list * data_type * bool)) list) t =
  let freshTypeFunctions = List.map (fun (typ) -> (fresh typ, ([], typ, false))) t in
  "impl Interface for "^ interface_impl_name ^" {\n" ^ indent ^ String.concat (" { unimplemented!() }\n" ^indent) (functions (f @ freshTypeFunctions)) ^ "{ unimplemented!() }\n}"

and print_format f =
  match f with
  | (name, data_types) -> "enum " ^  name ^ " { "^ enum ^"(" ^ String.concat ", " (List.map (fun data -> print_type data) data_types) ^ ") }"

and channels acc = function
  Send(sender, receiver, _, _, _, g) when List.exists (fun (a, b) -> sender = a && receiver = b) acc ->
    channels acc g
  | Send(sender, receiver, _, _, _, g) ->
    channels ([(receiver, sender);(sender, receiver)]@acc) g
  | Compute(_, _, g) -> channels acc g
  | DefGlobal(_, _, g, g') -> channels (channels acc g) g'
  | _ -> acc

and output_channel s r = function
  Send(sender, receiver, _, _, t, g) when sender = s && receiver = r ->
    "Send<Repr<" ^ term_as_type t ^ ">, " ^ output_channel s r g ^ ">"
  | Send(sender, receiver, _, _, t, g) when sender = r && receiver = s ->
    "Recv<Repr<" ^ term_as_type t ^">, " ^ output_channel s r g ^ ">"
  | Send(_, _, _, _, _, g) -> output_channel s r g
  | Compute(_, _, g) -> output_channel s r g
  | _ -> "Eps"

and principal_channels principal channels = 
  List.filter (fun (s, _) -> principal = s) channels

and to_local_type global_type participant =
  match global_type with
    Send(sender, receiver, opt, x, t, g) when participant = sender -> LSend(sender ^ receiver, opt, t, to_local_type g participant)
  | Send(sender, receiver, opt, x, t, g) when participant = receiver -> LRecv(receiver ^ sender, opt, PVar(x, None), t, to_local_type g participant)
  | Send(_, _, _, _, _, g) -> to_local_type g participant
  | Compute(p, letb, g) when participant = p -> local_let_bind letb (to_local_type g participant)
  | Compute(p, letb, g) -> (to_local_type g participant)
  | DefGlobal(name, params, g, g') -> to_local_type (unwrapGlobal g' g) participant
  | _ -> LLocalEnd

and rust_equations equations =
  let start = "#[cfg(test)]\nmod tests {\n\tuse super::*;\n" in
  let rec inner equations counter =
    match equations with
    | [] -> ""
    | ((lhs, rhs)::l) -> Printf.sprintf "\t#[test]\n\tfn test_equation_%d() {\n\t\tassert_eq!(%s, %s);\n\t}\n%s" counter (show_term lhs) (show_term rhs) (inner l (counter + 1)) in
  if List.length equations < 1 
    then ""
    else start ^ (inner equations 1) ^ "}"


let rust_output (pr:problem) : unit =
  let knowledge = List.map (fun (p, _) -> p, initial_knowledge p [] pr.knowledge) pr.principals in
  let env = List.map (fun (p, e) -> (p, List.map (fun (i, d, _) -> (i, d)) e)) knowledge in
  Printf.printf "%s\n" (rust_handwritten);
  let channel_pairs = channels [] pr.protocol in
  List.iter (fun (s, r) -> 
      Printf.printf "type %s%s = %s;\n" s r (output_channel s r pr.protocol)) channel_pairs;
  let abstract_types = List.filter_map (function DAType(s1,s2) -> Some(DAType(s1,s2)) | _ -> None) pr.types in
  let concrete_types = List.filter_map (function DType(s1) -> Some(DType(s1)) | _ -> None) pr.types in
  Printf.printf "\n%s\n" (rust_a_types abstract_types);
  Printf.printf "\n%s\n" (rust_types concrete_types);
  Printf.printf "\n%s\n" (rust_formats pr.formats);
  Printf.printf "\n%s\n" (rust_functions pr.functions concrete_types);
  Printf.printf "\n%s\n" (rust_equations pr.equations);
  List.iter (fun (p, b) -> Printf.printf "\n%s\n" (rust_process (principal_channels p channel_pairs) pr.knowledge p (compile env pr.formats pr.functions p pr.protocol))) pr.principals;
