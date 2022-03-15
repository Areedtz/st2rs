open Types
open Localtypes
open Rusttranslator

let abstract_type = "T"
let concrete_type = "/* unimplemented */"
let indent = "    "
let enum = "I"
let enum_func =  "::"^enum

let rec show_term_pair = function
    [] -> ""
  | (x::[]) -> show_term x
  | (x::xs) ->  show_term x ^ ", " ^ show_term_pair xs

and show_term = function
    Var(x) -> x
  | Func(name, args) -> name ^ "(" ^ show_term_list_without_lending args ^ ")"
  | Form(name, args) -> show_format name ^ "(" ^ show_term_list_without_lending args ^ ")"
  | Tuple(args) -> "(" ^ show_term_pair args ^ ")"
  | Eq(t1, t2) -> show_term t1 ^ " = " ^ show_term t2
  | And(t1, t2) -> show_term t1 ^ " & " ^ show_term t2
  | Or(t1, t2) -> show_term t1 ^ " | " ^ show_term t2
  | Not(t) -> "~" ^ show_term t

and show_format name = name ^ enum_func

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
    

and channels acc = function
  Send(sender, receiver, _, _, _, g) | Branch(sender, receiver, _, _, _, g) when List.exists (fun (a, b) -> sender = a && receiver = b) acc ->
    channels acc g
  | Send(sender, receiver, _, _, _, g) | Branch(sender, receiver, _, _, _, g) ->
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

let rec show_equation_params = function
    [] -> ""
    | ((name, dtype)::l) -> Printf.sprintf "\t\tlet %s = fresh_%s();\n%s" name dtype (show_equation_params l)

and rust_equations function_types equations =
  let start = "#[cfg(test)]\nmod tests {\n\tuse super::*;\n" in
  let inner equation counter =
    match equation with 
    (lhs, rhs) -> 
      let equation_params = build_equation_params lhs rhs function_types in
      Printf.sprintf "\t#[test]\n\tfn test_equation_%d() {\n%s\t\tassert_eq!(%s, %s);\n\t}\n" counter (show_equation_params equation_params) (show_term lhs) (show_term rhs) in
  if List.length equations < 1 
    then ""
    else start ^ (List.fold_left (fun acc equation -> acc ^ equation) "" (List.mapi (fun i equation -> inner equation i) equations)) ^ "}"

let output_principal_channels principal_locals =
  let rec build_channel local_types s r continue = 
    match local_types with
    | LSend(sender, receiver, _, _, dt, local_type) when sender = s && receiver = r ->
      "Send<Repr<" ^ show_dtype dt ^ ">, " ^ build_channel local_type s r continue ^ ">"
    | LRecv(sender, receiver, _, pattern, _, local_type) when sender = r && receiver = s ->
      begin
        match pattern with
        | PVar(_, dt) -> "Recv<Repr<" ^ show_dtype dt ^ ">, " ^ build_channel local_type s r continue ^ ">"
        | _ -> raise (TypeError ("LRecv pattern should always be a PVar"))
      end
    | LChoose(sender, receiver, lb, rb, _, local_type) when sender = r && receiver = s ->
      let continue_type = build_channel local_type s r continue in
      "Choose<" ^ build_channel lb s r continue_type ^ ", " ^ build_channel rb s r continue_type ^ ">"
    | LOffer(sender, receiver, lb, rb, _, local_type) when sender = s && receiver = r ->
      let continue_type = build_channel local_type s r continue in
      "Offer<" ^ build_channel lb s r continue_type ^ ", " ^ build_channel rb s r continue_type ^ ">"
    | LSend(_, _, _, _, _, local_type) | LRecv(_, _, _, _, _, local_type) | LNew(_, _, local_type) |
        LLet(_, _, local_type) | LEvent(_, _, local_type) | LChoose(_, _, _, _, _, local_type) | LOffer(_, _, _, _, _, local_type) ->
      build_channel local_type s r continue
    | LBranchEnd -> continue
    | LLocalEnd -> "Eps" in
  let rec inner local_types channels = 
    match local_types with
    | LSend(sender, receiver, _, _, _, local_type) | LOffer(sender, receiver, _, _, _, local_type) ->
      begin
        match List.assoc_opt (sender ^ receiver) channels with
        | Some(_) -> inner local_type channels
        | None -> inner local_type ((sender ^ receiver, build_channel local_types sender receiver "") :: channels)
      end
    | LRecv(sender, receiver, _, _, _, local_type) | LChoose(sender, receiver, _, _, _, local_type) ->
      begin
        match List.assoc_opt (receiver ^ sender) channels with
        | Some(_) -> inner local_type channels
        | None -> inner local_type ((receiver ^ sender, build_channel local_types receiver sender "") :: channels)
      end
    | LNew(_, _, local_type) | LLet(_, _, local_type) | LEvent(_, _, local_type) ->
      inner local_type channels
    | LLocalEnd -> channels in
  List.fold_left (fun acc (channel_name, channel) -> acc ^ (Printf.sprintf "type %s = %s;\n" channel_name channel)) "" (inner principal_locals [])

and show_knowledge = function
  knowledge ->
      let vars = List.flatten (List.map (fun (_, e) -> e) knowledge) in
      let uniq_vars = List.sort_uniq (fun (a,_,_) (c,_,_) -> compare a c) vars in
      let sorted = List.sort (fun (_,_,f) (_,_,f') -> 
        match f with 
        | Null -> -1
        | _ -> 1
        ) uniq_vars in
      (String.concat "\n" (List.map (fun (name, dtype, func) -> 
        match func with
        | Null -> "\tlet " ^ name ^ " = fresh_" ^ show_dtype dtype ^ "();"
        | _ -> "\tlet " ^ name ^ " = " ^ show_term func ^ ";"
        ) sorted) ^ "\n")
  
let rec show_principal_knowledge principal knowledge =
  List.map (fun (t, _, _, _) -> t) (List.filter (fun (_, _, p, _) -> principal = p) knowledge)

let rust_output (pr:problem) : unit =
  let knowledge = List.map (fun (p, _) -> p, initial_knowledge p [] pr.knowledge) pr.principals in
  let env = List.map (fun (p, e) -> (p, List.map (fun (i, d, _) -> (i, d)) e)) knowledge in
  let function_types = List.map (fun f -> build_function_types f) pr.functions in
  Printf.printf "%s\n" (rust_handwritten);
  let channel_pairs = channels [] pr.protocol in
  let principal_locals = List.map (fun (p, _) -> (p, (compile env pr.formats pr.functions pr.events p pr.protocol))) pr.principals in
  List.iter (fun (p, _) -> 
      Printf.printf "%s\n" (output_principal_channels (List.assoc p principal_locals))) pr.principals;
  let abstract_types = List.filter_map (function DAType(s1,s2) -> Some(DAType(s1,s2)) | _ -> None) pr.types in
  let concrete_types = List.filter_map (function DType(s1) -> Some(DType(s1)) | _ -> None) pr.types in
  Printf.printf "\n%s\n" (rust_a_types abstract_types);
  Printf.printf "\n%s\n" (rust_types concrete_types);
  Printf.printf "\n%s\n" (rust_formats pr.formats);
  Printf.printf "\n%s\n" (rust_functions pr.functions concrete_types);
  Printf.printf "\n%s\n" (rust_equations function_types pr.equations);
  List.iter (fun (p, b) -> Printf.printf "\n%s\n" (rust_process (principal_channels p channel_pairs) pr.knowledge p (List.assoc p principal_locals))) pr.principals;

  Printf.printf "\nfn main() {%s\n" "";
  Printf.printf "%s\n" (show_knowledge knowledge);
  List.iteri (fun i (s, r) -> if i mod 2 = 0 then () else Printf.printf "\tlet (%s, %s) = session_channel();\n" ("c_" ^ s ^ r) ("c_" ^ r ^ s)) channel_pairs;
  List.iter (fun (p, _) ->
    Printf.printf "\tlet %s_t = thread::spawn(move || %s(%s));\n" (String.lowercase p) (String.lowercase p) (String.concat ", " ((List.map (fun (s, r) -> "c_" ^ s ^ r) (List.rev (principal_channels p channel_pairs)))@(show_principal_knowledge p pr.knowledge)))) pr.principals;
  Printf.printf "\tlet _ = (%s);\n" (String.concat ", " (List.map (fun (p, _) -> (String.lowercase p) ^ "_t.join()") pr.principals));
  Printf.printf "}%s" "";
