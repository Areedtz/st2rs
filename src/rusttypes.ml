open Types
open Rusttranslator

let abstract_type = "T"
let concrete_type = "/* unimplemented */"
let enum = "I"
let enum_func =  "::" ^ enum

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
  | Null | If(_,_,_) -> ""

and show_format name = name ^ enum_func

(* List options: empty, single item, list *)
and show_term_list_without_lending = function
    [] -> ""
  | [x] -> "" ^ show_term x (* In all cases we want to lend the value *)
  | (x::xs) ->  "" ^ show_term x ^ ", " ^ show_term_list_without_lending xs

and print_type t =
  match t with
  | DType(dtype) -> dtype
  | DAType(wrapper, name) -> wrapper ^ "<" ^ name ^ ">"
  | _ -> ""

and rust_types type_list =
  let types = List.map (fun t -> "type " ^ print_type t ^ " = " ^ concrete_type ^ ";") type_list in
  String.concat "\n" (types)

and rust_a_types type_list =
  let types = List.map (function t -> "#[derive(Serialize, Deserialize)]\npub struct " ^ print_type t ^ "(Vec<u8>, PhantomData<T>);") type_list in
  String.concat "\n" (types)

and channels acc = function
  Send(sender, receiver, _, _, _, g) when List.exists (fun (a, b) -> sender = a && receiver = b) acc ->
    channels acc g
  | Send(sender, receiver, _, _, _, g) ->
    channels ([(receiver, sender); (sender, receiver)]@acc) g
  | Compute(_, _, g) -> channels acc g
  | DefGlobal(_, g, g') -> channels (channels acc g) g'
  | Branch(sender, receiver, _, _, _) when not (List.exists (fun (a, b) -> sender = a && receiver = b) acc) ->
    [(receiver, sender); (sender, receiver)]@acc
  | _ -> acc

and principal_channels principal channels = 
  List.filter (fun (s, _) -> principal = s) channels

let rec show_equation_params = function
    [] -> ""
    | ((name, dtype)::l) -> sprintf "\t\tlet %s = fresh_%s();\n%s" name dtype (show_equation_params l)

and rust_equations function_types equations =
  let start = "#[cfg(test)]\nmod tests {\n\tuse super::*;\n" in
  let inner equation counter =
    match equation with 
    (lhs, rhs) -> 
      let equation_params = build_equation_params lhs rhs function_types in
      sprintf "\t#[test]\n\tfn test_equation_%d() {\n%s\t\tassert_eq!(%s, %s);\n\t}\n" counter (show_equation_params equation_params) (show_term lhs) (show_term rhs) in
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
    | LChoose(sender, receiver, lb, rb, local_type_lb, local_type_rb) when sender = r && receiver = s ->
      let continue_left = build_channel local_type_lb s r continue in
      let continue_right = build_channel local_type_rb s r continue in
      "Choose<" ^ build_channel lb s r continue_left ^ ", " ^ build_channel rb s r continue_right ^ ">"
    | LOffer(sender, receiver, lb, rb, local_type_lb, local_type_rb) when sender = s && receiver = r ->
      let continue_left = build_channel local_type_lb s r continue in
      let continue_right = build_channel local_type_rb s r continue in
      "Offer<" ^ build_channel lb s r continue_left ^ ", " ^ build_channel rb s r continue_right ^ ">"
    | LSend(_, _, _, _, _, local_type) | LRecv(_, _, _, _, _, local_type) | LNew(_, _, local_type) |
        LLet(_, _, local_type) | LEvent(_, _, local_type) | LChoose(_, _, _, _, local_type, _) | LOffer(_, _, _, _, local_type, _) -> (* For non-branching principles, so we can pick either local_type_lb or local_type_rb for LChoose and LOffer *)
      build_channel local_type s r continue
    | LLocalEnd -> "Eps"
    | LCall(_, _) -> continue in
  let rec inner local_types channels = 
    match local_types with
    | LSend(sender, receiver, _, _, _, local_type) | LRecv(receiver, sender, _, _, _, local_type)  -> (* s & r are flipped in LRecv *)
      begin
        match List.assoc_opt (sender ^ receiver) channels with
        | Some(_) -> inner local_type channels
        | None -> inner local_type ((sender ^ receiver, build_channel local_types sender receiver "") :: channels)
      end
    | LOffer(sender, receiver, _, _, local_type_lb, local_type_rb) | LChoose(receiver, sender, _, _, local_type_lb, local_type_rb) -> (* s & r are flipped in LChoose *)
      begin
        match List.assoc_opt (sender ^ receiver) channels with
        | Some(_) -> inner local_type_rb (inner local_type_lb channels)
        | None -> inner local_type_rb ((sender ^ receiver, build_channel local_types sender receiver "") :: (inner local_type_lb channels))
      end
    | LNew(_, _, local_type) | LLet(_, _, local_type) | LEvent(_, _, local_type) ->
      inner local_type channels
    | LLocalEnd | LCall(_, _) -> channels in
  List.fold_left (fun acc (channel_name, channel) -> acc ^ (sprintf "type %s = %s;\n" channel_name channel)) "" (inner principal_locals [])

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
  printf "%s\n" (rust_handwritten);
  let channel_pairs = channels [] pr.protocol in
  let global_funs = build_global_funs_list pr.protocol in
  let principal_locals = List.map (fun (p, _) -> (p, (compile pr.principals false env pr.formats pr.functions pr.events global_funs p pr.protocol))) pr.principals in
  List.iter (fun (p, _) -> 
      printf "%s\n" (output_principal_channels (List.assoc p principal_locals))) pr.principals;
  let abstract_types = List.filter_map (function DAType(s1,s2) -> Some(DAType(s1,s2)) | _ -> None) pr.types in
  let concrete_types = List.filter_map (function DType(s1) -> Some(DType(s1)) | _ -> None) pr.types in
  if List.length abstract_types > 0 then printf "\n%s\n" (rust_a_types abstract_types);
  printf "%s\n" (rust_types concrete_types);
  if List.length pr.formats > 0 then printf "\n%s\n" (rust_formats pr.formats);
  printf "\n%s\n" (rust_functions pr.functions concrete_types);
  printf "\n%s\n" (rust_equations function_types pr.equations);
  List.iter (fun (p, b) -> printf "\n%s\n" (rust_process (principal_channels p channel_pairs) pr.knowledge p (List.assoc p principal_locals))) pr.principals;
  printf "\nfn main() {%s\n" "";
  printf "%s\n" (show_knowledge knowledge);
  List.iteri (fun i (s, r) -> if i mod 2 = 0 then () else printf "\tlet (%s, %s) = session_channel();\n" ("c_" ^ s ^ r) ("c_" ^ r ^ s)) channel_pairs;
  if List.length channel_pairs > 0 then printf "%s\n" (rust_a_types abstract_types);
  List.iter (fun (p, _) ->
    printf "\tlet %s_t = thread::spawn(move || %s(%s));\n" (String.lowercase_ascii p) (String.lowercase_ascii p) (String.concat ", " ((List.map (fun (s, r) -> "c_" ^ s ^ r) (List.rev (principal_channels p channel_pairs)))@(show_principal_knowledge p pr.knowledge)))) pr.principals;
  printf "\tlet _ = (%s);\n" (String.concat ", " (List.map (fun (p, _) -> (String.lowercase_ascii p) ^ "_t.join()") pr.principals));
  printf "}%s" "";
