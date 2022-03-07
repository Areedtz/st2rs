open Types
open Localtypes

let rec show_params = function
  [] -> ""
  | [t] -> "" ^ (show_dtype t)
  | (t::ts) -> "" ^ (show_dtype t) ^ ", " ^ show_params ts

and show_function = function
  (f, (args_t, ret, _, _)) -> "fun " ^ f ^ "(" ^ (show_params args_t) ^ "): " ^ (show_dtype ret)

and build_function = function
    (f, (args_t, _, _, _)) -> (f, List.map (fun t -> show_dtype t) args_t)

and show_dtype t =
  match t with
  | DType(dtype) -> dtype
  | DAType(at, dt) -> at ^ "<" ^ dt ^">"
  | DTType(_) | DFType(_) -> "bitstring"
  | _ -> ""

and show_pattern = function
    PVar(x, None) -> x
  | PVar(x, dt) -> x ^ ": " ^ show_dtype dt
  | PForm(name, args) -> name ^ "(" ^ show_pattern_list args ^ ")"
  | PTuple(args) -> "(" ^ show_pattern_list args ^ ")"
  | PMatch(t) -> "=" ^ show_term t

let rec show_term = function
    Var(x) -> x
  | Func(name, args) -> name ^ "(" ^ show_term_list args ^ ")"
  | Form(name, args) -> name ^ "(" ^ show_term_list args ^ ")"
  | Tuple(args) -> "(" ^ show_term_list args ^ ")"
  | Eq(t1, t2) -> show_term t1 ^ " = " ^ show_term t2
  | And(t1, t2) -> show_term t1 ^ " && " ^ show_term t2
  | Or(t1, t2) -> show_term t1 ^ " || " ^ show_term t2
  | Not(t) -> "not(" ^ show_term t ^ ")"
  | If(cond, tterm, fterm) -> "( if(" ^ show_term cond ^ ") then " ^ show_term tterm ^ " else " ^ show_term fterm ^ " )"
  | Null -> ""

and show_term_list = function
  [] -> ""
| [x] -> show_term x
| (x::xs) -> show_term x ^ ", " ^ show_term_list xs

let rec build_equation_params t pos function_types names_and_types = (* [(var name, type)...] *)
  match (t, function_types) with
  | (Var(_), []) -> []
  | (Var(x), _) -> [(x, List.nth function_types pos)]
  | (Func(name, args), _) -> List.flatten (List.mapi (fun i arg -> build_equation_params arg i (List.assoc name names_and_types) names_and_types) args)
  | _ -> []

and show_equation_params equation names_and_types = 
  match equation with
  | (t1, t2) -> String.concat ", " (
      List.map (fun (a,b) -> a ^ ": " ^ b) 
        (List.sort_uniq (fun (a,_) (c,_) -> compare a c) (List.merge (fun _ _ -> 0) (build_equation_params t1 0 [] names_and_types) (build_equation_params t2 0 [] names_and_types))))

and show_equation equation names_and_types =
  match equation with
  | (t1, t2) -> "equation forall " ^ (show_equation_params equation names_and_types) ^ ";\n\t" ^ (show_term t1) ^ " = " ^ (show_term t2)

and show_channel parties = function
  Public -> "c"
  | Auth -> "c_" ^ parties ^ "_auth"
  | Conf -> "c_" ^ parties ^ "_conf"
  | AuthConf -> "c_" ^ parties ^ "_authconf"

and show_local_type = function
    LSend(ident, opt, t, local_type) -> "\tout(" ^ show_channel ident opt ^ ", " ^ show_term t  ^");\n"^ show_local_type local_type
  | LNew (ident, data_type, local_type) -> "\tnew " ^ ident ^ ": " ^ show_dtype data_type ^ ";\n" ^ show_local_type local_type
  | LLet (ident, term, local_type) -> "\tlet " ^ show_pattern ident ^ " = " ^ show_term term ^ " in\n" ^ show_local_type local_type
  | LRecv (ident, opt, pattern, term, local_type) -> "\tin(" ^ show_channel ident opt ^ ", " ^ show_pattern pattern ^ ");\n" ^ show_local_type local_type
  | LEvent (ident, termlist, local_type) -> "\tevent " ^ ident ^ "(" ^ show_term_list termlist ^ ");\n" ^ show_local_type local_type
  | LChoose(lb, rb, local_type) -> "\tChoose:\n\t\tLeft:\n\t\t" ^ show_local_type lb ^ "\n\t\tRight:\n\t\t" ^ show_local_type rb ^ "\n" ^ show_local_type local_type
  | LOffer(lb, rb, local_type) -> "\tOffer:\n\t\tLeft:\n\t\t" ^ show_local_type lb ^ "\n\t\tRight:\n\t\t" ^ show_local_type rb ^ "\n" ^ show_local_type local_type
  | LBranchEnd -> "\tBranchEnd"
  | LLocalEnd -> "\t0."

and show_format = function
  (name, types) -> "fun " ^ name ^ "(" ^ (String.concat ", " (List.map (fun t -> show_dtype t) types)) ^ "): bitstring [data]."

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
      | Null -> "\tnew " ^ name ^ ": " ^ show_dtype dtype ^ ";"
      | _ -> "\tlet " ^ name ^ " = " ^ show_term func ^ " in"
      ) sorted) ^ "\n")

and show_party_params = function
  params -> List.map (fun (name, dtype, _) -> name ^ ": " ^ show_dtype dtype) params

and instantiate_party_process_vars party = function
  knowledge -> List.map (fun (i, _, _) -> i) (List.assoc party knowledge)

let rec build_channels acc = function
    Send(sender, receiver, opt, _, _, g) | Branch(sender, receiver, opt, _, _, g) when opt != Public ->
      let channel_name = show_channel (if receiver < sender then receiver ^ sender else sender ^ receiver) opt in
      let parties = (sender, receiver) in
      build_channels ((parties, channel_name)::acc) g
  | Send(_, _, _, _, _, g) | Branch(_, _, _, _, _, g) | Compute(_, _, g) -> build_channels acc g
  | DefGlobal(_, _, g, g') -> build_channels (build_channels acc g) g'
  | _ -> List.sort_uniq (fun (_, a) (_, b) -> compare a b) acc

let rec show_party_channels p acc suffix channels =
  match channels with
  [] -> acc
  | [((sender, receiver), channel)] when p = sender || p = receiver -> (channel ^ suffix)::acc
  | (((sender, receiver), channel)::xs) when p = sender || p = receiver -> show_party_channels p ((channel ^ suffix)::acc) suffix xs
  | [x] -> acc
  | (_::xs) -> show_party_channels p acc suffix xs

let proverif (pr:problem): unit =
  let knowledge = List.map (fun (p, _) -> p, initial_knowledge p [] pr.knowledge) pr.principals in
  let env = List.map (fun (p, e) -> (p, List.map (fun (i, d, _) -> (i, d)) e)) knowledge in
  let function_types = List.map (fun f -> build_function f) pr.functions in
  let channels = build_channels [] pr.protocol in
  let channel_inits = String.concat "\n" (List.map (fun (_, a) -> "\tnew " ^ a ^ ": channel;") channels) in
  Printf.printf  "(* Protocol: %s *)\n\n" pr.name;
  Printf.printf "free c: channel.%s\n\n" "";
  List.iter (fun t -> 
    Printf.printf "type %s.\n" (show_dtype t)) pr.types;
  Printf.printf "%s\n" "";
  List.iter (fun f -> 
    Printf.printf "%s\n" (show_format f)) pr.formats;
  Printf.printf "%s\n" "";
  List.iter (fun t -> 
    Printf.printf "%s.\n" (show_function t)) pr.functions;
  Printf.printf "%s\n" "";
  List.iter (fun e -> 
    Printf.printf "%s.\n" (show_equation e function_types)) pr.equations;
  Printf.printf "%s\n" "";
  List.iter (fun (p, b) -> Printf.printf "let %s(%s) = \n%s\n\n" p (String.concat ", " ((show_party_channels p [] ": channel" channels)@(show_party_params (List.assoc p knowledge)))) (show_local_type (compile env pr.formats pr.functions p pr.protocol))) pr.principals;
  Printf.printf "process (\n%s\n%s\n\t%s\n)" channel_inits (show_knowledge knowledge) (String.concat " |\n\t" (List.map (fun (p, _) -> p ^ "(" ^ (String.concat ", " ((show_party_channels p [] "" channels)@(instantiate_party_process_vars p knowledge))) ^ ")") pr.principals))
