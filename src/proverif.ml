open Types

let rec show_params = function
  [] -> ""
  | [t] -> "" ^ (show_dtype t)
  | (t::ts) -> "" ^ (show_dtype t) ^ ", " ^ show_params ts

and show_function = function
  (f, (args_t, ret, _, _)) -> "fun " ^ f ^ "(" ^ (show_params args_t) ^ "): " ^ (show_dtype ret)

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

and show_pattern_list = function
    [] -> ""
  | [x] -> show_pattern x
  | (x::xs) -> show_pattern x ^ ", " ^ show_pattern_list xs

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

and show_equation_params equation names_and_types = 
  match equation with
  | (t1, t2) -> String.concat ", " (
      List.map (fun (a,b) -> a ^ ": " ^ b) 
        (build_equation_params t1 t2 names_and_types))

and show_equation equation names_and_types =
  match equation with
  | (t1, t2) -> "equation forall " ^ (show_equation_params equation names_and_types) ^ ";\n\t" ^ (show_term t1) ^ " = " ^ (show_term t2)

and show_event = function
  NonInjEvent(id, args) -> "event(" ^ id ^ "(" ^ show_term_list args ^ "))"
  | InjEvent(id, args) -> "inj-event(" ^ id ^ "(" ^ show_term_list args ^ "))"

let rec show_query = function
  ReachQuery(event) -> show_event event
  | CorrQuery(event, next) -> "(" ^ show_event event ^ " ==> " ^ show_query next ^ ")"

let rec build_query_params query funcs event_names_and_types function_names_and_types = (* [(var name, type)...] *)
  let rec inner e t pos function_types =
    match (t, function_types) with
    | (Var(x), []) -> 
      let types = List.assoc e event_names_and_types in
      if pos >= List.length types then raise (SyntaxError(sprintf "Too many arguments passed to %s event" e));
      [(x, List.nth types pos)]
    | (Var(x), _) -> 
      if pos >= List.length function_types then raise (SyntaxError(sprintf "Too many arguments passed to function in event %s" e));
      [(x, List.nth function_types pos)]
    | (Func(name, args), []) -> 
      let function_type = 
        begin
          match List.assoc_opt name funcs with
          | Some(_, dt, _, _) -> show_dtype dt
          | None -> raise (SyntaxError(sprintf "Function %s is not defined" name))
        end in
      let types = List.assoc e event_names_and_types in
      if pos >= List.length types then raise (SyntaxError(sprintf "Too many arguments passed to %s event" e));
      let event_param_type = List.nth types pos in
      if function_type <> event_param_type then raise (TypeError(sprintf "Function type %s doesn't match type %s needed for event" function_type event_param_type));
      List.flatten (List.mapi (fun i arg -> inner e arg i (List.assoc name function_names_and_types)) args)
    | (Func(name, args), _) -> 
      List.flatten (List.mapi (fun i arg -> inner e arg i (List.assoc name function_names_and_types)) args)
    | _ -> [] in
  let params = 
    match query with
    | ReachQuery(event) -> 
      begin
        match event with
        | NonInjEvent(e, args) | InjEvent(e, args) -> 
          let event_types = List.assoc e event_names_and_types in
          if List.length args <> List.length event_types then raise (SyntaxError(sprintf "Wrong number of arguments passed to %s event" e));
          List.flatten (List.mapi (fun i arg -> inner e arg i []) args)
      end
    | CorrQuery(event, next) ->
      begin
        match event with
        | NonInjEvent(e, args) | InjEvent(e, args) -> 
          let event_types = List.assoc e event_names_and_types in
          if List.length args <> List.length event_types then raise (SyntaxError(sprintf "Wrong number of arguments passed to %s event" e));
          List.flatten (List.mapi (fun i arg -> inner e arg i []) args)
      end
      @ build_query_params next funcs event_names_and_types function_names_and_types in
  List.sort_uniq (fun (a, _) (c, _) -> compare a c) params

and show_query_params query funcs event_names_and_types function_names_and_types =
  let params = build_query_params query funcs event_names_and_types function_names_and_types in
  if List.length params > 0 then
    String.concat ", " (
        List.map (fun (a,b) -> a ^ ": " ^ b) params) ^ ";\n\t"
  else ""

and show_query_with_params query funcs event_names_and_types function_names_and_types =
  match query with
  | ReachQuery(event) -> "query " ^ (show_query_params query funcs event_names_and_types function_names_and_types) ^ show_event event
  | CorrQuery(event, next) -> "query " ^ (show_query_params query funcs event_names_and_types function_names_and_types) ^ show_query query

and show_channel parties = function
  Public -> "c"
  | Auth -> "c_" ^ parties ^ "_auth"
  | Conf -> "c_" ^ parties ^ "_conf"
  | AuthConf -> "c_" ^ parties ^ "_authconf"

let rec show_party_channels p acc suffix channels =
  match channels with
  [] -> acc
  | [((sender, receiver), channel)] when p = sender || p = receiver -> (channel ^ suffix)::acc
  | (((sender, receiver), channel)::xs) when p = sender || p = receiver -> show_party_channels p ((channel ^ suffix)::acc) suffix xs
  | [x] -> acc
  | (_::xs) -> show_party_channels p acc suffix xs

and show_local_type p bnr channels prefix continue = function
    LSend(sender, receiver, opt, t, _, local_type) -> prefix ^ "out(" ^ show_channel (get_channel_name sender receiver) opt ^ ", " ^ show_term t  ^");\n"^ show_local_type p bnr channels prefix continue local_type
  | LRecv (sender, receiver, opt, pattern, term, local_type) -> prefix ^ "in(" ^ show_channel (get_channel_name sender receiver) opt ^ ", " ^ show_pattern pattern ^ ");\n" ^ show_local_type p bnr channels prefix continue local_type
  | LNew (ident, data_type, local_type) -> prefix ^ "new " ^ ident ^ ": " ^ show_dtype data_type ^ ";\n" ^ show_local_type p bnr channels prefix continue local_type
  | LLet (ident, term, local_type) -> prefix ^ "let " ^ show_pattern ident ^ " = " ^ show_term term ^ " in\n" ^ show_local_type p bnr channels prefix continue local_type
  | LEvent (ident, termlist, local_type) -> prefix ^ "event " ^ ident ^ "(" ^ show_term_list termlist ^ ");\n" ^ show_local_type p bnr channels prefix continue local_type
  | LChoose(_, _, lb, rb, penv, local_type) | LOffer(_, _, lb, rb, penv, local_type) -> 
      let branch_call = sprintf "%sB%d(%s)" p (bnr+1) (String.concat ", " ((show_party_channels p [] "" channels)@(List.map (fun (name, _) -> name) penv))) in
      let left = sprintf "%s(\n%s\tlet Left(leftbr) = branchchoice in\n%s\n%s)" prefix prefix (show_local_type p bnr channels (prefix ^ "\t") branch_call lb) prefix in
      let right = sprintf "%s(\n%s\tlet Right(rightbr) = branchchoice in\n%s\n%s)" prefix prefix (show_local_type p bnr channels (prefix ^ "\t") branch_call rb) prefix in
      sprintf "%sin(c, branchchoice: bitstring);\n%s\n%s|\n%s" prefix left prefix right
  | LLocalEnd -> prefix ^ "0"
  | LBranchEnd -> prefix ^ continue

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

and show_event_def = function
  (name, dtypes) -> "event " ^ name ^ "(" ^ String.concat ", " (List.map (fun t -> show_dtype t) dtypes) ^ ")"

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

and build_event_types = function
  (e, args) -> (e, List.map (fun t -> show_dtype t) args)

let rec find_and_print_branch_functions bnr channels l p =
  match l with
  | LChoose(_, _, lb, rb, penv, next) | LOffer(_, _, lb, rb, penv, next) ->
    sprintf "%slet %sB%d(%s) =\n%s.\n\n" (find_and_print_branch_functions (bnr+1) channels next p) p bnr (String.concat ", " ((show_party_channels p [] ": channel" channels)@(List.map (fun (name, dt) -> name ^ ": " ^ show_dtype dt) penv))) (show_local_type p bnr channels "\t" "" next)
  | LSend(_, _, _, _, _, next) | LNew (_, _, next) | LLet (_, _, next)
  | LRecv (_, _, _, _, _, next) | LEvent (_, _, next) -> find_and_print_branch_functions bnr channels next p
  | LBranchEnd | LLocalEnd -> ""

let proverif (pr:problem): unit =
  let knowledge = List.map (fun (p, _) -> p, initial_knowledge p [] pr.knowledge) pr.principals in
  let env = List.map (fun (p, e) -> (p, List.map (fun (i, d, _) -> (i, d)) e)) knowledge in
  let function_types = List.map (fun f -> build_function_types f) pr.functions in
  let event_types = List.map (fun e -> build_event_types e) pr.events in
  let channels = build_channels [] pr.protocol in
  let channel_inits = String.concat "\n" (List.map (fun (_, a) -> "\tnew " ^ a ^ ": channel;") channels) in
  let locals = List.map (fun (p, _) -> (p, (compile env pr.formats pr.functions pr.events p pr.protocol))) pr.principals in
  printf  "(* Protocol: %s *)\n\n" pr.name;
  printf "free c: channel.\n\n%s\n\n" "fun Left(bitstring): bitstring [data].\nfun Right(bitstring): bitstring [data].";
  List.iter (fun t -> 
    printf "type %s.\n" (show_dtype t)) pr.types;
  printf "%s\n" "";
  List.iter (fun f -> 
    printf "%s\n" (show_format f)) pr.formats;
  printf "%s\n" "";
  List.iter (fun t -> 
    printf "%s.\n" (show_function t)) pr.functions;
  printf "%s\n" "";
  List.iter (fun e -> 
    printf "%s.\n" (show_equation e function_types)) pr.equations;
  printf "%s\n" "";
  List.iter (fun e -> 
    printf "%s.\n" (show_event_def e)) pr.events;
  if List.length pr.events > 0 then printf "%s\n" "";
  List.iter (fun q -> 
    printf "%s.\n" (show_query_with_params q pr.functions event_types function_types)) pr.queries;
  if List.length pr.queries > 0 then printf "%s\n" "";
  List.iter (fun (p, plocals) ->
    printf "%s" (find_and_print_branch_functions 1 channels plocals p)) locals;
  List.iter (fun (p, b) -> printf "let %s(%s) = \n%s.\n\n" p (String.concat ", " ((show_party_channels p [] ": channel" channels)@(show_party_params (List.assoc p knowledge)))) (show_local_type p 0 channels "\t" "" (List.assoc p locals))) pr.principals;
  printf "process (\n%s\n%s\n\t%s\n)" channel_inits (show_knowledge knowledge) (String.concat " |\n\t" (List.map (fun (p, _) -> p ^ "(" ^ (String.concat ", " ((show_party_channels p [] "" channels)@(instantiate_party_process_vars p knowledge))) ^ ")") pr.principals))
