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
    PVar(x, DNone) -> x
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
  | IfAssign(cond, tterm, fterm) -> "( if(" ^ show_term cond ^ ") then " ^ show_term tterm ^ " else " ^ show_term fterm ^ " )"
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

and show_events evs op = String.concat (sprintf " %s " op) (List.map (fun e -> show_event e) evs)

let rec show_query = function
  ReachQuery(events, Conjunction) -> show_events events "&&"
  | ReachQuery(events, Disjunction) -> show_events events "||"
  | CorrQuery(events, next) -> "(" ^ show_events events "&&" ^ " ==> " ^ show_query next ^ ")"

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
    | ReachQuery(events, _) ->
      (List.flatten (List.map (fun event ->
      begin
        match event with
        | NonInjEvent(e, args) | InjEvent(e, args) -> 
          let event_types = List.assoc e event_names_and_types in
          if List.length args <> List.length event_types then raise (SyntaxError(sprintf "Wrong number of arguments passed to %s event" e));
          List.flatten (List.mapi (fun i arg -> inner e arg i []) args)
      end) events))
    | CorrQuery(events, next) ->
      (List.flatten (List.map (fun event -> 
      begin
        match event with
        | NonInjEvent(e, args) | InjEvent(e, args) -> 
          let event_types = List.assoc e event_names_and_types in
          if List.length args <> List.length event_types then raise (SyntaxError(sprintf "Wrong number of arguments passed to %s event" e));
          List.flatten (List.mapi (fun i arg -> inner e arg i []) args)
      end) events))
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
  | ReachQuery(_, _) -> "query " ^ (show_query_params query funcs event_names_and_types function_names_and_types) ^ show_query query
  | CorrQuery(_, _) -> "query " ^ (show_query_params query funcs event_names_and_types function_names_and_types) ^ show_query query

and show_channel parties = function
  Public -> "c"
  | AuthConf -> "c_" ^ parties ^ "_priv"

let rec show_party_channels p acc suffix channels =
  match channels with
  [] -> acc
  | [((sender, receiver), channel)] when p = sender || p = receiver -> (channel ^ suffix)::acc
  | (((sender, receiver), channel)::xs) when p = sender || p = receiver -> show_party_channels p ((channel ^ suffix)::acc) suffix xs
  | [x] -> acc
  | (_::xs) -> show_party_channels p acc suffix xs

and show_local_type p channels prefix = function
    LSend(sender, receiver, opt, t, _, local_type) -> prefix ^ "out(" ^ show_channel (get_channel_name sender receiver) opt ^ ", " ^ show_term t  ^");\n"^ show_local_type p channels prefix local_type
  | LRecv (sender, receiver, opt, pattern, term, local_type) -> prefix ^ "in(" ^ show_channel (get_channel_name sender receiver) opt ^ ", " ^ show_pattern pattern ^ ");\n" ^ show_local_type p channels prefix local_type
  | LNew (ident, data_type, local_type) -> prefix ^ "new " ^ ident ^ ": " ^ show_dtype data_type ^ ";\n" ^ show_local_type p channels prefix local_type
  | LLet (ident, term, local_type) -> prefix ^ "let " ^ show_pattern ident ^ " = " ^ show_term term ^ " in\n" ^ show_local_type p channels prefix local_type
  | LEvent (ident, termlist, local_type) -> prefix ^ "event " ^ ident ^ "(" ^ show_term_list termlist ^ ");\n" ^ show_local_type p channels prefix local_type
  | LChoose(_, _, lb, rb) | LOffer(_, _, lb, rb) ->
      let left = sprintf "%s(\n%s\tlet Left(leftbr) = branchchoice in\n%s\n%s)" prefix prefix (show_local_type p channels (prefix ^ "\t") lb) prefix in
      let right = sprintf "%s(\n%s\tlet Right(rightbr) = branchchoice in\n%s\n%s)" prefix prefix (show_local_type p channels (prefix ^ "\t") rb) prefix in
      sprintf "%sin(c, branchchoice: bitstring);\n%s\n%s|\n%s" prefix left prefix right
  | LIf(cond, thenb, elseb) ->
      let if_then = sprintf "%sif %s then\n%s" prefix (show_term cond) (show_local_type p channels (prefix ^ "\t") thenb) in
      begin
        match elseb with
        | LLocalEnd -> sprintf "%s" if_then
        | _ -> sprintf "%s\n%selse\n%s" if_then prefix (show_local_type p channels (prefix ^ "\t") elseb)
      end
  | LCall(ident, params, _) ->
    sprintf "%s%s%s(%s)" prefix p ident (String.concat ", " ((show_party_channels p [] "" channels)@(List.map (fun (name, _) -> name) params)))
  | LQuit | LLocalEnd -> prefix ^ "0"

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
    if List.length sorted <> 0 then
      (String.concat "\n" (List.map (fun (name, dtype, func) -> 
        match func with
        | Null -> "\tnew " ^ name ^ ": " ^ show_dtype dtype ^ ";"
        | _ -> "\tlet " ^ name ^ " = " ^ show_term func ^ " in"
        ) sorted) ^ "\n\n")
      else ""

and show_party_params = function
  params -> List.map (fun (name, dtype, _) -> name ^ ": " ^ show_dtype dtype) params

and show_event_def = function
  (name, dtypes) -> "event " ^ name ^ "(" ^ String.concat ", " (List.map (fun t -> show_dtype t) dtypes) ^ ")"

and instantiate_party_process_vars party = function
  knowledge -> List.map (fun (i, _, _) -> i) (List.assoc party knowledge)

let rec build_channels acc = function
    Send(sender, receiver, opt, _, _, g) when opt != Public ->
      let channel_name = show_channel (if receiver < sender then receiver ^ sender else sender ^ receiver) opt in
      let parties = (sender, receiver) in
      build_channels ((parties, channel_name)::acc) g
  | Branch(sender, receiver, opt, lb, rb) when opt != Public ->
      let channel_name = show_channel (if receiver < sender then receiver ^ sender else sender ^ receiver) opt in
      let parties = (sender, receiver) in
      (parties, channel_name)::(build_channels (build_channels acc lb) rb)
  | Branch(sender, receiver, opt, lb, rb) ->
      build_channels (build_channels acc lb) rb
  | Send(_, _, _, _, _, g) | Compute(_, _, g) -> build_channels acc g
  | DefGlobal(_, g, g') -> build_channels (build_channels acc g) g'
  | _ -> List.sort_uniq (fun (_, a) (_, b) -> compare a b) acc

and build_event_types = function
  (e, args) -> (e, List.map (fun t -> show_dtype t) args)

let rec find_branch_functions = function
  | LChoose(_, _, lb, rb) | LOffer(_, _, lb, rb) ->
    find_branch_functions lb @ find_branch_functions rb
  | LSend(_, _, _, _, _, next) | LNew (_, _, next) | LLet (_, _, next)
  | LRecv (_, _, _, _, _, next) | LEvent (_, _, next) -> find_branch_functions next
  | LCall(name, params, next) -> find_branch_functions next@[(name, (params, next))]
  | LIf(_, nextthen, nextelse) ->
    begin
      match (nextthen, nextelse) with
      | (LQuit, LQuit) -> []
      | (LQuit, _) -> find_branch_functions nextelse
      | (_, _) -> find_branch_functions nextthen
    end
  | LQuit | LLocalEnd -> []

let rec find_and_print_branch_functions channels l p =
  let branch_functions = find_branch_functions l in
  let bf_uniq = List.rev (List.fold_left (fun acc func -> if List.mem func acc then acc else func::acc) [] branch_functions) in
  String.concat "" (List.map (
    fun (name, (params, next)) ->
      sprintf "let %s%s(%s) =\n%s.\n\n" p name (String.concat ", " ((show_party_channels p [] ": channel" channels)@(List.map (fun (name, dt) -> name ^ ": " ^ show_dtype dt) params))) (show_local_type p channels "\t" next)
    ) bf_uniq)

let proverif (pr:problem): unit =
  let knowledge = List.map (fun (p, _) -> p, initial_knowledge p [] pr.knowledge) pr.principals in
  let env = List.map (fun (p, e) -> (p, List.map (fun (i, d, _) -> (i, d)) e)) knowledge in
  let function_types = List.map (fun f -> build_function_types f) pr.functions in
  let event_types = List.map (fun e -> build_event_types e) pr.events in
  let channels = build_channels [] pr.protocol in
  let channel_inits = 
    if List.length channels <> 0 then
      String.concat "\n" (List.map (fun (_, a) -> "\tnew " ^ a ^ ": channel;") channels) ^ "\n" 
    else 
      "" in
  let locals = List.map (fun (p, _) -> (p, (compile pr.principals "" [] env pr.formats pr.functions pr.events [] p pr.protocol))) pr.principals in
  printf  "(* Protocol: %s *)\n\n" pr.name;
  printf "free c: channel.\n\n%s\n\n" "fun Left(bitstring): bitstring [data].\nfun Right(bitstring): bitstring [data].";
  List.iter (fun t -> 
    printf "type %s.\n" (show_dtype t)) pr.types;
  printf "%s\n" "";
  List.iter (fun f -> 
    printf "%s\n" (show_format f)) pr.formats;
  if List.length pr.formats > 0 then printf "%s\n" "";
  List.iter (fun t -> 
    printf "%s.\n" (show_function t)) pr.functions;
  if List.length pr.functions > 0 then printf "%s\n" "";
  List.iter (fun e -> 
    printf "%s.\n" (show_equation e function_types)) pr.equations;
    if List.length pr.equations > 0 then printf "%s\n" "";
  List.iter (fun e -> 
    printf "%s.\n" (show_event_def e)) pr.events;
  if List.length pr.events > 0 then printf "%s\n" "";
  List.iter (fun q -> 
    printf "%s.\n" (show_query_with_params q pr.functions event_types function_types)) pr.queries;
  if List.length pr.queries > 0 then printf "%s\n" "";
  List.iter (fun (p, plocals) ->
    printf "%s" (find_and_print_branch_functions channels plocals p)) locals;
  List.iter (fun (p, b) -> printf "let %s(%s) = \n%s.\n\n" p (String.concat ", " ((show_party_channels p [] ": channel" channels)@(show_party_params (List.assoc p knowledge)))) (show_local_type p channels "\t" (List.assoc p locals))) pr.principals;
  printf "process (\n%s%s\t%s\n)" channel_inits (show_knowledge knowledge) (String.concat " |\n\t" (List.map (fun (p, _) -> p ^ "(" ^ (String.concat ", " ((show_party_channels p [] "" channels)@(instantiate_party_process_vars p knowledge))) ^ ")") pr.principals))
