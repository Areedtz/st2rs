open Types
open Localtypes
open Typecheck2

exception SyntaxError of string

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
    Send(sender, receiver, opt, _, _, g) when opt != Public ->
      let channel_name = show_channel (if receiver < sender then receiver ^ sender else sender ^ receiver) opt in
      let parties = (sender, receiver) in
      build_channels ((parties, channel_name)::acc) g
  | Send(_, _, _, _, _, g) -> build_channels acc g
  | Compute(_, _, g) -> build_channels acc g
  | DefGlobal(_, _, g, g') -> build_channels (build_channels acc g) g'
  | _ -> List.sort_uniq (fun (_, a) (_, b) -> compare a b) acc

let rec show_party_channels p acc suffix channels =
  match channels with
  [] -> acc
  | [((sender, receiver), channel)] when p = sender || p = receiver -> (channel ^ suffix)::acc
  | (((sender, receiver), channel)::xs) when p = sender || p = receiver -> show_party_channels p ((channel ^ suffix)::acc) suffix xs
  | [x] -> acc
  | (_::xs) -> show_party_channels p acc suffix xs

let get_penv env p =
   match List.assoc_opt p env with
  | Some(penv) -> penv
  | None -> raise (TypeError("Could not find env for party"))

let safe_update p x dt env =
  match List.assoc_opt p env with
  | None -> raise (SyntaxError (Printf.sprintf "Principal %s was not found in environment" p))
  | Some(env_p) -> 
    match List.assoc_opt x env_p with
    | None -> update p ((x, dt)::env_p) env
    | Some(old_dt) -> 
      if old_dt <> dt
        then raise (TypeError (Printf.sprintf "Variable %s was previously assigned as %s. Cannot be reassigned as %s" x (show_dtype old_dt) (show_dtype dt)))
      else env

let rec get_term_type env forms funs = function
  | Var(x) -> 
    begin
      match List.assoc_opt x env with
      | Some(dt) -> dt
      | None -> raise (SyntaxError ("Variable " ^ x ^ " doesn't exist in env"))
    end
  | Func(f, args) -> 
    begin
      match List.assoc_opt f funs with
      | Some(param_types, dt, _, _) -> 
        if List.length args <> List.length param_types
          then 
            raise (SyntaxError (Printf.sprintf "Wrong number of parameters for function %s" f)) 
          else
            List.iteri (fun i arg ->
              let arg_type = get_term_type env forms funs arg in
              let param_type = List.nth param_types i in
              if arg_type <> param_type 
                then 
                  raise (TypeError (Printf.sprintf "Variable type %s doesn't match %s in signature of %s" (show_dtype arg_type) (show_dtype param_type) f))
                else ()
            ) args;
            dt
      | None -> raise (SyntaxError ("Function " ^ f ^ " doesn't exist in funs"))
    end
  | Form(f, args) -> 
    begin
      match List.assoc_opt f forms with
      | Some(param_types) -> 
        if List.length args <> List.length param_types
          then 
            raise (SyntaxError (Printf.sprintf "Wrong number of parameters for format %s" f)) 
          else
            List.iteri (fun i arg ->
              let arg_type = get_term_type env forms funs arg in
              let param_type = List.nth param_types i in
              if arg_type <> param_type 
                then 
                  raise (TypeError (Printf.sprintf "Variable type %s doesn't match %s in signature of %s" (show_dtype arg_type) (show_dtype param_type) f))
                else ()
            ) args;
            DTType(List.map (fun t -> get_term_type env forms funs t) args)
      | None -> raise (SyntaxError ("Form " ^ f ^ " doesn't exist in forms"))
    end
  | Tuple(args) -> DTType(List.map (fun t -> get_term_type env forms funs t) args)
  | Eq(t1, t2) ->
      DType "bool" (* TODO: Consider checking types of t1 and t2 *)
  | And(t1, t2) | Or(t1, t2) ->
      DType "bool"
  | Not(t) ->
      DType "bool" (* TODO: Consider if we need to check env *)
  | If(cond, t1, t2) -> 
      let first = get_term_type env forms funs t1 in
      let second = get_term_type env forms funs t2 in
      if first = second then first
      else raise (TypeError ("t1 and t2 are not of the same type in if-assignment"))
  | Null -> None
      
let rec get_pattern_types env forms funs = function
  | PVar(x, pdt) ->
    begin
      let existing_type = 
        match List.assoc_opt x env with
        | Some(dt) -> dt
        | None -> None in
      match pdt with
      | None -> [(x, existing_type)]
      | _ -> 
        if existing_type <> None && pdt <> existing_type
          then raise (TypeError(Printf.sprintf "Variable %s was matched to type %s, but was previously assigned as %s" x (show_dtype pdt) (show_dtype existing_type)))
          else [(x, pdt)]
    end
  | PMatch(t) ->
    begin
      match t with
      | Var(x) -> [(x, get_term_type env forms funs t)]
      | _ -> raise (SyntaxError(Printf.sprintf "Pattern match only supports variables, not %s" (show_term t)))
    end
  | PForm(f, args) ->
    begin
      match List.assoc_opt f forms with
      | None -> raise (SyntaxError(Printf.sprintf "Format %s was not found in format definitions" f))
      | Some(dtypes) -> 
        let argtypes = List.flatten (List.map (fun arg -> get_pattern_types env forms funs arg) args) in
        let combinetypes = List.combine argtypes dtypes in
        List.map (fun ((x, dt), fdt) ->
          if dt <> None && dt <> fdt
            then raise (TypeError(Printf.sprintf "Variable %s is used as %s, but was previously assigned as %s" x (show_dtype fdt) (show_dtype dt)))
            else (x, fdt)
        ) combinetypes
    end
  | PTuple(args) -> List.flatten (List.map (fun arg -> get_pattern_types env forms funs arg) args)

let dt_unpack dt =
  match dt with
  | DTType(types) -> types
  | _ -> [dt]

let rec compile env forms funs princ gt =
  match gt with
  | Send(s, r, opt, x, t, g) when princ = s ->
    let ttype = get_term_type (get_penv env s) forms funs t in (* also checks if s can send t *)
    let env' = safe_update r x ttype env in
    LSend((if r < s then r ^ s else s ^ r), opt, t, compile env' forms funs princ g)
  | Send(s, r, opt, x, t, g) when princ = r ->
    let ttype = get_term_type (get_penv env s) forms funs t in (* also checks if s can send t *)
    let env' = safe_update r x ttype env in
    
    LRecv((if r < s then r ^ s else s ^ r), opt, PVar(x, ttype), t, compile env' forms funs princ g)
  | Send(s, r, _, x, t, g) ->
    let ttype = get_term_type (get_penv env s) forms funs t in (* also checks if s can send t *)
    let env' = safe_update r x ttype env in
    compile env' forms funs princ g
  | Compute(p, letb, g) ->
    let rec compile_letb inner_env letb =
      match letb with
      | New(name, dt, next) ->
        let inner_env' = safe_update p name dt inner_env in
        if p = princ 
          then LNew(name, dt, compile_letb inner_env' next)
          else compile_letb inner_env' next
      | Let(pattern, term, next) ->
        let ptypes = get_pattern_types (get_penv inner_env p) forms funs pattern in
        let ttype = get_term_type (get_penv inner_env p) forms funs term in
        let inner_env' = if List.length ptypes == 1 then
          if (snd (List.nth ptypes 0)) <> None && (snd (List.nth ptypes 0)) <> ttype then
            raise (TypeError(Printf.sprintf "Mismatching types in left and right hand parts of assignment"))
          else 
            safe_update p (fst (List.nth ptypes 0)) ttype inner_env
        else
          let unpacked = dt_unpack ttype in
          if List.length ptypes == List.length unpacked then
            List.fold_left2 (fun acc ptype unpacktype -> 
              if (snd ptype) <> None && (snd ptype) <> unpacktype then
                raise (TypeError(Printf.sprintf "Mismatching types in left and right hand parts of assignment"))
              else 
                safe_update p (fst ptype) unpacktype acc
            ) inner_env ptypes unpacked
          else
            raise (SyntaxError(Printf.sprintf "Could not match left hand side of assignment to right hand side of assignment, mismatching number of variables")) in
        if p = princ then 
          LLet(pattern, term, compile_letb inner_env' next)
        else 
          compile_letb inner_env' next
      | LetEnd -> compile inner_env forms funs princ g in
    compile_letb env letb
  (*| Branch(s, r, opt, lb, rb, g) when princ = s ->
    let env' = List.filter (fun (p, _) -> p = s || p = r) env in
    LOffer(compile env' princ lb, compile env' princ rb, compile env princ g)
  | Branch(s, r, opt, lb, rb, g) when princ = r ->
    let env' = List.filter (fun (p, _) -> p = s || p = r) env in
    LChoose(compile env' princ lb, compile env' princ rb, compile env princ g)
  | Branch(_, _, _, _, _, g) ->
    compile env princ g*)
  | _ -> LLocalEnd

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