open Types

exception SyntaxError of string

let rec add_params_to_env env = function
    [] -> env
  | (param, princ)::params ->
    begin
      match List.assoc_opt princ env with
      | Some princ_env -> add_params_to_env (update princ (param::princ_env) env) params
      | None -> raise (SyntaxError ("Principal " ^ princ ^ " not defined"))
    end

(* Checks if term: exists, check_func *)
let rec get_term_type env funs = function
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
              let arg_type = get_term_type env funs arg in
              let param_type = List.nth param_types i in
              if arg_type <> param_type 
                then 
                  raise (TypeError (Printf.sprintf "Variable type %s doesn't match %s in signature of %s" (show_dtype arg_type) (show_dtype param_type) f))
                else ()
            ) args;
            dt
      | None -> raise (SyntaxError ("Function " ^ f ^ " doesn't exist in funs"))
    end
  | Form(f, args) -> DFType(f)
  | Tuple(l) ->
      DTType(List.map (fun t -> get_term_type env funs t) l) (* recursively gets terms with their env and funcs *)
  | Eq(t1, t2) ->
      DType "bool" (* TODO: Consider checking types of t1 and t2 *)
  | And(t1, t2) | Or(t1, t2) ->
      DType "bool"
  | Not(t) ->
      DType "bool" (* TODO: Consider if we need to check env *)
  | If(cond, t1, t2) -> 
      let first = get_term_type env funs t1 in
      let second = get_term_type env funs t2 in
      if first = second then first
      else raise (TypeError ("t1 and t2 are not of the same type in if-assignment"))
  | Null -> None

(* Checks if pattern: is not pre-defined, check_term, check_func, return list of errors *)
let rec get_pattern_type env funs = function
  | PVar(x, pdt) ->
    begin
      match pdt with
      | None -> 
        begin
          match List.assoc_opt x env with
          | Some(dt) -> dt
          | None -> raise (TypeError ("Type missing for variable " ^ x))
        end
      | _ -> pdt
    end
  | PMatch(t) ->
      get_term_type env funs t
  | PForm(f, args) -> DFType f (* TODO Format typechecking *) 
  | PTuple(l) -> DTType(List.map (fun p -> get_pattern_type env funs p) l)


let rec typecheck2 (pr:problem): tenv = 
  let p' = ("Dishonest", false)::pr.principals in
  let e = List.map (fun (p, x) -> p, (List.map (fun (var, dt, _) -> (var, dt)) (initial_knowledge p [] pr.knowledge))) pr.principals in (* (ident, (var name, data type))*)
  let env = check pr.protocol e [] pr.functions in 
  (*List.iter (fun (p, l) -> 
    Printf.printf "%s:\n" p; 
    List.iter (fun (id, dt) -> 
      Printf.printf "\t(%s: %s)\n" id (show_dtype dt)) l; 
    Printf.printf "%s\n" "") env;*)
  env
(* Checks global types, return list of errors *)
and check
  (g : global_type)                             (* Global type *)
  (env : tenv)                                  (* each princ. with their known var, as a list *)
  (def : (ident * (((ident * data_type) * principal) list * global_type)) list)  (* function name, it's env and the global type *)
  (funs : (ident * (data_type list * data_type * bool * data_type list)) list)          (* function name, argument types, return data type, is data function, generic types *)
  : tenv                (* error messages and where in code *)
   =
(* checks send: if p and q exist, if t is well-formed, updates env of q with x *)
match g with
| Send(p, q, _, n, t, g') ->
begin
  match List.assoc_opt p env with                  (* returns ident list of p in env *)
  | None -> raise (TypeError ("Principal " ^ p ^ " not defined"))
  | Some(env_p) ->
    match List.assoc_opt q env with
    | None -> raise (TypeError ("Principal " ^ q ^ " not defined"))
    | Some(env_q) ->
      let env' = update q ((n, get_term_type env_p funs t)::env_q) env in (* (principal, (ident, type) list) list *)
      check g' env' def funs
end

(* checks branch: if p and q exist, if t is well-formed, recursively check patterns *)
| Branch(p, q, _, t, args) ->
  begin
    match List.assoc_opt p env with
    | None -> raise (TypeError ("Principal " ^ p ^ " not defined"))
    | Some(env_p) ->
      match List.assoc_opt q env with
      | None -> raise (TypeError ("Principal " ^ q ^ " not defined"))
      | Some(env_q) ->
        raise (TypeError ("branches are currently not supported in TypeCheck2"))
        (*List.concat (List.map (
          fun (p, g) -> check g env def funs @ List.map (fun e -> (e, g)) (get_pattern_type env_q funs p)   (* pattern and global type *)
          ) args)*) (* TODO: Look into this later *)
  end

(* Checks let-binding: update env of participant *)
| Compute(p, lb, g') ->
begin
  match List.assoc_opt p env with
  | None -> raise (TypeError ("Principal " ^ p ^ " not defined"))
  | Some(env_p) ->
    let rec let_bind env_p env = function
      | New(x, data_type, lb) -> (* TODO Implement check for data type *)
        let env_p' = ((x, data_type)::env_p) in
        let_bind env_p' (update p env_p' env) lb
      | Let(pat, t, lb) ->
        let env_p' = binds env_p funs t pat @ env_p in
        let_bind env_p' (update p env_p' env) lb
      | Event(name, ts, lb) -> let_bind env_p env lb
      | LetEnd -> check g' env def funs
    in let_bind env_p env lb
end

(* Checks function definition: *)
| DefGlobal(f, params, g', g'') ->
  let def' = ((f, (params, g'))::def) in
  let env' = add_params_to_env env params in
  raise (TypeError ("DefGlobal is currently not supported in TypeCheck2"))
  (* (check g' env_param def' funs) @ (* obs recursion on def' *)
                       (check g'' env def' funs) *) (* TODO: Look into this later *)

(* Checks function calls *)
| CallGlobal(f, args) ->
  begin
    match List.assoc_opt f def with
      | None -> raise (TypeError ("Function " ^ f ^ " not declared"))
      | Some(params, g) ->
        if List.length args <> List.length params then
          raise (TypeError ("Arguments and parameter lengths do not match"))
        else
          env
  end

| GlobalEnd -> env