open Types

type resultOrError =
    Result of tenv
  | Error of string list

let rec add_params_to_env env = function
    [] -> Result env
  | (x, p)::params ->
    begin
      match List.assoc_opt p env with
      | Some p_env -> add_params_to_env (update p (x::p_env) env) params
      | None -> Error(["Principal " ^ p ^ " not defined"])
    end

(* Checks if function: exist, right number of args, if data func, return list of errors *)
let check_func f args is_pattern funs =
  match List.assoc_opt f funs with
    | None -> [f ^ " not defined"]
    | Some((targs, tres, data_fun, _)) ->
      let n_args = List.length targs in
      if List.length args <> n_args then
      ["Wrong number of parameters in " ^ f] (* Types.show_term (Func(f,args)) instead of f *)
      else [] @
      if is_pattern && not data_fun then [f ^ " is not a data function"] else []

(* Checks if term: exists, check_func, return list of errors *)
let rec check_term (env: (ident * data_type) list) (funs: (ident * (data_type list * data_type * bool * data_type list)) list) : term -> string list = function
  | Var(x) -> 
    begin
      match List.assoc_opt x env with 
      | Some(_) -> []
      | None -> [x ^ " not defined"]
    end
  | Func(f, args) ->
    check_func f args false funs @
    List.concat (List.map (check_term env funs) args)
  | Form(f, args) -> [] (* TODO Format typechecking *)
  | Tuple(l) ->
      List.concat(List.map (check_term env funs) l) (* recursively checks terms with their env and funcs, concat = flattens map *)
  | Eq(t1, t2) | And(t1, t2) | Or(t1, t2) ->
      check_term env funs t1 @ check_term env funs t2
  | Not(t) ->
      check_term env funs t
  | If(cond, t1, t2) -> 
      check_term env funs cond @ check_term env funs t1 @ check_term env funs t2 (* TODO: Should check if t1 and t2 are of the same type *)
  | Null -> []

(* Checks if pattern: is not pre-defined, check_term, check_func, return list of errors *)
let rec check_pattern env funs = function
  | PVar(x, _) -> 
    begin
      match List.assoc_opt x env with
      | Some(_) -> [x ^ " already defined in pattern"]
      | None -> [] (* check for free variables *)
    end
  | PMatch(t) ->
      check_term env funs t
  | PForm(f, args) -> [] (* TODO Format typechecking *) 
  | PFunc(f, args) ->
    check_func f args true funs @
      List.concat (List.map (check_pattern env funs) args)
  | PTuple(l) ->
      List.concat(List.map (check_pattern env funs) l)

(* Checks if term: exists, check_func, return list of errors *)
let rec get_term_type env funs = function
  | Var(x) -> 
    begin
      match List.assoc_opt x env with
      | Some(dt) -> dt
      | None -> raise (TypeError ("Variable doesn't exist in env")) (* TODO: Come up with better solution than raising an error *)
    end
  | Func(f, args) ->
    begin
      match List.assoc_opt f funs with
      | Some(_, dt, _, _) -> dt
      | None -> raise (TypeError ("Function doesn't exist in funs")) (* TODO: Come up with better solution than raising an error *)
    end
  | Form(f, args) -> None (* TODO Format typechecking *)
  | Tuple(l) ->
      DTType(List.map (fun t -> get_term_type env funs t) l) (* recursively gets terms with their env and funcs *)
  | Eq(t1, t2) | And(t1, t2) | Or(t1, t2) ->
      DType "bool" (* TODO: Consider checking types of t1 and t2 *)
  | Not(t) ->
      DType "bool" (* TODO: Consider if we need to check env *)
  | If(cond, t1, t2) -> 
      let first = get_term_type env funs t1 in
      let second = get_term_type env funs t2 in
      if first = second then first
      else raise (TypeError ("t1 and t2 are not of the same type in if-assignment")) (* TODO: Come up with better solution than raising an error *)
  | Null -> None

let rec typecheck (pr:problem): unit = 
  let p' = ("Dishonest", false)::pr.principals in
  let e = List.map (fun (p, x) -> p, (List.map (fun (var, dt, _) -> (var, dt)) (initial_knowledge p [] pr.knowledge))) pr.principals in (* (ident, (var name, data type))*)
  let messages = check pr.protocol e [] pr.functions in
  List.iter (fun (txt, ty) ->
    Printf.printf "Error: %s at %s" txt (show_global_type_nr ty)) messages
(* Checks global types, return list of errors *)
and check
  (g : global_type)                             (* Global type *)
  (env : tenv)                                  (* each princ. with their known var, as a list *)
  (def : (ident * ((ident * principal) list * global_type)) list)   (* function name, it's env and the global type *)
  (funs : (ident * (data_type list * data_type * bool * data_type list)) list)          (* function name, argument types, return data type, is data function, generic types *)
  : (string * global_type) list                 (* error messages and where in code *)
   =

(* checks send: if p and q exist, if t is well-formed, updates env of q with x *)
match g with
| Send(p, q, _, n, t, g') ->
begin
  match List.assoc_opt p env with                  (* returns ident list of p in env *)
  | None -> ["Principal " ^ p ^ " not defined", g]
  | Some(env_p) ->
    List.map (fun er -> (er, g)) (check_term env_p funs t) @ (* fun x.. : for return type error (message, G) *)
    match List.assoc_opt q env with
    | None -> ["Principal " ^ q ^ " not defined", g]
    | Some(env_q) ->
      let env' = update q ((n, get_term_type env_p funs t)::env_q) env in (* (principal, (ident, type) list) list *)
      check g' env' def funs
end

(* checks branch: if p and q exist, if t is well-formed, recursively check patterns *)
| Branch(p, q, _, t, args) ->
  begin
    match List.assoc_opt p env with
    | None -> ["Principal " ^ p ^ " not defined", g]
    | Some(env_p) ->
      List.map (fun e -> (e, g)) (check_term env_p funs t) @
      match List.assoc_opt q env with
      | None -> ["Principal " ^ q ^ " not defined", g]
      | Some(env_q) ->
        List.concat (List.map (
          fun (p, g) -> check g env def funs @ List.map (fun e -> (e, g)) (check_pattern env_q funs p)   (* pattern and global type *)
          ) args)
  end

(* Checks let-binding: update env of participant *)
| Compute(p, lb, g') ->
begin
  match List.assoc_opt p env with
  | None -> ["Principal " ^ p ^ " not defined", g]
  | Some(env_p) ->
    let rec let_bind env_p env = function
      | New(x, data_type, lb) -> (* TODO Implement check for data type *)
        let env_p' = ((x, data_type)::env_p) in
        let_bind env_p' (update p env_p' env) lb
      | Let(pat, t, lb) ->
        List.map (fun e -> (e, g)) (check_term env_p funs t) @
        List.map (fun e -> (e, g)) (check_pattern env_p funs pat) @
        let env_p' = binds env_p funs t pat @ env_p in
        let_bind env_p' (update p env_p' env) lb
      | Event(name, ts, lb) ->
        List.map (fun e -> (e, g)) (List.concat (List.map (check_term env_p funs) ts)) @
        let_bind env_p env lb
      | LetEnd -> check g' env def funs
    in let_bind env_p env lb
end

(* Checks function definition: *)
| DefGlobal(f, params, g', g'') ->
  let def' = ((f, (params, g'))::def) in
  let env' = add_params_to_env env params in
  begin
    match env' with
      | Error(err) -> List.map (fun e -> (e, g)) err
      | Result(env_param) -> (check g' env_param def' funs) @ (* obs recursion on def' *)
                       (check g'' env def' funs)
  end

(* Checks function calls *)
| CallGlobal(f, args) ->
  begin
    match List.assoc_opt f def with
      | None -> ["Funcion " ^ f ^ " not declared in ", g]
      | Some(params, g) ->
        if List.length args <> List.length params then
          ["Arguments and parameter lengths do not match in ", g]
        else
          List.map (fun e -> (e, g)) (List.concat (List.map (fun (t, (x, p)) -> check_term (List.assoc p env) funs t) (List.combine args params)))
  end

| GlobalEnd -> []
(*| _ -> [] *)