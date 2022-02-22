open Types

exception SyntaxError of string

let rec add_params_to_env env = function
    [] -> Result env
  | (param, princ)::params ->
    begin
      match List.assoc_opt princ env with
      | Some princ_env -> add_params_to_env (update princ (param::princ_env) env) params
      | None -> Error(["Principal " ^ princ ^ " not defined"])
    end)

(* Checks if func: exists, right number of args, passed variables match parameters types *)
let get_func_type f args funs =
  match List.assoc_opt f funs with
    | Some(param_types, dt, _, _) -> 
      if List.length args <> List.length param_types then raise (TypeError (Printf.sprintf "Wrong number of parameters for function %s" f)) 
      List.iteri (fun (i, arg) -> 
        let term_type = get_term_type env funs arg in
        let param_type = List.nth param_types i in
        if term_type <> param_type then 
          raise (TypeError (Printf.sprintf "Variable type %s doesn't match %s in signature of %s" term_type param_type f)) 
      ) args
      dt
    | None -> raise (TypeError ("Function doesn't exist in funs"))

(* Checks if term: exists, check_func, return list of errors *)
let rec get_term_type env funs = function
  | Var(x) -> 
    begin
      match List.assoc_opt x env with
      | Some(dt) -> dt
      | None -> raise (TypeError ("Variable doesn't exist in env"))
    end
  | Func(f, args) -> get_func_type f args funs
  | Form(f, args) -> DType(f)
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
      else raise (TypeError ("t1 and t2 are not of the same type in if-assignment")) (* TODO: Come up with better solution than raising an error *)
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
          | None -> raise (TypeError ("Type missing"))
        end
      | _ -> pdt
    end
  | PMatch(t) ->
      get_term_type env funs t
  | PForm(f, args) -> DType f (* TODO Format typechecking *) 
  | PFunc(f, args) -> get_func_type f args funs
  | PTuple(l) -> DTType(List.map (fun p -> get_pattern_type env funs p) l)


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
  (def : (ident * (((ident * data_type) * principal) list * global_type)) list)  (* function name, it's env and the global type *)
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

| GlobalEnd -> 
  begin
    List.iter (fun (p, l) -> 
    Printf.printf "%s:\n" p; 
    List.iter (fun (id, dt) -> 
      Printf.printf "\t(%s: %s)\n" id (show_dtype dt)) l; 
    Printf.printf "%s\n" "") env;
    []
  end
(*| _ -> [] *)