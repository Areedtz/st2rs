exception TypeError of string

type principal = string
type ident = string

(* 1. Types *)
type data_type =
    DType of ident
  | DAType of ident * ident
  | AType of ident
  | DTType of data_type list (* used for tuples in type checking *)
  | DFType of ident (* used for formats *)
  | None

(* Terms *)
type term =
    Var of ident
  | Func of ident * term list
  | Form of ident * term list
  | Tuple of term list
  | Eq of term * term
  | And of term * term
  | Or of term * term
  | Not of term
  | If of term * term * term
  | Null

type tenv = (principal * (ident * data_type) list) list

(* Pattern *)
type pattern =
    PVar of ident * data_type
  | PForm of ident * pattern list
  | PMatch of term
  | PTuple of pattern list

let rec pattern_to_term = function
    PVar(x, _) -> Var x
  | PMatch t -> t
  | PTuple args -> Tuple(List.map pattern_to_term args)

(* Let bindings *)
type let_bind =
    New of ident * data_type * let_bind
  | Let of pattern * term * let_bind
  | Event of ident * term list * let_bind
  | LetEnd

(* Channel options / Bullet notation *)
type channel_option =
  Public
  | Auth
  | Conf
  | AuthConf

(* Global types: p -> q *)
type global_type =
    Send of principal * principal * channel_option * ident * term * global_type
  | Branch of principal * principal * channel_option * global_type * global_type * global_type
  | Compute of principal * let_bind * global_type
  | DefGlobal of ident * ((ident * data_type) * principal) list * global_type * global_type
  | CallGlobal of ident * term list
  | GlobalEnd

(* Local Type *)
type local_type =
    LSend of ident * channel_option * term * local_type
  | LRecv of ident * channel_option * pattern * term * local_type
  | LOffer of local_type * local_type * local_type
  | LChoose of local_type * local_type * local_type
  | LNew of ident * data_type * local_type
  | LLet of pattern * term * local_type
  | LEvent of ident * term list * local_type
  | LLocalEnd

type problem = { name: ident;
                 principals: (principal * bool) list;
                 knowledge: (ident * data_type * principal * term) list;
                 types: data_type list;
                 functions: (ident * (data_type list * data_type * bool * data_type list)) list;
                 equations: (term * term) list;
                 formats: (ident * (data_type list)) list;
                 protocol: global_type }

type letb = LetB of pattern * term

(* 2. Should do when.. *)

(* Terms *)
let rec show_term = function
    Var(x) -> x
  | Func(name, args) -> name ^ "(" ^ show_term_list args ^ ")"
  | Form(name, args) -> name ^ "(" ^ show_term_list args ^ ")"
  | Tuple(args) -> "<" ^ show_term_list args ^ ">"
  | Eq(t1, t2) -> show_term t1 ^ " = " ^ show_term t2
  | And(t1, t2) -> show_term t1 ^ " & " ^ show_term t2
  | Or(t1, t2) -> show_term t1 ^ " | " ^ show_term t2
  | Not(t) -> "~" ^ show_term t
  | If(cond, tterm, fterm) -> "if(" ^ show_term cond ^ ", " ^ show_term tterm ^ ", " ^ show_term fterm ^ ")"
  | Null -> ""

(* List options: empty, single item, list *)
and show_term_list = function
    [] -> ""
  | [x] -> show_term x
  | (x::xs) -> show_term x ^ ", " ^ show_term_list xs

and show_pattern = function
    PVar(x, _) -> x
  | PForm(name, args) -> name ^ "(" ^ show_pattern_list args ^ ")"
  | PTuple(args) -> "<" ^ show_pattern_list args ^ ">"
  | PMatch(t) -> "=" ^ show_term t

and show_pattern_list = function
    [] -> ""
  | [x] -> show_pattern x
  | (x::xs) -> show_pattern x ^ ", " ^ show_pattern_list xs

and show_dtype_list = function
    [] -> ""
  | [x] -> show_dtype x
  | (x::xs) -> show_dtype x ^ ", " ^ show_dtype_list xs

and show_dtype t =
  match t with
  | DType dtype -> dtype
  | DAType(at, dt) -> at ^ "<" ^ dt ^">"
  | DTType l -> "(" ^ show_dtype_list l ^ ")"
  | DFType dtype -> dtype
  | _ -> ""

and show_let_bind = function
    New(name, data_type, letb) -> "  " ^ "new " ^ name ^ ";\n" ^ show_let_bind letb
  | Let(p, t, letb) -> "let " ^ show_pattern p ^ " = " ^ show_term t ^ " in\n" ^ show_let_bind letb
  | Event(f, args, letb) -> "event " ^ f ^ "("^ show_term_list args ^ ")\n" ^ show_let_bind letb
  | LetEnd -> ""

and show_channel_option = function
    Public   -> " -> "
  | Auth     -> " *-> "
  | Conf     -> " ->* "
  | AuthConf -> " *->* "

(* Show global types *)
and show_global_type = function
  Send(p, q, opt, x, t, g) -> p ^ show_channel_option opt ^ q ^ ": " ^ x ^ " = " ^ show_term t ^ "\n" ^ show_global_type g
| Compute(p, letb, g) ->
  p ^ " {\n" ^ show_let_bind letb ^ "}\n" ^ show_global_type g
| DefGlobal(name, params, g, g') ->
  name ^ "("^show_params params^")" ^ show_global_type g ^ "\nin\n"^show_global_type g'
| CallGlobal(name, params) ->
  name ^ "(" ^ show_term_list params ^ ")"
| GlobalEnd -> "end\n"

and show_global_type_nr = function
  Send(p, q, opt, x, t, g) -> p ^ show_channel_option opt ^ q ^ ": " ^ x ^ " = " ^ show_term t ^ " ..."
| Compute(p, letb, g) ->
  p ^ " {\n" ^ show_let_bind letb ^ "}...\n"
| DefGlobal(name, params, g, g') ->
  name ^ "("^show_params params^")" ^ show_global_type g ^ "\nin...\n"
| CallGlobal(name, params) ->
  name ^ "(" ^ show_term_list params ^ ")"
| GlobalEnd -> "end\n"

and show_branches = function
  [] -> ""
| ((p, g)::branches) ->
  show_pattern p ^ ": " ^ show_global_type g ^ "\n" ^ show_branches branches

and show_branches_nr = function
  [] -> ""
| ((p, g)::branches) ->
  show_pattern p ^ ": ...\n" ^ show_branches_nr branches

and show_params = function
  [] -> ""
| [((x, dt), p)] -> x ^ ": " ^ show_dtype dt ^ " @ " ^ p
| (((x, dt), p)::xs) -> x ^ ": " ^ show_dtype dt ^ " @ " ^ p ^ ", " ^ show_params xs

exception Lookup_failure

(* Update list of pair with x and y, returns updated env *)
let rec update x y = function
  | (x', y')::l ->             (* a::[b,c] = [a,b,c] add item to the beginning of a list *)
    if x = x' then (x, y)::l
    else (x', y')::update x y l
  | _ -> raise Lookup_failure;;
  (* env' = update q (x::env_q) env *)


let rec initial_knowledge p e = function
  | [] -> e
  | (t', dt', p', f') :: t ->
    if p' = p then initial_knowledge p ((t', dt', f')::e) t
    else initial_knowledge p e t

exception SyntaxError of string

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
