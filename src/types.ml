exception TypeError of string
exception SyntaxError of string

let printf = Printf.printf (* used for files referencing Types *)
let sprintf = Printf.sprintf

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
  | PMatch(t) -> t
  | PTuple(args) -> Tuple(List.map pattern_to_term args)
  | PForm(x, plist) -> Form(x, List.map pattern_to_term plist)

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

type event =
  NonInjEvent of ident * term list
  | InjEvent of ident * term list
  
type query =
  ReachQuery of event
  | CorrQuery of event * query

(* Global types: p -> q *)
type global_type =
    Send of principal * principal * channel_option * ident * term * global_type
  | Branch of principal * principal * channel_option * global_type * global_type
  | Compute of principal * let_bind * global_type
  | DefGlobal of ident * global_type * global_type
  | CallGlobal of ident
  | GlobalEnd

(* Local Type *)
type local_type =
  LSend of principal * principal * channel_option * term * data_type * local_type
  | LRecv of principal * principal * channel_option * pattern * term * local_type
  | LOffer of principal * principal * local_type * local_type * local_type
  | LChoose of principal * principal * local_type * local_type * local_type
  | LNew of ident * data_type * local_type
  | LLet of pattern * term * local_type
  | LEvent of ident * term list * local_type
  | LCall of ident * (ident * data_type) list
  | LLocalEnd

type problem = { name: ident;
                 principals: (principal * bool) list;
                 knowledge: (ident * data_type * principal * term) list;
                 types: data_type list;
                 functions: (ident * (data_type list * data_type * bool * data_type list)) list;
                 equations: (term * term) list;
                 formats: (ident * (data_type list)) list;
                 events: (ident * data_type list) list;
                 queries: query list;
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

and get_channel_name sender receiver = if receiver < sender then receiver ^ sender else sender ^ receiver

(* List options: empty, single item, list *)
and show_term_list = function
    [] -> ""
  | [x] -> show_term x
  | (x::xs) -> show_term x ^ ", " ^ show_term_list xs

and show_pattern = function
    PVar(x, None) -> x
  | PVar(x, dt) -> x ^ ": " ^ show_dtype dt
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
| DefGlobal(name, g, g') ->
  name ^ show_global_type g ^ "\nin\n"^ show_global_type g'
| CallGlobal(name) ->
  name ^ "()"
| Branch(p, q, opt, lb, rb) -> p ^ show_channel_option opt ^ q ^ " {\n\tLeft:" ^ show_global_type lb ^ "\n\tRight:" ^ show_global_type rb ^ "\n}\n"
| GlobalEnd -> "end\n"

and show_global_type_nr = function
  Send(p, q, opt, x, t, _) -> p ^ show_channel_option opt ^ q ^ ": " ^ x ^ " = " ^ show_term t ^ " ..."
| Compute(p, letb, _) ->
  p ^ " {\n" ^ show_let_bind letb ^ "}...\n"
| DefGlobal(name, g, _) ->
  name ^ "()" ^ show_global_type g ^ "\nin...\n"
| CallGlobal(name) ->
  name ^ "()"
| Branch(p, q, opt, _, _) -> p ^ show_channel_option opt ^ q ^ " {\n\tLeft:...\n\tRight:...\n}\n"
| GlobalEnd -> "end\n"

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

let get_penv env p =
  match List.assoc_opt p env with
 | Some(penv) -> penv
 | None -> raise (TypeError("Could not find env for party"))

let safe_update p x dt env =
 match List.assoc_opt p env with
 | None -> raise (SyntaxError (sprintf "Principal %s was not found in environment" p))
 | Some(env_p) -> 
   match List.assoc_opt x env_p with
   | None -> update p ((x, dt)::env_p) env
   | Some(old_dt) -> 
     if old_dt <> dt
       then raise (TypeError (sprintf "Variable %s was previously assigned as %s. Cannot be reassigned as %s" x (show_dtype old_dt) (show_dtype dt)))
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
           raise (SyntaxError (sprintf "Wrong number of parameters for function %s" f)) 
         else
           List.iteri (fun i arg ->
             let arg_type = get_term_type env forms funs arg in
             let param_type = List.nth param_types i in
             if arg_type <> param_type 
               then 
                 raise (TypeError (sprintf "Variable type %s doesn't match %s in signature of %s" (show_dtype arg_type) (show_dtype param_type) f))
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
           raise (SyntaxError (sprintf "Wrong number of parameters for format %s" f)) 
         else
           List.iteri (fun i arg ->
             let arg_type = get_term_type env forms funs arg in
             let param_type = List.nth param_types i in
             if arg_type <> param_type 
               then 
                 raise (TypeError (sprintf "Variable type %s doesn't match %s in signature of %s" (show_dtype arg_type) (show_dtype param_type) f))
               else ()
           ) args;
           DFType f
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
         then raise (TypeError(sprintf "Variable %s was matched to type %s, but was previously assigned as %s" x (show_dtype pdt) (show_dtype existing_type)))
         else [(x, pdt)]
   end
 | PMatch(t) ->
   begin
     match t with
     | Var(x) -> [(x, get_term_type env forms funs t)]
     | _ -> raise (SyntaxError(sprintf "Pattern match only supports variables, not %s" (show_term t)))
   end
 | PForm(f, args) ->
   begin
     match List.assoc_opt f forms with
     | None -> raise (SyntaxError(sprintf "Format %s was not found in format definitions" f))
     | Some(dtypes) -> 
       let argtypes = List.flatten (List.map (fun arg -> get_pattern_types env forms funs arg) args) in
       let combinetypes = List.combine argtypes dtypes in
       List.map (fun ((x, dt), fdt) ->
         if dt <> None && dt <> fdt
           then raise (TypeError(sprintf "Variable %s is used as %s, but was previously assigned as %s" x (show_dtype fdt) (show_dtype dt)))
           else (x, fdt)
       ) combinetypes
   end
 | PTuple(args) -> List.flatten (List.map (fun arg -> get_pattern_types env forms funs arg) args)

let dt_unpack dt =
 match dt with
 | DTType(types) -> types
 | _ -> [dt]

let check_event_types env evs forms funs name terms = 
  let event_types = 
    match List.assoc_opt name evs with
    | Some(types) -> types
    | None -> raise (SyntaxError(sprintf "Event %s was not defined before being used" name)) in
  if List.length event_types <> List.length terms then raise (SyntaxError(sprintf "Too many params passed to event %s" name));
  List.iteri (fun i t -> if List.nth event_types i <> get_term_type env forms funs t then raise (TypeError(sprintf "Term %s doesn't match type %s" (show_term t) (show_dtype (List.nth event_types i))))) terms

let check_principle_exists principals p =
  if List.exists (fun (pl, _) -> pl = p) principals 
    then () 
    else raise (SyntaxError (sprintf "Principal %s does not exist in principal list" p))

let rec get_last_global_type gt =
  match gt with
  | Send(_, _, _, _, _, g) | Compute(_, _, g) | DefGlobal(_, _, g) | Branch(_, _, _, g, _)-> get_last_global_type g
  | _ -> gt

let rec get_last_local_type lt =
  match lt with
  | LSend(_, _, _, _, _, lt) | LRecv (_, _, _, _, _, lt) 
  | LNew (_, _, lt) | LLet (_, _, lt)
  | LEvent (_, _, lt) | LChoose(_, _, lt, _, _) | LOffer(_, _, lt, _, _) -> get_last_local_type lt
  | _ -> lt

let rec compile principals env forms funs evs gfuns princ gt =
 match gt with
 | Send(s, r, opt, x, t, g) when princ = s ->
   check_principle_exists principals s; check_principle_exists principals r;
   let ttype = get_term_type (get_penv env s) forms funs t in (* also checks if s can send t *)
   let env' = safe_update r x ttype env in
   LSend(s, r, opt, t, ttype, compile principals env' forms funs evs gfuns princ g)
 | Send(s, r, opt, x, t, g) when princ = r ->
   check_principle_exists principals s; check_principle_exists principals r;
   let ttype = get_term_type (get_penv env s) forms funs t in (* also checks if s can send t *)
   let env' = safe_update r x ttype env in
   
   LRecv(s, r, opt, PVar(x, ttype), t, compile principals env' forms funs evs gfuns princ g)
 | Send(s, r, _, x, t, g) ->
   check_principle_exists principals s; check_principle_exists principals r;
   let ttype = get_term_type (get_penv env s) forms funs t in (* also checks if s can send t *)
   let env' = safe_update r x ttype env in
   compile principals env' forms funs evs gfuns princ g
 | Compute(p, letb, g) ->
   check_principle_exists principals p;
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
           raise (TypeError(sprintf "Mismatching types in left and right hand parts of assignment"))
         else 
           safe_update p (fst (List.nth ptypes 0)) ttype inner_env
       else
         let unpacked = dt_unpack ttype in
         if List.length ptypes == List.length unpacked then
           List.fold_left2 (fun acc ptype unpacktype -> 
             if (snd ptype) <> None && (snd ptype) <> unpacktype then
               raise (TypeError(sprintf "Mismatching types in left and right hand parts of assignment"))
             else 
               safe_update p (fst ptype) unpacktype acc
           ) inner_env ptypes unpacked
         else
           raise (SyntaxError(sprintf "Could not match left hand side of assignment to right hand side of assignment, mismatching number of variables")) in
       if p = princ then 
         LLet(pattern, term, compile_letb inner_env' next)
       else 
         compile_letb inner_env' next
     | Event(name, terms, next) ->
       check_event_types (get_penv inner_env p) evs forms funs name terms;
       if p = princ then
         LEvent(name, terms, compile_letb inner_env next)
       else compile_letb inner_env next
     | LetEnd -> compile principals inner_env forms funs evs gfuns princ g in
   compile_letb env letb
 | Branch(s, r, _, lb, rb) when princ = s ->
   check_principle_exists principals s; check_principle_exists principals r;
   let env' = List.filter (fun (p, _) -> p = s || p = r) env in
   (* Check if lb and rb ends the same way *)
   let last_global_lb = get_last_global_type lb in
   let last_global_rb = get_last_global_type rb in
   if (compare last_global_lb last_global_rb) <> 0 then raise (SyntaxError (sprintf "Left and right branch don't end the same way"));
   let next = 
    begin
      match last_global_lb with
      | CallGlobal(name) -> compile principals env forms funs evs gfuns princ (List.assoc name gfuns)
      | _ -> LLocalEnd
    end in
   LOffer(s, r, compile principals env forms funs evs gfuns princ lb, compile principals env forms funs evs gfuns princ rb, next)
 | Branch(s, r, _, lb, rb) when princ = r ->
   check_principle_exists principals s; check_principle_exists principals r;
   let env' = List.filter (fun (p, _) -> p = s || p = r) env in
   (* Check if lb and rb ends the same way *)
   let last_global_lb = get_last_global_type lb in
   let last_global_rb = get_last_global_type rb in
   if (compare last_global_lb last_global_rb) <> 0 then raise (SyntaxError (sprintf "Left and right branch don't end the same way"));
   let next = 
    begin
      match last_global_lb with
      | CallGlobal(name) -> compile principals env forms funs evs gfuns princ (List.assoc name gfuns)
      | _ -> LLocalEnd
    end in
   LChoose(s, r, compile principals env forms funs evs gfuns princ lb, compile principals env forms funs evs gfuns princ rb, next)
 | Branch(s, r, _, lb, _) ->
   check_principle_exists principals s; check_principle_exists principals r;
   let next = get_last_global_type lb in
   begin
    match next with
    | Branch(_, _, _, _, _) -> compile principals env forms funs evs gfuns princ next
    | CallGlobal(name) -> compile principals env forms funs evs gfuns princ (List.assoc name gfuns)
    | _ -> LLocalEnd
   end
 | DefGlobal(_, _, g) -> compile principals env forms funs evs gfuns princ g
 | CallGlobal(name) -> compile principals env forms funs evs gfuns princ (List.assoc name gfuns)
 | _ -> LLocalEnd

 and build_global_funs_list = function
  DefGlobal(name, g, gt) -> (name, g)::(build_global_funs_list gt)
  | Send(_, _, _, _, _, gt) | Compute(_, _, gt) -> build_global_funs_list gt
  | _ -> []

and build_function_types = function
 (f, (args_t, _, _, _)) -> (f, List.map (fun t -> show_dtype t) args_t)

 let rec build_equation_params t1 t2 names_and_types = (* [(var name, type)...] *)
  let rec inner t pos function_types =
    begin
    match (t, function_types) with
    | (Var(_), []) -> []
    | (Var(x), _) -> [(x, List.nth function_types pos)]
    | (Func(name, args), _) -> List.flatten (List.mapi (fun i arg -> inner arg i (List.assoc name names_and_types)) args)
    | _ -> []
    end in
  List.sort_uniq (fun (a,_) (c,_) -> compare a c) ((inner t1 0 [])@(inner t2 0 []))
