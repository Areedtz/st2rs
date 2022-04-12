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
  | DNone

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
  | IfAssign of term * term * term
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
  | IfBlock of term * let_bind * let_bind
  | LetQuit
  | LetEnd

(* Channel options / Bullet notation *)
type channel_option =
  Public
  | AuthConf

type event_list_type =
  Conjunction
  | Disjunction

type event =
  NonInjEvent of ident * term list
  | InjEvent of ident * term list

type query =
  ReachQuery of event list * event_list_type
  | CorrQuery of event list * query

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
  | LOffer of principal * principal * local_type * local_type
  | LChoose of principal * principal * local_type * local_type
  | LNew of ident * data_type * local_type
  | LLet of pattern * term * local_type
  | LEvent of ident * term list * local_type
  | LCall of ident * (ident * data_type) list * local_type
  | LIf of term * local_type * local_type
  | LQuit
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
  | IfAssign(cond, tterm, fterm) -> "if(" ^ show_term cond ^ ", " ^ show_term tterm ^ ", " ^ show_term fterm ^ ")"
  | Null -> ""

and get_channel_name sender receiver = if receiver < sender then receiver ^ sender else sender ^ receiver

(* List options: empty, single item, list *)
and show_term_list = function
    [] -> ""
  | [x] -> show_term x
  | (x::xs) -> show_term x ^ ", " ^ show_term_list xs

and show_pattern = function
    PVar(x, DNone) -> x
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

and show_channel_option = function
    Public   -> " -> "
  | AuthConf -> " *->* "

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

let show_penv penv = sprintf "[\n%s\n]" (String.concat ",\n" (List.map (fun (x, dt) -> sprintf "\t\t{\n\t\t\tName: %s,\n\t\t\tData Type: %s\n\t\t}" x (show_dtype dt)) penv))

let show_env env = String.concat ", " (List.map (fun (p, penv) -> sprintf "{\n\tPrincipal: %s,\n\tEnvironment: %s\n}\n" p (show_penv penv)) env)

let get_penv env p =
  match List.assoc_opt p env with
 | Some(penv) -> penv
 | None -> raise (TypeError("Could not find env for party"))

let safe_update p x dt env =
 match List.assoc_opt p env with
 | None -> raise (SyntaxError (sprintf "Principal %s was not found in environment %s" p (String.concat ", " (List.map (fun (p, _) -> p) env))))
 | Some(env_p) -> 
   match List.assoc_opt x env_p with
   | None -> update p ((x, dt)::env_p) env
   | Some(old_dt) -> 
     if old_dt <> dt
       then raise (TypeError (sprintf "Variable %s was previously assigned as %s. Cannot be reassigned as %s" x (show_dtype old_dt) (show_dtype dt)))
     else env

let safe_join_envs env1 env2 = List.fold_left (fun acc1 (p, penv) -> List.fold_left (fun acc2 (x, dt) -> safe_update p x dt acc2) acc1 penv) env1 env2    

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
 | Eq(t1, t2) | And(t1, t2) | Or(t1, t2) ->
    let first = get_term_type env forms funs t1 in
    let second = get_term_type env forms funs t2 in
    if first = second 
      then DType "bool"
      else raise (TypeError (sprintf "Variable type %s doesn't match %s in condition check" (show_dtype first) (show_dtype second)))
 | Not(t) ->
    let term_type = get_term_type env forms funs t in
    begin
      match term_type with
      | DType(b) when b = "bool" -> DType "bool"
      | _ ->  raise (TypeError (sprintf "Variable type %s doesn't match bool in negation" (show_dtype term_type)))
    end
 | IfAssign(cond, t1, t2) ->
    let cond_type = get_term_type env forms funs cond in
    begin
      match cond_type with
      | DType(b) when b = "bool" -> 
        let first = get_term_type env forms funs t1 in
        let second = get_term_type env forms funs t2 in
        if first = second then first
        else raise (TypeError ("t1 and t2 are not of the same type in if-assignment"))
      | _ ->  raise (TypeError (sprintf "Variable type %s doesn't match bool in if condition" (show_dtype cond_type)))
    end
 | Null -> DNone
     
let rec get_pattern_types env forms funs = function
 | PVar(x, pdt) ->
   begin
     let existing_type = 
       match List.assoc_opt x env with
       | Some(dt) -> dt
       | None -> DNone in
     match pdt with
     | DNone -> [(x, existing_type)]
     | _ -> 
       if existing_type <> DNone && pdt <> existing_type
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
     | None -> raise (SyntaxError (sprintf "Format %s was not found in format definitions" f))
     | Some(dtypes) -> 
       let argtypes = List.flatten (List.map (fun arg -> get_pattern_types env forms funs arg) args) in
       let combinetypes = List.combine argtypes dtypes in
       (f, DFType f)::List.map (fun ((x, dt), fdt) ->
         if dt <> DNone && dt <> fdt
           then raise (TypeError(sprintf "Variable %s is used as %s, but was previously assigned as %s" x (show_dtype fdt) (show_dtype dt)))
           else (x, fdt)
       ) combinetypes
   end
 | PTuple(args) -> List.flatten (List.map (fun arg ->
      begin
        match arg with
        | PVar(x, DNone) -> 
          begin
            match List.assoc_opt x env with
            | None -> raise (TypeError(sprintf "Variable %s is being defined as part of a tuple, but no type is given" x))
            | _ -> ()
          end
        | _ -> ()
      end;
      get_pattern_types env forms funs arg) args
   )

let dt_unpack dt =
 match dt with
 | DTType(types) -> types
 | _ -> [dt]

let check_event_types env evs forms funs name terms = 
  let event_types = 
    match List.assoc_opt name evs with
    | Some(types) -> types
    | None -> raise (SyntaxError(sprintf "Event %s was not defined before being used" name)) in
  if List.length event_types <> List.length terms then raise (SyntaxError(sprintf "Wrong number of params passed to event %s" name));
  List.iteri (fun i t -> if List.nth event_types i <> get_term_type env forms funs t then raise (TypeError(sprintf "Term %s doesn't match type %s" (show_term t) (show_dtype (List.nth event_types i))))) terms

let check_principle_exists principals p =
  if List.exists (fun (pl, _) -> pl = p) principals 
    then () 
    else raise (SyntaxError (sprintf "Principal %s does not exist in principal list" p))

let rec get_last_global_type gt =
  match gt with
  | Send(_, _, _, _, _, g) | Compute(_, _, g) | DefGlobal(_, _, g) -> get_last_global_type g
  | _ -> gt

let rec get_last_local_type lt =
  match lt with
  | LSend(_, _, _, _, _, lt) | LRecv (_, _, _, _, _, lt) 
  | LNew (_, _, lt) | LLet (_, _, lt)
  | LEvent (_, _, lt) -> get_last_local_type lt
  | _ -> lt

let rec get_term_variables = function
  | Var(x) -> [x]
  | Func(_, args) | Form(_, args) | Tuple(args) -> List.flatten (List.map (get_term_variables) args)
  | Eq(t1, t2) | And(t1, t2) | Or(t1, t2) -> List.flatten (List.map (get_term_variables) [t1;t2])
  | Not(t) -> get_term_variables t
  | IfAssign(cond, t1, t2) -> List.flatten (List.map (get_term_variables) [cond; t1;t2])
  | Null -> []

let rec get_pattern_variables = function
  PVar(x, _) -> [x]
  | PForm(_, args) | PTuple(args) -> List.flatten (List.map (get_pattern_variables) args)
  | PMatch(t) -> get_term_variables t

let rec get_free_variables gfuns reserved princ gt acc =
  match gt with
 | Send(s, _, _, _, t, g) when princ = s ->
   get_free_variables gfuns reserved princ g (get_term_variables t@acc)
 | Send(_, r, _, x, _, g) when princ = r ->
   let newreserved = if (List.exists (fun y -> x = y) acc) then reserved else (x::reserved) in
   get_free_variables gfuns newreserved princ g acc
 | Send(_, _, _, _, _, g) -> get_free_variables gfuns reserved princ g acc
 | Compute(p, letb, g) when p = princ ->
    let rec inner reserved letb acc =
      match letb with
      | New(x, _, next) -> 
        let newreserved = if (List.exists (fun y -> x = y) acc) then reserved else (x::reserved) in (* If var exists in the acc and it gets defined again, we don't add it to the reserved list as we need it passed on *)
          inner newreserved next acc
      | Let(pattern, term, next) -> 
        let newacc = get_term_variables term@acc in
        let newreserved = (List.filter (fun x -> not(List.exists (fun y -> x = y) newacc)) (get_pattern_variables pattern))@reserved in
        inner newreserved next newacc
      | Event(_, terms, next) -> inner reserved next ((List.flatten (List.map (get_term_variables) terms))@acc)
      | IfBlock(cond, thenb, elseb) ->
        let newacc = get_term_variables cond@acc in
        inner reserved thenb (inner reserved elseb newacc)
      | LetQuit -> List.filter (fun x -> not(List.exists (fun y -> x = y) reserved)) acc
      | LetEnd -> get_free_variables gfuns reserved princ g acc in
    inner reserved letb acc
 | Compute(_, _, g) -> get_free_variables gfuns reserved princ g acc
 | Branch(_, _, _, lb, rb) -> get_free_variables gfuns reserved princ lb (get_free_variables gfuns reserved princ rb acc)
 | DefGlobal(name, def, g) -> get_free_variables ((name, def)::gfuns) reserved princ g acc (* Get the free vars for any global func when we find its definition *)
 | CallGlobal(name) -> get_free_variables gfuns reserved princ (List.assoc name gfuns) acc (* Get the free vars for a global func if we find a call to it *)
 | GlobalEnd -> List.filter (fun x -> not(List.exists (fun y -> x = y) reserved)) acc

let rec compile principals if_prefix orig_env env forms funs evs gfuns princ gt =
 match gt with
 | Send(s, r, opt, x, t, g) when princ = s ->
   check_principle_exists principals s; check_principle_exists principals r;
   let ttype = get_term_type (get_penv env s) forms funs t in (* also checks if s can send t *)
   let env' = safe_update r x ttype env in
   LSend(s, r, opt, t, ttype, compile principals if_prefix orig_env env' forms funs evs gfuns princ g)
 | Send(s, r, opt, x, t, g) when princ = r ->
   check_principle_exists principals s; check_principle_exists principals r;
   let ttype = get_term_type (get_penv env s) forms funs t in (* also checks if s can send t *)
   let env' = safe_update r x ttype env in
   LRecv(s, r, opt, PVar(x, ttype), t, compile principals if_prefix orig_env env' forms funs evs gfuns princ g)
 | Send(s, r, _, x, t, g) ->
   check_principle_exists principals s; check_principle_exists principals r;
   let ttype = get_term_type (get_penv env s) forms funs t in (* also checks if s can send t *)
   let env' = safe_update r x ttype env in
   compile principals if_prefix orig_env env' forms funs evs gfuns princ g
 | Compute(p, letb, g) ->
   check_principle_exists principals p;
   let rec compile_letb inner_env return_type letb =
     match letb with
     | New(name, dt, next) ->
       let inner_env' = safe_update p name dt inner_env in
       if p = princ 
         then LNew(name, dt, compile_letb inner_env' return_type next)
         else compile_letb inner_env' return_type next
     | Let(pattern, term, next) ->
       let ptypes = get_pattern_types (get_penv inner_env p) forms funs pattern in
       let ttype = get_term_type (get_penv inner_env p) forms funs term in
       let inner_env' = if List.length ptypes == 1 then
         if (snd (List.nth ptypes 0)) <> DNone && (snd (List.nth ptypes 0)) <> ttype then begin
           printf "%s" (show_dtype (ttype));
           raise (TypeError(sprintf "Mismatching types in left and right hand parts of assignment")) end
         else 
           safe_update p (fst (List.nth ptypes 0)) ttype inner_env
       else
         let unpacked = dt_unpack ttype in
         if List.length ptypes == List.length unpacked then
           List.fold_left2 (fun acc ptype unpacktype -> 
             if (snd ptype) <> DNone && (snd ptype) <> unpacktype then
               raise (TypeError(sprintf "Mismatching types in left and right hand parts of assignment"))
             else 
               safe_update p (fst ptype) unpacktype acc
           ) inner_env ptypes unpacked
         else 
          match ptypes with
          | ((_, t))::xs when t = ttype -> List.fold_left (fun acc ptype -> safe_update p (fst ptype) (snd ptype) acc) inner_env ptypes
          | _ -> raise (SyntaxError(sprintf "Could not match left hand side of assignment to right hand side of assignment, mismatching number of variables")) in
       if p = princ then 
         LLet(pattern, term, compile_letb inner_env' return_type next)
       else 
         compile_letb inner_env' return_type next
     | Event(name, terms, next) ->
       check_event_types (get_penv inner_env p) evs forms funs name terms;
       if p = princ then
         LEvent(name, terms, compile_letb inner_env return_type next)
       else compile_letb inner_env return_type next
     | IfBlock(cond, thenb, elseb) ->
       let _ = get_term_type (get_penv inner_env p) forms funs cond in
       if p = princ then
        let penv = get_penv inner_env princ in
        let free_vars = get_free_variables gfuns [] princ g [] in
        let free_vars_uniq = List.sort_uniq (fun x y -> compare x y) free_vars in
        let free_vars_with_types = List.map (fun x -> 
          match List.assoc_opt x penv with
          | Some(dt) -> (x, dt)
          | None -> raise (SyntaxError(sprintf "Could not find variable %s used in function %s in env" x "_Condition"))
        ) free_vars_uniq in
        let name = if_prefix ^ "I" in
        let next = compile principals name orig_env inner_env forms funs evs gfuns princ g in
        begin
          match next with
          | LLocalEnd -> LIf(cond, compile_letb inner_env None thenb, compile_letb inner_env None elseb)
          | _ -> 
            let lcall = LCall(name, free_vars_with_types, next) in
            LIf(cond, compile_letb inner_env (Some(lcall)) thenb, compile_letb inner_env (Some(lcall)) elseb)
        end
       else compile principals if_prefix orig_env inner_env forms funs evs gfuns princ g
      | LetQuit ->
          if p = princ then
            LQuit
          else compile principals if_prefix orig_env inner_env forms funs evs gfuns princ g
      | LetEnd ->
          begin
            match return_type with
            | Some(lcall) -> lcall
            | None -> compile principals if_prefix orig_env inner_env forms funs evs gfuns princ g
          end in
   compile_letb env None letb
 | Branch(s, r, _, lb, rb) when princ = s ->
   check_principle_exists principals s; check_principle_exists principals r;
   let env' = List.filter (fun (p, _) -> p = s || p = r) env in
   let passed_env = if List.length orig_env <> 0 then safe_join_envs orig_env env else env in
   LOffer(s, r, compile principals (if_prefix ^ "BL") passed_env env' forms funs evs gfuns princ lb, compile principals (if_prefix ^ "BR") passed_env env' forms funs evs gfuns princ rb)
 | Branch(s, r, _, lb, rb) when princ = r ->
   check_principle_exists principals s; check_principle_exists principals r;
   let env' = List.filter (fun (p, _) -> p = s || p = r) env in
   let passed_env = if List.length orig_env <> 0 then safe_join_envs orig_env env else env in (* Updating the original env with the one from a branch and passing it on for the nested branching where it will get filtered again *)
   LChoose(s, r, compile principals (if_prefix ^ "BL") passed_env env' forms funs evs gfuns princ lb, compile principals (if_prefix ^ "BR") passed_env env' forms funs evs gfuns princ rb)
 | Branch(s, r, _, lb, rb) ->
    check_principle_exists principals s; check_principle_exists principals r;
    let left = compile principals if_prefix orig_env env forms funs evs gfuns princ lb in
    let right = compile principals if_prefix orig_env env forms funs evs gfuns princ rb in
    if (compare left right) <> 0 then raise (SyntaxError (sprintf "Left and right branch don't end the same way for %s" princ)); (* Compare branches for all non-branching parties *)
    left
 | DefGlobal(name, def, g) -> compile principals if_prefix orig_env env forms funs evs ((name, def)::gfuns) princ g (* Adding functions as we find them; Even if we have 2 global funcs defined with the same name, we would know which one we are referring to when we have the call *)
 | CallGlobal(name) when List.length orig_env <> 0 -> (* If in a branch - when calling LCall we will pass false if not in a branch *)
    let penv = get_penv env princ in
    let free_vars = get_free_variables gfuns [] princ (List.assoc name gfuns) [] in
    let free_vars_uniq = List.sort_uniq (fun x y -> compare x y) free_vars in
    let free_vars_with_types = List.map (fun x -> 
      match List.assoc_opt x penv with
      | Some(dt) -> (x, dt)
      | None -> raise (SyntaxError(sprintf "Could not find variable %s used in function %s in env" x name))
    ) free_vars_uniq in
    let passed_env = safe_join_envs orig_env env in (* If in branch, we combine the env that was present during the branching with the original env, so in case new variable were created, we get an updated env going forward *)
    LCall(name, free_vars_with_types, compile principals name [] passed_env forms funs evs gfuns princ (List.assoc name gfuns)) (* Have all the vars with their types that we pass to the principal's call for their part in the global func *)
 | CallGlobal(name) -> compile principals name [] env forms funs evs gfuns princ (List.assoc name gfuns)
 | GlobalEnd -> LLocalEnd

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
