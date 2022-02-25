open Printf

exception TypeError of string

type principal = string
type ident = string
type lemma = string
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
  | PFunc of ident * pattern list
  | PTuple of pattern list

let rec pattern_to_term = function
    PVar(x, _) -> Var x
  | PMatch t -> t
  | PFunc(f, args) -> Func(f, List.map pattern_to_term args)
  | PTuple args -> Tuple(List.map pattern_to_term args)

let id_to_var = List.map (fun x -> Var x)
let prin_to_ident x = (x : ident)

(* Let bindings *)
type let_bind =
    New of ident * data_type * let_bind
  | Let of pattern * term * let_bind
  | Event of ident * term list * let_bind
  | LetEnd

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

let rec binds env funs t = function
    PVar(x, dt) -> 
      begin
        match dt with
        | None -> [x, get_term_type env funs t]
        | _ -> [x, dt]
      end
  | PMatch(_) -> []
  | PFunc(_, args) | PForm(_, args) | PTuple args -> List.concat (List.map (binds env funs t) args)

(* Channel options / Bullet notation *)
type channel_option =
  Public
  | Auth
  | Conf
  | AuthConf

(* Global types: p -> q *)
type global_type =
    Send of principal * principal * channel_option * ident * term * global_type
  | Branch of principal * principal * channel_option * term * (pattern * global_type) list
  | Compute of principal * let_bind * global_type
  | DefGlobal of ident * ((ident * data_type) * principal) list * global_type * global_type
  | CallGlobal of ident * term list
  | GlobalEnd

(* Local Type *)
type local_type =
    LSend of ident * channel_option * term * local_type
  | LRecv of ident * channel_option * pattern * term * local_type
  | LSelect of principal * (term * local_type) list
  | LBranch of principal * (pattern * local_type) list
  | LNew of ident * data_type * local_type
  | LLet of pattern * term * local_type
  | LEvent of ident * term list * local_type
  | LDefLocal of ident * ident list * local_type * local_type
  | LCallLocal of ident * term list * local_type
  | LLocalEnd


(*
type prindis =
  | Prin of principal
  | DPrin of principal * ident
*)

type problem = { name: ident;
                 principals: (principal * bool) list;
                 knowledge: (ident * data_type * principal * term) list;
                 types: data_type list;
                 functions: (ident * (data_type list * data_type * bool * data_type list)) list;
                 equations: (term * term) list;
                 formats: (ident * (data_type list)) list;
                 protocol: global_type;
                 lemm: lemma option}

type fact = Fact of ident * term list
type letb = LetB of pattern * term
type msr_rule = Rule of letb list * fact list * fact list * fact list

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
  | PFunc(name, args) -> name ^ "(" ^ show_pattern_list args ^ ")"
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
| Branch(p, q, opt, t, branches) ->
  p ^ show_channel_option opt ^ q ^ ": match " ^ show_term t ^ " with {\n" ^ show_branches branches ^ "}\n"
| Compute(p, letb, g) ->
  p ^ " {\n" ^ show_let_bind letb ^ "}\n" ^ show_global_type g
| DefGlobal(name, params, g, g') ->
  name ^ "("^show_params params^")" ^ show_global_type g ^ "\nin\n"^show_global_type g'
| CallGlobal(name, params) ->
  name ^ "(" ^ show_term_list params ^ ")"
| GlobalEnd -> "end\n"

and show_global_type_nr = function
  Send(p, q, opt, x, t, g) -> p ^ show_channel_option opt ^ q ^ ": " ^ x ^ " = " ^ show_term t ^ " ..."
| Branch(p, q, opt, t, branches) ->
  p ^ show_channel_option opt ^ q ^ ": match " ^ show_term t ^ " with {\n" ^ show_branches_nr branches ^ "}\n"
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

(* show rules *)
let show_fact (Fact(f, args)) = f ^"("^show_term_list args^")"
let rec show_facts = function
    [] -> ""
  | [f] -> show_fact f
  | f::fs -> show_fact f^", "^show_facts fs

let show_rule (Rule(letb, l, e, r)) =
  let rec show_letb = function
      [] -> ""
    | [LetB(pat, t)] -> show_term (pattern_to_term pat) ^" = "^ show_term t
    | LetB(pat, t)::letb' -> show_term (pattern_to_term pat) ^" = "^ show_term t ^"\n"^show_letb letb' in
  (if letb = [] then "" else "let " ^ show_letb letb ^ " in\n")^" [" ^ show_facts l ^"] --["^ show_facts e^"]-> ["^ show_facts r^"]\n"

let rec show_rules n = function
    [] -> ""
  | [r] -> "rule R"^string_of_int n^":\n"^show_rule r
  | r::rs -> "rule R"^string_of_int n^":\n"^show_rule r ^"\n\n"^show_rules (n+1) rs


let rec show_fdefs = function
    [] -> ""
  | [f, (args_t, res_t, _, _)] -> f ^ "/" ^ string_of_int (List.length args_t)
  | (f, (args_t, res_t, _, _))::fs -> f ^ "/" ^ string_of_int (List.length args_t) ^", "^show_fdefs fs

let rec show_eqdefs = function
    [] -> ""
  | [(t1, t2)] -> show_term t1 ^ " = " ^ show_term t2
  | (t1, t2)::ts -> show_term t1 ^ " = " ^ show_term t2 ^ ", " ^ show_eqdefs ts

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

let rec print_sep = function
| [] -> ()
| [x] -> printf "%s" x
| (x::xs) -> printf "%s, " x; print_sep xs

let mscgen (pr:problem): unit =
  let last l = List.nth l (List.length l - 1) in
  let rec mscglobal = function
  | Send(p, q, _, x, t, g) -> p ^ "->" ^ q ^ " [label=\"" ^ x ^ " = " ^ show_term t ^ "\"];\n" ^ mscglobal g
  | Branch(p, q, _, t, branches) ->
    let rec mscbranches = function
    | [] -> ""
    | ((pat, g)::branches) ->
      p ^ "->" ^ q ^ " [label=\"" ^ show_term t ^ " = " ^ show_pattern pat ^ "];\n " ^ mscglobal g ^ "---;\n" ^ mscbranches branches
    in "---;\n" ^ mscbranches branches ^ "\n"
  | Compute(p, letb, g) ->
    p ^" box "^p^ " [label=\"" ^ show_let_bind letb ^ "\"];\n" ^ mscglobal g
  | DefGlobal(name, params, g, g') ->
    fst (List.hd pr.principals) ^ " alt " ^ fst(last pr.principals) ^" [label=\""^name ^ "("^show_params params^")\"] {\n" ^ mscglobal g ^ "\n};\n"^mscglobal g'
  | CallGlobal(name, params) -> "\n" (* TODO: replace with a proper call *)
  | GlobalEnd -> "\n"
  in
  printf "msc {\n";
  print_sep ((List.map fst) pr.principals);
  printf ";\n";
  printf "%s\n" (mscglobal pr.protocol);
  printf "}\n"
