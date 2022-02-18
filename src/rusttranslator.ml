open Rusttypes2
open Types
open Rustprinter

let next_var =
  let private_counter = ref (-1) in
  fun () ->
    private_counter := !private_counter + 1;
    "v"^string_of_int(!private_counter)

let indent = "    "
let abstract_traits = ":Serialize + DeserializeOwned"

let rec translateTerm t =
  match t with
  | Var(x) -> Id(ID(x))
  | Func(name, args) -> Exp(Id(ID(name)), translateArgs args)
  | Form(name, args) -> EStruct((ID(name)), StructValues(List.map (fun x-> StructValue x) ((List.map (fun a -> translateTerm a) args))))
  | Tuple(args) -> Id(ID("(" ^ printExp (translateArgs args) ^ ")"))
  | Eq(l, r) -> Id(ID(printExp (translateTerm l) ^ " == " ^ printExp (translateTerm r)))
  | And(l, r) -> Id(ID(printExp (translateTerm l) ^ " && " ^ printExp (translateTerm r)))
  | If(cond, t1, t2) -> Id(ID("if " ^ printExp (translateTerm cond) ^ " {\n\t\t" ^ printExp (translateTerm t1) ^ "\n\t} else {\n\t\t" ^ printExp (translateTerm t2) ^ "\n\t}"))

and combineConditions cons =
  let rec inner cons acc =
    match cons with
      [] -> []
    | head :: tail ->
      let t =  translatePattern head acc in
      snd(t)@inner tail acc
  in
  inner cons []


and translatePattern pat (conditions : (term * term) list) =
  match pat with
    PVar(x, _) -> ((ID(x)), conditions)
  | PForm(fname, args) ->
    ((ID(fname)), (combineConditions args))
  | PMatch(t) ->
      let var = next_var() in
      (ID(var), (t, Var(var))::conditions)
  | PTuple(args) -> 
    let pargs = String.concat ", " (List.map (fun p -> printrId (fst (translatePattern p conditions))) args) in
    (ID("(" ^ pargs ^ ")"), conditions)

and translateArgs args =
  Exps( (List.map (fun a -> translateTerm a) args))

and toStructPattern fname args =
  StructPattern(ID(fname), List.map (fun a -> fst(translatePattern (a) [])) args)

and toFunction name exp =
  Exp(Id(ID(name)), exp)

and freshType t =
    "fresh_" ^ show_dtype t

and fresh name data_type  =
  SDeclExp(DeclExp((ID(name)), toFunction (freshType data_type) (Ids([]))))

and equals_condition_patterns = function
    (t1, t2)::[] -> OExp(translateTerm t1, Equals, translateTerm t2)
  | (t1, t2)::tail -> OExp(OExp(translateTerm t1, Equals, translateTerm t2),And, equals_condition_patterns tail)
  
and close_channels channels = List.map (fun (s, r) -> SExp(toFunction "close" (Id(ID("c_" ^ s ^ r))))) channels

and process channels = function
    LSend(ident, opt, t, local_type) ->
    let send = toFunction "send" (Exps([Id(ID("c_" ^ ident)); translateTerm t])) in
    SDeclExp(DeclExp(fst(translatePattern (PVar ("c_" ^ ident, None)) []), send))::process channels local_type
  | LNew (ident, data_type, local_type) -> (fresh ident data_type)::process channels local_type
  | LLet (PForm(fname, args), term, local_type) ->
    let patterns = List.map (fun a -> translatePattern (a) []) args in
    let conditions = List.flatten(List.map (fun x -> snd(x)) patterns) in
    let pats = List.map (fun x-> fst(x)) patterns in
    let strPtn = StructPattern(ID(fname), pats) in
    if(conditions = []) then SDeclExp(PatrExp(strPtn, translateTerm term))::process channels local_type
    else SDeclExp(PatrExp(strPtn, translateTerm term))::[SIfStatement(If((equals_condition_patterns conditions), BStmts(process channels local_type)))]
  | LLet (PMatch(mat), term, local_type) ->
    [SIfStatement(If(OExp(translateTerm mat, Equals, translateTerm term), BStmts(process channels local_type)))]
  | LLet (ident, term, local_type) ->
    let patterns = translatePattern ident [] in
    let conditions = snd(patterns) in
    if(conditions = []) then begin
      SDeclExp(DeclExp(fst(patterns), translateTerm term))::process channels local_type end
    else begin
      [SIfStatement(If((equals_condition_patterns conditions), BStmts(process channels local_type)))]
    end
  | LRecv (ident, opt, PVar(x, _), term, LLet (PForm(fname, args), Var(xx), local_type)) ->
    SDeclExp(DeclExp((ID("(c_" ^ ident ^ ", " ^ x ^ ")")), toFunction "recv" (Id(ID("c_" ^ ident)))))::SDeclExp(PatrExp(toStructPattern fname args, Id(ID(xx))))::process channels local_type
  | LRecv (ident, opt, PVar(x, _), term, local_type) ->  SDeclExp(DeclExp((ID("(c_" ^ ident ^ ", " ^ x ^ ")")), toFunction ("recv") (Id(ID("c_" ^ ident)))))::process channels local_type
  | LEvent (ident, term, local_type) -> process channels local_type
  | LLocalEnd -> close_channels channels
  | _ -> [End]

and typedIds t =
  let rec inner dt i =
    match dt with
      [] -> []
    | x::xs -> TypedID(ID("a" ^(string_of_int i)), Custom(show_dtype x)) :: (inner xs (i+1)) in
  TypedIDs(inner t 1)

and abstract_types (t : data_type list) =
  String.concat "" (List.map (fun tt -> show_dtype tt) t)

and func f =
  match f with
    (name,(args,ret,bool,[])) -> Function(ID(name), typedIds(args), Custom(show_dtype ret), BStmts([SExp(Unimplemented)]))
  | (name,(args,ret,bool,a_types)) -> Function(ID(name ^ "<" ^ (abstract_types a_types ^ abstract_traits) ^ ">"), typedIds(args), Custom(show_dtype ret), BStmts([SExp(Unimplemented)]))

and functions (f : (ident * (data_type list * data_type * bool * data_type list)) list) =
  List.map (fun f -> func f) f

and format f =
  match f with
  | (name, data_types) -> Struct(ID(name), RTypes(List.map (fun d-> Custom(show_dtype d)) data_types))

and formats f =
  List.map (fun f -> format f) f

and rust_formats form =
  printStructs(formats form)

and rust_handwritten =
  printHandWritten

and rust_functions (f : (ident * (data_type list * data_type * bool * data_type list)) list) t =
  let freshTypeFunctions = List.map (fun (typ) -> (freshType typ, ([], typ, false, []))) t in
  printFunctions (functions (f @ freshTypeFunctions))

let rec translateKnowledge principal knowledge acc =
  match knowledge with
  | [] -> acc
  | (t, dt, p) :: k ->
    if p = principal then 
      translateKnowledge principal k ((TypedID(ID(t), Custom(show_dtype dt))) :: acc)
    else translateKnowledge principal k acc
    
let rec translateChannels principal channels acc =
  match channels with
  | [] -> acc
  | (s, r) :: c ->
    if s = principal then
      translateChannels principal c (TypedID(ID("c_" ^ s ^ r), Custom("Chan<(), " ^ s ^ r ^ ">")) :: acc)
    else translateChannels principal c acc

let rust_process channels knowledge principal proc = (printStatements (SFunction(Function(ID(String.lowercase principal),TypedIDs(translateChannels principal channels [] @ translateKnowledge principal knowledge []), Empty,(BStmts(process channels proc))))))
