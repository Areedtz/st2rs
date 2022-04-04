open Rusttypes2

let sprintf = Printf.sprintf

let handwritten =
"extern crate session_types;
use session_types::*;
use std::{marker};
use serde::{Serialize, Deserialize};
use std::thread;
use std::process;
use std::borrow::Borrow;
use std::marker::PhantomData;
use serde::de::DeserializeOwned;

#[derive(Serialize, Deserialize)]
pub struct Repr<T>(Vec<u8>, PhantomData<T>);

impl<T : Serialize + DeserializeOwned> Represent<T> for Repr<T> {
    fn from_repr(b: Repr<T>) -> T { bincode::deserialize(&b.0[..]).unwrap() }
    fn to_repr(b: T) -> Repr<T> { Repr(bincode::serialize(&b).unwrap(), PhantomData) }
}

trait Represent<T> {
    fn from_repr(_: Repr<T>) -> T;
    fn to_repr(_: T) -> Repr<T>;
}

fn send<E, P, A: marker::Send + Serialize + DeserializeOwned + 'static>(c: Chan<E, Send<Repr<A>, P>>, v: A) -> Chan<E, P> { c.send(Repr::to_repr(v)) }
fn recv<E, P, A: marker::Send + Serialize + DeserializeOwned + 'static>(c: Chan<E, Recv<Repr<A>, P>>) -> (Chan<E, P>, A) { let (c, x) = c.recv(); (c, Repr::from_repr(x)) }
fn close<E>(c: Chan<E, Eps>) { c.close() }
"

let printHandWritten = handwritten

let rec tabulate len =
  match len with
  | 0 -> ""
  | _ -> "\t" ^ tabulate (len-1)

let rec printStructPattern = function
      StructPattern(rId, args) -> printrId rId ^ "(" ^ String.concat ", " (List.map (fun a -> printrId a) args) ^ ")"

and printStructValues = function
    StructValue(x) -> printExp 0 x

and printStruct = function
    Struct(ID(name), RTypes(types)) -> "#[derive(Serialize, Deserialize)]\n" ^ "struct " ^ name ^ "(" ^ printTypes types ^ ");"

and printStructs structs = String.concat "\n\n" (List.map (fun s-> printStruct s) structs)

and printrId = function
    ID(s) -> s

and printExp tab = function
      Id(id) -> printrId id
    | Ids([]) -> ""
    | Ids(lst) -> "(" ^ String.concat ", " (List.map (fun i-> printrId i) lst) ^ ")"
    | Ref(ref, exp) -> "&" ^ printExp tab exp
    | EStruct(id, StructValues(structValues)) -> printrId id ^ "(" ^ String.concat ", " (List.map (fun x-> printStructValues x) structValues) ^ ")"
    | Exp(exp1, exp2) -> printExp tab exp1 ^ "(" ^ printExp tab exp2 ^ ")"
    | Exps(exps) -> String.concat ", " (List.map (fun i-> printExp tab i) exps)
    | OExp(exp, Equals, exp2) -> printExp tab exp ^ " == " ^  printExp tab exp2
    | OExp(exp, And, exp2) -> printExp tab exp ^ " && " ^ printExp tab exp2
    | OExp(exp, Or, exp2) -> printExp tab exp ^ " || " ^ printExp tab exp2
    | If(cond, block1, block2) -> sprintf "if %s %s else %s" (printExp tab cond) (printBlock (tab+1) block1) (printBlock (tab+1) block2)
    | Unimplemented -> "unimplemented!()"

and printSDeclExp tab = function
    DeclExp (rId, exp) -> "let " ^ printrId rId ^ " = " ^ printExp tab exp
  | PatrExp (s, exp) -> "let " ^ printStructPattern s ^ " = " ^ printExp tab exp

and printBlock tab = function
      Empty -> "{ }"
    | BStmts(lst) when List.length lst = 0 -> "{ }"
    | BStmts(lst) -> 
      let s = String.concat ("\n" ^ tabulate tab) (List.map (fun s -> 
        match s with 
        | SBranch(_) -> printStatements tab s
        | _ -> printStatements tab s ^ ";"
      ) lst) in
      ("{\n" ^ tabulate tab) ^ s ^ ("\n" ^ tabulate (tab - 1)  ^ "}")

and printType = function
    U8 -> "u8"
  | U16 -> "u16"
  | U32 -> "u32"
  | U64 -> "u64"
  | I8 -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | F32 -> "f32"
  | F64 -> "f64"
  | Isize -> "isize"
  | Usize -> "usize"
  | Str -> "str"
  | Boolean -> "bool"
  | Empty -> ""
  | Custom(s) -> s

and printTypes t =
  String.concat ", " (List.map (fun typ -> printType typ) t)

and printTypedId = function
    TypedID (id,typ) -> printrId id ^ ": " ^ printType typ

and printTypedIds t =
  String.concat ", " (List.map (fun typ -> printTypedId typ) t)

and printFunction = function
    Function(id, TypedIDs(args), Empty, block) -> "fn " ^ printrId id ^ "(" ^ printTypedIds args ^ ")" ^  " " ^ printBlock 1 block
  | Function(id, TypedIDs(args), typ, block) -> "fn " ^ printrId id ^ "(" ^ printTypedIds args ^ ") -> " ^ printType typ ^ " " ^ printBlock 1 block

and printFunctions funs = String.concat "\n" (List.map (fun f-> printFunction f) funs)

and printIf tab ifst =
  match ifst with
  | If(cond, thenb, Empty) -> "if (" ^ printExp tab cond ^ ") " ^ printBlock (tab+1) thenb
  | If(cond, thenb, elseb) -> "if (" ^ printExp tab cond ^ ") " ^ printBlock (tab+1) thenb ^ " else " ^ printBlock (tab+1) elseb

and printStmtList tab lst = tabulate tab ^ String.concat (";\n" ^ (tabulate tab)) (List.map (fun s -> printStatements tab s) lst)

and printBranch tab = function
  Offer(rid, lb, rb) -> 
    let id = printrId rid in
    let mtch = sprintf "match %s.offer() {\n" id in
    let left = sprintf "%sLeft(%s) => %s,\n" (tabulate (tab+1)) id (printBlock (tab+2) lb) in
    let right = sprintf "%sRight(%s) => %s" (tabulate (tab+1)) id (printBlock (tab+2) rb) in
    sprintf "%s%s%s\n%s}" mtch left right (tabulate (tab))
  | Choose(rid, lb, rb) ->
    let id = printrId rid in
    let comment = sprintf "// Need to make a choice on %s. Either %s.sel1() or %s.sel2()\n" id id id in
    let left = sprintf "%s/*\n%s;\n%s*/\n" (tabulate tab) (printStmtList tab lb) (tabulate tab) in
    let right = sprintf "%s/*\n%s;\n%s*/" (tabulate tab) (printStmtList tab rb) (tabulate tab) in
    sprintf "%s%s\n%s" comment left right

and printStatements tab = function
      SDeclExp(declExp) -> printSDeclExp tab declExp
    | SBlock(block) -> printBlock tab block
    | SExp(exp) -> printExp tab exp
    | SFunction(rFunction) -> printFunction rFunction
    | SIfStatement(ifStatement) -> printIf tab ifStatement
    | SBranch(branch) -> printBranch tab branch
    | End -> ""
