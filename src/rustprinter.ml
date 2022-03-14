open Rusttypes2

let handwritten =
"extern crate session_types;
use session_types::*;
use std::{marker};
use serde::{Serialize, Deserialize};
use std::thread;
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

let tabulate len = Printf.sprintf "%*s" (len*4) ""

let rec printStructPattern = function
      StructPattern(rId, args) -> printrId rId ^ "(" ^ String.concat ", " (List.map (fun a -> printrId a) args) ^ ")"

and printStructValues = function
    StructValue(x) -> printExp x


and printStruct = function
    Struct(ID(name), RTypes(types)) -> "#[derive(Serialize, Deserialize)]\n" ^ "struct " ^ name ^ "(" ^ printTypes types ^ ");"


and printStructs structs = String.concat "\n\n" (List.map (fun s-> printStruct s) structs)

and printrId = function
    ID(s) -> s

and printExp = function
      Id(id) -> printrId id
    | Ids([]) -> ""
    | Ids(lst) -> "(" ^ String.concat ", " (List.map (fun i-> printrId i) lst) ^ ")"
    | Ref(ref, exp) -> "&" ^ printExp exp
    | EStruct(id, StructValues(structValues)) -> printrId id ^ "(" ^ String.concat ", " (List.map (fun x-> printStructValues x) structValues) ^ ")"
    | Exp(exp1, exp2) -> printExp exp1 ^ "(" ^ printExp exp2 ^ ")"
    | Exps(exps) -> String.concat ", " (List.map (fun i-> printExp i) exps)
    | OExp(exp, Equals, exp2) -> "&" ^ printExp exp ^ " == " ^  "&" ^ printExp exp2
    | OExp(exp, And, exp2) -> printExp exp ^ " && " ^ printExp exp2
    | Unimplemented -> "unimplemented!()"


and printSDeclExp = function
    DeclExp (rId, exp) -> "let " ^ printrId rId ^ " = " ^ printExp exp
  | PatrExp (s, exp) -> "let " ^ printStructPattern s ^ " = " ^ printExp exp
(* Repr::from_repr *)
and printBlock tab = function
      Empty -> "{ }"
    | BStmts(lst) -> ("{\n" ^ tabulate tab) ^ String.concat (";\n" ^ tabulate tab) (List.map (fun s -> printStatements s) lst) ^ ("\n" ^ tabulate (tab - 1)  ^ "}")


and printType = function
    U8 -> "u8"
  | Custom(s) -> s

and printTypes t =
  String.concat ", " (List.map (fun typ -> printType typ) t)

and printTypedId = function
    TypedID (id,typ) -> printrId id ^ ": " ^ printType typ

and printTypedIds t =
  String.concat ", " (List.map (fun typ -> printTypedId typ) t)

and printFunction = function
    Function(id, TypedIDs(args), Empty, block) -> "fn " ^ printrId id ^ "(" ^ printTypedIds args ^ ")"^  " " ^ printBlock 1 block
  | Function(id, TypedIDs(args), typ, block) -> "fn " ^ printrId id ^ "(" ^ printTypedIds args ^ ") -> "^ printType typ ^ " " ^ printBlock 1 block
  | Function(id, args, Empty, block) -> "fn " ^ printrId id ^ "(" ^ ") " ^ printBlock 1 block

and printFunctions funs = String.concat "\n" (List.map (fun f-> printFunction f) funs)

and printIf st block =
  "if " ^ printExp st  ^ " " ^ printBlock 1 block

and printBranch = function
  Branch(rid, lb, rb) -> 
    let id = printrId rid in
    Printf.sprintf "let %s = match %s.offer() {\n\t\tLeft(%s) => %s,\n\t\tRight(%s) => %s\n\t}" id id id (printBlock 3 lb) id (printBlock 3 rb)

and printStatements = function
      SDeclExp(declExp) -> printSDeclExp declExp
    | SBlock(block) -> printBlock 1 block
    | SExp(exp) -> printExp exp
    | SFunction(rFunction) -> printFunction rFunction
    | SIfStatement(If(st, block)) -> printIf st block
    | SBranch(branch) -> printBranch branch
    | End -> ""
