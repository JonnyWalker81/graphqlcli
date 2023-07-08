(* type Query { *)
(*   someField: String *)
(* } *)

type number_kind =
  | IntegerKind
  | FloatKind
[@@deriving show, eq, sexp]

type t =
  | Type
  | Name of string
  | Schema
  | Query
  | Mutation
  | Subscription
  | Enum
  | Union
  | Scalar
  | Input
  | LeftBrace
  | RightBrace
  | LeftParen
  | RightParen
  | LeftBracket
  | RightBracket
  | Exclamation
  | Equal
  | Comma
  | Colon
  | Ellipsis
  | Pipe
  | Ampersand
  | At
  | String
  | StringLiteral of string
  | Number of
      { kind : number_kind
      ; value : string
      }
  | Boolean of bool
  | True
  | False
  | Null
  | Semicolon
  | Comment of string
  | ID
  | Illegal
  | Eof
[@@deriving show, eq, sexp]

let lookup_keyword name =
  match name with
  (* | "String" -> String *)
  (* | "schema" -> Schema *)
  (* | "query" -> Query *)
  (* | "mutation" -> Mutation *)
  (* | "type" -> Type *)
  (* | "scalar" -> Scalar *)
  (* | "input" -> Input *)
  (* | "Int" -> Integer *)
  (* | "Float" -> Float *)
  (* | "Boolean" -> Boolean *)
  (* | "union" -> Union *)
  (* | "enum" -> Enum *)
  (* | "ID" -> ID *)
  | "true" -> True
  | "false" -> False
  | "null" -> Null
  | _ -> Name name
;;

let to_name tok =
  match tok with
  | Type -> "type"
  | Input -> "input"
  | Schema -> "schema"
  | _ -> failwith "unrecognized token"
;;
