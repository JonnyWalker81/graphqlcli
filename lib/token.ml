(* type Query { *)
(*   someField: String *)
(* } *)

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
  | Pipe
  | String
  | StringLiteral of string
  | Float
  | Integer
  | Boolean
  | Semicolon
  | Comment of string
  | ID
  | Illegal
  | Eof
[@@deriving show, eq, sexp]

let lookup_keyword name =
  match name with
  (* | "String" -> String *)
  | "schema" -> Schema
  (* | "query" -> Query *)
  (* | "mutation" -> Mutation *)
  | "type" -> Type
  | "scalar" -> Scalar
  | "input" -> Input
  (* | "Int" -> Integer *)
  (* | "Float" -> Float *)
  (* | "Boolean" -> Boolean *)
  | "union" -> Union
  | "enum" -> Enum
  (* | "ID" -> ID *)
  | _ -> Name name
