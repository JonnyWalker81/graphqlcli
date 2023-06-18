(* type Query { *)
(*   someField: String *)
(* } *)

type t =
  | Type
  | Name of string
  | LeftBrace
  | RightBrace
  | Colon
  | String
  | Illegal
  | Eof
[@@deriving show, eq, sexp]


let lookup_keyword name =
 match name with
 | "String" -> String
 | _ -> Illegal
