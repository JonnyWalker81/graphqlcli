type field_type = { name : string }
[@@deriving show { with_path = false }, sexp]

(* and non_null_type = *)
(*   | NonNullNamedType of graphql_type *)
(*   | NonNullListType of graphql_type list *)
(* [@@deriving show { with_path = false }, sexp] *)
and graphql_type =
  | NamedType of string
  | ListType of graphql_type
  | NonNullType of graphql_type
[@@deriving show { with_path = false }, sexp]

type argument_definition = { name : string; ty : graphql_type }
[@@deriving show { with_path = false }, sexp]

type field = {
  name : string;
  args : argument_definition list option;
  ty : graphql_type;
}
[@@deriving show { with_path = false }, sexp]

type object_type = { name : string; fields : field list }
[@@deriving show { with_path = false }, sexp]

type union_type = { name : string; members : string list }
[@@deriving show { with_path = false }, sexp]

type enum_type = { name : string; values : string list }
[@@deriving show { with_path = false }, sexp]

type input_type = { name : string; fields: field list }
[@@deriving show { with_path = false }, sexp]

type type_definition =
  | Scalar of string
  | Object of object_type
  | Union of union_type
  | Enum of enum_type
  | Input of input_type
  | Comment of string
[@@deriving show { with_path = false }, sexp]

type operation_type = { name : string }
[@@deriving show { with_path = false }, sexp]

type schema = {
  query : operation_type option;
  mutation : operation_type option;
  subscription : operation_type option;
}
[@@deriving show { with_path = false }, sexp]

type definition = TypeDefinition of type_definition | Schema of schema
[@@deriving show { with_path = false }, sexp]

type node = Document of definition list
