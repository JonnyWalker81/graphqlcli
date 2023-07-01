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

type argument_definition = {
  name : string;
  ty : graphql_type;
  description : string option;
}
[@@deriving show { with_path = false }, sexp]

type field = {
  name : string;
  args : argument_definition list option;
  ty : graphql_type;
  description : string option;
}
[@@deriving show { with_path = false }, sexp]

(* type scalar_type = { name : string; description : string option } *)
(* [@@deriving show { with_path = false }, sexp] *)

type object_type = {
  name : string;
  fields : field list;
  description : string option;
}
[@@deriving show { with_path = false }, sexp]

(* type union_member = { *)
(*   name: string; *)
(*   description: string option; *)
(* } *)
(* [@@deriving show { with_path = false }, sexp] *)

type op_union_enum_value = { name : string; description : string option }
[@@deriving show { with_path = false }, sexp]

type union_type = {
  name : string;
  members : op_union_enum_value list;
  description : string option;
}
[@@deriving show { with_path = false }, sexp]

type enum_type = {
  name : string;
  values : op_union_enum_value list;
  description : string option;
}
[@@deriving show { with_path = false }, sexp]

type input_type = {
  name : string;
  fields : field list;
  description : string option;
}
[@@deriving show { with_path = false }, sexp]

type object_field = {
  name : string;
  value : object_type;
  description : string option;
}
[@@deriving show { with_path = false }, sexp]

and object_field_type =
  | String of string
  | Int of int
  | Float of float
  | Boolean of bool
  | Null
  | Enum of string
  | List of object_type list
  | Object of object_field list
[@@deriving show { with_path = false }, sexp]

(* type operation_arg = { name : string; value : object_field_type } *)
type operation_arg = { name : string; value : string }
[@@deriving show { with_path = false }, sexp]

type type_definition =
  | Scalar of op_union_enum_value
  | Object of object_type
  | Union of union_type
  | Enum of enum_type
  | Input of input_type
  | Comment of string
[@@deriving show { with_path = false }, sexp]

(* type variable_definition = { name : string; ty : graphql_type} *)
(* [@@deriving show { with_path = false }, sexp] *)

(* type operation_type = { name : string; description: string option } *)
(* [@@deriving show { with_path = false }, sexp] *)

type schema = {
  query : op_union_enum_value option;
  mutation : op_union_enum_value option;
  subscription : op_union_enum_value option;
  description : string option;
}
[@@deriving show { with_path = false }, sexp]

type operation = Query | Mutation | Subscription
[@@deriving show { with_path = false }, sexp]

type sub_field = { name : string; fields : selection_field list }
[@@deriving show { with_path = false }, sexp]

and selection_field =
  | Field of string
  | SpreadField of string
  | SubField of sub_field
[@@deriving show { with_path = false }, sexp]

(* and selection_set = { *)
(*   set: selection_field list *)
(* } *)
(* [@@deriving show { with_path = false }, sexp] *)

type operation_definition = {
  name : string;
  operation : operation;
  args : operation_arg list option;
  variables : argument_definition list option;
  selection : selection_field list;
}
[@@deriving show { with_path = false }, sexp]

type fragment_definition = {
  name : string;
  type_condition : string;
  selection : selection_field list;
}
[@@deriving show { with_path = false }, sexp]

type executable_definition =
  | OperationDefinition of operation_definition
  | FragmentDefinition of fragment_definition
[@@deriving show { with_path = false }, sexp]

type definition =
  | TypeDefinition of type_definition
  | Schema of schema
  | ExecutableDefinition of executable_definition
[@@deriving show { with_path = false }, sexp]

type node = Document of definition list
