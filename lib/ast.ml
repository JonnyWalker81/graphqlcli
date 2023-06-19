type field_type = {
  name: string
}
[@@deriving show { with_path = false }, sexp]

type field = {
  name: string;
  ty: field_type
}
[@@deriving show { with_path = false }, sexp]

type object_type = {
  name: string;
  fields: field list
}
[@@deriving show { with_path = false }, sexp]

type type_definition =
  | Scalar of string
  | Object of object_type
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
