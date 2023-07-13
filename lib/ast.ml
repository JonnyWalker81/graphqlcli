open Base

module Value = struct
  type t =
    | String of string
    | Int of int
    | Float of float
    | Boolean of bool
    | Enum of string
    | Null
    | Object of (string * t) list
    | List of t list
  [@@deriving show { with_path = false }, sexp]
end

module GraphqlType = struct
  type t =
    | NamedType of string
    | ListType of t
    | NonNullType of t
  [@@deriving show { with_path = false }, sexp]

  let rec name t =
    match t with
    | NamedType s -> s
    | ListType ty -> name ty
    | NonNullType ty -> name ty
  ;;
end

module DirectiveArg = struct
  type t =
    { name : string
    ; value : Value.t
    }
  [@@deriving show { with_path = false }, sexp]
end

module Directive = struct
  type t =
    { name : string
    ; args : DirectiveArg.t list
    }
  [@@deriving show { with_path = false }, sexp]
end

module ArgumentDefiniton = struct
  type t =
    { name : string
    ; ty : GraphqlType.t
    ; default_value : Value.t option
    ; directives : Directive.t list
    ; description : string option
    }
  [@@deriving show { with_path = false }, sexp]
end

module TypeSystemDirectiveLocation = struct
  type t =
    | SCHEMA
    | SCALAR
    | OBJECT
    | FIELD_DEFINITION
    | ARGUMENT_DEFINITION
    | INTERFACE
    | UNION
    | ENUM
    | ENUM_VALUE
    | INPUT_OBJECT
    | INPUT_FIELD_DEFINITION
  [@@deriving show { with_path = false }, sexp]
end

module ExecutableDirectiveLocation = struct
  type t =
    | QUERY
    | MUTATION
    | SUBSCRIPTION
    | FIELD
    | FRAGMENT_DEFINITION
    | FRAGMENT_SPREAD
    | INLINE_FRAGMENT
  [@@deriving show { with_path = false }, sexp]
end

module DirectiveLocation = struct
  type t =
    | ExecutableDirectiveLocation of ExecutableDirectiveLocation.t
    | TypeSystemDirectiveLocation of TypeSystemDirectiveLocation.t
  [@@deriving show { with_path = false }, sexp]
end

module DirectiveDefinition = struct
  type t =
    { name : string
    ; args : ArgumentDefiniton.t list
    ; locations : DirectiveLocation.t list
    ; description : string option
    ; builtin : bool
    }
  [@@deriving show { with_path = false }, sexp]
end

module Field = struct
  type t =
    { name : string
    ; args : ArgumentDefiniton.t list
    ; ty : GraphqlType.t
    ; directives : Directive.t list
    ; default_value : Value.t option
    ; description : string option
    }
  [@@deriving show { with_path = false }, sexp]
end

module ObjectType = struct
  type t =
    { name : string
    ; implements : string list
    ; fields : Field.t list
    ; description : string option
    ; builtin : bool
    }
  [@@deriving show { with_path = false }, sexp]
end

module BaseValue = struct
  type t =
    { name : string
    ; description : string option
    }
  [@@deriving show { with_path = false }, sexp]
end

module UnionType = struct
  type t =
    { name : string
    ; members : BaseValue.t list
    ; directives : Directive.t list
    ; description : string option
    ; builtin : bool
    }
  [@@deriving show { with_path = false }, sexp]
end

module EnumMember = struct
  type t =
    { name : string
    ; directives : Directive.t list
    ; description : string option
    }
  [@@deriving show { with_path = false }, sexp]
end

module EnumType = struct
  type t =
    { name : string
    ; values : EnumMember.t list
    ; directives : Directive.t list
    ; description : string option
    ; builtin : bool
    }
  [@@deriving show { with_path = false }, sexp]
end

module InputType = struct
  type t =
    { name : string
    ; fields : Field.t list
    ; directives : Directive.t list
    ; description : string option
    ; builtin : bool
    }
  [@@deriving show { with_path = false }, sexp]
end

module ObjectField = struct
  type t =
    { name : string
    ; value : ObjectType.t
    ; description : string option
    }
  [@@deriving show { with_path = false }, sexp]
end

module ObjectFieldType = struct
  type t =
    | String of string
    | Int of int
    | Float of float
    | Boolean of bool
    | Null
    | Enum of string
    | List of ObjectType.t list
    | Object of ObjectField.t list
  [@@deriving show { with_path = false }, sexp]
end

module OperationArg = struct
  type t =
    { name : string
    ; value : string
    }
  [@@deriving show { with_path = false }, sexp]
end

module InterfaceType = struct
  type t =
    { name : string
    ; fields : Field.t list
    ; directives : Directive.t list
    ; description : string option
    ; builtin : bool
    }
  [@@deriving show { with_path = false }, sexp]
end

module ScalarType = struct
  type t =
    { name : string
    ; directives : Directive.t list
    ; description : string option
    ; builtin : bool
    }
  [@@deriving show { with_path = false }, sexp]
end

module TypeDefinition = struct
  type t =
    | Scalar of ScalarType.t
    | Object of ObjectType.t
    | Interface of InterfaceType.t
    | Union of UnionType.t
    | Enum of EnumType.t
    | Input of InputType.t
    | Comment of string
  [@@deriving show { with_path = false }, sexp]

  let is_builtin = function
    | Scalar s -> s.builtin
    | Object o -> o.builtin
    | Interface i -> i.builtin
    | Union u -> u.builtin
    | Enum e -> e.builtin
    | Input i -> i.builtin
    | _ -> false
  ;;

  let name = function
    | Scalar s -> s.name
    | Object o -> o.name
    | Interface i -> i.name
    | Union u -> u.name
    | Enum e -> e.name
    | Input i -> i.name
    | _ -> ""
  ;;

  let fields = function
    | Object o -> o.fields
    | Interface i -> i.fields
    | Input i -> i.fields
    | _ -> []
  ;;
end

module Schema = struct
  type t =
    { query : BaseValue.t option
    ; mutation : BaseValue.t option
    ; subscription : BaseValue.t option
    ; directives : Directive.t list
    ; description : string option
    }
  [@@deriving show { with_path = false }, sexp]
end

module Operation = struct
  type t =
    | Query
    | Mutation
    | Subscription
  [@@deriving show { with_path = false }, sexp]
end

module SelectionField = struct
  type t =
    | Field of string
    | SpreadField of string
    | SubField of sub_field
  [@@deriving show { with_path = false }, sexp]

  and sub_field =
    { name : string
    ; fields : t list
    }
  [@@deriving show { with_path = false }, sexp]
end

module OperationDefinition = struct
  type t =
    { name : string
    ; operation : Operation.t
    ; args : OperationArg.t list option
    ; variables : ArgumentDefiniton.t list option
    ; selection : SelectionField.t list
    }
  [@@deriving show { with_path = false }, sexp]
end

module FragmentDefinition = struct
  type t =
    { name : string
    ; type_condition : string
    ; selection : SelectionField.t list
    }
  [@@deriving show { with_path = false }, sexp]
end

module ExecutableDefinition = struct
  type t =
    | OperationDefinition of OperationDefinition.t
    | FragmentDefinition of FragmentDefinition.t
  [@@deriving show { with_path = false }, sexp]
end

module Definition = struct
  type t =
    | TypeDefinition of TypeDefinition.t
    | Schema of Schema.t
    | ExecutableDefinition of ExecutableDefinition.t
    | Directive of DirectiveDefinition.t
  [@@deriving show { with_path = false }, sexp]
end

type node = Document of Definition.t list
