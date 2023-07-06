open Core

(* open Base *)
(* open Sexplib.Std *)

(* module Field = struct *)
(*   type field_type = { name : string } *)
(*   [@@deriving show { with_path = false }, sexp] *)
(* end *)

(* and non_null_type = *)
(*   | NonNullNamedType of graphql_type *)
(*   | NonNullListType of graphql_type list *)
(* [@@deriving show { with_path = false }, sexp] *)

(* module Values = struct *)
(*   (\* type 'a t = (string, 'a, String.comparator_witness) Map.t *\) *)
(*   type 'a t = (string, 'a) Hashtbl.t *)

(*   let pp ppr values = Fmt.str "map" *)

(*   (\* let pp pp_key pp_value ppf values = *\) *)
(*   (\*   Hashtbl.iteri values ~f:(fun ~key ~data -> *\) *)
(*   (\*       Format.fprintf ppf "@[<1>%a: %a@]@." pp_key key pp_value data) *\) *)
(*   (\* ;; *\) *)
(*   (\* let pp ppf values = *\) *)
(*   (\*   Hashtbl.iteri values ~f:(fun ~key ~data -> *\) *)
(*   (\*       Format.fprintf ppf "@[<1>%s: %s@]@." key data) *\) *)
(* end *)

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

  (* and string_map = (string, t, String.comparator_witness) Map.t *)
  (* [@@deriving show { with_path = false }, sexp] *)

  (* let to_string = function *)
  (*   | String s -> Fmt.strf "%s" s *)
  (*   | _ -> Fmt.str "" *)

  (* let pp ppf values = *)
  (*   Hashtbl.iter values ~f:(fun ~key ~data -> *)
  (*       Format.fprintf ppf "@[<1>%s: %s@]@." key (to_string data)) *)
end

module GraphqlType = struct
  type t =
    | NamedType of string
    | ListType of t
    | NonNullType of t
  [@@deriving show { with_path = false }, sexp]
end

module ArgumentDefiniton = struct
  type t =
    { name : string
    ; ty : GraphqlType.t
    ; default_value : Value.t option
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
    ; args : ArgumentDefiniton.t list option
    ; locations : DirectiveLocation.t list
    ; description : string option
    }
  [@@deriving show { with_path = false }, sexp]
end

module Field = struct
  type t =
    { name : string
    ; args : ArgumentDefiniton.t list option
    ; ty : GraphqlType.t
    ; description : string option
    }
  [@@deriving show { with_path = false }, sexp]
end

(* type scalar_type = { name : string; description : string option } *)
(* [@@deriving show { with_path = false }, sexp] *)

module ObjectType = struct
  type t =
    { name : string
    ; implements : string list
    ; fields : Field.t list
    ; description : string option
    }
  [@@deriving show { with_path = false }, sexp]
end

(* type union_member = { *)
(*   name: string; *)
(*   description: string option; *)
(* } *)
(* [@@deriving show { with_path = false }, sexp] *)

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
    ; description : string option
    }
  [@@deriving show { with_path = false }, sexp]
end

module EnumType = struct
  type t =
    { name : string
    ; values : BaseValue.t list
    ; description : string option
    }
  [@@deriving show { with_path = false }, sexp]
end

module InputType = struct
  type t =
    { name : string
    ; fields : Field.t list
    ; description : string option
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

(* type operation_arg = { name : string; value : object_field_type } *)
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
    ; description : string option
    }
  [@@deriving show { with_path = false }, sexp]
end

module TypeDefinition = struct
  type t =
    | Scalar of BaseValue.t
    | Object of ObjectType.t
    | Interface of InterfaceType.t
    | Union of UnionType.t
    | Enum of EnumType.t
    | Input of InputType.t
    | Comment of string
  [@@deriving show { with_path = false }, sexp]
end

(* type variable_definition = { name : string; ty : graphql_type} *)
(* [@@deriving show { with_path = false }, sexp] *)

(* type operation_type = { name : string; description: string option } *)
(* [@@deriving show { with_path = false }, sexp] *)

module Schema = struct
  type t =
    { query : BaseValue.t option
    ; mutation : BaseValue.t option
    ; subscription : BaseValue.t option
    ; directives : DirectiveDefinition.t list option
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

(* and selection_set = { *)
(*   set: selection_field list *)
(* } *)
(* [@@deriving show { with_path = false }, sexp] *)

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
