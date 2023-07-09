type t =
  | DuplicateType of string
  | TypeNotFound of string
  | QuerySchemaRequired
  | SchemaTypesMustBeDifferent of string

let show = function
  | DuplicateType s -> Fmt.str "Duplicate type found: %s" s
  | TypeNotFound s -> Fmt.str "Type not found: %s" s
  | QuerySchemaRequired -> Fmt.str "Schema requires at least a Query field"
  | SchemaTypesMustBeDifferent typ ->
    Fmt.str "Schema types must be different, %s used multiple times" typ
;;
