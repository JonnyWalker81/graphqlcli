type t =
  | DuplicateType of string
  | TypeNotFound of string
  | QuerySchemaRequired
  | SchemaTypesMustBeDifferent of string
  | NameShouldNotStartWithUnderscores of string
  | DirectiveArgMustBeInputType of (string * string)

let show = function
  | DuplicateType s -> Fmt.str "Duplicate type found: %s" s
  | TypeNotFound s -> Fmt.str "Type not found: %s" s
  | QuerySchemaRequired -> Fmt.str "Schema requires at least a Query field"
  | SchemaTypesMustBeDifferent typ ->
    Fmt.str "Schema types must be different, %s used multiple times" typ
  | NameShouldNotStartWithUnderscores s ->
    Fmt.str
      "Name '%s' must not begin with '__', which is reserved by GraphQL introspection.`, "
      s
  | DirectiveArgMustBeInputType (dir, typ) ->
    Fmt.str "Directive, '%s', with arg '%s', must be input type." dir typ
;;
