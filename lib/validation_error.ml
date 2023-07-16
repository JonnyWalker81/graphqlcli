type t =
  | DuplicateType of string
  | TypeNotFound of string
  | QuerySchemaRequired
  | SchemaTypesMustBeDifferent of string
  | NameShouldNotStartWithUnderscores of string
  | DirectiveArgMustBeInputType of (string * string)
  | DirectiveMustNotReferenceItself of string
  | InvalidFieldName of (string * string)
  | FieldArgMustBeInputType of (string * string)
  | TypeMustHaveOneOrMoreFields of string
  | FieldMustBeOutputType of (string * string)
  | MultipleSchemaEntryPoints
  | ExpectedType of string

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
  | DirectiveMustNotReferenceItself name ->
    Fmt.str "Directive arg, '%s', should not use its own directive." name
  | InvalidFieldName (a, f) ->
    Fmt.str "Invalid Field Name, '%s', for type '%s', cannot start with undersores." a f
  | FieldArgMustBeInputType (dir, typ) ->
    Fmt.str "Field, '%s', with arg '%s', must be input type." dir typ
  | TypeMustHaveOneOrMoreFields name ->
    Fmt.str "Type, '%s', needs to have one or more fields." name
  | FieldMustBeOutputType (field, name) ->
    Fmt.str "Field, '%s', with type '%s', must be output type." field name
  | MultipleSchemaEntryPoints ->
    Fmt.str
      "Cannot have multiple schema entry points, consider schema extensions instead."
  | ExpectedType t -> Fmt.str "Expected: %s" t
;;
