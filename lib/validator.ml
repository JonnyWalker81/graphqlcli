[@@@warning "-27"]

open Ast
open Base

let ( let* ) res f = Base.Result.bind res ~f
let ( let+ ) res f = Base.Option.bind res ~f

type 'a validator_map = (string, 'a, String.comparator_witness) Map.t

type t =
  { ast : Ast.node
  ; schema : Schema.t option
  ; types : TypeDefinition.t validator_map
  ; directives : DirectiveDefinition.t validator_map
  ; possible_types : TypeDefinition.t validator_map
  ; implements : TypeDefinition.t validator_map
  }

let init ast =
  { ast
  ; schema = None
  ; types = Map.empty (module String)
  ; directives = Map.empty (module String)
  ; possible_types = Map.empty (module String)
  ; implements = Map.empty (module String)
  }
;;

let pp_validator_map m f =
  Map.iteri ~f:(fun ~key ~data -> Fmt.pr "%s -> %s@." key (f data)) m
;;

let pp_validator_schema validator =
  match validator.schema with
  | Some s -> Fmt.pr "%s@." (Schema.show s)
  | _ -> Fmt.pr "schema = None\n"
;;

let print_validator validator =
  pp_validator_schema validator;
  pp_validator_map validator.types TypeDefinition.show;
  pp_validator_map validator.directives DirectiveDefinition.show
;;

let is_valid_name name =
  match String.to_list name with
  | '_' :: '_' :: _ -> false
  | _ -> true
;;

let is_input_type typ =
  match typ with
  | TypeDefinition.Enum _ | TypeDefinition.Scalar _ | TypeDefinition.Input _ -> true
  | _ -> false
;;

let is_output_type = function
  | TypeDefinition.Enum _
  | TypeDefinition.Scalar _
  | TypeDefinition.Object _
  | TypeDefinition.Interface _
  | TypeDefinition.Union _ -> true
  | _ -> false
;;

let update_types_map validator key data =
  match Map.add ~key ~data validator.types with
  | `Ok m ->
    Ok
      { ast = validator.ast
      ; schema = validator.schema
      ; types = m
      ; directives = validator.directives
      ; possible_types = validator.possible_types
      ; implements = validator.implements
      }
  | `Duplicate -> let () = Fmt.pr "key: %s\n" key in
                  Error (Validation_error.DuplicateType key)
;;

let update_possible_types_map validator key data =
  match Map.add ~key ~data validator.possible_types with
  | `Ok m ->
    Ok
      { ast = validator.ast
      ; schema = validator.schema
      ; types = validator.types
      ; directives = validator.directives
      ; possible_types = m
      ; implements = validator.implements
      }
  | _ -> Ok validator
;;

let update_implements_map validator key data =
  match Map.add ~key ~data validator.implements with
  | `Ok m ->
    Ok
      { ast = validator.ast
      ; schema = validator.schema
      ; types = validator.types
      ; directives = validator.directives
      ; possible_types = validator.possible_types
      ; implements = m
      }
  | _ -> Ok validator
;;

let add_type_def validator def =
  if TypeDefinition.should_validate def then
    update_types_map validator (TypeDefinition.name def) def
  else
    Ok validator

let add_directive validator directive =
  match
    Map.add ~key:directive.DirectiveDefinition.name ~data:directive validator.directives
  with
  | `Ok m ->
    Ok
      { ast = validator.ast
      ; schema = validator.schema
      ; types = validator.types
      ; directives = m
      ; possible_types = validator.possible_types
      ; implements = validator.implements
      }
  | `Duplicate -> Error (Validation_error.DuplicateType directive.name)
;;

let rec validate validator node =
  let* validator = build_types_map validator node in
  (* let* validator = build_possible_types_map validator node in *)
  match node with
  | Ast.Document d -> validate_document validator d
  | _ -> Error (Validation_error.ExpectedType "node type")

and build_possible_types_map validator node =
  match node with
  | Ast.Document defs -> process_types validator defs
  | _ -> Error (Validation_error.ExpectedType "document (build_possible_types_map)")

and process_types validator defs =
  let rec process_types' defs =
    match defs with
    | [] -> Ok validator
    | def :: rest ->
      (match def with
      | Definition.TypeDefinition td ->
        let* validator = process_possible_types validator td in
        process_types' defs
      | _ -> process_types' rest)
  in
  process_types' defs

and process_possible_types validator def =
  match def with
  | TypeDefinition.Union u -> process_possible_types' validator u.members
  | _ -> Ok validator

and process_possible_types' validator defs =
  match defs with
  | [] -> Ok validator
  | def :: rest ->
    (match Map.find validator.types def.name with
    | Some t -> process_possible_types' validator rest
    | None -> Error (Validation_error.TypeNotFound def.name))

and build_types_map validator node =
  match node with
  | Ast.Document defs ->
    (match count_schemas validator defs with
    | i when i > 1 -> Error Validation_error.MultipleSchemaEntryPoints
    | _ -> process_document validator defs)
  | _ -> Error (Validation_error.ExpectedType "expected document (build_types_map)")

and process_document validator defs =
  match defs with
  | [] -> Ok validator
  | def :: rest ->
    (match def with
    | Definition.TypeDefinition def ->
      let* validator = add_type_def validator def in
      process_document validator rest
    | Definition.Directive directive ->
      let* validator = add_directive validator directive in
      process_document validator rest
    | Definition.Schema s ->
      process_document
        { ast = validator.ast
        ; schema = Some s
        ; types = validator.types
        ; directives = validator.directives
        ; possible_types = validator.possible_types
        ; implements = validator.implements
        }
        rest
    | _ -> Ok validator)

and count_schemas validator defs =
  List.count defs ~f:(fun d ->
      match d with
      | Definition.Schema _ -> true
      | _ -> false)

and validate_document validator doc =
  let rec validate_document' validator defs =
    match defs with
    | [] -> Ok validator
    | def :: rest ->
      (match validate_def validator def with
      | Ok validator -> validate_document' validator rest
      | e -> e)
  in
  validate_document' validator doc

and validate_def validator = function
  | Definition.TypeDefinition td -> validate_type_def validator td
  | Definition.Directive d -> validate_directive validator d
  | Definition.Schema s -> validate_schema validator s
  | _ -> failwith "unexpected def"

and validate_schema validator schema =
  let* validator =
    match schema.query with
    | None -> Error Validation_error.QuerySchemaRequired
    | _ -> Ok validator
  in
  let seen = Map.empty (module String) in
  let* validator, seen = validate_schema_type validator schema.query seen in
  let* validator, seen = validate_schema_type validator schema.mutation seen in
  let* validator, seen = validate_schema_type validator schema.subscription seen in
  Ok validator

and validate_schema_type validator typ seen =
  match typ with
  | None -> Ok (validator, seen)
  | Some s ->
    let* validator =
      match Map.find validator.types s.name with
      | Some _ -> Ok validator
      | None -> Error (Validation_error.TypeNotFound s.name)
    in
    (match Map.add ~key:s.name ~data:s.name seen with
    | `Ok m ->
      let seen = m in
      Ok (validator, seen)
    | `Duplicate -> Error (Validation_error.SchemaTypesMustBeDifferent s.name))

and validate_directive validator directive =
  let* validator =
    if not directive.builtin then validate_name validator directive.name else Ok validator
  in
  let* validator = validate_args validator directive.args directive.name in
  let* validator =
    validate_directive_not_referenced validator directive.args directive.name
  in
  Ok validator

and validate_directive_not_referenced validator args name =
  match
    List.find args ~f:(fun a ->
        match List.find a.directives ~f:(fun d -> String.compare d.name name = 0) with
        | Some _ -> true
        | _ -> false)
  with
  | Some a -> Error (Validation_error.DirectiveMustNotReferenceItself name)
  | None -> Ok validator

and validate_args validator args current_directive_name =
  match
    List.find args ~f:(fun a ->
        let typ = Map.find validator.types (GraphqlType.name a.ty) in
        match typ with
        | None -> true
        | Some typ -> not (is_input_type typ))
  with
  | Some a ->
    Error (Validation_error.DirectiveArgMustBeInputType (current_directive_name, a.name))
  | None -> Ok validator

and validate_name validator name =
  match is_valid_name name with
  | false -> Error (Validation_error.NameShouldNotStartWithUnderscores name)
  | _ -> Ok validator

and validate_type_def validator td =
  let* validator =
    if not (TypeDefinition.is_builtin td)
    then validate_name validator (TypeDefinition.name td)
    else Ok validator
  in
  let* validator = validate_valid_type validator (TypeDefinition.name td) in
  let* validator = validate_type_fields validator td in
  Ok validator

and validate_valid_type validator name =
  match name with
  | "" -> Ok validator
  | _ ->
    (match Map.find validator.types name with
    | None -> Error (Validation_error.TypeNotFound name)
    | Some _ -> Ok validator)

and validate_type_fields validator td =
  let* validator = validate_type_fields_count validator td in
  validate_fields validator td

and validate_type_fields_count validator td =
  match td with
  | TypeDefinition.Object _ | TypeDefinition.Interface _ | TypeDefinition.Input _ ->
    (match List.length (TypeDefinition.fields td) = 0 with
    | true ->
      Error (Validation_error.TypeMustHaveOneOrMoreFields (TypeDefinition.name td))
    | false -> Ok validator)
  | _ -> Ok validator

and validate_fields validator td =
  let fields =
    match td with
    | TypeDefinition.Object o -> o.fields
    | _ -> []
  in
  validate_each_field validator fields

and validate_each_field validator fields =
  match fields with
  | [] -> Ok validator
  | f :: rest ->
    let* validator = validate_name validator f.name in
    let* validator = validate_field_output_type validator f in
    let* validator = validate_valid_type validator (GraphqlType.name f.ty) in
    let* validator = validate_field_args validator f.args f.name in
    validate_each_field validator rest

and validate_field_output_type validator field =
  match
    match Map.find validator.types (GraphqlType.name field.ty) with
    | None -> true
    | Some typ -> is_output_type typ
  with
  | false ->
    Error (Validation_error.FieldMustBeOutputType (field.name, GraphqlType.name field.ty))
  | true -> Ok validator

and validate_field_args validator args field_name =
  let* validator = validate_field_args_name validator args field_name in
  let* validator = validate_field_args_type validator args in
  validate_field_args_input_type validator args field_name

and validate_field_args_input_type validator args field_name =
  match
    List.find args ~f:(fun a ->
        match Map.find validator.types (GraphqlType.name a.ty) with
        | None -> true
        | Some typ -> not (is_input_type typ))
  with
  | Some a -> Error (Validation_error.FieldArgMustBeInputType (field_name, a.name))
  | None -> Ok validator

and validate_field_args_type validator args =
  match
    List.find args ~f:(fun a ->
        match validate_valid_type validator (GraphqlType.name a.ty) with
        | Error e -> true
        | _ -> false)
  with
  | Some a -> Error (Validation_error.TypeNotFound (GraphqlType.name a.ty))
  | None -> Ok validator

and validate_field_args_name validator args field_name =
  match
    List.find args ~f:(fun a ->
        match validate_name validator a.name with
        | Error e -> true
        | _ -> false)
  with
  | Some a -> Error (Validation_error.InvalidFieldName (a.name, field_name))
  | None -> Ok validator
;;

module Test = struct
  open Core

  let valiate_document input =
    let prelude = In_channel.read_all "../../../prelude.graphql" in
    let program = Parser.parse_documents [ true, prelude; false, input ] in
    match program with
    | Ok program ->
      let validator = init program in
      let validator = validate validator program in
      (match validator with
      | Ok validator -> Fmt.pr "Valid Document"
      | Error msg -> Fmt.pr "Validation Error: %s\n" (Validation_error.show msg))
    | Error (line, err) ->
      Fmt.pr "error parsing program(%d): %s" line (Parse_error.show err)
  ;;

  let%expect_test "testValidDocument" =
    let input =
      {|

    "directive desc"
directive @example(arg: String) on FIELD_DEFINITION


        schema {
        query: Query
        mutation: Mutation
}

      type Query {
        foo: String
      }

        type Mutation {
        bar(arg1: String): Int!
}

      type Foo {
        bar: String
      }

      type Baz {
        qux: Foo!
      }

|}
    in
    valiate_document input;
    [%expect {| Valid Document |}]
  ;;

  let%expect_test "testSchemaTypeNotFound" =
    let input = {|
      schema {
        query: MyQuery
      }
       |} in
    valiate_document input;
    [%expect {| Validation Error: Type not found: MyQuery |}]
  ;;

  let%expect_test "testSchemaDuplicateTypes" =
    let input =
      {|
      schema {
        query: MyQuery
        mutation: MyQuery
      }

      type MyQuery {
         field1: String
        }
       |}
    in
    valiate_document input;
    [%expect
      {| Validation Error: Schema types must be different, MyQuery used multiple times |}]
  ;;

  let%expect_test "testMultipleSchemas" =
    let input =
      {|
      schema {
        query: MyQuery
      }

      schema {
        query: MyQuery
      }

      type MyQuery {
         field1: String
        }
       |}
    in
    valiate_document input;
    [%expect
      {| Validation Error: Cannot have multiple schema entry points, consider schema extensions instead. |}]
  ;;

  let%expect_test "testTypeNameValidation" =
    let input = {|
      type __Query {
        foo: String
      }
       |} in
    valiate_document input;
    [%expect
      {| Validation Error: Name '__Query' must not begin with '__', which is reserved by GraphQL introspection.`, |}]
  ;;

  let%expect_test "testFieldArgNameValidation" =
    let input =
      {|
      type Query {
        foo(__bar: Boolean): String
      }
       |}
    in
    valiate_document input;
    [%expect
      {| Validation Error: Invalid Field Name, '__bar', for type 'foo', cannot start with undersores. |}]
  ;;

  let%expect_test "testValidTypeValidation" =
    let input =
      {|
      type Query {
        foo(__bar: Boolean): ReturnType
      }
       |}
    in
    valiate_document input;
    [%expect {| Validation Error: Type not found: ReturnType |}]
  ;;

  let%expect_test "testValidTypeValidation" =
    let input = {|
      type Query {
      }
       |} in
    valiate_document input;
    [%expect {| Validation Error: Type, 'Query', needs to have one or more fields. |}]
  ;;

  let%expect_test "testArgTypeNotFound" =
    let input =
      {|
      type Query {
      foo(arg1: [[Bar]]!): String
      }
       |}
    in
    valiate_document input;
    [%expect {| Validation Error: Type not found: Bar |}]
  ;;

  let%expect_test "testArgNotInputType" =
    let input =
      {|
      type Obj {
      fld(arg1: String): Int
      }

      type Query {
      foo(arg1: Obj!): String
      }
       |}
    in
    valiate_document input;
    [%expect {| Validation Error: Field, 'foo', with arg 'arg1', must be input type. |}]
  ;;

  let%expect_test "testFielfNotOutputType" =
    let input =
      {|
      type Obj {
        fld(arg1: String): Int
      }

      input FooInput {
        arg1: String
      }

      type Query {
        foo(arg1: Obj!): FooInput
      }
       |}
    in
    valiate_document input;
    [%expect
      {| Validation Error: Field, 'foo', with type 'FooInput', must be output type. |}]
  ;;
end
