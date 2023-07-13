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
  }

let init ast =
  { ast
  ; schema = None
  ; types = Map.empty (module String)
  ; directives = Map.empty (module String)
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
  | _ ->
    let () = Fmt.pr "%s@.\n" (TypeDefinition.show typ) in
    false
;;

let update_types_map validator key data =
  let dup = Map.add ~key ~data validator.types in
  match dup with
  | `Ok m ->
    Ok
      { ast = validator.ast
      ; schema = validator.schema
      ; types = m
      ; directives = validator.directives
      }
  | `Duplicate -> Error (Parse_error.DuplicateType key)
;;

let add_type_def validator def =
  match def with
  | TypeDefinition.Object o -> update_types_map validator o.name def
  | TypeDefinition.Scalar s -> update_types_map validator s.name def
  | TypeDefinition.Enum e -> update_types_map validator e.name def
  | TypeDefinition.Union u -> update_types_map validator u.name def
  | TypeDefinition.Interface i -> update_types_map validator i.name def
  | TypeDefinition.Input i -> update_types_map validator i.name def
  | _ -> Ok validator
;;

let add_directive validator directive =
  let dup =
    Map.add ~key:directive.DirectiveDefinition.name ~data:directive validator.directives
  in
  match dup with
  | `Ok m ->
    Ok
      { ast = validator.ast
      ; schema = validator.schema
      ; types = validator.types
      ; directives = m
      }
  | `Duplicate -> Error (Parse_error.DuplicateType directive.name)
;;

let rec validate validator node =
  let* validator = build_map validator node in
  match node with
  | Ast.Document d -> validate_document validator d
  | _ -> failwith "unexpected node"

and build_map validator node =
  match node with
  | Ast.Document defs ->
    let rec process_document validator defs =
      match defs with
      | [] -> Ok validator
      | def :: rest ->
        (match def with
        | Definition.TypeDefinition def ->
          (match add_type_def validator def with
          | Ok validator -> process_document validator rest
          | e -> e)
        | Definition.Directive directive ->
          (match add_directive validator directive with
          | Ok validator -> process_document validator rest
          | e -> e)
        | Definition.Schema s ->
          process_document
            { ast = validator.ast
            ; schema = Some s
            ; types = validator.types
            ; directives = validator.directives
            }
            rest
        | _ -> Ok validator)
    in
    process_document validator defs
  | _ -> failwith "expected document"

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

and validate_def validator def =
  match def with
  | Definition.TypeDefinition td -> validate_type_def validator td
  | Definition.Directive d -> validate_directive validator d
  | Definition.Schema s -> validate_schema validator s
  | _ -> failwith "unexpected def"

and validate_schema validator schema =
  let* validator =
    match schema.query with
    | None -> Error Parse_error.QuerySchemaRequired
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
      | None -> Error (Parse_error.TypeNotFound s.name)
    in
    (match Map.add ~key:s.name ~data:s.name seen with
    | `Ok m ->
      let seen = m in
      Ok (validator, seen)
    | `Duplicate -> Error (Parse_error.SchemaTypesMustBeDifferent s.name))

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
  | Some a -> Error (Parse_error.DirectiveMustNotReferenceItself name)
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
    Error (Parse_error.DirectiveArgMustBeInputType (current_directive_name, a.name))
  | None -> Ok validator

and validate_name validator name =
  match is_valid_name name with
  | false -> Error (Parse_error.NameShouldNotStartWithUnderscores name)
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
    | None -> Error (Parse_error.TypeNotFound name)
    | Some _ -> Ok validator)

and validate_type_fields validator td =
  validate_fields validator (TypeDefinition.fields td)

and validate_fields validator fields =
  match fields with
  | [] -> Ok validator
  | f :: rest ->
    let* validator = validate_name validator f.name in
    let* validator = validate_valid_type validator (GraphqlType.name f.ty) in
    let* validator = validate_field_args validator f.args f.name in
    validate_fields validator rest

and validate_field_args validator args field_name =
  let* validator = validate_field_args_name validator args field_name in
  let* validator = validate_field_args_type validator args in
  validate_field_args_input_type validator args field_name

and validate_field_args_input_type validator args field_name =
  match
    List.find args ~f:(fun a ->
        let typ = Map.find validator.types (GraphqlType.name a.ty) in
        match typ with
        | None -> true
        | Some typ -> not (is_input_type typ))
  with
  | Some a -> Error (Parse_error.FieldArgMustBeInputType (field_name, a.name))
  | None -> Ok validator

and validate_field_args_type validator args =
  match
    List.find args ~f:(fun a ->
        match validate_valid_type validator (GraphqlType.name a.ty) with
        | Error e -> true
        | _ -> false)
  with
  | Some a -> Error (Parse_error.TypeNotFound a.name)
  | None -> Ok validator

and validate_field_args_name validator args field_name =
  match
    List.find args ~f:(fun a ->
        match validate_name validator a.name with
        | Error e -> true
        | _ -> false)
  with
  | Some a -> Error (Parse_error.InvalidFieldName (a.name, field_name))
  | None -> Ok validator
;;

module Test = struct
  open Core

  let valiate_document input =
    let prelude = In_channel.read_all "../../../prelude.graphql" in
    let program = Parser.parse_documents [ true, prelude; false, input ] in
    (* match Ok(Ast.Document program) with *)
    match program with
    | Ok program ->
      let validator = init program in
      let validator = validate validator program in
      (match validator with
      | Ok validator -> print_validator validator
      | Error msg -> Fmt.pr "Validation Error: %s\n" (Parse_error.show msg))
    | Error msg -> Fmt.failwith "error parsing program...%s" msg
  ;;

  let%expect_test "testTypeCheck" =
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
    [%expect {|

|}]
  ;;
end
