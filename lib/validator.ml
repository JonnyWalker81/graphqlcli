[@@@warning "-27"]

open Ast
open Base

let ( let* ) res f = Base.Result.bind res ~f

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

let add_type_def validator def =
  match def with
  | TypeDefinition.Object o ->
    let dup = Map.add ~key:o.name ~data:def validator.types in
    (match dup with
    | `Ok m ->
      Ok
        { ast = validator.ast
        ; schema = validator.schema
        ; types = m
        ; directives = validator.directives
        }
    | `Duplicate -> Error (Parse_error.DuplicateType o.name))
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

and validate_directive validator directive = Ok validator
and validate_type_def validator td = Ok validator

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

module Test = struct
  let valiate_document input =
    let lexer = Lexer.init input in
    let parser = Parser.init lexer in
    let program = Parser.parse parser in
    match program with
    | Ok program ->
      let validator = init program in
      let validator = validate validator program in
      (match validator with
      | Ok validator -> print_validator validator
      | Error msg -> Fmt.pr "Validation Error: %s" (Parse_error.show msg))
    | Error msg -> Fmt.failwith "error parsing program...%s" msg
  ;;

  let%expect_test "testTypeCheck" =
    let input =
      {|

    "directive desc"
directive @example on FIELD_DEFINITION

        schema {
        query: Query
        mutation: Mutation
}

      type Query {
        foo: String
      }

        type Mutation {
        bar: Int!
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
