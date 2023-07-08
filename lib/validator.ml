[@@@warning "-27"]

open Ast
open Base

let ( let* ) res f = Base.Result.bind res ~f

type 'a validator_map = (string, 'a, String.comparator_witness) Map.t

type t =
  { ast : Ast.node
  ; types : Definition.t validator_map
  }

let init ast = { ast; types = Map.empty (module String) }

let rec validate validator node =
  let validator = build_map validator in
  match node with
  | Ast.Document d -> validate_document validator d
  | _ -> failwith "unexpected node"

and build_map validator node =
  match node with
  | Ast.Document defs ->
    let rec process_document validator defs =
      match defs with
      | [] -> Ok validator
      | def :: rest -> process_document validator rest
    in
    process_document validator defs
  | _ -> failwith "expected document"

and validate_document validator doc =
  let rec validate_document' validator defs =
    match defs with
    | [] -> Ok validator
    | def :: rest ->
      let* validator = validate_def validator def in
      validate_document' validator rest
  in
  validate_document' validator doc

and validate_def validator def =
  match def with
  | Definition.TypeDefinition td -> validate_type_def validator td
  | _ -> failwith "unexpected def"

and validate_type_def validator td = Ok validator

let print_validator validator = Fmt.pr "test"

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
      | Error msg -> Fmt.failwith "error...%s" msg)
    | Error msg -> Fmt.failwith "error parsing program...%s" msg
  ;;

  let%expect_test "testTypeCheck" =
    let input =
      {|
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
