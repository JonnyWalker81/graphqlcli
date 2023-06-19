open Base
open Core

let ( let* ) res f = Base.Result.bind res ~f

type t = { lexer : Lexer.t; cur_token : Token.t; peek_token : Token.t }
[@@deriving show]

let next_token parser =
  let lexer, peek = Lexer.next_token parser.lexer in
  { lexer; cur_token = parser.peek_token; peek_token = peek }

let chomp_semicolon parser =
  match parser.peek_token with
  | Token.Semicolon -> next_token parser
  | _ -> parser

let expect_peek parser condition =
  match condition parser.peek_token with
  | true -> (next_token parser, true)
  | _ -> (parser, false)

let expect_peek_left_brace parser =
  expect_peek parser (function Token.LeftBrace -> true | _ -> false)

let expect_peek_right_brace parser =
  expect_peek parser (function Token.RightBrace -> true | _ -> false)

let expect_peek_colon parser =
  expect_peek parser (function Token.Colon -> true | _ -> false)

let peek_is parser token = phys_equal parser.peek_token token

let expect_current_is parser token =
  if Token.equal parser.cur_token token then true else false

let current_token_is parser token = phys_equal token parser.cur_token

let rec skip_while parser condition =
  if condition parser then skip_while (next_token parser) condition else parser

let init lexer =
  let parser =
    { lexer; cur_token = Token.Illegal; peek_token = Token.Illegal }
  in
  let parser = next_token parser in
  let parser = next_token parser in
  parser

let rec parse parser =
  let rec parse' parser definitions =
    match parser.cur_token with
    | Token.Eof -> (parser, List.rev definitions)
    | _ -> (
        match parse_definition parser with
        | Ok (parser, s) -> parse' (next_token parser) (s :: definitions)
        | Error err -> failwith (Printf.sprintf "parse error: %s" err))
  in
  let _, definitions = parse' parser [] in
  Ok (Ast.Document definitions)

and parse_definition parser =
  match parser.cur_token with
  | Token.Type -> parse_type (next_token parser)
  | Token.Schema -> parse_schema (next_token parser)
  | _ -> failwith "unexpected definition"

and parse_schema parser =
  match parser.cur_token with
  | Token.LeftBrace -> parse_schema_def (next_token parser)
  | _ -> Error "parse_schema: expected left brace"

and parse_schema_def parser =
  let rec parse_schema_def' parser fields =
    match parser.peek_token with
    | Token.RightBrace | Token.Eof -> Ok (parser, List.rev fields)
    | _ -> (
        let* parser, op = parse_schema_op_type parser in
        match parser.peek_token with
        | Token.RightBrace -> Ok (parser, List.rev fields)
        | _ -> parse_schema_def' parser (op :: fields))
  in
  let* parser, fields = parse_schema_def' parser [] in
  let query = List.find fields ~f:(fun (name, _) -> name_is name "query") in
  let mutation =
    List.find fields ~f:(fun (name, _) -> name_is name "mutation")
  in
  let subscription =
    List.find fields ~f:(fun (name, _) -> name_is name "subscription")
  in
  match parser.cur_token with
  | Token.RightBrace ->
      Ok
        ( next_token parser,
          Ast.Schema
            {
              query = make_schema_op query;
              mutation = make_schema_op mutation;
              subscription = make_schema_op subscription;
            } )
  | _ -> Error "parse_schema_def: expected closing right brace"

and make_schema_op op =
  match op with
  | Some (name, ty) ->
      let () = Printf.printf "%s -> %s\n" name ty in
      Some Ast.{ name = ty }
  | None -> None

and name_is a b = String.compare a b = 0

and parse_schema_op_type parser =
  let* parser, op_name = parse_name parser in
  match parser.peek_token with
  | Token.Colon ->
      let parser = next_token parser in
      let parser = next_token parser in
      let* parser, name = parse_name parser in
      Ok (next_token parser, (op_name, name))
  | _ ->
      Error
        (Printf.sprintf "parse_schema_op_type: expected colon: %s"
           (Token.show parser.peek_token))

and parse_type parser =
  let* parser, name = parse_name parser in
  let parser, ok = expect_peek_left_brace parser in
  match (parser, ok) with
  | parser, true ->
      let* parser, fields = parse_type_definition parser in
      Ok (parser, Ast.TypeDefinition (Ast.Object { name; fields }))
  | _, false -> Error "expected left brace"

and parse_type_definition parser =
  let rec parse_type_def' parser fields =
    match parser.peek_token with
    | Token.RightBrace -> Ok (parser, List.rev fields)
    | _ -> (
        let parser = next_token parser in
        let* parser, name = parse_name parser in
        match parser.peek_token with
        | Token.Colon ->
            let parser = next_token parser in
            let parser = next_token parser in
            let* parser, type_name = parse_name parser in
            let field = Ast.{ name; ty = Ast.{ name = type_name } } in
            parse_type_def' parser (field :: fields)
        | _ -> Error "parse_type_definition: expected a name")
  in
  let* parser, td = parse_type_def' parser [] in
  let parser, ok = expect_peek_right_brace parser in
  match (parser, ok) with
  | parser, true -> Ok (parser, td)
  | _, false -> Error "expected right brace"

and parse_name parser =
  match parser.cur_token with
  | Token.Name n -> Ok (parser, n)
  | _ ->
      Error
        (Printf.sprintf "parse_name: expected name: %s"
           (Token.show parser.cur_token))

let show_type_definition = Ast.show_type_definition

let string_of_definition = function
  | Ast.TypeDefinition def -> Fmt.str "  %s@." (show_type_definition def)
  | Ast.Schema s -> Fmt.str "  %s@." (Ast.show_schema s)

(* let string_of_ident ident = Ast.(ident.identifier) *)

let print_node = function
  | Ast.Document document ->
      Fmt.pr "Document: [@.";
      List.iter document ~f:(fun d -> Fmt.pr "  %s@." (string_of_definition d));
      Fmt.pr "]@."
  | _ -> failwith "yaya"

module Test = struct
  let expect_document input =
    let lexer = Lexer.init input in
    let parser = init lexer in
    let program = parse parser in
    match program with
    | Ok program -> print_node program
    | Error msg -> Fmt.failwith "error...%s" msg
  (* | Error msg -> Fmt.failwith "%a@." pp_parse_error msg *)

  let%expect_test "testSimpleDocument" =
    let input =
      {|
                 type Query {
                    foo: String
                 }
                 |}
    in
    expect_document input;
    [%expect
      {|
      Document: [
          (Object
         { name = "Query"; fields = [{ name = "foo"; ty = { name = "String" } }] })

      ]

              |}]

  let%expect_test "testDocumentSchema" =
    let input =
      {|
                 schema {
                   query: MyQuery
                   mutation: MyMutation
                 }
                 |}
    in
    expect_document input;
    [%expect {|
      mutation -> MyMutation
      query -> MyQuery
      Document: [
          { query = (Some { name = "MyQuery" });
        mutation = (Some { name = "MyMutation" }); subscription = None }

      ]

              |}]
end
