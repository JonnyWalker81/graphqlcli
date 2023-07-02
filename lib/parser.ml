[@@@warning "-27"]

open Base
open Core
open Ast

let ( let* ) res f = Base.Result.bind res ~f
let ( let+ ) res f = Option.bind res ~f

type t = { lexer : Lexer.t; cur_token : Token.t; peek_token : Token.t }
[@@deriving show]

let next_token parser =
  let lexer, peek = Lexer.next_token parser.lexer in
  { lexer; cur_token = parser.peek_token; peek_token = peek }

let init lexer =
  let parser =
    { lexer; cur_token = Token.Illegal; peek_token = Token.Illegal }
  in
  let parser = next_token parser in
  let parser = next_token parser in
  parser

let line parser = parser.lexer.line

let chomp_semicolon parser =
  match parser.peek_token with
  | Token.Semicolon -> next_token parser
  | _ -> parser

let chomp parser tok =
  match tok with
  | tok when phys_equal tok parser.peek_token -> next_token parser
  | _ -> parser

let chomp_comment parser =
  match parser.peek_token with
  | Token.Comment _ -> next_token parser
  | _ -> parser

let expect_peek parser condition =
  match condition parser.peek_token with
  | true -> (next_token parser, true)
  | _ -> (parser, false)

let expect_peek_left_brace parser =
  expect_peek parser (function Token.LeftBrace -> true | _ -> false)

let expect_peek_right_brace parser =
  expect_peek parser (function Token.RightBrace -> true | _ -> false)

let expect_peek_right_bracket parser =
  expect_peek parser (function Token.RightBracket -> true | _ -> false)

let expect_peek_colon parser =
  expect_peek parser (function Token.Colon -> true | _ -> false)

let expect_peek_equal parser =
  expect_peek parser (function Token.Equal -> true | _ -> false)

let peek_is parser token = phys_equal parser.peek_token token

let peek_token_is_name parser =
  match parser.peek_token with Token.Name _ -> true | _ -> false

let expect_current_is parser token =
  if Token.equal parser.cur_token token then true else false

let current_token_is parser token = phys_equal token parser.cur_token

let current_token_is_name parser =
  match parser.cur_token with Token.Name _ -> true | _ -> false

let rec skip_while parser condition =
  if condition parser then skip_while (next_token parser) condition else parser

let print_parser_state ?(msg = "parser: ") parser =
  Printf.printf "%s: cur: %s, peek: %s\n" msg
    (Token.show parser.cur_token)
    (Token.show parser.peek_token)

let failwith_parser_error parser msg =
  failwith
    (Printf.sprintf "Parser Error (line: %d): %s -- cur: %s, peek: %s"
       (line parser) msg
       (Token.show parser.cur_token)
       (Token.show parser.peek_token))

let rec parse parser =
  let rec parse' parser definitions =
    match parser.cur_token with
    | Token.Eof -> (parser, List.rev definitions)
    | _ -> (
        match parse_definition parser with
        | Ok (parser, s) -> parse' (next_token parser) (s :: definitions)
        | Error err -> failwith_parser_error parser err)
  in
  let _, definitions = parse' parser [] in
  Ok (Ast.Document definitions)

and parse_definition parser =
  let parser, description =
    match parse_description parser with
    | parser, Some d -> (next_token parser, Some d)
    | parser, None -> (parser, None)
  in
  match parser.cur_token with
  | Token.Type -> parse_type (next_token parser) description
  | Token.Schema -> parse_schema (next_token parser) description
  | Token.Scalar -> parse_scalar (next_token parser) description
  | Token.Union -> parse_union (next_token parser) description
  | Token.Enum -> parse_enum (next_token parser) description
  | Token.Input -> parse_input_type (next_token parser) description
  | Token.Comment comment ->
      Ok (parser, Definition.TypeDefinition (TypeDefinition.Comment comment))
  | Token.Query | Token.Mutation
  | Token.Name "query"
  | Token.Name "mutation"
  | Token.LeftBrace ->
      parse_executable_document parser
  | Token.Name "fragment" -> parse_fragment (next_token parser)
  | Token.Name "interface" -> parse_interface (next_token parser) description
  | _ ->
      failwith
        (Printf.sprintf "unexpected definition: %s"
           (Token.show parser.cur_token))

and parse_interface parser description =
  let* parser, name = parse_name parser in
  let parser, ok = expect_peek_left_brace parser in
  match (parser, ok) with
  | parser, true ->
      let* parser, fields = parse_type_definition parser in
      Ok
        ( parser,
          Definition.TypeDefinition
            (TypeDefinition.Interface { name; fields; description }) )
  | _, false -> Error "parse_interface: expected left brace"

and parse_fragment parser =
  let* parser, name = parse_name parser in
  match parser.peek_token with
  | Token.Name "on" ->
      let parser = next_token parser in
      let parser = next_token parser in
      let* parser, type_condition = parse_name parser in
      let parser, ok = expect_peek_left_brace parser in
      if ok then
        let* parser, selection = parse_selection_set parser in

        let parser, ok = expect_peek_right_brace parser in
        if ok then
          Ok
            ( parser,
              Definition.ExecutableDefinition
                (ExecutableDefinition.FragmentDefinition
                   { name; type_condition; selection }) )
        else
          failwith_parser_error parser
            "expected right brace after fragment seletion"
      else
        failwith_parser_error parser "expected left brace for fragment seletion"
  | _ -> failwith_parser_error parser "expected 'on' token"

and parse_executable_document parser =
  match parser.cur_token with
  | Token.LeftBrace ->
      failwith_parser_error parser "unnamed query is not supported"
  | Token.Name "query" -> parse_query parser
  | Token.Name "mutation" -> parse_mutation parser
  | _ -> failwith_parser_error parser "expected query or mutation"

and parse_query parser =
  let parser = next_token parser in
  let* parser, _name = parse_name parser in
  let* parser, vars =
    match parser.peek_token with
    | Token.LeftParen ->
        let* parser, vars = parse_variables parser in
        let parser = next_token parser in
        Ok (parser, vars)
    | _ -> Ok (parser, None)
  in
  let parser, ok = expect_peek_left_brace parser in
  match (parser, ok) with
  | parser, true -> (
      let parser = next_token parser in
      let* parser, operation = parse_name parser in
      let* parser, args, operation =
        match parser.peek_token with
        | Token.LeftParen ->
            (* let parser = next_token parser in *)
            let* parser, args = parse_args parser in
            Ok (parser, args, operation)
        | _ -> Ok (parser, None, _name)
      in
      let parser, ok = expect_peek_left_brace parser in
      let* parser, selection =
        match (parser, ok) with
        | parser, true -> parse_selection_set parser
        | _ ->
            failwith_parser_error parser
              "parse_query: expected left brace for section"
      in
      let parser, ok = expect_peek_right_brace parser in
      match ok with
      | false ->
          failwith_parser_error parser
            "expected closing right brace for selection set"
      | true -> (
          let parser, ok = expect_peek_right_brace parser in
          match ok with
          | false ->
              failwith_parser_error parser
                "expected closing right brace for query"
          | true ->
              Ok
                ( parser,
                  Definition.ExecutableDefinition
                    (ExecutableDefinition.OperationDefinition
                       {
                         name = operation;
                         operation = Operation.Query;
                         args;
                         variables = vars;
                         selection;
                       }) )))
  | _ -> failwith_parser_error parser "parse_query: expected left brace"

and parse_selection_set parser =
  let rec parse_selection_set' parser set =
    match parser.peek_token with
    | Token.Name _ ->
        let parser = next_token parser in
        let* parser, name = parse_name parser in
        let ss = SelectionField.Field name in
        parse_selection_set' parser (ss :: set)
    | Token.Ellipsis ->
        let parser = next_token parser in
        let parser = next_token parser in
        let* parser, name = parse_name parser in
        let ss = SelectionField.SpreadField name in
        parse_selection_set' parser (ss :: set)
    | _ -> Ok (parser, set)
  in
  parse_selection_set' parser []

and parse_args parser =
  let parser = next_token parser in
  let parser = next_token parser in
  let rec parse_args' parser args =
    let parser = chomp parser Token.Comma in
    match parser.peek_token with
    | Token.Colon -> (
        let* parser, name = parse_name parser in
        let parser, ok = expect_peek_colon parser in
        match (parser, ok) with
        | parser, true ->
            let parser = next_token parser in

            let* parser, var_name = parse_name parser in
            let arg = OperationArg.{ name; value = var_name } in
            parse_args' parser (arg :: args)
        | _ ->
            failwith_parser_error parser
              "parse_args: expected colon after var name")
    | Token.RightParen ->
        let parser = next_token parser in
        Ok (parser, Some args)
    | _ -> failwith_parser_error parser "parse_args: peek should be a colon"
  in
  parse_args' parser []

and parse_variables parser =
  let parser = next_token parser in
  let parser = next_token parser in
  let rec parse_variables' parser vars =
    let parser = chomp parser Token.Comma in
    match parser.peek_token with
    | Token.Colon -> (
        let* parser, name = parse_name parser in
        let parser, ok = expect_peek_colon parser in
        match (parser, ok) with
        | parser, true ->
            let parser = next_token parser in

            let* parser, gql_type = parse_graphql_type parser in
            let var =
              ArgumentDefiniton.{ name; ty = gql_type; description = None }
            in
            parse_variables' parser (var :: vars)
        | _ ->
            failwith_parser_error parser
              "parse_variables: expected colon after var name")
    | Token.RightParen -> Ok (parser, Some vars)
    | _ ->
        failwith_parser_error parser "parse_varialbes: peek should be a colon"
  in
  parse_variables' parser []

and parse_mutation parser =
  let _parser = next_token parser in
  failwith "parse_mutation"

and parse_input_type parser description =
  let* parser, name = parse_name parser in
  let parser, ok = expect_peek_left_brace parser in
  match (parser, ok) with
  | parser, true ->
      let* parser, fields = parse_type_definition parser in
      Ok
        ( parser,
          Definition.TypeDefinition
            (TypeDefinition.Input { name; fields; description }) )
  | _, false -> Error "expected left brace"

and parse_enum parser description =
  let* parser, name = parse_name parser in
  let parser, ok = expect_peek_left_brace parser in
  if not ok then failwith "parse_enum: expected left brace"
  else
    let parser = next_token parser in
    let rec parse_enum_value parser vals =
      let parser, enum_desc =
        match parse_description parser with
        | parser, Some d ->
            let parser = next_token parser in
            (parser, Some d)
        | _ -> (parser, None)
      in
      match parser.cur_token with
      | Token.RightBrace -> Ok (parser, List.rev vals)
      | Token.Comment _ -> parse_enum_value (next_token parser) vals
      | _ ->
          let* parser, name = parse_name parser in
          let parser = next_token parser in
          parse_enum_value parser
            (BaseValue.{ name; description = enum_desc } :: vals)
    in
    let* parser, vals = parse_enum_value parser [] in
    Ok
      ( parser,
        Definition.TypeDefinition
          (TypeDefinition.Enum EnumType.{ name; values = vals; description }) )

and parse_union parser description =
  let* parser, union_name = parse_name parser in
  let parser, ok = expect_peek_equal parser in
  if not ok then failwith "parse_union: expected ="
  else
    let parser = next_token parser in
    let parser, first_union_desc =
      match parse_description parser with
      | parser, Some d ->
          let parser = next_token parser in
          (parser, Some d)
      | _ -> (parser, None)
    in
    let* parser, name = parse_name parser in

    let rec parse_member parser members =
      let parser, union_desc =
        match parse_description parser with
        | parser, Some d ->
            let parser = next_token parser in
            (parser, Some d)
        | _ -> (parser, None)
      in

      match parser.peek_token with
      | Token.Pipe ->
          let parser = next_token parser in
          let parser = next_token parser in
          let* parser, name = parse_name parser in
          parse_member parser
            (BaseValue.{ name; description = union_desc } :: members)
      | Token.Comment _ -> parse_member (next_token parser) members
      | _ -> Ok (parser, List.rev members)
    in
    let* parser, members =
      parse_member parser [ BaseValue.{ name; description = first_union_desc } ]
    in
    Ok
      ( parser,
        Definition.TypeDefinition
          (TypeDefinition.Union { name = union_name; members; description }) )

and parse_description parser =
  match parser.cur_token with
  | Token.StringLiteral s -> (parser, Some s)
  | _ -> (parser, None)

and parse_scalar parser description =
  let* parser, name = parse_name parser in
  Ok
    ( parser,
      Definition.TypeDefinition (TypeDefinition.Scalar { name; description }) )

and parse_schema parser description =
  match parser.cur_token with
  | Token.LeftBrace -> parse_schema_def (next_token parser) description
  | _ -> Error "parse_schema: expected left brace"

and parse_schema_def parser description =
  let rec parse_schema_def' parser fields =
    match parser.peek_token with
    | Token.RightBrace | Token.Eof -> Ok (parser, List.rev fields)
    | _ -> (
        let* parser, op = parse_schema_op_type parser in
        match parser.peek_token with
        | Token.RightBrace -> Ok (parser, List.rev_append [ op ] fields)
        | Token.Name _ -> parse_schema_def' (next_token parser) (op :: fields)
        | Token.StringLiteral _ ->
            parse_schema_def' (next_token parser) (op :: fields)
        | _ -> failwith_parser_error parser "parse_schema_def: unexpected token"
        )
  in
  let* parser, fields = parse_schema_def' parser [] in
  let query = List.find fields ~f:(fun (name, _) -> name_is name "query") in
  let mutation =
    List.find fields ~f:(fun (name, _) -> name_is name "mutation")
  in
  let subscription =
    List.find fields ~f:(fun (name, _) -> name_is name "subscription")
  in
  match parser.peek_token with
  | Token.RightBrace ->
      Ok
        ( next_token parser,
          Definition.Schema
            {
              query = make_schema_op query;
              mutation = make_schema_op mutation;
              subscription = make_schema_op subscription;
              description;
            } )
  | _ -> Error "parse_schema_def: expected closing right brace"

and make_schema_op op =
  match op with
  | Some (_, o) ->
      (* let () = Printf.printf "%s -> %s\n" value ty in *)
      Some BaseValue.{ name = o.name; description = o.description }
  | None -> None

and name_is a b = String.compare a b = 0

and parse_schema_op_type parser =
  let parser, description =
    match parse_description parser with
    | parser, Some d ->
        let parser = next_token parser in
        (parser, Some d)
    | _ -> (parser, None)
  in
  let* parser, op_name = parse_name parser in
  match parser.peek_token with
  | Token.Colon ->
      let parser = next_token parser in
      let parser = next_token parser in
      let* parser, name = parse_name parser in
      Ok (parser, (op_name, BaseValue.{ name; description }))
  | _ ->
      Error
        (Printf.sprintf "parse_schema_op_type: expected colon: %s"
           (Token.show parser.peek_token))

and parse_type parser description =
  let* parser, name = parse_name parser in
  let* parser, implements = parse_implements parser in
  let parser, ok = expect_peek_left_brace parser in
  match (parser, ok) with
  | parser, true ->
      let* parser, fields = parse_type_definition parser in
      Ok
        ( parser,
          Definition.TypeDefinition
            (TypeDefinition.Object { name; implements; fields; description }) )
  | _, false -> Error "expected left brace"

and parse_implements parser =
  match parser.peek_token with
  | Token.Name "implements" ->
      let parser = next_token parser in
      let parser = next_token parser in
      let rec parse_implements' parser names =
        let* parser, name = parse_name parser in
        match parser.peek_token with
        | Token.LeftBrace ->
            Ok (parser, List.rev (List.rev_append [ name ] names))
        | Token.Ampersand ->
            let parser = next_token parser in
            let parser = next_token parser in
            parse_implements' parser (name :: names)
        | _ ->
            failwith_parser_error parser
              "parse_implements': expected &, name or left brace"
      in
      parse_implements' parser []
  | _ -> Ok (parser, [])

and parse_type_definition parser =
  let rec parse_type_def' parser fields =
    let parser, description =
      match parser.peek_token with
      | Token.StringLiteral _ ->
          let parser = next_token parser in
          parse_description parser
      | _ -> (parser, None)
    in
    match parser.peek_token with
    | Token.RightBrace | Token.Eof -> Ok (parser, List.rev fields)
    | Token.Comment _ -> parse_type_def' (next_token parser) fields
    | Token.Name _ | Token.Type | Token.Input -> (
        let parser = next_token parser in
        let* parser, name = parse_name parser in
        let parser, args =
          match parser.peek_token with
          | Token.LeftParen ->
              let parser = next_token parser in
              let parser, args = parse_field_args parser in
              (parser, args)
          | _ -> (parser, None)
        in

        match parser.peek_token with
        | Token.Colon ->
            let parser = next_token parser in
            let parser = next_token parser in
            let* parser, ty = parse_graphql_type parser in
            let field = Field.{ name; args; ty; description } in
            parse_type_def' parser (field :: fields)
        | Token.Name _ ->
            let parser = next_token parser in
            let* parser, ty = parse_graphql_type parser in

            let field = Field.{ name; args; ty; description } in
            parse_type_def' parser (field :: fields)
        | _ -> failwith "parse_type_definition: expected a name or a colon")
    | _ ->
        failwith
          (Printf.sprintf
             "parse_type_definition: expected right brace, comment, or name: \
              cur: %s, peek: %s"
             (Token.show parser.cur_token)
             (Token.show parser.peek_token))
  in
  let* parser, td = parse_type_def' parser [] in
  let parser, ok = expect_peek_right_brace parser in
  match (parser, ok) with
  | parser, true -> Ok (parser, td)
  | _, false -> Error "expected right brace"

and parse_field_args parser =
  let rec parse_field_args' parser args =
    let parser = chomp parser Token.Comma in
    let parser = chomp_comment parser in
    let parser, arg_desc =
      match parser.peek_token with
      | Token.StringLiteral _ -> parse_description (next_token parser)
      | _ -> (parser, None)
    in

    match parser.peek_token with
    | Token.RightParen ->
        let parser =
          if not (current_token_is parser Token.RightParen) then
            next_token parser
          else parser
        in
        (parser, Some (List.rev args))
    | Token.Name _ | Token.Type | Token.Input -> (
        let parser = next_token parser in
        match parser.peek_token with
        | Token.Colon -> (
            let n = parse_name parser in
            let parser, name =
              match n with
              | Ok (parser, name) -> (parser, name)
              | Error _ -> failwith "parse_field_arg: error parsing name"
            in
            match parser.peek_token with
            | Token.Colon ->
                let parser = next_token parser in
                let parser = next_token parser in
                let ty = parse_graphql_type parser in
                let parser, gql_type =
                  match ty with
                  | Ok (parser, gql_type) -> (parser, gql_type)
                  | Error _ -> failwith "error parsing graphql type"
                in
                let parser, arg =
                  ( parser,
                    ArgumentDefiniton.
                      { name; ty = gql_type; description = arg_desc } )
                in
                parse_field_args' parser (arg :: args)
            | _ -> failwith "parse_field_args")
        | _ ->
            failwith
              (Printf.sprintf "expected args: cur: %s, peek: %s"
                 (Token.show parser.cur_token)
                 (Token.show parser.peek_token)))
    | _ ->
        failwith
          (Printf.sprintf "parse_field_args: expected name - cur: %s, peek: %s"
             (Token.show parser.cur_token)
             (Token.show parser.peek_token))
  in
  parse_field_args' parser []

and parse_graphql_type parser =
  match parser.cur_token with
  | Token.LeftBracket -> (
      let parser = next_token parser in
      let* parser, gql_type = parse_graphql_type parser in
      let parser, ok = expect_peek_right_bracket parser in
      match (parser, ok) with
      | parser, true -> (
          match parser.peek_token with
          | Token.Exclamation ->
              let parser = next_token parser in
              Ok
                (parser, GraphqlType.NonNullType (GraphqlType.ListType gql_type))
          | _ -> Ok (parser, GraphqlType.ListType gql_type))
      | _ ->
          Error
            "parse_graphql_type: expected closing right bracket for list type")
  | Token.Name _ -> (
      let* parser, name = parse_name parser in
      match parser.peek_token with
      | Token.Exclamation ->
          let parser = next_token parser in

          Ok (parser, GraphqlType.NonNullType (GraphqlType.NamedType name))
      | _ -> Ok (parser, GraphqlType.NamedType name))
  | _ -> Error "parse_graphql_type: expected a name or left bracket"

and parse_name parser =
  match parser.cur_token with
  | Token.Name n -> Ok (parser, n)
  | Token.Type ->
      Ok (parser, Token.to_name Token.Type)
      (* HACK: if type is seen where a name is expected than set name to "type" *)
  | Token.Input ->
      Ok (parser, Token.to_name Token.Input)
      (* HACK: if type is seen where a name is expected than set name to "input" *)
  | _ ->
      Error
        (Printf.sprintf "parse_name: expected name: cur: %s, peek: %s"
           (Token.show parser.cur_token)
           (Token.show parser.peek_token))

let parse_document input =
  let lexer = Lexer.init input in
  let parser = init lexer in
  let document = parse parser in
  document

let show_type_definition = TypeDefinition.show

let string_of_definition = function
  | Definition.TypeDefinition def -> Fmt.str "  %s@." (show_type_definition def)
  | Definition.Schema s -> Fmt.str "  %s@." (Schema.show s)
  | Definition.ExecutableDefinition e ->
      Fmt.str "  %s@." (ExecutableDefinition.show e)

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
           "Scalar Description"
           scalar UUID

        "description for type"
                    type Query {
                        "field description"
                       foo: String
                       bar: String!
                       baz: [String]
                       qux: [String]!
                       get: [String!]!
                       set: [[String!]!]!
                    }
                    |}
    in
    expect_document input;
    [%expect
      {|
         Document: [
             (Scalar { name = "UUID"; description = (Some "Scalar Description") })

             (Object
            { name = "Query"; implements = [];
              fields =
              [{ name = "foo"; args = None; ty = (NamedType "String");
                 description = (Some "field description") };
                { name = "bar"; args = None; ty = (NonNullType (NamedType "String"));
                  description = None };
                { name = "baz"; args = None; ty = (ListType (NamedType "String"));
                  description = None };
                { name = "qux"; args = None;
                  ty = (NonNullType (ListType (NamedType "String")));
                  description = None };
                { name = "get"; args = None;
                  ty = (NonNullType (ListType (NonNullType (NamedType "String"))));
                  description = None };
                { name = "set"; args = None;
                  ty =
                  (NonNullType
                     (ListType
                        (NonNullType (ListType (NonNullType (NamedType "String"))))));
                  description = None }
                ];
              description = (Some "description for type") })

         ]

                 |}]

  let%expect_test "testDocumentSchema" =
    let input =
      {|
              "schema description"
                    schema {
                      query: MyQuery
                      "mutation description"
                      mutation: MyMutation
                    }

              scalar Status
                    |}
    in
    expect_document input;
    [%expect
      {|
         Document: [
             { query = (Some { name = "MyQuery"; description = None });
           mutation =
           (Some { name = "MyMutation"; description = (Some "mutation description") });
           subscription = None; description = (Some "schema description") }

             (Scalar { name = "Status"; description = None })

         ]

                 |}]

  let%expect_test "testFieldWithArgs" =
    let input =
      {|
         scalar UUID
                  type Query {
                     fooQuery(arg1: Int, arg2: Boolean!): String
                  }

        type Mutation {
        fooMut(fooArg1: String
        "arg description"
        fooArg2: Int!): [String!]!
        bar(barArg1: Boolean!
            barArg2: Boolean): [String!]!
}
                  |}
    in
    expect_document input;
    [%expect
      {|
      Document: [
          (Scalar { name = "UUID"; description = None })

          (Object
         { name = "Query"; implements = [];
           fields =
           [{ name = "fooQuery";
              args =
              (Some [{ name = "arg1"; ty = (NamedType "Int"); description = None };
                      { name = "arg2"; ty = (NonNullType (NamedType "Boolean"));
                        description = None }
                      ]);
              ty = (NamedType "String"); description = None }
             ];
           description = None })

          (Object
         { name = "Mutation"; implements = [];
           fields =
           [{ name = "fooMut";
              args =
              (Some [{ name = "fooArg1"; ty = (NamedType "String");
                       description = None };
                      { name = "fooArg2"; ty = (NonNullType (NamedType "Int"));
                        description = (Some "arg description") }
                      ]);
              ty = (NonNullType (ListType (NonNullType (NamedType "String"))));
              description = None };
             { name = "bar";
               args =
               (Some [{ name = "barArg1"; ty = (NonNullType (NamedType "Boolean"));
                        description = None };
                       { name = "barArg2"; ty = (NamedType "Boolean");
                         description = None }
                       ]);
               ty = (NonNullType (ListType (NonNullType (NamedType "String"))));
               description = None }
             ];
           description = None })

      ] |}]

  let%expect_test "testUnion" =
    let input =
      {|
           "union description"
            union foo =
                """bar union desc"""
                Bar
              | Foobar
                     |}
    in
    expect_document input;
    [%expect
      {|
         Document: [
             (Union
            { name = "foo";
              members =
              [{ name = "Bar"; description = (Some "bar union desc") };
                { name = "Foobar"; description = None }];
              description = (Some "union description") })

         ] |}]

  let%expect_test "testEnum" =
    let input =
      {|
            "Enum description"
             enum Foo {
               "bar description"
               BAR
               FOOBAR
               "baz desc"
               BAZ
             }
                       |}
    in
    expect_document input;
    [%expect
      {|
           Document: [
               (Enum
              { name = "Foo";
                values =
                [{ name = "BAR"; description = (Some "bar description") };
                  { name = "FOOBAR"; description = None };
                  { name = "BAZ"; description = (Some "baz desc") }];
                description = (Some "Enum description") })

           ] |}]

  let%expect_test "testComments" =
    let input =
      {|
              scalar Cost #cost type
              #this is a top-level comment
              union bar = Bar | Foobar | Qux #this is a comment at the end of a line

              enum Kind {
                NORTH
                SOUTH # enum comment
                EAST
                WEST
     }

              type Mutation {
           foo: String #query foo field
     }
                       |}
    in
    expect_document input;
    [%expect
      {|
           Document: [
               (Scalar { name = "Cost"; description = None })

               (Comment "cost type")

               (Comment "this is a top-level comment")

               (Union
              { name = "bar";
                members =
                [{ name = "Bar"; description = None };
                  { name = "Foobar"; description = None };
                  { name = "Qux"; description = None }];
                description = None })

               (Enum
              { name = "Kind";
                values =
                [{ name = "NORTH"; description = None };
                  { name = "SOUTH"; description = None };
                  { name = "EAST"; description = None };
                  { name = "WEST"; description = None }];
                description = None })

               (Object
              { name = "Mutation"; implements = [];
                fields =
                [{ name = "foo"; args = None; ty = (NamedType "String");
                   description = None }
                  ];
                description = None })

           ] |}]

  let%expect_test "testInputType" =
    let input =
      {|
        """Input description"""
              input QueryInput {
           category: String
        "input field desc"
           name: String!
           labels: [String!]
     }
                       |}
    in
    expect_document input;
    [%expect
      {|

           Document: [
               (Input
              { name = "QueryInput";
                fields =
                [{ name = "category"; args = None; ty = (NamedType "String");
                   description = None };
                  { name = "name"; args = None; ty = (NonNullType (NamedType "String"));
                    description = (Some "input field desc") };
                  { name = "labels"; args = None;
                    ty = (ListType (NonNullType (NamedType "String")));
                    description = None }
                  ];
                description = (Some "Input description") })

           ] |}]

  let%expect_test "testArgsWithComment" =
    let input =
      {|
              type Query {
           category(id: UUID #comment
                    name: String
           ): String
           name: String!
           labels: [String!]
     }
                       |}
    in
    expect_document input;
    [%expect
      {|

           Document: [
               (Object
              { name = "Query"; implements = [];
                fields =
                [{ name = "category";
                   args =
                   (Some [{ name = "id"; ty = (NamedType "UUID"); description = None };
                           { name = "name"; ty = (NamedType "String");
                             description = None }
                           ]);
                   ty = (NamedType "String"); description = None };
                  { name = "name"; args = None; ty = (NonNullType (NamedType "String"));
                    description = None };
                  { name = "labels"; args = None;
                    ty = (ListType (NonNullType (NamedType "String")));
                    description = None }
                  ];
                description = None })

           ] |}]

  let%expect_test "testTypeUsedAsArgName" =
    let input =
      {|
              type Query {
           category(input: String!
                    type: String
           ): String
     }
                       |}
    in
    expect_document input;
    [%expect
      {|

           Document: [
               (Object
              { name = "Query"; implements = [];
                fields =
                [{ name = "category";
                   args =
                   (Some [{ name = "input"; ty = (NonNullType (NamedType "String"));
                            description = None };
                           { name = "type"; ty = (NamedType "String");
                             description = None }
                           ]);
                   ty = (NamedType "String"); description = None }
                  ];
                description = None })

           ] |}]

  let%expect_test "testSimpleQueryExecDoc" =
    let input = {|
query foo{
      foo {
        field
    }
}

|} in
    expect_document input;
    [%expect
      {|
      Document: [
          (OperationDefinition
         { name = "foo"; operation = Query; args = None; variables = None;
           selection = [(Field "field")] })

      ]

|}]

  let%expect_test "testSimpleQueryExecDocWithArgs" =
    let input =
      {|
query foo($var1: String){
      foo(var1: $var1) {
        field
    }
}

|}
    in
    expect_document input;
    [%expect
      {|
      Document: [
          (OperationDefinition
         { name = "foo"; operation = Query;
           args = (Some [{ name = "var1"; value = "$var1" }]);
           variables =
           (Some [{ name = "$var1"; ty = (NamedType "String"); description = None }
                   ]);
           selection = [(Field "field")] })

      ]

|}]

  let%expect_test "testQueryWithSpread" =
    let input =
      {|
query foo($var1: String){
      foo(var1: $var1) {
        ...field
    }
}

|}
    in
    expect_document input;
    [%expect
      {|
      Document: [
          (OperationDefinition
         { name = "foo"; operation = Query;
           args = (Some [{ name = "var1"; value = "$var1" }]);
           variables =
           (Some [{ name = "$var1"; ty = (NamedType "String"); description = None }
                   ]);
           selection = [(SpreadField "field")] })

      ]

|}]

  let%expect_test "testFragment" =
    let input =
      {|
fragment Actions on Actions {
  view
  edit
  add
  delete
}
|}
    in
    expect_document input;
    [%expect
      {|
      Document: [
          (FragmentDefinition
         { name = "Actions"; type_condition = "Actions";
           selection =
           [(Field "delete"); (Field "add"); (Field "edit"); (Field "view")] })

      ] |}]

  let%expect_test "testInterface" =
    let input =
      {|
        "interface description"
        interface Foo {
           bar: String
           "field desc"
           baz: String!
           """field block desc"""
           var: [String]!
        }
|}
    in
    expect_document input;
    [%expect
      {|
      Document: [
          (Interface
         { name = "Foo";
           fields =
           [{ name = "bar"; args = None; ty = (NamedType "String");
              description = None };
             { name = "baz"; args = None; ty = (NonNullType (NamedType "String"));
               description = (Some "field desc") };
             { name = "var"; args = None;
               ty = (NonNullType (ListType (NamedType "String")));
               description = (Some "field block desc") }
             ];
           description = (Some "interface description") })

      ]

       |}]

  let%expect_test "testInterfaceImplements" =
    let input =
      {|
type Person implements NamedEntity {
  name: String
  age: Int
}
|}
    in
    expect_document input;
    [%expect
      {|
      Document: [
          (Object
         { name = "Person"; implements = ["NamedEntity"];
           fields =
           [{ name = "name"; args = None; ty = (NamedType "String");
              description = None };
             { name = "age"; args = None; ty = (NamedType "Int");
               description = None }
             ];
           description = None })

      ]
|}]

  let%expect_test "testInterfaceMultipleImplements" =
    let input =
      {|
type Person implements NamedEntity & ValuedEntity{
  name: String
  age: Int
}
|}
    in
    expect_document input;
    [%expect
      {|
      Document: [
          (Object
         { name = "Person"; implements = ["NamedEntity"; "ValuedEntity"];
           fields =
           [{ name = "name"; args = None; ty = (NamedType "String");
              description = None };
             { name = "age"; args = None; ty = (NamedType "Int");
               description = None }
             ];
           description = None })

      ]
|}]
end
