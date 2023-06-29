open Base
open Core

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
  let parser, description = parse_description parser in
  let parser =
    match description with Some _ -> next_token parser | None -> parser
  in
  match parser.cur_token with
  | Token.Type -> parse_type (next_token parser) description
  | Token.Schema -> parse_schema (next_token parser) description
  | Token.Scalar -> parse_scalar (next_token parser) description
  | Token.Union -> parse_union (next_token parser) description
  | Token.Enum -> parse_enum (next_token parser) description
  | Token.Input -> parse_input_type (next_token parser) description
  | Token.Comment comment ->
      Ok (parser, Ast.TypeDefinition (Ast.Comment comment))
  | Token.Query | Token.Mutation
  | Token.Name "query"
  | Token.Name "mutation"
  | Token.LeftBrace ->
      parse_executable_document parser
  | Token.Name "fragment" -> parse_fragment (next_token parser)
  | _ ->
      failwith
        (Printf.sprintf "unexpected definition: %s"
           (Token.show parser.cur_token))

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
              Ast.ExecutableDefinition
                (Ast.FragmentDefinition { name; type_condition; selection }) )
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
                  Ast.ExecutableDefinition
                    (Ast.OperationDefinition
                       Ast.
                         {
                           name = operation;
                           operation = Ast.Query;
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
        let ss = Ast.Field name in
        parse_selection_set' parser (ss :: set)
    | Token.Ellipsis ->
        let parser = next_token parser in
        let parser = next_token parser in
        let* parser, name = parse_name parser in
        let ss = Ast.SpreadField name in
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
            let arg = Ast.{ name; value = var_name } in
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
            let var = Ast.{ name; ty = gql_type; description = None } in
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
      Ok (parser, Ast.TypeDefinition (Ast.Input { name; fields; description }))
  | _, false -> Error "expected left brace"

and parse_enum parser description =
  let* parser, name = parse_name parser in
  let parser, ok = expect_peek_left_brace parser in
  if not ok then failwith "parse_enum: expected left brace"
  else
    let parser = next_token parser in
    let rec parse_enum_value parser vals =
      match parser.cur_token with
      | Token.RightBrace -> Ok (parser, List.rev vals)
      | Token.Comment _ -> parse_enum_value (next_token parser) vals
      | _ ->
          let* parser, name = parse_name parser in
          let parser = next_token parser in
          parse_enum_value parser (Ast.{ name; description = None } :: vals)
    in
    let* parser, vals = parse_enum_value parser [] in
    Ok
      ( parser,
        Ast.TypeDefinition (Ast.Enum { name; values = vals; description }) )

and parse_union parser description =
  let* parser, union_name = parse_name parser in
  let parser, ok = expect_peek_equal parser in
  if not ok then failwith "parse_union: expected ="
  else
    let parser = next_token parser in
    let* parser, name = parse_name parser in
    let rec parse_member parser members =
      match parser.peek_token with
      | Token.Pipe ->
          let parser = next_token parser in
          let parser = next_token parser in
          let* parser, name = parse_name parser in
          parse_member parser (name :: members)
      | Token.Comment _ -> parse_member (next_token parser) members
      | _ -> Ok (parser, List.rev members)
    in
    let* parser, members = parse_member parser [ name ] in
    Ok
      ( parser,
        Ast.TypeDefinition
          (Ast.Union { name = union_name; members; description }) )

and parse_description parser =
  match parser.cur_token with
  | Token.StringLiteral s -> (parser, Some s)
  | _ -> (parser, None)

and parse_scalar parser description =
  let* parser, name = parse_name parser in
  Ok (parser, Ast.TypeDefinition (Ast.Scalar { name; description }))

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
        | _ -> failwith "parse_schema_def: unexpected token")
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
          Ast.Schema
            {
              query = make_schema_op query;
              mutation = make_schema_op mutation;
              subscription = make_schema_op subscription;
              description;
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
      Ok (parser, (op_name, name))
  | _ ->
      Error
        (Printf.sprintf "parse_schema_op_type: expected colon: %s"
           (Token.show parser.peek_token))

and parse_type parser description =
  let* parser, name = parse_name parser in
  let parser, ok = expect_peek_left_brace parser in
  match (parser, ok) with
  | parser, true ->
      let* parser, fields = parse_type_definition parser in
      Ok (parser, Ast.TypeDefinition (Ast.Object { name; fields; description }))
  | _, false -> Error "expected left brace"

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
            let field = Ast.{ name; args; ty; description } in
            parse_type_def' parser (field :: fields)
        | Token.Name _ ->
            let parser = next_token parser in
            let* parser, ty = parse_graphql_type parser in

            let field = Ast.{ name; args; ty; description } in
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
  let rec parse_field_args' (parser : t) (args : Ast.argument_definition list) :
      t * Ast.argument_definition list option =
    let parser = chomp parser Token.Comma in
    let parser = chomp_comment parser in
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
                  (parser, Ast.{ name; ty = gql_type; description = None })
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
              Ok (parser, Ast.NonNullType (Ast.ListType gql_type))
          | _ -> Ok (parser, Ast.ListType gql_type))
      | _ ->
          Error
            "parse_graphql_type: expected closing right bracket for list type")
  | Token.Name _ -> (
      let* parser, name = parse_name parser in
      match parser.peek_token with
      | Token.Exclamation ->
          let parser = next_token parser in

          Ok (parser, Ast.NonNullType (Ast.NamedType name))
      | _ -> Ok (parser, Ast.NamedType name))
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

let show_type_definition = Ast.show_type_definition

let string_of_definition = function
  | Ast.TypeDefinition def -> Fmt.str "  %s@." (show_type_definition def)
  | Ast.Schema s -> Fmt.str "  %s@." (Ast.show_schema s)
  | Ast.ExecutableDefinition e ->
      Fmt.str "  %s@." (Ast.show_executable_definition e)

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
            { name = "Query";
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
                    schema {
                      query: MyQuery
                      mutation: MyMutation
                    }

              scalar Status
                    |}
    in
    expect_document input;
    [%expect
      {|
         mutation -> MyMutation
         query -> MyQuery
         Document: [
             { query = (Some { name = "MyQuery" });
           mutation = (Some { name = "MyMutation" }); subscription = None;
           description = None }

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
        fooMut(fooArg1: String, fooArg2: Int!): [String!]!
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
         { name = "Query";
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
         { name = "Mutation";
           fields =
           [{ name = "fooMut";
              args =
              (Some [{ name = "fooArg1"; ty = (NamedType "String");
                       description = None };
                      { name = "fooArg2"; ty = (NonNullType (NamedType "Int"));
                        description = None }
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
            union foo = Bar | Foobar
                     |}
    in
    expect_document input;
    [%expect
      {|
         Document: [
             (Union { name = "foo"; members = ["Bar"; "Foobar"]; description = None })

         ] |}]

  let%expect_test "testEnum" =
    let input =
      {|
             enum Foo {
               BAR
               FOOBAR
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
                [{ name = "BAR"; description = None };
                  { name = "FOOBAR"; description = None };
                  { name = "BAZ"; description = None }];
                description = None })

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
              { name = "bar"; members = ["Bar"; "Foobar"; "Qux"]; description = None })

               (Enum
              { name = "Kind";
                values =
                [{ name = "NORTH"; description = None };
                  { name = "SOUTH"; description = None };
                  { name = "EAST"; description = None };
                  { name = "WEST"; description = None }];
                description = None })

               (Object
              { name = "Mutation";
                fields =
                [{ name = "foo"; args = None; ty = (NamedType "String");
                   description = None }
                  ];
                description = None })

           ] |}]

  let%expect_test "testInputType" =
    let input =
      {|
              input QueryInput {
           category: String
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
                    description = None };
                  { name = "labels"; args = None;
                    ty = (ListType (NonNullType (NamedType "String")));
                    description = None }
                  ];
                description = None })

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
              { name = "Query";
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
              { name = "Query";
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
end
