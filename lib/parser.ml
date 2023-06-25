open Base
open Core

let ( let* ) res f = Base.Result.bind res ~f
let ( let+ ) res f = Option.bind res ~f

type t = { lexer : Lexer.t; cur_token : Token.t; peek_token : Token.t }
[@@deriving show]

let next_token parser =
  let lexer, peek = Lexer.next_token parser.lexer in
  { lexer; cur_token = parser.peek_token; peek_token = peek }

let chomp_semicolon parser =
  match parser.peek_token with
  | Token.Semicolon -> next_token parser
  | _ -> parser

let chomp parser tok =
  match tok with
  | tok when phys_equal tok parser.peek_token ->
      next_token parser
  | _ -> parser

let chomp_comment parser =
  match parser.peek_token with
  | Token.Comment _ ->
      next_token parser
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
  | Token.Scalar -> parse_scalar (next_token parser)
  | Token.Union -> parse_union (next_token parser)
  | Token.Enum -> parse_enum (next_token parser)
  | Token.Input -> parse_input_type (next_token parser)
  | Token.Comment comment ->
      Ok (parser, Ast.TypeDefinition (Ast.Comment comment))
  | _ ->
      failwith
        (Printf.sprintf "unexpected definition: %s"
           (Token.show parser.cur_token))

and parse_input_type parser =
  let* parser, name = parse_name parser in
  let parser, ok = expect_peek_left_brace parser in
  match (parser, ok) with
  | parser, true ->
      let* parser, fields = parse_type_definition parser in
      Ok (parser, Ast.TypeDefinition (Ast.Input { name; fields }))
  | _, false -> Error "expected left brace"

and parse_enum parser =
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
          parse_enum_value parser (name :: vals)
    in
    let* parser, vals = parse_enum_value parser [] in
    Ok (parser, Ast.TypeDefinition (Ast.Enum { name; values = vals }))

and parse_union parser =
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
    Ok (parser, Ast.TypeDefinition (Ast.Union { name = union_name; members }))

and parse_scalar parser =
  let* parser, name = parse_name parser in
  Ok (parser, Ast.TypeDefinition (Ast.Scalar name))

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

and parse_type parser =
  let* parser, name = parse_name parser in
  let parser, ok = expect_peek_left_brace parser in
  match (parser, ok) with
  | parser, true ->
    let* parser, fields =
      (* let parser = next_token parser in *)
      parse_type_definition parser in
      Ok (parser, Ast.TypeDefinition (Ast.Object { name; fields }))
  | _, false -> Error "expected left brace"

and parse_type_definition parser =
  (* let () = print_parser_state ~msg:"parsing type definition..." parser in *)
  let rec parse_type_def' parser fields =
    match parser.peek_token with
    | Token.RightBrace | Token.Eof -> Ok (parser, List.rev fields)
    |Token.Comment _ -> parse_type_def' (next_token parser) fields
    |Token.Name _ | Token.Type | Token.Input -> (
              let parser = next_token parser in
        (* let parser = *)
        (*   if current_token_is_name parser then parser else next_token parser *)
        (* in *)
        let* parser, name = parse_name parser in
  (* let () = print_parser_state ~msg:"\t parsed name" parser in *)
        (* let* parser, name = parse_name (parser) in *)
        let parser, args =
          match parser.peek_token with
          | Token.LeftParen ->
              (* let parser = next_token parser in *)
              let parser = next_token parser in
              let parser, args = parse_field_args parser in
              (parser, args)
          | _ -> (parser, None)
        in

        match parser.peek_token with
        | Token.Colon ->

  (* let () = print_parser_state ~msg:"\t found colon, parsing type" parser in *)
            let parser = next_token parser in
            let parser = next_token parser in
            let* parser, ty = parse_graphql_type parser in

  (* let () = print_parser_state ~msg:"\t parsed type" parser in *)
            let field = Ast.{ name; args; ty } in
            parse_type_def' parser (field :: fields)
        | Token.Name _ ->
            let parser = next_token parser in
            let* parser, ty = parse_graphql_type parser in

            let field = Ast.{ name; args; ty } in
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
  (* let () = print_parser_state ~msg:"parsing field args" parser  in *)
    let parser = chomp parser Token.Comma in
    let parser = chomp_comment parser in
  (* let () = print_parser_state ~msg:"parsing field args (after chomp)" parser  in *)
    match
      parser.peek_token
    with
    | Token.RightParen ->
        let parser =
          if not (current_token_is parser Token.RightParen) then
            next_token parser
          else parser
        in
        (parser, Some (List.rev args))
    | Token.Name _ | Token.Type | Token.Input-> (
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
                let parser, arg = (parser, Ast.{ name; ty = gql_type }) in
                parse_field_args' parser (arg :: args)
            | _ -> failwith "parse_field_args")
        | _ ->
            failwith
              (Printf.sprintf "expected args: cur: %s, peek: %s"
                 (Token.show parser.cur_token)
                 (Token.show parser.peek_token)))
    | _ -> failwith (Printf.sprintf "parse_field_args: expected name - cur: %s, peek: %s" (Token.show parser.cur_token) (Token.show parser.peek_token))
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
              (* let parser = next_token parser in *)
              (* let parser = next_token parser in *)
              let parser = next_token parser in
              Ok (parser, Ast.NonNullType (Ast.ListType gql_type))
          | _ ->
            (* let parser = next_token parser in *)
            (* let parser = next_token parser in *)
            Ok (parser, Ast.ListType gql_type))
      | _ -> Error "parse_graphql_type: expected closing right bracket for list type")
  | Token.Name _ -> (
      let* parser, name = parse_name parser in
      match parser.peek_token with
      | Token.Exclamation ->
          (* let parser = next_token parser in *)
          let parser = next_token parser in

          Ok (parser, Ast.NonNullType (Ast.NamedType name))
      | _ ->
          (* let parser = next_token parser in *)
          Ok (parser, Ast.NamedType name))
  | _ -> Error "parse_graphql_type: expected a name or left bracket"

and parse_name parser =
  match parser.cur_token with
  | Token.Name n -> Ok (parser, n)
  | Token.Type -> Ok(parser, Token.to_name Token.Type) (* HACK: if type is seen where a name is expected than set name to "type" *)
  | Token.Input -> Ok(parser, Token.to_name Token.Input) (* HACK: if type is seen where a name is expected than set name to "input" *)
  | _ ->
      Error
        (Printf.sprintf "parse_name: expected name: cur: %s, peek: %s"
           (Token.show parser.cur_token)
           (Token.show parser.peek_token))

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
           scalar UUID
                    type Query {
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
             (Scalar "UUID")

             (Object
            { name = "Query";
              fields =
              [{ name = "foo"; args = None; ty = (NamedType "String") };
                { name = "bar"; args = None; ty = (NonNullType (NamedType "String")) };
                { name = "baz"; args = None; ty = (ListType (NamedType "String")) };
                { name = "qux"; args = None;
                  ty = (NonNullType (ListType (NamedType "String"))) };
                { name = "get"; args = None;
                  ty = (NonNullType (ListType (NonNullType (NamedType "String")))) };
                { name = "set"; args = None;
                  ty =
                  (NonNullType
                     (ListType
                        (NonNullType (ListType (NonNullType (NamedType "String"))))))
                  }
                ]
              })

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
           mutation = (Some { name = "MyMutation" }); subscription = None }

             (Scalar "Status")

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
          (Scalar "UUID")

          (Object
         { name = "Query";
           fields =
           [{ name = "fooQuery";
              args =
              (Some [{ name = "arg1"; ty = (NamedType "Int") };
                      { name = "arg2"; ty = (NonNullType (NamedType "Boolean")) }]);
              ty = (NamedType "String") }
             ]
           })

          (Object
         { name = "Mutation";
           fields =
           [{ name = "fooMut";
              args =
              (Some [{ name = "fooArg1"; ty = (NamedType "String") };
                      { name = "fooArg2"; ty = (NonNullType (NamedType "Int")) }]);
              ty = (NonNullType (ListType (NonNullType (NamedType "String")))) };
             { name = "bar";
               args =
               (Some [{ name = "barArg1"; ty = (NonNullType (NamedType "Boolean"))
                        };
                       { name = "barArg2"; ty = (NamedType "Boolean") }]);
               ty = (NonNullType (ListType (NonNullType (NamedType "String")))) }
             ]
           })

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
             (Union { name = "foo"; members = ["Bar"; "Foobar"] })

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
               (Enum { name = "Foo"; values = ["BAR"; "FOOBAR"; "BAZ"] })

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
               (Scalar "Cost")

               (Comment "cost type")

               (Comment "this is a top-level comment")

               (Union { name = "bar"; members = ["Bar"; "Foobar"; "Qux"] })

               (Enum { name = "Kind"; values = ["NORTH"; "SOUTH"; "EAST"; "WEST"] })

               (Object
              { name = "Mutation";
                fields = [{ name = "foo"; args = None; ty = (NamedType "String") }] })

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
                [{ name = "category"; args = None; ty = (NamedType "String") };
                  { name = "name"; args = None; ty = (NonNullType (NamedType "String"))
                    };
                  { name = "labels"; args = None;
                    ty = (ListType (NonNullType (NamedType "String"))) }
                  ]
                })

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
                   (Some [{ name = "id"; ty = (NamedType "UUID") };
                           { name = "name"; ty = (NamedType "String") }]);
                   ty = (NamedType "String") };
                  { name = "name"; args = None; ty = (NonNullType (NamedType "String"))
                    };
                  { name = "labels"; args = None;
                    ty = (ListType (NonNullType (NamedType "String"))) }
                  ]
                })

           ] |}]

let%expect_test "testTypeUsedAsArgName" =
    let input =
      {|
              type Query {
           category(name: String!
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
                   (Some [{ name = "name"; ty = (NonNullType (NamedType "String")) };
                           { name = "type"; ty = (NamedType "String") }]);
                   ty = (NamedType "String") }
                  ]
                })

           ] |}]

end
