[@@@warning "-27"]

open Base
open Core
open Ast

let ( let* ) res f = Base.Result.bind res ~f
let ( let+ ) res f = Option.bind res ~f

type t =
  { lexer : Lexer.t
  ; cur_token : Token.t
  ; peek_token : Token.t
  }
[@@deriving show]

let next_token parser =
  let lexer, peek = Lexer.next_token parser.lexer in
  { lexer; cur_token = parser.peek_token; peek_token = peek }
;;

let init lexer =
  let parser = { lexer; cur_token = Token.Illegal; peek_token = Token.Illegal } in
  let parser = next_token parser in
  let parser = next_token parser in
  parser
;;

let line parser = parser.lexer.line

let chomp_semicolon parser =
  match parser.peek_token with
  | Token.Semicolon -> next_token parser
  | _ -> parser
;;

let chomp parser tok =
  match tok with
  | tok when phys_equal tok parser.peek_token -> next_token parser
  | _ -> parser
;;

let chomp_comment parser =
  match parser.peek_token with
  | Token.Comment _ -> next_token parser
  | _ -> parser
;;

let expect_peek parser condition =
  match condition parser.peek_token with
  | true -> next_token parser, true
  | _ -> parser, false
;;

let expect_peek_left_brace parser =
  expect_peek parser (function
      | Token.LeftBrace -> true
      | _ -> false)
;;

let expect_peek_right_brace parser =
  expect_peek parser (function
      | Token.RightBrace -> true
      | _ -> false)
;;

let expect_peek_right_bracket parser =
  expect_peek parser (function
      | Token.RightBracket -> true
      | _ -> false)
;;

let expect_peek_colon parser =
  expect_peek parser (function
      | Token.Colon -> true
      | _ -> false)
;;

let expect_peek_equal parser =
  expect_peek parser (function
      | Token.Equal -> true
      | _ -> false)
;;

let expect_peek_at parser =
  expect_peek parser (function
      | Token.At -> true
      | _ -> false)
;;

let expect_peek_on parser =
  expect_peek parser (function
      | Token.Name "on" -> true
      | _ -> false)
;;

let expect_peek_name parser name =
  expect_peek parser (function
      | Token.Name n when String.compare n name = 0 -> true
      | _ -> false)
;;

let peek_is parser token = phys_equal parser.peek_token token

let peek_token_is_name parser =
  match parser.peek_token with
  | Token.Name _ -> true
  | _ -> false
;;

let expect_current_is parser token =
  if Token.equal parser.cur_token token then true else false
;;

let current_token_is parser token = phys_equal token parser.cur_token

let current_token_is_name parser =
  match parser.cur_token with
  | Token.Name _ -> true
  | _ -> false
;;

let rec skip_while parser condition =
  if condition parser then skip_while (next_token parser) condition else parser
;;

let parser_error parser err = Error (parser.lexer.line, err)

let name_to_directive_location name =
  match name with
  | Token.Name "FIELD_DEFINITION" ->
    DirectiveLocation.TypeSystemDirectiveLocation
      TypeSystemDirectiveLocation.FIELD_DEFINITION
  | Token.Name "SCHEMA" ->
    DirectiveLocation.TypeSystemDirectiveLocation TypeSystemDirectiveLocation.SCHEMA
  | Token.Name "SCALAR" ->
    DirectiveLocation.TypeSystemDirectiveLocation TypeSystemDirectiveLocation.SCALAR
  | Token.Name "OBJECT" ->
    DirectiveLocation.TypeSystemDirectiveLocation TypeSystemDirectiveLocation.OBJECT
  | Token.Name "ARGUMENT_DEFINITION" ->
    DirectiveLocation.TypeSystemDirectiveLocation
      TypeSystemDirectiveLocation.ARGUMENT_DEFINITION
  | Token.Name "INTERFACE" ->
    DirectiveLocation.TypeSystemDirectiveLocation TypeSystemDirectiveLocation.INTERFACE
  | Token.Name "ENUM" ->
    DirectiveLocation.TypeSystemDirectiveLocation TypeSystemDirectiveLocation.ENUM
  | Token.Name "ENUM_VALUE" ->
    DirectiveLocation.TypeSystemDirectiveLocation TypeSystemDirectiveLocation.ENUM_VALUE
  | Token.Name "INPUT_OBJECT" ->
    DirectiveLocation.TypeSystemDirectiveLocation TypeSystemDirectiveLocation.INPUT_OBJECT
  | Token.Name "INPUT_FIELD_DEFINITION" ->
    DirectiveLocation.TypeSystemDirectiveLocation
      TypeSystemDirectiveLocation.INPUT_FIELD_DEFINITION
  | Token.Name "FIELD" ->
    DirectiveLocation.ExecutableDirectiveLocation ExecutableDirectiveLocation.FIELD
  | Token.Name "FRAGMENT_SPREAD" ->
    DirectiveLocation.ExecutableDirectiveLocation
      ExecutableDirectiveLocation.FRAGMENT_SPREAD
  | Token.Name "INLINE_FRAGMENT" ->
    DirectiveLocation.ExecutableDirectiveLocation
      ExecutableDirectiveLocation.INLINE_FRAGMENT
  | Token.Name "QUERY" ->
    DirectiveLocation.ExecutableDirectiveLocation ExecutableDirectiveLocation.QUERY
  | Token.Name "MUTATION" ->
    DirectiveLocation.ExecutableDirectiveLocation ExecutableDirectiveLocation.MUTATION
  | Token.Name "SUBSCRIPTION" ->
    DirectiveLocation.ExecutableDirectiveLocation ExecutableDirectiveLocation.SUBSCRIPTION
  | Token.Name "FRAGMENT_DEFINITION" ->
    DirectiveLocation.ExecutableDirectiveLocation
      ExecutableDirectiveLocation.FRAGMENT_DEFINITION
  | _ ->
    failwith
      (Parse_error.show (Parse_error.UnexpectedDirectiveLocation (Token.show name)))
;;

let print_parser_state ?(msg = "parser: ") parser =
  Printf.printf
    "%s: cur: %s, peek: %s\n"
    msg
    (Token.show parser.cur_token)
    (Token.show parser.peek_token)
;;

let failwith_parser_error parser msg =
  failwith
    (Printf.sprintf
       "Parser Error (line: %d): %s -- cur: %s, peek: %s"
       (line parser)
       msg
       (Token.show parser.cur_token)
       (Token.show parser.peek_token))
;;

let rec parse parser = parse_ parser []

and parse_ parser definitions =
  match parser.cur_token with
  | Token.Eof -> Ok (parser, List.rev definitions)
  | _ ->
    (match parse_definition parser with
    | Ok (parser, def) -> parse_ (next_token parser) (def :: definitions)
    | Error err -> Error err)

and parse_definition parser =
  let parser, description =
    match parse_description parser with
    | parser, Some d -> next_token parser, Some d
    | parser, None -> parser, None
  in
  match parser.cur_token with
  | Token.Name "type" -> parse_type (next_token parser) description
  | Token.Name "schema" -> parse_schema parser description
  | Token.Name "scalar" -> parse_scalar (next_token parser) description
  | Token.Name "enum" -> parse_enum (next_token parser) description
  | Token.Name "union" -> parse_union (next_token parser) description
  | Token.Name "input" -> parse_input_type (next_token parser) description
  | Token.Comment comment ->
    Ok (parser, Definition.TypeDefinition (TypeDefinition.Comment comment))
  | Token.Query | Token.Mutation
  | Token.Name "query"
  | Token.Name "mutation"
  | Token.LeftBrace -> parse_executable_document parser
  | Token.Name "fragment" -> parse_fragment (next_token parser)
  | Token.Name "interface" -> parse_interface (next_token parser) description
  | Token.Name "directive" -> parse_directive_definition parser description
  | Token.Name "extend" -> parse_type_system_extension parser description
  | _ ->
    parser_error parser (Parse_error.UnexpectedDefinition (Token.show parser.cur_token))

and parse_type_system_extension parser description =
  match parser.peek_token with
  | Token.Name "type" | Token.Type -> parse_type_extension parser description
  | Token.Name "interface" -> parse_interface_extension parser description
  | Token.Name "union" -> parse_union_extension parser description
  | Token.Name "scalar" -> parse_scalar_extension parser description
  | Token.Name "enum" -> parse_enum_extension parser description
  | Token.Name "input" -> parse_input_extension parser description
  | _ ->
    parser_error
      parser
      (Parse_error.ExpectedValidTypeName
         ("parse_type_system_extension", Token.show parser.peek_token))

and parse_input_extension parser description =
  match expect_peek_name parser "input" with
  | parser, true ->
    let parser = next_token parser in
    let* parser, name = parse_name parser in
    let* parser, directives = parse_directives parser in
    let parser, ok = expect_peek_left_brace parser in
    (match parser, ok with
    | parser, true ->
      let* parser, fields = parse_type_definition parser in
      Ok
        ( parser
        , Definition.TypeDefinition
            (TypeDefinition.Input
               { name; fields; directives; description; builtin = false }) )
    | _, false ->
      Ok
        ( parser
        , Definition.TypeDefinition
            (TypeDefinition.Input
               { name; fields = []; directives; description; builtin = false }) ))
  | _ -> parser_error parser (Parse_error.ParseError "parse_interface_extension")

and parse_enum_extension parser description =
  match expect_peek_name parser "enum" with
  | parser, true ->
    let parser = next_token parser in
    let* parser, name = parse_name parser in
    let* parser, directives = parse_directives parser in
    let parser, ok = expect_peek_left_brace parser in
    (match parser, ok with
    | parser, true ->
      let parser = next_token parser in
      let* parser, values = parse_enum_values parser in
      Ok
        ( parser
        , Definition.TypeDefinition
            (TypeDefinition.Enum
               { name; values; directives; description; builtin = false }) )
    | _ ->
      Ok
        ( parser
        , Definition.TypeDefinition
            (TypeDefinition.Enum
               { name; values = []; directives; description; builtin = false }) ))
  | _ -> parser_error parser (Parse_error.ParseError "parse_enum_extension")

and parse_scalar_extension parser description =
  match expect_peek_name parser "scalar" with
  | parser, true ->
    let parser = next_token parser in
    let* parser, name = parse_name parser in
    let* parser, directives = parse_directives parser in
    Ok
      ( parser
      , Definition.TypeDefinition
          (TypeDefinition.Scalar { name; directives; description; builtin = false }) )
  | _ ->
    parser_error
      parser
      (Parse_error.ExpectedToken ("parse_scalar_extension", "expected scalar keyword"))

and parse_union_extension parser description =
  match expect_peek_name parser "union" with
  | parser, true ->
    let parser = next_token parser in
    let* parser, name = parse_name parser in
    let* parser, directives = parse_directives parser in
    let parser, ok = expect_peek_equal parser in
    (match parser, ok with
    | parser, true ->
      let* parser, members = parse_union_members parser in
      Ok
        ( parser
        , Definition.TypeDefinition
            (TypeDefinition.Union
               { name; members; directives; description; builtin = false }) )
    | _, false ->
      Ok
        ( parser
        , Definition.TypeDefinition
            (TypeDefinition.Union
               { name; members = []; directives; description; builtin = false }) ))
  | _ -> parser_error parser (Parse_error.ParseError "parse_union_extension")

and parse_interface_extension parser description =
  match expect_peek_name parser "interface" with
  | parser, true ->
    let parser = next_token parser in
    let* parser, name = parse_name parser in
    let* parser, directives = parse_directives parser in
    let parser, ok = expect_peek_left_brace parser in
    (match parser, ok with
    | parser, true ->
      let* parser, fields = parse_type_definition parser in
      Ok
        ( parser
        , Definition.TypeDefinition
            (TypeDefinition.Interface
               { name; fields; directives; description; builtin = false }) )
    | _, false ->
      Ok
        ( parser
        , Definition.TypeDefinition
            (TypeDefinition.Interface
               { name; fields = []; directives; description; builtin = false }) ))
  | _ -> parser_error parser (Parse_error.ParseError "parse_interface_extention")

and parse_type_extension parser description =
  match expect_peek_name parser "type" with
  | parser, true ->
    let parser = next_token parser in
    let* parser, name = parse_name parser in
    let* parser, directives = parse_directives parser in
    let* parser, implements = parse_implements parser in
    let parser, ok = expect_peek_left_brace parser in
    (match parser, ok with
    | parser, true ->
      let* parser, fields = parse_type_definition parser in
      Ok
        ( parser
        , Definition.TypeDefinition
            (TypeDefinition.Object
               { name; implements; fields; description; builtin = false }) )
    | _, false ->
      Ok
        ( parser
        , Definition.TypeDefinition
            (TypeDefinition.Object
               { name; implements; fields = []; description; builtin = false }) ))
  | _ -> parser_error parser (Parse_error.ParseError "parse_type_extention")

and parse_directive_definition parser description =
  let parser, ok = expect_peek_at parser in
  match ok with
  | true ->
    let parser = next_token parser in
    let* parser, name = parse_name parser in
    let* parser, args =
      match parser.peek_token with
      | Token.LeftParen ->
        let parser = next_token parser in
        parse_field_args parser
      | _ -> Ok (parser, [])
    in
    let parser, ok = expect_peek_on parser in
    (match ok with
    | true ->
      let parser =
        match parser.peek_token with
        | Token.Pipe -> next_token parser
        | _ -> parser
      in
      let rec parse_locations parser locations =
        match parser.peek_token with
        | Token.Pipe ->
          let parser = next_token parser in
          let loc = name_to_directive_location parser.peek_token in
          parse_locations (next_token parser) (loc :: locations)
        | _ ->
          Ok
            ( parser
            , Definition.Directive
                DirectiveDefinition.
                  { name; args; locations; description; builtin = false } )
      in
      parse_locations (next_token parser) [ name_to_directive_location parser.peek_token ]
    | false ->
      parser_error parser (Parse_error.ExpectedToken ("parse_directive", "expected 'on'")))
  | false ->
    parser_error parser (Parse_error.ExpectedToken ("parse_directive", "expected '@'"))

and parse_interface parser description =
  let* parser, name = parse_name parser in
  let* parser, directives = parse_directives parser in
  let parser, ok = expect_peek_left_brace parser in
  match parser, ok with
  | parser, true ->
    let* parser, fields = parse_type_definition parser in
    Ok
      ( parser
      , Definition.TypeDefinition
          (TypeDefinition.Interface
             { name; fields; directives; description; builtin = false }) )
  | _, false ->
    Ok
      ( parser
      , Definition.TypeDefinition
          (TypeDefinition.Interface
             { name; fields = []; directives; description; builtin = false }) )

and parse_fragment parser =
  let* parser, name = parse_name parser in
  match parser.peek_token with
  | Token.Name "on" ->
    let parser = next_token parser in
    let parser = next_token parser in
    let* parser, type_condition = parse_name parser in
    let parser, ok = expect_peek_left_brace parser in
    if ok
    then
      let* parser, selection = parse_selection_set parser in
      let parser, ok = expect_peek_right_brace parser in
      if ok
      then
        Ok
          ( parser
          , Definition.ExecutableDefinition
              (ExecutableDefinition.FragmentDefinition { name; type_condition; selection })
          )
      else
        parser_error
          parser
          (Parse_error.ExpectedToken
             ("parse_fragment", "expected right brace after fragment selection"))
    else
      parser_error
        parser
        (Parse_error.ExpectedToken
           ("parse_fragment", "expected left brace for fragment selection"))
  | _ ->
    parser_error
      parser
      (Parse_error.ExpectedToken ("parse_fragments", "expected 'on' token"))

and parse_executable_document parser =
  match parser.cur_token with
  | Token.LeftBrace -> failwith_parser_error parser "unnamed query is not supported"
  | Token.Name "query" -> parse_query parser
  | Token.Name "mutation" -> parse_mutation parser
  | _ ->
    parser_error
      parser
      (Parse_error.ExpectedToken
         ("parse_executable_document", "expected query or mutation"))

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
  match parser, ok with
  | parser, true ->
    let parser = next_token parser in
    let* parser, operation = parse_name parser in
    let* parser, args, operation =
      match parser.peek_token with
      | Token.LeftParen ->
        let* parser, args = parse_args parser in
        Ok (parser, args, operation)
      | _ -> Ok (parser, None, _name)
    in
    let parser, ok = expect_peek_left_brace parser in
    let* parser, selection =
      match parser, ok with
      | parser, true -> parse_selection_set parser
      | _ ->
        parser_error
          parser
          (Parse_error.ExpectedToken ("parse_query", "expected left brace for selection"))
    in
    let parser, ok = expect_peek_right_brace parser in
    (match ok with
    | false ->
      parser_error
        parser
        (Parse_error.ExpectedToken
           ("parse_query", "expected closing right brace for selection set"))
    | true ->
      let parser, ok = expect_peek_right_brace parser in
      (match ok with
      | false ->
        parser_error
          parser
          (Parse_error.ExpectedToken
             ("parse_query", "expected closing right brace for query"))
      | true ->
        Ok
          ( parser
          , Definition.ExecutableDefinition
              (ExecutableDefinition.OperationDefinition
                 { name = operation
                 ; operation = Operation.Query
                 ; args
                 ; variables = vars
                 ; selection
                 }) )))
  | _ ->
    parser_error parser (Parse_error.ExpectedToken ("parse_query", "expected left brace"))

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
    | Token.Colon ->
      let* parser, name = parse_name parser in
      let parser, ok = expect_peek_colon parser in
      (match parser, ok with
      | parser, true ->
        let parser = next_token parser in
        let* parser, var_name = parse_name parser in
        let arg = OperationArg.{ name; value = var_name } in
        parse_args' parser (arg :: args)
      | _ ->
        parser_error
          parser
          (Parse_error.ExpectedToken ("parse_args", "expected colon after var name")))
    | Token.RightParen ->
      let parser = next_token parser in
      Ok (parser, Some args)
    | _ ->
      parser_error
        parser
        (Parse_error.ExpectedToken ("parse_args", "peek should be colon"))
  in
  parse_args' parser []

and parse_variables parser =
  let parser = next_token parser in
  let parser = next_token parser in
  let rec parse_variables' parser vars =
    let parser = chomp parser Token.Comma in
    match parser.peek_token with
    | Token.Colon ->
      let* parser, name = parse_name parser in
      let parser, ok = expect_peek_colon parser in
      (match parser, ok with
      | parser, true ->
        let parser = next_token parser in
        let* parser, gql_type = parse_graphql_type parser in
        let var =
          ArgumentDefiniton.
            { name
            ; ty = gql_type
            ; default_value = None
            ; directives = []
            ; description = None
            }
        in
        parse_variables' parser (var :: vars)
      | _ ->
        parser_error
          parser
          (Parse_error.ExpectedToken ("parse_variables", "expected colon after var name")))
    | Token.RightParen -> Ok (parser, Some vars)
    | _ ->
      parser_error
        parser
        (Parse_error.ExpectedToken ("parse_variableds", "peek should be colon"))
  in
  parse_variables' parser []

and parse_mutation parser =
  let _parser = next_token parser in
  failwith "parse_mutation"

and parse_input_type parser description =
  let* parser, name = parse_name parser in
  let* parser, directives = parse_directives parser in
  let parser, ok = expect_peek_left_brace parser in
  match parser, ok with
  | parser, true ->
    let* parser, fields = parse_type_definition parser in
    Ok
      ( parser
      , Definition.TypeDefinition
          (TypeDefinition.Input { name; fields; directives; description; builtin = false })
      )
  | _, false ->
    Ok
      ( parser
      , Definition.TypeDefinition
          (TypeDefinition.Input
             { name; fields = []; directives; description; builtin = false }) )

and parse_enum parser description =
  let* parser, name = parse_name parser in
  let* parser, directives = parse_directives parser in
  let parser, ok = expect_peek_left_brace parser in
  if not ok
  then
    Ok
      ( parser
      , Definition.TypeDefinition
          (TypeDefinition.Enum
             EnumType.{ name; values = []; directives; description; builtin = false }) )
  else (
    let parser = next_token parser in
    let* parser, values = parse_enum_values parser in
    Ok
      ( parser
      , Definition.TypeDefinition
          (TypeDefinition.Enum
             EnumType.{ name; values; directives; description; builtin = false }) ))

and parse_enum_values parser =
  let rec parse_enum_value parser vals =
    let parser, enum_desc =
      match parse_description parser with
      | parser, Some d ->
        let parser = next_token parser in
        parser, Some d
      | _ -> parser, None
    in
    match parser.cur_token with
    | Token.RightBrace -> Ok (parser, List.rev vals)
    | Token.Comment _ -> parse_enum_value (next_token parser) vals
    | _ ->
      let* parser, name = parse_name parser in
      let* parser, directives = parse_directives parser in
      let parser = next_token parser in
      parse_enum_value
        parser
        (EnumMember.{ name; directives; description = enum_desc } :: vals)
  in
  parse_enum_value parser []

and parse_union parser description =
  let* parser, union_name = parse_name parser in
  let* parser, directives = parse_directives parser in
  let parser, ok = expect_peek_equal parser in
  if not ok
  then
    Ok
      ( parser
      , Definition.TypeDefinition
          (TypeDefinition.Union
             { name = union_name; members = []; directives; description; builtin = false })
      )
  else
    let* parser, members = parse_union_members parser in
    Ok
      ( parser
      , Definition.TypeDefinition
          (TypeDefinition.Union
             { name = union_name; members; directives; description; builtin = false }) )

and parse_union_members parser =
  let parser = chomp parser Token.Pipe in
  let parser = next_token parser in
  let parser, first_union_desc =
    match parse_description parser with
    | parser, Some d ->
      let parser = next_token parser in
      parser, Some d
    | _ -> parser, None
  in
  let* parser, name = parse_name parser in
  let rec parse_members' parser members =
    let parser, union_desc =
      match parse_description parser with
      | parser, Some d ->
        let parser = next_token parser in
        parser, Some d
      | _ -> parser, None
    in
    match parser.peek_token with
    | Token.Pipe ->
      let parser = next_token parser in
      let parser = next_token parser in
      let* parser, name = parse_name parser in
      parse_members' parser (BaseValue.{ name; description = union_desc } :: members)
    | Token.Comment _ -> parse_members' (next_token parser) members
    | _ -> Ok (parser, List.rev members)
  in
  parse_members' parser [ BaseValue.{ name; description = first_union_desc } ]

and parse_description parser =
  match parser.cur_token with
  | Token.StringLiteral s -> parser, Some s
  | _ -> parser, None

and parse_directives parser =
  let rec parse_directives' parser directives =
    match parser.peek_token with
    | Token.At ->
      let parser = next_token parser in
      let parser = next_token parser in
      let* parser, name = parse_name parser in
      let* parser, args = parse_directive_args parser in
      let directive = Directive.{ name; args } in
      parse_directives' parser (directive :: directives)
    | _ -> Ok (parser, List.rev directives)
  in
  parse_directives' parser []

and parse_directive_args parser =
  match parser.peek_token with
  | Token.LeftParen ->
    let parser = next_token parser in
    let rec parse_directive_args' parser args =
      match parser.peek_token with
      | Token.RightParen ->
        let parser = next_token parser in
        Ok (parser, List.rev args)
      | Token.Name _ ->
        let parser = next_token parser in
        let* parser, name = parse_name parser in
        (match peek_is parser Token.Colon with
        | true ->
          let* parser, value = parse_value_literal parser in
          (match value with
          | Some v ->
            parse_directive_args' parser (DirectiveArg.{ name; value = v } :: args)
          | None -> parse_directive_args' parser args)
        | false ->
          parser_error
            parser
            (Parse_error.ExpectedToken
               ("parse_directive_args", "expected colon after arg name")))
      | _ ->
        parser_error
          parser
          (Parse_error.ExpectedToken
             ("parse_directive_args", "expected name or right paren"))
    in
    parse_directive_args' parser []
  | _ -> Ok (parser, [])

and parse_scalar parser description =
  let* parser, name = parse_name parser in
  let* parser, directives = parse_directives parser in
  Ok
    ( parser
    , Definition.TypeDefinition
        (TypeDefinition.Scalar { name; directives; description; builtin = false }) )

and parse_schema parser description =
  let* parser, directives = parse_directives parser in
  match parser.peek_token with
  | Token.LeftBrace ->
    let parser = next_token parser in
    parse_schema_def (next_token parser) description directives
  | _ -> parser_error parser (Parse_error.ExpectedToken ("parse_schema", "left brace"))

and parse_schema_def parser description directives =
  let rec parse_schema_def' parser fields =
    match parser.peek_token with
    | Token.RightBrace | Token.Eof -> Ok (parser, List.rev fields)
    | _ ->
      let* parser, op = parse_schema_op_type parser in
      (match parser.peek_token with
      | Token.RightBrace -> Ok (parser, List.rev_append [ op ] fields)
      | Token.Name _ -> parse_schema_def' (next_token parser) (op :: fields)
      | Token.StringLiteral _ -> parse_schema_def' (next_token parser) (op :: fields)
      | _ ->
        parser_error
          parser
          (Parse_error.ExpectedToken ("parse_schema_def", "unexpected token")))
  in
  let* parser, fields = parse_schema_def' parser [] in
  let query = List.find fields ~f:(fun (name, _) -> name_is name "query") in
  let mutation = List.find fields ~f:(fun (name, _) -> name_is name "mutation") in
  let subscription = List.find fields ~f:(fun (name, _) -> name_is name "subscription") in
  match parser.peek_token with
  | Token.RightBrace ->
    Ok
      ( next_token parser
      , Definition.Schema
          { query = make_schema_op query
          ; mutation = make_schema_op mutation
          ; subscription = make_schema_op subscription
          ; directives
          ; description
          } )
  | _ ->
    parser_error
      parser
      (Parse_error.ExpectedToken ("parse_schema_def", "expected closing right brace"))

and make_schema_op op =
  match op with
  | Some (_, o) -> Some BaseValue.{ name = o.name; description = o.description }
  | None -> None

and name_is a b = String.compare a b = 0

and parse_schema_op_type parser =
  let parser, description =
    match parse_description parser with
    | parser, Some d ->
      let parser = next_token parser in
      parser, Some d
    | _ -> parser, None
  in
  let* parser, op_name = parse_name parser in
  match parser.peek_token with
  | Token.Colon ->
    let parser = next_token parser in
    let parser = next_token parser in
    let* parser, name = parse_name parser in
    Ok (parser, (op_name, BaseValue.{ name; description }))
  | _ ->
    parser_error
      parser
      (Parse_error.ExpectedToken ("parse_schema_op_type", "expected colon"))

and parse_type parser description =
  let* parser, name = parse_name parser in
  let* parser, directives = parse_directives parser in
  let* parser, implements = parse_implements parser in
  let parser, ok = expect_peek_left_brace parser in
  match parser, ok with
  | parser, true ->
    let* parser, fields = parse_type_definition parser in
    Ok
      ( parser
      , Definition.TypeDefinition
          (TypeDefinition.Object
             { name; implements; fields; description; builtin = false }) )
  | _, false ->
    Ok
      ( parser
      , Definition.TypeDefinition
          (TypeDefinition.Object
             { name; implements; fields = []; description; builtin = false }) )

and parse_implements parser =
  match parser.peek_token with
  | Token.Name "implements" ->
    let parser = next_token parser in
    let parser = next_token parser in
    let rec parse_implements' parser names =
      let* parser, name = parse_name parser in
      match parser.peek_token with
      | Token.LeftBrace -> Ok (parser, List.rev (List.rev_append [ name ] names))
      | Token.Ampersand ->
        let parser = next_token parser in
        let parser = next_token parser in
        parse_implements' parser (name :: names)
      | _ ->
        parser_error
          parser
          (Parse_error.ExpectedToken
             ("parse_implements", "expected '&', name or left brace"))
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
      | _ -> parser, None
    in
    match parser.peek_token with
    | Token.RightBrace | Token.Eof -> Ok (parser, List.rev fields)
    | Token.Comment _ -> parse_type_def' (next_token parser) fields
    | Token.Name _ | Token.Type | Token.Input ->
      let parser = next_token parser in
      let* parser, name = parse_name parser in
      let* parser, args =
        match parser.peek_token with
        | Token.LeftParen ->
          let parser = next_token parser in
          parse_field_args parser
        | _ -> Ok (parser, [])
      in
      (match parser.peek_token with
      | Token.Colon ->
        let parser = next_token parser in
        let parser = next_token parser in
        let* parser, ty = parse_graphql_type parser in
        let* parser, default_value =
          match peek_is parser Token.Equal with
          | true -> parse_value_literal (next_token parser)
          | false -> Ok (parser, None)
        in
        let* parser, directives = parse_directives parser in
        let field =
          Field.{ name; args; directives; ty; default_value = None; description }
        in
        parse_type_def' parser (field :: fields)
      | Token.Name _ ->
        let parser = next_token parser in
        let* parser, ty = parse_graphql_type parser in
        let* parser, default_value =
          match peek_is parser Token.Equal with
          | true -> parse_value_literal (next_token parser)
          | false -> Ok (parser, None)
        in
        let* parser, directives = parse_directives parser in
        let field = Field.{ name; args; ty; directives; default_value; description } in
        parse_type_def' parser (field :: fields)
      | _ ->
        parser_error
          parser
          (Parse_error.ExpectedToken ("parse_type_definition", "expected a name or colon")))
    | _ ->
      parser_error
        parser
        (Parse_error.ExpectedToken
           ("parse_type_definition", "expected right brace, comment, or name"))
  in
  let* parser, td = parse_type_def' parser [] in
  let parser, ok = expect_peek_right_brace parser in
  match parser, ok with
  | parser, true -> Ok (parser, td)
  | _, false ->
    parser_error
      parser
      (Parse_error.ExpectedToken ("parse_type_definition", "expected right brace"))

and parse_field_args parser =
  let rec parse_field_args' parser args =
    let parser = chomp parser Token.Comma in
    let parser = chomp_comment parser in
    let parser, arg_desc =
      match parser.peek_token with
      | Token.StringLiteral _ -> parse_description (next_token parser)
      | _ -> parser, None
    in
    match parser.peek_token with
    | Token.RightParen ->
      let parser =
        if not (current_token_is parser Token.RightParen)
        then next_token parser
        else parser
      in
      Ok (parser, List.rev args)
    | Token.Name _ | Token.Type | Token.Input ->
      let parser = next_token parser in
      (match parser.peek_token with
      | Token.Colon ->
        let* parser, name = parse_name parser in
        (match parser.peek_token with
        | Token.Colon ->
          let parser = next_token parser in
          let parser = next_token parser in
          let* parser, gql_type = parse_graphql_type parser in
          let* parser, default_value =
            match parser.peek_token with
            | Token.Equal -> parse_value_literal parser
            | _ -> Ok (parser, None)
          in
          let* parser, directives = parse_directives parser in
          let parser, arg =
            ( parser
            , ArgumentDefiniton.
                { name; ty = gql_type; default_value; directives; description = arg_desc }
            )
          in
          parse_field_args' parser (arg :: args)
        | _ -> failwith "parse_field_args")
      | _ -> failwith_parser_error parser "expected args")
    | _ -> failwith_parser_error parser "parse_field_args..."
  in
  parse_field_args' parser []

and parse_value_literal parser =
  let parser = next_token parser in
  match parser.peek_token with
  | Token.StringLiteral s ->
    let parser = next_token parser in
    Ok (parser, Some (Value.String s))
  | Token.Number { kind; value } ->
    Ok (next_token parser, Some (Value.Int (int_of_string value)))
  | Token.LeftBracket ->
    (* let parser = next_token parser in *)
    let rec parse_list_values parser vals =
      let parser = chomp parser Token.Comma in
      match parser.peek_token with
      | Token.RightBracket ->
        let parser = next_token parser in
        Ok (parser, Some (Value.List (List.rev vals)))
      | _ ->
        let* parser, value = parse_value_literal parser in
        (match value with
        | Some v -> parse_list_values parser (v :: vals)
        | _ -> parse_list_values parser vals)
    in
    parse_list_values parser []
  | Token.LeftBrace ->
    let parser = next_token parser in
    let rec parse_object_values parser pairs =
      let parser = chomp parser Token.Comma in
      match parser.peek_token with
      | Token.RightBrace ->
        let parser = next_token parser in
        Ok (parser, Some (Value.Object (List.rev pairs)))
      | Token.Name name ->
        let parser = next_token parser in
        (match peek_is parser Token.Colon with
        | true ->
          let parser = next_token parser in
          let* parser, value = parse_value_literal parser in
          (match value with
          | Some v -> parse_object_values parser ((name, v) :: pairs)
          | _ -> parse_object_values parser pairs)
        | _ ->
          parser_error
            parser
            (Parse_error.ExpectedToken
               ("parse_value_literal", "expected colon while parsing object literal")))
      | _ ->
        parser_error
          parser
          (Parse_error.ExpectedToken
             ("parse_value_literal", "expected right brace or name"))
    in
    parse_object_values parser []
  | Token.True -> Ok (next_token parser, Some (Value.Boolean true))
  | Token.False -> Ok (next_token parser, Some (Value.Boolean false))
  | Token.Null -> Ok (next_token parser, Some Value.Null)
  | _ -> Ok (parser, None)

and parse_graphql_type parser =
  match parser.cur_token with
  | Token.LeftBracket ->
    let parser = next_token parser in
    let* parser, gql_type = parse_graphql_type parser in
    let parser, ok = expect_peek_right_bracket parser in
    (match parser, ok with
    | parser, true ->
      (match parser.peek_token with
      | Token.Exclamation ->
        let parser = next_token parser in
        Ok (parser, GraphqlType.NonNullType (GraphqlType.ListType gql_type))
      | _ -> Ok (parser, GraphqlType.ListType gql_type))
    | _ ->
      parser_error
        parser
        (Parse_error.ExpectedToken
           ("parse_graphql_type", "expected closing right bracket for list type")))
  | Token.Name _ ->
    let* parser, name = parse_name parser in
    (match parser.peek_token with
    | Token.Exclamation ->
      let parser = next_token parser in
      Ok (parser, GraphqlType.NonNullType (GraphqlType.NamedType name))
    | _ -> Ok (parser, GraphqlType.NamedType name))
  | _ ->
    parser_error
      parser
      (Parse_error.ExpectedToken ("parse_graphql_type", "expected a name or left bracket"))

and parse_name parser =
  match parser.cur_token with
  | Token.Name n -> Ok (parser, n)
  | Token.Type ->
    Ok (parser, Token.to_name Token.Type)
    (* HACK: if type is seen where a name is expected than set name to "type" *)
  | Token.Input ->
    Ok (parser, Token.to_name Token.Input)
    (* HACK: if type is seen where a name is expected than set name to "input" *)
  | _ -> parser_error parser (Parse_error.ExpectedToken ("parse_name", "expected name"))
;;

let mark_type_as_builtin def =
  match def with
  | TypeDefinition.Scalar s ->
    TypeDefinition.Scalar
      { name = s.name
      ; directives = s.directives
      ; description = s.description
      ; builtin = true
      }
  | TypeDefinition.Object o ->
    TypeDefinition.Object
      { name = o.name
      ; implements = o.implements
      ; fields = o.fields
      ; description = o.description
      ; builtin = true
      }
  | TypeDefinition.Interface i ->
    TypeDefinition.Interface
      { name = i.name
      ; fields = i.fields
      ; directives = i.directives
      ; description = i.description
      ; builtin = true
      }
  | TypeDefinition.Union u ->
    TypeDefinition.Union
      { name = u.name
      ; members = u.members
      ; directives = u.directives
      ; description = u.description
      ; builtin = true
      }
  | TypeDefinition.Enum e ->
    TypeDefinition.Enum
      { name = e.name
      ; values = e.values
      ; directives = e.directives
      ; description = e.description
      ; builtin = true
      }
  | TypeDefinition.Input i ->
    TypeDefinition.Input
      { name = i.name
      ; fields = i.fields
      ; directives = i.directives
      ; description = i.description
      ; builtin = true
      }
  | _ -> def
;;

let mark_directive_as_builtin dir =
  DirectiveDefinition.
    { name = dir.name
    ; args = dir.args
    ; locations = dir.locations
    ; description = dir.description
    ; builtin = true
    }
;;

let mark_builtin defs doc =
  let rec mark' defs marked =
    match defs with
    | [] -> List.rev marked
    | def :: rest ->
      let def =
        match doc with
        | true, doc ->
          (match def with
          | Definition.TypeDefinition td ->
            Definition.TypeDefinition (mark_type_as_builtin td)
          | Definition.Directive d -> Definition.Directive (mark_directive_as_builtin d)
          | _ -> def)
        | false, _ -> def
      in
      mark' rest (def :: marked)
  in
  mark' defs []
;;

let parse_document input =
  let lexer = Lexer.init input in
  let parser = init lexer in
  let* _, definitions = parse parser in
  Ok (Ast.Document definitions)
;;

let parse_documents docs =
  let rec parse_many' docs definitions =
    match docs with
    | [] -> Ok (List.rev definitions)
    | doc :: rest ->
      let _, input = doc in
      let lexer = Lexer.init input in
      let parser = init lexer in
      let* _, defs = parse parser in
      let defs = mark_builtin defs doc in
      parse_many' rest (List.append defs definitions)
  in
  let* definitions = parse_many' docs [] in
  Ok (Ast.Document definitions)
;;

(* let parse_documents docs = *)
(*   let document = List.fold_left  docs *)
(*   Fmt.failwith "parse list of documents" *)

let string_of_definition = function
  | Definition.TypeDefinition def -> Fmt.str "  %s@." (TypeDefinition.show def)
  | Definition.Schema s -> Fmt.str "  %s@." (Schema.show s)
  | Definition.ExecutableDefinition e -> Fmt.str "  %s@." (ExecutableDefinition.show e)
  | Definition.Directive d -> Fmt.str "  %s@." (DirectiveDefinition.show d)
;;

let print_node = function
  | Ast.Document document ->
    Fmt.pr "Document: [@.";
    List.iter document ~f:(fun d -> Fmt.pr "  %s@." (string_of_definition d));
    Fmt.pr "]@."
  | _ -> failwith "yaya"
;;

module Test = struct
  let expect_document input =
    let lexer = Lexer.init input in
    let parser = init lexer in
    match parse parser with
    | Ok (_, program) -> print_node (Ast.Document program)
    | Error _ -> Fmt.pr "error parsing document"
  ;;

  (* match program with
    | Ok program -> print_node program
    | Error msg -> Fmt.failwith "error...%s" msg
    *)

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
             (Scalar
            { name = "UUID"; directives = [];
              description = (Some "Scalar Description"); builtin = false })

             (Object
            { name = "Query"; implements = [];
              fields =
              [{ name = "foo"; args = []; ty = (NamedType "String"); directives = [];
                 default_value = None; description = (Some "field description") };
                { name = "bar"; args = []; ty = (NonNullType (NamedType "String"));
                  directives = []; default_value = None; description = None };
                { name = "baz"; args = []; ty = (ListType (NamedType "String"));
                  directives = []; default_value = None; description = None };
                { name = "qux"; args = [];
                  ty = (NonNullType (ListType (NamedType "String"))); directives = [];
                  default_value = None; description = None };
                { name = "get"; args = [];
                  ty = (NonNullType (ListType (NonNullType (NamedType "String"))));
                  directives = []; default_value = None; description = None };
                { name = "set"; args = [];
                  ty =
                  (NonNullType
                     (ListType
                        (NonNullType (ListType (NonNullType (NamedType "String"))))));
                  directives = []; default_value = None; description = None }
                ];
              description = (Some "description for type"); builtin = false })

         ]

                 |}]
  ;;

  let%expect_test "testDocumentSchema" =
    let input =
      {|
              "schema description"
                    schema @skip @example {
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
        subscription = None;
        directives =
        [{ name = "skip"; args = [] }; { name = "example"; args = [] }];
        description = (Some "schema description") }

          (Scalar
         { name = "Status"; directives = []; description = None; builtin = false })

      ] |}]
  ;;

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
          (Scalar
         { name = "UUID"; directives = []; description = None; builtin = false })

          (Object
         { name = "Query"; implements = [];
           fields =
           [{ name = "fooQuery";
              args =
              [{ name = "arg1"; ty = (NamedType "Int"); default_value = None;
                 directives = []; description = None };
                { name = "arg2"; ty = (NonNullType (NamedType "Boolean"));
                  default_value = None; directives = []; description = None }
                ];
              ty = (NamedType "String"); directives = []; default_value = None;
              description = None }
             ];
           description = None; builtin = false })

          (Object
         { name = "Mutation"; implements = [];
           fields =
           [{ name = "fooMut";
              args =
              [{ name = "fooArg1"; ty = (NamedType "String"); default_value = None;
                 directives = []; description = None };
                { name = "fooArg2"; ty = (NonNullType (NamedType "Int"));
                  default_value = None; directives = [];
                  description = (Some "arg description") }
                ];
              ty = (NonNullType (ListType (NonNullType (NamedType "String"))));
              directives = []; default_value = None; description = None };
             { name = "bar";
               args =
               [{ name = "barArg1"; ty = (NonNullType (NamedType "Boolean"));
                  default_value = None; directives = []; description = None };
                 { name = "barArg2"; ty = (NamedType "Boolean");
                   default_value = None; directives = []; description = None }
                 ];
               ty = (NonNullType (ListType (NonNullType (NamedType "String"))));
               directives = []; default_value = None; description = None }
             ];
           description = None; builtin = false })

      ] |}]
  ;;

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
              directives = []; description = (Some "union description");
              builtin = false })

         ] |}]
  ;;

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
                [{ name = "BAR"; directives = []; description = (Some "bar description")
                   };
                  { name = "FOOBAR"; directives = []; description = None };
                  { name = "BAZ"; directives = []; description = (Some "baz desc") }];
                directives = []; description = (Some "Enum description");
                builtin = false })

           ] |}]
  ;;

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
               (Scalar
              { name = "Cost"; directives = []; description = None; builtin = false })

               (Comment "cost type")

               (Comment "this is a top-level comment")

               (Union
              { name = "bar";
                members =
                [{ name = "Bar"; description = None };
                  { name = "Foobar"; description = None };
                  { name = "Qux"; description = None }];
                directives = []; description = None; builtin = false })

               (Enum
              { name = "Kind";
                values =
                [{ name = "NORTH"; directives = []; description = None };
                  { name = "SOUTH"; directives = []; description = None };
                  { name = "EAST"; directives = []; description = None };
                  { name = "WEST"; directives = []; description = None }];
                directives = []; description = None; builtin = false })

               (Object
              { name = "Mutation"; implements = [];
                fields =
                [{ name = "foo"; args = []; ty = (NamedType "String"); directives = [];
                   default_value = None; description = None }
                  ];
                description = None; builtin = false })

           ] |}]
  ;;

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
                [{ name = "category"; args = []; ty = (NamedType "String");
                   directives = []; default_value = None; description = None };
                  { name = "name"; args = []; ty = (NonNullType (NamedType "String"));
                    directives = []; default_value = None;
                    description = (Some "input field desc") };
                  { name = "labels"; args = [];
                    ty = (ListType (NonNullType (NamedType "String"))); directives = [];
                    default_value = None; description = None }
                  ];
                directives = []; description = (Some "Input description");
                builtin = false })

           ] |}]
  ;;

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
                   [{ name = "id"; ty = (NamedType "UUID"); default_value = None;
                      directives = []; description = None };
                     { name = "name"; ty = (NamedType "String"); default_value = None;
                       directives = []; description = None }
                     ];
                   ty = (NamedType "String"); directives = []; default_value = None;
                   description = None };
                  { name = "name"; args = []; ty = (NonNullType (NamedType "String"));
                    directives = []; default_value = None; description = None };
                  { name = "labels"; args = [];
                    ty = (ListType (NonNullType (NamedType "String"))); directives = [];
                    default_value = None; description = None }
                  ];
                description = None; builtin = false })

           ] |}]
  ;;

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
                   [{ name = "input"; ty = (NonNullType (NamedType "String"));
                      default_value = None; directives = []; description = None };
                     { name = "type"; ty = (NamedType "String"); default_value = None;
                       directives = []; description = None }
                     ];
                   ty = (NamedType "String"); directives = []; default_value = None;
                   description = None }
                  ];
                description = None; builtin = false })

           ] |}]
  ;;

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
  ;;

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
           (Some [{ name = "$var1"; ty = (NamedType "String");
                    default_value = None; directives = []; description = None }
                   ]);
           selection = [(Field "field")] })

      ]

|}]
  ;;

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
           (Some [{ name = "$var1"; ty = (NamedType "String");
                    default_value = None; directives = []; description = None }
                   ]);
           selection = [(SpreadField "field")] })

      ]

|}]
  ;;

  let%expect_test "testFragment" =
    let input = {|
fragment Actions on Actions {
  view
  edit
  add
  delete
}
|} in
    expect_document input;
    [%expect
      {|
      Document: [
          (FragmentDefinition
         { name = "Actions"; type_condition = "Actions";
           selection =
           [(Field "delete"); (Field "add"); (Field "edit"); (Field "view")] })

      ] |}]
  ;;

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
           [{ name = "bar"; args = []; ty = (NamedType "String"); directives = [];
              default_value = None; description = None };
             { name = "baz"; args = []; ty = (NonNullType (NamedType "String"));
               directives = []; default_value = None;
               description = (Some "field desc") };
             { name = "var"; args = [];
               ty = (NonNullType (ListType (NamedType "String"))); directives = [];
               default_value = None; description = (Some "field block desc") }
             ];
           directives = []; description = (Some "interface description");
           builtin = false })

      ]

       |}]
  ;;

  let%expect_test "testInterfaceImplements" =
    let input = {|
type Person implements NamedEntity {
  name: String
  age: Int
}
|} in
    expect_document input;
    [%expect
      {|
      Document: [
          (Object
         { name = "Person"; implements = ["NamedEntity"];
           fields =
           [{ name = "name"; args = []; ty = (NamedType "String"); directives = [];
              default_value = None; description = None };
             { name = "age"; args = []; ty = (NamedType "Int"); directives = [];
               default_value = None; description = None }
             ];
           description = None; builtin = false })

      ]
|}]
  ;;

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
           [{ name = "name"; args = []; ty = (NamedType "String"); directives = [];
              default_value = None; description = None };
             { name = "age"; args = []; ty = (NamedType "Int"); directives = [];
               default_value = None; description = None }
             ];
           description = None; builtin = false })

      ]
|}]
  ;;

  let%expect_test "testDirectives" =
    let input = {|
    "directive desc"
directive @example on FIELD_DEFINITION

|} in
    expect_document input;
    [%expect
      {|
      Document: [
          { name = "example"; args = [];
        locations = [(TypeSystemDirectiveLocation FIELD_DEFINITION)];
        description = (Some "directive desc"); builtin = false }

      ]
  |}]
  ;;

  let%expect_test "testDirectivesMultipleFields" =
    let input =
      {|
    directive @example on
  | FIELD_DEFINITION
  | OBJECT
  | INTERFACE

        type Foo {
bar: String
}
|}
    in
    expect_document input;
    [%expect
      {|
      Document: [
          { name = "example"; args = [];
        locations =
        [(TypeSystemDirectiveLocation INTERFACE);
          (TypeSystemDirectiveLocation OBJECT);
          (TypeSystemDirectiveLocation FIELD_DEFINITION)];
        description = None; builtin = false }

          (Object
         { name = "Foo"; implements = [];
           fields =
           [{ name = "bar"; args = []; ty = (NamedType "String"); directives = [];
              default_value = None; description = None }
             ];
           description = None; builtin = false })

      ]

|}]
  ;;

  let%expect_test "testDirectivesMultipleFields" =
    let input =
      {|
    directive @deprecated(
  reason: String
) on FIELD_DEFINITION | ENUM_VALUE

|}
    in
    expect_document input;
    [%expect
      {|
      Document: [
          { name = "deprecated";
        args =
        [{ name = "reason"; ty = (NamedType "String"); default_value = None;
           directives = []; description = None }
          ];
        locations =
        [(TypeSystemDirectiveLocation ENUM_VALUE);
          (TypeSystemDirectiveLocation FIELD_DEFINITION)];
        description = None; builtin = false }

      ]
|}]
  ;;

  let%expect_test "testDirectiveWithArgDefaultValue" =
    let input =
      {|

directive @deprecated(
  reason: String = "No longer supported"
  id: Int = 42
) on FIELD_DEFINITION | ENUM_VALUE
    |}
    in
    expect_document input;
    [%expect
      {|
      Document: [
          { name = "deprecated";
        args =
        [{ name = "reason"; ty = (NamedType "String");
           default_value = (Some (String "No longer supported")); directives = [];
           description = None };
          { name = "id"; ty = (NamedType "Int"); default_value = (Some (Int 42));
            directives = []; description = None }
          ];
        locations =
        [(TypeSystemDirectiveLocation ENUM_VALUE);
          (TypeSystemDirectiveLocation FIELD_DEFINITION)];
        description = None; builtin = false }

      ]

    |}]
  ;;

  let%expect_test "testArgDirectiveWithDefaultValue" =
    let input =
      {|

type ExampleType {
  newField: String
  oldField: String @deprecated(reason: "Use `newField`.")
}    |}
    in
    expect_document input;
    [%expect
      {|
      Document: [
          (Object
         { name = "ExampleType"; implements = [];
           fields =
           [{ name = "newField"; args = []; ty = (NamedType "String");
              directives = []; default_value = None; description = None };
             { name = "oldField"; args = []; ty = (NamedType "String");
               directives =
               [{ name = "deprecated";
                  args = [{ name = "reason"; value = (String "Use `newField`.") }]
                  }
                 ];
               default_value = None; description = None }
             ];
           description = None; builtin = false })

      ]

    |}]
  ;;
end
