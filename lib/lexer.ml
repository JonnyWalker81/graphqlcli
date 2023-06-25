open Base
open Core

type t = { input : string; position : int; read_position : int; ch : char; line: int}
[@@deriving show]

let null_byte = '\x00'

let read_char lexer =
  let read_to_end = lexer.read_position >= String.length lexer.input in
  let new_ch =
    if read_to_end then null_byte
    else String.get lexer.input lexer.read_position
  in
  {
    lexer with
    position = lexer.read_position;
    read_position = lexer.read_position + 1;
    ch = new_ch;
    line = 1;
  }

let init input =
  let lexer = { input; position = 0; read_position = 0; ch = null_byte; line = 1} in
  read_char lexer

let is_letter ch =
  match ch with
  | 'A' .. 'Z' -> true
  | 'a' .. 'z' -> true
  | '_' -> true
  | _ -> false

let is_whitespace ch =
  match ch with
  | ' ' -> true
  | '\t' -> true
  | '\n' ->  true
  | '\r' -> true
  | _ -> false

let is_digit ch = match ch with '0' .. '9' -> true | _ -> false
let is_letter_or_digit ch = is_letter ch || is_digit ch

let rec read_while condition lexer acc =
  if condition lexer.ch then
    read_while condition (read_char lexer) (acc ^ String.make 1 lexer.ch)
  else (lexer, acc)

let read_identifier lexer = read_while is_letter_or_digit lexer ""
let read_number lexer = read_while is_digit lexer ""

let rec skip_while condition lexer =
  if condition lexer.ch then skip_while condition (read_char lexer) else lexer

let peek lexer ch current matched =
  if lexer.read_position >= String.length lexer.input then (lexer, Token.Eof)
  else
    let peek_char = String.get lexer.input lexer.read_position in
    if Char.to_int peek_char = Char.to_int ch then
      let lexer = read_char lexer in
      (read_char lexer, matched)
    else (read_char lexer, current)

let peek_is lexer amount to_match =
  if lexer.position + amount >= String.length lexer.input then (lexer, false)
  else
    let peek_char = String.get lexer.input (lexer.position + amount) in
    if Char.to_int peek_char = Char.to_int to_match then (lexer, true)
    else (lexer, false)

let read_multiline_string lexer =
  let rec loop lexer str =
    match lexer.ch with
    | '\x00' -> (read_char lexer, str)
    | _ ->
      let lexer, is_quote_one = peek_is lexer 1 '"' in
      let lexer, is_quote_two = peek_is lexer 2 '"' in
      if is_quote_one && is_quote_two then (read_char lexer, str)
      else
        let str = str ^ Char.escaped lexer.ch in
        loop (read_char lexer) str
  in
  let lexer, s = loop lexer "" in
  (lexer, Token.StringLiteral s)

let read_string lexer =
  let lexer, is_quote_one = peek_is lexer 1 '"' in
  let lexer, is_quote_two = peek_is lexer 2 '"' in
  if is_quote_one && is_quote_two then read_multiline_string lexer
  else
    let rec loop lexer str =
      match lexer.ch with
      | '"' | '\x00' -> (read_char lexer, str)
      | _ ->
        let str = str ^ Char.escaped lexer.ch in
        loop (read_char lexer) str
    in
    let lexer, s = loop lexer "" in
    (lexer, Token.StringLiteral s)

let is_not_end_of_line ch = match ch with '\n' | '\r' -> false | _ -> true

let read_comment lexer =
  let lexer, comment = read_while is_not_end_of_line lexer "" in
  (lexer, Token.Comment comment)

let next_token lexer =
  let lexer = skip_while is_whitespace lexer in
  let ch = lexer.ch in
  match ch with
  | '{' -> (read_char lexer, Token.LeftBrace)
  | '}' -> (read_char lexer, Token.RightBrace)
  | '=' -> (read_char lexer, Token.Equal)
  | '(' -> (read_char lexer, Token.LeftParen)
  | ')' -> (read_char lexer, Token.RightParen)
  | ',' -> (read_char lexer, Token.Comma)
  | ':' -> (read_char lexer, Token.Colon)
  | ';' -> (read_char lexer, Token.Semicolon)
  | '[' -> (read_char lexer, Token.LeftBracket)
  | ']' -> (read_char lexer, Token.RightBracket)
  | '!' -> (read_char lexer, Token.Exclamation)
  | '|' -> (read_char lexer, Token.Pipe)
  | '"' -> read_string (read_char lexer)
  | '#' -> read_comment (read_char lexer)
  | '\x00' -> (read_char lexer, Token.Eof)
  | ch when is_letter ch ->
    let lexer, ident = read_identifier lexer in
    (lexer, Token.lookup_keyword ident)
  | _ -> (read_char lexer, Token.Illegal)

let tokenize input =
  let lexer = init input in
  let rec tokenize' lexer tokens =
    match next_token lexer with
    | _, Token.Eof -> List.rev_append tokens [ Token.Eof ]
    | lexer, token -> tokenize' lexer (token :: tokens)
  in
  tokenize' lexer []

module Test = struct
  let print_tokens tokens =
    List.iter tokens ~f:(fun token -> Fmt.pr "%s\n" @@ Token.show token)

  let%expect_test "testNextToken" =
    let input = {|=(){},;;|} in
    let tokens = tokenize input in
    print_tokens tokens;
    [%expect
      {|
      Token.Equal
      Token.LeftParen
      Token.RightParen
      Token.LeftBrace
      Token.RightBrace
      Token.Comma
      Token.Semicolon
      Token.Semicolon
      Token.Eof
|}]

  let%expect_test "testTokenize" =
    let input =
      {|
schema {
  query: MyQueryRootType
  mutation: MyMutationRootType
}

type MyQueryRootType {
  someField: String
}

type MyMutationRootType {
  setSomeField(to: String): String
}

enum Language {
  EN
  FR
  CH
}
scalar Time
input Point2D {
  x: Float
  y: Float
}
"this is a string"
#this is a comment
#this is another comment

union Uni = Foo | Bar

type Foo {
  bar: [String!]!
  baz: Boolean
}

        foo(bar: Boolean!
            baz: Boolean): String

        """This is a multi
         line string in graphQL"""
|}
    in
    let tokens = tokenize input in
    print_tokens tokens;
    [%expect
      {|
      Token.Schema
      Token.LeftBrace
      (Token.Name "query")
      Token.Colon
      (Token.Name "MyQueryRootType")
      (Token.Name "mutation")
      Token.Colon
      (Token.Name "MyMutationRootType")
      Token.RightBrace
      Token.Type
      (Token.Name "MyQueryRootType")
      Token.LeftBrace
      (Token.Name "someField")
      Token.Colon
      (Token.Name "String")
      Token.RightBrace
      Token.Type
      (Token.Name "MyMutationRootType")
      Token.LeftBrace
      (Token.Name "setSomeField")
      Token.LeftParen
      (Token.Name "to")
      Token.Colon
      (Token.Name "String")
      Token.RightParen
      Token.Colon
      (Token.Name "String")
      Token.RightBrace
      Token.Enum
      (Token.Name "Language")
      Token.LeftBrace
      (Token.Name "EN")
      (Token.Name "FR")
      (Token.Name "CH")
      Token.RightBrace
      Token.Scalar
      (Token.Name "Time")
      Token.Input
      (Token.Name "Point2D")
      Token.LeftBrace
      (Token.Name "x")
      Token.Colon
      (Token.Name "Float")
      (Token.Name "y")
      Token.Colon
      (Token.Name "Float")
      Token.RightBrace
      (Token.StringLiteral "this is a string")
      (Token.Comment "this is a comment")
      (Token.Comment "this is another comment")
      Token.Union
      (Token.Name "Uni")
      Token.Equal
      (Token.Name "Foo")
      Token.Pipe
      (Token.Name "Bar")
      Token.Type
      (Token.Name "Foo")
      Token.LeftBrace
      (Token.Name "bar")
      Token.Colon
      Token.LeftBracket
      (Token.Name "String")
      Token.Exclamation
      Token.RightBracket
      Token.Exclamation
      (Token.Name "baz")
      Token.Colon
      (Token.Name "Boolean")
      Token.RightBrace
      (Token.Name "foo")
      Token.LeftParen
      (Token.Name "bar")
      Token.Colon
      (Token.Name "Boolean")
      Token.Exclamation
      (Token.Name "baz")
      Token.Colon
      (Token.Name "Boolean")
      Token.RightParen
      Token.Colon
      (Token.Name "String")
      (Token.StringLiteral "")
      (Token.StringLiteral "This is a multi\\n         line string in graphQL")
      (Token.StringLiteral "")
      Token.Eof
|}]
end
