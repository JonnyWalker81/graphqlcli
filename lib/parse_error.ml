type t =
  | UnexpectedDefinition of string
  | ExpectedToken of (string * string)
  | ParseError of string
  | UnexpectedDirectiveLocation of string
  | ExpectedValidTypeName of (string * string)

let show = function
  | UnexpectedDefinition name -> Fmt.str "Unexpected Definition: %s" name
  | ExpectedToken (location, token) ->
    Fmt.str "Unexpected Token: %s, in %s" location token
  | ParseError tag -> Fmt.str "Error parsing document: %s" tag
  | UnexpectedDirectiveLocation name ->
    Fmt.str "Unexpected directive location name: %s" name
  | ExpectedValidTypeName (tag, name) ->
    Fmt.str "Invalid type token name (%s): %s" tag name
;;
