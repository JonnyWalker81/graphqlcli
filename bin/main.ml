open Core
open Graphqlcli

let ( let* ) res f = Base.Result.bind res ~f

let parse file =
  let files =
    match Sys.is_directory file with
    | `Yes -> Sys.ls_dir file
    | _ -> [ "./prelude.graphql"; file ]
  in
  let docs = List.map files ~f:(fun input -> In_channel.read_all input) in
  let () = Fmt.pr "Parsing schema: %s\n" file in
  let document = Parser.parse_documents docs in
  match document with
  | Ok document ->
    let () = Fmt.pr "Validating schema: %s\n" file in
    let validator = Validator.init document in
    let validator = Validator.validate validator document in
    (match validator with
    | Ok _ -> Fmt.pr "Vaidated schema: %s\n" file
    | Error msg -> Fmt.pr "Validation Error: %s" (Parse_error.show msg))
  | Error e -> Fmt.pr "Error parsing schema: %s" e
;;

let command =
  Command.basic
    ~summary:"Parse GraphQL schema file"
    ~readme:(fun () -> "")
    Command.Param.(
      map (anon ("filename" %: string)) ~f:(fun filename () -> parse filename))
;;

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
