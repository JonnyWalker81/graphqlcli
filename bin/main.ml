open Core
open Graphqlcli

let parse file =
  let contents = In_channel.read_all file in
  let document = Parser.parse_document contents in
  match document with
  | Ok _ -> Printf.printf "parsed document\n"
  | Error e -> Printf.printf "Error parsing document: %s" e
;;

let command =
  Command.basic
    ~summary:"Parse GraphQL schema file"
    ~readme:(fun () -> "")
    Command.Param.(
      map (anon ("filename" %: string)) ~f:(fun filename () -> parse filename))
;;

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
