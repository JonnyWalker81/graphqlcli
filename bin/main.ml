open Core
open Graphqlcli

let do_hash file =
  Md5.digest_file_blocking file |> Md5.to_hex |> print_endline

let parse file =
  let contents = In_channel.read_all file in
  let lexer = Lexer.init contents in
  let parser = Parser.init lexer in
  let document = Parser.parse parser in
  match document with
  | Ok _ ->
    Printf.printf "parsed document"
  | Error e -> Printf.printf "Error parsing document: %s" e

let command =
  Command.basic
    ~summary:"Parse GraphQL schema file"
    ~readme:(fun () -> "")
    Command.Param.(
      map
        (anon ("filename" %: string))
        ~f:(fun filename () -> parse filename))

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
