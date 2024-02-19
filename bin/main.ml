open Core
open Graphqlcli

(* let ( let* ) res f = Base.Result.bind res ~f *)

let parse file output generate =
  let files =
    match Sys_unix.is_directory file with
    | `Yes -> Sys_unix.ls_dir file
    | _ -> [ "./prelude.graphql"; file ]
  in
  let docs =
    List.map files ~f:(fun input ->
        let contents = In_channel.read_all input in
        match input with
        | "./prelude.graphql" -> true, contents
        | _ -> false, contents)
  in
  let () = Fmt.pr "Parsing schema: %s\n" file in
  let document = Parser.parse_documents docs in
  match document with
  | Ok document ->
    (* let () = (Parser.print_node document) in *)
    let () = Fmt.pr "Validating schema: %s\n" file in
    let validator = Validator.init document in
    let validator = Validator.validate validator document in
    (match validator with
    | Ok _ ->
      let () = Fmt.pr "Vaidated schema: %s\n" file in
      (match output, generate with
      | Some o, false ->
        let _res = Processor.build_csv document o in
        Fmt.pr "Processed schema..."
      | None, true ->
        let _res =
          Processor.generate_graphql_client_queries document (fun () ->
              Fmt.pr "callback \n")
        in
        Fmt.pr "Generated client types/queries/files..."
      | _ -> Fmt.pr "nothing to process...")
    | Error msg -> Fmt.pr "Validation Error: %s\n" (Validation_error.show msg))
  | Error (line, e) -> Fmt.pr "Error parsing schema(%d): %s\n" line (Parse_error.show e)
;;

let command =
  Command.basic
    ~summary:"Parse GraphQL schema file"
    ~readme:(fun () -> "")
    (let%map_open.Command use_string =
       flag "-c" (optional string) ~doc:"output csv of queries and mutations"
     and generate = flag "-g" no_arg ~doc:"generate client GraphQL queries"
     and filename =
       (* anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type)) *)
       anon (maybe_with_default "-" ("filename" %: string))
     in
     fun () ->
       match use_string, generate with
       | Some output, true | Some output, false -> parse filename (Some output) generate
       | None, true | None, false -> parse filename None generate
       | _ -> parse filename None false)
;;

(* Command.Param.( *)
(*   map (anon ("filename" %: string)) ~f:(fun filename () -> parse filename)) *)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
