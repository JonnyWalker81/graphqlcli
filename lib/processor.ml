open Base
open Jingoo
(* open Core *)

let ( let* ) res f = Base.Result.bind res ~f
let ( let+ ) res f = Option.bind res ~f

let process_object kind fields =
  let rec process_fields fields =
    match fields with
    | [] -> Ok ()
    | f :: rest ->
      let () = Fmt.pr "%s,%s\n" kind f.Ast.Field.name in
      process_fields rest
  in
  process_fields fields
;;

let process_def kind def =
  match def with
  | Ast.Definition.TypeDefinition td ->
    (match td with
    | Ast.TypeDefinition.Object o ->
      let () = Fmt.pr "%s: %s\n" kind o.name in
      process_object kind o.fields
    | _ ->
      let () = Fmt.pr "" in
      Ok ())
  | _ -> Ok ()
;;

let build_csv doc output =
  let () = Fmt.pr "building csv: %s\n" output in
  let () = Fmt.pr "operation,name\n" in
  match doc with
  | Ast.Document defs ->
    let rec process_document doc =
      match doc with
      | [] -> Ok ()
      | def :: rest ->
        let def_name = Ast.Definition.name def in
        if phys_equal def_name "Query" || phys_equal def_name "Mutation"
        then
          (* let () = Fmt.pr "def: %s\n" (Ast.Definition.name def) in *)
          let* _res = process_def def_name def in
          (* let () = Fmt.pr "\n" in *)
          process_document rest
        else process_document rest
    in
    process_document defs
  | _ -> Ok ()
;;

let process_query_def def =
  let t =
    Jg_template.from_file
      "lib/query.jingoo"
      ~models:[ "msg", Jg_types.Tstr (Ast.Definition.name def) ]
  in
  let () = Fmt.pr "%s" t in
  Ok ()
;;

let generate_graphql_client_queries document _callback =
  let () = _callback () in
  let () = Fmt.pr "Generating GraphQL client queries...\n" in
  match document with
  | Ast.Document defs ->
    let rec process_document_queries defs =
      match defs with
      | [] -> Ok ()
      | def :: rest ->
        let* _res = process_query_def def in
        process_document_queries rest
    in
    process_document_queries defs
  | _ -> Ok ()
;;
