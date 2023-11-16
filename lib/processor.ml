(* open Base *)
(* open Core *)

let process_object kind fields =
  let rec process_fields fields =
    match fields with
    | [] -> ()
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
    | _ -> Fmt.pr "")
  | _ -> Fmt.pr ""
;;

let build_csv doc output =
  let () = Fmt.pr "building csv: %s\n" output in
  let () = Fmt.pr "operation,name\n" in
  match doc with
  | Ast.Document defs ->
    let rec process_document doc =
      match doc with
      | [] -> ()
      | def :: rest ->
        let def_name = Ast.Definition.name def in
        if def_name = "Query" || def_name = "Mutation"
        then (
          (* let () = Fmt.pr "def: %s\n" (Ast.Definition.name def) in *)
          let () = process_def def_name def in
          (* let () = Fmt.pr "\n" in *)
          process_document rest)
        else process_document rest
    in
    process_document defs
  | _ -> ()
;;
