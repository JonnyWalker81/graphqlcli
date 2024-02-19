open Base
open Ast
(* open Jingoo *)
(* open Core *)

let ( let* ) res f = Base.Result.bind res ~f
let ( let+ ) res f = Option.bind res ~f

type 'a validator_map = (string, 'a, String.comparator_witness) Map.t

let pp_validator_map m f =
  Map.iteri ~f:(fun ~key ~data -> Fmt.pr "%s -> %s@." key (f data)) m
;;

type t = { types : TypeDefinition.t validator_map }

let init () = { types = Map.empty (module String) }

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

let collect_vars field =
  List.fold field.Ast.Field.args ~init:[] ~f:(fun acc arg ->
      let arg_name = arg.Ast.ArgumentDefinition.name in
      let arg_type = Ast.GraphqlType.name arg.Ast.ArgumentDefinition.ty in
      let arg_str = Printf.sprintf "$%s: %s" arg_name arg_type in
      arg_str :: acc)
;;

let collect_args field =
  List.fold field.Ast.Field.args ~init:[] ~f:(fun acc arg ->
      let arg_name = arg.Ast.ArgumentDefinition.name in
      let arg_str = Printf.sprintf "%s: $%s" arg_name arg_name in
      arg_str :: acc)
;;

let collect_fields fields =
  List.fold fields ~init:[] ~f:(fun acc field ->
      let field_name = field.Ast.Field.name in
      field_name :: acc)
;;

let print_spaces n =
  let rec loop n =
    match n with
    | 0 -> ()
    | _ ->
      let () = Fmt.pr " " in
      loop (n - 1)
  in
  loop n
;;

let rec process_field_type processor t seen indent =
  match t with
  | Ast.TypeDefinition.Object obj ->
    (* let () = Fmt.pr "processing fields...%s\n" obj.name in *)
    let rec process_fields fields =
      match fields with
      | [] -> Ok ()
      | field :: rest ->
        let () = print_spaces (indent * 3) in
        let () = Fmt.pr "%s\n" field.Ast.Field.name in
        let field_type_name = Ast.GraphqlType.name field.Ast.Field.ty in
        (* let () = Fmt.pr "%s\n" field_type_name in *)
        (* let field_type_kind = Ast.GraphqlType.kind field.Ast.Field.ty in *)
        let _ =
          match Map.find processor.types field_type_name with
          | None -> Error (Processor_error.TypeNotFound field_type_name)
          | Some tt ->
            (* let () = Fmt.pr "field type: %s\n" (TypeDefinition.kind t) in *)
            (match tt with
            | Ast.TypeDefinition.Object _oobj ->
              (* let () = Fmt.pr "   innner processing fields...%s\n" oobj.name in *)
              let () = print_spaces (indent * 3) in
              let () = Fmt.pr "{\n" in
              (* let f = collect_fields oobj.fields in *)
              (* let () = Fmt.pr "   fields: %s\n" (String.concat ~sep:", " f) in *)
              (match Set.mem seen field_type_name with
              | false ->
                let seen = Set.add seen field_type_name in
                let res = process_field_type processor tt seen (indent + 1) in
                let () = print_spaces (indent * 5) in
                let () = Fmt.pr "}\n" in
                res
              | _ -> Ok ())
            | _ -> Ok ())
        in
        (* let () = Fmt.pr "%s: %s\n" field.Ast.Field.name field_type_name in *)
        process_fields rest
    in
    process_fields obj.fields
  | _ -> Ok ()
;;

let process_query_def processor field =
  let args = collect_vars field in
  let () =
    Fmt.pr "query %s(%s) {\n" field.Ast.Field.name (String.concat ~sep:", " args)
  in
  let () =
    Fmt.pr
      "\t%s(%s){\n"
      field.Ast.Field.name
      (String.concat ~sep:", " (collect_args field))
  in
  (* let () = Fmt.pr "%s\n" (Ast.GraphqlType.name field.ty) in *)
  let field_type_name = Ast.GraphqlType.name field.ty in
  let _ =
    match Map.find processor.types field_type_name with
    | None -> Error (Processor_error.TypeNotFound field_type_name)
    | Some t ->
      let seen = Set.empty (module String) in
      process_field_type processor t seen 1
  in
  let () = Fmt.pr "\t}\n" in
  let () = Fmt.pr "}\n" in
  Ok ()
;;

(* let process_query_def field = *)
(*   let t = *)
(*     Jg_template2.from_file *)
(*       "lib/query.jingoo" *)
(*       ~models:[ "msg", Jg_types.Tstr field.Ast.Field.name ] *)
(*   in *)
(*   let () = Fmt.pr "%s" t in *)
(*   Ok () *)
(* ;; *)

let process_queries processor def =
  match def with
  | Ast.Definition.TypeDefinition td ->
    (match td with
    | Ast.TypeDefinition.Object obj ->
      let () = Fmt.pr "processing Query fields...%s\n" obj.name in
      let rec process_query_fields fields =
        match fields with
        | [] -> Ok ()
        | field :: rest ->
          (* let () = Fmt.pr "%s\n" field.Ast.Field.name in *)
          let _ = process_query_def processor field in
          process_query_fields rest
      in
      process_query_fields obj.fields
    | _ -> Ok ())
  | _ -> Ok ()
;;

let update_types_map processor key data =
  match String.length key with
  | 0 -> Ok processor
  | _ ->
    let () = Fmt.pr "key: %s\n" key in
    (match Map.add ~key ~data processor.types with
    | `Ok m -> Ok { types = m }
    | `Duplicate ->
      let () = Fmt.pr "Duplicate key: %s\n" key in
      Error (Processor_error.DuplicateType key))
;;

let add_type_def processor def = update_types_map processor (TypeDefinition.name def) def

let rec process_document processor defs =
  match defs with
  | [] -> Ok processor
  | def :: rest ->
    (match def with
    | Definition.TypeDefinition def ->
      let* processor = add_type_def processor def in
      process_document processor rest
    | _ -> process_document processor rest)
;;

let build_types_map processor node =
  match node with
  | Ast.Document defs -> process_document processor defs
  | _ -> Ok processor
;;

let generate_graphql_client_queries document _callback =
  let () = _callback () in
  let () = Fmt.pr "Generating GraphQL client queries...\n" in
  let processor = init () in
  let processor = build_types_map processor document in
  match processor with
  | Ok p ->
    let () = Fmt.pr "Processor types: %d\n" (Map.length p.types) in
    (match document with
    | Ast.Document defs ->
      let rec process_document_queries doc =
        match doc with
        | [] -> Ok ()
        | def :: rest ->
          let def_name = Ast.Definition.name def in
          if String.equal def_name "Query"
          then (
            let () = Fmt.pr "%s\n" def_name in
            let () = Fmt.pr "def: %s\n" (Ast.Definition.name def) in
            let* _res = process_queries p def in
            let () = Fmt.pr "\n" in
            process_document_queries rest)
          else process_document_queries rest
      in
      process_document_queries defs
    | _ -> Ok ())
  | Error e ->
    let () = Fmt.pr "Error: %s\n" (Processor_error.show e) in
    Error e
;;

module Test = struct
  let expect_document input =
    let doc = Parser.parse_document input in
    match doc with
    | Error (_, e) -> Fmt.pr "Error: %s\n" (Parse_error.show e)
    | Ok doc ->
      let _ = generate_graphql_client_queries doc (fun () -> Fmt.pr "callback \n") in
      ()
  ;;

  let%expect_test "testSimpleDocument" =
    let input =
      {|
      type Boo {
        ahh: String
      }
      type Baz {
        qux: Boo
        last: Foo
      }

      type Foo {
        bar: Baz
        name: String
      }

      type Query {
        hello: Foo
      }
      |}
    in
    expect_document input;
    [%expect {|
      |}]
  ;;
end
