open Base
    open Ast


let rec type_check node =
  let env = Environment.empty () in
  match node with
  | Ast.Document d -> type_check_document d env
  | _ -> failwith "unexpected node"

and type_check_document doc env=
  let rec type_check_document' defs env =
    match defs with
    | [] -> ()
    | def :: rest ->
      let () = type_check_def def env in
      type_check_document' rest env
  in
  type_check_document' doc env

and type_check_def def env =
  match def with
  | Definition.TypeDefinition td -> type_check_type_def td env
  | _ -> failwith "unexpected def"

and type_check_type_def td _env =
  Fmt.pr "%s@"(TypeDefinition.show td)


module Test = struct
  let type_check_document input =
    let lexer = Lexer.init input in
    let parser = Parser.init lexer in
    let program = Parser.parse parser in
    match program with
    | Ok program -> type_check program
    | Error msg -> Fmt.failwith "error...%s" msg
  (* | Error msg -> Fmt.failwith "%a@." pp_parse_error msg *)

(*   let%expect_test "testTypeCheck" = *)
(*     let input = {| *)
(*       type Foo { *)
(*        bar: String *)
(* } *)

(*       type Baz { *)
(*       qux: Int! *)
(* } *)
(* |} in *)
(*     type_check_document input; *)
(*     [%expect {| *)

(* |}] *)

  end
