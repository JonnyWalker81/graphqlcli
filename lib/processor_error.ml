type t =
  | ExpectedType of string
  | DuplicateType of string
  | TypeNotFound of string
[@@deriving sexp]

let show = function
  | ExpectedType s -> Fmt.str "Expected: %s" s
  | DuplicateType s -> Fmt.str "Duplicate: %s" s
  | TypeNotFound s -> Fmt.str "Not found: %s" s
;;
