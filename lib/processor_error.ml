type t = ExpectedType of string

let show = function
  | ExpectedType s -> Fmt.pr "Expected: %s" s
;;
