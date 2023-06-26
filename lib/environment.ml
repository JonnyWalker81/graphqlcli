open Base

type 'a t = { map : (string, 'a, String.comparator_witness) Map.t }

let empty () = { map = Map.empty (module String) }

let get env key =
  Map.find_exn env.map key

let set env key value =
  Map.set env.map ~key:key ~data:value
