include Stdlib.List

type ('a, 'b) continue_or_stop =
  | Continue of 'a
  | Stop of 'b

let fold_until ~init ~f ~finish list =
  let rec aux el list =
    match list with
    | [] -> finish el
    | x :: q ->
      match f el x with
      | Continue a -> aux a q
      | Stop b -> b
  in
  aux init list
