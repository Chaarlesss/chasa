
let rec compose = function
  | [] -> 0
  | cmp :: tl ->
    let r = cmp () in
    if r <> 0 then
      r
    else
      compose tl
