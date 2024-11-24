let code =
  let const_val = 1 in
  let computed_val = 2 + 2 in
  [%expr
    let delayed_val = 200 in
    [ 100 + [%int const_val]; [%int computed_val]; delayed_val ]]


let () = Format.printf "%a@." Ppxlib.Pprintast.expression code
