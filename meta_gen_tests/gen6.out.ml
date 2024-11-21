let () =
  let x = 1 in
  let y = 2 in
  Format.printf "%a@." Ppxlib.Pprintast.expression
    (Ppxlib.Ast_builder.Default.eint ~loc:Ppxlib.Location.none y)
