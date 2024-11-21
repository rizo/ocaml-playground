open struct
  module Ast = Ppxlib.Ast_builder.Make (struct
    let loc = Ppxlib.Location.none
  end)
  let mknoloc = Ppxlib.Loc.make ~loc:Ppxlib.Location.none
end

let () =
  let x = 1 in
  let y = 2 in
  let open Ast in
  pexp_let Nonrecursive
    [ value_binding ~pat:(ppat_var (mknoloc "z")) ~expr:(eint 200) ]
    (elist [ eint x; eapply (evar "+") [ eint 100; eint y ]; evar "z" ])
  |> Format.printf "%a@." Ppxlib.Pprintast.expression
