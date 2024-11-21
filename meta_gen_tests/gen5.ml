open struct
  module Ast = Ppxlib.Ast_builder.Make (struct
    let loc = Ppxlib.Location.none
  end)
  let loc = Ppxlib.Location.none
end

let () =
  let x = 1 in
  let y = 2 in
  [%expr
    let z = 200 in
    [ [%e Ast.eint x]; 100 + [%e Ast.eint y]; z ]]
  |> Format.printf "%a@." Ppxlib.Pprintast.expression
