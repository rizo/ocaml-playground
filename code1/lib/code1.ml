open struct
  module B = Ppxlib.Ast_builder.Default
end

open Ppxlib

let expand ~ctxt _input =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let output = B.estring ~loc "HELLO oo" in
  output


let extension =
  Extension.V3.declare "eval" Ppxlib.Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand


let () =
  Ppxlib.Driver.V2.register_transformation "eval" ~extensions:[ extension ]
