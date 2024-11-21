open struct
  module Ml = Ppxlib.Ast_builder.Default
  let noloc = Ppxlib.Location.none
end

open struct
  let gen_print_expr ~loc expr =
    Ml.eapply ~loc
      (Ml.evar ~loc "Format.printf")
      [
        Ml.estring ~loc "%a@."; Ml.evar ~loc "Ppxlib.Pprintast.expression"; expr;
      ]
end

open Ppxlib

let process_code_extension ~ctxt code =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  (* let code_code = Ppxlib.Ast_traverse.do_not_enter_let_module in *)
  gen_print_expr ~loc code


let process_int_extension ~ctxt int_expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Ml.pexp_apply ~loc
    (Ml.evar ~loc "Ppxlib.Ast_builder.Default.eint")
    [
      (Labelled "loc", Ml.evar ~loc "Ppxlib.Location.none"); (Nolabel, int_expr);
    ]


let code_extension =
  Extension.V3.declare "code" Ppxlib.Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    process_code_extension


let int_extension =
  Extension.V3.declare "int" Ppxlib.Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    process_int_extension


let () =
  Ppxlib.Driver.V2.register_transformation "code"
    ~extensions:[ code_extension; int_extension ]

(* ~impl:(fun _ctxt items ->
   match items with
   | [ { pstr_desc = Ppxlib.Parsetree.Pstr_eval (expr, _); _ } ] ->
     let loc = noloc in
     let print_exp =
       Ml.eapply ~loc
         (Ml.evar ~loc "Format.eprintf")
         [
           Ml.estring ~loc "%a@.";
           Ml.evar ~loc "Ppxlib.Pprintast.structure";
           expr;
         ]
     in
     [ Ml.pstr_eval ~loc print_exp [] ]
   | _ -> assert false *)
(* Format.eprintf "%a@." Ppxlib.Pprintast.structure items; )*)
