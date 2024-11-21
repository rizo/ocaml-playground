open struct
  module Ml = Ppxlib.Ast_builder.Default
  let noloc = Ppxlib.Location.none
end

open struct
  let lift_print_expr ~loc expr =
    Ml.eapply ~loc
      (Ml.evar ~loc "Format.printf")
      [
        Ml.estring ~loc "%a@."; Ml.evar ~loc "Ppxlib.Pprintast.expression"; expr;
      ]


  let lift_int_expr ~loc int_expr =
    Ml.pexp_apply ~loc
      (Ml.evar ~loc "Ppxlib.Ast_builder.Default.eint")
      [
        (Labelled "loc", Ml.evar ~loc "Ppxlib.Location.none");
        (Nolabel, int_expr);
      ]
end

open Ppxlib

let lift =
  let loc = noloc in
  object (_self)
    inherit [Ppxlib.expression] Ppxlib.Ast_traverse.lift as super
    inherit! Ppxlib_metaquot_lifters.expression_lifters loc

    method! expression e =
      match e.pexp_desc with
      | Pexp_extension
          (( { txt = "int"; _ },
             PStr [ { pstr_desc = Pstr_eval (int_exp, _); _ } ] ) as ext) ->
        lift_int_expr ~loc int_exp
      | Pexp_extension (({ txt = "int"; _ }, _) as ext) ->
        failwith "invalid [%int] payload"
      | _ -> super#expression e
  end


let process_code_extension ~ctxt code =
  let _loc = Expansion_context.Extension.extension_point_loc ctxt in
  (* let code_code = Ppxlib.Ast_traverse.do_not_enter_let_module in *)
  let code_code = lift#expression code in
  lift_print_expr ~loc:noloc code_code


let code_extension =
  Extension.V3.declare "expr" Ppxlib.Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    process_code_extension


let () =
  Ppxlib.Driver.V2.register_transformation "code" ~extensions:[ code_extension ]

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
