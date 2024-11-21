;;let module Staged_1055449391 =
    struct
      let modcontext''_ = Ppx_stage.Internal.empty_modcontext
      let staged1 =
        {
          Ppx_stage.compute = (fun env'' -> 1);
          Ppx_stage.source =
            (fun ren'' ->
               fun modst'' ->
                 Ppx_stage.Internal.substitute_holes
                   (Ppx_stage.Internal.rename_modules_in_exp modst''
                      modcontext''_
                      (Marshal.from_string
                         "\132\149\166\190\000\000\000\030\000\000\000\b\000\000\000\030\000\000\000\029\192\145\160!1@\176\192)./gen3.mlA@O\192\004\002A@P@@@"
                         0)) (function | _ -> assert false))
        }
      let staged2 =
        {
          Ppx_stage.compute = (fun env'' -> 2);
          Ppx_stage.source =
            (fun ren'' ->
               fun modst'' ->
                 Ppx_stage.Internal.substitute_holes
                   (Ppx_stage.Internal.rename_modules_in_exp modst''
                      modcontext''_
                      (Marshal.from_string
                         "\132\149\166\190\000\000\000\030\000\000\000\b\000\000\000\030\000\000\000\029\192\145\160!2@\176\192)./gen3.mlBUd\192\004\002BUe@@@"
                         0)) (function | _ -> assert false))
        }
      let staged3 hole''_1 hole''_2 =
        let z''b1 = Ppx_stage.Internal.fresh_variable "z" in
        let contents''_1 =
          hole''_1
            {
              Ppx_stage.compute =
                (fun env -> Ppx_stage.Internal.compute_variable z''b1 env);
              Ppx_stage.source =
                (fun ren ->
                   fun modst -> Ppx_stage.Internal.source_variable z''b1 ren)
            } in
        let contents''_2 =
          hole''_2
            {
              Ppx_stage.compute =
                (fun env -> Ppx_stage.Internal.compute_variable z''b1 env);
              Ppx_stage.source =
                (fun ren ->
                   fun modst -> Ppx_stage.Internal.source_variable z''b1 ren)
            } in
        {
          Ppx_stage.compute =
            (fun env'' ->
               let z = 200 in
               [contents''_1.Ppx_stage.compute
                  (Ppx_stage.Internal.Environ.add env'' z''b1 z);
               100 +
                 (contents''_2.Ppx_stage.compute
                    (Ppx_stage.Internal.Environ.add env'' z''b1 z));
               z]);
          Ppx_stage.source =
            (fun ren'' ->
               fun modst'' ->
                 Ppx_stage.Internal.substitute_holes
                   (Ppx_stage.Internal.rename_modules_in_exp modst''
                      modcontext''_
                      (Marshal.from_string
                         "\132\149\166\190\000\000\002V\000\000\000\154\000\000\002V\000\000\002U\192\178@\160\192\192\144\160!z\176\192)./gen3.mlDqw\192\004\002Dqx@\176\192\004\004Dqw\192\004\005Dqx@@@\192\145\160#200@\176\192\004\011Dq{\192\004\012Dq~@@@@\176\192\004\014Dqs\192\004\015Dq~@@\192\169\160\144\"::\176\192\004\022E\000B\000F\192\004\023E\000B\000_A\144\192\152\160\192\144\160\144\",1\176\192\004\"E\000B\000H\192\004#E\000B\000I@\176\192\004%E\000B\000F\192\004&E\000B\000L@@@\160\192\169\160\144\004\024\176\192\004-E\000B\000N\192\004.E\000B\000_A\144\192\152\160\192\165\192\144\160\144!+\176\192\004;E\000B\000R\192\004<E\000B\000S@\176\192\004>E\000B\000R\192\004?E\000B\000S@@@\160\160@\192\145\160#100@\176\192\004GE\000B\000N\192\004HE\000B\000Q@@@\160\160@\192\144\160\144\",2\176\192\004QE\000B\000V\192\004RE\000B\000W@\176\192\004TE\000B\000T\192\004UE\000B\000Z@@@@\176\192\004WE\000B\000N\192\004XE\000B\000Z@@@\160\192\169\160\144\004J\176\192\004_E\000B\000\\\192\004`E\000B\000_A\144\192\152\160\192\144\160\144!z\176\192\004kE\000B\000\\\192\004lE\000B\000]@\176\192\004nE\000B\000\\\192\004oE\000B\000]@@@\160\192\169\160\144\"[]\176\192\004wE\000B\000^\192\004xE\000B\000_A@\176\192\004zE\000B\000^\192\004{E\000B\000_A@@@\176\192\004}E\000B\000\\\192\004~E\000B\000_A@@\176\192\004\128E\000B\000\\\192\004\129E\000B\000_A@@@\176\192\004\131E\000B\000N\192\004\132E\000B\000_A@@\176\192\004\134E\000B\000N\192\004\135E\000B\000_A@@@\176\192\004\137E\000B\000F\192\004\138E\000B\000_A@@\176\192\004\140E\000B\000D\192\004\141E\000B\000_@@@\176\192\004\143Dqs\192\004\144E\000B\000_@@@"
                         0))
                   (function
                    | Ppx_stage.Internal.SubstHole 1 ->
                        (Ppx_stage.Internal.Renaming.with_renaming z''b1
                           contents''_1.Ppx_stage.source) ren'' modst''
                    | Ppx_stage.Internal.SubstHole 2 ->
                        (Ppx_stage.Internal.Renaming.with_renaming z''b1
                           contents''_2.Ppx_stage.source) ren'' modst''
                    | _ -> assert false))
        }
    end in
    let x = Staged_1055449391.staged1 in
    let y = Staged_1055449391.staged2 in
    (Staged_1055449391.staged3 (fun z -> x) (fun z -> y)) |>
      (Format.printf "%a@." Ppx_stage.print)
