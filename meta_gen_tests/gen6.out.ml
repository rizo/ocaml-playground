;;let const_val = 1 in
  let computed_val = 2 + 2 in
  Format.printf "%a@." Ppxlib.Pprintast.expression
    {
      pexp_desc =
        (Pexp_let
           (Nonrecursive,
             [{
                pvb_pat =
                  {
                    ppat_desc =
                      (Ppat_var
                         {
                           txt = "delayed_val";
                           loc =
                             {
                               loc_start =
                                 {
                                   pos_fname = "./gen6.ml";
                                   pos_lnum = 4;
                                   pos_bol = 56;
                                   pos_cnum = 62
                                 };
                               loc_end =
                                 {
                                   pos_fname = "./gen6.ml";
                                   pos_lnum = 4;
                                   pos_bol = 56;
                                   pos_cnum = 73
                                 };
                               loc_ghost = false
                             }
                         });
                    ppat_loc =
                      {
                        loc_start =
                          {
                            pos_fname = "./gen6.ml";
                            pos_lnum = 4;
                            pos_bol = 56;
                            pos_cnum = 62
                          };
                        loc_end =
                          {
                            pos_fname = "./gen6.ml";
                            pos_lnum = 4;
                            pos_bol = 56;
                            pos_cnum = 73
                          };
                        loc_ghost = false
                      };
                    ppat_loc_stack = [];
                    ppat_attributes = []
                  };
                pvb_expr =
                  {
                    pexp_desc =
                      (Pexp_constant (Pconst_integer ("200", None)));
                    pexp_loc =
                      {
                        loc_start =
                          {
                            pos_fname = "./gen6.ml";
                            pos_lnum = 4;
                            pos_bol = 56;
                            pos_cnum = 76
                          };
                        loc_end =
                          {
                            pos_fname = "./gen6.ml";
                            pos_lnum = 4;
                            pos_bol = 56;
                            pos_cnum = 79
                          };
                        loc_ghost = false
                      };
                    pexp_loc_stack = [];
                    pexp_attributes = []
                  };
                pvb_attributes = [];
                pvb_loc =
                  {
                    loc_start =
                      {
                        pos_fname = "./gen6.ml";
                        pos_lnum = 4;
                        pos_bol = 56;
                        pos_cnum = 58
                      };
                    loc_end =
                      {
                        pos_fname = "./gen6.ml";
                        pos_lnum = 4;
                        pos_bol = 56;
                        pos_cnum = 79
                      };
                    loc_ghost = false
                  }
              }],
             {
               pexp_desc =
                 (Pexp_construct
                    ({
                       txt = (Lident "::");
                       loc =
                         {
                           loc_start =
                             {
                               pos_fname = "./gen6.ml";
                               pos_lnum = 5;
                               pos_bol = 83;
                               pos_cnum = 87
                             };
                           loc_end =
                             {
                               pos_fname = "./gen6.ml";
                               pos_lnum = 5;
                               pos_bol = 83;
                               pos_cnum = 145
                             };
                           loc_ghost = true
                         }
                     },
                      (Some
                         {
                           pexp_desc =
                             (Pexp_tuple
                                [{
                                   pexp_desc =
                                     (Pexp_apply
                                        ({
                                           pexp_desc =
                                             (Pexp_ident
                                                {
                                                  txt = (Lident "+");
                                                  loc =
                                                    {
                                                      loc_start =
                                                        {
                                                          pos_fname =
                                                            "./gen6.ml";
                                                          pos_lnum = 5;
                                                          pos_bol = 83;
                                                          pos_cnum = 91
                                                        };
                                                      loc_end =
                                                        {
                                                          pos_fname =
                                                            "./gen6.ml";
                                                          pos_lnum = 5;
                                                          pos_bol = 83;
                                                          pos_cnum = 92
                                                        };
                                                      loc_ghost = false
                                                    }
                                                });
                                           pexp_loc =
                                             {
                                               loc_start =
                                                 {
                                                   pos_fname = "./gen6.ml";
                                                   pos_lnum = 5;
                                                   pos_bol = 83;
                                                   pos_cnum = 91
                                                 };
                                               loc_end =
                                                 {
                                                   pos_fname = "./gen6.ml";
                                                   pos_lnum = 5;
                                                   pos_bol = 83;
                                                   pos_cnum = 92
                                                 };
                                               loc_ghost = false
                                             };
                                           pexp_loc_stack = [];
                                           pexp_attributes = []
                                         },
                                          [(Nolabel,
                                             {
                                               pexp_desc =
                                                 (Pexp_constant
                                                    (Pconst_integer
                                                       ("100", None)));
                                               pexp_loc =
                                                 {
                                                   loc_start =
                                                     {
                                                       pos_fname =
                                                         "./gen6.ml";
                                                       pos_lnum = 5;
                                                       pos_bol = 83;
                                                       pos_cnum = 87
                                                     };
                                                   loc_end =
                                                     {
                                                       pos_fname =
                                                         "./gen6.ml";
                                                       pos_lnum = 5;
                                                       pos_bol = 83;
                                                       pos_cnum = 90
                                                     };
                                                   loc_ghost = false
                                                 };
                                               pexp_loc_stack = [];
                                               pexp_attributes = []
                                             });
                                          (Nolabel,
                                            (Ppxlib.Ast_builder.Default.eint
                                               ~loc:Ppxlib.Location.none
                                               const_val))]));
                                   pexp_loc =
                                     {
                                       loc_start =
                                         {
                                           pos_fname = "./gen6.ml";
                                           pos_lnum = 5;
                                           pos_bol = 83;
                                           pos_cnum = 87
                                         };
                                       loc_end =
                                         {
                                           pos_fname = "./gen6.ml";
                                           pos_lnum = 5;
                                           pos_bol = 83;
                                           pos_cnum = 109
                                         };
                                       loc_ghost = false
                                     };
                                   pexp_loc_stack = [];
                                   pexp_attributes = []
                                 };
                                {
                                  pexp_desc =
                                    (Pexp_construct
                                       ({
                                          txt = (Lident "::");
                                          loc =
                                            {
                                              loc_start =
                                                {
                                                  pos_fname = "./gen6.ml";
                                                  pos_lnum = 5;
                                                  pos_bol = 83;
                                                  pos_cnum = 111
                                                };
                                              loc_end =
                                                {
                                                  pos_fname = "./gen6.ml";
                                                  pos_lnum = 5;
                                                  pos_bol = 83;
                                                  pos_cnum = 145
                                                };
                                              loc_ghost = true
                                            }
                                        },
                                         (Some
                                            {
                                              pexp_desc =
                                                (Pexp_tuple
                                                   [Ppxlib.Ast_builder.Default.eint
                                                      ~loc:Ppxlib.Location.none
                                                      computed_val;
                                                   {
                                                     pexp_desc =
                                                       (Pexp_construct
                                                          ({
                                                             txt =
                                                               (Lident "::");
                                                             loc =
                                                               {
                                                                 loc_start =
                                                                   {
                                                                    pos_fname
                                                                    =
                                                                    "./gen6.ml";
                                                                    pos_lnum
                                                                    = 5;
                                                                    pos_bol =
                                                                    83;
                                                                    pos_cnum
                                                                    = 132
                                                                   };
                                                                 loc_end =
                                                                   {
                                                                    pos_fname
                                                                    =
                                                                    "./gen6.ml";
                                                                    pos_lnum
                                                                    = 5;
                                                                    pos_bol =
                                                                    83;
                                                                    pos_cnum
                                                                    = 145
                                                                   };
                                                                 loc_ghost =
                                                                   true
                                                               }
                                                           },
                                                            (Some
                                                               {
                                                                 pexp_desc =
                                                                   (Pexp_tuple
                                                                    [
                                                                    {
                                                                    pexp_desc
                                                                    =
                                                                    (Pexp_ident
                                                                    {
                                                                    txt =
                                                                    (Lident
                                                                    "delayed_val");
                                                                    loc =
                                                                    {
                                                                    loc_start
                                                                    =
                                                                    {
                                                                    pos_fname
                                                                    =
                                                                    "./gen6.ml";
                                                                    pos_lnum
                                                                    = 5;
                                                                    pos_bol =
                                                                    83;
                                                                    pos_cnum
                                                                    = 132
                                                                    };
                                                                    loc_end =
                                                                    {
                                                                    pos_fname
                                                                    =
                                                                    "./gen6.ml";
                                                                    pos_lnum
                                                                    = 5;
                                                                    pos_bol =
                                                                    83;
                                                                    pos_cnum
                                                                    = 143
                                                                    };
                                                                    loc_ghost
                                                                    = false
                                                                    }
                                                                    });
                                                                    pexp_loc
                                                                    =
                                                                    {
                                                                    loc_start
                                                                    =
                                                                    {
                                                                    pos_fname
                                                                    =
                                                                    "./gen6.ml";
                                                                    pos_lnum
                                                                    = 5;
                                                                    pos_bol =
                                                                    83;
                                                                    pos_cnum
                                                                    = 132
                                                                    };
                                                                    loc_end =
                                                                    {
                                                                    pos_fname
                                                                    =
                                                                    "./gen6.ml";
                                                                    pos_lnum
                                                                    = 5;
                                                                    pos_bol =
                                                                    83;
                                                                    pos_cnum
                                                                    = 143
                                                                    };
                                                                    loc_ghost
                                                                    = false
                                                                    };
                                                                    pexp_loc_stack
                                                                    = [];
                                                                    pexp_attributes
                                                                    = []
                                                                    };
                                                                    {
                                                                    pexp_desc
                                                                    =
                                                                    (Pexp_construct
                                                                    ({
                                                                    txt =
                                                                    (Lident
                                                                    "[]");
                                                                    loc =
                                                                    {
                                                                    loc_start
                                                                    =
                                                                    {
                                                                    pos_fname
                                                                    =
                                                                    "./gen6.ml";
                                                                    pos_lnum
                                                                    = 5;
                                                                    pos_bol =
                                                                    83;
                                                                    pos_cnum
                                                                    = 144
                                                                    };
                                                                    loc_end =
                                                                    {
                                                                    pos_fname
                                                                    =
                                                                    "./gen6.ml";
                                                                    pos_lnum
                                                                    = 5;
                                                                    pos_bol =
                                                                    83;
                                                                    pos_cnum
                                                                    = 145
                                                                    };
                                                                    loc_ghost
                                                                    = true
                                                                    }
                                                                    }, None));
                                                                    pexp_loc
                                                                    =
                                                                    {
                                                                    loc_start
                                                                    =
                                                                    {
                                                                    pos_fname
                                                                    =
                                                                    "./gen6.ml";
                                                                    pos_lnum
                                                                    = 5;
                                                                    pos_bol =
                                                                    83;
                                                                    pos_cnum
                                                                    = 144
                                                                    };
                                                                    loc_end =
                                                                    {
                                                                    pos_fname
                                                                    =
                                                                    "./gen6.ml";
                                                                    pos_lnum
                                                                    = 5;
                                                                    pos_bol =
                                                                    83;
                                                                    pos_cnum
                                                                    = 145
                                                                    };
                                                                    loc_ghost
                                                                    = true
                                                                    };
                                                                    pexp_loc_stack
                                                                    = [];
                                                                    pexp_attributes
                                                                    = []
                                                                    }]);
                                                                 pexp_loc =
                                                                   {
                                                                    loc_start
                                                                    =
                                                                    {
                                                                    pos_fname
                                                                    =
                                                                    "./gen6.ml";
                                                                    pos_lnum
                                                                    = 5;
                                                                    pos_bol =
                                                                    83;
                                                                    pos_cnum
                                                                    = 132
                                                                    };
                                                                    loc_end =
                                                                    {
                                                                    pos_fname
                                                                    =
                                                                    "./gen6.ml";
                                                                    pos_lnum
                                                                    = 5;
                                                                    pos_bol =
                                                                    83;
                                                                    pos_cnum
                                                                    = 145
                                                                    };
                                                                    loc_ghost
                                                                    = true
                                                                   };
                                                                 pexp_loc_stack
                                                                   = [];
                                                                 pexp_attributes
                                                                   = []
                                                               })));
                                                     pexp_loc =
                                                       {
                                                         loc_start =
                                                           {
                                                             pos_fname =
                                                               "./gen6.ml";
                                                             pos_lnum = 5;
                                                             pos_bol = 83;
                                                             pos_cnum = 132
                                                           };
                                                         loc_end =
                                                           {
                                                             pos_fname =
                                                               "./gen6.ml";
                                                             pos_lnum = 5;
                                                             pos_bol = 83;
                                                             pos_cnum = 145
                                                           };
                                                         loc_ghost = true
                                                       };
                                                     pexp_loc_stack = [];
                                                     pexp_attributes = []
                                                   }]);
                                              pexp_loc =
                                                {
                                                  loc_start =
                                                    {
                                                      pos_fname = "./gen6.ml";
                                                      pos_lnum = 5;
                                                      pos_bol = 83;
                                                      pos_cnum = 111
                                                    };
                                                  loc_end =
                                                    {
                                                      pos_fname = "./gen6.ml";
                                                      pos_lnum = 5;
                                                      pos_bol = 83;
                                                      pos_cnum = 145
                                                    };
                                                  loc_ghost = true
                                                };
                                              pexp_loc_stack = [];
                                              pexp_attributes = []
                                            })));
                                  pexp_loc =
                                    {
                                      loc_start =
                                        {
                                          pos_fname = "./gen6.ml";
                                          pos_lnum = 5;
                                          pos_bol = 83;
                                          pos_cnum = 111
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "./gen6.ml";
                                          pos_lnum = 5;
                                          pos_bol = 83;
                                          pos_cnum = 145
                                        };
                                      loc_ghost = true
                                    };
                                  pexp_loc_stack = [];
                                  pexp_attributes = []
                                }]);
                           pexp_loc =
                             {
                               loc_start =
                                 {
                                   pos_fname = "./gen6.ml";
                                   pos_lnum = 5;
                                   pos_bol = 83;
                                   pos_cnum = 87
                                 };
                               loc_end =
                                 {
                                   pos_fname = "./gen6.ml";
                                   pos_lnum = 5;
                                   pos_bol = 83;
                                   pos_cnum = 145
                                 };
                               loc_ghost = true
                             };
                           pexp_loc_stack = [];
                           pexp_attributes = []
                         })));
               pexp_loc =
                 {
                   loc_start =
                     {
                       pos_fname = "./gen6.ml";
                       pos_lnum = 5;
                       pos_bol = 83;
                       pos_cnum = 85
                     };
                   loc_end =
                     {
                       pos_fname = "./gen6.ml";
                       pos_lnum = 5;
                       pos_bol = 83;
                       pos_cnum = 145
                     };
                   loc_ghost = false
                 };
               pexp_loc_stack = [];
               pexp_attributes = []
             }));
      pexp_loc =
        {
          loc_start =
            {
              pos_fname = "./gen6.ml";
              pos_lnum = 4;
              pos_bol = 56;
              pos_cnum = 58
            };
          loc_end =
            {
              pos_fname = "./gen6.ml";
              pos_lnum = 5;
              pos_bol = 83;
              pos_cnum = 145
            };
          loc_ghost = false
        };
      pexp_loc_stack = [];
      pexp_attributes = []
    }
