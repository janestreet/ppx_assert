open Ppxlib

let expand_test_pred ~loc:_ ~path:_ ~stackify typ =
  let loc = { typ.ptyp_loc with loc_ghost = true } in
  [%expr
    fun ?(here = []) ?message predicate t ->
      let pos = [%e Ppx_here_expander.lift_position_as_string ~loc] in
      let sexpifier = [%e Ppx_sexp_conv_expander.Sexp_of.core_type typ ~stackify] in
      [%e
        if stackify
        then [%expr Ppx_assert_lib.Runtime.test_pred__stack]
        else [%expr Ppx_assert_lib.Runtime.test_pred]]
        ~pos
        ~sexpifier
        ~here
        ?message
        predicate
        t]
;;

let expand_test_eq ~loc:_ ~path:_ ~stackify typ =
  let loc = { typ.ptyp_loc with loc_ghost = true } in
  [%expr
    fun ?(here = []) ?message ?equal t1 t2 ->
      let pos = [%e Ppx_here_expander.lift_position_as_string ~loc] in
      let sexpifier = [%e Ppx_sexp_conv_expander.Sexp_of.core_type typ ~stackify] in
      let comparator =
        [%e
          Merlin_helpers.hide_expression
            (Ppx_compare_expander.Compare.core_type ~with_local:stackify typ)]
      in
      [%e
        if stackify
        then [%expr Ppx_assert_lib.Runtime.test_eq__stack]
        else [%expr Ppx_assert_lib.Runtime.test_eq]]
        ~pos
        ~sexpifier
        ~comparator
        ~here
        ?message
        ?equal
        t1
        t2]
;;

let expand_test_result ~loc:_ ~path:_ ~stackify typ =
  let loc = { typ.ptyp_loc with loc_ghost = true } in
  [%expr
    fun ?(here = []) ?message ?equal ~expect got ->
      let pos = [%e Ppx_here_expander.lift_position_as_string ~loc] in
      let sexpifier = [%e Ppx_sexp_conv_expander.Sexp_of.core_type typ ~stackify] in
      let comparator =
        [%e
          Merlin_helpers.hide_expression
            (Ppx_compare_expander.Compare.core_type ~with_local:stackify typ)]
      in
      [%e
        if stackify
        then [%expr Ppx_assert_lib.Runtime.test_result__stack]
        else [%expr Ppx_assert_lib.Runtime.test_result]]
        ~pos
        ~sexpifier
        ~comparator
        ~here
        ?message
        ?equal
        ~expect
        ~got]
;;

let extensions =
  let declare name expand ~stackify =
    [ Extension.declare
        (if stackify then name ^ "__stack" else name)
        Extension.Context.expression
        Ast_pattern.(ptyp __)
        (fun ~loc ~path typ -> expand ~loc ~path ~stackify typ)
    ; (* types are templated over mode, not alloc *)
      (let name = if stackify then name ^ "__local" else name in
       Extension.declare
         name
         Extension.Context.core_type
         Ast_pattern.(ptyp __)
         (fun ~loc ~path:_ ty ->
           let loc = { loc with loc_ghost = true } in
           let open Ast_builder.Default in
           let ident = Located.lident ~loc ("Ppx_assert_lib.Runtime." ^ name) in
           ptyp_constr ~loc ident [ ty ]))
    ]
  in
  [ true; false ]
  |> List.concat_map (fun stackify ->
    [ declare "test_pred" expand_test_pred ~stackify
    ; declare "test_eq" expand_test_eq ~stackify
    ; declare "test_result" expand_test_result ~stackify
    ])
  |> List.concat
;;

let () = Driver.register_transformation "assert" ~extensions
