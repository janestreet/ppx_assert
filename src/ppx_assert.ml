open Ppxlib
open Stdppx

let kinds =
  [ "value_or_null"; "bits8"; "bits16"; "bits32"; "bits64"; "word" ]
  |> List.map ~f:(fun jkind ->
    { Ppxlib_jane.pjka_desc =
        Pjk_abbreviation (Ast_builder.Default.Located.lident ~loc:Location.none jkind)
    ; pjka_loc = Location.none
    })
;;

let suffix_for_manual_mangling ?kind ?alloc ?mode () =
  let loc = Location.none in
  let explicitness = Ppx_template_expander.Explicitness.Drop_axis_if_all_defaults in
  let allocs =
    Option.map ~f:(fun alloc -> explicitness, [ { txt = alloc; loc } ]) alloc
  in
  let modes =
    Option.map
      ~f:(fun mode -> explicitness, [ { txt = Ppxlib_jane.Shim.Mode.Mode mode; loc } ])
      mode
  in
  match
    (Ppx_template_expander.Mangle.suffix_for_manual_mangling
    [@alert "-for_specific_ppx_uses"])
      ?allocs
      ?modes
      ~kinds:(explicitness, Option.to_list kind)
      ()
  with
  | Ok suffix -> suffix
  | Error _ -> failwith "ppx_assert internal error: failed to mangle a template suffix"
;;

let expression_name name ~stackify ~kind =
  let alloc = if stackify then "stack" else "heap" in
  name ^ suffix_for_manual_mangling ~kind ~alloc ()
;;

let type_name name ~stackify ~kind =
  let mode = if stackify then "local" else "global" in
  name ^ suffix_for_manual_mangling ~kind ~mode ()
;;

let runtime_expression ~loc name ~stackify ~kind =
  Ast_builder.Default.pexp_ident
    ~loc
    (Ast_builder.Default.Located.lident
       ~loc
       ("Ppx_assert_lib.Runtime." ^ expression_name name ~stackify ~kind))
;;

let expand_test_pred ~loc:_ ~path:_ ~stackify ~kind typ =
  let loc = { typ.ptyp_loc with loc_ghost = true } in
  [%expr
    fun ?(here = []) ?message predicate t ->
      let pos = [%e Ppx_here_expander.lift_position_as_string ~loc] in
      let sexpifier = [%e Ppx_sexp_conv_expander.Sexp_of.core_type typ ~stackify] in
      [%e runtime_expression ~loc "test_pred" ~stackify ~kind]
        ~pos
        ~sexpifier
        ~here
        ?message
        predicate
        t]
;;

let expand_test_eq ~loc:_ ~path:_ ~stackify ~kind typ =
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
      [%e runtime_expression ~loc "test_eq" ~stackify ~kind]
        ~pos
        ~sexpifier
        ~comparator
        ~here
        ?message
        ?equal
        t1
        t2]
;;

let expand_test_result ~loc:_ ~path:_ ~stackify ~kind typ =
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
      [%e runtime_expression ~loc "test_result" ~stackify ~kind]
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
  let declare name expand ~stackify ~kind =
    [ Extension.declare
        (expression_name name ~stackify ~kind)
        Extension.Context.expression
        Ast_pattern.(ptyp __)
        (fun ~loc ~path typ -> expand ~loc ~path ~stackify ~kind typ)
    ; (* types are templated over mode, not alloc *)
      (let name = type_name name ~stackify ~kind in
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
  let ( let* ) l f = List.concat_map l ~f in
  let* stackify = [ true; false ] in
  let* kind = kinds in
  let* name, expand =
    [ "test_pred", expand_test_pred
    ; "test_eq", expand_test_eq
    ; "test_result", expand_test_result
    ]
  in
  declare name expand ~stackify ~kind
;;

let () = Driver.register_transformation "assert" ~extensions
