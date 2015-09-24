open Parsetree
open! Ppx_core.Std

[@@@metaloc loc]

let expand_test_pred typ =
  let loc = typ.ptyp_loc in
  let pos = loc.loc_start in
  [%expr fun ?(here= []) ?message predicate t ->
       let pos       = [%e Ppx_here_expander.lift_position ~loc pos] in
       let sexpifier = [%e Ppx_sexp_conv_expander.sexp_of typ      ] in
       Ppx_assert_lib.Runtime.test_pred
         ~pos ~sexpifier ~here ?message predicate t
  ]
;;


let expand_test_eq typ =
  let loc = typ.ptyp_loc in
  let pos = loc.loc_start in
  [%expr fun ?(here= []) ?message ?equal t1 t2 ->
       let pos        = [%e Ppx_here_expander.lift_position ~loc pos] in
       let sexpifier  = [%e Ppx_sexp_conv_expander.sexp_of typ      ] in
       let comparator = [%e Ppx_compare_expander.compare typ        ] in
       Ppx_assert_lib.Runtime.test_eq
         ~pos ~sexpifier ~comparator ~here ?message ?equal t1 t2
  ]
;;

let expand_test_result typ =
  let loc = typ.ptyp_loc in
  let pos = loc.loc_start in
  [%expr fun ?(here= []) ?message ?equal ~expect got ->
       let pos        = [%e Ppx_here_expander.lift_position ~loc pos] in
       let sexpifier  = [%e Ppx_sexp_conv_expander.sexp_of typ      ] in
       let comparator = [%e Ppx_compare_expander.compare typ        ] in
       Ppx_assert_lib.Runtime.test_result
         ~pos ~sexpifier ~comparator ~here ?message ?equal ~expect ~got
  ]
;;

let exts =
  let declare name expand =
    Extension.declare name Extension.Context.expression Ast_pattern.(ptyp __) expand
  in
  [ declare "test_pred"   expand_test_pred
  ; declare "test_eq"     expand_test_eq
  ; declare "test_result" expand_test_result
  ]
;;

let map = object
  inherit Ast_traverse.map as super

  method! expression e =
    let e = super#expression e in
    match e.pexp_desc with
    | Pexp_extension ext ->
      (match Extension.convert exts ext with
       | None   -> e
       | Some e -> e)
    | _ -> e
end

let () =
  Ppx_driver.register_code_transformation
    ~name:"test"
    ~impl:map#structure
    ~intf:map#signature
;;
