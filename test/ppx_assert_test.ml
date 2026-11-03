open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib
open Conv

module Sexp = struct
  include Core.Sexp

  let of_string = Sexplib.Sexp.of_string
end

let () =
  (Printexc.register_printer [@ocaml.alert "-unsafe_multidomain"]) (fun exc ->
    match sexp_of_exn_opt exc with
    | None -> None
    | Some sexp -> Some (Sexp.to_string_hum ~indent:2 sexp))
;;

let re_pos = Str.regexp "[a-zA-Z0-9_/]*\\.ml:[0-9]*:[0-9]*"
let hide_position_details str = Str.global_replace re_pos "F:L:C" str

type 'a box = { a : 'a } [@@deriving sexp_of, compare]

(* Aliases for Ox_kernel's unboxed integer types, templated over kind so the tests below
   can be templated instead of duplicated per type. *)

let test_exn exn str =
  let sexp_str = Sexp.of_string str in
  let sexp_exn =
    match sexp_of_exn_opt exn with
    | None -> assert false
    | Some sexp -> Sexp.of_string (hide_position_details (Sexp.to_string sexp))
  in
  [%test_eq: Sexp.t] sexp_exn sexp_str
;;

let%test_unit _ = [%test_eq: int] 1 1

let%test _ =
  try
    [%test_eq: int * int] ~here:[ [%here] ] ~message:"int tuple" (5, 5) (5, 6);
    false
  with
  | e ->
    test_exn
      e
      "(runtime.ml.E \"int tuple: comparison failed\" ((5 5) vs (5 6) (Loc F:L:C) (Stack \
       (F:L:C))))";
    true
;;

let%test_unit _ = [%test_result: int] (1 + 2) ~message:"size" ~expect:3

let%test _ =
  try
    [%test_result: int * int] ~here:[ [%here] ] (5, 5) ~expect:(5, 6);
    false
  with
  | e ->
    test_exn
      e
      "(runtime.ml.E \"got unexpected result\"((expected (5 6)) (got (5 5)) (Loc F:L:C) \
       (Stack (F:L:C))))";
    true
;;

let%test _ =
  try
    [%test_pred: float] ~message:"price" (fun x -> equal_float 3. x) 5.;
    false
  with
  | e ->
    test_exn e "(runtime.ml.E \"price: predicate failed\" ((Value 5) (Loc F:L:C)))";
    true
;;

let%test_unit _ = [%test_eq: int] ~equal:(fun x y -> x mod 2 = y mod 2) 4 6

(* An example where the list of positions that <:test_eq< ... >> takes comes in handy,
   because the position of <:test_eq< ... >> itself is not very informative. *)
let test_is_zero ~here x = [%test_eq: int] 0 x ~here:([%here] :: here)

let test_odds n ~here =
  for i = 0 to n do
    let odd = (2 * i) + 1 in
    test_is_zero ~here:([%here] :: here) (odd - odd)
  done
;;

let test_evens n ~here =
  for i = 0 to n do
    let even = 2 * i in
    test_is_zero ~here:([%here] :: here) (even - even)
  done
;;

let test_all n =
  test_odds n ~here:[ [%here] ];
  test_evens n ~here:[ [%here] ]
;;

let%test_unit _ = test_all 10
let _ = ([%test_result: int] : [%test_result: int])
let _ = ([%test_eq: int] : [%test_eq: int])
let _ = ([%test_pred: int] : [%test_pred: int])

(* example using [box], defined before the template block, at different allocs *)
let _ = [%test_eq: int box] { a = 1 + 2 } { a = 3 * 1 }
let _ = Int64_u.([%test_eq: t box] { a = 1L + 2L } { a = 3L * 1L })

(* [%test_result] etc should make portable functions *)
let%test_unit _ =
  let _test = [%test_result: int] in
  let _test = [%test_eq: int] in
  let _test = [%test_pred: int] in
  (* ensure this holds for higher-order [compare] functions *)
  let _test = [%test_result: string option] in
  let _test = [%test_eq: string option] in
  let _test = [%test_pred: string option] in
  ()
;;

let require_failed f =
  match f () with
  | () -> failwith "expected assertion to fail"
  | exception _ -> ()
;;

(* [%test_result], [%test_eq], and [%test_pred] support Ox_kernel's unboxed integer types:
   they pass on equal values and fail on unequal ones. *)

let _ = ([%test_result: i] : [%test_result: i])
let _ = ([%test_eq: i] : [%test_eq: i])
let _ = ([%test_pred: i] : [%test_pred: i])

let%test_unit _ =
  let x = i_of_int_exn 100 in
  let x' = i_of_int_exn 0 in
  [%test_result: i] x ~expect:x;
  [%test_eq: i] x x;
  [%test_pred: i] (fun y -> equal_i y x) x;
  require_failed (fun () -> [%test_result: i] x ~expect:x');
  require_failed (fun () -> [%test_eq: i] x x');
  require_failed (fun () -> [%test_pred: i] (fun y -> equal_i y x') x)
;;
