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

type%template ('a : k) box = { a : 'a }
[@@deriving sexp_of ~stackify, compare ~localize] [@@kind k = (bits64, value)]

(* Aliases for Ox_kernel's unboxed integer types, templated over kind so the tests below
   can be templated instead of duplicated per type. *)
[%%template
type i = Ox_kernel.I8.t
[@@deriving compare ~localize, equal ~localize, sexp_of ~stackify] [@@kind bits8]

type i = Ox_kernel.I16.t
[@@deriving compare ~localize, equal ~localize, sexp_of ~stackify] [@@kind bits16]

type i = Ox_kernel.I32.t
[@@deriving compare ~localize, equal ~localize, sexp_of ~stackify] [@@kind bits32]

type i = Ox_kernel.I64.t
[@@deriving compare ~localize, equal ~localize, sexp_of ~stackify] [@@kind bits64]

type i = Ox_kernel.Isize.t
[@@deriving compare ~localize, equal ~localize, sexp_of ~stackify] [@@kind word]

let i_of_int_exn = Ox_kernel.I8.of_int_exn [@@kind bits8]
let i_of_int_exn = Ox_kernel.I16.of_int_exn [@@kind bits16]
let i_of_int_exn = Ox_kernel.I32.of_int_exn [@@kind bits32]
let i_of_int_exn = Ox_kernel.I64.of_int_exn [@@kind bits64]
let i_of_int_exn = Ox_kernel.Isize.of_int_exn [@@kind word]]

[%%template
[@@@alloc.default a @ m = (stack_local, heap_global)]

let test_exn exn str =
  let sexp_str = Sexp.of_string str in
  let sexp_exn =
    match sexp_of_exn_opt exn with
    | None -> assert false
    | Some sexp -> Sexp.of_string (hide_position_details (Sexp.to_string sexp))
  in
  ([%test_eq: Sexp.t] [@alloc a]) sexp_exn sexp_str
;;

let%test_unit _ = ([%test_eq: int] [@alloc a]) 1 1

let%test _ =
  try
    ([%test_eq: int * int] [@alloc a]) ~here:[ [%here] ] ~message:"int tuple" (5, 5) (5, 6);
    false
  with
  | e ->
    (test_exn [@alloc a])
      e
      "(runtime.ml.E \"int tuple: comparison failed\" ((5 5) vs (5 6) (Loc F:L:C) (Stack \
       (F:L:C))))";
    true
;;

let%test_unit _ = ([%test_result: int] [@alloc a]) (1 + 2) ~message:"size" ~expect:3

let%test _ =
  try
    ([%test_result: int * int] [@alloc a]) ~here:[ [%here] ] (5, 5) ~expect:(5, 6);
    false
  with
  | e ->
    (test_exn [@alloc a])
      e
      "(runtime.ml.E \"got unexpected result\"((expected (5 6)) (got (5 5)) (Loc F:L:C) \
       (Stack (F:L:C))))";
    true
;;

let%test _ =
  try
    ([%test_pred: float] [@alloc a])
      ~message:"price"
      (fun x -> (equal_float [@mode m]) 3. x)
      5.;
    false
  with
  | e ->
    (test_exn [@alloc a])
      e
      "(runtime.ml.E \"price: predicate failed\" ((Value 5) (Loc F:L:C)))";
    true
;;

let%test_unit _ = ([%test_eq: int] [@alloc a]) ~equal:(fun x y -> x mod 2 = y mod 2) 4 6

(* An example where the list of positions that <:test_eq< ... >> takes comes in handy,
   because the position of <:test_eq< ... >> itself is not very informative. *)
let test_is_zero ~here x = ([%test_eq: int] [@alloc a]) 0 x ~here:([%here] :: here)

let test_odds n ~here =
  for i = 0 to n do
    let odd = (2 * i) + 1 in
    (test_is_zero [@alloc a]) ~here:([%here] :: here) (odd - odd)
  done
;;

let test_evens n ~here =
  for i = 0 to n do
    let even = 2 * i in
    (test_is_zero [@alloc a]) ~here:([%here] :: here) (even - even)
  done
;;

let test_all n =
  (test_odds [@alloc a]) n ~here:[ [%here] ];
  (test_evens [@alloc a]) n ~here:[ [%here] ]
;;

let%test_unit _ = (test_all [@alloc a]) 10
let _ = ([%test_result: int] [@alloc a] : ([%test_result: int][@mode m]))
let _ = ([%test_eq: int] [@alloc a] : ([%test_eq: int][@mode m]))
let _ = ([%test_pred: int] [@alloc a] : ([%test_pred: int][@mode m]))

(* example using [box], defined before the template block, at different allocs *)
let _ = ([%test_eq: int box] [@alloc a]) { a = 1 + 2 } { a = 3 * 1 }

let%template _ =
  Int64_u.(
    ([%test_eq: (t box[@kind bits64])] [@alloc a]) { a = #1L + #2L } { a = #3L * #1L })
;;

(* [%test_result] etc should make portable functions *)
let%test_unit _ =
  let _test @ portable = [%test_result: int] [@alloc a] in
  let _test @ portable = [%test_eq: int] [@alloc a] in
  let _test @ portable = [%test_pred: int] [@alloc a] in
  (* ensure this holds for higher-order [compare] functions *)
  let _test @ portable = [%test_result: string option] [@alloc a] in
  let _test @ portable = [%test_eq: string option] [@alloc a] in
  let _test @ portable = [%test_pred: string option] [@alloc a] in
  ()
;;]

let require_failed f =
  match f () with
  | () -> failwith "expected assertion to fail"
  | exception _ -> ()
;;

(* [%test_result], [%test_eq], and [%test_pred] support Ox_kernel's unboxed integer types:
   they pass on equal values and fail on unequal ones. *)
[%%template
[@@@alloc.default a @ l = (stack_local, heap_global)]
[@@@kind.default k = (bits8, bits16, bits32, bits64, word)]

let _ =
  ([%test_result: (i[@kind k])] [@kind k] [@alloc a]
   : ([%test_result: (i[@kind k])][@kind k] [@mode l]))
;;

let _ =
  ([%test_eq: (i[@kind k])] [@kind k] [@alloc a]
   : ([%test_eq: (i[@kind k])][@kind k] [@mode l]))
;;

let _ =
  ([%test_pred: (i[@kind k])] [@kind k] [@alloc a]
   : ([%test_pred: (i[@kind k])][@kind k] [@mode l]))
;;

let%test_unit _ =
  let x = (i_of_int_exn [@kind k]) 100 in
  let x' = (i_of_int_exn [@kind k]) 0 in
  ([%test_result: (i[@kind k])] [@kind k] [@alloc a]) x ~expect:x;
  ([%test_eq: (i[@kind k])] [@kind k] [@alloc a]) x x;
  ([%test_pred: (i[@kind k])] [@kind k] [@alloc a]) (fun y -> (equal_i [@kind k]) y x) x;
  require_failed (fun () ->
    ([%test_result: (i[@kind k])] [@kind k] [@alloc a]) x ~expect:x');
  require_failed (fun () -> ([%test_eq: (i[@kind k])] [@kind k] [@alloc a]) x x');
  require_failed (fun () ->
    ([%test_pred: (i[@kind k])] [@kind k] [@alloc a])
      (fun y -> (equal_i [@kind k]) y x')
      x)
;;]
