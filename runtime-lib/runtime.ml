open Sexplib0
open Sexp_conv

[%%template
[@@@mode.default m = (local, global)]

type ('a : k) test_pred =
  ?here:Lexing.position list
  -> ?message:string
  -> ('a @ m -> bool) @ local
  -> 'a @ m
  -> unit
[@@kind k = (value_or_null, bits8, bits16, bits32, bits64, word)]

type ('a : k) test_eq =
  ?here:Lexing.position list
  -> ?message:string
  -> ?equal:('a @ m -> 'a @ m -> bool) @ local
  -> 'a @ m
  -> 'a @ m
  -> unit
[@@kind k = (value_or_null, bits8, bits16, bits32, bits64, word)]

type ('a : k) test_result =
  ?here:Lexing.position list
  -> ?message:string
  -> ?equal:('a @ m -> 'a @ m -> bool) @ local
  -> expect:'a @ m
  -> 'a @ m
  -> unit
[@@kind k = (value_or_null, bits8, bits16, bits32, bits64, word)]]

let%template[@alloc heap] globalize_sexp sexp = sexp
let%template[@alloc stack] globalize_sexp sexp = Sexp.globalize sexp

exception E of string * Sexp.t [@@deriving sexp]

(* Same format as [Base.Source_code_position.sexp_of_t]. *)
let sexp_of_position ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) =
  Sexp.Atom (Printf.sprintf "%s:%d:%d" pos_fname pos_lnum (pos_cnum - pos_bol))
;;

let exn_sexp_style ~message ~pos ~here ~tag body =
  let message =
    match message with
    | None -> tag
    | Some s -> s ^ ": " ^ tag
  in
  let sexp =
    Sexp.List
      (body
       @ [ Sexp.List [ Sexp.Atom "Loc"; Sexp.Atom pos ] ]
       @
       match here with
       | [] -> []
       | _ -> [ Sexp.List [ Sexp.Atom "Stack"; sexp_of_list sexp_of_position here ] ])
  in
  (* Here and in other places we return exceptions, rather than directly raising, and
     instead raise at the latest moment possible, so backtrace don't include noise from
     these functions that construct exceptions. *)
  E (message, sexp)
;;

let[@cold] exn_test_eq ~message ~pos ~here ~t1 ~t2 =
  exn_sexp_style ~message ~pos ~here ~tag:"comparison failed" [ t1; Sexp.Atom "vs"; t2 ]
;;

let[@cold] exn_test_result ~message ~pos ~here ~expect ~got =
  exn_sexp_style
    ~message
    ~pos
    ~here
    ~tag:"got unexpected result"
    [ Sexp.List [ Sexp.Atom "expected"; expect ]; Sexp.List [ Sexp.Atom "got"; got ] ]
;;

let r_diff = Basement.Dynamic.make (None : (from_:string -> to_:string -> unit) option)
let set_diff_function f = Basement.Dynamic.set_root r_diff f

[%%template
[@@@alloc.default a @ m = (stack_local, heap_global)]

let[@cold] exn_test_pred (type t : k) ~message ~pos ~here ~sexpifier (t : t @ m) =
  exn_sexp_style
    ~message
    ~pos
    ~here
    ~tag:"predicate failed"
    [ Sexp.List [ Sexp.Atom "Value"; (globalize_sexp [@alloc a]) (sexpifier t) ] ]
[@@kind k = (value_or_null, bits8, bits16, bits32, bits64, word)]
;;

let test_pred (type t : k) ~pos ~sexpifier ~here ?message predicate (t : t @ m) =
  if not (predicate t)
  then raise ((exn_test_pred [@kind k] [@alloc a]) ~message ~pos ~here ~sexpifier t)
[@@kind k = (value_or_null, bits8, bits16, bits32, bits64, word)]
;;

let[@cold] test_result_or_eq_failed
  (type t : k)
  ~sexpifier
  ~(expect : t @ m)
  ~(got : t @ m)
  =
  let got = (globalize_sexp [@alloc a]) (sexpifier got) in
  let expect = (globalize_sexp [@alloc a]) (sexpifier expect) in
  (match Basement.Dynamic.get r_diff with
   | None -> ()
   | Some diff ->
     let from_ = Sexp.to_string_hum expect in
     let to_ = Sexp.to_string_hum got in
     diff ~from_ ~to_);
  `Fail (expect, got)
[@@kind k = (value_or_null, bits8, bits16, bits32, bits64, word)]
;;

let test_result_or_eq
  (type t : k)
  ~sexpifier
  ~comparator
  ~equal
  ~(expect : t @ m)
  ~(got : t @ m)
  =
  let pass =
    match equal with
    | None -> comparator got expect = 0
    | Some f -> f got expect
  in
  if pass
  then `Pass
  else (test_result_or_eq_failed [@kind k] [@alloc a]) ~sexpifier ~expect ~got
[@@kind k = (value_or_null, bits8, bits16, bits32, bits64, word)]
;;

let test_eq
  (type t : k)
  ~pos
  ~sexpifier
  ~comparator
  ~here
  ?message
  ?equal
  (t1 : t @ m)
  (t2 : t @ m)
  =
  match
    (test_result_or_eq [@kind k] [@alloc a])
      ~sexpifier
      ~comparator
      ~equal
      ~expect:t1
      ~got:t2
  with
  | `Pass -> ()
  | `Fail (t1, t2) -> raise (exn_test_eq ~message ~pos ~here ~t1 ~t2)
[@@kind k = (value_or_null, bits8, bits16, bits32, bits64, word)]
;;

let[@warning "-16"] test_result
  (type t : k)
  ~pos
  ~sexpifier
  ~comparator
  ~here
  ?message
  ?equal
  ~(expect : t @ m)
  ~(got : t @ m)
  =
  match
    (test_result_or_eq [@kind k] [@alloc a]) ~sexpifier ~comparator ~equal ~expect ~got
  with
  | `Pass -> ()
  | `Fail (expect, got) -> raise (exn_test_result ~message ~pos ~here ~expect ~got)
[@@kind k = (value_or_null, bits8, bits16, bits32, bits64, word)]
;;]
