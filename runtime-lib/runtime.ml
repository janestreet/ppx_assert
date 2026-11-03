open Sexplib0
open Sexp_conv

type 'a test_pred =
  ?here:Lexing.position list -> ?message:string -> ('a -> bool) -> 'a -> unit

type 'a test_eq =
  ?here:Lexing.position list
  -> ?message:string
  -> ?equal:('a -> 'a -> bool)
  -> 'a
  -> 'a
  -> unit

type 'a test_result =
  ?here:Lexing.position list
  -> ?message:string
  -> ?equal:('a -> 'a -> bool)
  -> expect:'a
  -> 'a
  -> unit

let globalize_sexp sexp = sexp

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

let[@cold] exn_test_pred (type t) ~message ~pos ~here ~sexpifier (t : t) =
  exn_sexp_style
    ~message
    ~pos
    ~here
    ~tag:"predicate failed"
    [ Sexp.List [ Sexp.Atom "Value"; globalize_sexp (sexpifier t) ] ]
;;

let test_pred (type t) ~pos ~sexpifier ~here ?message predicate (t : t) =
  if not (predicate t) then raise (exn_test_pred ~message ~pos ~here ~sexpifier t)
;;

let[@cold] test_result_or_eq_failed (type t) ~sexpifier ~(expect : t) ~(got : t) =
  let got = globalize_sexp (sexpifier got) in
  let expect = globalize_sexp (sexpifier expect) in
  (match Basement.Dynamic.get r_diff with
   | None -> ()
   | Some diff ->
     let from_ = Sexp.to_string_hum expect in
     let to_ = Sexp.to_string_hum got in
     diff ~from_ ~to_);
  `Fail (expect, got)
;;

let test_result_or_eq (type t) ~sexpifier ~comparator ~equal ~(expect : t) ~(got : t) =
  let pass =
    match equal with
    | None -> comparator got expect = 0
    | Some f -> f got expect
  in
  if pass then `Pass else test_result_or_eq_failed ~sexpifier ~expect ~got
;;

let test_eq (type t) ~pos ~sexpifier ~comparator ~here ?message ?equal (t1 : t) (t2 : t) =
  match test_result_or_eq ~sexpifier ~comparator ~equal ~expect:t1 ~got:t2 with
  | `Pass -> ()
  | `Fail (t1, t2) -> raise (exn_test_eq ~message ~pos ~here ~t1 ~t2)
;;

let[@warning "-16"] test_result
  (type t)
  ~pos
  ~sexpifier
  ~comparator
  ~here
  ?message
  ?equal
  ~(expect : t)
  ~(got : t)
  =
  match test_result_or_eq ~sexpifier ~comparator ~equal ~expect ~got with
  | `Pass -> ()
  | `Fail (expect, got) -> raise (exn_test_result ~message ~pos ~here ~expect ~got)
;;
