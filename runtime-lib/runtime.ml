open Sexplib
open Sexplib.Conv

exception E of string * Sexp.t [@@deriving sexp]

(* This is the same function as Ppx_here.lift_position_as_string. *)
let make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol =
  String.concat ""
    [ pos_fname
    ; ":"; string_of_int pos_lnum
    ; ":"; string_of_int (pos_cnum - pos_bol)
    ]

let string_of_loc {Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol} =
  make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol

let sexp_of_loc t =
  Sexp.Atom (string_of_loc t)

let failwith message sexp = raise (E (message, sexp))

let fail_in_sexp_style ~message ~pos ~here ~tag body =
  let message =
    match message with
    | None -> tag
    | Some s -> s ^ ": " ^ tag
  in
  let sexp =
    Sexp.List (
      body
      @ [ Sexp.List [ Sexp.Atom "Loc"; Sexp.Atom pos ] ]
      @ begin match here with
        | [] -> []
        | _ -> [ Sexp.List [ Sexp.Atom "Stack"; sexp_of_list sexp_of_loc here ] ]
        end
    )
  in
  failwith message sexp

let test_pred ~pos ~sexpifier ~here ?message predicate t =
  if not (predicate t) then begin
    fail_in_sexp_style ~message ~pos ~here ~tag:"predicate failed" [
      Sexp.List [Sexp.Atom "Value"; sexpifier t]
    ]
  end

let r_diff : (from_:string -> to_:string -> unit) option ref = ref None
let set_diff_function f = r_diff := f

let test_result_or_eq ~sexpifier ~comparator ?equal ~expect ~got ~fail =
  let pass =
    match equal with
    | None -> comparator got expect = 0
    | Some f -> f got expect
  in
  if not pass then begin
    let got = sexpifier got in
    let expect = sexpifier expect in
    begin match !r_diff with
    | None -> ()
    | Some diff ->
      let from_ = Sexp.to_string_hum expect in
      let to_ = Sexp.to_string_hum got in
      diff ~from_ ~to_
    end;
    fail ~expect ~got
  end

let test_eq ~pos ~sexpifier ~comparator ~here ?message ?equal t1 t2 =
  test_result_or_eq ~sexpifier ~comparator ?equal
    ~expect:t1 ~got:t2 ~fail:(fun ~expect:t1 ~got:t2 ->
    fail_in_sexp_style ~message ~pos ~here ~tag:"comparison failed" [
      t1;
      Sexp.Atom "vs";
      t2;
    ])

let test_result ~pos ~sexpifier ~comparator ~here ?message ?equal ~expect ~got =
  test_result_or_eq ~sexpifier ~comparator ?equal ~expect ~got ~fail:(fun ~expect ~got ->
    fail_in_sexp_style ~message ~pos ~here ~tag:"got unexpected result" [
      Sexp.List [Sexp.Atom "expected"; expect];
      Sexp.List [Sexp.Atom "got"; got];
    ])
