open Sexplib

(** Functions called by the generated code *)

val test_pred :
  pos:string ->
  sexpifier:('a -> Sexp.t) ->
  here:Lexing.position list ->
  ?message:string ->
  ('a -> bool) ->
  'a ->
  unit

val test_eq :
  pos:string ->
  sexpifier:('a -> Sexp.t) ->
  comparator:('a -> 'a -> int) ->
  here:Lexing.position list ->
  ?message:string ->
  ?equal:('a -> 'a -> bool) ->
  'a ->
  'a ->
  unit

val test_result :
  pos:string ->
  sexpifier:('a -> Sexp.t) ->
  comparator:('a -> 'a -> int) ->
  here:Lexing.position list ->
  ?message:string ->
  ?equal:('a -> 'a -> bool) ->
  expect:'a ->
  got:'a ->
  unit

(** Called to set/unset the [diff] function, used by [test_result] *)
val set_diff_function : (from_:string -> to_:string -> unit) option -> unit

(** [string_of_loc] and [sexp_of_loc] are exposed to be rebound in core_kernel. *)
val string_of_loc : Lexing.position -> string
val sexp_of_loc : Lexing.position -> Sexp.t





