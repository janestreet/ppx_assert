@@ portable

open Base

[%%template:
[@@@mode.default m = (local, global)]

(** Types used in the generated code *)

type 'a test_pred =
  ?here:Lexing.position list
  -> ?message:string
  -> ('a @ m -> bool) @ local
  -> 'a @ m
  -> unit

type 'a test_eq =
  ?here:Lexing.position list
  -> ?message:string
  -> ?equal:('a @ m -> 'a @ m -> bool) @ local
  -> 'a @ m
  -> 'a @ m
  -> unit

type ('a : value_or_null) test_result =
  ?here:Lexing.position list
  -> ?message:string
  -> ?equal:('a @ m -> 'a @ m -> bool) @ local
  -> expect:'a @ m
  -> 'a @ m
  -> unit]

[%%template:
[@@@alloc.default __ @ m = (stack_local, heap_global)]

(** Functions called by the generated code *)

val test_pred
  : ('a : value_or_null).
  pos:string
  -> sexpifier:('a @ m -> Sexp.t @ m) @ local
  -> here:Lexing.position list
  -> ?message:string
  -> ('a @ m -> bool) @ local
  -> 'a @ m
  -> unit

val test_eq
  : ('a : value_or_null).
  pos:string
  -> sexpifier:('a @ m -> Sexp.t @ m) @ local
  -> comparator:('a @ m -> 'a @ m -> int) @ local
  -> here:Lexing.position list
  -> ?message:string
  -> ?equal:('a @ m -> 'a @ m -> bool) @ local
  -> 'a @ m
  -> 'a @ m
  -> unit

val test_result
  : ('a : value_or_null).
  pos:string
  -> sexpifier:('a @ m -> Sexp.t @ m) @ local
  -> comparator:('a @ m -> 'a @ m -> int) @ local
  -> here:Lexing.position list
  -> ?message:string
  -> ?equal:('a @ m -> 'a @ m -> bool) @ local
  -> expect:'a @ m
  -> got:'a @ m
  -> unit]

(** Called to set/unset the [diff] function, used by [test_result] *)
val set_diff_function : (from_:string -> to_:string -> unit) option @ portable -> unit
