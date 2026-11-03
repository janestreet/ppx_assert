@@ portable

open Sexplib0

[%%template:
[@@@mode.default m = (local, global)]

(** Types used in the generated code *)

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

[%%template:
[@@@alloc.default __ @ m = (stack_local, heap_global)]

(** Functions called by the generated code *)

val test_pred
  : ('a : k).
  pos:string
  -> sexpifier:('a @ m -> Sexp.t @ m) @ local
  -> here:Lexing.position list
  -> ?message:string
  -> ('a @ m -> bool) @ local
  -> 'a @ m
  -> unit
[@@kind k = (value_or_null, bits8, bits16, bits32, bits64, word)]

val test_eq
  : ('a : k).
  pos:string
  -> sexpifier:('a @ m -> Sexp.t @ m) @ local
  -> comparator:('a @ m -> 'a @ m -> int) @ local
  -> here:Lexing.position list
  -> ?message:string
  -> ?equal:('a @ m -> 'a @ m -> bool) @ local
  -> 'a @ m
  -> 'a @ m
  -> unit
[@@kind k = (value_or_null, bits8, bits16, bits32, bits64, word)]

val test_result
  : ('a : k).
  pos:string
  -> sexpifier:('a @ m -> Sexp.t @ m) @ local
  -> comparator:('a @ m -> 'a @ m -> int) @ local
  -> here:Lexing.position list
  -> ?message:string
  -> ?equal:('a @ m -> 'a @ m -> bool) @ local
  -> expect:'a @ m
  -> got:'a @ m
  -> unit
[@@kind k = (value_or_null, bits8, bits16, bits32, bits64, word)]]

(** Called to set/unset the [diff] function, used by [test_result] *)
val set_diff_function : (from_:string -> to_:string -> unit) option @ portable -> unit
