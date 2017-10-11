open Format
open Typing
open Utils.Error

exception Blame of range

val pp_substitutions : formatter -> substitutions -> unit
