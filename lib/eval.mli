open Format
open Typing
open Utils.Error

exception Blame of range * Syntax.CC.polarity

val pp_substitutions : formatter -> substitutions -> unit
