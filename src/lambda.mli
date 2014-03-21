type position = { lnum : int; cnum : int }
type location = (position * position)

type context

exception Error of (location * string)
exception NoRuleApplies

val string_of_error : (location * string) -> string

type t =
  | App of (location * t * t)
  | Abs of (location * string * t)
  | Val of (location * int * int)

val empty_ctx : context
val length_of_ctx : context -> int

val name_of_index : location -> context -> int -> string
val primitive_of_index : location -> context -> int -> t
val index_of_name : location -> context -> string -> int

val add_name : context -> string -> context
val add_primitive : context -> string -> t -> context

val to_string : context -> t -> string
val print_context : context -> unit

val shifting : int -> t -> t
val substitution : int -> t -> t -> t
val substitution_top : t -> t -> t

val eval : context -> t -> t
