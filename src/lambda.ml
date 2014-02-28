type position = { lnum : int; cnum : int }
type location = (position * position)

type t =
  | App of (location * t * t)
  | Abs of (location * string * t)
  | Val of (location * int * int)

type binding = Name
type context = (string * binding) list

exception Error of (location * string)
exception NoRuleApplies

let string_of_error (location, error) =
  let string_of_position { lnum; cnum; } =
    "[" ^ (string_of_int lnum) ^ ":" ^ (string_of_int cnum) ^ "]"
  in error ^ " at " ^ (string_of_position (fst location)) ^ " " ^ (string_of_position (snd location))

(* Handle context *)

let rec is_name_bound ctx name = match ctx with
  | [] -> false
  | (x, _) :: r when x = name -> true
  | x :: r -> is_name_bound r name

let rec pick_freshname ctx x = match is_name_bound ctx x with
  | true -> pick_freshname ctx (x ^ "'")
  | false -> ((x, Name) :: ctx, x)

let length_of_ctx ctx = List.length ctx

let add_binding ctx x bind = (x, bind) :: ctx
let add_name ctx x = add_binding ctx x Name

let name_of_index loc ctx index =
  try let (n, _) = List.nth ctx index in n
  with _ -> raise (Error (loc, "Variable lookup failure"))

let index_of_name loc ctx name =
  let rec aux acc = function
    | [] -> raise (Error (loc, "Identifier " ^ name ^ " is unbound"))
    | (x, _) :: r when x = name -> acc
    | x :: r -> aux (acc + 1) r
  in aux 0 ctx

let empty_ctx = []

(* Pretty print *)

let to_string ctx t =
  let buffer = Buffer.create 16 in
  let rec aux ctx = function
    | Abs (loc, name, expr) ->
      let (ctx', x') = pick_freshname ctx name in
      Buffer.add_string buffer "(λ ";
      Buffer.add_string buffer x';
      Buffer.add_string buffer ". ";
      aux ctx' expr;
      Buffer.add_string buffer ")";
    | App (loc, a, b) ->
      Buffer.add_string buffer "(";
      aux ctx a;
      Buffer.add_string buffer " ";
      aux ctx b;
      Buffer.add_string buffer ")";
    | Val (loc, index, depth) ->
      if length_of_ctx ctx = depth then
        Buffer.add_string buffer (name_of_index loc ctx index)
      else
        Buffer.add_string buffer "[bad index]"
  in aux ctx t; Buffer.contents buffer

(* Shifting *)

let shifting d t =
  let rec aux c = function
    | Val (loc, index, depth) when index >= c -> Val (loc, index + d, depth + d)
    | Val (loc, index, depth) -> Val (loc, index, depth + d)
    | Abs (loc, name, expr) -> Abs (loc, name, aux (c + 1) expr)
    | App (loc, a, b) -> App (loc, aux c a, aux c b)
  in aux 0 t

(* Substitution *)

let substitution j s t =
  let rec aux c = function
    | Val (loc, index, depth) when index = j + c -> shifting c s
    | Val (loc, index, depth) -> Val (loc, index, depth)
    | Abs (loc, name, expr) -> Abs (loc, name, aux (c + 1) expr)
    | App (loc, a, b) -> App (loc, aux c a, aux c b)
  in aux 0 t

let substitution_top s t =
  shifting (-1) (substitution 0 (shifting 1 s) t)

let is_val ctx = function
  | Abs (_, _, _) -> true
  | _ -> false

let rec eval ctx t =
  let rec aux ctx = function
    | App (loc, Abs (_, name, expr), value) when is_val ctx value ->
      substitution_top value expr
    | App (loc, a, b) when is_val ctx a ->
      let b' = aux ctx b in
      App (loc, a, b')
    | App (loc, a, b) ->
      let a' = eval ctx a in
      App (loc, a', b)
    | _ -> raise NoRuleApplies
  in try let t' = aux ctx t in eval ctx t'
    with NoRuleApplies -> t
