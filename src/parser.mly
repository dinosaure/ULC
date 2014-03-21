%{
  let get_loc start_pos end_pos =
    let get_pos pos =
      { Lambda.lnum = pos.Lexing.pos_lnum;
        cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol; }
    in (get_pos start_pos, get_pos end_pos)
%}

%token EOF
%token Lambda
%token Dot
%token Underscore
%token Semicolon
%token LPA
%token RPA
%token <string> TermID

%start main
%start single
%type <Lambda.context -> (Lambda.t list * Lambda.context)> main
%type <Lambda.context -> Lambda.t> single

%%

main:
  | EOF
  { fun ctx -> ([], ctx) }
  | x = term Semicolon r = main
  { fun ctx ->
    let x = x ctx in
    let (r, ctx') = r ctx in
    (x :: r, ctx') }

single:
  | EOF
  { fun ctx -> raise (Failure "EOF")}
  | x = term EOF
  { x }

term:
  | a = application
  { a }
  | Lambda t = TermID Dot e = term
  { fun ctx -> let loc = get_loc $startpos $endpos in 
    let ctx' = Lambda.add_name ctx t in
    Lambda.Abs (loc, t, e ctx') }
  | Lambda Underscore Dot e = term
  { fun ctx -> let loc = get_loc $startpos $endpos in 
    let ctx' = Lambda.add_name ctx "_" in
    Lambda.Abs (loc, "_", e ctx') }


application:
  | a = atomic
  { a }
  | a = application b = atomic
  { fun ctx ->
    let loc = get_loc $startpos $endpos in
    let a = a ctx in
    let b = b ctx in
    Lambda.App (loc, a, b) }


atomic:
  | t = TermID
  { fun ctx -> let loc = get_loc $startpos $endpos
    in Lambda.Val (loc, Lambda.index_of_name loc ctx t, Lambda.length_of_ctx ctx) }
  | LPA e = term RPA
  { e }
