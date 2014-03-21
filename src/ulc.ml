let input = ref stdin
let output = ref stdout
let rest = ref []

let usage = "usage " ^ Sys.argv.(0) ^ " input [-o output]"

let arglist = [
  ("-o", Arg.String (fun s -> output := (open_out s)), ": define output file");
]

let ulc_of_input input =
  Parser.main Lexer.main (Lexing.from_channel input)

let ulc_of_string str =
  Parser.single Lexer.main (Lexing.from_string str)

let ( >>= ) context (name, value) =
  Lambda.add_primitive context name (ulc_of_string value context)

let compiler input output context =
  let (l, c) = ulc_of_input input context
  in List.iter
    (fun x ->
      try output_string output (Lambda.to_string c (Lambda.eval c x)); output_string output ";\n"
      with Lambda.Error (location, error) -> prerr_endline (Lambda.string_of_error (location, error))) l;
    Lambda.print_context c

let () =
  try
    Arg.parse arglist (fun x -> rest := x :: !rest) usage;
    compiler
      (if List.length !rest = 0 then !input else (open_in (List.hd !rest)))
      !output
      (Lambda.empty_ctx
      >>= ("zero", "\\f.\\x.x")
      >>= ("succ", "\\n.\\f.\\x. f (n f x)")
      >>= ("and", "\\p.\\q. p q p")
      >>= ("or", "\\p.\\q. p p q")
      >>= ("true", "\\a.\\b. a")
      >>= ("false", "\\a.\\b. b")
      >>= ("if", "\\p.\\a.\\b. p a b"))
  with
    | Failure s -> prerr_endline s
    | Sys_error s -> prerr_endline s
    | Lambda.Error (location, error) -> prerr_endline (Lambda.string_of_error (location, error))
