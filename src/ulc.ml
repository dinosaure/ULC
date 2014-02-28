let input = ref stdin
let output = ref stdout
let rest = ref []

let usage = "usage " ^ Sys.argv.(0) ^ " input [-o output]"

let arglist = [
  ("-o", Arg.String (fun s -> output := (open_out s)), ": define output file");
]

let compiler input output =
  let (l, c) = Parser.main Lexer.main (Lexing.from_channel input) Lambda.empty_ctx
  in List.iter
    (fun x ->
      try output_string output (Lambda.to_string c (Lambda.eval c x)); output_string output ";\n"
      with Lambda.Error (location, error) -> prerr_endline (Lambda.string_of_error (location, error))) l

let () =
  try 
    Arg.parse arglist (fun x -> rest := x :: !rest) usage;
    compiler (if List.length !rest = 0 then !input else (open_in (List.hd !rest))) !output
  with
    | Failure s -> prerr_endline s
    | Sys_error s -> prerr_endline s
    | Lambda.Error (location, error) -> prerr_endline (Lambda.string_of_error (location, error))
