{
  exception Error

  open Parser
}

let alpha = ['a' - 'z' 'A' - 'Z']
let term = (['a' - 'z'] alpha *)

rule main = parse
  | eof                 { EOF }
  | [' ' '\t']          { main lexbuf }
  | '\n'                { Lexing.new_line lexbuf; main lexbuf }
  | '\\'                { Lambda }
  | '.'                 { Dot }
  | '('                 { LPA }
  | ')'                 { RPA }
  | '_'                 { Underscore }
  | ';'                 { Semicolon }
  | term as t           { TermID t }
  | _                   { raise Error }
