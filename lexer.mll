{
open Utils.Error

let reservedWords = [
  ("fun", fun r -> Parser.FUN r);
  ("true", fun r -> Parser.TRUE r);
  ("false", fun r -> Parser.FALSE r);
  ("int", fun r -> Parser.INT r);
  ("bool", fun r -> Parser.BOOL r);
]

let range_of lexbuf =
  {
    start_p=Lexing.lexeme_start_p lexbuf;
    end_p=Lexing.lexeme_end_p lexbuf;
  }
}

rule main = parse
  [' ' '\t']+ { main lexbuf }
| [' ' '\t']* '\n'+ { Lexing.new_line lexbuf; main lexbuf }
| ['0'-'9']+
  {
    let value = int_of_string (Lexing.lexeme lexbuf) in
    let range = range_of lexbuf in
    Parser.INTV { value=value; range=range }
  }
| "(" { Parser.LPAREN (range_of lexbuf) }
| ")" { Parser.RPAREN (range_of lexbuf) }
| ":" { Parser.COLON (range_of lexbuf) }
| ";;" { Parser.SEMISEMI (range_of lexbuf) }
| "->" { Parser.RARROW (range_of lexbuf) }
| "+" { Parser.PLUS (range_of lexbuf) }
| "*" { Parser.STAR (range_of lexbuf) }
| "?" { Parser.QUESTION (range_of lexbuf) }
| "<" { Parser.LT (range_of lexbuf) }
| "'a" (['1'-'9'] ['0'-'9']* as a)
  {
    let value = int_of_string a in
    let range = range_of lexbuf in
    Parser.GPARAM { value=value; range=range }
  }
| "'b" (['1'-'9'] ['0'-'9']* as b)
  {
    let value = int_of_string b in
    let range = range_of lexbuf in
    Parser.SPARAM { value=value; range=range }
  }
| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
  {
    let id = Lexing.lexeme lexbuf in
    let range = range_of lexbuf in
    try
      (List.assoc id reservedWords) range
    with
    _ -> Parser.ID { value=id; range=range }
  }
| eof { exit 0 }
