{

let reservedWords = [
  ("fun", Parser.FUN);
  ("true", Parser.TRUE);
  ("false", Parser.FALSE);
  ("int", Parser.INT);
  ("bool", Parser.BOOL);
]

}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+ { main lexbuf }
| ['0'-'9']+ { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }
| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ":" { Parser.COLON }
| ";;" { Parser.SEMISEMI }
| "->" { Parser.RARROW }
| "+" { Parser.PLUS }
| "*" { Parser.STAR }
| "?" { Parser.QUESTION }
| "<" { Parser.LT }
| "'a" { Parser.GPARAM }
| "'b" { Parser.SPARAM }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
  {
    let id = Lexing.lexeme lexbuf in
    try
      List.assoc id reservedWords
    with
    _ -> Parser.ID id
  }
| eof { exit 0 }
