module Error = struct
  open Lexing

  type range = {
    start_p: position;
    end_p: position;
  }

  type 'a with_range = {
    value: 'a;
    range: range;
  }
end
