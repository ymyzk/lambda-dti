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

  let join_range r1 r2 = {
    start_p=r1.start_p;
    end_p=r2.end_p;
  }
end
