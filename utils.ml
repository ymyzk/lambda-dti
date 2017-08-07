module Error = struct
  open Format
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

  let dummy_range = {
    start_p=dummy_pos;
    end_p=dummy_pos;
  }

  let pp_range ppf {start_p=p1; end_p=p2} =
    (* TODO: show filename *)
    fprintf ppf "line %d, character %d -- line %d, character %d"
      p1.pos_lnum
      (p1.pos_cnum - p1.pos_bol)
      p2.pos_lnum
      (p2.pos_cnum - p2.pos_bol)
end
