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
    if p1.pos_fname <> "" then
      fprintf ppf "File \"%s\", " p1.pos_fname;
    fprintf ppf "line %d, character %d -- line %d, character %d"
      p1.pos_lnum
      (p1.pos_cnum - p1.pos_bol)
      p2.pos_lnum
      (p2.pos_cnum - p2.pos_bol)
end

module Format = struct
  open Format

  let empty_formatter = make_formatter (fun _ _ _ -> ()) (fun _ -> ())

  let make_print_debug debug f =
    if debug then
      fprintf err_formatter f
    else
      fprintf empty_formatter f
end

module List = struct
  let zip l1 l2 =
    let rec zip' l1 l2 l = match l1, l2 with
      | [], _ | _, [] -> List.rev l
      | (x :: xs), (y :: ys) -> zip' xs ys @@ (x, y) :: l
    in
    zip' l1 l2 []

  let repeat i n =
    let rec f i n l = if n <= 0 then l else f i (n - 1) @@ i :: l in
    f i n []
end
