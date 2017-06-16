(fun (f: int->int) -> f 1)
  (fun (x: int) -> x);;

(fun (f: ?) -> (f: ? => int->int) 1);;

(fun (x: int) -> x): int->int => ?;;

(fun (f: ?) -> (f: ? => ?->?) (1: int => ?));;

(fun (f: ?) -> (f: ? => ?->?) (1: int => ?))
  ((fun (x: int) -> x): int->int => ?);;

(fun (x: 'a1) -> x);;
(fun (x: 'b1) -> x);;

(fun (x: 'a1) -> x): 'a1->'a1 => ?;;
(fun (x: 'b1) -> x): 'b1->'b1 => ?;;

(fun (f: ?) -> (f: ? => ?->?) (1: int => ?))
  ((fun (x: 'a1) -> x): 'a1->'a1 => ?);;
