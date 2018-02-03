# lambda-rti

[![Build Status](https://travis-ci.org/ymyzk/lambda-rti.svg?branch=master)](https://travis-ci.org/ymyzk/lambda-rti)

**lambda-rti** is an interpreter of the implicitly typed gradual language (ITGL).
This implementation consists of:

- Garcia and Cimini's type inference algorithm;
- a cast-inserting translator from the ITGL to the blame calculus; and
- an evaluator of the blame calculus with runtime type inference.

## Requirements
- OCaml 4.03.0+
- Jbuiler (Dune)
- Menhir
- OUnit (for running unit tests)

## Getting started
### Global installation
```console
$ jbuilder build
$ jbuilder install
$ lrti
```

Run `$ lrti --help` for command line options.

### Local installation
```console
$ jbuilder build
$ ./_build/default/bin/main.exe
```

Run `$ ./_build/default/bin/main.exe --help` for command line options.

## Running tests
```console
$ jbuilder runtest
```

## Syntax
### Top-level
- Let declaration: `let x ... = e;;`
- Recursion declaration: `let rec f x ... = e;;`
- Expression: `e;;`

### Expressions `e`
- Constants: integers, `true`, `false`, and `()`
- Unary operators: `+` and `-`
- Binary operators: `+`, `-`, `*`, `/`, `mod`, `=`, `<>`, `<`, `<=`, `>`, `>=`, `&&`, and `||`
- Abstraction:
  - Simple: `fun x -> e`
  - Multiple parameters: `fun x y z ... -> e`
  - With type annotations: `fun (x: U1) y (z: U3) ... -> e`
- Application: `e1 e2`
- Let expression:
  - Simple: `let x = e1 in e2`
  - Multiple parameters: `let x y z ... = e1 in e2`
  - With type annotations: `let (x:U1) y (z: U3) ... : U ... = e1 in e2`
- Recursion:
  - Simple: `let rec f x = e1 in e2`
  - Multiple parameters: `let rec f x y z ... = e1 in e2`
  - With type annotations: `let rec f (x: U1) y (z: U3) ... : U = e1 in e2`
- If-then-else Expression: `if e1 then e2 else e3`
- Sequence of expressions: `e1; e2`
- Type ascription: `(e : U)`

### Types `U`
- Dynamic type: `?`
- Base types: `bool`, `int`, and `unit`
- Function type: `U -> U`

### Comments
- Simple: `(* comments *)`
- Nested comments: `(* leave comments here (* nested comments are also supported *) *)`

## Standard library
Some useful functions are available:
```
# is_bool;;
- : ? -> bool = <fun>
# is_int;;
- : ? -> bool = <fun>
# is_unit;;
- : ? -> bool = <fun>
# is_fun;;
- : ? -> bool = <fun>

# succ;;
- : int -> int = <fun>
# pred;;
- : int -> int = <fun>
# max;;
- : int -> int -> int = <fun>
# min;;
- : int -> int -> int = <fun>
# abs;;
- : int -> int = <fun>
# max_int;;
- : int = 4611686018427387903
# min_int;;
- : int = -4611686018427387904

# not;;
- : bool -> bool = <fun>

# print_bool;;
- : bool -> unit = <fun>
# print_int;;
- : int -> unit = <fun>
# print_newline;;
- : unit -> unit = <fun>

# ignore;;
- : 'a -> unit = <fun>

# exit;;
- : int -> unit = <fun>
```

## Examples
```
# (fun (x:?) -> x + 2) 3;;
- : int = 5

# (fun (x:?) -> x + 2) true;;
Blame on the expression side:
line 2, character 14 -- line 2, character 15

# (fun (x:?) -> x 2) (fun y -> true);;
- : ? = true: bool => ?

# (fun (x:?) -> x) (fun y -> y);;
- : ? = <fun>: ? -> ? => ?

# (fun (x:?) -> x 2) (fun y -> y);;
- : ? = 2: int => ?

# (fun (f:?) -> f true) ((fun x -> x) ((fun (y:?) -> y) (fun z -> z + 1)));;
Blame on the environment side:
line 6, character 55 -- line 6, character 69

# (fun (f:?) -> f 2) ((fun x -> x) ((fun (y:?) -> y) (fun z -> z + 1)));;
- : ? = 3: int => ?

# let id x = x;;
id : 'a -> 'a = <fun>

# let dynid (x:?) = x;;
dynid : ? -> ? = <fun>

# succ;;
- : int -> int = <fun>

# (fun (f:?) -> f 2) (id (dynid succ));;
- : ? = 3: int => ?

# (fun (f:?) -> f true) (id (dynid succ));;
Blame on the environment side:
line 12, character 33 -- line 12, character 37

# let rec sum (n:?) = if n < 1 then 0 else n + sum (n - 1);;
sum : ? -> int = <fun>

# sum 100;;
- : int = 5050

# sum true;;
Blame on the expression side:
line 13, character 23 -- line 13, character 24

# exit 0;;
```

## References
- [Ronald Garcia and Matteo Cimini. Principal Type Schemes for Gradual Programs. In Proc. of ACM POPL, 2015.](https://dl.acm.org/citation.cfm?id=2676992)
