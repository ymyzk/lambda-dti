# lambda-dti

[![Build Status](https://travis-ci.org/ymyzk/lambda-dti.svg?branch=master)](https://travis-ci.org/ymyzk/lambda-dti)

**lambda-dti** is an interpreter of the implicitly typed gradual language (ITGL) which uses *dynamic type inference* for evaluating programs.
This implementation consists of:

- Garcia and Cimini's type inference algorithm;
- a cast-inserting translator from the ITGL to the blame calculus;
- an evaluator of the blame calculus with dynamic type inference; and
- some extensions to the ITGL.

This is an artifact of "Dynamic Type Inference for Gradual Hindley-Milner Typing" in POPL 2019.

## How to use the artifact
Please see [HOW_TO_USE_ARTIFACT.md](HOW_TO_USE_ARTIFACT.md).

## Requirements
- opam 2.0.0+
- OCaml 4.03.0+
- Dune 1.3.0+ (formerly known as Jbuilder)
- Menhir
- OUnit 2 (for running unit tests)

## Getting started
### A. Building from source
```console
$ dune build
$ ./_build/default/bin/main.exe
```
Run `$ ./_build/default/bin/main.exe --help` for command line options.

(Optional) To install the application,
```
$ dune install
$ ldti
```

### B. Running a Docker image
```console
$ docker run -it --rm ymyzk/lambda-dti
```

## Tips
### Running tests
```console
$ dune runtest
```

### Using debug mode
By enabling the debug mode, our interpreter show various messages to stderr.
```console
$ ldti -d
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
- If-then-else expression: `if e1 then e2 else e3`
- Sequence of expressions: `e1; e2`
- Type ascription: `(e : U)`

### Types `U`
- Dynamic type: `?`
- Base types: `bool`, `int`, and `unit`
- Function type: `U -> U`
- Type variables: `'a`, `'b`, ...

### Comments
- Simple: `(* comments *)`
- Nested comments: `(* leave comments here (* nested comments are also supported *) *)`

## Standard library
Some useful functions and values are available:
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

# (fun (x:?) -> x) (fun y -> y);;
- : ? = <fun>: ? -> ? => ?

(* DTI: a type of y is instantiated to int *)
# (fun (x:?) -> x 2) (fun y -> y);;
- : ? = 2: int => ?

(* DTI: a type of x is instantiated to X1->X2 where X1 and X2 are fresh,
   then X1 and X2 are instantiated to int *)
# (fun (f:?) -> f 2) ((fun x -> x) ((fun (y:?) -> y) (fun z -> z + 1)));;
- : ? = 3: int => ?

# (fun (f:?) -> f true) ((fun x -> x) ((fun (y:?) -> y) (fun z -> z + 1)));;
Blame on the environment side:
line 8, character 55 -- line 8, character 69

(* Let polymorphism *)
# let id x = x;;
id : 'a -> 'a = <fun>

# let dynid (x:?) = x;;
dynid : ? -> ? = <fun>

(* succ is in the standard library *)
# succ;;
- : int -> int = <fun>

# (fun (f:?) -> f 2) (id (dynid succ));;
- : ? = 3: int => ?

# (fun (f:?) -> f true) (id (dynid succ));;
Blame on the environment side:
line 15, character 33 -- line 15, character 37

(* Recursion *)
# let rec sum (n:?) = if n < 1 then 0 else n + sum (n - 1);;
sum : ? -> int = <fun>

# sum 100;;
- : int = 5050

# sum true;;
Blame on the expression side:
line 17, character 23 -- line 17, character 24

# exit 0;;
```

## References
- [Ronald Garcia and Matteo Cimini. Principal Type Schemes for Gradual Programs. In Proc. of ACM POPL, 2015.](https://dl.acm.org/citation.cfm?id=2676992)
