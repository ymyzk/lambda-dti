# lambda-rti

[![Build Status](https://travis-ci.org/ymyzk/lambda-rti.svg?branch=master)](https://travis-ci.org/ymyzk/lambda-rti)

**lambda-rti** is an interpreter of the implicitly typed gradual language (ITGL).
This implementation consists of:

- Garcia and Cimini's type inference algorithm;
- Cast-inserting translator from the ITGL to the blame calculus; and
- Evaluator of the blame calculus with runtime type inference.

## Requirements
- OCaml 4.03.0+
- Jbuiler
- Menhir
- OUnit (for running unit tests)

## Getting started
```console
$ jbuilder build
$ ./_build/default/bin/main.exe
```

Run `$ ./_build/default/bin/main.exe --help` for command line options.

## Syntax
### Top-level
- Let declaration: `let x y ... = e;;`
- Recursion declaration: `let rec x y ... = e;;`
- Expression: `e;;`

### Expressions `e`
- Constants: integers, `true`, and `false`
- Binary operators: `+`, `-`, `*`, `/`, `=`, `<`, `<=`, `>`, and `>=`
- Abstraction: `fun x -> e` and `fun (x: U) -> e`
- Application: `e1 e2`
- Let expression: `let x y ... = e1 in e2` and `let x (y: U1) ... = e2`
- Recursion: `let rec x y ... = e1 in e2` and `let x (y: U1) ... = e2`
- If-then-else Expression: `if e1 then e2 else e3`
- Type ascription: `(e : U)`

### Types `U`
- Dynamic type: `?`
- Base types: `bool` and `int`
- Function type: `U -> U`

## Examples
```
# (fun (x:?) -> x + 2) 3;;
- : int = 5

# (fun (x:?) -> x + 2) true;;
Blame: line 2, character 21 -- line 2, character 25

# (fun (x:?) -> x 2) (fun y -> true);;
- : ? = true: bool => ?

# (fun (x:?) -> x) (fun y -> y);;
- : ? = <wrapped_fun>: ? -> ? => ?

# (fun (x:?) -> x 2) (fun y -> y);;
- : ? = 2: int => ?

# (fun (f:?) -> f true) ((fun x -> x) ((fun (y:?) -> y) (fun z -> z + 1)));;
Blame: line 6, character 55 -- line 7, character 69

# (fun (f:?) -> f 2) ((fun x -> x) ((fun (y:?) -> y) (fun z -> z + 1)));;
- : ? = 3: int => ?

# let id x = x;;
id : 'a -> 'a = <fun>

# let dynid (x:?) = x;;
dynid : ? -> ? = <fun>

# let succ x = x + 1;;
succ : int -> int = <fun>

# (fun (f:?) -> f 2) (id (dynid succ));;
- : ? = 3: int => ?

# (fun (f:?) -> f true) (id (dynid succ));;
Blame: line 12, character 18 -- line 13, character 22
```

## References
- Yusuke Miyazaki and Atsushi Igarashi. Runtime Type Inference for Gradual Typing with ML-Style Polymorphism.
- [Ronald Garcia and Matteo Cimini. Principal Type Schemes for Gradual Programs. In Proc. of ACM POPL, 2015.](https://dl.acm.org/citation.cfm?id=2676992)
