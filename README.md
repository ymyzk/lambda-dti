# interpreter
## Getting started
- Run `$ jbuilder build`
- Run `$ ./_build/default/bin/main.exe`

## Syntax
### Top-level
- Let declaration: `let x y ... = e;;`
- Expression: `e;;`

### Expressions `e`
- Constants: integers, `true`, and `false`
- Binary operators: `+`, `*`, and `<`
- Abstraction: `fun x -> e` and `fun (x: U) -> e`
- Application: `e1 e2`
- Let expression: `let x y ... = e1 in e2` and `let x (y: U1) ... = e2`

### Types `U`
- Dynamic type: `?`
- Base types: `bool` and `int`
- Function type: `U -> U`

## Command line options
- Run `$ ./_build/default/bin/main.exe --help`
