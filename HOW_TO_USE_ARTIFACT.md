# How to use the artifact
## Requirements
- VM Player: VirtualBox 5.2.18 r124319 (Qt5.6.3)
- Appliance of the artifact: `lambda-dti.ova`

## Getting Started
1. Download the appliance (the image of the VM)
- URL: https://www.fos.kuis.kyoto-u.ac.jp/~miyazaki/popl/lambda-dti.ova

2. Import the appliance and create a VM
- Click `File` -> `Import Appliance` on VirtualBox and choose `lambda-dti.ova`
- A new VM will be created

3. Start the VM
- The VM is configured to automatically login as `popl` user and open a terminal

4. Start the interpreter and try it!
- Run `ldti` on the terminal
- Try some inputs:
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

## Next Step
Please see [README.md](README.md) for the full syntax.

## Remark
- To rebuild the interpreter, run `dune build && dune install` in `~/lambda-dti/`
- Linux users and password
  - User: `root` / Password: `popl`
  - User: `popl` / Password: `popl`
