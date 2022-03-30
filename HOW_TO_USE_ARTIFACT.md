# How to use the artifact
## Requirements
- VM Player: VirtualBox 5.2.18 r124319 (Qt5.6.3)
- Appliance of the artifact: `lambda-dti.ova`
  - URL: https://dl.acm.org/do/10.1145/3291628/full/

## Getting Started
1. [Download the appliance](https://dl.acm.org/do/10.1145/3291628/full/) (the image of the VM)

2. Extract the downloaded artifact.

3. Import the appliance and create a VM
  - Click `File` -> `Import Appliance` on VirtualBox and choose `lambda-dti/vm/lambda-dti.ova`
  - A new VM will be created

4. Start the VM
  - The VM is configured to automatically login as `popl` user and open a terminal

5. Start the interpreter and try it!
  - Run `ldti` on the terminal
  - Try some inputs
    - Input: `# (fun (x:?) -> x 2) (fun y -> y);;`
    - Output: `- : ? = 2: int => ?`

## Next Step
Please see [~/lambda-dti/README.md](README.md) for more examples and full syntax.
[~/lambda-dti/test/test_examples.ml](test/test_examples.ml) also contains sample programs.

## Remark
- To rebuild the interpreter, run `dune build && dune install` in `~/lambda-dti/`
- Linux users and password
  - User: `root` / Password: `popl`
  - User: `popl` / Password: `popl`
