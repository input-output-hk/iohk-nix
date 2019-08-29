# How to run your script in CI (Buildkite)

Generally you edit `.buildkite/pipeline.yml` and add a step.

```yaml
steps:
  - label: 'Count lines of code'
    command: '.buildkite/loc.sh'
    agents:
      system: x86_64-linux
```

* Beware of YAML syntax. It's safest just to quote all string values.
* The `.buildkite` directory is a convenient place to dump your scripts.
* But note that `command:` paths are relative to the project root, not
  `.buildkite`.
* Keep the steps simple so that they are easy to copy & paste to test
  locally.
* Set the `agents:` filter to just Linux, because those build agents
  will be faster and more reliable.


## Build Environment

Here is an example script to count project lines of code and provide a
motivational message.

```
#!/bin/sh

cloc .

echo 'wow, good job!!!' | cowsay
```

The obvious problem with this script is that you don't know whether
these programs are installed on the build agent(s).

In fact, on the build machines there are a _very_ limited set of
commands installed:

* bash
* buildkite-agent
* coreutils
* git
* nix

So here follows a number of ways to get a build environment for the
script. Pick the simplest way that works for you.


## Method 1: `nix-shell -p`

Change your command to:

```
nix-shell -p cloc cowsay --run .buildkite/loc.sh
```

This works if there's just a few programs from Nixpkgs that you need.


## Method 2: `nix-shell`

If the necessary commands are already in `shell.nix`, or you are
willing to add them to the project default shell, then do:

```
nix-shell --run .buildkite/loc.sh
```


## Method 3: Alternative `nix-shell`

If you don't want to bloat the standard shell with CI-specific
commands, then create alternative shells. This approach is also useful
if you need to set up more complicated environments.

Either create a separate nix file:

```
nix-shell my-other-shell.nix --run my-script.sh
```

And/or add it as an attribute to the existing `shell.nix`:

```
nix-shell -A attr --run my-script.sh
```

Example complicated shell file:

```nix
# my-other-shell.nix

{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [ turtle hxt ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-ci-env";
  buildInputs = [ ghc cloc cowsay git ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
  YOLO = "1";
}
```


## Method 4: Shebang Script

This uses `nix-shell` as the interpreter of the script. The file
obviously needs the execute bit set (`chmod +x my-script.sh`).

```bash
#! /usr/bin/env nix-shell
#! nix-shell -i bash -p cloc cowsay

cloc .

echo 'wow, good job!!!' | cowsay
```

To run with `nix-shell`:

```
./my-script.sh
```

To run without `nix-shell`:

```
bash my-script.sh
```

Use any of the above `nix-shell` methods in the line after the
shebang. But you must provide `-i` with the real script interpreter
command.


## Method 5: Build script

This is the most advanced method. You build the script and its
dependencies with `nix-build`, then run the result.

```
nix-build my-script.nix -o my-script.sh && ./my-script.sh
```

This way is sort of cool because you can ensure a more pure runtime
environment, or you can transfer a self-contained script to another
host with `nix-copy-closure`.

```nix
# my-script.nix

{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  deps = [ cloc cowsay ];
in
  writeShellScript "my-script.sh" ''
    #!${runtimeShell}

    export PATH=${lib.makeBinPath deps}:$PATH

    cloc .

    echo 'wow, good job!!!' | cowsay
  ''
```


## Note: Running scripts with bash

Note that the shebang `#!/bin/sh` will run your script with some crap
shell that's not bash. A portable way to run your script with bash is
to use:

```
#!/usr/bin/env bash
```


## Note: Handling failure

Shell scripts are really dodgy when it comes to error handling. Add
this line to the top of your script to ensure that it exits when a
command fails, or an undefined variable is referenced:

```bash
set -euo pipefail
```


## Note: Running scripts with Turtle

Why not write your scripts in Haskell?
Use [Turtle](http://hackage.haskell.org/package/turtle) and
[nix-shell](https://nixos.org/nixpkgs/manual/#how-to-create-ad-hoc-environments-for-nix-shell).

```
nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ turtle ])" \
    --run "runghc myscript.hs"
```
