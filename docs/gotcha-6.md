## Longer error messages from `nix build`

`nix build` is one of the new CLI commands introduced in Nix 2.0.  Its
usage is different from `nix-build` (notice the hyphen), and has some
improvements.

### Question

Does anyone know how to make nix show longer error messages?

I'm using it to build the interpreter and it's only showing me the
last ten lines of a moderately large type error, which isn't very
helpful.

I can search for the log file in /nix/store/var and manually
decompress it, but that's not ideal.


### Answer

In your `nix build` command, replace `build` with `log`. This should
work for both local builds and builds which hydra has done.

