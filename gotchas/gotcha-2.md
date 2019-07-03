## Haskell.nix Dependency Build Problems

Sometimes the dependencies of a project fail to build and you will
need to add overrides to the Haskell.nix config to fix it.

This file lists build problems that you may encounter and their solutions.

### `No attribute: hsc2hs`

This is a bug in Haskell.nix which you will see if you have a
dependency which uses `hsc2hs` as a build tool.

Work around it by adding `hsc2hs` to the `extra-deps` of `stack.yaml`.

### Errors about `Divisible` when compiling `contravariant`

```
[1 of 3] Compiling Data.Functor.Contravariant.Divisible ( src/Data/Functor/Contravariant/Divisible.hs, /home/refold/code/scrive/contravariant-1.5/dist-newstyle/build/x86_64-linux/ghc-8.6.4/contravariant-1.5/build/Data/Functor/Contravariant/Divisible.o ) [Data.StateVar changed]

src/Data/Functor/Contravariant/Divisible.hs:233:10: error:
    • Could not deduce (Contravariant (Backwards f))
        arising from the superclasses of an instance declaration
      from the context: Divisible f
        bound by the instance declaration
        at src/Data/Functor/Contravariant/Divisible.hs:233:10-47
    • In the instance declaration for ‘Divisible (Backwards f)’
```

This is a bug in GHC 8.6.1 through to 8.6.4. It shipped with the wrong
version of the transformers package .

Work around this by adding the following to `stack.yaml`:

```yaml
extra-deps:
- process-1.6.5.0
- transformers-0.5.6.2
```

### Errors building `transformers-0.5.5.0`

This error will occur on `stack-to-nix` projects which use LTS 13.*
snapshots. It is because those stackage snapshots have incorrect
information about which package versions are shipped with GHC 8.6.4.

```
Configuring library for transformers-0.5.5.0..
Setup: Encountered missing dependencies:
base (>=2 && <6) && <0
```

Work around this by adding the following to `stack.yaml` and
regenerating with `stack-to-nix`.

```yaml
extra-deps:
- process-1.6.5.0
- transformers-0.5.6.2
```

### Errors about package version bounds

This generally won't happen if you are using the Cabal solver
(`plan-to-nix`) or a Stackage LTS.

But if you need to "jailbreak" a package, add a module to your package
set like this:

```nix
{
  packages.transformers.components.library.doExactConfig = true;
}
```

### `attribute ghc864 missing`

This happens when a compiler is not available in the Nixpkgs package
collection that you are using. Nixpkgs will only keep one compiler of
each major version.

You may need to use a different Nixpkgs channel.

If using `stack-to-nix`, you could choose a different Stackage snapshot.

If using `plan-to-nix`, you could regenerate your plan and nix with a
compiler version that exists in your Nixpkgs.


### `attribute 'lts-13.17' missing`

```
error: attribute 'lts-13.17' missing, at /nix/store/z229kzbikg42cxd1xq42wwsp1divan0z-source/default.nix:81:19
```

This means that the resolver specified in `stack.yaml` was not found
in `stackage.nix`. The snapshot is probably more recent than your
version of `stackage.nix`.

The repos `stackage.nix` and `hackage.nix` are updated daily, along
with their JSON pins in `haskell.nix`.

The best course of action is usually to update your `haskell.nix`
version.

For projects that use `iohk-nix` to grab `haskell.nix` this usually
means updating `iohk-nix` pinned version in your project.
This can be done by running a `nix/update-iohk-nix.sh` script
or manually updating the json file describing the pin:
```
nix-prefetch-git https://github.com/input-output-hk/iohk-nix | tee iohk-nix.json
```

If this does not fix the issue then the version of `haskell.nix` in
`iohk-nix` is out-of-date. In this case, running
`pins/update-defaults.sh` in `iohk-nix` and merging the changes
is needed beforehand.

### `error: attribute '0.42.0' missing, at /home/user/project/nix/pkgs.nix:10:22`

This usually means that the the given version of a package does not
exist in `hackage.nix`.

As above, the best course of action is usually to update your
`haskell.nix` version, which will bring in a newer version of
`hackage.nix`.

### `pq` dependency missing, or other `extra-libraries` package

If a build fails due to an unknown system dependency listed in the
`extra-libraries` section of a cabal file, then it must be added to
the file
[`haskell.nix/lib/system-nixpkgs-map.nix`](https://github.com/input-output-hk/haskell.nix/blob/master/lib/system-nixpkgs-map.nix).

This file maps the package name listed in `extra-libraries` to an
attribute of the Nixpkgs package collection.


### warning: dumping very large path

Builds are slow to start, with the following message printed:

    warning: dumping very large path (> 256 MiB); this may run out of memory

This can happen when copying the project sources into the Nix
store. It is probably copying your local `.stack-work` or
`dist-newstyle`, which is not what you want.

Add a source filter (e.g. `iohkLib.cleanHaskellSource`) to your
Haskell.nix project to avoid this.
See [Filtering Sources](./gotcha-3.md).
