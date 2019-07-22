## attribute `unbuildable` missing

```
ERROR: evaluation completed in 9.13 seconds with 3 errors
nix-tools.cexes.x86_64-pc-mingw32-cardano-ledger.cardano-chain-validation-exe.x86_64-linux: attribute 'unbuildable' missing,
at /nix/store/16sxhsfhvya2yvn6xdjqrz5c6m9w8yp1-hackage-exprs-source/hackage/unix-2.7.2.2-r2-4ef1d010d70a4a07a717e853d4a440c105dad38c6199316e320fdd4c48dacd34.nix:23:56
...
```
### Probable cause

This usually arises when using dependencies that are incompatible with the compilation target (i.e. in this case `unix` when building windows binaries)

### Diagnosis

We know the compilation target is windows and the dependency Nix is failing to build is `unix` because of `.x86_64-pc-mingw32-cardano-ledger` and `/hackage/unix-2.7.2.2`
in the error message.

If we look at the `unix` [cabal file](https://github.com/haskell/unix/blob/master/unix.cabal#L66):

    if os(windows)
        -- This package currently supports neither Cygwin nor MinGW,
        -- therefore os(windows) is effectively not supported.
        build-depends: unbuildable<0
        buildable: False

This is the source of our error!

### Fix

We need to locate which package has the `unix` dependency. If it's not in your package's `.cabal` file, you will need to check each of your depdendencies to see which has a `unix` dependency.


When you have located the offending dependency, in the nix derivation for that dependency you should see `(hsPkgs.unix)` which should be replaced with:
```
          (if system.isWindows
           then (hsPkgs.Win32)
           else (hsPkgs.unix))
```

This will allow nix to build the appropriate dependencies and your error should be gone!