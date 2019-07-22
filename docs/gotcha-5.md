## GHCi gotchas

### Strange package errors

If you get errors about missing modules that _should_ be present.

```
<interactive>:1:6: error:
    Not in scope: ‘System.IO.hSetBuffering’
    No module named ‘System.IO’ is imported.
```

Then remove any local GHC environment files:

    rm .ghc.environment.*


### can't load .so/.DLL

When loading modules in GHCi, an error similar to the following occurs:

```
can't load .so/.DLL for: libHSrandom-1.1-3ypV4EIycgb35PKjTYYr5q.so (libHSrandom-1.1-3ypV4EIycgb35PKjTYYr5q.so: cannot open shared object file: No such file or directory)
```

There is probably a dodgy `random` package in `~/.ghc`. This may be
because a Nix store garbage collection has removed the library which
this module required.

The solution is to remove it with: (replace 8.6.3 with your GHCi version).

```
rm -rf ~/.ghc/x86_64-linux-8.6.3/package.conf.d/
```
