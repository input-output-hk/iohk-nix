### Overriding Version Pins

Sometimes for testing purposes you need to override versions of
certain repos.

It is often more convenient to point to a local repo clone rather than
a revision that has been pushed to GitHub.

#### Overriding Haskell.nix version

Edit your `NIX_PATH` environment variable to point to a local clone,
or add the `-I` option to `nix-build` or `nix-shell`. For example:

```
$ nix-build -I haskell=$HOME/iohk/haskell.nix
```
or
```
$ export NIX_PATH=haskell=$HOME/iohk/haskell.nix:$NIX_PATH
$ nix-build
```

If it is set correctly, you should see the following message when building:

```
trace: using search host <haskell>
```

#### Overriding the iohk-nix version

Rather than editing `iohk-nix-src.json`, most projects allow you to
add a `NIX_PATH` entry for `iohk_nix`, as above. For example:

```
nix-build -I iohk_nix=$HOME/iohk/iohk-nix
```

#### Overriding the `hackage.nix` or `stackage.nix` version

When importing `haskell.nix`, provide the `hackageSourceJSON` or
`stackageSourceJSON` arguments, which are paths to a JSON pin
file. For example:

```
import ./haskell.nix {
  inherit pkgs;
  hackageSourceJSON = ./hackage-nix.json;
}
```
