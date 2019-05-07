### Filtering sources

Usually you want to filter the sources of your project's package to
prevent unnecessary rebuilds.

Do this by adding a module that overrides the `src` option:

```nix
{
  packages.my-project.src = pkgs.lib.cleanSource ./.;
}
```

Look in [&lt;nixpkgs/lib/sources.nix&gt;](https://github.com/NixOS/nixpkgs/blob/master/lib/sources.nix)
for the source filtering functions.
