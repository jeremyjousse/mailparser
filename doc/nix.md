# Nix
[Nix](https://nixos.org/) is a package manager that goes beyond traditional ones (like Apt, NPM, Brew, etc.) as it allows:
* full reproducibility (the same input guarantee to give exactly the same output)
* "local" packages, meaning you don't need to install globally
* describe in a Nix file everything you need to build or run an application, and Nix configures everything locally:
  * language dependencies like JDK 14 or GHC 8.8.3
  * build systems like Maven or Stack
  * system dependencies like Zlib or PostgreSQL
  * environment variables like `FEDID_URL`
  * etc.

Below is a high-level explanation of our different Nix files.

## `shell.nix`

This is the file that describes everything you need to build/run Mail Parser.

The most important parts are:
* `buildInputs` describing all executables that must be available in the path (compiler, formatter, PostgreSQL...)
* the environment variables with sensible default values

Additionally, we make wrappers around some executables (e.g. Ormolu and Stack) to pass some additional flags automatically.

## `nix/sources.[json|nix]`
These files are generated and managed by [Niv](https://github.com/nmattia/niv), a simple dependency manager for Nix.

While [Nixpkgs](https://github.com/NixOS/nixpkgs) provides a single entry point for tens of thousands of packages, some are not part of it, and thus additional dependencies must be managed.
Handling the Nixpkgs and other repositories by hand is tedious: whenever you want to add or update a dependency, you need to find a recent Git hash, change it, prefetch the new `tar.gz`, find the `sha256`, change it. Niv makes this a one liner `niv update`.

* To update all dependencies, run `niv update` (or `niv update foobar` to update the `foobar` dependency only)
* To add a dependency to repository `https://github.com/foo/bar`, run `niv add foo/bar`
* To remove a dependency, run `niv drop bar`

## `nix/default.nix`
This file centralizes `nixpkgs` imports, so that other Nix files can simply `import ./nix {}` and have a fully configured Nixpkgs.

## `nix/stack-integration.nix`
This Nix derivation is based on [Stack documentation](https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file) to integrate Nix with Stack. Stack needing to have access to everything during build (e.g. PostgreSQL drivers for Persistent), it must be aware of Nix, since those executables/libraries are provided by Nix.

We "tell" Stack to use this Nix shell file in `shell.nix`, by wrapping the `stack` executable to pass the `--nix-shell-file` option.
