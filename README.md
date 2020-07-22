# Mailparser

Mailparser is a Haskell/Purescipt app which aims to parse a Gmail account in order to extract mailing list statistics.

## Build
### Using Nix (recommended)

For more information on how we use Nix, see [the dedicated page](nix.md)

### Enjoy

If you have [Nix](https://nixos.org/nix/) installed on your machine (compatible with all Linux, macOS and Windows WSL distributions), you don't need to install anything else, the `shell.nix` file will take care of providing everything (Stack, Spago, Postgres, etc.) without global installation. Run `nix-shell` and you will land in an environment where everything is configured.

### Optional: Enable Direnv
Direnv additionally provides auto-loading when navigating into directories in shells. Thus you can rely on it to automatically load `shell.nix` whenever you `cd` to `mailparser` while keeping your favorite shell (ZSH, Bash, Fish...).

* install globally [direnv](https://direnv.net/) with your favorite package manager (with Nix: `nix-env --install direnv`, or `nix-env -i direnv` for short)
* enable `direnv` integration in this directory: `direnv allow`
* now whenever you `cd` into a `mailparser` directory, your shell will be configured with dependencies and environment variables:
```shell
$ cd ~
$ echo $FOO

$ ormolu --version
The program ‘ormolu’ is currently not installed.

$ cd code/mailparser/
$ echo $FOO
bar
$ ormolu --version
ormolu 0.0.3.1

$ cd ..
$ echo $FOO

$ ormolu --version
The program ‘ormolu’ is currently not installed.
```