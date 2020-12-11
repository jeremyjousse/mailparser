let

  # Must be the same as the Stack resolver we use!
  ghcCompiler = "ghc884";

  # By default, all functions/values come fron Nixpkgs
in with import nix/default.nix { };

let

  # Wrap Ormolu to add our default extensions that are not enabled by default in Ormolu
  # Run 'ormolu --manual-exts' to find out which extensions are not enabled by default
  # Inspired by https://nixos.wiki/wiki/Nix_Cookbook
  # If for whatever reason you want to reformat the whole code base, run:
  # for f in ./**/*.hs; do
  #   ormolu --mode inplace "$f"
  # done
  ormolu-wrapped = symlinkJoin {
    name = "ormolu";
    paths = [ ormolu ];
    buildInputs = [ makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/ormolu \
        --add-flags "\
          --ghc-opt -XBangPatterns \
          --ghc-opt -XTypeApplications \
        "
    '';
  };

  # Wrap Stack to configure Nix integration and target the correct Stack-Nix file
  # --nix -> Enable Nix support
  # --no-nix-pure -> Pass environment variables (like LOCALES or FEDID_URL)
  # --nix-shell-file nix/stack-integration.nix -> Specify the Nix file to use (otherwise it uses shell.nix by default, which we don't want)
  stack-wrapped = symlinkJoin {
    name = "stack";
    paths = [ stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --no-nix-pure \
          --nix-shell-file ../nix/stack-integration.nix \
        "
    '';
  };

in mkShell {
  # Useful packages for Mail Parser, you can manually use `nix-shell` to get a shell with everything configured, or even better, install `direnv`, see `.envrc`
  buildInputs = [
    # Packages needed to build/run the backend #
    stack-wrapped
    zlib
    postgresql_12

    # Packages needed to build/run the front
    nodejs-12_x
    purescript
    spago

    # Development tools #
    # Formatter
    ormolu-wrapped
    # Code suggestions
    hlint
    # Haddock parsing and search
    haskell.packages.${ghcCompiler}.hoogle
    # Language Server Protocol (e.g. to use an LSP plugin in VS Code, Atom, Emacs, etc.) #
    haskell.packages.${ghcCompiler}.haskell-language-server
    # Needed by hie-bios, see https://github.com/haskell/haskell-language-server/issues/176
    haskell.packages.${ghcCompiler}.tasty-discover

    # Nix integration #
    # Manage Nix package versions easily
    niv
    # Nix file formatter
    nixfmt

    # Quality of life improvements to start a local server #
    docker
    docker-compose
  ];

  # Force the Nix path to our frozen set of packages, to ensure Stack-with-Nix uses the correct one rather than the global <nixpkgs>
  # See https://nixos.org/nixos/nix-pills/nix-search-paths.html for more information
  NIX_PATH = "nixpkgs=" + path;

  FOO = "bar";
}
