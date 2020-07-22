{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.postgresql_12
  ];

  shellHook = ''
    echo 'Loading API env vars from api/.env '
    . api/.env
  '';
}


