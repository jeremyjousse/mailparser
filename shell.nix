{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.docker
    pkgs.docker-compose
    pkgs.nodejs-12_x
    pkgs.postgresql_12
  ];

  shellHook = ''
    echo 'Loading API env vars from api/.env '
    . api/.env
  '';
}


