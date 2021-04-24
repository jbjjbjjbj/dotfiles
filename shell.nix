{ pkgs ? import <nixpkgs> {}, pythonPackages ? pkgs.python38Packages }:

pkgs.mkShell {
    buildInputs = with pythonPackages; [
        pyyaml
    ];
}
