{ pkgs ? import <nixpkgs> {}, pythonPackages ? pkgs.python39Packages }:

pkgs.mkShell {
    buildInputs = with pythonPackages; [
        pyyaml tabulate
    ];
}
