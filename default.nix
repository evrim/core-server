{ pkgs ? (import <nixpkgs> {})
}:

pkgs.stdenv.mkDerivation {
  name = "core-server";
  source = ./.;
  buildInputs = with pkgs; [ 
    sbcl 
    which
    darcs
    git
    screen
  ];
}
