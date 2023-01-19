{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "11d3bd58ce6e32703bf69cec04dc7c38eabe14ba";
    sha256 = "0q24hb4a3fvcizns17ddd4pshlbkfdq2m6pgcjfslwlvgnbrli5l";
  }) {
    inherit pkgs;
  };
in
  pkgs.mkShell {
    buildInputs = [
      easy-ps.purs-0_15_7
      easy-ps.spago
      easy-ps.purs-tidy
      pkgs.nodejs-18_x
    ];

    shellHook = ''
        export PATH=$PWD/node_modules/.bin:$PATH
      '';
  }
