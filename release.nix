{src ? ./. , systems ? [ builtins.currentSystem ] }:
let
  pkgs = import <nixpkgs> {};
in {
  build = pkgs.lib.genAttrs systems (system:
    let
      pkgs = import <nixpkgs> {inherit system;};
    in
      pkgs.callPackage src {}
  );
}
