with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "vishetnet";
  buildInputs = [
    nodejs
    purescript
  ];
   shellHook = ''
     export PATH=$PATH:$(pwd)/node_modules/.bin
   '';
}
