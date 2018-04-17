{ package ? "primitive-median", compiler ? "ghc841" }:

(import ./default.nix {
  inherit package compiler;
}).primitive-median
