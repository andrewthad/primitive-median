{ package ? "primitive-median", compiler ? "ghc841" }:
let fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
    nixpkgs = fetchNixpkgs {
      rev = "c4dea092a3831fa3b0cd86b72282d89141b6d945";
      sha256 = "04c83ivnyaayx724xbkpv02xad83xmzi4mjns0hd4sk15m84s5hn"; 
      sha256unpacked = "1gcjmv7fq3lfd5agija20v9qr5qfqs4m9jlrbbxjagqvhp665in8"; 
    };
    pkgs = import nixpkgs { config = {}; overlays = []; };
    inherit (pkgs) haskell;
 
  filterPredicate = p: type:
    let path = baseNameOf p; in !(
       (type == "directory" && path == "dist")
    || (type == "symlink"   && path == "result")
    || (type == "directory" && path == ".git")
    || (type == "symlink"   && pkgs.lib.hasPrefix "result" path)
    || pkgs.lib.hasSuffix "~" path
    || pkgs.lib.hasSuffix ".o" path
    || pkgs.lib.hasSuffix ".so" path
    || pkgs.lib.hasSuffix ".nix" path);
    
  overrides = haskell.packages.${compiler}.override {
    overrides = self: super:
    with haskell.lib;
    with { cp = file: (self.callPackage (./nix/haskell + "/${file}.nix") {}); 
           build = name: path: self.callCabal2nix name (builtins.filterSource filterPredicate path) {}; 
         };
    {
      primitive = cp "primitive"; 
      primitive-median = build "primitive-median" ./.;
    };
  };
in rec {
  drv = overrides.${package};
  primitive-median = if pkgs.lib.inNixShell then drv.env else drv;
}
