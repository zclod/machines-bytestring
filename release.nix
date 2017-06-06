let config = {
  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: rec {

        machines-bytestring = self.callPackage ./default.nix {};

      };
    };
  };
};

pkgs = import <nixpkgs> {inherit config;};

in {
  machines-bytestring = pkgs.haskellPackages.callPackage ./default.nix {};
}
