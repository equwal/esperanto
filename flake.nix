{
  description = "Morphological compression of the Esperanto language.";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/e158d9ed9b51c98974c5e66e1ba1c9e0255fecaa";
  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAll = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in {
      packages = forAll (pkgs: with pkgs; rec {
        esperanto = sbcl.buildASDFSystem {
          pname = "esperanto";
          version = "0.0.1";
          src = (lib.cleanSourceWith { src = self; filter = p: _: !(lib.hasSuffix ".fasl" p || lib.hasPrefix ".#" (baseNameOf p)); });
          systems = [ "esperanto" ];
          lispLibs = [  ];
          meta = { description = "Morphological compression of the Esperanto language."; homepage = "https://github.com/equwal/esperanto"; license = lib.licenses.gpl3Only; };
        };
        default = esperanto;
        # an SBCL with this system (and its dependencies) preloaded: `nix run .#sbcl`
        sbcl-with = sbcl.withPackages (ps: [ esperanto ]);
      });
      apps = forAll (pkgs: {
        sbcl = { type = "app"; program = "${self.packages.${pkgs.system}.sbcl-with}/bin/sbcl"; };
      });
    };
}
