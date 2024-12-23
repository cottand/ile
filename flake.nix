{

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }: (flake-utils.lib.eachDefaultSystem (system:
    let pkgs = import nixpkgs { inherit system; }; in {

      packages.default = pkgs.buildGoModule {
        inherit system;
        vendorHash = null;
        pname = "ile";
        version = "0.0-dev";
        src = nixpkgs.lib.sources.cleanSource ./.;
        ldflags = [ "-s -w" ];
        CGO_ENABLED = "0";

        ANTLR_BIN = "${pkgs.antlr4_13}/bin/antlr";

        buildInputs = [
          pkgs.antlr4_13
        ];
      };
    }));
}
