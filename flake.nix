{

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, ... }: (flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      name = "ile";
      antlr-format = pkgs.buildNpmPackage {
        pname = "antlr-format-cli";
        version = "v1.2.8-cli";

        src = (pkgs.fetchFromGitHub {
          owner = "antlr-ng";
          repo = "antlr-format";
          rev = "v1.2.8-cli";
          hash = "sha256-yYrBLoT73MVRAOpoJhRNc5wr096FbxJNK8iyNiCiTjI=";
        }) + "/cli";

        sourceRoot = pkgs.fetchFromGitHub {
          owner = "antlr-ng";
          repo = "antlr-format";
          rev = "v1.2.8-cli";
          hash = "sha256-yYrBLoT73MVRAOpoJhRNc5wr096FbxJNK8iyNiCiTjI=";
        };



        npmDepsHash = "sha256-FlFbTdmBv324jlP85/aHT8FzpR0NzcrGdD2PMuq/TAo=";
        npmPackFlags = [ "--ignore-scripts" ];
        installPhase = ''
          ls -la dist
          mkdir -p $out/bin
          cp -r dist/* $out/bin
        '';

        nativeBuildInputs = [ pkgs.esbuild ];
      };
    in
    rec {
      packages.default = self.packages.${system}.ile;
      packages.ile = pkgs.callPackage ./package.nix { };
      packages.ile-wasm = packages.ile.override {
        env.GOOS = "js";
        env.GOARCH = "wasm";
      };
    }));
}
