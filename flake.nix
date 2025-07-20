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
      packages."ile-wasm" = packages.ile.overrideAttrs (final: prev: {
        env.GOOS = "js";
        env.GOARCH = "wasm";
        pname = "ile.wasm";

        # we would need a WASM host to run WASM-compiled tests, so let's not
        doCheck = false;
        postInstall = ''
        '';
      });

      packages.go-wasm-exec = pkgs.runCommand "go-wasm-exec" {} ''
        mkdir -p $out/lib
        GOROOT=$(${pkgs.go}/bin/go env GOROOT)
        ${pkgs.go}/bin/go version

        cp -r $GOROOT/lib/wasm/ $out/lib/wasm
      '';

#        # also add wasm_exec.js, which comes with the go toolchain
#        postInstall = ''
#          mkdir -p $out/misc/wasm
#          GOROOT=$(${pkgs.go}/bin/go env GOROOT)
#          cp $GOROOT/misc/wasm/*.js $out/misc/wasm
#        '';

      # TODO derivation with go's files here rather than the postInstall above

    }));
}
