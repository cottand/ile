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
      packages.ile-wasm = packages.ile.overrideAttrs (final: prev: {
        env.GOOS = "js";
        env.GOARCH = "wasm";
        pname = "ile.wasm";

        # we would need a WASM host to run WASM-compiled tests, so let's not
        doCheck = false;
        postInstall = ''
        '';
      });
      packages.ile-wasm-wasi = packages.ile.overrideAttrs (final: prev: {
        env.GOOS = "wasip1";
        env.GOARCH = "wasm";
        pname = "ile.wasm";

        # we would need a WASM host to run WASM-compiled tests, so let's not
        doCheck = false;
        postInstall = ''
        '';
      });

      packages.ile-wasm-types = pkgs.writeText "ile-wasm.d.ts" (builtins.readFile ./ile/wasm.d.ts);

      packages.go-wasm-exec = pkgs.runCommand "go-wasm-exec" { } ''
        mkdir -p $out/lib
        GOROOT=$(${pkgs.go}/bin/go env GOROOT)
        ${pkgs.go}/bin/go version

        cp -r $GOROOT/lib/wasm/ $out/lib/wasm
      '';

      packages.web-playground-assets = pkgs.runCommand "ile-web-playground" { } ''
        mkdir -p $out
        cp ${./playground/index.html} $out/index.html
        cp ${packages.go-wasm-exec}/lib/wasm/wasm_exec.js $out/wasm_exec.js
        cp ${packages.ile-wasm}/bin/js_wasm/ile $out/ile.wasm
      '';

      packages.web-playground = pkgs.writeShellScriptBin "web-playground" ''
        echo "Serving playground at http://localhost:8000"
        ${pkgs.python3}/bin/python3 -m http.server -d ${packages.web-playground-assets} 8000
      '';

    }));
}
