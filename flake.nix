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
    {
      packages.default = self.packages.${system}.ile;
      packages.ile = pkgs.buildGoModule {
        inherit system;
        vendorHash = null;
        pname = name;
        version = "0.0-dev";
        src = nixpkgs.lib.sources.cleanSource ./.;
        ldflags = [ "-s -w" ];

        CGO_ENABLED = "0";
        ANTLR_BIN = "${pkgs.antlr}/bin/antlr";

        # will run antlr codegen
        preBuild = ''
          go generate ./...
        '';

        buildInputs = [
          pkgs.antlr
          pkgs.installShellFiles
          pkgs.clang # required for testing via Go plugins, which requires CGO
#          antlr-format
        ];

        postInstall = ''
          mkdir -p share/completions
          $out/bin/${name} completion bash > share/completions/${name}.bash
          $out/bin/${name} completion fish > share/completions/${name}.fish
          $out/bin/${name} completion zsh > share/completions/${name}.zsh

          # implicit behavior
          installShellCompletion share/completions/${name}.{bash,fish,zsh}
        '';
      };
    }));
}
