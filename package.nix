{ system, lib, buildGoModule, antlr, installShellFiles, clang, ... }:
let
  name = "ile";
in
buildGoModule {
  inherit system;
  vendorHash = null;
  pname = name;
  version = "0.0-dev";
  src = lib.sources.cleanSource ./.;
  ldflags = [ "-s -w" ];

  env.CGO_ENABLED = "0";
  ANTLR_BIN = "${antlr}/bin/antlr";

  # will run antlr codegen
  preBuild = ''
    go generate ./...
  '';

  buildInputs = [
    antlr
    installShellFiles
    clang # required for testing via Go plugins, which requires CGO
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
}
