{
  description = "Idris2 URL";

  nixConfig = {
    substituters = [
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store?priority=0"
      "https://mirrors.ustc.edu.cn/nix-channels/store?priority=10"
      "https://cache.nixos.org/?priority=20"
    ];

  };

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs@{ flake-parts, self, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];

      perSystem = { config, pkgs, lib, ... }:
        {

          formatter = pkgs.nixpkgs-fmt;

          devShells.default = pkgs.mkShell {
            name = "idris2-url";
            packages = with pkgs; [ ];
          };
        };
    };

}
