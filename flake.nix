{
  description = "reel";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      projectName = "reel";
    in rec {
      packages.${projectName} = pkgs.rustPlatform.buildRustPackage {
        pname = projectName;
        version = "0.1.0";
        src = ./.;
        cargoLock.lockFile = ./Cargo.lock;

        nativeBuildInputs = with pkgs; [llvmPackages_14.libclang emacs];

        LIBCLANG_PATH = "${pkgs.llvmPackages_14.libclang.lib}/lib";
        BINDGEN_EXTRA_CLANG_ARGS = with pkgs;
          lib.concatStringsSep " " [
            (builtins.readFile "${stdenv.cc}/nix-support/libc-crt1-cflags")
            (builtins.readFile "${stdenv.cc}/nix-support/libc-cflags")
            (builtins.readFile "${stdenv.cc}/nix-support/cc-cflags")
            "-isystem ${llvmPackages_14.libclang.lib}/lib/clang/${
              lib.getVersion llvmPackages_14.libclang
            }/include"
            "-isystem ${emacs}/include"
          ];
      };

      packages.default = self.packages.${system}.${projectName};

      apps.${projectName} =
        flake-utils.lib.mkApp {drv = packages.${projectName};};

      apps.default = self.apps.${system}.${projectName};

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          cargo
          rustc
          rustfmt
          clippy
          rust-analyzer
          rustup

          emacs
          nodejs_20
          nodePackages_latest.eask
        ];

        nativeBuildInputs = with pkgs; [llvmPackages_14.libclang];

        LIBCLANG_PATH = "${pkgs.llvmPackages_14.libclang.lib}/lib";
        BINDGEN_EXTRA_CLANG_ARGS = with pkgs;
          lib.concatStringsSep " " [
            (builtins.readFile "${stdenv.cc}/nix-support/libc-crt1-cflags")
            (builtins.readFile "${stdenv.cc}/nix-support/libc-cflags")
            (builtins.readFile "${stdenv.cc}/nix-support/cc-cflags")
            "-isystem ${llvmPackages_14.libclang.lib}/lib/clang/${
              lib.getVersion llvmPackages_14.libclang
            }/include"
            "-isystem ${emacs}/include"
          ];
      };
    });
}
