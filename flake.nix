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
      pkgs = import nixpkgs {
        inherit system;
      };

      projectName = "reel";

      # NOTE: we shouldn't have to manually add libiconv, but this gets around
      # an issue with building cargo on darwin
      nativeBuildInputs = with pkgs;
        [
          llvmPackages_16.libclang
          llvmPackages_16.clangUseLLVM
          emacs
          libiconv
          pkg-config
          openssl
        ]
        ++ (
          if pkgs.lib.hasSuffix "darwin" system
          then with pkgs; [darwin.apple_sdk.frameworks.SystemConfiguration]
          else []
        );

      libclangPath = "${pkgs.llvmPackages_16.libclang.lib}/lib";
      bindgenExtraClangArgs = with pkgs;
        lib.concatStringsSep " " [
          (builtins.readFile "${stdenv.cc}/nix-support/libc-crt1-cflags")
          (builtins.readFile "${stdenv.cc}/nix-support/libc-cflags")
          (builtins.readFile "${stdenv.cc}/nix-support/cc-cflags")
          "-isystem ${llvmPackages_16.libclang.lib}/lib/clang/${
            lib.getVersion llvmPackages_16.libclang
          }/include"
          "-isystem ${emacs}/include"
        ];
    in rec {
      packages.${projectName} = pkgs.rustPlatform.buildRustPackage {
        inherit nativeBuildInputs;

        pname = projectName;
        version = "0.1.0";
        src = ./.;
        cargoLock.lockFile = ./Cargo.lock;

        LIBCLANG_PATH = libclangPath;
        BINDGEN_EXTRA_CLANG_ARGS = bindgenExtraClangArgs;
      };

      packages.default = self.packages.${system}.${projectName};

      apps.${projectName} =
        flake-utils.lib.mkApp {drv = packages.${projectName};};

      apps.default = self.apps.${system}.${projectName};

      formatter = pkgs.alejandra;

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          cargo
          rustc
          rustfmt
          clippy
          rust-analyzer
          rustup

          emacs
          nodePackages_latest.nodejs
          nodePackages_latest.eask
        ];

        inherit nativeBuildInputs;

        LIBCLANG_PATH = libclangPath;
        BINDGEN_EXTRA_CLANG_ARGS = bindgenExtraClangArgs;
      };
    });
}
