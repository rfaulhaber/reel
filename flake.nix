{
  description = "reel";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    projectName = "reel";
    version = "0.1.0";
    mkClangPath = pkgs: "${pkgs.llvmPackages_16.libclang.lib}/lib";
    mkBindgenExtraClangArgs = pkgs:
      with pkgs;
        lib.concatStringsSep " " [
          (builtins.readFile "${stdenv.cc}/nix-support/libc-crt1-cflags")
          (builtins.readFile "${stdenv.cc}/nix-support/libc-cflags")
          (builtins.readFile "${stdenv.cc}/nix-support/cc-cflags")
          "-isystem ${llvmPackages_16.libclang.lib}/lib/clang/${
            lib.getVersion llvmPackages_16.libclang
          }/include"
          "-isystem ${emacs}/include"
        ];
    mkDefaultNativeBuildInputs = pkgs:
      with pkgs; [
        llvmPackages_16.libclang
        llvmPackages_16.clangUseLLVM
        emacs
        libiconv
        pkg-config
        openssl
      ];
  in {
    packages = {
      aarch64-darwin = let
        system = "aarch64-darwin";
        pkgs = import nixpkgs {inherit system;};
      in {
        ${projectName} = pkgs.rustPlatform.buildRustPackage {
          inherit version;

          nativeBuildInputs =
            (mkDefaultNativeBuildInputs pkgs)
            ++ [
              pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
            ];

          pname = projectName;
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;

          LIBCLANG_PATH = mkClangPath pkgs;
          BINDGEN_EXTRA_CLANG_ARGS = mkBindgenExtraClangArgs pkgs;
        };

        default = self.packages.aarch64-darwin.${projectName};
      };

      x86_64-linux = let
        system = "x86_64-linux";
        pkgs = import nixpkgs {inherit system;};
      in {
        ${projectName} = pkgs.rustPlatform.buildRustPackage {
          inherit version;

          nativeBuildInputs = mkDefaultNativeBuildInputs pkgs;

          pname = projectName;
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;

          LIBCLANG_PATH = mkClangPath pkgs;
          BINDGEN_EXTRA_CLANG_ARGS = mkBindgenExtraClangArgs pkgs;
        };
        default = self.packages.x86_64-linux.${projectName};
      };
    };
    devShells = let
      mkBuildInputs = pkgs:
        with pkgs; [
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
    in {
      aarch64-darwin.default = let
        pkgs = import nixpkgs {system = "aarch64-darwin";};
      in
        pkgs.mkShell {
          nativeBuildInputs = self.packages.aarch64-darwin.${projectName}.nativeBuildInputs;

          buildInputs = mkBuildInputs pkgs;

          LIBCLANG_PATH = self.packages.aarch64-darwin.${projectName}.LIBCLANG_PATH;
          BINDGEN_EXTRA_CLANG_ARGS = self.packages.aarch64-darwin.${projectName}.BINDGEN_EXTRA_CLANG_ARGS;
        };
      x86_64-linux.default = let
        pkgs = import nixpkgs {system = "x86_64-linux";};
      in
        pkgs.mkShell {
          nativeBuildInputs = self.packages.x86_64-linux.${projectName}.nativeBuildInputs;

          buildInputs = mkBuildInputs pkgs;

          LIBCLANG_PATH = self.packages.x86_64-linux.${projectName}.LIBCLANG_PATH;
          BINDGEN_EXTRA_CLANG_ARGS = self.packages.x86_64-linux.${projectName}.BINDGEN_EXTRA_CLANG_ARGS;
        };
    };

    formatter = {
      aarch64-darwin = let pkgs = import nixpkgs {system = "aarch64-darwin";}; in pkgs.alejandra;
      x86_64-linux = let pkgs = import nixpkgs {system = "x86_64-linux";}; in pkgs.alejandra;
    };
  };
}
