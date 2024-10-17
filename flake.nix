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
    supportedSystems = ["x86_64-linux" "aarch64-darwin"];
    forSystems = systems: f:
      nixpkgs.lib.genAttrs systems
      (system: f system (import nixpkgs {inherit system;}));
    forAllSystems = forSystems supportedSystems;
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
    mkDefaultNativeBuildInputs = system: pkgs:
      with pkgs;
        [
          emacs
          libiconv
          llvmPackages_16.clangUseLLVM
          llvmPackages_16.libclang
        ]
        ++ lib.optionals
        (system == "aarch64-darwin")
        [
          pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
        ];
  in {
    packages = forAllSystems (
      system: pkgs: {
        ${projectName} = pkgs.rustPlatform.buildRustPackage {
          inherit version;

          nativeBuildInputs = mkDefaultNativeBuildInputs system pkgs;

          pname = projectName;
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;

          LIBCLANG_PATH = mkClangPath pkgs;
          BINDGEN_EXTRA_CLANG_ARGS = mkBindgenExtraClangArgs pkgs;
        };
        default = self.packages.${system}.${projectName};
      }
    );
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

          just
        ];
    in
      forAllSystems
      (system: pkgs: {
        default = pkgs.mkShell {
          nativeBuildInputs = self.packages.${system}.${projectName}.nativeBuildInputs;

          buildInputs = mkBuildInputs pkgs;

          LIBCLANG_PATH = self.packages.${system}.${projectName}.LIBCLANG_PATH;
          BINDGEN_EXTRA_CLANG_ARGS = self.packages.${system}.${projectName}.BINDGEN_EXTRA_CLANG_ARGS;
        };
      });
    formatter = forAllSystems (
      system: pkgs:
        pkgs.alejandra
    );
  };
}
