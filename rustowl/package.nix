# RustOwl's server and its Neovim client, built from one locked source.
#
# The Neovim client must match the server it talks to, so both read the same
# manifest here; rustowl/default.nix installs the server and vim/default.nix
# packages the client.
{
  inputs,
  lib,
  pkgs,
}: let
  src = inputs.rustowl_src;
  cargoManifest = builtins.fromTOML (builtins.readFile "${src}/Cargo.toml");
  toolchainConfig = builtins.fromTOML (builtins.readFile "${src}/rust-toolchain.toml");
  toolchainChannel = toolchainConfig.toolchain.channel;
  toolchainDate = lib.concatStringsSep "-" (lib.tail (lib.splitString "-" toolchainChannel));
  hostTuple = pkgs.stdenv.hostPlatform.rust.rustcTarget;
  rustowlToolchain = "${toolchainChannel}-${hostTuple}";
  rustToolchain = pkgs.rust-bin.fromRustupToolchainFile "${src}/rust-toolchain.toml";
  rustPlatform = pkgs.makeRustPlatform {
    cargo = rustToolchain;
    rustc = rustToolchain;
  };
  rustowl = rustPlatform.buildRustPackage {
    pname = "rustowl";
    version = cargoManifest.package.version;
    inherit src;

    cargoLock.lockFile = "${src}/Cargo.lock";
    nativeBuildInputs = [pkgs.pkg-config];
    buildInputs = [pkgs.openssl];
    doCheck = false;

    RUSTOWL_TOOLCHAIN = rustowlToolchain;
    HOST_TUPLE = hostTuple;
    TOOLCHAIN_CHANNEL = toolchainChannel;
    TOOLCHAIN_DATE = toolchainDate;

    postInstall = ''
      mkdir -p "$out/bin/sysroot"
      ln -s ${rustToolchain} "$out/bin/sysroot/${rustowlToolchain}"
    '';
  };
  neovimPlugin = pkgs.vimUtils.buildVimPlugin {
    pname = "rustowl-nvim";
    version = cargoManifest.package.version;
    inherit src;

    postInstall = ''
      find "$out" -mindepth 1 -maxdepth 1 \
        ! -name lua ! -name ftplugin -exec rm -rf {} +
    '';
  };
in {
  inherit neovimPlugin;
  server = rustowl;
}
