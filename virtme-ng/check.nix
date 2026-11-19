{
  homes,
  lib,
  pkgs,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  inherit (import ../lib/homes.nix {inherit lib;}) shared;
  virtme = shared homes "the installed virtme-ng package" (home:
    toString (lib.findFirst (p: lib.getName p == "virtme-ng")
      (throw "virtme-ng is missing from home.packages")
      home.config.home.packages));
in {
  virtme-module-preparation = mkCheck {
    name = "virtme-module-preparation";
    tools = [pkgs.binutils pkgs.cpio pkgs.kmod pkgs.stdenv.cc];
    script = ''
      export HOME="$PWD/home"
      mkdir -p "$HOME" tree/arch/x86/boot tree/drivers
      cd tree
      printf 'Linux version 0.0.0\n' > arch/x86/boot/bzImage
      printf 'CONFIG_MODULES=y\n' > .config
      printf 'drivers/probe.o\n' > modules.order
      touch modules.builtin modules.builtin.modinfo System.map
      # depmod indexes ELF objects. No kernel build or module load is needed.
      printf 'const char info[] __attribute__((section(".modinfo"))) = "license=GPL";\n' | "$CC" -x c -c -o drivers/probe.ko -

      # Exercise the installed wrapper, not the unwrapped preparation helper.
      # Force 9p so even the dry run doesn't start a virtiofsd process.
      timeout -k 5s 30s ${virtme}/bin/virtme-run --kdir "$PWD" --mods auto \
        --dry-run --force-9p --disable-kvm --force-initramfs --save-initramfs initrd

      moddir=.virtme_mods/lib/modules/0.0.0
      module="$moddir/kernel/drivers/probe.ko"
      test -L "$module"
      [[ $(readlink "$module") != /* ]]
      test "$module" -ef drivers/probe.ko
      test "$moddir/build" -ef .
      grep -Fx 'kernel/drivers/probe.ko:' "$moddir/modules.dep"
      cpio -i --to-stdout bin/busybox <initrd | cmp - ${lib.getExe pkgs.pkgsStatic.busybox}
    '';
  };
}
