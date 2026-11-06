{pkgs, ...}: {
  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      serif = ["Noto Serif"];
      sansSerif = ["Noto Sans"];
      monospace = ["FantasqueSansM Nerd Font Mono"];
      emoji = ["Noto Color Emoji"];
    };

    # Every VF Cantonese face, colour or monochrome, declares
    # maxp.maxFunctionDefs = 7 but defines 118 functions in fpgm. FreeType
    # rejects the font's TrueType hinting ("too many function definitions"),
    # and LibreOffice then leaves some characters blank on screen, while its
    # PDF export, which embeds unhinted outlines, shows them. The autohinter
    # never runs the font's bytecode. "contains" also matches the HKEdB,
    # Heritage, and Support families.
    #
    # This is text because `settings` emits <edit> before <test>, and
    # fontconfig applies an edit that precedes the test to every font.
    configFile.vf-cantonese-autohint.text = ''
      <?xml version="1.0"?>
      <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
      <fontconfig>
        <description>Autohint VF Cantonese, whose hinting FreeType rejects</description>
        <match target="font">
          <test name="family" compare="contains">
            <string>VF Cantonese</string>
          </test>
          <edit name="autohint" mode="assign">
            <bool>true</bool>
          </edit>
        </match>
      </fontconfig>
    '';
  };

  home.packages = with pkgs; [
    atkinson-hyperlegible
    atkinson-hyperlegible-mono
    atkinson-hyperlegible-next
    atkinson-monolegible
    ubuntu-classic
    fira
    fira-code
    intel-one-mono
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    noto-fonts-monochrome-emoji

    nerd-fonts.caskaydia-cove
    nerd-fonts.comic-shanns-mono
    nerd-fonts.dejavu-sans-mono
    nerd-fonts.droid-sans-mono
    nerd-fonts.fantasque-sans-mono
    nerd-fonts.fira-code
    nerd-fonts.intone-mono
    nerd-fonts.iosevka-term
    nerd-fonts.jetbrains-mono
    nerd-fonts.meslo-lg
    nerd-fonts.mononoki
    nerd-fonts.noto
    nerd-fonts.ubuntu-mono
    nerd-fonts.zed-mono
  ];
}
