{
  config,
  lib,
  pkgs,
  ...
}: let
  # Untagged text such as file names in Dolphin, save dialogs, and Ghostty
  # takes Chinese characters from whichever installed font fontconfig ranks
  # first for the default family: on Kinoite with canto.hk's fonts installed,
  # VF Cantonese for sans-serif and monospace and Symbola for serif.
  cjkFallback = {
    serif = "Noto Serif CJK HK";
    sansSerif = "Noto Sans CJK HK";
    monospace = "Noto Sans Mono CJK HK";
  };

  # Listing cjkFallback in defaultFonts would override the face the distro
  # picks for text tagged zh, yue, ja, or ko (e.g. Noto Sans CJK SC for
  # zh-cn). fontconfig gives an untagged request the locale's language (en
  # here), and "zh" also matches tags such as zh-cn and zh-hant-hk.
  cjkLangs = ["zh" "yue" "ja" "ko"];

  cjkMatch = family: cjk: let
    untagged =
      map (lang: ''<test qual="all" name="lang" compare="not_contains"><string>${lang}</string></test>'')
      cjkLangs;
  in ''
    <match target="pattern">
    ${lib.concatLines untagged}<test qual="any" name="family"><string>${lib.escapeXML family}</string></test>
    <edit name="family" mode="append" binding="same"><string>${lib.escapeXML cjk}</string></edit>
    </match>
  '';
in {
  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      serif = ["Noto Serif"];
      sansSerif = ["Noto Sans"];
      monospace = ["FantasqueSansM Nerd Font Mono"];
      emoji = ["Noto Color Emoji"];
    };

    # Default priority 90 runs after 52-hm-default-fonts turns generic
    # names like sans-serif into Noto Sans. append puts the CJK face right
    # after that family, and binding="same" gives it the family's binding,
    # so a font that better matches the locale's language can't outrank it:
    # with a weak binding and a fr locale, VF Cantonese wins on fr coverage.
    configFile.cjk-fallback.text = ''
      <?xml version="1.0"?>
      <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
      <fontconfig>
      <description>Use Noto CJK for Chinese characters in untagged text</description>
      ${lib.concatStrings (lib.mapAttrsToList (
          generic: cjk:
            lib.concatMapStrings (family: cjkMatch family cjk)
            config.fonts.fontconfig.defaultFonts.${generic}
        )
        cjkFallback)}</fontconfig>
    '';

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
