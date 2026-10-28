{
  lib,
  pkgs,
  ...
}: let
  # canto.hk's Cantonese Fonts put tone-marked Jyutping above Han characters.
  # They're a paid download, so they stay out of Git and the Nix store;
  # install them by hand into ~/.local/share/fonts/canto-hk. Without them
  # fontconfig falls back to Noto.
  #
  # Use the monochrome Hei files (VF Cantonese) and Heritage (VF Cantonese
  # Heritage). The color Hei files are also named VF Cantonese, and Firefox
  # Nightly 161.0a1 draws their characters and Jyutping on top of each other.
  #
  # Firefox's font.name.*.zh-HK and zh-TW prefs win over fontconfig, so set
  # them to "" in user.js.
  #
  # binding="strong": `fc-validate -l zh-hk` says these fonts are 5 glyphs
  # short of zh-hk, and a weak binding loses to Noto Sans CJK HK.
  # prepend_first: Home Manager's 52-hm-default-fonts.conf runs first and
  # puts a strong Noto Sans ahead of sans-serif; a plain prepend lands
  # behind it.
  cantoneseFamilies = {
    sans-serif = "VF Cantonese";
    serif = "VF Cantonese Heritage";
  };
  # A lang test needs the whole tag (zh-hant doesn't match zh-hant-hk), so
  # each tag is listed.
  cantoneseLangs = [
    "zh-hk"
    "zh-tw"
    "zh-mo"
    "zh-hant"
    "zh-hant-hk"
    "zh-hant-tw"
    "yue"
    "yue-hk"
    "yue-hant"
    "yue-hant-hk"
  ];
  # Plain text, not `settings`: that puts <edit> before <test>, which makes
  # the rule fire for every language.
  cantoneseMatch = lang: generic: family: ''
    <match target="pattern">
      <test name="lang"><string>${lang}</string></test>
      <test qual="any" name="family"><string>${generic}</string></test>
      <edit name="family" mode="prepend_first" binding="strong"><string>${family}</string></edit>
    </match>
  '';
  cantoneseMatches =
    lib.concatMapStrings (
      lang: lib.concatStrings (lib.mapAttrsToList (cantoneseMatch lang) cantoneseFamilies)
    )
    cantoneseLangs;
in {
  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      serif = ["Noto Serif"];
      sansSerif = ["Noto Sans"];
      monospace = ["FantasqueSansM Nerd Font Mono"];
      emoji = ["Noto Color Emoji"];
    };
    configFile.cantonese-jyutping = {
      text = ''
        <?xml version="1.0"?>
        <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
        <fontconfig>
        ${cantoneseMatches}</fontconfig>
      '';
    };
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
