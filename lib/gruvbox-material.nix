# Gruvbox Material palettes shared by the terminal agents' themes.
#
# The OpenCode and oh-my-pi themes name their colours from one palette so a
# changed hue lands in both. `darkMedium` is the material dark-medium variant;
# `mixDarkMedium` is the generated mix variant, which differs only in its
# foreground and accent hues.
let
  darkMedium = {
    bgDim = "#1b1b1b";
    bg0 = "#282828";
    bg1 = "#32302f";
    bg2 = "#32302f";
    bg3 = "#45403d";
    bg5 = "#5a524c";
    bgStatusline2 = "#3a3735";
    fg0 = "#d4be98";
    fg1 = "#ddc7a1";
    gray0 = "#7c6f64";
    gray1 = "#928374";
    gray2 = "#a89984";
    red = "#ea6962";
    orange = "#e78a4e";
    yellow = "#d8a657";
    green = "#a9b665";
    aqua = "#89b482";
    blue = "#7daea3";
    purple = "#d3869b";
    diffRed = "#402120";
    diffGreen = "#34381b";
    diffBlue = "#0e363e";
    visualRed = "#4c3432";
    visualGreen = "#3b4439";
  };
in {
  inherit darkMedium;
  mixDarkMedium =
    darkMedium
    // {
      fg0 = "#e2cca9";
      fg1 = "#e2cca9";
      red = "#f2594b";
      orange = "#f28534";
      yellow = "#e9b143";
      green = "#b0b846";
      aqua = "#8bba7f";
      blue = "#80aa9e";
    };
}
