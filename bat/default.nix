{...}: let
  lessFlags = import ../lib/less-flags.nix;
in {
  programs.bat = {
    enable = true;
    config = {
      theme = "gruvbox-dark";
      style = "plain";
      pager = "less ${lessFlags}";
    };
  };
}
