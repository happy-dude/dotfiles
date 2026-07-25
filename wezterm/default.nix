{...}: {
  # Home Manager's wezterm package fails to build on Ubuntu 24.10, so WezTerm
  # is installed from its PPA and only the configuration is owned here.
  xdg.configFile."wezterm/wezterm.lua".source = ./.config/wezterm/wezterm.lua;
}
