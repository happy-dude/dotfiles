{
  config,
  lib,
  pkgs,
  inputs,
  username,
  ...
}: {
  xdg.configFile."zsh/.p10k.zsh".source = ./.config/zsh/.p10k.zsh;
  xdg.configFile."zsh/.zlogin".source = ./.config/zsh/.zlogin;
  xdg.configFile."zsh/.zlogout".source = ./.config/zsh/.zlogout;
  xdg.configFile."zsh/.zpreztorc".source = ./.config/zsh/.zpreztorc;
  xdg.configFile."zsh/.zprofile".source = ./.config/zsh/.zprofile;
  xdg.configFile."zsh/.zshenv".source = ./.config/zsh/.zshenv;
  xdg.configFile."zsh/.zshrc".source = ./.config/zsh/.zshrc;
  xdg.configFile."zsh/completions/_rustup" = lib.mkIf (username == "schan") {
    source = "${pkgs.rustup}/share/zsh/site-functions/_rustup";
  };

  xdg.configFile."zsh/.zprezto".source = inputs.prezto;

  programs.zsh = {
    dotDir = config.home.homeDirectory;

    enable = true;
    enableCompletion = false; # Let prezto handle compinit

    envExtra = ''
      # Put the content of your .zshenv file here
      ${builtins.readFile ./.zshenv}
      # Keep mutable source-install fallbacks behind Home Manager packages.
      typeset -gU path PATH
      for fallback_path in "$HOME/go/bin" "$HOME/.cargo/bin"; do
        path=(''${path:#$fallback_path})
        [[ -d "$fallback_path" ]] && path+=("$fallback_path")
      done
      unset fallback_path
    '';
  };
}
