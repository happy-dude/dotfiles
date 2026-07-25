{
  config,
  lib,
  pkgs,
  inputs,
  username,
  ...
}: {
  home.activation.createZshStateDirectory = config.lib.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir -p ${lib.escapeShellArg "${config.xdg.stateHome}/zsh"}
  '';

  xdg.configFile."zsh/.p10k.zsh".source = ./.config/zsh/.p10k.zsh;
  xdg.configFile."zsh/.zlogin".source = ./.config/zsh/.zlogin;
  xdg.configFile."zsh/.zlogout".source = ./.config/zsh/.zlogout;
  xdg.configFile."zsh/.zpreztorc".source = ./.config/zsh/.zpreztorc;
  xdg.configFile."zsh/completions/_rustup" = lib.mkIf (username == "schan") {
    source = "${pkgs.rustup}/share/zsh/site-functions/_rustup";
  };

  xdg.configFile."zsh/.zprezto".source = inputs.prezto;

  programs.zsh = {
    dotDir = "${config.xdg.configHome}/zsh";

    enable = true;
    enableCompletion = false; # Let prezto handle compinit

    envExtra = ''
      ${builtins.readFile ./.config/zsh/.zshenv}
      # Keep XDG_STATE_HOME available for HISTFILE set in .zpreztorc.
      export XDG_STATE_HOME="''${XDG_STATE_HOME:=$HOME/.local/state}"
      # Keep mutable source-install fallbacks behind Home Manager packages.
      typeset -gU path PATH
      for fallback_path in "$HOME/go/bin" "$HOME/.cargo/bin"; do
        path=(''${path:#$fallback_path})
        [[ -d "$fallback_path" ]] && path+=("$fallback_path")
      done
      unset fallback_path
    '';
    profileExtra = builtins.readFile ./.config/zsh/.zprofile;
    initContent = lib.mkAfter (builtins.readFile ./.config/zsh/.zshrc);
  };
}
