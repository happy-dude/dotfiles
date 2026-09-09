{...}: {
  programs.fzf = {
    enable = true;
    enableFishIntegration = true;
    # prezto's editor module resets every keymap with `bindkey -d` when it
    # loads, which would discard the bindings the module installs first;
    # .zshrc sources the fzf integration after prezto instead.
    enableZshIntegration = false;
  };
}
