{...}: {
  programs.fzf = {
    enable = true;
    defaultCommand = "rg --files --hidden --follow --glob '!.git'";
    enableFishIntegration = true;
    # prezto's editor module resets every keymap with `bindkey -d` when it
    # loads, which would discard the bindings the module installs first;
    # .zshrc sources the fzf integration after prezto instead.
    enableZshIntegration = false;
  };
}
