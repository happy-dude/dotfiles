{ config, pkgs, ... }:

{
  home.file.".p10k.zsh".source = ./.p10k.zsh;
  home.file.".zlogin".source = ./.zlogin;
  home.file.".zlogout".source = ./.zlogout;
  home.file.".zpreztorc".source = ./.zpreztorc;
  home.file.".zprofile".source = ./.zprofile;
  home.file.".zshenv".source = ./.zshenv;
  home.file.".zshrc".source = ./.zshrc;

  # for some reason, home-manager errors
  #… while adding path '/nix/store/...-source/zsh/.zprezto'
  # error: path '/nix/store/...-source/zsh/.zprezto' does not exist
  home.file.".zprezto".source = ./.zprezto;

  programs.zsh = {
    enable = true;
  };

}
