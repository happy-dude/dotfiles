{ config, pkgs, ... }:

{
  #xdg.configFile."fish/config.fish".source = ./.config/fish/config.fish;
  xdg.configFile."fish/completions/nix.fish".source =
    "${pkgs.nix}/share/fish/vendor_completions.d/nix.fish";

  programs.fish = {
    enable = true;

    shellInit = ''
      ${builtins.readFile .config/fish/config.fish}
    '';

    plugins = [
      {
        name = "autopair.fish";
        src = pkgs.fetchFromGitHub {
          owner = "jorgebucaran";
          repo = "autopair.fish";
          rev = "4d1752ff5b39819ab58d7337c69220342e9de0e2";
          hash = "sha256-qt3t1iKRRNuiLWiVoiAYOu+9E7jsyECyIqZJ/oRIT1A=";
        };
      }
      {
        name = "nvm.fish";
        src = pkgs.fetchFromGitHub {
          owner = "jorgebucaran";
          repo = "nvm.fish";
          rev = "cc70373951379cb986a99f059bfc1e9834a3bdd7";
          hash = "sha256-ZY443mWe/J2eSylzgNEJiLvurqE9StWGb0fvGHthqA0=";
        };
      }
      {
        name = "puffer-fish";
        src = pkgs.fetchFromGitHub {
          owner = "nickeb96";
          repo = "puffer-fish";
          rev = "83174b07de60078be79985ef6123d903329622b8";
          hash = "sha256-Dhx5+XRxJvlhdnFyimNxFyFiASrGU4ZwyefsDwtKnSg=";
        };
      }
      {
        name = "spark.fish";
        src = pkgs.fetchFromGitHub {
          owner = "jorgebucaran";
          repo = "spark.fish";
          rev = "90a60573ec8a8ecb741a861e0bfca2362f297e5f";
          hash = "sha256-cRSZeqtXSaEKuHeTSk3Kpmwf98mKJ986x1KSxa/HggU=";
        };
      }
      {
        name = "sponge";
        src = pkgs.fetchFromGitHub {
          owner = "meaningful-ooo";
          repo = "sponge";
          rev = "384299545104d5256648cee9d8b117aaa9a6d7be";
          hash = "sha256-MdcZUDRtNJdiyo2l9o5ma7nAX84xEJbGFhAVhK+Zm1w=";
        };
      }
      {
        name = "tide";
        src = pkgs.fetchFromGitHub {
          owner = "IlanCosman";
          repo = "tide";
          rev = "fcda500d2c2996e25456fb46cd1a5532b3157b16";
          hash = "sha256-dzYEYC1bYP0rWpmz0fmBFwskxWYuKBMTssMELXXz5H0=";
        };
      }
      {
        name = "z";
        src = pkgs.fetchFromGitHub {
          owner = "jethrokuan";
          repo = "z";
          rev = "d2f502f5575b18a32e1bee2f2b3f869a5053c159";
          hash = "sha256-4c9ScQVf55b2ANaR7Lp/oqLeuK+FxH/wKmSNLV+b/CE=";
        };
      }
    ];

  };

  #home.packages = with pkgs; [
  #  fishPlugins.autopair
  #  fishPlugins.puffer
  #  fishPlugins.sponge
  #  fishPlugins.z
  #  fishPlugins.spark
  #  fishPlugins.tide
  #];
}
