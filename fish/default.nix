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
          rev = "85cadd56f71b11574566dbd6c32e0027e361d085";
          hash = "sha256-SI30Md3OKCN5qZ3pT/ZcweB8njGZ9okJFnCLqmfmL44=";
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
      #{
      #  name = "sponge";
      #  src = pkgs.fetchFromGitHub {
      #    owner = "meaningful-ooo";
      #    repo = "sponge";
      #    rev = "384299545104d5256648cee9d8b117aaa9a6d7be";
      #    hash = "sha256-MdcZUDRtNJdiyo2l9o5ma7nAX84xEJbGFhAVhK+Zm1w=";
      #  };
      #}
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
          rev = "26a50962bc68f5cb60fc488ee008b3d4d5be75f4";
          hash = "sha256-4+58sbZf852HImPqWmlJUtuZI0464nx+SyvZbrtsG+E=";
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
