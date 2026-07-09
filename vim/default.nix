{ pkgs, ... }:

let
  nvimTreesitter = pkgs.vimPlugins.nvim-treesitter;
  treesitterRuntime = pkgs.symlinkJoin {
    name = "nvim-treesitter-runtime";
    paths = nvimTreesitter.withAllGrammars.passthru.dependencies;
  };
in
{
  home.file = {
    ".local/share/nvim/site/parser".source = "${treesitterRuntime}/parser";
    ".local/share/nvim/site/queries".source = "${treesitterRuntime}/queries";
  };
}
