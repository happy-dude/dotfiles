{pkgs, ...}: let
  nvimTreesitter = pkgs.vimPlugins.nvim-treesitter;

  treesitterRuntime = pkgs.symlinkJoin {
    name = "nvim-treesitter-runtime";
    # Query-only languages have no grammar derivation, so include the plugin
    # runtime alongside withAllGrammars dependencies.
    paths = nvimTreesitter.withAllGrammars.passthru.dependencies ++ ["${nvimTreesitter}/runtime"];
  };
in {
  home.file = {
    ".local/share/nvim/site/parser".source = "${treesitterRuntime}/parser";
    ".local/share/nvim/site/queries".source = "${treesitterRuntime}/queries";
  };
}
