{
  pkgs,
  self,
  treefmt-nix,
}: let
  sortGitmodules =
    pkgs.writers.writePython3Bin
    "sort-gitmodules"
    {}
    (builtins.readFile ./scripts/sort_gitmodules.py);
  sortGitmodulesTest =
    pkgs.runCommand
    "sort-gitmodules-test"
    {nativeBuildInputs = [sortGitmodules];}
    ''
      printf '%s\n' \
        '[submodule "zeta"]' \
        $'\tpath = modules/zeta' \
        $'\turl = https://example.invalid/zeta' \
        '[submodule "alpha"]' \
        $'\tpath = modules/alpha' \
        $'\turl = https://example.invalid/alpha' \
        >.gitmodules

      sort-gitmodules .gitmodules
      mapfile -t sections < <(grep '^\[submodule' .gitmodules)
      [[ ''${sections[0]} == '[submodule "alpha"]' ]]
      [[ ''${sections[1]} == '[submodule "zeta"]' ]]
      before=$(sha256sum .gitmodules)
      sort-gitmodules .gitmodules
      after=$(sha256sum .gitmodules)
      [[ $before == "$after" ]]
      touch "$out"
    '';
  treefmtEval = treefmt-nix.lib.evalModule pkgs {
    projectRootFile = "flake.nix";
    enableDefaultExcludes = false;
    programs = {
      clang-format = {
        enable = true;
        includes = [
          "*.c"
          "*.cc"
          "*.cpp"
          "*.cxx"
          "*.h"
          "*.hh"
          "*.hpp"
          "*.hxx"
        ];
      };
      alejandra.enable = true;
      fish_indent = {
        enable = true;
        includes = [
          "*.fish"
          "*.fish.example"
        ];
      };
      shfmt = {
        enable = true;
        useEditorConfig = true;
      };
      stylua.enable = true;
      prettier = {
        enable = true;
        settings.proseWrap = "always";
      };
      ruff-format.enable = true;
      taplo.enable = true;
    };
    settings.excludes = [
      "agents/prompts/kagi-*.md" # fixed instruction budget; preserve whitespace
      "other/**" # non-managed reference configs
      "karabiner/**" # macOS + generated backups
      "rime/**/*.yaml" # input-method schemas and dictionaries (data, not code)
      "*.patch"
      "package-lock.json"
      "go.mod"
      "go.sum"
      ".gitattributes"
      ".gitignore"
      ".hgignore"
      ".svnignore"
      "*.lock"
      "LICENSE"
    ];
    settings.formatter.gitmodules = {
      command = pkgs.lib.getExe sortGitmodules;
      includes = [".gitmodules"];
    };
  };
in {
  formatter = treefmtEval.config.build.wrapper;
  checks = {
    formatting = treefmtEval.config.build.check self;
    gitmodules-format = sortGitmodulesTest;
  };
}
