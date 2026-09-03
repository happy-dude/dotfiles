{
  pkgs,
  self,
  treefmt-nix,
}: let
  mkCheck = import ./lib/mkCheck.nix {inherit pkgs;};
  sortGitmodules = import ./lib/python/mkScript.nix {inherit pkgs;} {
    name = "sort-gitmodules";
    source = ./scripts/sort_gitmodules.py;
  };
  sortGitmodulesTest = mkCheck {
    name = "sort-gitmodules-test";
    tools = [sortGitmodules];
    script = ''
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

      # A symlinked or non-regular .gitmodules is refused.
      ln -s .gitmodules linked.gitmodules
      if sort-gitmodules linked.gitmodules 2>/dev/null; then
        echo 'accepted a symlinked .gitmodules' >&2
        exit 1
      fi

      # Lines before the first section survive an actual reorder.
      printf '%s\n' '# vendored modules' >preamble.gitmodules
      printf '%s\n' \
        '[submodule "zeta"]' \
        $'\tpath = modules/zeta' \
        '[submodule "alpha"]' \
        $'\tpath = modules/alpha' \
        >>preamble.gitmodules
      sort-gitmodules preamble.gitmodules
      head -1 preamble.gitmodules | grep -qx '# vendored modules'
      grep -m1 '^\[submodule' preamble.gitmodules |
        grep -qx '\[submodule "alpha"\]'

      # A blank line inside a block stays blank rather than becoming a
      # tab-only line.
      printf '%s\n' \
        '[submodule "alpha"]' \
        $'\tpath = modules/alpha' \
        >blank.gitmodules
      printf '\n' >>blank.gitmodules
      printf '%s\n' $'\turl = https://example.invalid/alpha' \
        >>blank.gitmodules
      sort-gitmodules blank.gitmodules
      if grep -qx $'\t' blank.gitmodules; then
        echo 'a blank line became a tab-only line' >&2
        exit 1
      fi
      grep -qx "" blank.gitmodules

      # An empty file is not rewritten.
      touch empty.gitmodules
      sort-gitmodules empty.gitmodules
      test ! -s empty.gitmodules
    '';
  };
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
