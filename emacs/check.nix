{
  pkgs,
  self,
}: {
  emacs =
    pkgs.runCommand "dotfiles-emacs-checks"
    {
      nativeBuildInputs = [
        pkgs.emacs-nox
        pkgs.findutils
      ];
    }
    ''
      while IFS= read -r -d ''' file; do
        emacs --batch --quick "$file" --eval '(check-parens)'
      done < <(find ${self} -type f -name '*.el' -print0)

      while IFS= read -r -d ''' file; do
        emacs --batch --quick "$file" \
          --eval "(require 'org-lint)" \
          --eval '(let ((reports (org-lint))) (when reports (error "%s: %S" buffer-file-name reports)))'
      done < <(find ${self} -type f -name '*.org' -print0)

      touch "$out"
    '';
}
