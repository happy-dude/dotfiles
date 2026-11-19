# Credential files and directories the editors must not copy.
#
# Vim and Neovim keep backup, swap, and persistent-undo files, and Emacs keeps
# backups, auto-save files, and undo-tree history. Each can hold a file's
# contents in plain text, outside the directory the secret lives in, so none
# is written for these paths. vim/default.nix passes this set to
# vim/.vim/vimrc_dir/cache.vim as g:dotfiles_sensitive_paths, and
# emacs/default.nix generates sensitive-paths.el for emacs/init.el; the
# editor-secret-state and emacs-runtime checks exercise every entry.
#
# A file entry names one file; a directory entry covers everything below it.
# Both editors also match the path an entry resolves to and the path an opened
# name resolves to, so a symlink on either side is covered.
#
# Where an entry is matched depends on what it holds:
#
# - anyDepthDirectories hold credentials wherever they are found: in another
#   user's home, on a mounted backup, or copied into a project. A path is
#   sensitive when one of these names is a directory anywhere in it.
#   Both editors also resolve its current home instance: if ~/.ssh is a
#   symlink to ~/private/keys, opening ~/private/keys/id directly is sensitive.
# - home, xdgConfig and xdgData entries are an application's state, and the
#   same names are ordinary elsewhere. A project's own .claude, .config/gh
#   or .local/share/opencode/auth.json keeps its state copies.
#
# Add the state directory of any tool that stores a token or key in plain
# text. A file Home Manager links from the store needs no entry.
{
  # Directory names, matched at any depth of a path.
  anyDepthDirectories = [
    ".aws"
    ".gnupg"
    ".kube"
    ".password-store"
    ".ssh"
  ];

  # Relative to the home directory.
  home = {
    files = [
      ".authinfo"
      ".authinfo.gpg"
      ".claude.json"
      ".netrc"
    ];
    directories = [
      ".claude"
      ".codex"
      ".omp"
    ];
  };

  # Relative to the XDG config directory: ~/.config, and $XDG_CONFIG_HOME as
  # well when it is an absolute path. Emacs matches a remote name against
  # .config alone, because finding the remote directory would take a
  # connection.
  xdgConfig = {
    files = [
      "fish/secrets.fish"
      "git/local.config"
      "nix/local.conf"
    ];
    directories = [
      "gh"
      "glab-cli"
      "op"
      "opencode"
      "rclone"
    ];
  };

  # The stable OpenCode CLI reads auth.json into the credential table without
  # removing the old file. SQLite's database and sidecars stay private too.
  xdgData = {
    files = [
      "opencode/auth.json"
      "opencode/opencode.db"
      "opencode/opencode.db-journal"
      "opencode/opencode.db-shm"
      "opencode/opencode.db-wal"
    ];
    directories = [];
  };
}
