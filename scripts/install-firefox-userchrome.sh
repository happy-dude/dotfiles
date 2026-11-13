#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

repo_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
readonly SOURCE_CSS="$repo_dir/other/firefox/chrome/userChrome.css"

# Every refusal must stop the script before it writes, whatever the call
# context, so this die exits rather than returning like scripts/lib/report.sh.
die() {
  printf 'error: %s\n' "$*" >&2
  exit 1
}

# The staged copy is removed on any exit until it has been renamed into place.
staged_css=""
trap '[[ -z $staged_css ]] || rm -f -- "$staged_css"' EXIT

usage() {
  cat <<'EOF'
Usage: install-firefox-userchrome.sh <profile-directory>
       install-firefox-userchrome.sh --list

Run from a checkout of the dotfiles repository. Copy
other/firefox/chrome/userChrome.css to <profile-directory>/chrome/. A different
existing userChrome.css is kept beside it as userChrome.css.backup-<timestamp>;
move that file back to undo. No other profile file changes.

A Lepton (Firefox-UI-Fix) checkout in chrome/ tracks its own userChrome.css, so
installing leaves that checkout with a modified file and an untracked backup;
`git -C <profile-directory>/chrome checkout -- userChrome.css` also restores
Lepton's copy.

--list prints each profile under ~/.mozilla/firefox and
$XDG_CONFIG_HOME/mozilla/firefox with the version and installation directory
of the Firefox that last ran it.
EOF
}

list_profiles() {
  local root compat version platform found=0

  for root in "$HOME/.mozilla/firefox" \
    "${XDG_CONFIG_HOME:-$HOME/.config}/mozilla/firefox"; do
    for compat in "$root"/*/compatibility.ini; do
      [[ -f $compat ]] || continue
      version=$(sed -n 's/^LastVersion=//p' "$compat")
      platform=$(sed -n 's/^LastPlatformDir=//p' "$compat")
      printf '%s\t%s\t%s\n' "${compat%/compatibility.ini}" \
        "${version:-unknown}" "${platform:-unknown}"
      found=1
    done
  done
  ((found)) || die "no Firefox profiles found"
}

# Remove /* */ and // comments from pref files on stdin, keeping line breaks.
# Firefox's pref parser skips both forms. A "//" inside a string value, such
# as a URL, is cut too; only the boolean prefs below are read, so that is safe.
strip_pref_comments() {
  awk '{
    out = ""
    while (length($0) > 0) {
      if (in_block) {
        i = index($0, "*/")
        if (i == 0) { $0 = ""; break }
        $0 = substr($0, i + 2); in_block = 0
      } else {
        b = index($0, "/*"); l = index($0, "//")
        if (l && (!b || l < b)) { out = out substr($0, 1, l - 1); break }
        if (!b) { out = out $0; break }
        out = out substr($0, 1, b - 1); $0 = substr($0, b + 2); in_block = 1
      }
    }
    print out
  }'
}

# Print the value a pref takes at Firefox's next start, or nothing when
# neither file sets it. Firefox reads user.js after prefs.js, and within a file
# the last assignment wins.
pref_value() {
  local profile=$1
  local pattern="^[[:space:]]*user_pref\(\"${2//./\\.}\","
  local file line value=""

  for file in "$profile/prefs.js" "$profile/user.js"; do
    [[ -f $file ]] || continue
    line=$(strip_pref_comments <"$file" | grep -E -- "$pattern" | tail -n 1) ||
      true
    [[ -z $line ]] ||
      value=$(sed -E 's/^[^,]*,[[:space:]]*//; s/\);.*$//' <<<"$line")
  done
  printf '%s' "$value"
}

report_prefs() {
  local profile=$1

  [[ $(pref_value "$profile" toolkit.legacyUserProfileCustomizations.stylesheets) == true ]] ||
    printf '%s\n' "warning: Firefox loads userChrome.css only when" \
      "  toolkit.legacyUserProfileCustomizations.stylesheets = true" >&2
  [[ $(pref_value "$profile" browser.urlbar.scotchBonnet.disableOneOffs) == true ]] ||
    printf '%s\n' "note: the stylesheet expects" \
      "  browser.urlbar.scotchBonnet.disableOneOffs = true" >&2
}

# Replace <profile>/chrome/userChrome.css with one rename in the same
# directory, so a failure leaves the previous file in place. The new copy is
# staged before the backup is taken, so a failed copy leaves chrome/ as it
# was; the backup name is created exclusively and is never overwritten.
install_into() {
  local profile=$1
  local chrome=$profile/chrome
  local dest=$chrome/userChrome.css
  local backup="" tracked=""

  [[ -f $SOURCE_CSS ]] || die "tracked stylesheet not found: $SOURCE_CSS"
  [[ -d $profile && -f $profile/compatibility.ini ]] ||
    die "not a Firefox profile directory (no compatibility.ini): $profile"
  [[ ! -L $dest ]] ||
    die "refusing to replace symlink: $dest -> $(readlink -- "$dest")"

  if [[ -e $dest ]] && cmp -s -- "$SOURCE_CSS" "$dest"; then
    printf 'Already installed: %s\n' "$dest"
    report_prefs "$profile"
    return
  fi

  mkdir -p -- "$chrome"
  staged_css=$(mktemp -- "$chrome/.userChrome.css.XXXXXX")
  cp -- "$SOURCE_CSS" "$staged_css"
  chmod 0644 -- "$staged_css"

  if [[ -e $dest ]]; then
    backup=$dest.backup-$(date +%Y%m%d-%H%M%S)
    (set -o noclobber && cat -- "$dest" >"$backup") 2>/dev/null ||
      die "could not create backup (it may already exist): $backup"
    touch -r "$dest" -- "$backup"
    git -C "$chrome" ls-files --error-unmatch -- userChrome.css \
      >/dev/null 2>&1 && tracked=1
  fi
  mv -f -- "$staged_css" "$dest"
  staged_css=""

  printf 'Installed: %s\n' "$dest"
  if [[ -n $backup ]]; then
    printf 'Previous file: %s\n' "$backup"
    printf 'Undo with:     mv -f -- %q %q\n' "$backup" "$dest"
  fi
  if [[ -n $tracked ]]; then
    printf '%s\n' "note: $chrome is a git checkout that tracks userChrome.css;" \
      "  git -C $(printf '%q' "$chrome") checkout -- userChrome.css restores its copy" >&2
  fi
  report_prefs "$profile"
  printf 'Restart Firefox to load the stylesheet.\n'
}

case ${1-} in
-h | --help)
  usage
  ;;
--list)
  (($# == 1)) || {
    usage >&2
    exit 1
  }
  list_profiles
  ;;
"" | -*)
  usage >&2
  exit 1
  ;;
*)
  (($# == 1)) || {
    usage >&2
    exit 1
  }
  install_into "$1"
  ;;
esac
