#!/usr/bin/env bash

# install-firefox-userchrome.sh replaces a file inside a live Firefox profile,
# so every case runs against a disposable fixture profile and checks that a
# replaced or refused file survives unchanged.

set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd -- "$SCRIPT_DIR/.." && pwd)"
INSTALL="$REPO_DIR/scripts/install-firefox-userchrome.sh"
SOURCE_CSS="$REPO_DIR/other/firefox/chrome/userChrome.css"

# shellcheck disable=SC1091 # Sourced from the repository.
source "$REPO_DIR/scripts/lib/test-helpers.sh"
test_setup
export HOME="$TMPDIR_TEST/home"
export XDG_CONFIG_HOME="$TMPDIR_TEST/xdg"

make_profile() {
  mkdir -p -- "$1"
  printf '%s\n' '[Compatibility]' 'LastVersion=162.0a1_20261113085116/20261113085116' \
    'LastPlatformDir=/usr/lib/firefox-nightly' >"$1/compatibility.ini"
}

count_backups() {
  local backups=("$1"/chrome/userChrome.css.backup-*)
  if [[ -e ${backups[0]} ]]; then
    printf '%s\n' "${#backups[@]}"
  else
    printf '0\n'
  fi
}

# Print a stable fingerprint of every file under a directory.
snapshot() {
  (cd -- "$1" && find . -type f -print0 | LC_ALL=C sort -z |
    xargs -0 -r sha256sum)
}

# Run the installer expecting failure and require the expected explanation.
assert_refuses() {
  local description=$1
  local expected=$2
  shift 2
  local output status=0

  output="$(bash "$INSTALL" "$@" 2>&1)" || status=$?
  ((status != 0)) || fail "$description: expected a non-zero exit"
  grep -F -- "$expected" <<<"$output" >/dev/null ||
    fail "$description: expected '$expected', got: $output"
}

# Install with only the given prefs files and report whether the loading
# warning appeared.
stylesheet_warning() {
  local profile=$1
  local prefs_js=$2
  local user_js=$3
  local output

  rm -f -- "$profile/prefs.js" "$profile/user.js" "$profile/chrome/userChrome.css"
  [[ -z $prefs_js ]] || printf '%s\n' "$prefs_js" >"$profile/prefs.js"
  [[ -z $user_js ]] || printf '%s\n' "$user_js" >"$profile/user.js"
  output="$(bash "$INSTALL" "$profile" 2>&1)"
  if grep -F -- 'warning:' <<<"$output" >/dev/null; then
    printf 'warned\n'
  else
    printf 'quiet\n'
  fi
}

help_output="$(bash "$INSTALL" --help)"
grep -F -- 'Usage:' <<<"$help_output" >/dev/null ||
  fail 'help output does not describe usage'

assert_refuses 'no arguments' 'Usage:'
assert_refuses 'unknown option' 'Usage:' --bogus
assert_refuses 'two profiles' 'Usage:' "$TMPDIR_TEST" "$TMPDIR_TEST"
assert_refuses 'non-profile directory' 'not a Firefox profile directory' \
  "$TMPDIR_TEST"
assert_refuses 'empty profile list' 'no Firefox profiles found' --list

# Both roots are listed, and a missing version reads as unknown.
profile="$HOME/.mozilla/firefox/abcd1234.default-nightly"
make_profile "$profile"
xdg_profile="$XDG_CONFIG_HOME/mozilla/firefox/efgh5678.default"
mkdir -p -- "$xdg_profile"
printf '[Compatibility]\n' >"$xdg_profile/compatibility.ini"
list_output="$(bash "$INSTALL" --list)"
expected_list="$profile"$'\t'162.0a1_20261113085116/20261113085116$'\t'/usr/lib/firefox-nightly
expected_list+=$'\n'"$xdg_profile"$'\t'unknown$'\t'unknown
[[ $list_output == "$expected_list" ]] ||
  fail "profile list is wrong: $list_output"

# A fresh profile gets a 0644 stylesheet, no backup, and the pref warnings.
fresh_output="$(bash "$INSTALL" "$profile" 2>&1)"
cmp -s -- "$SOURCE_CSS" "$profile/chrome/userChrome.css" ||
  fail 'fresh install did not copy the stylesheet'
[[ $(stat -c %a "$profile/chrome/userChrome.css") == 644 ]] ||
  fail 'installed stylesheet is not mode 0644'
[[ $(count_backups "$profile") == 0 ]] || fail 'fresh install created a backup'
grep -F -- 'toolkit.legacyUserProfileCustomizations.stylesheets' \
  <<<"$fresh_output" >/dev/null || fail "missing pref not reported: $fresh_output"
grep -F -- 'note: the stylesheet expects' <<<"$fresh_output" >/dev/null ||
  fail "missing companion pref not noted: $fresh_output"
grep -F -- 'Restart Firefox' <<<"$fresh_output" >/dev/null ||
  fail "fresh install did not ask for a restart: $fresh_output"
[[ -z $(find "$profile/chrome" -name '.userChrome.css.*') ]] ||
  fail 'staged copy was left behind'

# Running again changes nothing and asks for no restart.
rerun_output="$(bash "$INSTALL" "$profile" 2>&1)"
grep -F -- 'Already installed' <<<"$rerun_output" >/dev/null ||
  fail "rerun did not report the existing install: $rerun_output"
if grep -F -- 'Restart Firefox' <<<"$rerun_output" >/dev/null; then
  fail "rerun asked for a restart: $rerun_output"
fi
[[ $(count_backups "$profile") == 0 ]] || fail 'rerun created a backup'

# Which pref assignment counts: prefs.js alone, user.js over prefs.js, the
# last assignment in a file, and assignments hidden by // or /* */ comments.
pref=toolkit.legacyUserProfileCustomizations.stylesheets
on="user_pref(\"$pref\", true);"
off="user_pref(\"$pref\", false);"
assert_pref_warning() {
  local description=$1
  local expected=$2
  local observed

  observed=$(stylesheet_warning "$profile" "$3" "$4")
  [[ $observed == "$expected" ]] ||
    fail "$description: expected $expected, got $observed"
}
assert_pref_warning 'prefs.js only' quiet "$on" ''
assert_pref_warning 'user.js overrides prefs.js' warned "$on" "$off"
assert_pref_warning 'last assignment wins' quiet "$off"$'\n'"$on" ''
assert_pref_warning 'line comment ignored' warned "// $on" ''
assert_pref_warning 'trailing line comment ignored' warned "$off // $on" ''
assert_pref_warning 'block comment ignored' warned \
  "$off"$'\n/*\n'"$on"$'\n*/' ''
assert_pref_warning 'one-line block comment ignored' quiet \
  "/* x */ $on"$'\n'"/* $off */" ''

# A different file survives as the only backup, the undo command restores
# it, and the rest of chrome/ is untouched.
printf '/* lepton */\n' >"$profile/chrome/userChrome.css"
mkdir -p -- "$profile/chrome/css"
printf 'lepton\n' >"$profile/chrome/css/leptonChrome.css"
printf 'content\n' >"$profile/chrome/userContent.css"
siblings_before=$(
  snapshot "$profile/chrome/css"
  sha256sum <"$profile/chrome/userContent.css"
)
replace_output="$(bash "$INSTALL" "$profile" 2>&1)"
cmp -s -- "$SOURCE_CSS" "$profile/chrome/userChrome.css" ||
  fail 'replacement did not copy the stylesheet'
[[ $(count_backups "$profile") == 1 ]] || fail 'replacement did not keep one backup'
backup=("$profile"/chrome/userChrome.css.backup-*)
[[ $(<"${backup[0]}") == '/* lepton */' ]] || fail 'backup lost the previous file'
siblings_after=$(
  snapshot "$profile/chrome/css"
  sha256sum <"$profile/chrome/userContent.css"
)
[[ $siblings_after == "$siblings_before" ]] || fail 'other chrome/ files changed'
undo_command=$(sed -n 's/^Undo with: *//p' <<<"$replace_output")
[[ -n $undo_command ]] || fail "replacement printed no undo command: $replace_output"
eval "$undo_command"
[[ $(<"$profile/chrome/userChrome.css") == '/* lepton */' ]] ||
  fail 'printed undo command did not restore the previous file'

# A checkout that tracks userChrome.css is pointed at git to restore it.
git -C "$profile/chrome" init -q
git -C "$profile/chrome" add userChrome.css
git_output="$(bash "$INSTALL" "$profile" 2>&1)"
grep -F -- 'checkout -- userChrome.css' <<<"$git_output" >/dev/null ||
  fail "tracked userChrome.css was not reported: $git_output"
rm -rf -- "$profile/chrome/.git"

# A backup name that already exists is refused and both files survive.
printf '/* other */\n' >"$profile/chrome/userChrome.css"
fake_bin="$TMPDIR_TEST/fake-bin"
mkdir -p -- "$fake_bin"
printf '#!/bin/sh\necho 20000101-000000\n' >"$fake_bin/date"
chmod +x -- "$fake_bin/date"
taken="$profile/chrome/userChrome.css.backup-20000101-000000"
printf 'earlier backup\n' >"$taken"
PATH="$fake_bin:$PATH" assert_refuses 'existing backup name' \
  'could not create backup' "$profile"
[[ $(<"$taken") == 'earlier backup' ]] || fail 'existing backup was overwritten'
[[ $(<"$profile/chrome/userChrome.css") == '/* other */' ]] ||
  fail 'refused install replaced the stylesheet'
[[ -z $(find "$profile/chrome" -name '.userChrome.css.*') ]] ||
  fail 'refused install left a staged copy'

# A missing tracked stylesheet is refused before chrome/ is touched.
broken_repo="$TMPDIR_TEST/broken-repo"
mkdir -p -- "$broken_repo/scripts"
cp -- "$INSTALL" "$broken_repo/scripts/"
chrome_before=$(snapshot "$profile/chrome")
INSTALL="$broken_repo/scripts/install-firefox-userchrome.sh" \
  assert_refuses 'missing tracked stylesheet' 'tracked stylesheet not found' \
  "$profile"
[[ $(snapshot "$profile/chrome") == "$chrome_before" ]] ||
  fail 'refusal without a tracked stylesheet changed chrome/'

# A failure after staging removes the staged copy and leaves chrome/ as it was,
# with no backup of the untouched file.
printf '#!/bin/sh\nexit 1\n' >"$fake_bin/chmod"
chmod +x -- "$fake_bin/chmod"
status=0
PATH="$fake_bin:$PATH" bash "$INSTALL" "$profile" >/dev/null 2>&1 || status=$?
((status != 0)) || fail 'install with a failing chmod succeeded'
[[ $(snapshot "$profile/chrome") == "$chrome_before" ]] ||
  fail 'failed staging changed chrome/'
rm -f -- "$fake_bin/chmod"

# A symlink is refused even when its target already matches.
other_css="$TMPDIR_TEST/other.css"
link_profile="$TMPDIR_TEST/link-profile"
make_profile "$link_profile"
mkdir -p -- "$link_profile/chrome"
for target in different identical dangling; do
  rm -f -- "$link_profile/chrome/userChrome.css" "$other_css"
  case $target in
  different) printf '/* other */\n' >"$other_css" ;;
  identical) cp -- "$SOURCE_CSS" "$other_css" ;;
  dangling) ;;
  esac
  ln -s -- "$other_css" "$link_profile/chrome/userChrome.css"
  assert_refuses "$target symlink" 'refusing to replace symlink' "$link_profile"
  [[ -L $link_profile/chrome/userChrome.css ]] || fail "$target symlink was replaced"
  [[ $(count_backups "$link_profile") == 0 ]] || fail "$target symlink got a backup"
done

printf 'install-firefox-userchrome: all cases passed\n'
