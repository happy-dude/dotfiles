#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
export LC_ALL=C.UTF-8

script_path=${BASH_SOURCE[0]}
if [[ $script_path == */* ]]; then
  script_dir=${script_path%/*}
else
  script_dir=.
fi
root=$(cd -- "$script_dir/.." && pwd -P)
source_dir="$root/agents/prompts"
agent_dir="$root/agents/generated/codex-agents"
profile_dir="$root/agents/generated/codex-profiles"
agents=(kernel language)
check=false
stale=()

die() {
  printf '%s\n' "$*" >&2
  exit 1
}

usage() {
  printf 'usage: %s [--check]\n' "${0##*/}"
}

trim() {
  local value=$1
  value=${value#"${value%%[![:space:]]*}"}
  value=${value%"${value##*[![:space:]]}"}
  printf '%s' "$value"
}

json_string() {
  local value=$1
  local result=
  local character escaped
  local codepoint high low
  local i

  for ((i = 0; i < ${#value}; i++)); do
    character=${value:i:1}
    printf -v codepoint '%d' "'$character"
    case $codepoint in
    8) escaped='\b' ;;
    9) escaped='\t' ;;
    10) escaped='\n' ;;
    12) escaped='\f' ;;
    13) escaped='\r' ;;
    34) escaped='\"' ;;
    92) printf -v escaped '%s%s' \\ \\ ;;
    *)
      if ((codepoint >= 0x20 && codepoint <= 0x7e)); then
        escaped=$character
      elif ((codepoint <= 0xffff)); then
        printf -v escaped '%s%04x' '\u' "$codepoint"
      else
        codepoint=$((codepoint - 0x10000))
        high=$((0xd800 + (codepoint >> 10)))
        low=$((0xdc00 + (codepoint & 0x3ff)))
        printf -v escaped '%s%04x%s%04x' '\u' "$high" '\u' "$low"
      fi
      ;;
    esac
    result+=$escaped
  done
  printf '"%s"' "$result"
}

render() {
  local name=$1
  local kind=$2
  local source="$source_dir/$name.md"
  local -a lines=()
  local frontmatter_end=-1
  local first_body=-1
  local last_body=-1
  local metadata_name=
  local description=
  local current_key=
  local line key value
  local i

  mapfile -t lines <"$source"
  ((${#lines[@]} > 0)) || die "empty prompt: $name.md"
  [[ ${lines[0]} == --- ]] || die "missing frontmatter: $name.md"

  for ((i = 1; i < ${#lines[@]}; i++)); do
    if [[ ${lines[i]} == --- ]]; then
      frontmatter_end=$i
      break
    fi
  done
  ((frontmatter_end >= 0)) || die "unterminated frontmatter: $name.md"

  for ((i = 1; i < frontmatter_end; i++)); do
    line=${lines[i]}
    if [[ $line =~ ^(name|description):(.*)$ ]]; then
      key=${BASH_REMATCH[1]}
      value=$(trim "${BASH_REMATCH[2]}")
      current_key=$key
      if [[ $key == name ]]; then
        metadata_name=$value
      else
        description=$value
      fi
    elif [[ $line == [[:space:]]* && -n $current_key ]]; then
      value=$(trim "$line")
      if [[ -n $value ]]; then
        if [[ $current_key == name ]]; then
          metadata_name+="${metadata_name:+ }$value"
        else
          description+="${description:+ }$value"
        fi
      fi
    else
      current_key=
    fi
  done

  [[ -n $metadata_name ]] || die "missing name in frontmatter: $name.md"
  [[ -n $description ]] || die "missing description in frontmatter: $name.md"
  [[ $metadata_name == "$name" ]] ||
    die "frontmatter name does not match filename: $name.md"

  for ((i = frontmatter_end + 1; i < ${#lines[@]}; i++)); do
    if [[ -n ${lines[i]//[[:space:]]/} ]]; then
      ((first_body < 0)) && first_body=$i
      last_body=$i
    fi
  done

  if [[ $kind == agent ]]; then
    printf 'name = %s\n' "$(json_string "$metadata_name")"
    printf 'description = %s\n' "$(json_string "$description")"
  fi
  printf "developer_instructions = '''\n"
  if ((first_body >= 0)); then
    for ((i = first_body; i <= last_body; i++)); do
      line=${lines[i]}
      [[ $line != *"'''"* ]] ||
        die "prompt cannot contain TOML literal delimiter: $name.md"
      printf '%s\n' "$line"
    done
  fi
  printf "'''\n"
  if [[ $kind == profile && $name == kernel ]]; then
    printf 'model_reasoning_effort = "medium"\n'
  fi
}

write_or_check() {
  local name=$1
  local kind=$2
  local output=$3
  local temporary

  temporary=$(mktemp)
  trap 'rm -f -- "$temporary"' RETURN
  render "$name" "$kind" >"$temporary"
  if $check; then
    if [[ ! -f $output ]] || ! cmp -s -- "$temporary" "$output"; then
      stale+=("${output#"$root/"}")
    fi
  else
    mkdir -p -- "${output%/*}"
    mv -- "$temporary" "$output"
  fi
  trap - RETURN
  rm -f -- "$temporary"
}

case ${1-} in
"") ;;
--check) check=true ;;
-h | --help)
  usage
  exit 0
  ;;
*)
  usage >&2
  exit 2
  ;;
esac
(($# <= 1)) || {
  usage >&2
  exit 2
}

for name in "${agents[@]}"; do
  write_or_check "$name" agent "$agent_dir/$name.toml"
  write_or_check "$name" profile "$profile_dir/$name.config.toml"
done

if ((${#stale[@]} > 0)); then
  printf 'stale generated Codex agent files:\n' >&2
  printf '  %s\n' "${stale[@]}" >&2
  printf 'run scripts/generate_codex_agents.sh\n' >&2
  exit 1
fi
