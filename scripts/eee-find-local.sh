#!/usr/bin/env bash
set -Eeuo pipefail

INITIAL_QUERY="${1:-}"

FD="$(command -v fd || command -v fdfind || true)"
FZF="$(command -v fzf || true)"
BAT="$(command -v bat || command -v batcat || true)"

if [[ -z "$FD" ]]; then
  echo "error: tool 'fd' not installed" >&2
  exit 1
fi

if [[ -z "$FZF" ]]; then
  echo "error: tool 'fzf' not installed" >&2
  exit 1
fi

preview_cmd='if [[ -n "'$BAT'" ]]; then "'$BAT'" -n --color=always -- {}; else cat -- {}; fi'

selection="$(
  "$FD" --type f --type l --hidden --exclude .git --exclude target |
    "$FZF" \
      --query "$INITIAL_QUERY" \
      --border \
      --layout reverse \
      --info=inline-right \
      --exact \
      --input-border \
      --cycle \
      --color "border:#A15ABD" \
      --header-first \
      --header "CWD:$(pwd)" \
      --preview "$preview_cmd" \
      --preview-window 'right,60%,border-bottom,wrap,+{2}+3/3,~3' \
      --bind 'ctrl-/:change-preview-window(down|hidden|)' \
      --bind 'ctrl-f:page-down,ctrl-b:page-up'
)"

if [[ -n "$selection" ]]; then
  realpath -- "$selection"
fi
