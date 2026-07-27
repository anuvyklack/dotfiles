# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## What this repository is

Personal Fish shell configuration (`~/.config/fish`). Changes here affect
the interactive shell directly — there is no build step or test suite.

## Plugin management

Plugins are managed by **Fisher**. The source of truth is `fish_plugins`.

```fish
fisher update          # update all plugins
fisher install <plug>  # add a plugin (updates fish_plugins automatically)
fisher remove <plug>   # remove a plugin
```

Installed plugins:
- `ilancosman/tide@v6` — prompt (configured via universal variables in
  `fish_variables` and `tide configure`)
- `sshilovsky/fish-helix` — Helix-style modal key bindings
- `patrickf1/fzf.fish` — fzf keybindings and widgets
- `halostatue/fish-chezmoi@v1` — wraps `chezmoi cd` to actually `cd`

## Architecture

| Path | Purpose |
|---|---|
| `config.fish` | Interactive-only settings: syntax-highlighting colors, prompt colors, tool init (`atuin`, `zoxide`), keybindings, aliases/abbrs |
| `conf.d/` | Auto-sourced on every shell start (both interactive and non-interactive) — conda init, chezmoi completions, tide init, fzf bindings |
| `functions/` | Auto-loaded on first call; most files here are from Fisher plugins |
| `completions/` | Tab-completion scripts (also mostly from plugins) |
| `fish_plugins` | Fisher plugin list — edit this then run `fisher update` |
| `fish_variables` | Universal variables set by `set -U`, including all `tide_*` config |
| `themes/` | `.theme` files for `fish_config theme`; `OneDark.theme` is kept as reference only — the live colors are the `set -g fish_color_*` lines in `config.fish` |

**Colors are global, not universal.** Since fish 4.3 the `fish_color_*`,
`fish_pager_color_*` and `fish_key_bindings` variables default to global
scope, so they must be set on every shell start from `config.fish`. Do not
use `fish_config theme choose` / `theme save` to persist a theme — the
former writes `conf.d/fish_frozen_theme.fish` (unmanaged by chezmoi) and
the latter reintroduces universal variables. Edit the `set -g` block in
`config.fish` instead. Variables left unset fall back to fish's built-in
defaults.

## Key design points

**Helix-style modal editing** is provided by `sshilovsky/fish-helix`. The
active key binding function is set as a *global* variable near the top of
the Keybindings section in `config.fish`: `set -g fish_key_bindings
fish_helix_key_bindings`. It must stay ahead of every `bind` in that file —
assigning the variable erases all bindings and re-runs the binding function
from scratch, so anything bound earlier is lost. Cursor shapes are managed
internally by the plugin via `fish_vi_cursor`. Tide reads
`$fish_bind_mode` for its vi-mode indicator and is compatible with
fish-helix's mode values (`insert`, `default`, `visual`, `replace`).

**Tool initializers** in `config.fish` are guarded by `type -q <tool>` so
the config stays portable when a tool is absent.

**`conf.d/` runs unconditionally** (not inside `status is-interactive`).
Interactive-only code belongs in `config.fish` inside the `if status
is-interactive` block, not in `conf.d/`.

**Running fish commands** from a Bash/zsh session (as Claude Code does):
`fish -c "set -U tide_character_color dc5bfc"`. Only *universal* variable
changes made this way persist across interactive shells; global ones
(colors, `fish_key_bindings`) live only for that one command, so to test
them use `fish -i -c '...'`, which sources `config.fish` first.

**Tide prompt** is configured exclusively through universal variables
(`set -U tide_*`). To reconfigure interactively: `tide configure`. Custom
overrides live at the bottom of the `if status is-interactive` block in
`config.fish`.

## bind key syntax

Use named forms (`ctrl-h`, `ctrl-j`, `ctrl-o`) not escape sequences
(`\ch`, `\cj`, `\cO`). Named form is the only documented format per the
Fish docs. For punctuation not in `bind --key-names`, use the literal
character in quotes: `'ctrl-;'` (works), `ctrl-semicolon` (errors).

## Editing tide icon assignments

Lines like `set -g tide_character_icon <unicode>` contain multi-byte
characters that break the Edit tool's string matcher. Use a Python
`str.replace` script via Bash instead.

## Validating changes

Source the changed file directly to test without opening a new shell:

```fish
source ~/.config/fish/config.fish
source ~/.config/fish/conf.d/some-file.fish
```

For functions, Fish reloads them automatically on next call after editing.
To force-reload a running shell: `functions --erase <name>` then call it.
