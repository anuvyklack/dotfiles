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
| `config.fish` | Interactive-only settings: prompt colors, tool init (`atuin`, `zoxide`), keybindings, aliases/abbrs |
| `conf.d/` | Auto-sourced on every shell start (both interactive and non-interactive) — conda init, chezmoi completions, tide init, fzf bindings |
| `functions/` | Auto-loaded on first call; most files here are from Fisher plugins |
| `completions/` | Tab-completion scripts (also mostly from plugins) |
| `fish_plugins` | Fisher plugin list — edit this then run `fisher update` |
| `fish_variables` | Universal variables set by `set -U`, including all `tide_*` config |
| `themes/` | `.theme` files for `fish_config theme` |

## Key design points

**Helix-style modal editing** is provided by `sshilovsky/fish-helix`. The
active key binding function is stored as a universal variable — to switch
bindings: `set -U fish_key_bindings fish_helix_key_bindings`. Cursor shapes
are managed internally by the plugin via `fish_vi_cursor`. Tide reads
`$fish_bind_mode` for its vi-mode indicator and is compatible with
fish-helix's mode values (`insert`, `default`, `visual`, `replace`).

**Tool initializers** in `config.fish` are guarded by `type -q <tool>` so
the config stays portable when a tool is absent.

**`conf.d/` runs unconditionally** (not inside `status is-interactive`).
Interactive-only code belongs in `config.fish` inside the `if status
is-interactive` block, not in `conf.d/`.

**Running fish commands** from a Bash/zsh session (as Claude Code does):
`fish -c "set -U fish_key_bindings fish_helix_key_bindings"`. Universal
variable changes made this way persist across interactive shells.

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
