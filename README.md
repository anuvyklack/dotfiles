This is mine Ansible driven dotfiles.  I use them on Ubuntu and Fedora. 
To install them, clone this repo and run `./setup` from the root of this repo.

But I don't recommend installing them direct with install script: they are
tailored for me and constantly changing. Instead, you can use them as
inspiration.

## zsh

Environment variables are in the `.zprofile` file. 

Login shell settings are in `.zshrc` file.
I use [zsh-snap](https://github.com/marlonrichert/zsh-snap) plugin mamager with
[powerlevel10k](https://github.com/romkatv/powerlevel10k) prompt theme framework.

In `.zlogin` file is a code, that byte-compiles into a background all zsh
scripts.  All custom zsh functions are compiles into single library file.
Binary files automatically recompiles when source code changes.

### zsh plugins

#### completion

I simluteniously use built-in zsh completion along with [fzf-tab](https://github.com/Aloxaf/fzf-tab)
plugin and switch thwm in place with a `Shift + Tab` keymap (search `completion-switch` in
`zshrc` file).

| compsys | fzf-tab |
| ------- | ------- |
| ![](https://user-images.githubusercontent.com/13056013/194636968-6e924d76-d879-491f-85f4-7a3f1082798f.png) | ![](https://user-images.githubusercontent.com/13056013/194620767-af75162d-73ed-4fd4-8a32-254591563d42.png) |

#### vi mode

For better Vi mode emulating in zsh prompt I use [zsh-vi-mode](https://github.com/jeffreytse/zsh-vi-mode).

#### fzf

I use custom `CTRL-T` command: it returns the selected file with absolute path
relevant to home directory, if the file is inside home directory, and relative
to root in other cases, instead of returning relative path to present directory
as `CTRL-T` does by default.


#### man

The `man` binary is replaced with the [wrapper function](https://github.com/anuvyklack/dotfiles/blob/docs/roles/zsh/files/functions/man).
Along with `MANWIDTH` env variables, wraps manuals content into 80 character
column and center it in a terminal.

<!-- <img src="https://user-images.githubusercontent.com/13056013/194579745-9e4f51d9-6302-4319-898c-2f4708267f49.png" width="400" height="200" /> -->
<img width="400" src="https://user-images.githubusercontent.com/13056013/194579745-9e4f51d9-6302-4319-898c-2f4708267f49.png" />

### Installing CLI tools

Not all packages are present in the Ubuntu repositories, and those that are
present often are outdated and I like to use the lates programs versions. So
I come to a platform independent way:
Most of the CLI utils that I use: fzf, zoxide, exa, rg, etc, I install 
directly from github releases with [bin](https://github.com/marcosnils/bin)
binary manager.
See [install_missing_programs](https://github.com/anuvyklack/dotfiles/blob/main/roles/zsh/files/functions/install_missing_programs)
file.

To install completion files for such programs without clonning the whole repo and
symlink targer files, I download them directly from github repos with `curl`.
See [github_completion](https://github.com/anuvyklack/dotfiles/blob/main/roles/zsh/files/functions/github_completion)
file.

#### tmux

In Ubuntu repos tmux is outdated and I use the floating window feature,
so I make [build-tmux](https://github.com/anuvyklack/dotfiles/blob/main/roles/tmux/tasks/build-tmux.yaml)
task that compiles tmux from source.

## keybindings

For convenience I rebind some keys using
[dual function keys](https://gitlab.com/interception/linux/plugins/dual-function-keys)
(see `dual-function-keys` role):

* `Caps Lock` to `Esc` on tap, and to `Ctrl` when another key pressed  while
  holding `Caps Lock`;

* `Space` to `Space` on tap and to `Ctrl` on hold;

* Left and right `Shift`-s to `(` and `)` on tap correspondingly, and to `Shift`
  on hold.

## neovim

I'm a big fun of Neovim and my Neovim config is a whole big story.

![](https://user-images.githubusercontent.com/13056013/194704141-13227c6e-8a3b-4525-a915-33aaaea29f22.png)

