Configuration Files
===================

Zsh has several system-wide and user-local configuration files.

System-wide configuration files are by default in `/etc`.

User-local configuration files have the same name as their global counterparts
but are prefixed with a dot (hidden).  Zsh looks for these files in the path
stored in the `$ZDOTDIR` environment variable.  If said variable is not defined,
Zsh will use the user's home directory.

File Descriptions
-----------------

The configuration files are read in the following order:

  01. /etc/zshenv
  02. ~/.zshenv
  03. /etc/zprofile
  04. ~/.zprofile
  05. /etc/zshrc
  06. ~/.zshrc
  08. /etc/zlogin
  09. ~/.zlogin
  10. ~/.zlogout
  11. /etc/zlogout


### .zshenv

*Read every time*

This file is always sourced, so it should set environment variables which need
to be updated frequently.  `$PATH` (or its associated counterpart path) is
a good example because you probably don't want to restart your whole session to
make it update.  By setting it in that file, reopening a terminal emulator will
start a new Zsh instance with the PATH value updated.

But be aware that this file is read even when Zsh is launched to run a single
command (with the -c option), even by another tool like make, and thus, it
should be kept as small as possible.  You should be very careful to not modify
the default behavior of standard commands because it may break some tools (by
setting aliases for example).


### .zprofile

*Read at login*

This file was added for [KornShell][1] fans.  You may treat that file like
`.zshenv` but for commands and variables which should be set once or which don't
need to be updated frequently:

* environment variables to configure tools (flags for compilation, data folder
  location, etc.)
* configuration which execute commands
  (like `SCONSFLAGS="--jobs=$(( $(nproc) 1 ))"`) as it may take some time to
  execute.

If you modify this file, you can apply the configuration updates by running
a login shell:

    exec zsh --login

### .zshrc

*Read when interactive*

It is a good practise to put here everything needed only for interactive usage:

* prompt,
* command completion,
* command correction,
* command suggestion,
* command highlighting,
* output coloring,
* aliases,
* key bindings,
* commands history management,
* other miscellaneous interactive tools (auto_cd, manydots-magic)...

### .zlogin

*Read at login*

This file is like `.zprofile`, but is read after `.zshrc`.  You can consider the
shell to be fully set up at `.zlogin` execution time.  This is not the file to
define aliases, functions, shell options, and key bindings.  It should not
change the shell environment.  It is usually used for messages such as
[fortune][2], [msgs][3], or for the creation of files.

So, I use it to launch external commands which do not modify shell behaviors
(e.g. a login manager).

### .zlogout

*Read at logout, Within login shell*

Here, you can clear your terminal or any other resource which was setup at
login.


## How I choose where to put a setting

* if it is needed by a command run non-interactively: `.zshenv`
* if it should be updated on each new shell: `.zshenv`
* if it runs a command which may take some time to complete: `.zprofile`
* if it is related to interactive usage: `.zshrc`
* if it is a command to be run when the shell is fully setup: `.zlogin`
* if it releases a resource acquired at login: `.zlogout`


Some zsh syntax
===============

autoload -Uz function_name
:   **-U** - prevents alias from being expanded. That is, whenever you define an
    alias and a function having the same name, the alias will be considered first
    instead, so -U just skips alias expansion.

    **-z** - indicates that the function will be auto-loaded using zsh or
    ksh style.


[1]: http://www.kornshell.com
[2]: http://en.wikipedia.org/wiki/Fortune_(Unix)
[3]: http://www.manpagez.com/man/1/msgs

