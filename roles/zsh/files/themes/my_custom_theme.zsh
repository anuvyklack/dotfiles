# My custom prompt

autoload -U colors && colors

git_prompt()
{
  temp=`git symbolic-ref HEAD 2>/dev/null | cut -d / -f 3`
  if [ "$temp" != "" ]; then echo " ($temp)"; fi
}

setopt prompt_subst

# autoload -Uz vcs_info
# precmd_vcs_info() { vcs_info }
# precmd_functions+=( precmd_vcs_info )
# zstyle ':vcs_info:git:*' formats '%b'


# %n  Имя пользователя
# %m  Имя компьютера (до первой точки)
# %M  Полное имя компьютера
# %~  Путь к текущему каталогу относительно домашнего
# %d  Полный путь к текущей директории
# %T  Время в формате HH:MM
# %*  Время в формате HH:MM:SS
# %D  Дата в формате YY-MM-DD
# %B, %b  Начало и конец выделения жирным
# %U, %u  Начало и конец подчеркивания
# red green yellow blue magenta cyan black white

# RPROMPT=\$vcs_info_msg_0_
# PROMPT=\$vcs_info_msg_0_'%# '

PROMPT=$'\n %~$(git_prompt) $ '
# "правостороннее" приглашение
RPROMPT=' %T '  # показывает время

