setlocal nosmartindent " Turn off for python, because it creates a problem
                       " with unable to indent the lines with "#" symbol at
                       " the beginning.
setlocal textwidth=80
set colorcolumn=75,+1  " Показывать ограничительную линию в 75 колонке (для
                       " комментариев) и в следующей колонке после textwidth.

setlocal foldmethod=expr  " Use treesitter base folding.
