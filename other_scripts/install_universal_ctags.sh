#!/usr/bin/env bash

# Installing universal-ctags
if $(brew list universal-ctags > /dev/null 2>&1)
then
    echo -e "${LGREEN}universal-ctags ${WHITE}already installed${RESET}"
else
    echo -e "${LYELLOW}brew ${WHITE}install ${LGREEN}universal-ctags${RESET}"
    brew install --HEAD universal-ctags/universal-ctags/universal-ctags
fi
