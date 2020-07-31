#!/usr/bin/env zsh

local brewapps=(
    bat
    exa
    # lsd
    # lf
    # fd
    # neovim
    # node
)

# Install Homebrew

# Check we have correct dependencies installed for brew
echo -e "\e[1;37mInstalling \e[1;33mHomebrew \e[1;37mdependencies\e[0m"
sudo apt-get install -y -q build-essential curl file git

# Install Homebrew and it's packages
if [[ ! -d "/home/linuxbrew/.linuxbrew" ]]
then
    echo -e "\e[1;37mInstalling \e[1;33mHomebrew \e[0m"
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
else
    echo -e "\e[1;33mHomebrew \e[1;37malready installed \e[0m"
fi
eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
echo ''

echo -e "\e[1;37mInstalling \e[1;33mHomebrew \e[37;1mpackages: \e[0m"

brewinstall() # {{{
{
    # Check if installed and install using `brew` otherwise.
    if $(brew list $1 > /dev/null 2>&1)
    then
        echo -e "\e[32;1m$1 \e[37;1malready installed \e[0m"
    else
        echo ''
        echo -e "\e[33;1mbrew \e[37;1minstall \e[32;1m$1\e[0m"
        brew install $1
        echo ''
    fi
}
# }}}

for APP in $brewapps; do brewinstall $APP; done


# # Installing universal-ctags {{{
# if $(brew list universal-ctags > /dev/null 2>&1)
# then
#     echo -e "\e[32;1muniversal-ctags \e[37;1malready installed \e[0m"
# else
#     echo -e "\e[33;1mbrew \e[37;1minstall \e[32;1muniversal-ctags \e[0m"
#     brew install --HEAD universal-ctags/universal-ctags/universal-ctags
# fi
# # }}}
