#!/usr/bin/env zsh

source color_output.sh

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
echo -e "${WHITE}Installing ${LYELLOW}Homebrew ${WHITE}dependencies${RESET}"
sudo apt-get install -y -q build-essential curl file git

# Install Homebrew and it's packages
if [[ ! -d "/home/linuxbrew/.linuxbrew" ]]
then
    echo -e "${WHITE}Installing ${LYELLOW}Homebrew${RESET}"
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
else
    echo -e "${LYELLOW}Homebrew ${WHITE}already installed${RESET}"
fi
eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
echo ''

echo -e "${WHITE}Installing ${LYELLOW}Homebrew ${WHITE}packages:${RESET}"

brewinstall() # {{{
{
    # Check if installed and install using `brew` otherwise.
    if $(brew list $1 > /dev/null 2>&1)
    then
        echo -e "${LGREEN} $1 ${WHITE}already installed${RESET}"
    else
        echo ''
        echo -e "${LYELLOW}brew ${WHITE}install ${LGREEN} $1 ${RESET}"
        brew install $1
        echo ''
    fi
}
# }}}

for APP in $brewapps; do brewinstall $APP; done
