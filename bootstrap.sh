#!/usr/bin/env bash
set -e

tags="$1"

if [ -z $tags ]; then
  tags="all"
fi

# The home dir of current script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# # if command -v conda && conda env list | grep -q ansible; then
# if command -v conda >/dev/null 2>&1 \
#    && \
#    conda env list | grep -q ansible
# then
#   conda activate ansible
# fi

# -x - True if file exists and is executable.
if ! [ -x "$(command -v ansible)" ]; then
  sudo apt-get install ansible
  ansible-galaxy collection install community.general
fi

ansible-playbook -i $DIR/hosts $DIR/dotfiles.yml --ask-become-pass --tags $tags
