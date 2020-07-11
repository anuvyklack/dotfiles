#!/usr/bin/env bash
set -e

tags="$1"

if [ -z $tags ]; then
  tags="all"
fi

# The home dir of current script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

if ! [ -x "$(command -v ansible)" ]; then
  sudo apt-get install ansible
fi

ansible-playbook -i $DIR/hosts $DIR/dotfiles.yml --ask-become-pass --tags $tags
