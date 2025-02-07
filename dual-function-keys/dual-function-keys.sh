#!/usr/bin/env bash

export DOTFILES_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/chezmoi"
source "$DOTFILES_DIR/functions/utils.sh"

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$script_dir/interception-tools.sh"

dual_function_keys() {
    # set -x
    local app="dual-function-keys"
    local repo_url="https://gitlab.com/interception/linux/plugins/dual-function-keys.git"

    local src_dir="$SOFTWARE_DIR/$app"
    local install_dir="$STOW_DIR/$app"

    if git_clone_or_pull "$repo_url" "$src_dir"; then
        pushd $src_dir > /dev/null
        log_blue "Building" $app
        make
        log_blue "Installing" $app
        sudo make install PREFIX=$install_dir
        log_blue "Stow" $app
        sudo stow --dir=$STOW_DIR -R $app
        make clean
        popd > /dev/null
    fi

    # set +x
    return 0
}

copy_config_files() {
    local script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    local config_files="$script_dir/config"
    local files=(
        "kinesis-advantage2"
        "laptop-keyboard"
    )
    log_blue "Copy config files"
    for file in "${files[@]}"; do
        file="$file.yaml"
        sudo cp "$config_files/$file" "/etc/interception-tools/$file"
    done
    log_green "Done"
    sudo cp "$config_files/udevmon.yaml" "/etc/udevmon.yaml"
    sudo cp "$config_files/udevmon.service" "/etc/systemd/system/udevmon.service"
    log_blue "Enable udevmon systemd service"
    sudo systemctl enable --now udevmon
    log_green "Done"
}

dual_function_keys
copy_config_files
