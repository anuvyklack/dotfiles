#!/usr/bin/env bash

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export DOTFILES="${DOTFILES:-XDG_DATA_HOME/chezmoi}"
export SOFTWARE="${SOFTWARE:-$HOME/soft}"
export STOW_DIR="${STOW_DIR:-/usr/local/stow}"

[ ! -d "$STOW_DIR" ] && sudo mkdir -p "$STOW_DIR"

log() {
    echo -n "$(color $1 bold $2) "
    shift 2
    echo "$*"
}

which_os() {
    if [[ "$(uname -s)" == "Darwin" ]]; then
        echo "darwin"
    elif [[ -f /etc/os-release ]]; then
        source /etc/os-release
        echo "$ID"
    else
        echo "unknown"
    fi
}


interception_tools() {
    # set -x
    local app="interception-tools"
    local repo_url="https://gitlab.com/interception/linux/tools.git"

    local src_dir="$SOFTWARE/$app"
    local build_dir="$src_dir/build"
    local install_dir="$STOW_DIR/$app"

    log blue "Install prerequisites"
    case $(which_os) in
        fedora)
            sudo dnf install -y cmake libevdev-devel systemd-devel yaml-cpp-devel boost-devel stow
            ;;
        debian|ubuntu)
            sudo apt-get install -y cmake libevdev-dev libudev-dev libyaml-cpp-dev libboost-dev stow
            ;;
        *)
            log red "Unsapported OS: $(which_os)"
            exit 1
            ;;
    esac

    if git clone-or-pull "$repo_url" "$src_dir"; then
        [ -d $build_dir ] && rm -rf $build_dir
        mkdir $build_dir
        pushd $build_dir > /dev/null
        log blue "Building" $app
        cmake -DCMAKE_BUILD_TYPE=Release \
              -DCMAKE_INSTALL_PREFIX=$install_dir \
              -S $src_dir -B $build_dir
        make
        log blue "Installing" $app
        sudo make install
        log blue "Stow" $app
        sudo stow --dir=$STOW_DIR -R $app
        log blue "Remove $build_dir"
        rm -rf $build_dir
        popd > /dev/null
    fi

    [ ! -d "/etc/$app" ] && sudo mkdir -p "/etc/$app"
    # set +x
    return 0
}


dual_function_keys() {
    # set -x
    local app="dual-function-keys"
    local repo_url="https://gitlab.com/interception/linux/plugins/dual-function-keys.git"

    local src_dir="$SOFTWARE/$app"
    local install_dir="$STOW_DIR/$app"

    if git clone-or-pull "$repo_url" "$src_dir"; then
        pushd $src_dir > /dev/null
        log blue "Building" $app
        make
        log blue "Installing" $app
        sudo make install PREFIX=$install_dir
        log blue "Stow" $app
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
    log blue "Copy config files"
    for file in "${files[@]}"; do
        file="$file.yaml"
        sudo cp "$config_files/$file" "/etc/interception-tools/$file"
    done
    log green "Done"
    sudo cp "$config_files/udevmon.yaml" "/etc/udevmon.yaml"
    sudo cp "$config_files/udevmon.service" "/etc/systemd/system/udevmon.service"
    log blue "Enable udevmon systemd service"
    sudo systemctl enable --now udevmon
    log green "Done"
}

interception_tools
dual_function_keys
copy_config_files
