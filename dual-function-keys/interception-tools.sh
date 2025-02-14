#!/usr/bin/env bash

DOTFILES="${XDG_DATA_HOME:-$HOME/.local/share}/chezmoi"
source "$DOTFILES/scripts/utils.sh"

interception_tools() {
    # set -x
    local app="interception-tools"
    local repo_url="https://gitlab.com/interception/linux/tools.git"

    local src_dir="$SOFTWARE_DIR/$app"
    local build_dir="$src_dir/build"
    local install_dir="$STOW_DIR/$app"

    log_blue "Install prerequisites"
    case $(which_os) in
        fedora)
            sudo dnf install -y cmake libevdev-devel systemd-devel yaml-cpp-devel boost-devel stow
            ;;
        debian|ubuntu)
            sudo apt-get install -y cmake libevdev-dev libudev-dev libyaml-cpp-dev libboost-dev stow
            ;;
        *)
            error "Unsapported OS: $(which_os)"
            ;;
    esac

    if git_clone_or_pull "$repo_url" "$src_dir"; then
        [ -d $build_dir ] && rm -rf $build_dir
        mkdir $build_dir
        pushd $build_dir
        log_blue "Building" $app
        cmake -DCMAKE_BUILD_TYPE=Release \
              -DCMAKE_INSTALL_PREFIX=$install_dir \
              -S $src_dir -B $build_dir
        make
        log_blue "Installing" $app
        sudo make install
        log_blue "Stow" $app
        sudo stow --dir=$STOW_DIR -R $app
        log_blue "Remove $build_dir"
        rm -rf $build_dir
        popd
    fi

    [ ! -d "/etc/$app" ] && sudo mkdir -p "/etc/$app"
    # set +x
    return 0
}

interception_tools
