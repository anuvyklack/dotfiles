#!/usr/bin/env bash

log_blue() {
    echo -en "\e[01;31m" "$1" "\e[0m"
    shift
    echo -e "\e[01;34m" "$*" "\e[0m"
}

error() {
    # bold red
    echo -e "\e[01;31m" "$@" "\e[0m"
    exit 1
}

if ! chezmoi="$(command -v chezmoi)"; then
    local bin_dir="${HOME}/.local/bin"
    local chezmoi="${bin_dir}/chezmoi"
    log_blue "Installing chezmoi to '${chezmoi}'"
    if command -v curl >/dev/null; then
        chezmoi_install_script="$(curl -fsSL https://get.chezmoi.io)"
    elif command -v wget >/dev/null; then
        chezmoi_install_script="$(wget -qO- https://get.chezmoi.io)"
    else
        error "To install chezmoi, you must have curl or wget."
    fi
    sh -c "${chezmoi_install_script}" -- -b "${bin_dir}"
fi
