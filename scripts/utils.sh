# Get the directory of the current script:
# cd to the current script directory and print it with `pwd`. This gives us the
# absolute path, even if the script is called with a relative path.
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

source "$script_dir/color.sh"

#-- Environment variables ------------------------------------------------------
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export DOTFILES="${DOTFILES:-XDG_DATA_HOME/chezmoi}"
export SOFTWARE="${SOFTWARE_DIR:-$HOME/soft}"
export STOW_DIR="${STOW_DIR:-/usr/local/stow}"

[ ! -d "$STOW_DIR" ] && sudo mkdir -p "$STOW_DIR"

#-- log ------------------------------------------------------------------------

log_color() {
  echo -n "$(color $1 bold $2) "
  shift 2
  echo "$*"
}

# Usage:
#   log_yellow "yellow text" "white text"
log_red() { log_color "red" "$@"; }
log_blue() { log_color "blue" "$@"; }
log_green() { log_color "green" "$@"; }
log_yellow() { log_color "yellow" "$@"; }

log_manual_action() {
  echo -n "⚠️ "
  log_red "$@"
}

log_error() {
  echo -n "❌ "
  log_red "$@"
}

error() {
    log_error "$@"
    exit 1
}

#-- git ------------------------------------------------------------------------

git_check_remote() {
    local upstream="${1:-@{u}}"  # Default upstream branch is @{u}

    git remote update # > /dev/null 2>&1

    local local_commit=$(git rev-parse @)
    local remote_commit=$(git rev-parse "$upstream")
    local base_commit=$(git merge-base @ "$upstream")

    if [[ "$local_commit" == "$remote_commit" ]]; then
        echo "up-to-date"
    elif [[ "$local_commit" == "$base_commit" ]]; then
        echo "need-pull"
    elif [[ "$remote_commit" == "$base_commit" ]]; then
        echo "need-push"
    else
        echo "diverge"
    fi
}

# Usage:
# git_clone_or_pull "url" "path"
# Parameters:
# - $1 :: URL of the repository in the form accepted by git.
# - $2 :: The full PATH to the directory where to clone repository.
git_clone_or_pull() {
    local git_url=$1 # Repository URL in the form accepted by git
    local repo_path=$2 # The full path to the repo directory
    local status=1

    if [ -d "$repo_path" ]; then
        log_blue "Repository already exists at $repo_path. Checking for updates..."
        pushd "$repo_path" > /dev/null

        if [[ "$(git_check_remote)" == "need-pull" ]]; then
            git pull
            status=$?
            if [ $status -eq 0 ]; then
                log_green "Repository was updated."
            else
                log_red "Failed to pull changes."
            fi
        else
            log_green "Repository is already up to date."
        fi

        popd > /dev/null
        return $status
    else
        git clone "$git_url" "$repo_path" # > /dev/null 2>&1
        return $?
    fi
}

#-------------------------------------------------------------------------------

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

# sudo() {
#     # shellcheck disable=SC2312
#     if [ "$(id -u)" -eq 0 ]; then
#         "$@"
#     else
#         if ! command sudo --non-interactive true 2>/dev/null; then
#             command sudo --validate
#         fi
#             command sudo "$@"
#     fi
# }
