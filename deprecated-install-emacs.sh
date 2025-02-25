#!/usr/bin/env bash

# Emacs version and download url
app="emacs-30.1"
# git clone --depth 1 --branch emacs-30 git://git.savannah.gnu.org/emacs.git emacs-30
archive="$app.tar.xz"
download_url="https://mirror.truenetwork.ru/gnu/emacs/$archive"

# set -x # enable debugging

#-------------------------------------------------------------------------------
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export DOTFILES="${DOTFILES:-$XDG_DATA_HOME/chezmoi}"
export SOFTWARE="${SOFTWARE:-$HOME/soft}"
export STOW_DIR="${STOW_DIR:-/usr/local/stow}"

[ ! -d "$STOW_DIR" ] && sudo mkdir -p "$STOW_DIR"
#-------------------------------------------------------------------------------
# sudo true # get sudo rights to not ask them later

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


log blue "-- Installing prerequisites --------------------------------------------------------"
case $(which_os) in
    fedora)
        sudo dnf install -y cmake libevdev-devel systemd-devel yaml-cpp-devel boost-devel stow
        sudo dnf group install -y "Development Tools"
        sudo dnf builddep -y emacs
        sudo dnf install -y libtree-sitter-devel \
                            ImageMagick ImageMagick-devel \
                            libwebp libwebp-devel \
                            libXi-devel \
                            xml2 libxml2 libxml2-devel
        echo
        ;;
    # debian|ubuntu)
    # sudo apt-get install -y build-essential git autoconf texinfo libgnutls28-dev libxml2-dev libncurses5-dev libjansson-dev
    #    ;;
    *)
        log red "Unsapported OS: $(which_os)"
        exit 1
        ;;
esac

log blue "Download $app"

if [[ ! -d "$SOFTWARE/$app" ]]; then
    pushd $SOFTWARE > /dev/null
    if [[ ! -f "$SOFTWARE/$archive" ]]; then
        # git clone "git://git.sv.gnu.org/emacs.git" --depth 1
        curl -LO $download_url
    fi
    tar xf "$archive"
    popd > /dev/null
fi

pushd "$SOFTWARE/$app" > /dev/null

log blue "-- Configure -----------------------------------------------------------------------"
log blue "autogen.sh"

# Options:
#    --with-native-compilation :: use the libgccjit native compiler
#    --with-pgtk               :: better font rendering
#    --with-x-toolkit=gtk3     :: widgets toolkit
#    --with-tree-sitter        :: syntax parsing
#    --with-wide-int           :: larger file size limit
#    --with-json               :: fast JSON
#    --with-gnutls             :: TLS/SSL
#    --with-mailutils          :: e-mail
#    --without-pop             :: no pop3 (insecure channels)
#    --with-cairo              :: vector graphics backend
#    --with-imagemagick        :: raster images backend
./autogen.sh && ./configure \
    --with-dbus --with-pgtk --with-native-compilation=aot --with-sqlite3 \
    --with-x-toolkit=gtk3 \
    --with-tree-sitter --with-harfbuzz \
    --with-cairo --with-imagemagick \
    --with-gif --with-jpeg --with-png --with-rsvg --with-tiff --with-webp \
    --with-modules --with-json --enable-link-time-optimization --with-gpm=no \
    --with-wide-int --with-gnutls \
    --with-xwidgets --with-xinput2 --with-xft --with-xpm --with-xml2 \
    --with-mailutils --without-pop \
    --without-compress-install \
    --prefix="$STOW_DIR/$app"

# Compiler flags:
# -O2           :: Turn on a bunch of optimization flags. There's also -O3, but
#                  it increases the instruction cache footprint, which may end up
#                  reducing performance.
# -pipe         :: Reduce temporary files to the minimum.
# -mtune=native :: Optimize code for the local machine (under ISA constraints).
# -march=native :: Enable all instruction subsets supported by the local machine.
# -fomit-frame-pointer :: Small functions don't need a frame pointer (optimization).
CFLAGS="-O2 -pipe -mtune=native -march=native -fomit-frame-pointer"

# log blue "-- Building ------------------------------------------------------------------------"
# make -j $(nproc --ignore=1) > /dev/null
#
# log blue "-- Installing ----------------------------------------------------------------------"
# sudo make install
#
# log blue "Stow" $app
# sudo stow --dir=$STOW_DIR -R $app

# set +x # disable debugging

popd > /dev/null
