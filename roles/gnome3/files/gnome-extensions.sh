#!/usr/bin/bash

extension () {
   # return the path to extencion schemas folder
   echo "$HOME/.local/share/gnome-shell/extensions/$1/schemas"
}


# Dash to Dock
# -----------------------------------------------------------------------------

gsettings --schemadir $(extension dash-to-dock@micxgx.gmail.com) \
   set org.gnome.shell.extensions.dash-to-dock \
   dock-position "BOTTOM" # "LEFT"

# Don't show trash can
gsettings --schemadir $(extension dash-to-dock@micxgx.gmail.com) \
   set org.gnome.shell.extensions.dash-to-dock \
   show-trash false

# Don't show maunted volumes and devices
gsettings --schemadir $(extension dash-to-dock@micxgx.gmail.com) \
   set org.gnome.shell.extensions.dash-to-dock \
   show-mounts false

gsettings --schemadir $(extension dash-to-dock@micxgx.gmail.com) \
   set org.gnome.shell.extensions.dash-to-dock \
   isolate-workspaces true

gsettings --schemadir $(extension dash-to-dock@micxgx.gmail.com) \
   set org.gnome.shell.extensions.dash-to-dock \
   isolate-monitors true


# Caffeine
# -----------------------------------------------------------------------------
# Restore states across reboots
gsettings --schemadir $(extension caffeine@patapon.info) \
   set org.gnome.shell.extensions.caffeine \
   restore-state true
