
# Показывать иконоки при переключении открытых приложений
gsettings set org.gnome.desktop.wm.keybindings \
   switch-applications "['<Alt>Tab']"

gsettings set org.gnome.desktop.wm.keybindings \
   switch-applications-backward "['<Alt><Shift>Tab']"

gsettings set org.gnome.shell.app-switcher \
   current-workspace-only true

# Показывать превью окон вместо иконок при переключении открытых приложений
gsettings set org.gnome.desktop.wm.keybindings \
   switch-windows "['<Super>Tab']"

gsettings set org.gnome.desktop.wm.keybindings \
   switch-windows-backward  "['<Super><Shift>Tab']"

gsettings set org.gnome.shell.window-switcher \
   current-workspace-only true

# Show folder first (before files) in Nautilus
gsettings set org.gtk.Settings.FileChooser \
   sort-directories-first true


# Extensions settings
# -----------------------------------------------------------------------------

extension () {
   # return the path to extencion schemas folder
   echo "$HOME/.local/share/gnome-shell/extensions/$1/schemas"
}

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
