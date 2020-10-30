# Использовать превью окон вместо иконок при переключении открытых приложений.

# gsettings set org.gnome.desktop.wm.keybindings switch-applications "[]"
# gsettings set org.gnome.desktop.wm.keybindings switch-applications-backward "[]"
# gsettings set org.gnome.desktop.wm.keybindings switch-windows "['<Alt>Tab', '<Super>Tab']"
# gsettings set org.gnome.desktop.wm.keybindings switch-windows-backward  "['<Alt><Shift>Tab', '<Super><Shift>Tab']"
# gsettings set org.gnome.shell.app-switcher current-workspace-only true
# gsettings set org.gnome.shell.window-switcher current-workspace-only true

gsettings set org.gnome.desktop.wm.keybindings switch-applications "['<Alt>Tab']"
gsettings set org.gnome.desktop.wm.keybindings switch-applications-backward "['<Alt><Shift>Tab']"
gsettings set org.gnome.shell.app-switcher current-workspace-only true

gsettings set org.gnome.desktop.wm.keybindings switch-windows "['<Super>Tab']"
gsettings set org.gnome.desktop.wm.keybindings switch-windows-backward  "['<Super><Shift>Tab']"
gsettings set org.gnome.shell.window-switcher current-workspace-only true
