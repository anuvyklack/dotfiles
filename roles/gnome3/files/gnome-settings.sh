#!/usr/bin/bash

# Lock screen
# -----------------------------------------------------------------------------
gsettings set org.gnome.desktop.screensaver \
   lock-enabled false


# Files
# -----------------------------------------------------------------------------

# Show folder first (before files) in Nautilus
gsettings set org.gtk.Settings.FileChooser \
   sort-directories-first true

# File history duration
gsettings set org.gnome.desktop.privacy \
   recent-files-max-age 30

gsettings set org.gnome.desktop.privacy \
   remove-old-trash-files true

gsettings set org.gnome.desktop.privacy \
   remove-old-temp-files true


# Status bar
# -----------------------------------------------------------------------------
# Show battary percentage
gsettings set org.gnome.desktop.interface \
   show-battery-percentage true

# Automatic weather location
gsettings set org.gnome.shell.weather \
   automatic-location true


# Keyboard
# -----------------------------------------------------------------------------
# Enable russian keyboard layout
gsettings set org.gnome.desktop.input-sources \
   sources "[('xkb', 'us'), ('xkb', 'ru')]"

gsettings set org.gnome.desktop.input-sources \
   per-window true


# Windows and applications switching
# -----------------------------------------------------------------------------
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

# On switching show windows only from current workspace
gsettings set org.gnome.shell.window-switcher \
   current-workspace-only true


# Touchpad
# -----------------------------------------------------------------------------
# In Pop!_OS two-finger scrolling and edge-scrolling cannot be able simultaneously
gsettings set org.gnome.desktop.peripherals.touchpad \
   two-finger-scrolling-enabled true

gsettings set org.gnome.desktop.peripherals.touchpad \
   edge-scrolling-enabled false


# Touchpad scrolling settings
gsettings set org.gnome.desktop.peripherals.touchpad \
   natural-scroll true


# Date and time
# -----------------------------------------------------------------------------
gsettings set org.gnome.desktop.interface \
   clock-format '24h'

gsettings set org.gtk.Settings.FileChooser \
   clock-format '24h'


# Night Light
# -----------------------------------------------------------------------------
gsettings set org.gnome.settings-daemon.plugins.color \
   night-light-enabled true

gsettings set org.gnome.settings-daemon.plugins.color \
   night-light-temperature "uint32 3500"


# Gnome Tweaks
# -----------------------------------------------------------------------------
gsettings set org.gnome.desktop.wm.preferences \
   button-layout 'appmenu:minimize,close'
