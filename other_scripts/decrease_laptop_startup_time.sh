#!/usr/bin/env bash

# Disable waiting for the network before loading in userspace.
sudo systemctl disable NetworkManager-wait-online.service
