#!/usr/bin/env bash

# See https://github.com/commercialhaskell/stack/issues/2536 and
# https://bugs.launchpad.net/ubuntu/+source/systemd/+bug/1624320/comments/8 where
# systemd appears to add a bogus nameserver
rm -f /etc/resolv.conf
ln -s /run/systemd/resolve/resolv.conf /etc/resolv.conf

apt-get update

# Install ncurses and headers
apt-get install -y libncurses5-dev libncursesw5-dev

# Install Haskell Stack
curl -sSL https://get.haskellstack.org/ | sh

