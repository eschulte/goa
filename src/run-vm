#!/bin/sh
#
# Usage:
#   run-vm hda port
hda="$1"
port="$2"

# find the OS, using code from
# http://unix.stackexchange.com/questions/6345
if [ -f /etc/lsb-release ]; then
    . /etc/lsb-release
    OS=$DISTRIB_ID
elif [ -f /etc/debian_version ]; then
    OS=Debian
elif [ -f /etc/arch-release ]; then
    OS=Arch
else
    OS=$(uname -s)
fi

# OS-specific qemu command
case $OS in
    Debian) cmd="kvm"      ;;
    Ubuntu) cmd="kvm"      ;;
    Arch)   cmd="qemu-kvm" ;;
    *)      cmd="qemu"     ;;
esac

$cmd -hda $hda -nographic -m 1G \
    -net nic -net user,hostfwd=tcp:127.0.0.1:$port-:22 -daemonize
