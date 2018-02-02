#!/bin/bash

set -o errexit
set -o nounset

ensure_cmd() {
    command -v $1 >/dev/null || { 
        cat <<EOF
A required command is not in the session PATH.
Please install it and retry.

Missing command: $1
EOF
    exit 1
    }
}

check_no_root() {
    if [[ `id -u` == 0 ]]; then
        echo This script must not be run as root
        exit 1
    fi
}

check_ada_compiler() {
    return
}

function install_linux() {
    for cmd in git id; do
        ensure_cmd $cmd
    done

    check_no_root
    check_ada_compiler
}

function main() {

case $OSTYPE in
    linux-gnu)
        install_linux;;
    *)
        echo Unsupported platform: "[$OSTYPE]"
        exit 1;;
esac

}

main "$@"
