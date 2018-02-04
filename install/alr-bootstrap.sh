#!/bin/bash

# DEFINITIONS SECTION

repo_branch=master
repo_url=https://git@bitbucket.org/aleteolabs/alr.git

required_tools="git id"
required_compiler="gprbuild gnatmake gnatls"

alire_folder=${XDG_CONFIG_HOME:-$HOME/.config}/alire
alire_src=$alire_folder/alr
alire_bin=$alire_src/bin/alr

# ERROR MANAGEMENT

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

set -o errexit
set -o nounset

# CODE

function check_cmd() {
    command -v $1 >/dev/null || { 
        cat <<EOF
A required tool is not installed or in the session PATH.
Please install it and retry.

Missing executable: $1
EOF
    exit 1
    }
}

function check_gnat() {
    command -v $1 >/dev/null || { 
        cat <<EOF
Recent versions of the GNAT compiler and GPR build tool are required, 
but they have not been found in your PATH.
The missing executable is: $1

In the latest Debian stable or Ubuntu LTS releases, the following command
should be able to install appropriate versions:

sudo apt install gnat gprbuild

EOF
    exit 1
    }
}

function check_no_root() {
    if [[ `id -u` == 0 ]]; then
        cat <<EOF
This script should be run as the user that will use the compiler.        
However, you seem to be running it as root.

Press any key if you really want to continue as root, or Ctrl-C to stop now.
EOF
        read
    fi
}

function fetch_and_compile() {    
    rm -rf $alire_folder
    mkdir -p $alire_folder
    
    echo Cloning alr sources...
    git clone --recurse-submodules -b $repo_branch $repo_url $alire_src
    pushd $alire_src
    git submodule update --recursive --remote
    popd
    
    echo Compiling...
    gprbuild -j0 -p -XSELFBUILD=False -P $alire_src/alr_env.gpr
}

function do_install() {
    cat <<EOF

Compilation successful. Please enter a writable folder in which
the alr executable will be installed, preferably in your path.
Do not use substitutions like ~ or \$HOME:

Enter installation folder:
EOF
    while [[ "${bin_folder:-}" == "" ]]; do
        read bin_folder
    done
    
    echo ' '
    echo Ready to install alr in $bin_folder
    echo Press enter to proceed or Ctrl-C to stop now.
    read
    
    cp -fv $alire_bin $bin_folder/alr
}

function saluton() {
    echo ' '
    echo Detected Ada compiler:
    gnatls -v | head -3
    
    cat <<EOF

Everything is ready to start the installation.
Press any key to continue or Ctrl-C to exit.
EOF
    read    
}

function install_linux() {
    for cmd in $required_tools; do
        check_cmd $cmd
    done

    check_no_root
    
    for cmd in $required_compiler; do 
        check_gnat $cmd
    done
    
    saluton
    
    fetch_and_compile
    do_install
    
    echo ' '
    echo Installation complete, enter "'alr'" to start using it.
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
