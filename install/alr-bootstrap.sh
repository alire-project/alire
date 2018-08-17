#!/usr/bin/env bash

# DEFINITIONS SECTION

repo_branch=${1:-master}
repo_url=https://github.com/alire-project/alr.git

required_tools="git id"
optional_tools="hg sudo"
required_compiler="gprbuild gnatmake gnatls"

alire_folder=${XDG_CONFIG_HOME:-$HOME/.config}/alire
alire_src=$alire_folder/alr
alire_bin=$alire_src/bin/alr
alire_version=unknown

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

Missing required executable: $1
EOF
    exit 1
    }
}

function check_optional() {
    command -v $1 >/dev/null || { 
        cat <<EOF
An optional tool is not installed or in the session PATH.
Some alr functions might not be available but installation can proceed.

Missing optional executable: $1
EOF
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
    git clone -b $repo_branch $repo_url $alire_src
    pushd $alire_src
    git submodule update --init --recursive
    alire_version=`git describe --always --all | cut -f2 -d'/'`
    popd
    
    echo Compiling...
    gprbuild -j0 -p -XSELFBUILD=False -P $alire_src/alr_env.gpr
}

function check_folder_writable() {
    if ! [ -d "$1" ]; then
        echo $1 is not a folder!
        return 1
    elif !(touch "$1"/.alrfolder 2>/dev/null && rm -f "$1"/.alrfolder 2>/dev/null); then
        echo $1 is not writable!
        return 1
    fi
    return 0
}

function do_install() {
    cat <<EOF

Compilation successful. Please enter a writable folder in which
the alr executable will be installed, preferably in your path.

EOF
    
    while :; do
        echo Enter installation folder:
        bin_folder=""
        while [[ "${bin_folder:-}" == "" ]]; do
            read bin_folder
        done

        bin_folder=`eval echo $bin_folder`
        check_folder_writable "$bin_folder" || continue

        echo ' '
        echo Ready to install alr $alire_version in $bin_folder
        echo Press Y to proceed, any other key to entry another path, or Ctrl-C to stop now.
        read -rsn1 proceed
        [ "$proceed" = "y" ] && break
        [ "$proceed" = "Y" ] && break
    done
    
    echo ' '
    cp -fv "$alire_bin" "$bin_folder"/alr
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

    for cmd in $optional_tools; do
        check_optional $cmd
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
