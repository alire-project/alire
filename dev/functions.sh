#!/usr/bin/env

function guess_OS() {
    # Returns one of the values needed in ALIRE_OS, using environment variables

    if [ -n "$WINDIR" ]; then
        echo windows
    elif [ -n "$OSTYPE" ]; then
        case "$OSTYPE" in # only available on bash shells
            "linux-gnu")
                echo linux
                ;;
            "freebsd")
                echo freebsd
                ;;
            "openbsd")
                echo openbsd
                ;;
            "darwin"*) # varies with versions: darwin18, darwin19, etc.
                echo macos
                ;;
            *)
                echo unknown # give up, builds depending on ALIRE_OS will fail.
                ;;
        esac
    else
        echo unknown
    fi
}

function get_OS() {
    # Returns one of the values needed in ALIRE_OS, using `uname``

    OS=`uname -s`

    case "$OS" in
    "Linux")
        echo linux
        ;;
    "FreeBSD")
        echo freebsd
        ;;
    "OpenBSD")
        echo openbsd
        ;;
    "Darwin")
        echo macos
        ;;
    "Windows")
        echo windows
        ;;
    *)
        # Fall back to use environment clues
        guess_OS
        ;;
    esac
}
