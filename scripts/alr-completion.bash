#!/usr/bin/env bash

if ! builtin type -P alr &>/dev/null; then
    echo alr must be in PATH for completion to work
    return
fi

# Commands
_alr_commands=$(alr | sed -n '/Valid commands:/,/Help topics:/p' | tail -n +3 | head -n -2 | awk '{ print $1 }' | xargs)

# Long global switches
_alr_global_switches=$(alr -h | grep -Eo -- '--[[:alnum:]-]+' | xargs)

# Crate names
_alr_crates=$(alr search --crates | cut -f1 -d' ')

# Command-aware long switches
function _alr_completion() {
    curr=$2
    prev=$3

    # Identify which command is being entered, if any
    found=false
    for word in "${COMP_WORDS[@]}"; do # completed words
        for cmd in $_alr_commands; do  # alr commands
            if [ "$word" == "$cmd" ]; then
                # Note that $cmd holds the identified command, that we will use later
                found=true
                break 2
            fi
        done
    done

    # When no command found, suggest commands and long global switches.
    # Although global switches can be used even after the command is given, by
    # hidding them after the command we obtain clearer command-specific
    # suggestions. 
    $found || COMPREPLY+=($(compgen -W "$_alr_commands $_alr_global_switches" -- $curr))

    # When command found, always add the compatible command switches
    if $found ; then
        cmd_switches=$(alr $cmd -h | grep -Eo -- '--[[:alnum:]-]+' | xargs)
        [ "$cmd_switches" != "" ] && COMPREPLY+=($(compgen -W "$cmd_switches" -- $curr))
    fi

    # Command-specific completions
    $found &&\
    case $cmd in
        get | show)
            # Suggest crate names
            COMPREPLY+=($(compgen -W "$_alr_crates" -- $curr))
            ;;

        index)
            # Suggest paths when adding a new index
            [ "$prev" == "--add" ] && compopt -o dirnames
            ;;

        run)
            # Suggest only the executables explicitly declared by the release
            COMPREPLY+=($(compgen -W "$(alr run --list |\
                                        sed -n '/builds these executables/,//p' |\
                                        tail -n +2 |\
                                        awk '{print $1}')" -- $curr))
            ;;
        
        with)
            # When the previous word is "with", show any crate:
            [ "$prev" == "with" ] && COMPREPLY+=($(compgen -W "$_alr_crates" -- $curr))
            # When the previous word is "--del", show direct dependencies:
            [ "$prev" == "--del" ] && COMPREPLY+=($(compgen -W "$(alr with | tail +2 | grep -Eo -- '[_a-z0-9]+')" -- $curr))
            ;;
    esac
} 

# Bind the function that performs context-aware completion
complete -F _alr_completion alr
