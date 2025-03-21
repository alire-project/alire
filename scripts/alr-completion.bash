#!/usr/bin/env bash

if ! builtin type -P alr &>/dev/null; then
    echo alr must be in PATH for completion to work
    return
fi

# Detect if --builtin is supported, by checking the version output by alr
# --version. Any version older than 2.2.0 will not support --builtin.
builtin=""
alr_version=$(alr --version | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)
if [[ -n "$alr_version" ]]; then
    # Check if version is at least 2.2.0
    major=$(echo "$alr_version" | cut -d. -f1)
    minor=$(echo "$alr_version" | cut -d. -f2)
    if [[ $major -gt 2 ]] || [[ $major -eq 2 && $minor -ge 2 ]]; then
        builtin="--builtin"
    fi
fi

# Disable index auto-update to avoid interference with commands below
if alr settings --global | grep -q index.auto_update= ; then
    update_period=$(alr settings --global | grep index.auto_update= | cut -f2 -d=)
else
    update_period=unset
fi
alr settings --global --set $builtin index.auto_update 0

# Commands/Topics: all line-first words not starting with capital letter, after # COMMANDS
_alr_commands=$(alr | grep COMMANDS -A 99 | awk '{print $1}' | grep -v '[[:upper:]]' | xargs)

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
        get | install | show | toolchain)
            # Suggest crate names
            COMPREPLY+=($(compgen -W "$_alr_crates" -- $curr))
            ;;

        index)
            # Suggest paths when adding a new index
            [ "$prev" == "--add" ] && compopt -o dirnames
            ;;

        publish)
            # Suggest files when using a non-standard manifest
            [ "$prev" == "--manifest" ] && compopt -o filenames
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

# Re-enable index auto-update to avoid interference with commands below
if [ "$update_period" != "unset" ]; then
    alr settings --global --set $builtin index.auto_update $update_period
fi
