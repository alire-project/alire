#!/usr/bin/env bash

# Verify that all pins in alire.toml match the submodule versions.
# Must be run from the repository root

# Install tomlq if not already installed
if ! command -v tomlq &> /dev/null; then
    sudo apt-get install -y yq
fi

# Iterate over pins getting their commit hashes
exit_code=0
submodules=$(git submodule)
while read -r dep_name commit_hash; do
    if echo "$submodules" | grep -q $commit_hash; then
        echo "OK:    $commit_hash $dep_name"
    else
        echo "ERROR: $commit_hash $dep_name"
        exit_code=1
    fi
done < <(tomlq -r '.pins[] | to_entries[] | "\(.key) \(.value.commit)"' alire.toml)

exit $exit_code