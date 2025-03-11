#!/usr/bin/bash

# Harden the script

trap 'cd "$startdir"' EXIT
trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

set -o errexit
set -o nounset

# Function that recursively hashes a directory and prints the hash
function hashdir() {
    local dir="$1" # Directory to hash
    find "$dir" -type f -exec sha256sum {} + | sort | sha256sum | cut -d ' ' -f 1
}

# Start by entering the directory of the script
startdir=$PWD
pushd "$(dirname "$0")"
scriptdir=$PWD

# Identify the base dir of the project
base=$(git rev-parse --show-toplevel)

# Check whether we need to regenerate, based on the hash of the templates
# stored in ./templates.hash

old_hash=$(cat ./templates.hash 2>/dev/null || true)
new_hash=$(hashdir "$base/templates")
if [ "$old_hash" = "$new_hash" ]; then
    echo "No changes in templates, skipping generation"
    exit 0
fi

# Location of generated files
generated=$base/src/templates

# Build awsres from AWS only if awsres is not yet available here or in path

if [ ! -f awsres ] && ! command -v awsres &> /dev/null; then

    echo "Building awsres from AWS..."

    workdir=$PWD
    tmp=$(mktemp -d)
    pushd "$tmp"

    alr get --build aws^24
    find . -name awsres -exec cp {} "$workdir" \;

    # Clean up
    popd
    rm -rf "$tmp"
fi

# Actually generate the embedded resources

export PATH+=":$PWD"

# Clean up old resources
rm -rf $generated
mkdir -p $generated

# AWSRes - Resource Creator v1.3
# Usage : awsres [-hopqrRzu] file1/dir1 [-zu] [file2/dir2...]
#         -a      : packages are named after the actual filenames
#         -o dir  : specify the output directory
#         -p str  : prefix all resource names with the given string
#         -R      : activate recursivity
#         -r name : name of the root package (default res)
#         -z      : enable compression of following resources
#         -u      : disable compression of following resources
#         -q      : quiet mode

# We change momentarily into the templates folder to shorten generated filenames
# and avoid problems with ".." in paths that confuse awsres.
pushd "$base/templates" && \
awsres \
    -a \
    -o $generated \
    -R \
    -r r \
    . && \
popd

# We use actual file names rather than hashes so changes in version control, and
# detecting missing resources, is easier.

# Fix bad package names (two __ in a row) for some files (like .gitignore)
find $generated -type f -exec sed -i 's/__/_/g' {} \;

# Likewise, but for file names
find $generated -type f -name '*__*' -exec \
    bash -c 'mv "$1" "${1//__/_}"' _ {} \;

# Since we don't want to depend on superheavy AWS, we replace calls to it with
# our own registering procedure.
find $generated -type f -exec \
    sed -i 's/AWS\.Resources\.Embedded/Alire.Templates/g' {} \;

# Silence warnings in generated registering code
sed -i '1s/^/pragma Warnings (Off);\n/' $generated/r.adb

# Write hash after successful generation
echo "$new_hash" > "$scriptdir"/templates.hash
echo "Templates hash updated in $scriptdir/templates.hash with value $new_hash"

echo "Resources created successfully"