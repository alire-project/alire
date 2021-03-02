# Replace $1 by $2 in place, recursively

old="$1"
new="$2"

if [ "$old" == "" ] || [ "$new" == "" ]; then
    echo use: $0 '<old text> <new text>'
    exit 1
fi

grep -rl . | parallel sed -i "s/${old}/${new}/g" {}
