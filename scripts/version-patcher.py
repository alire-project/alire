import os
import re
import subprocess
import sys

def replace_version(filename, build_info):
    pattern = r'(Current : constant String := "[^+]+\+)([^"]*)(";)'

    # Depending on the context in which this is run, there may be mix-ups with
    # line terminators between the environment detected, github runner, etc...
    # So just keep them as they are and that should always work.

    with open(filename, 'rb') as file:
        content = file.read().decode()

    # The pattern captures the part between '+' and '";', replacing it with our
    # new build information

    new_content = re.sub(pattern, r'\g<1>' + build_info + r'\3', content)

    # A few sanity checks and write if needed

    if new_content == content:
        if build_info in content:
            print(f"Note: version in {filename} already up to date")
        else:
            print(f"WARNING: failed to update version in {filename}")
    else:
        # Ensure the content line terminators are not changed
        with open(filename, 'wb') as file:
            file.write(new_content.encode())


# If a flag exists, skip any updating, just print a message and exit
if "ALR_VERSION_DONT_PATCH" in os.environ:
    print("Note: skipping version update")
    sys.exit(0)

# If there is an argument to the script, retrieve it here and use it as the new
# dirty flag after the commit
if len(sys.argv) > 1:
    dirty = sys.argv[1]
else:
    # Detect whether the current directory contains changes
    if subprocess.call(['git', 'diff-index', '--quiet', 'HEAD', '--']) != 0:
        dirty = "_dirty"
    else:
        dirty = ""

# Find the short git commit of the repository in the current directory
commit = subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD']).decode('utf-8').strip()

# Replace the build version part with the short commit hash plus any extra info
print(f"Updating version in src/alire/alire-version.ads to commit {commit}{dirty}...")
replace_version('src/alire/alire-version.ads', commit+dirty)
