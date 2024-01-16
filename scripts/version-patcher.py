import os
import re
import subprocess
import sys

def replace_version(filename, new_text):
    pattern = r'(Current : constant String := "[^+]+\+)([^"]*)(";)'
    with open(filename, 'r') as file:
        content = file.read()

    new_content = re.sub(pattern, r'\g<1>' + new_text + r'\3', content)

    if new_content == content:
        if new_text in content:
            print(f"Note: version in {filename} already up to date")
        else:
            print(f"WARNING: failed to update version in {filename}")
    else:
        with open(filename, 'w') as file:
            file.write(new_content)

# If a flag exists, skip any updating, just print a message and exit
if "ALR_VERSION_DONT_PATCH" in os.environ:
    print("Note: skipping version update")
    sys.exit(0)

# If there is an argument to the script, retrieve it here and use it as the new
# dirty flag
if len(sys.argv) > 1:
    dirty = sys.argv[1]
else:
    # Detect the current directory contains changes
    if subprocess.call(['git', 'diff-index', '--quiet', 'HEAD', '--']) != 0:
        dirty = "_dirty"
    else:
        dirty = ""

# Find the short git commit of the repository in the current directory
commit = subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD']).decode('utf-8').strip()

# Replace the build version part with the short commit hash
print(f"Updating version in src/alire/alire-version.ads to commit {commit}{dirty}...")
replace_version('src/alire/alire-version.ads', commit+dirty)
