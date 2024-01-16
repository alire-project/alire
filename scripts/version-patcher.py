import re
import subprocess
import sys

def replace_version(filename, new_text):
    pattern = r'(Current : constant String := "[^+]+\+)([^"]*)(";)'
    with open(filename, 'r') as file:
        content = file.read()

    new_content = re.sub(pattern, r'\g<1>' + new_text + r'\3', content)

    with open(filename, 'w') as file:
        file.write(new_content)

# If there is an argument to the script, retrieve it here and use it as the new
# version
if len(sys.argv) > 1:
    commit = sys.argv[1]
else:
    # Find the short git commit of the repository in the current directory
    commit = subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD']).decode('utf-8').strip()

# Replace the build version part with the short commit hash
print(f"Updating version in src/alire/alire-version.ads to commit {commit}...")
replace_version('src/alire/alire-version.ads', commit)
