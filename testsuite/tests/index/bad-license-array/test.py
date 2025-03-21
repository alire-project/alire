"""Test that licenses given as arrays are rejected
"""

from drivers.alr import alr_manifest, init_local_crate, run_alr
from drivers.asserts import assert_substring
import re

init_local_crate()

# Replace "licenses" line in alire.toml with an array
with open("alire.toml", "r") as f:
    lines = f.readlines()

new_lines = []
for line in lines:
    if line.strip().startswith("licenses"):
        new_lines.append('licenses = ["MIT"]\n')
    else:
        new_lines.append(line)

with open("alire.toml", "w") as f:
    f.writelines(new_lines)

p = run_alr('show', complain_on_error=False)
assert_substring('cannot be an array', p.out)


print('SUCCESS')
