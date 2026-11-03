"""
Text that isn't valid UTF-8 at a prompt is rejected, and the question asked again
"""

import os

from drivers.alr import run_alr, run_alr_interactive
from drivers.asserts import assert_eq

run_alr_interactive(['init', '--bin', 'xxx'],
                    output=['> ' for _ in range(9)],
                    input=[b'caf\xe9',  # Latin-1, not valid UTF-8
                           b'caf\xc3',  # Truncated UTF-8
                           'good',      # Description
                           'name',      # Full user name
                           '',          # Github login
                           '',          # Email
                           '',          # License
                           '',          # Tags
                           ''],         # Website
                    timeout=5)

# Answers would end up in the wrong questions had the first ones been accepted

os.chdir("xxx")
lines = run_alr("show").out.splitlines()
assert_eq("good", lines[0].split(": ", 1)[1])
assert_eq("name", lines[3].split(": ", 1)[1])

print('SUCCESS')
