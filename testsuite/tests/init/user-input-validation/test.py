"""
Check that a crate initialized with funny user name is loadable
"""

import os.path

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import content_of

# Preconfigure needed fields
name = "Äł O'Reilly O\"Raro"
run_alr("config", "--global", "--set", "user.email", "abc@de.com")
run_alr("config", "--global", "--set", "user.github_login", "abcde")
run_alr("config", "--global", "--set", "user.name", name)

# Create crate
run_alr("init", "--bin", "xxx")

# Check that it can be shown, which will load the manifest
os.chdir("xxx")
p = run_alr("show")
assert name in p.out, f"Unexpected output: {p.out}"

print('SUCCESS')
