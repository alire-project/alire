"""
Test that `alr printenv` quotes values so that POSIX shells and cmd.exe take
them literally, as alr sets them for its own subprocesses, and that values
cmd.exe cannot take literally are reported instead of being printed.
PowerShell is tested in printenv/quoting-powershell.
"""

import os
import shutil
import subprocess

from drivers.alr import init_local_crate, run_alr, set_environ_in_manifest
from drivers.asserts import assert_eq, assert_match, assert_substring

VALUES = {
    "TEST_QUOTES": "it's \"quoted\"",
    "TEST_SUBST": "$(touch pwned) `touch pwned` $HOME",
    "TEST_META": "a; b && c | d > e < f & g ^h (i)",
    "TEST_LINES": "line 1\nline 2",
    "TEST_UNICODE": "ő € 中 \U0001F600",
}

init_local_crate(update=False)
set_environ_in_manifest(VALUES)

# POSIX shell, where available: the values come out as given, and nothing is
# run

if shutil.which("sh"):
    with open("env.sh", "w", encoding="utf-8") as f:
        f.write(run_alr("printenv", "--unix").out)
    for name, value in VALUES.items():
        out = subprocess.run(["sh", "-c", f'. ./env.sh && printf %s "${name}"'],
                             capture_output=True, encoding="utf-8",
                             check=True).stdout
        assert_eq(value, out, label=name)
    assert not os.path.exists("pwned"), "printenv output ran a command"

# cmd.exe: values it cannot take literally are an error...

p = run_alr("printenv", "--wincmd", complain_on_error=False)
assert_substring("cannot be given literally to cmd.exe", p.out)

# ...and other values are quoted as a whole, so metacharacters are literal

set_environ_in_manifest({"TEST_META": VALUES["TEST_META"]})
assert_match(r'.*set "TEST_META=a; b && c \| d > e < f & g \^h \(i\)"\n',
             run_alr("printenv", "--wincmd").out)

# Names that not every shell accepts are an error

set_environ_in_manifest({"TEST-NAME": "x"})
p = run_alr("printenv", "--unix", complain_on_error=False)
assert_substring("invalid environment variable name 'TEST-NAME'", p.out)

print("SUCCESS")
