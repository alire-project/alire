"""
Check cache location overrides in increasing order of precedence.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import on_windows

if on_windows():
    drive = "C:"
else:
    drive = ""

# Default cache location (inside test config dir)
assert_match(r".*cache folder:[^\n]*config__cache-relocation/alr-config/cache",
             run_alr("version").out.replace("\\", "/"))

# Check toolchain location inside cache location
assert_match(r".*toolchain folder:[^\n]*config__cache-relocation/alr-config/cache/toolchains",
             run_alr("version").out.replace("\\", "/"))

# Override via config (takes precedence)
run_alr("config", "--global", "--set", "cache.dir", f"{drive}/relocated-to-root")
assert_match(r".*cache folder:[^\n]*/relocated-to-root",
             run_alr("version").out.replace("\\", "/"))

# Check toolchain location inside cache location
assert_match(r".*toolchain folder:[^\n]*/relocated-to-root/toolchains",
             run_alr("version").out.replace("\\", "/"))

# Check toolchain override via config (takes precedence over cache override)
run_alr("config", "--global", "--set", "toolchain.dir", f"{drive}/relocated-toolchains")
assert_match(r".*toolchain folder:[^\n]*/relocated-toolchains",
             run_alr("version").out.replace("\\", "/"))

print("SUCCESS")
