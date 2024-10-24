"""
Verify that `alr clean --cache` works properly
"""

import os

from drivers.alr import run_alr, init_local_crate, alr_pin
from drivers.asserts import assert_file_exists
from drivers.helpers import init_git_repo


# `alr init` a couple of crates to use as dependencies.
init_local_crate("yyy", enter=False)
yyy_path = os.path.join(os.getcwd(), "yyy")
yyy_url = "git+file:" + yyy_path
init_git_repo("yyy")
init_local_crate("zzz", enter=False)
zzz_path = os.path.join(os.getcwd(), "zzz")

# `alr init` a new crate and add various dependencies (a plain dependency,
# a git dependency, a plain pin and a git pin).
init_local_crate()
run_alr("with", "hello")
run_alr("with", "libfoo")
alr_pin("yyy", url=yyy_url)
alr_pin("zzz", path=zzz_path)

# Verify the 'alire/cache' directory now exists and contains a 'pins' directory
# (also 'dependencies' when running in sandboxed mode).
assert_file_exists(os.path.join("alire", "cache", "pins"))

# Run the 'alr clean --cache' command.
run_alr("clean", "--cache")

# Verify that the cache directory no longer exists
assert_file_exists(os.path.join("alire", "cache"), wanted=False)


print('SUCCESS')
