"""
Ensure that no unnecessary rebuilds happend due to crate config generation
"""

import os

from drivers import builds
from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.asserts import assert_match

init_local_crate()
alr_with("libhello", manual=False)
run_alr("build") # First build

# Same build, nothing should be recompiled
p = run_alr("build", quiet=False)
# gprbuild < 26: 'gprbuild: "xxx.exe" up to date'
# gprbuild >= 26: '"xxx.exe" up to date' (prefix dropped)
assert_match('.*"xxx.*" up to date', p.out)

# An update regenerates all config files, but those unchanged aren't written
# again, so neither the root nor its dependencies are compiled again
run_alr("update")
p = run_alr("build", quiet=False)
assert_match('.*"xxx.*" up to date', p.out)

# And a shared dependency stays marked as built, so builds don't lock it
if builds.are_shared():
    assert os.path.isfile(os.path.join(builds.find_dir("libhello"),
                                       "alire", "flags", "built")), \
        "Shared dependency no longer marked as built"

# Switch to another profile and build must happen
p = run_alr("build", "--validation", quiet=False)
# gprbuild < 26: '[Ada]   xxx.adb'; gprbuild >= 26: '[Ada Compile]   xxx.adb'
assert_match(r'.*\[Ada(?: Compile)?\]\s+xxx\.adb', p.out)

# Use same profile, nothing should be recompiled
p = run_alr("build", "--validation", quiet=False)
assert_match('.*"xxx.*" up to date', p.out)

print('SUCCESS')
