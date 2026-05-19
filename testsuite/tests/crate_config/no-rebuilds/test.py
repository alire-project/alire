"""
Ensure that no unnecessary rebuilds happend due to crate config generation
"""

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match

init_local_crate()
run_alr("build") # First build

# Same build, nothing should be recompiled
p = run_alr("build", quiet=False)
# gprbuild < 26: 'gprbuild: "xxx.exe" up to date'
# gprbuild >= 26: '"xxx.exe" up to date' (prefix dropped)
assert_match('.*"xxx.*" up to date', p.out)

# Switch to another profile and build must happen
p = run_alr("build", "--validation", quiet=False)
# gprbuild < 26: '[Ada]   xxx.adb'; gprbuild >= 26: '[Ada Compile]   xxx.adb'
assert_match(r'.*\[Ada(?: Compile)?\]\s+xxx\.adb', p.out)

# Use same profile, nothing should be recompiled
p = run_alr("build", "--validation", quiet=False)
assert_match('.*"xxx.*" up to date', p.out)

print('SUCCESS')
