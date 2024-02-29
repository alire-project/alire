"""
Check the basic report of cache use
"""

from drivers import builds
from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_match
from drivers.helpers import contents

# Default cache status after clean install

assert_match("""\
Path:.*alr-config/cache
Size: 0.0 B
""",
run_alr("cache").out)

# Compile something with a dependency and there should be something in the
# cache when builds are shared.

init_local_crate()
alr_with("libhello")
run_alr("build")
p = run_alr("cache")
if builds.are_shared():
    # Something already in the cache
    assert_match(r"Path:.*alr-config/cache\nSize: (?!0.0 B).*\n", p.out)
else:
    # Still nothing if no shared cache
    assert_match(r"Path:.*alr-config/cache\nSize: 0.0 B\n", p.out)

# After installing some toolchain, for sure there should be something in the cache

run_alr("toolchain", "--select", "gnat_native=1", "gprbuild")
try:
    p = run_alr("cache")
except:
    # Something strange is happening...
    assert False, contents("alr-config/cache")
assert_match(r"Path:.*alr-config/cache\nSize: (?!0.0 B).*\n", p.out)

print("SUCCESS")
