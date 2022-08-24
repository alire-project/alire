"""
Check that the last build profile is stored properly, no matter its procedence
"""

from drivers.alr import run_alr, init_local_crate, alr_manifest
from drivers.asserts import assert_eq, assert_match

init_local_crate("xxx")

# Check profile is the default one (development) if unspecified

run_alr("build")
assert_match(".*last_build_profile=xxx=DEVELOPMENT.*",
             run_alr("config").out)

# Check explicit profile in command line

run_alr("build", "--release")
assert_match(".*last_build_profile=xxx=RELEASE.*",
             run_alr("config").out)

# Check implicit profile when build is indirect is last that was used:

run_alr("run")  # Causes a build with the last used profile
assert_match(".*last_build_profile=xxx=RELEASE.*",
             run_alr("config").out)

# Check explicit profile requested in the manifest

with open(alr_manifest(), "at") as manifest:
    manifest.writelines(["[build-profiles]\n",
                         "xxx = 'validation'\n"])

run_alr("build")
assert_match(".*last_build_profile=xxx=VALIDATION.*",
             run_alr("config").out)

print('SUCCESS')
