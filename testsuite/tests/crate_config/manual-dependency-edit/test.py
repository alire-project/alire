"""
A dependency added or removed by editing the manifest by hand is added to or
removed from the generated project file of the root crate
"""

from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.asserts import assert_in_file, assert_not_substring
from drivers.helpers import content_of

CONFIG = "config/xxx_config.gpr"
WITH = 'with "libhello.gpr";'

init_local_crate()
run_alr("build")

# Added by hand, then built

alr_with("libhello")
run_alr("build")
assert_in_file(CONFIG, WITH)

# Removed by hand, then built

alr_with("libhello", delete=True)
run_alr("build")
assert_not_substring(WITH, content_of(CONFIG))

print("SUCCESS")
