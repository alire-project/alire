"""
Verify that workspace metadata does not specify an origin
"""

from drivers.alr import run_alr, init_local_crate
from drivers.helpers import content_of
from glob import glob
from os import chdir
from os.path import join


def assert_no_origin(crate):
    assert "origin" not in content_of(join("alire", crate + ".toml")), \
        "found unexpected contents in manifest of crate " + crate


# case 1, a gotten crate
p = run_alr('get', 'libhello')
chdir(glob("libhello*")[0])
assert_no_origin("libhello")
chdir("..")

# case 2, a fresh crate
init_local_crate("xxx")
assert_no_origin("xxx")

print('SUCCESS')
