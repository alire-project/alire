"""
Check that an upstream manifest is backed up and not used on retrieval
"""

from drivers.alr import run_alr
from drivers.helpers import contents
from os import chdir, path
# from drivers.asserts import assert_eq, assert_match

run_alr('get', 'crate')
chdir('crate_1.0.0_filesystem')

assert path.isfile('alire/crate.toml.upstream'), \
    "Expected backup file missing"

for line in contents('alire/crate.toml'):
    assert not line.beginswith('badproperty'), \
        "Unexpected contents in manifest file"

print('SUCCESS')
