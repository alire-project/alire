"""
Check that an upstream manifest is backed up and not used upon `alr get`
"""

from drivers.alr import run_alr
from drivers.helpers import content_of
from os import chdir, path

# Retrieve a crate that bundles a manifest file
run_alr('get', 'crate')
chdir('crate_1.0.0_filesystem')

upstream = path.join('alire', 'alire.toml.upstream')

# Verify that the manifest has been properly renamed
assert path.isfile(upstream), "Expected backup file missing"

# Verify that contents are as expected in the generated and backed up manifests
assert "badproperty" not in content_of("alire.toml"), \
        "Unexpected contents present in manifest file"
assert "badproperty" in content_of(upstream), \
        "Unexpected contents missing in upstream manifest file"

print('SUCCESS')
