"""
`alr sync` regenerates the build configuration from the manifest.
"""

import os

from drivers.alr import init_local_crate, run_alr, use_index_with_manifests
from drivers.asserts import assert_substring
from drivers.helpers import content_of

use_index_with_manifests({})
init_local_crate()

with open("alire.toml", "a") as f:
    f.write('[configuration.variables]\n'
            'Sync_Var = {type = "Boolean", default = true}\n')
run_alr("sync")

assert_substring("Sync_Var : constant Boolean := True",
                 content_of(os.path.join("config", "xxx_config.ads")))

print("SUCCESS")
