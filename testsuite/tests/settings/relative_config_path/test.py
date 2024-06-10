"""
Verify that using a relative path for Alire config dir is ok
"""

import os

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import lines_of

run_alr("--settings=.", "settings", "--global",
        "--set", "some_config_key", "true")


assert_eq("some_config_key = true\n", lines_of ("settings.toml")[0])


print('SUCCESS')
