"""
Test that non-ASCII configuration values are written as they are (UTF-8) to
the generated Ada, GPR and C configuration files, and not encoded again as if
they were Latin-1, as when written through Ada.Text_IO.
"""

import os

from drivers.alr import alr_manifest, init_local_crate, run_alr
from drivers.asserts import assert_substring

VALUE = "César € 中"

init_local_crate("xxx", update=False)
with open(alr_manifest(), "a", encoding="utf-8") as f:
    f.write("\n[configuration.variables]\n"
            f'Text = {{ type = "String", default = "{VALUE}" }}\n')

run_alr("update")

for ext in ["ads", "gpr", "h"]:
    with open(os.path.join("config", f"xxx_config.{ext}"),
              encoding="utf-8") as f:
        assert_substring(f'"{VALUE}"', f.read())

print("SUCCESS")
