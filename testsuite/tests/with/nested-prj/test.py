"""
Verify proper working of nested project files (bugfix for #634)
"""

import os

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_eq

# Initialize a project, have it to have a nested project file

init_local_crate("xxx")

# Make the project file nested (sources too, to avoid modifying the gpr file)
os.mkdir("nested")
os.rename("xxx.gpr", os.path.join("nested", "xxx.gpr"))
os.rename("src", os.path.join("nested", "src"))

# Update info in the manifest
with open("alire.toml", "at") as manifest:
    manifest.write("project-files=['nested/xxx.gpr']")

# Should not fail
run_alr("with", "libhello")

print('SUCCESS')
