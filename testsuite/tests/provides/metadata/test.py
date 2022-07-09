"""
Test the creation of the metadata file containing crate aliases (provides.toml)
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

aliases_file = os.path.join(os.environ['ALR_CONFIG'],
                            "indexes", "providers.toml")
# This is the file where crate aliases are stored

# Initially, the file mustn't exist
assert not os.path.isfile(aliases_file), f"Unexpected file: {aliases_file}"

# Force reading of the full index
run_alr("search", "--crates")

# Now, the file must exist
assert os.path.isfile(aliases_file), f"Missing file: {aliases_file}"

# Let's verify its contents
with open(aliases_file, "rt") as file:
    contents = file.readlines()

contents = "".join(contents).replace("\n", "")

assert_eq('aliased = ["crate",]',
          contents)
# This means that "crate" provides "aliased", so when solving "aliased" we also
# need to load "crate"

print('SUCCESS')
