"""
Run tests using a custom runner
"""

import os.path
import uuid

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match
from drivers.helpers import replace_in_file

init_local_crate()

assert not os.path.exists("./tests")

# successful custom test runner
with open("./alire.toml", "a") as f:
   f.write("""[test]
command = ["echo", "custom runner OK"]
directory = "."
""")

p = run_alr("test")
assert_match(".*custom runner OK.*", p.out)

# failing custom test runner
nonexistent = uuid.uuid4().hex
assert not os.path.exists(nonexistent)
replace_in_file("./alire.toml",
                'command = ["echo", "custom runner OK"]',
                f'runner = ["ls", "{nonexistent}"]')
run_alr("test", complain_on_error=False)

print('SUCCESS')
