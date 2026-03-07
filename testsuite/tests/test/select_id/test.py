"""
Select a specific test runner by its id
"""

import os.path

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_not_substring, assert_substring

init_local_crate("xxx")

with open("alire.toml", "+a") as f:
   f.writelines([
      "\n",
      "[[test]]\n",
      "id = 'a'\n",
      "command = ['echo', 'MAGIC_TEST_A']\n",
      "directory = '.'\n",
      "\n",
      "[[test]]\n",
      "id = 'b'\n",
      "command = ['echo', 'MAGIC_TEST_B']\n",
      "directory = '.'\n",
   ])

# check that both test runners run without argyments
p = run_alr("test")
assert_substring("MAGIC_TEST_A", p.out)
assert_substring("MAGIC_TEST_B", p.out)

# check that arguments are not forwarded
p = run_alr("test", "EXTRA_MAGIC")
assert_substring("MAGIC_TEST_A", p.out)
assert_substring("MAGIC_TEST_B", p.out)
assert_not_substring("EXTRA_MAGIC", p.out)

# check that only one test runner is selected with "--id"
p = run_alr("test", "--id", "a")
assert_substring ("MAGIC_TEST_A", p.out)
assert_not_substring("MAGIC_TEST_B", p.out)

# check that arguments are forwarded with "--id"
p = run_alr("test", "--id", "b", "EXTRA_MAGIC")
assert_substring ("MAGIC_TEST_B", p.out)
assert_not_substring("MAGIC_TEST_A", p.out)
assert_substring ("EXTRA_MAGIC", p.out)

# check for failure when using an invalid id
p = run_alr("test", "--id", "c", complain_on_error = False)

print('SUCCESS')
