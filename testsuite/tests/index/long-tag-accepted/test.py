"""
Test that a tag longer than 15 characters is accepted
"""

from drivers.alr import run_alr
from drivers.asserts import assert_substring


# "internationalization" is 20 characters long
p = run_alr("--format=TOML", "show", "hello_world")
assert_substring('tags = [\n"internationalization",\n]', p.out)

print('SUCCESS')
