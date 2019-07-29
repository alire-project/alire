"""
Test Jekyll static website framework output.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq


p = run_alr('show', '--jekyll', 'hello', complain_on_error=True, quiet=False)
assert_eq(
      '---\n'
      'layout: crate\n'
      'crate: "hello"\n'
      'authors: ["Bob", "Alice"]\n'
      'maintainers: ["alejandro@mosteo.com", "bob@example.com"]\n'
      'licenses: ["GPL_3_0", "MIT"]\n'
      'websites: ["example.com"]\n'
      'version: "1.0.1"\n'
      'dependencies: [{crate: "libhello", version: "Within_Major (1.0.0)"}]\n'
      '---\n'
      '"Hello, world!" demonstration project\n'
      '\n', p.out)

print('SUCCESS')
