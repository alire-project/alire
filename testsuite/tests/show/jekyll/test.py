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
      'licenses: ["GPL-3.0-only OR MIT"]\n'
      'websites: ["example.com"]\n'
      'tags: ["tag1", "other-tag"]\n'
      'version: "1.0.1"\n'
      'short_description: "\\"Hello, world!\\" demonstration project"\n'
      'dependencies: [{crate: "libhello", version: "^1.0"}]\n'
      '---\n'
      'This is an example of long description in a multi-line string.\n'
      '\n'
      'Markdown formating `can` be used to have "nice" display on the website.\n'
      '\n'
      '\n', p.out)

print('SUCCESS')
