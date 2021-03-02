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
      'authors: ["Bob",\n'
      '"Alice"]\n'
      'maintainers: ["alejandro@mosteo.com",\n'
      '"bob@example.com"]\n'
      'licenses: ["GPL-3.0-only OR MIT"]\n'
      'websites: ["example.com"]\n'
      'tags: ["tag1",\n'
      '"other-tag"]\n'
      'version: "1.0.1"\n'
      'short_description: "\\"Hello, world!\\" demonstration project"\n'
      'dependencies: [{crate: "libhello", version: "^1.0"}]\n'
      'configuration.variables: [{name: \'Var1\', type: \'Boolean\'},\n'
      '{name: \'Var2\', type: \'String\', default: "str"},\n'
      '{name: \'Var3\', type: \'Enum (A, B)\', default: "A"},\n'
      '{name: \'Var4\', type: \'Integer range -9223372036854775808 .. 9223372036854775807\', default: "0"},\n'
      '{name: \'Var5\', type: \'Integer range -1 .. 1\', default: "0"},\n'
      '{name: \'Var6\', type: \'Real range -1.00000000000000E+00 .. 1.00000000000000E+00\', default: "0.00000000000000E+00"},\n'
      '{name: \'Var7\', type: \'Real range -inf .. +inf\', default: "0.00000000000000E+00"}]\n'
      'configuration.settings: [{crate: \'hello\', settings: [{name: \'Var1\', value: "TRUE"}]},\n'
      '{crate: \'libhello\', settings: [{name: \'Var1\', value: "FALSE"}]}]\n'
      '\n'
      '---\n'
      'This is an example of long description in a multi-line string.\n'
      '\n'
      'Markdown formating `can` be used to have "nice" display on the website.\n'
      '\n'
      '\n', p.out)

print('SUCCESS')
