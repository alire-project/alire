"""
Verify various bad pins in the manifest are rejected
"""

from drivers.alr import run_alr, init_local_crate, alr_lockfile, alr_manifest
from drivers.helpers import lines_of
from drivers.asserts import assert_match

import os.path


def check(pin, error):
    """
    Insert a pin at the end of the manifest, verify that it is rejected, and
    remove it from the manifest. Check the error produced against the one given
    """
    with open(alr_manifest(), "a") as manifest:
        manifest.write("\n[[pins]]\n" + pin + "\n")

    # Remove lockfile to ensure reload
    if os.path.exists(alr_lockfile()):
        os.remove(alr_lockfile())

    p = run_alr("pin", complain_on_error=False)
    assert p.status != 0, "Unexpected success for pin: " + pin
    assert_match(".*Cannot continue with invalid session.*" +
                 error + ".*\n",
                 p.out)

    # Restore the manifest
    lines = lines_of(alr_manifest())
    lines.pop()
    with open(alr_manifest(), "w") as manifest:
        manifest.write("".join(lines))

    # Verify the manifest is OK again
    run_alr("pin")


init_local_crate()

# Invalid empty pins
check("foo = { }", "invalid pin description")
check("foo = ''", "Malformed semantic version in pin")

# Single string must be a version
check("foo = 'https://github.com/alire-project/alire.git'",
      "Malformed semantic version in pin")

# Mixed incompatible fields
check("foo = { version = '3', url='https://' }",
      "forbidden extra entries")
check("foo = { path = '/', url='https://' }",
      "forbidden extra entries")
check("foo = { path = '/', version='6976' }",
      "forbidden extra entries")
check("foo = { path = '/', "
      "commit = '0123456789012345678901234567890123456789' }",
      "forbidden extra entries")

# Extraneous field
check("foo = { wont_ever_exist='' }", "invalid pin description")

# Commit without url
check("foo = { commit = '0123456789012345678901234567890123456789' }",
      "invalid pin description")

# Invalid commit
check("foo = { url = 'https://', commit = '1234abcd' }",
      "invalid commit")

# Extra unknown field
check("foo = { version = '3', wont_exist='' }",
      "forbidden extra entries")
check("foo = { path = '/', wont_exist='' }",
      "forbidden extra entries")
check("foo = { url = 'https://', wont_exist='' }",
      "forbidden extra entries")
check("foo = { url = 'https://', wont_exist='', "
      "commit = '0123456789012345678901234567890123456789' }",
      "forbidden extra entries")

print('SUCCESS')
