"""
Test alr edit with two project files defined.
"""

from glob import glob
from drivers.alr import run_alr, alr_settings_set
from drivers.asserts import assert_match, assert_not_substring
from drivers.helpers import MockCommand
import os
import shutil


# Get the "libhello" project and enter its directory
run_alr('get', 'libhello')
os.chdir(glob('libhello*')[0])

# Set GNATstudio as prefered editor
alr_settings_set('editor.cmd', 'gnatstudio -P ${GPR_FILE}')

gs = shutil.which('gnatstudio')

if gs is None:
    # GNATstudio not in PATH: Check that we get an error saying GS not
    # available
    p = run_alr('edit', '--project=project1.gpr', complain_on_error=False)
    assert_match(".*GNAT Studio not available or not in PATH.*", p.out)

# Check that we get an error when multiple project files are defined
p = run_alr('edit', complain_on_error=False)
assert_match(".*Please specify a project file with --project=.*", p.out)

# Set an editor that doesn't exist (different than GNAT Studio)
alr_settings_set('editor.cmd', 'doesnt_exist arg1 ab${GPR_FILE}ab arg3')
p = run_alr('edit', '--project=project1.gpr', complain_on_error=False)
assert_match("ERROR: 'doesnt_exist' not available or not in PATH.*", p.out)

# Use echo as an editor to check command line args
alr_settings_set('editor.cmd', 'echo arg1 ab${GPR_FILE}ab arg3')
p = run_alr('edit', '--project=project1.gpr', quiet=False)
assert_match(r"arg1 abproject1\.gprab arg3", p.out)

# Verify that an `editor.cmd` value in a crate's local `settings.toml` is
# ignored with a warning (otherwise this would offer an inconspicuous vector
# through which a malicious crate could execute arbitrary commands).
with open(os.path.join("alire", "settings.toml"), "a") as f:
    f.write('editor.cmd = "malicious_cmd"\n')
with MockCommand("malicious_cmd", "print('Malicious cmd called')", "cmd_dir"):
    p = run_alr('edit', '--project=project1.gpr', quiet=False)
    assert_not_substring("Malicious cmd called", p.out)
    assert_match(
        (
            r"ERROR: Configuration key 'editor\.cmd' must be set globally\."
            r".*ERROR: 'editor\.cmd' is ignored"
        ),
        p.out,
    )
    assert_match(r".*arg1 abproject1\.gprab arg3", p.out)

print('SUCCESS')
