"""
Test `get`ing a tarball release with a different download command configured.
"""


import os
import shutil

from drivers.alr import crate_dirname, fixtures_path, run_alr, alr_settings_set
from drivers.asserts import assert_match, assert_substring
from drivers.helpers import MockCommand, on_windows


# The script for the mock download command. Prints its arguments, then copies
# `libhello_1.0.0.tgz` from fixtures to the specified output file.
COMMAND_SCRIPT = f"""\
import shutil, sys
print(f"Mock command called with arguments: {{sys.argv}}")
shutil.copy(r"{fixtures_path('crates', 'libhello_1.0.0.tgz')}", sys.argv[4])
"""


def set_download_cmd(cmd: str):
    """
    Set the download command in `alr`'s global settings to `cmd`.
    """
    alr_settings_set("origins.archive.download_cmd", cmd)


# Mock `curl` so it always fails, and put the mock download command on `PATH`.
alr_settings_set("msys2.install_dir", os.path.abspath("does_not_exist"),
                 builtin=on_windows())
mock_curl = MockCommand("curl", "raise Exception", "cmd_dir")
mock_download_cmd = MockCommand("command_name", COMMAND_SCRIPT, "cmd_dir")
with mock_curl, mock_download_cmd:
    # Verify that `alr get libhello` fails, because it attempts to call `curl`.
    p = run_alr("get", "libhello", complain_on_error=False)
    assert_match(
        (
            r'.*Command \["curl", "https://some\.host/path/to/archive\.tgz",'
            r' "-L", "-s", "-o", "[^"]*archive.tgz"\] exited with code 1'
        ),
        p.out
    )

    # Check that the default curl command is changed correctly when `-v` is
    # specified.
    p = run_alr("-v", "get", "libhello", complain_on_error=False, quiet=False)
    assert_match(
        (
            r'.*Command \["curl", "https://some\.host/path/to/archive\.tgz",'
            r' "-L", "--progress-bar", "-o", "[^"]*archive.tgz"\] exited with '
            r'code 1'
        ),
        p.out
    )

    # Configure `alr` to use the mock download command instead of `curl`.
    set_download_cmd("command_name arg1 --url=${URL} arg2 ${DEST} arg3")

    # Verify that `alr get libhello` now succeeds, by calling the configured
    # command.
    pattern = (
        r".*Mock command called with arguments: \['[^']*', 'arg1', "
        r"'--url=https://some\.host/path/to/archive\.tgz', 'arg2', "
        r"'[^']*archive.tgz', 'arg3'\]"
    )
    p = run_alr("get", "libhello", quiet=False)
    assert_match(pattern, p.out)
    shutil.rmtree(crate_dirname("libhello"))

    # Verify that changing the logging level doesn't affect the command used.
    p = run_alr("-v", "get", "libhello", quiet=False)
    assert_match(pattern, p.out)
    shutil.rmtree(crate_dirname("libhello"))

    # Verify that replacements are case-insensitive.
    set_download_cmd("command_name arg1 --url=${UrL} arg2 ${DeSt} arg3")
    p = run_alr("get", "libhello", quiet=False)
    assert_match(pattern, p.out)
    shutil.rmtree(crate_dirname("libhello"))

    # Verify that an invalid formatting key raises an appropriate error.
    set_download_cmd("command_name --url=${URL} ${DEST} ${INVALID}")
    p = run_alr("get", "libhello", complain_on_error=False)
    assert_match(r".*Unknown formatting key: INVALID", p.out)

    # Verify that a command not found on `PATH` raises an appropriate error.
    set_download_cmd("non_existent --url=${URL} ${DEST} arg3")
    p = run_alr("get", "libhello", complain_on_error=False)
    assert_substring("'non_existent' not available or not in PATH.", p.out)


print('SUCCESS')
