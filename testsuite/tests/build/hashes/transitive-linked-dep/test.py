"""Test that build hashes reflect changes in transitive linked dependencies."""

from glob import glob
from pathlib import Path

from drivers.alr import alr_pin, alr_settings_set, alr_with, init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_match, assert_not_substring
from drivers.builds import path as builds_path
from drivers.helpers import (
    MockCommand,
    append_to_file,
    on_windows,
    prepend_to_file,
    replace_in_file,
)

test_root = Path.cwd()
libhello_manifest = test_root / "pinned_libhello" / "alire.toml"


# Create a crate with dependencies `xxx -> hello -> libhello`, and pin
# `libhello` to a local path.
init_local_crate()
alr_with("hello")
alr_pin("libhello", path="../pinned_libhello")
# Make `xxx` actually depend on `hello`.
main_file = test_root / "xxx" / "src" / "xxx.adb"
prepend_to_file(main_file, ["with Hello;"])
replace_in_file(main_file, "null;", "Hello;")


def count_hello_dirs() -> int:
    """Count the number of shared build directories for the `hello` crate."""
    return len(glob(f"{builds_path()}/hello*/*"))


def assert_new_hash() -> None:
    """Build and assert that hello is compiled in a new build directory."""
    expected = count_hello_dirs() + 1
    p = run_alr("build", quiet=False)
    actual = count_hello_dirs()
    assert actual == expected, "new shared build directory not created"
    # Check `gprbuild` reports a rebuild of `hello.adb`.
    assert_match(r".*\[Ada\]\s+hello\.adb\n", p.out)


def assert_build_reused() -> None:
    """Build and assert that no new build dir is created and hello is not recompiled."""
    expected = count_hello_dirs()
    p = run_alr("build", quiet=False)
    actual = count_hello_dirs()
    assert actual == expected, "new shared build directory created unexpectedly"
    # Check there was no `[Ada]   hello.adb` from `gprbuild`.
    assert_not_substring("hello.adb", p.out)


# The first `alr build` will put `hello` in a new shared build directory,
# with a dependence on the pinned `libhello`.
assert_new_hash()
assert_eq("Hello world, from the pinned libhello!\n", run_alr("run").out)

# Changing a GPR external should rebuild hello in a new directory.
replace_in_file(libhello_manifest, 'SET_LOCALLY = "val_A"', 'SET_LOCALLY = "val_B"')
assert_new_hash()
# Reverting the change should reuse the previous build.
replace_in_file(libhello_manifest, 'SET_LOCALLY = "val_B"', 'SET_LOCALLY = "val_A"')
assert_build_reused()

# An external set by the root crate should yield the same.
root_manifest = test_root / "xxx" / "alire.toml"
append_to_file(root_manifest, ["[gpr-set-externals]", 'SET_BY_ROOT = "val_C"'])
assert_new_hash()
replace_in_file(root_manifest, 'SET_BY_ROOT = "val_C"', 'SET_BY_ROOT = "val_D"')
assert_new_hash()
replace_in_file(root_manifest, '[gpr-set-externals]\nSET_BY_ROOT = "val_D"', "")
assert_build_reused()

# An environment variable change should give a new hash.
replace_in_file(libhello_manifest, 'MY_ENV.set = "env_one"', 'MY_ENV.set = "env_two"')
assert_new_hash()
replace_in_file(libhello_manifest, 'MY_ENV.set = "env_two"', 'MY_ENV.set = "env_one"')
assert_build_reused()
append_to_file(root_manifest, ["[environment]", 'MY_ENV.append = "env_root"'])
assert_new_hash()
replace_in_file(root_manifest, '[environment]\nMY_ENV.append = "env_root"', "")
assert_build_reused()

# Changing the compiler should give a new hash.
#
# The `gnat_external` in `build_hash_index` has `version-command = ["echo", "1.0"]`;
# we simulate a compiler change by mocking it to return `1.2` instead.
if on_windows():
    alr_settings_set("msys2.install_dir", str(test_root / "nonexistent"))
with MockCommand("echo", "print('1.2')", "mock_cmd_dir"):
    assert_new_hash()
assert_build_reused()


print("SUCCESS")
