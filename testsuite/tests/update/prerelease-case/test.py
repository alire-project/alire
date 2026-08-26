"""
Verify that cache directory collisions on case-insensitive filesystems fail loudly.
"""

from pathlib import Path
import re
from textwrap import dedent

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import fs_folds_case, write_version_crate

CRATE = "dir_crate"
LOWER = "1.0.0-pre"
UPPER = "1.0.0-PRE"

START_DIR = Path.cwd()

CASE_INSENSITIVE = fs_folds_case(START_DIR)

CRATE_DIRS = {LOWER: START_DIR / "src_lower", UPPER: START_DIR / "src_upper"}
# The manifests cannot coexist in the same index on a case-insensitive filesystem.
INDEXES = {LOWER: START_DIR / "index_lower", UPPER: START_DIR / "index_upper"}


# Create two releases differing only in version prerelease casing.
for version in (LOWER, UPPER):
    crate_dir = CRATE_DIRS[version]
    write_version_crate(CRATE, version, into=crate_dir)
    manifest_dir = INDEXES[version] / CRATE[:2] / CRATE
    manifest_dir.mkdir(parents=True, exist_ok=True)
    (manifest_dir / f"{CRATE}-{version}.toml").write_text(
        dedent(
            f"""\
            name = "{CRATE}"
            version = "{version}"
            description = "Reports its own version"
            maintainers = ["some@one.com"]

            [origin]
            url = 'file:{crate_dir}'
            """
        )
    )

# Create a workspace whose main program prints the version reported by the
# dependency's deployed sources.
init_local_crate()
Path("src", "xxx.adb").write_text(
    dedent(
        f"""\
        with Ada.Text_IO;
        with {CRATE.title()};

        procedure Xxx is
        begin
           Ada.Text_IO.Put_Line ({CRATE.title()}.Version);
        end Xxx;
        """
    )
)


def check_version(version: str) -> None:
    # Force a full rebuild, so this check only depends on the deployed
    # sources and not on `gprbuild`'s staleness detection
    run_alr("build", "--", "-f")
    p = run_alr("run", "--skip-build")
    assert_eq(f"{version}\n", p.out)


# Test switching between the two releases.
run_alr("with", f"{CRATE}={LOWER}")
check_version(LOWER)

run_alr("with", "--del", CRATE)

if CASE_INSENSITIVE:
    p = run_alr("with", f"{CRATE}={UPPER}", complain_on_error=False)
    assert_match(
        rf".*Cannot deploy {re.escape(CRATE)}={re.escape(UPPER)} into "
        rf"'.*(/|\\){re.escape('dir_crate_1.0.0_PRE_filesystem')}': a release "
        rf"whose version differs only in character case is already deployed "
        rf"there, and the filesystem at that location is case-insensitive\.",
        p.out
    )
else:
    run_alr("with", f"{CRATE}={UPPER}")
    check_version(UPPER)

    run_alr("with", "--del", CRATE)
    run_alr("with", f"{CRATE}={LOWER}")
    check_version(LOWER)

print("SUCCESS")
