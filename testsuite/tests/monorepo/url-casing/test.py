"""
Check two crates from the same monorepo with differently cased URLs can coexist.
"""

from pathlib import Path
from subprocess import run
from textwrap import dedent

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_eq
from drivers.helpers import fs_folds_case, init_git_repo, write_version_crate

CRATES = ["crate_a", "crate_b"]

START_DIR = Path.cwd()
INDEX = START_DIR / "my_index"


def add_manifest(crate: str, url: Path, commit: str) -> None:
    """Add a release to the local index, located in a monorepo at url."""
    manifest_dir = INDEX / crate[:2] / crate
    manifest_dir.mkdir(parents=True, exist_ok=True)
    (manifest_dir / f"{crate}-1.0.0.toml").write_text(
        dedent(
            f"""\
            name = "{crate}"
            version = "1.0.0"
            description = "Reports its own version"
            maintainers = ["some@one.com"]

            [origin]
            url = 'git+file:{url}'
            commit = "{commit}"
            subdir = "{crate}"
            """
        )
    )


# Create monorepo with two crates
for crate in CRATES:
    write_version_crate(crate, "1.0.0", into=START_DIR / "MonoRepo" / crate)
commit = init_git_repo(START_DIR / "MonoRepo")

# Publish the two crates with differently cased URLs.
#
# On case-insensitive filesystems, the lowercase URL reaches the same
# repository by case folding; otherwise we materialize it as a clone.
if not fs_folds_case(START_DIR):
    run(
        ["git", "clone", START_DIR / "MonoRepo", START_DIR / "monorepo"],
        capture_output=True,
        check=True,
    )
add_manifest("crate_a", START_DIR / "MonoRepo", commit)
add_manifest("crate_b", START_DIR / "monorepo", commit)

# Check that a workspace depending on both crates works. On case-insensitive
# filesystems, crate_b reuses crate_a's checkout, which deployment must not
# mistake for an error condition.
init_local_crate()
run_alr("with", "crate_a")
run_alr("with", "crate_b")
Path("src", "xxx.adb").write_text(
    dedent(
        """\
        with Ada.Text_IO;
        with Crate_A;
        with Crate_B;

        procedure Xxx is
        begin
           Ada.Text_IO.Put_Line (Crate_A.Version);
           Ada.Text_IO.Put_Line (Crate_B.Version);
        end Xxx;
        """
    )
)

p = run_alr("run")
assert_eq("1.0.0\n1.0.0\n", p.out)


print("SUCCESS")
