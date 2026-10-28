"""
Check that two crates from forks of a monorepo, at the same commit, get their
own checkouts instead of evicting each other from a shared one.
"""

import os
from glob import glob
from pathlib import Path
from subprocess import run
from textwrap import dedent

from drivers import builds
from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_not_substring
from drivers.helpers import init_git_repo, write_version_crate, URL_HASH_GLOB

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


# Create a monorepo with two crates, and a fork of it: same repository name
# and commit, different URL
upstream = START_DIR / "upstream" / "monorepo"
fork = START_DIR / "fork" / "monorepo"
for crate in CRATES:
    write_version_crate(crate, "1.0.0", into=upstream / crate)
commit = init_git_repo(upstream)
run(["git", "clone", upstream, fork], capture_output=True, check=True)

add_manifest("crate_a", upstream, commit)
add_manifest("crate_b", fork, commit)

# A workspace depending on both crates works, without either checkout being
# taken for a foreign one
init_local_crate()
output = run_alr("with", "crate_a", quiet=False).out
output += run_alr("with", "crate_b", quiet=False).out
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
output += run_alr("build", quiet=False).out
assert_not_substring("does not match", output)

p = run_alr("run")
assert_eq("1.0.0\n1.0.0\n", p.out)

# Each fork has its own checkout
checkouts = glob(os.path.join(builds.deps_dir(),
                              f"monorepo_{URL_HASH_GLOB}_{commit[:8]}"))
assert len(checkouts) == 2, f"Unexpected checkouts: {checkouts}"

# Nothing is deployed again on later builds
output = run_alr("build", quiet=False).out
assert_not_substring("Deploying", output)
assert_not_substring("does not match", output)

print("SUCCESS")
