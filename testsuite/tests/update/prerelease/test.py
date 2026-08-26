"""
Test switching between versions that differ only in their pre-release part.
"""

import hashlib
import tarfile
from pathlib import Path
from textwrap import dedent

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_eq
from drivers.helpers import commit_all, init_git_repo, write_version_crate

VERSIONS = ["1.0.0-rc1", "1.0.0-rc2", "1.0.0"]

START_DIR = Path.cwd()
INDEX = START_DIR / "my_index"


def add_manifest(crate: str, version: str, origin: str) -> None:
    """Add a release to the local index, with the given [origin] contents."""
    manifest_dir = INDEX / crate[:2] / crate
    manifest_dir.mkdir(parents=True, exist_ok=True)
    (manifest_dir / f"{crate}-{version}.toml").write_text(
        dedent(
            f"""\
            name = "{crate}"
            version = "{version}"
            description = "Reports its own version"
            maintainers = ["some@one.com"]

            [origin]
            """
        )
        + origin
    )


# A crate in a plain git repository
repo = START_DIR / "git_crate_repo"
repo.mkdir()
init_git_repo(repo)
for version in VERSIONS:
    write_version_crate("git_crate", version, into=repo)
    commit = commit_all(repo)
    add_manifest(
        "git_crate", version, f"url = 'git+file:{repo}'\ncommit = '{commit}'\n"
    )

# A crate within a monorepo
repo = START_DIR / "monorepo"
repo.mkdir()
init_git_repo(repo)
for version in VERSIONS:
    write_version_crate(
        "monorepo_crate", version, into=repo / "crates" / "monorepo_crate"
    )
    commit = commit_all(repo)
    add_manifest(
        "monorepo_crate",
        version,
        f"url = 'git+file:{repo}'\n"
        f'commit = "{commit}"\nsubdir = "crates/monorepo_crate"\n',
    )

# A crate in a plain directory
for version in VERSIONS:
    src_dir = START_DIR / f"dir_crate_{version}"
    write_version_crate("dir_crate", version, into=src_dir)
    add_manifest("dir_crate", version, f"url = 'file:{src_dir}'\n")

# A crate in a local source archive
for version in VERSIONS:
    src_dir = START_DIR / f"archive_crate_{version}"
    write_version_crate("archive_crate", version, into=src_dir)
    archive = START_DIR / f"archive_crate-{version}.tgz"
    with tarfile.open(archive, "w:gz") as tar:
        tar.add(src_dir, arcname=src_dir.name)
    digest = hashlib.sha512(archive.read_bytes()).hexdigest()
    add_manifest(
        "archive_crate",
        version,
        f"url = 'file:{archive}'\nhashes = ['sha512:{digest}']\n",
    )


def switch_and_check(crate: str, version: str, previous: "str | None") -> None:
    """
    Replace the workspace's dependency (`previous`, if any) with crate=version
    and check the version reported.
    """
    if previous is not None:
        run_alr("with", "--del", previous)
    run_alr("with", f"{crate}={version}")

    # Force a full rebuild, so this check only depends on the deployed
    # sources and not on `gprbuild`'s staleness detection (which can be
    # confused when file timestamps are identical)
    run_alr("build", "--", "-f")

    p = run_alr("run", "--skip-build")
    assert_eq(f"{version}\n", p.out, label=f"run output for {crate}={version}")


# Create a workspace which prints the dependency's version, and cycle through
# crate/version combinations.
init_local_crate()
dependency = None  # The name of the workspace's current dependency, if any

for crate in ("git_crate", "monorepo_crate", "dir_crate", "archive_crate"):
    unit = crate.title()
    Path("src", "xxx.adb").write_text(
        dedent(f"""\
            with Ada.Text_IO;
            with {unit};

            procedure Xxx is
            begin
               Ada.Text_IO.Put_Line ({unit}.Version);
            end Xxx;
            """)
    )

    for version in (*VERSIONS, VERSIONS[0]):
        switch_and_check(crate, version, previous=dependency)
        dependency = crate


print("SUCCESS")
