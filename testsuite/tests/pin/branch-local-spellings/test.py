"""
Check that `alr update` does not re-clone a local branch pin when the URL
hasn't changed.
"""

import os
import shutil
import subprocess
from pathlib import Path
from typing import Dict

from drivers.alr import alr_pin, alr_unpin, init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_not_substring, assert_substring
from drivers.helpers import git_branch, init_git_repo, on_windows, which


# Initialise two "remote" git repos.
init_local_crate(name="upstream", enter=False)
init_git_repo("upstream")
branch = git_branch("upstream")
upstream = Path.cwd() / "upstream"
upstream_other = Path.cwd() / "upstream_other"
shutil.copytree(upstream, upstream_other)

pin_dir = Path("alire", "cache", "pins", "upstream")
marker = pin_dir / "marker.txt"


def cygwin_tools_dir() -> "Path | None":
    """Directory of the `git` on PATH if it is from Cygwin/MSYS2, else `None`."""
    if not on_windows():
        return None
    git = which("git")
    if not git:
        return None
    tools_dir = Path(git).parent
    if all((tools_dir / f"{tool}.exe").is_file() for tool in ("cygpath", "mount")):
        return tools_dir
    return None


CYGWIN_TOOLS = cygwin_tools_dir()


def cygwin_tool(name: str, *args: str) -> str:
    """Run a Cygwin tool from CYGWIN_TOOLS and return its stripped stdout."""
    assert CYGWIN_TOOLS is not None
    p = subprocess.run(
        [CYGWIN_TOOLS / f"{name}.exe", *args],
        capture_output=True,
        text=True,
        check=True,
    )
    return p.stdout.strip()


def cygdrive_spelling(path: Path) -> str:
    """The `/cygdrive/c/...` (or similar) spelling of an absolute native path."""
    # `mount -p` prints a header line, then the prefix as the first column.
    prefix = cygwin_tool("mount", "-p").splitlines()[-1].split()[0].rstrip("/")
    drive = path.drive.rstrip(":").lower()
    spelling = f"{prefix}/{drive}/{path.relative_to(path.anchor).as_posix()}"

    # Check that Cygwin agrees this denotes our native path.
    assert_eq(str(path), cygwin_tool("cygpath", "-w", spelling))
    return spelling


def spellings(path: Path) -> Dict[str, str]:
    """
    Spellings of the same local repo: absolute vs relative; native path
    separators vs forward slashes vs `/cygdrive/...` (Cygwin/MSYS2 only); bare
    vs `git+file:` prefix.
    """
    result = {}
    for kind, p in [("abs", path), ("rel", Path("..", path.name))]:
        forms = [("native", str(p)), ("posix", p.as_posix())]
        if kind == "abs" and CYGWIN_TOOLS:
            forms.append(("cygdrive", cygdrive_spelling(p)))
        for sep, spelling in forms:
            result[f"{kind}_{sep}"] = spelling
            result[f"{kind}_git_file_{sep}"] = f"git+file:{spelling}"
    return result


for label, url in spellings(upstream).items():
    init_local_crate(f"client_{label}")

    # Pinning deploys the checkout for the first time.
    alr_pin("upstream", url=url, branch=branch)
    assert pin_dir.is_dir()

    # Leave a telltale that would not survive a fresh clone.
    marker.write_text("still here\n")

    # Updating must pull the existing checkout, not re-clone it.
    p = run_alr("update", quiet=False)
    assert_not_substring("Switching pin", p.out)
    assert_substring("Pulling upstream", p.out)
    assert marker.is_file()

    # An actual change of URL results in a fresh clone.
    new_url = spellings(upstream_other)[label]
    alr_unpin("upstream", update=False)
    alr_pin("upstream", url=new_url, branch=branch, update=False)
    p = run_alr("update", quiet=False)
    assert_substring("Switching pin", p.out)
    assert not marker.is_file()

    os.chdir("..")


print("SUCCESS")
