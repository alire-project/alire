"""
A shared monorepo checkout whose recorded origin does not match the release
being deployed (e.g. one planted by another crate with a colliding folder name)
is not reused, but deployed again with a warning.
"""

import os
from glob import glob

from drivers import builds
from drivers.alr import run_alr, init_local_crate, alr_with, alr_publish
from drivers.asserts import assert_match, assert_substring
from drivers.helpers import init_git_repo, URL_HASH_GLOB
from subprocess import run

# A crate nested in a repository, published to the test index

start_dir = os.getcwd()
os.mkdir("monoproject.upstream")
os.chdir("monoproject.upstream")
init_local_crate("mycrate", enter=False, with_maintainer_login=True)
os.chdir(start_dir)
commit = init_git_repo("monoproject.upstream")
assert run(["git", "clone",
            "monoproject.upstream", "monoproject"]).returncode == 0
os.chdir(os.path.join("monoproject", "mycrate"))
alr_publish("mycrate", "0.1.0-dev",
            index_path=os.path.join(start_dir, "my_index"))

# Used as a dependency, its checkout records its origin (in shared mode, the
# checkout in the vault is only deployed when building)

os.chdir(start_dir)
init_local_crate("top")
alr_with("mycrate")
run_alr("build")

origin_flag = glob(os.path.join(builds.deps_dir(),
                                f"monoproject_{URL_HASH_GLOB}_{commit[:8]}",
                                "alire", "flags", "monorepo_data.toml"))[0]
with open(origin_flag) as f:
    recorded = f.read()
assert_substring(f'commit = "{commit}"', recorded)
assert_substring("monoproject.upstream", recorded)

# A checkout recorded for another origin is deployed again

with open(origin_flag, "w") as f:
    f.write('commit = "' + commit + '"\n'
            'url = "git+https://example.com/someone/monoproject.git"\n')

# Depending on the build mode, the checkout is deployed when updating or when
# building
output = (run_alr("update", quiet=False).out
          + run_alr("build", quiet=False).out)
assert_match(".*Warning: The checkout at .*monoproject_[0-9a-f]{8}_"
             + commit[:8] + " does not match the origin of mycrate=0.1.0-dev,"
             " deploying it again", output)

with open(origin_flag) as f:
    assert f.read() == recorded, "Origin not recorded again"

print("SUCCESS")
