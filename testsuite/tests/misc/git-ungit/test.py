"""
Verify the proper "ungitting" of git origins
"""

import os
import shutil

from drivers.alr import alr_with, crate_dirname, init_local_crate, run_alr
from drivers.asserts import assert_file_exists

# By default, git deployments are shallow and see their .git directory removed
# Check that and that enabling legacy behavior works

cwd = os.getcwd()
foo_dir = crate_dirname("libfoo")

# By default .git should not be there
for wanted in [False, True]:
    run_alr("get", "libfoo")

    # Check root gotten crate
    assert_file_exists(os.path.join(foo_dir, ".git"),
                       wanted=wanted)

    # Check as dependency
    init_local_crate()
    alr_with("libfoo")

    assert_file_exists(os.path.join("alire", "cache", "dependencies",
                                    foo_dir, ".git"),
                       wanted=wanted)

    if not wanted:
        # Enable for next round
        run_alr("config", "--global", "--set",
                "dependencies.git.keep_repository", "true")

        # Cleanup for next round
        os.chdir(cwd)
        shutil.rmtree(crate_dirname("libfoo"))
        shutil.rmtree("xxx")

print('SUCCESS')
