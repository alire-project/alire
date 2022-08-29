"""
Verify that recursive pins work for remote urls (simulated as local git remotes
using absolute paths)
"""

from drivers.alr import run_alr, alr_pin, init_local_crate
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import init_git_repo, dir_separator

import re
import os

#  We are going to setup xxx --> yyy --> zzz, where xxx and zzz live at the
#  same level, and yyy is at ./nest/yyy. Both yyy and zzz will be git
#  repositories, so we refer to them by their absolute path (as if they were
#  remote URLs)

# zzz crate/repo
init_local_crate(name="zzz", enter=False)
path_zzz = os.path.join(os.getcwd(), "zzz")
init_git_repo(path_zzz)

# yyy crate/repo
os.mkdir("nest")
os.chdir("nest")
init_local_crate(name="yyy")
alr_pin("zzz", url=path_zzz)
os.chdir("..")
path_yyy = os.path.join(os.getcwd(), "yyy")
init_git_repo(path_yyy)

# xxx crate
os.chdir("..")
init_local_crate()
alr_pin("yyy", url=path_yyy)

# Should work properly
p = run_alr("pin")

# Absolute path to simulate a remote URL is platform dependent:
s = dir_separator()
assert_match(re.escape('yyy file:alire/cache/pins/yyy ') +  # local rel path
             '.*' + re.escape(f'{s}nest{s}yyy\n') +         # remote abs url
             re.escape('zzz file:alire/cache/pins/zzz ') +  # local rel path
             '.*' + re.escape(f'{s}zzz\n'),                 # remote abs url
             p.out)

print('SUCCESS')
