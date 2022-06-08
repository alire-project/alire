"""
Test that style checks are disabled in generated code
"""

from drivers.alr import run_alr

import os

# Get and check post fetch action
run_alr('get', 'hello_world')
os.chdir("hello_world_0.1.0_filesystem/")
run_alr('build', '--validation')

print('SUCCESS')
