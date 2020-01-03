"""
Test that a mismatched branch in the community index is detected
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import os

# Initialize a git repo in the index location, so it returns a 'master' branch

# To do so, first retrieve the index location in the test config
p = run_alr('index', '--list')
location = p.out.split()[7]

# To avoid git complaints about missing info
gitconfig = '-c user.name="Fubar Snafu" -c user.email="fubar@snafu.org"'

# Create master branch by adding the files in the index
start = os.getcwd()
os.chdir(location)
os.system('git ' + gitconfig + ' init -q .')
os.system('git ' + gitconfig + ' add .')
os.system('git ' + gitconfig + ' commit -q -m initialize')
os.chdir(start)

# Run the test. No alr version should use 'master' for the community index
p = run_alr('list',  # Causes loading of the index
            complain_on_error=False)

assert_match('.*Mismatched branch in checked out community index.*',
             p.out)


print('SUCCESS')
