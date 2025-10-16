"""
Test `alr run -a` argument splitting
"""

from drivers.alr import run_alr, CalledProcessError
from drivers.asserts import assert_match
from subprocess import run

import os, re, shutil

target = 'echo_arguments_1.0.0_filesystem'
alr_path = os.environ['ALR_PATH']

def check_run(arguments, match="", should_fail=False):
    if not should_fail:
        p = run_alr('run', '-a', arguments, quiet=not match, complain_on_error=not should_fail)
        if match:
            assert_match(match, p.out, flags=re.S)
    else:
        # Need to check stderr on failure, so using subprocess.run
        p = run([alr_path, "run", "-a", arguments], capture_output=True)
        if 0 == p.returncode:
            raise CalledProcessError
        if match:
            assert_match(match, p.stderr.decode(), flags=re.S)

p = run_alr('get', 'echo_arguments', quiet=True)
os.chdir(target)

# Split on spaces
check_run('code example with spaces', match=".*'code'\n'example'\n'with'\n'spaces'")

# Escaped spaces
check_run('code\\ example\\ with\\ spaces', match=".*'code example with spaces'")

# Unnecessary escapes
check_run('\\c\\o\\d\\e \\example \\with \\s\\p\\a\\c\\e\\s', match=".*'code'\n'example'\n'with'\n'spaces'")

# Double quotes
check_run('code "example with" spaces', match=".*'code'\n'example with'\n'spaces'")

# Single quotes
check_run('code \'example with\' spaces', match=".*'code'\n'example with'\n'spaces'")

# Escaped double quotes
check_run('code \\"example with\\" spaces', match=".*'code'\n'\"example'\n'with\"'\n'spaces'")

# Escaped single quotes
check_run('code \\\'example with\\\' spaces', match=".*'code'\n'\\\\'example'\n'with\\\\''\n'spaces'")

# Nested escaped double quotes
check_run('code \"example \\\" with\" spaces', match=".*'code'\n'example \" with'\n'spaces'")

# Escaped closing single quote (closing quote cannot be escaped)
check_run('code \'example \\\' with spaces', match=".*'code'\n'example \\\\\\\\'\n'with'\n'spaces'")

# Nested double & single quotes
check_run('code \"example \'with spaces\'\"', match=".*'code'\n'example \\\\'with spaces\\\\''")

# Unterminated escape should fail
check_run('code example with spaces\\', match=".*Unterminated escape sequence in command:.*", should_fail=True)

# Unterminated single quote should fail
check_run('code example with \'spaces', match=".*Unterminated single quote sequence in command:.*", should_fail=True)

# Unterminated double quote should fail
check_run('code example with \"spaces', match=".*Unterminated double quote sequence in command:.*", should_fail=True)

os.chdir('..')
shutil.rmtree(target)

print('SUCCESS')
