"""
Test --github switch functionality in alr init command
"""

import shutil

from drivers.alr import run_alr, alr_settings_set
from os.path import exists

# Helper functions
def check_with_github_files(crate_path):
    """Check that GitHub files were created in the specified crate directory"""
    assert exists(f"{crate_path}/.github"), "GitHub directory not created"
    assert exists(f"{crate_path}/.github/workflows"), "GitHub workflows directory not created"
    assert exists(f"{crate_path}/.github/workflows/publish.yml"), "Publish workflow not created"
    assert exists(f"{crate_path}/.github/workflows/selftest.yml"), "Self-test workflow not created"
    assert exists(f"{crate_path}/README.md"), "README.md not created"


def check_no_github_files(crate_path):
    """Check that GitHub files were not created in the specified crate directory"""
    assert not exists(f"{crate_path}/.github"), "GitHub directory should not be created"
    assert not exists(f"{crate_path}/README.md"), "README.md should not be created"


# Test --github=true generates GitHub files
run_alr("init", "--bin", "--github=true", "test_crate")
check_with_github_files("test_crate")

# Test --github=false does not generate GitHub files
shutil.rmtree("test_crate")
run_alr("init", "--bin", "--github=false", "test_crate")
check_no_github_files("test_crate")

# Test default behavior (no --github switch)
shutil.rmtree("test_crate")
run_alr("init", "--bin", "test_crate")
check_no_github_files("test_crate")
shutil.rmtree("test_crate")

# Test --github switch without explicit value
run_alr("init", "--bin", "--github", "test_crate")
check_with_github_files("test_crate")
shutil.rmtree("test_crate")

# Test implicit enabling via settings
alr_settings_set("init.github_files", "true")
run_alr("init", "--bin", "test_crate")
check_with_github_files("test_crate")
shutil.rmtree("test_crate")

# Test explicit disabling via --github=false when enabled in settings
run_alr("init", "--bin", "--github=false", "test_crate")
check_no_github_files("test_crate")

print('SUCCESS')
