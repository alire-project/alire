from drivers.alr import init_local_crate, run_alr

ORG="github.com:/alire-project"
INDEX_REPO="test-index"
CRATE_REPO="libhello"
SSH_IMPLICIT_INDEX = f"git@{ORG}/{INDEX_REPO}"
SSH_EXPLICIT_INDEX = f"git+ssh://{SSH_IMPLICIT_INDEX}"

# Test that we can add an index using implicit ssh
run_alr("index", "--name", "implicit", "--add", SSH_IMPLICIT_INDEX)
run_alr("index", "--check")
run_alr("index", "--update-all")  # Check pulling

# Remove so it can be re-added
run_alr("index", "--del", "implicit")

# Test that we can add an index using explicit ssh
run_alr("index", "--name", "explicit", "--add", SSH_EXPLICIT_INDEX)
run_alr("index", "--check")
run_alr("index", "--update-all")

# Test that we can pin a crate using implicit ssh
init_local_crate()
run_alr("with", "libhello", "--use", f"git@{ORG}/{CRATE_REPO}")
run_alr("update")  # Check pulling pin

# Remove and re-pin using explicit ssh
run_alr("with", "--del", "libhello")
run_alr("with", "libhello", "--use", f"git+ssh://git@{ORG}/{CRATE_REPO}")
run_alr("update")


print("SUCCESS")
