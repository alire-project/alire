from drivers.alr import init_local_crate, run_alr
import os, subprocess

HOST = "github.com"
INDEX = "git@github.com:alire-project/test-index.git"
KNOWN_HOSTS_DIRECTORY = "~/.ssh"

# Test setup.
os.makedirs(KNOWN_HOSTS_DIRECTORY, exist_ok=True)
with open(KNOWN_HOSTS_DIRECTORY + "/known_hosts", "a+") as known_hosts:
    if HOST not in known_hosts.read():
        host_key = subprocess.run(["ssh-keyscan", HOST], capture_output=True).stdout
        known_hosts.write(f"{host_key}\n")
init_local_crate()

# Ensure we can add an index using ssh.
run_alr("index", "--name", "test", "--add", INDEX, complain_on_error=True)
run_alr("index", "--check", complain_on_error=True)

# Ensure we can pin a crate using ssh.
run_alr("with", "libhello", "--use", "git@github.com:alire-project/libhello.git")

# Test teardown.
run_alr("index", "--del", "test", complain_on_error=True)

print("SUCCESS")
