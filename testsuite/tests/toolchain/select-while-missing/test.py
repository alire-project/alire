"""
Check a particular error in which selecting a compiler via `alr toolchain
--select <compiler>` fails if the previously configured compiler is missing on
disk.
"""

from drivers.alr import run_alr

# Configure a valid compiler
run_alr("toolchain", "--select", "gnat_native", "gprbuild")

# Configure an invalid compiler
run_alr("config", "--global", "--set", "toolchain.use.gnat", "gnat_nono=1.2.3")

# This must succeed
run_alr("toolchain", "--select", "gnat_native=1.0")

print("SUCCESS")
