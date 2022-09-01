"""
Test the output of the `alr show --dependents` switch
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq

# These test rely on these dependency chains:
# crate3 -> crate2a -> crate1
# crate3 -> drate2b -> crate1

# Invalid switch value
p = run_alr("show", "--dependents=bad", "crate1", complain_on_error=False)
assert_eq("ERROR: --dependents invalid value: bad\n", p.out)

# Default switch value
p = run_alr("show", "--dependents", "crate1")
assert_eq("""CRATE    VERSION  DEPENDENCY
crate2a  2.0.0    ~1.0.0
crate2b  2.0.0    ^1.0.0
""", p.out)

# Explicit direct switch value
p = run_alr("show", "--dependents=direct", "crate1")
assert_eq("""CRATE    VERSION  DEPENDENCY
crate2a  2.0.0    ~1.0.0
crate2b  2.0.0    ^1.0.0
""", p.out)

# Shortest dependency
p = run_alr("show", "--dependents=shortest", "crate1")
assert_eq("""CRATE    VERSION  DEPENDENCY  CHAIN
crate2a  2.0.0    ~1.0.0      crate2a=2.0.0,crate1=1.0.0
crate2b  2.0.0    ^1.0.0      crate2b=2.0.0,crate1=1.0.0
crate3   3.0.0    ~1.0.0      crate3=3.0.0,crate2a=2.0.0,crate1=1.0.0
""", p.out)

# All dependency chains
p = run_alr("show", "--dependents=all", "crate1")
assert_eq("""CRATE    VERSION  DEPENDENCY  CHAIN
crate2a  2.0.0    ~1.0.0      crate2a=2.0.0,crate1=1.0.0
crate2b  2.0.0    ^1.0.0      crate2b=2.0.0,crate1=1.0.0
crate3   3.0.0    ~1.0.0      crate3=3.0.0,crate2a=2.0.0,crate1=1.0.0
crate3   3.0.0    ^1.0.0      crate3=3.0.0,crate2b=2.0.0,crate1=1.0.0
""", p.out)

# Partial chain to crate2a
p = run_alr("show", "--dependents=all", "crate2a")
assert_eq("""CRATE   VERSION  DEPENDENCY  CHAIN
crate3  3.0.0    *           crate3=3.0.0,crate2a=2.0.0
""", p.out)

# Empty output for crate3
p = run_alr("show", "--dependents=all", "crate3")
assert_eq("""CRATE  VERSION  DEPENDENCY  CHAIN
""", p.out)

print('SUCCESS')
