"""
Check that improper profiles in the command-line don't slip by
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_match

init_local_crate()


def check(args, msg: str):
    p = run_alr(*args, complain_on_error=False)
    assert p.status != 0, f"Command 'alr {args}' should have errored"
    assert_match(f".*{msg}.*", p.out)


#  Conflicting use of wildcards
check(["build", "--profiles=*=release,%=release"], msg="Only one of")

#  Duplicate wildcard
check(["build", "--profiles=*=release,*=release"], msg="Only one of")

#  Invalid profile name
check(["build", "--profiles=*=rilis"], msg="Invalid profile value")

#  Duplicated crate
check(["build", "--profiles=xxx=release,xxx=release"], msg="Duplicated crate")


print('SUCCESS')
