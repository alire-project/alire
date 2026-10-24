"""
Suggest closest command if user misstypes
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq

# test for common spelling mistakes and misstypes
#
misstypes = [
    ("versiom", "version"),
    ("serach", "search"),
    ("self-updated", "self-update"),
    ("alr-update", "self-update"),
    ("alrupdate", "update"),
    ("rum", "run"),
]

for e in misstypes:
    result_template = f"""ERROR: Unrecognized command: {e[0]}

 Did you mean '{e[1]}'?
"""

    result = run_alr(e[0], quiet=True, complain_on_error=False)

    assert_eq(result.out, result_template)


# Test "malicious" misstypes or non existsing commands

misstypes2 = ["this_command-doesNot-Exist", "kgzkgzgkzlgddg"]

alr_help = run_alr("", quiet=True, complain_on_error=False).out

for e in misstypes2:
    alr_result = run_alr(e, quiet=True, complain_on_error=False)

    # The help messaage from `alr help`, `alr` and "alr unknowncommand" differ
    # at the beginning. We have to clean them up and preppend our error line.
    result = str(f"ERROR: Unrecognized command: {e}\n\n") + "".join(
        str(alr_help).splitlines(keepends=True)[2:]
    )

    assert_eq(str(alr_result.out), result)

print("SUCCESS")
