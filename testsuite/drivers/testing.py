"""
Helpers for exercising the built-in `alr test` runner: writing test sources
that declare themselves with `pragma Alire_Test`, and inspecting the runner's
structured output.
"""

import json
import os

from drivers.alr import DEFAULT_CRATE_NAME


def write_test(stem: str, body: str, prelude: str = "",
               crate: str = DEFAULT_CRATE_NAME,
               declare: bool = True) -> None:
    """
    Write a test source file under tests/src/ of `crate`, created with
    `init_local_crate(crate, with_test=True)`. `stem` is the suffix after
    `<crate>_tests-`, e.g. crate "xxx" and stem "named" yields
    tests/src/xxx_tests-named.adb. `prelude` is inserted ahead of the unit
    declaration, where compilation pragmas must live.

    When `declare`, a bare `pragma Alire_Test;` is prepended so the source is
    run as a test.
    """
    unit = f"{crate}_tests"
    proc = f"{unit.title()}.{stem.title()}"
    path = f"./tests/src/{unit}-{stem}.adb"
    if declare:
        prelude = "pragma Alire_Test;\n" + prelude
    with open(path, "w") as f:
        f.write(prelude)
        f.write(f"procedure {proc} is\n")
        f.write(f"begin\n   {body}\nend {proc};\n")


def declare_main_as_test(crate_dir: str) -> None:
    """
    Convert a plain crate's generated main (`<crate_dir>/src/<crate_dir>.adb`)
    into a test, so the built-in runner of the parent crate detects it.
    """
    name = os.path.basename(crate_dir.rstrip("/"))
    main = os.path.join(crate_dir, "src", f"{name}.adb")
    with open(main) as f:
        body = f.read()
    with open(main, "w") as f:
        f.write("pragma Ignore_Pragma (Alire_Test);\n"
                "pragma Alire_Test;\n" + body)


def find_test(tests: list[dict], name: str) -> dict:
    """
    Return the test entry named `name` from a list of JSON test entries, or
    raise AssertionError listing the available names.
    """
    for t in tests:
        if t["name"] == name:
            return t
    raise AssertionError(
        f"no test named {name!r} in {[t['name'] for t in tests]}"
        )


def parse_json_result(p):
    """Return the parsed JSON object from p.out.

    Trace.Error/Warning go to stderr, which e3 merges with stdout, so p.out may
    contain diagnostic lines before the JSON blob. The JSON itself may be
    pretty-printed across multiple lines, so we find the first line that starts
    with '{' and parse from there to end of output.
    """
    lines = p.out.splitlines()
    for i, line in enumerate(lines):
        if line.lstrip().startswith("{"):
            return json.loads("\n".join(lines[i:]))
    raise AssertionError(
        f"no JSON object found in output:\n{p.out}"
    )
