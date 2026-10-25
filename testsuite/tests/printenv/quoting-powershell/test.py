"""
Test that `alr printenv --powershell` prints plain ASCII that PowerShell takes
literally, so the values come out as alr sets them for its own subprocesses
whatever encoding PowerShell reads the output with: UTF-8 (pwsh), ANSI
(Windows PowerShell reads scripts without a BOM as cp1252) or the console's.
Needs PowerShell 7 (pwsh), which can decode all of them.
"""

import os
import shutil
import subprocess

from drivers.alr import init_local_crate, run_alr, set_environ_in_manifest
from drivers.asserts import assert_eq

VALUES = {
    "TEST_QUOTES": "it's \"quoted\"",
    "TEST_SUBST": "$(touch pwned) `touch pwned` $HOME",
    "TEST_LINES": "line 1\nline 2",
    "TEST_TYPOGRAPHIC": "‘single’ ‚low‛",
    "TEST_UNICODE": "ő € 中 \U0001F600",
    # In cp1252, the second byte of 'ő' (C5 91) decodes to '‘'
    "TEST_MISREAD": "ő; touch pwned #",
}

# Read env.ps1 with the given encoding, run it and print a variable
PS_READ = """
[System.Text.Encoding]::RegisterProvider(
    [System.Text.CodePagesEncodingProvider]::Instance)
$text = [IO.File]::ReadAllText(
    "env.ps1", [Text.Encoding]::GetEncoding("{encoding}"))
Invoke-Expression $text
[Console]::Write($env:{name})
"""

if not shutil.which("pwsh"):
    print("SKIP: PowerShell 7 (pwsh) is not available")
else:
    init_local_crate(update=False)
    set_environ_in_manifest(VALUES)

    output = run_alr("printenv", "--powershell").out
    assert output.isascii(), f"PowerShell output is not ASCII:\n{output}"
    with open("env.ps1", "w", encoding="ascii") as f:
        f.write(output)

    for encoding in ["utf-8", "windows-1252", "ibm850"]:
        for name, value in VALUES.items():
            with open("read.ps1", "w") as f:
                f.write(PS_READ.replace("{encoding}", encoding)
                               .replace("{name}", name))
            out = subprocess.run(
                ["pwsh", "-NoProfile", "-NonInteractive", "-File",
                 "read.ps1"],
                capture_output=True, encoding="utf-8", check=True).stdout
            assert_eq(value, out, label=f"{name} read as {encoding}")
    assert not os.path.exists("pwned"), "printenv output ran a command"

    print("SUCCESS")
