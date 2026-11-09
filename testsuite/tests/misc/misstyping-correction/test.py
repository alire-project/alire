"""
notify the user when misstyping a command which one is the most likely one.
"""

from drivers.alr import run_alr

commands = [("rin", "pin"), # rin and pun eval to pin
            ("pun", "pin"), # their distance is both 1

            ("buplish", "publish"), # mix up two letters
            ("tolchains", "toolchain"), # remove and add a letter
            ("tollchan", "toolchain")] # misstype and remove letter


for command in commands:
    recieved_result = run_alr(command[0], complain_on_error=False)
    expected_result = f"""ERROR: Unrecognized command: {command[0]}\n\n Did you mean '{command[1]}'?\n"""
    assert (recieved_result.out == expected_result)

print('SUCCESS')
