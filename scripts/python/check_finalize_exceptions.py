#!/usr/bin/env python3
# Find all Finalize procedures that do not report unhandled exceptions

import re
import glob
import sys


def find_matching(source_file, match_pattern, exclude_pattern):
    match_re = re.compile(match_pattern)
    exclude_re = re.compile(exclude_pattern)

    count = 0
    with open(source_file, "r", encoding="utf-8") as file:
        previous_line = ""
        line_nbr = 1
        for line in file:
            if match_re.search(line) and not exclude_re.search(previous_line):
                print(f"{source_file}:{line_nbr}: " + line)
                count = count + 1
            previous_line = line
            line_nbr = line_nbr + 1

    return count > 0


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Please provide one directory path")
        sys.exit(1)

    count = 0
    for file in glob.glob(f"{sys.argv[1]}/**/*.ad[bs]"):
        if find_matching(
            file, ".*end Finalize;.*", ".*Alire.Utils.Finalize_Exception.*"
        ):
            count = count + 1

    if count > 0:
        sys.exit(1)
