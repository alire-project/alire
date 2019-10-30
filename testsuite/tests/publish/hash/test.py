"""
Check the `alr publish --hash` subcommand
"""

import re

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_eq

# Test vectors from https://www.di-mgt.com.au/sha_testvectors.html
filenames = ["empty.tgz",
             "abc.tgz",
             "bits448.tgz"]
contents = ["",
            "abc",
            "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"]
hashes = [
    "sha512:cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47"
    "d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e",

    "sha512:ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a21"
    "92992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f",

    "sha512:204a8fc6dda82f0a0ced7beb8e08a41657c16ef468b228a8279be331a703c33596"
    "fd15c13b1b07f9aa1d3bea57789ca031ad85c7a71dd70354ec631238ca3445"]

# Generate a test "crate" in the current folder with known contents and hash:
for i in range(len(filenames)):
    with open(filenames[i], "wb") as file:
        file.write(contents[i])

    # Hash and check
    p = run_alr('publish', '--hash', 'file://' + filenames[i], quiet=False)
    assert_eq(hashes[i] + '\n', p.out)


print('SUCCESS')
