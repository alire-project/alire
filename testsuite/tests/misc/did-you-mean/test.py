"""
Verify that an origin definition is rejected in a local manifest
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_match
from drivers.helpers import content_of
from os.path import join


init_local_crate("xxx")

original_toml = content_of("alire.toml")

def test(new_content, expected):
    with open("alire.toml", "w") as file:
        file.write (original_toml)
        file.write(new_content)

    p = run_alr("show", complain_on_error=False)
    assert_match(expected, p.out)


test("websit = true\n",
     ".*invalid property: 'websit'. Did you mean 'website'\?.*")

test("[build-profile]\n",
     ".*invalid property: 'build-profile'. Did you mean 'build-profiles'\?.*")

test("[build-profiles]\nxxx = \"releas\"",
     ".*Invalid build profile name: 'releas' for 'xxx'. Did you mean 'release'\?.*")

test("[build-profiles]\nxxx = \"rel\"",
     ".*nvalid build profile name: 'rel' for 'xxx'. Can be: release, validation, development.*")

test("[build-switche]\n",
     ".*invalid property: 'build-switche'. Did you mean 'build-switches'\?.*")

test("[build-switches]\nreleas.ada_version = \"ada12\"\n",
     ".*Invalid profile name: 'releas'. Did you mean 'release'\?.*")

test("[build-switches]\nrelease.ada_versino = \"ada12\"\n",
     ".*Invalid switch category: 'ada_versino'. Did you mean 'ada_version'\?.*")

test("[build-switches]\nrelease.ada_version = \"gnat_extensoni\"\n",
     ".*Invalid switch selector 'gnat_extensoni' for category 'ADA_VERSION'. Did you mean 'gnat_extensions'\?.*")

test("[environement]\nTEST.append = \"test\"\n",
     ".*invalid property: 'environement'. Did you mean 'environment'\?.*")

test("[environment.'case(os)'.linusc]\nTEST.append = \"test\"\n",
     ".*invalid enumeration value: 'linusc'. Did you mean 'linux'\?.*")

test("[environment.'case(host-arch)'.x86-46]\nTEST.append = \"test\"\n",
     ".*invalid enumeration value: 'x86-46'. Did you mean 'x86-64'\?.*")

test("[environment.'case(host-arch)'.plop]\nTEST.append = \"test\"\n",
     ".*invalid enumeration value: 'plop'. Can be: aarch64, aarch64-be, architecture-unknown, arm, i386, i686, x86-64.*")

print('SUCCESS')
