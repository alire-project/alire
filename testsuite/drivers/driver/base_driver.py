import os
from typing import Any, Dict
import e3

import drivers.helpers

from e3.testsuite.control import YAMLTestControlCreator
from e3.testsuite.driver.classic import ClassicTestDriver


class BaseDriver(ClassicTestDriver):
    """
    A common base driver from which the rest inherit, so common functionality
    can be shared by all drivers. In particular, enabling/disabling via the e3
    control feature.
    """

    # Environment variables that can be used to modify the testsuite behavior
    # in the control field of the test YAML file.
    # E.g.:
    #     control:
    #         - [SKIP, "skip_docker", "Docker is disabled"]
    MODIFIERS = {
          "distro"  : 'ALIRE_TESTSUITE_DISABLE_DISTRO'
        , "docker"  : 'ALIRE_TESTSUITE_DISABLE_DOCKER'
        , "local"   : 'ALIRE_TESTSUITE_ENABLE_LOCAL_TESTS'
        , "network" : 'ALIRE_TESTSUITE_DISABLE_NETWORK_TESTS'
    }

    # In the constructor, prepare the features map based on the environment
    # variables.
    def __init__(self, env: e3.env.Env, test_env: Dict[str, Any]):
        super().__init__(env, test_env)

        # Prepare the features map based on the environment variables
        self.skip = {}

        for modifier, env_var in BaseDriver.MODIFIERS.items():
            affirming = "ENABLE" in env_var
            key = f"skip_{modifier}"

            if env_var in os.environ:
                self.skip[key] = not affirming
            else:
                self.skip[key] = affirming

        # Hardcode OSes
        for osname in ["windows", "linux", "macos"]:
            self.skip[f"skip_{osname}"] = not getattr(drivers.helpers, f"on_{osname}")()
        self.skip[f"skip_unix"] = drivers.helpers.on_windows()

    @property
    def test_control_creator(self):
        # Give our custom dictionary to the control filter
        return YAMLTestControlCreator(self.skip)
