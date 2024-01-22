import hashlib
import json
import os
import subprocess
from importlib import import_module
from typing import Tuple

from drivers.helpers import FileLock, on_linux, MODIFIERS
from e3.testsuite.driver.classic import (ClassicTestDriver,
                                         TestAbortWithFailure,
                                         TestSkip)

DOCKERFILE = "Dockerfile"
TAG = "alire_testsuite"
ENV_FLAG = "ALIRE_DISABLE_DOCKER" # Export it to disable docker tests
LABEL_HASH = "hash"


def is_docker_available() -> bool:
    # Restrict docker testing only to Linux
    if not on_linux():
        return False

    # Detect explicitly disabled
    if ENV_FLAG in os.environ:
        return False

    # Detect python library
    try:
        import_module("docker")
    except ImportError:
        return False

    # Detect executable
    try:
        subprocess.run(["docker", "--version"], check=True,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except (subprocess.CalledProcessError, FileNotFoundError):
        return False

    return True


def skip_unless_docker_available() -> None:
    if not is_docker_available():
        print("SKIP: Docker testing is disabled or not available")
        exit(0)


def get_client():
    if is_docker_available():
        import docker
        return docker.from_env()
    else:
        return None


def compute_dockerfile_hash() -> str:
    with open(DOCKERFILE, 'r') as file:
        content = file.read()
    return hashlib.sha256(content.encode('utf-8')).hexdigest()


def already_built() -> Tuple[str, str]:
    file_hash = compute_dockerfile_hash()

    # Check existing images
    client = get_client()
    for image in client.images.list():
        if LABEL_HASH in image.labels and image.labels[LABEL_HASH] == file_hash:
            return image, file_hash

    return None, file_hash


def build_image() -> None:
    # We need to lock here as multiple docker tests might attempt to create the
    # image at the same time
    with FileLock("/tmp/alire_testsuite_docker.lock"):
        image, file_hash = already_built()

        if image:
            return

        # Do the actual build
        get_client().images.build(
            path="..", dockerfile=f"testsuite/{DOCKERFILE}", rm=True, tag=TAG,
            labels={LABEL_HASH : compute_dockerfile_hash()})


class DockerWrapperDriver(ClassicTestDriver):

    # This is a workaround for Windows, where attempting to use rlimit by e3-core
    # causes permission errors. TODO: remove once e3-core has a proper solution.
    @property
    def default_process_timeout(self):
        return None

    def run(self):
        if not is_docker_available():
            raise TestSkip('Docker testing is disabled or not available')

        build_image()

        # Augment the test environment with local modifiers that could
        # impact the wrapped test, if defined
        for modifier in [m for m in MODIFIERS if m in os.environ]:
            self.test_env[modifier] = os.environ[modifier]

        # Run our things
        try:
            container = get_client().containers.run(
                # Regular image launching (priviledged to allow unshare)
                image=TAG, tty=True, stdin_open=True, detach=True, privileged=True,

                # Pass the test environment to the container as JSON
                environment={"ALIRE_TEST_ENV": json.dumps(self.test_env)},

                # Bind the testsuite directory as read-only and the alr executable
                volumes={ os.path.abspath(".") : { "bind": "/testsuite", "mode": "ro" }
                          , os.path.abspath("..") + "/bin/alr" : { "bind": "/usr/bin/alr", "mode": "ro" }
                         },

                # In the container, launch the script that will setup the test
                command=
                    "/bin/python3 -c 'from drivers.driver import docker_nested;"
                    "docker_nested.main()'")

            # Wait for the container to finish and retrieve its output
            result = container.wait()
            output = container.logs().decode()

            if (code := result["StatusCode"]) != 0:
                self.result.log += f'Docker command failed with exit code {code} and output:\n{output}'
                raise TestAbortWithFailure(
                    f"Docker command failed with exit code {code}")

        finally:
            # Don't leave dead containers around
            if 'container' in locals() and container:
                container.remove()

            # Check that the test succeeded inside the docker container
            out_lines = output.splitlines()
            if out_lines and out_lines[-1] == 'SUCCESS':
                pass
            elif out_lines and (reason := out_lines[-1]).startswith('SKIP:'):
                raise TestSkip(reason.split(":")[-1].strip())
            else:
                self.result.log += 'missing SUCCESS output line'
                raise TestAbortWithFailure('missing SUCCESS output line')