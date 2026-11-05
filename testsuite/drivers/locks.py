"""
Helper functions for the testing of alr's locks, holding them from the test as
another alr process would
"""

import os
import subprocess
import threading

from drivers import builds
from drivers.alr import alr_settings_dir
from drivers.asserts import assert_eq, assert_not_substring
from drivers.helpers import on_windows

if on_windows():
    import msvcrt
else:
    import fcntl

WAITING = "Waiting for another alr process"
# Start of the message of an alr process waiting for a lock


def workspace_lock(root: str = ".") -> str:
    """
    Return the path to the lock file of the workspace at root
    """
    return os.path.join(root, "alire", "workspace.lock")


def build_lock(crate: str) -> str:
    """
    Return the path to the lock file of the shared build folder of a crate
    """
    folder = builds.find_dir(crate)
    release, build_hash = folder.split("/")[-2:]
    return os.path.join(alr_settings_dir(), "cache", "locks", "builds",
                        f"{release}_{build_hash[:8]}.lock")


class Held:
    """
    A lock on a file, held as another alr process would until released, or
    until the end of a `with` block when used as a context manager
    """

    def __init__(self, path: str):
        os.makedirs(os.path.dirname(os.path.abspath(path)), exist_ok=True)
        self._file = open(path, "a+b")
        self._file.seek(0)
        # On Windows, alr locks the first byte
        if on_windows():
            msvcrt.locking(self._file.fileno(), msvcrt.LK_NBLCK, 1)
        else:
            fcntl.flock(self._file, fcntl.LOCK_EX)

    def release(self):
        if self._file.closed:
            return
        if on_windows():
            self._file.seek(0)
            msvcrt.locking(self._file.fileno(), msvcrt.LK_UNLCK, 1)
        else:
            fcntl.flock(self._file, fcntl.LOCK_UN)
        self._file.close()

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        self.release()


class Waiting:
    """
    An alr process started while a lock it needs is held, once it has said it
    is waiting for it
    """

    def __init__(self, args: list, message: str = WAITING, timeout: int = 120,
                 **popen_args):
        self._proc = subprocess.Popen([os.environ["ALR_PATH"], "-n"] + args,
                                      stdout=subprocess.PIPE,
                                      stderr=subprocess.PIPE,
                                      text=True, **popen_args)
        self._err = []
        said_it = threading.Event()

        def read_stderr():
            for line in self._proc.stderr:
                self._err.append(line)
                if message in line:
                    said_it.set()

        # Read stderr as it comes, to know when it says it's waiting
        self._reader = threading.Thread(target=read_stderr)
        self._reader.start()

        assert said_it.wait(timeout=timeout), \
            f"alr did not say it was waiting: {''.join(self._err)}"
        assert self._proc.poll() is None, "alr did not wait for the lock"

    def finish(self, timeout: int = 120) -> str:
        """
        Wait for the process to end successfully once the lock is released,
        and return its output, which must not include the waiting message
        """
        out = self._proc.stdout.read()
        assert_eq(0, self._proc.wait(timeout=timeout),
                  out + "".join(self._err))
        self._reader.join()
        assert_not_substring(WAITING, out)  # Messages stay out of stdout
        return out
