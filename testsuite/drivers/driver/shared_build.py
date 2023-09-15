
from drivers.driver.python_script import PythonScriptDriver
from drivers.alr import run_alr


class SharedBuildDriver(PythonScriptDriver):
    """
    A specialization of the regular PythonScriptDriver that activates shared
    builds for the test. See notes in PythonScriptDriver for more details.
    """

    def run(self):
        # Inherited set up
        env = self.prepare()

        # Activate shared builds. We must do it this way because the final
        # environment doesn't apply to us yet.
        run_alr("-c", env["ALR_CONFIG"],
                "config", "--global", "--set", "dependencies.shared", "true")

        # Run as inherited
        self.run_script(env)
