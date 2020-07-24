import os.path
import rtoml


def is_index(path, semver):
    # Look for an "index.toml" file that contains a matching 'version = "x.x"'
    target = os.path.join(path, "index.toml")
    if not os.path.isfile(target):
        return False
    with open(target) as file:
        try:
            contents = rtoml.load(file)
            if "version" in contents:
                version = contents["version"]
                if version == semver:
                    return True
                else:
                    print(f"Version mismatch: {path} version is {version}, expected {semver}")
                    return False
            else:
                print(f"Malformed index file: no version found inside {path}")
        except rtoml.TomlParsingError:
            print(f"Not a target: {target} failed to load as TOML")
            raise
