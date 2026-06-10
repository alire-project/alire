"""
Helpers to validate Alire manifests against the manifest schema
"""

import os
import tomllib

import yaml
from jsonschema import Draft202012Validator, ValidationError

from drivers.alr import TESTSUITE_ROOT

# Locate the schema file
MANIFEST_SCHEMA_PATH = os.path.normpath(
    os.path.join(TESTSUITE_ROOT, os.pardir,
                 "schemas", "manifest-schema.yaml"))


def manifest_validator() -> Draft202012Validator:
    """Return a Draft 2020-12 validator for the manifest schema."""
    with open(MANIFEST_SCHEMA_PATH, encoding="utf-8") as f:
        schema = yaml.safe_load(f)
    Draft202012Validator.check_schema(schema)
    return Draft202012Validator(schema)


def manifest_errors(validator: Draft202012Validator,
                    toml_text: str) -> list[ValidationError]:
    """Validate TOML text, returning the validation errors (path-sorted)."""
    data = tomllib.loads(toml_text)

    def by_path(error: ValidationError) -> list:
        return list(error.path)

    errors = validator.iter_errors(data)
    return sorted(errors, key=by_path)
