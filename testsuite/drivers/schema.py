"""
Helpers to validate Alire manifests against the catalog JSON Schema that
is authored in YAML and shipped at <repo>/schemas/catalog-schema.yaml.
"""

import os
import tomllib

import yaml
from jsonschema import Draft202012Validator

from drivers.alr import TESTSUITE_ROOT

SCHEMA_PATH = os.path.normpath(
    os.path.join(TESTSUITE_ROOT, os.pardir,
                 "schemas", "catalog-schema.yaml"))


def catalog_validator():
    """Return a Draft 2020-12 validator for the catalog schema."""
    with open(SCHEMA_PATH, encoding="utf-8") as f:
        schema = yaml.safe_load(f)
    Draft202012Validator.check_schema(schema)
    return Draft202012Validator(schema)


def manifest_errors(validator, toml_text):
    """Validate TOML text, returning the validation errors (path-sorted)."""
    data = tomllib.loads(toml_text)
    return sorted(validator.iter_errors(data), key=lambda e: list(e.path))
