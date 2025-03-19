"""
Verify the two behaviors of --builtin in `alr settings`: error when setting a
non-builtin, warning when setting a builtin. This is kind of like `overriding`
for Ada.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_substring, assert_not_substring

WARNING_PREFIX = "Warning:"

def test_builtin_behavior(key, use_builtin, expected_message):
   """
   key: The setting key to test
   use_builtin: Whether to use the --builtin switch
   expected_message: The message to look for in the output
   """
   # The setting value is not important, only its key
   test_value = '"Test Value"'

   # We use user.name as the builtin setting for testing. We use quiet=False
   # because we don't want to suppress the warning.

   is_error_case   = use_builtin and not "user." in key
   is_warning_case = not use_builtin and "user." in key

   # Test with --set first (so it is set for --get later)
   set_args = ['settings', '--global']
   if use_builtin:
      set_args.append('--builtin')
   set_args.extend(['--set', key, test_value])

   p = run_alr(*set_args, complain_on_error=not is_error_case, quiet=False)
   if expected_message:
      assert_substring(expected_message, p.out)
      # Double-check warning format for when we check no warnings
      if is_warning_case:
         assert_substring(WARNING_PREFIX, p.out)
   else:
      # If expected_message is empty, check that there's no warning
      assert_not_substring(WARNING_PREFIX, p.out)

   # Test with --get
   get_args = ['settings', '--global']
   if use_builtin:
      get_args.append('--builtin')
   get_args.extend(['--get', key])

   p = run_alr(*get_args, complain_on_error=not is_error_case, quiet=False)
   if expected_message:
      assert_substring(expected_message, p.out)
   else:
      # If expected_message is empty, check that there's no warning
      assert_not_substring(WARNING_PREFIX, p.out)


# Using --builtin with a non-builtin setting (should fail)
test_builtin_behavior(
   key='test.nonbuiltin',
   use_builtin=True,
   expected_message="is not a built-in setting"
)

# Using a builtin setting without --builtin (should warn)
test_builtin_behavior(
   key='user.name',
   use_builtin=False,
   expected_message="is a built-in setting"
)

# Using a builtin setting with --builtin (should not warn)
test_builtin_behavior(
   key='user.name',
   use_builtin=True,
   expected_message=""  # No warning expected
)

# Using a non-builtin setting without --builtin (should not warn)
test_builtin_behavior(
   key='test.nonbuiltin',
   use_builtin=False,
   expected_message=""  # No warning expected
)

print("SUCCESS")
