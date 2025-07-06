- Test that an old dependency version is preserved
- Test that when impossible without upgrade, this is reported
- Test that when a dep is removed, it is not kept artificially in the solution
- Spurious empty line on `alr sync` before success message. Caused by too long
  transient status lines.
- Test that changing (not removing/adding) a dependency doesn't try to preserve
  the old release (as it may be impossible to fulfill) and that dependency is
freely updatable
- Test that indirect dependencies are not considered direct for some reason
- See why fixing a conflict doesnt show "hello" as a new crate in the solution when it was missing in the incomplete solution.
