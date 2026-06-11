## Checklist after releasing a new version

1. [ ] Update versions (to -dev)
  - `Alire.Version`
  - `alire.toml`
1. Publish new setup-alire vX defaulting to the latest release
1. Bump version for checks in alire-index / alire-index-checks
1. [ ] Publish `alr` in community index?
  - [ ] Release as many dependencies as possible (all needed)
  - [ ] Remove corresponding pins
  - [ ] Publish (but with caveat? It will not be the same exact build)