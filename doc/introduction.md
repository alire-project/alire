# Introduction

`Alire` is a source-based package manager for the Ada and SPARK programming
languages.

It is a way for developers to easily build upon projects (libraries or
programs) shared by the community, but also to easily share their projects for
others to build upon.

In the Alire vocabulary, sources of projects/libraries/programs are provided by
what is called a `crate`. A crate can depend on crates, and other crates can
depend on it. For instance, the `libgpr` crate depends on the `xmlada` crate.

Crates can have multiple dependencies, themselves having multiple dependencies.
This forms a dependency graph. Alire's main task is to automatically fetch,
build and upgrade the crates of the dependency graph so you don't have to do it
by hand.

The main interface into the `Alire` ecosystem is a command line tool called
`alr`.
