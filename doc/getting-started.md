# Getting Started

## Installation

You can download the last release of Alire at the [GitHub repository](https://github.com/alire-project/alire/releases).

You will also need a GNAT compiler toolchain. On Linux you can usually get it from your
distribution. Otherwise, and for Windows and macOS you can download and install
[GNAT Community](https://www.adacore.com/download).

## `alr` on Linux and macOS

For Linux and macOS, `Alire` is simply provided in an archive. 

Once the archive is extracted you have to add `alr` in the environment `PATH`:
```bash
$ export PATH=<PATH_TO_EXTRACTED>/bin/:$PATH
```

You will also have to add your GNAT toolchain in the environment`PATH`.

## `alr` on Windows

For Windows an installer is provided. The installer will create a shortcut to start `PowerShell` with `Alire` in the environment `PATH`.

Inside the `PowerShell` you will also have to add your GNAT toolchain in the environment`PATH`.
For instance with [GNAT Community](https://www.adacore.com/download) at the default location:
```powershell
PS> $env:Path += ";C:\GNAT\2020\bin\;C:\GNAT\2020-arm-elf\bin\"
```

On Windows, the first time you run `alr` the program will ask if you want to
install [msys2](https://www.msys2.org/). This is recommended as `alr` will use
`msys2` to automatically install required tools such as `git` or `make` that
you would otherwise have to install manually. `msys2` will also provide
external libraries required by some projects in the Alire index, allowing you
to build more projects out of the box.

## First steps

The following miniguide shows how to obtain and build already packaged
projects, and create your own. First, create or enter into some folder where
you don't mind that new project folders are created by the `alr` tool

Run `alr` without arguments to get a summary of available commands.

Run `alr --help` for global options about verbosity.

Run `alr help <command>` for more details about a command.

### Downloading, compiling and running an executable project

Obtaining an executable project already cataloged in Alire is straightforward.
We'll demonstrate it with the `hello` project which is a plain "Hello, world!"
application (or you can use the `hangman` or `eagle_lander` projects as funnier
alternatives).

Follow these steps:

1. Issue `alr get hello`
1. Enter the new folder you'll find under your current directory: `cd hello*`
1. Build and run the project with `alr run`. This will build and then launch
   the resulting executable.

As a shorthand, you can use `alr get --build hello` to get and build the
program in one step.

### Creating a new project

Alire allows you to initialize an empty GNAT binary or library project with
ease:

1. Issue `alr init --bin myproj` (you can use `--lib` for a library project)
1. Enter the folder: `cd myproj`
1. Check that it builds: `alr build`
1. Run it: `alr run`

## Dependencies and upgrading

Alire keeps track of a project dependencies in the file `./alire.toml` of your
project. You may check the one just created in the previous example.

This file can be managed through `alr`:

* `alr with project_name` adds a dependency. You can immediately 'with' its
  packages in your code.
* `alr with --del project_name` removes a dependency.

Using `alr with` without arguments will show the current dependencies of your
project. Using one of `--solve`, `--tree`, `--versions`, `--graph` will show
different details about the complete solution needed to fulfill dependencies.

Alternatively you can edit the file (example in the works) to add dependencies
and then issue:

* `alr update`, which will fetch any modified dependencies in your project.

## Finding available projects

For quick listing of crates and their descriptions you can use the `list`
command:

* `alr list [substring]`

There's also a `search` command which provides more details about specific
releases:

* `alr search <substring>` will look for `substring` in crate names.
* `alr search --list` will list the whole catalog.

Even more details are obtained with:

* `alr show <crate>`

This last command will show generic information. To see the one that
specifically applies to your platform:

* `alr show --system <crate>`

The list of projects and their descriptions are also available on the Alire
website:

* [alire.ada.dev](https://alire.ada.dev)

## Build environment

To create a build environment, `alr` sets environment variables such as
`GPR_PROJECT_PATH` before running `gprbuild`. If you want to run `gprbuild`
yourself or inside an editor (GNAT Studio), you can use the `printenv` command
to print the build environment:

* `alr printenv`

## Troubleshooting

By default `alr` is quite terse and will hide the output of subprocesses,
mostly reporting in case of failure. If you hit any problem, increasing
verbosity (`-v` or even `-vv`) is usually enough to get an idea of the root of
the problem. Additionally, `-d` will show tracebacks of exceptions.

## Running tests

`alr` comes with a test suite for self-checks. See the instructions in the
[README](https://github.com/alire-project/alire/blob/master/testsuite/README.md)
of the `testsuite` folder.

Additionally, you can test in batch the building of crates in your platform
with the `alr test` command. (See `alr test --help` output for instructions.)

## Migration of an existing Ada/SPARK project to Alire

First you have to decide on a crate name for your project, this name will have to follow the naming rules of Alire. You can find those rules using the command:
```bash
$ alr help identifiers
```

Avoid using `ada` as a prefix for your crate name, this will make the project harder to find in a list. `ada` suffix is ok when the project is a binding for an existing library (e.g. `sdlada`, `gtkada`).

We will use the name `my_crate` as an example, and consider that the repository uses the same name.

Clone your project repository and enter the directory:
```bash
$ git clone https://github.com/github_login/my_crate.git
$ cd my_crate
```

At this point you have a choice:
 1. Let Alire generate a new GPR project file for you. This is recommended for most projects, and in particular if your project has simple code organization and GPR project file. One of the advantages is that Alire will create a GPR project file “standardized” for best integration in the ecosystem.

 1. Keep your existing GPR project file. This is recommended for projects with complex GPR project file(s).

### 1: Using Alire GPR project file

If you want Alire to generate a project you first have to delete the existing GPR project file:

```bash
$ rm *.gpr
```

And then use `alr init` command to create a skeleton for your crate:

For a library:
```bash
$ alr init --in-place --lib my_crate
```
For an application:
```bash
$ alr init --in-place --bin my_crate
```

If this is your first time using `alr init`, you will have to provide some information like your name and GitHub login. 

You can ignore the warnings such as `Cannot create '[...]/my_crate/src/my_crate.ads'`, Alire is trying to create a root package for your crate but you probably already have one.

From this point you can edit the GPR project file to change the source dir or compilation flags, if needed.
And then try to compile your crate with:
```bash
$ alr build 
```

### 2: Using your own GPR project file(s)

If you want to keep the existing GPR project file, use `alr init` with the `--no-skel` option to skip the project skeleton creation:

For a library:
```bash
$ alr init --in-place --no-skel --lib my_crate
```
For an application:
```bash
$ alr init --in-place --no-skel --bin my_crate
```
If this is your first time using `alr init`, you will have to provide some information like your name and GitHub login. 

If your GPR project file does not match the crate name (i.e. `my_crate.gpr`), you have to add a 
`project-files` field in your `alire.toml` manifest. For instance:
```toml
project-files = ["project_file.gpr"]
```
Although this is not recommended (see Best practices), you can have multiple GPR project files:
```toml
project-files = ["project_file_1.gpr", "project_file_2.gpr"]
```

You can now compile your crate with:
```bash
$ alr build 
```
