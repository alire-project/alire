# Getting Started

## Installation

For the moment, only the latest development version is available. To obtain it
you need to perform a manual clone and build of the project.

First you need a GNAT compiler. On Linux you can usually get it from your
distribution. Otherwise, and for Windows and MacOS you can download and install
[GNAT Community](https://www.adacore.com/download).

Then clone the git repository:

`git clone --recursive https://github.com/alire-project/alire.git`

Note the **--recursive** flag; otherwise compilation will fail.

And build `alr`:

1. If running on macOS, `export OS=macOS`
1. `cd alire`
1. `gprbuild -j0 -p -P alr_env`

The executable will be found in `bin/alr`. You can add it to your PATH:

 * Linux/MacOS: `export PATH=$PATH:$PWD/bin`
 * Windows PowerShell: `$env:path += ";$pwd/bin"`

## `alr` on Windows

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

Alire keeps track of a project dependencies in the file
`./alire/project_name.toml` of your project. You may check the one just
created in the previous example.

This file can be managed through `alr`:

* `alr with project_name` adds a dependency. You can immediately 'with' its
  GPR project files and packages in your code.
* `alr with --del project_name` removes a dependency.
* `alr with --from yourproject.gpr` reads the given GPR file and adds
   dependencies specified in comments as alr invocations. For example:

    ```Ada
    with "xstrings"; -- alr with xstrings
    project My_Project is
    ```

Alternatively you can edit the file (example in the works) to add dependencies
and then issue:

* `alr update`, which will fetch any additional dependencies in your project.

## Finding available projects

For quick listing of projects and their descriptions you can use the `list`
command:

* `alr list [substring]`

There's also a `search` command which provides more details:

* `alr search <substring>` will look for `substring` in project names.
* `alr search --list` will list the whole catalog.

Even more details are obtained with:

* `alr show <project>`

This last command will show generic information. To see the one that
specifically applies to your platform:

* `alr show --system <project>`

The list of projects and their descriptions are also available on the Alire
website:

* [alire.ada.dev](https://alire.ada.dev)

## Build environment

To create a build environment, `alr` sets environment variables such as
`GPR_PROJECT_PATH` before running `gprbuild`. If you want to run `gprbuild`
yourself or inside an editor (GNAT Studio), you can use the `setenv` command to
print the build environment:

* `alr setenv`

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
