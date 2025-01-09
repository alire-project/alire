# Getting Started

## Installation

You can download the last release of Alire at the [GitHub
repository](https://github.com/alire-project/alire/releases).

## `alr` on Linux

On Linux, `Alire` is simply provided in an archive.

Once the archive is extracted you have to add `alr` in the environment `PATH` .
This may be done for the duration of a terminal session by running the command below:
```bash
$ export PATH="<PATH_TO_EXTRACTED>/bin/:$PATH"
```
Those wanting to keep this path permanently in their `PATH` environment may do so by pasting the above command into the `.profile` file of their user's account.

Alire provides GNAT toolchains hosted on x86-64 for Linux. If those toolchains do not
work for you, or if you are on another host architecture like ARM, you have the option
to look at the GNAT toolchains from the Linux distribution.

## `alr` on Windows

On Windows an installer is provided. The installer will create a shortcut to
start `PowerShell` with `Alire` in the environment `PATH`.

The first time you run `alr`, the program
will ask if you want to install
[msys2](https://www.msys2.org/) (except in the cases listed below). This is recommended as `alr` will use `msys2`
to automatically install required tools such as `git` or `make` that you would
otherwise have to install manually. `msys2` will also provide external
libraries required by some projects in the Alire index, allowing you to build
more projects out of the box.

`msys2` will not be installed
- when running `alr settings`, to allow uninterrupted configuration, and setting
  of `msys2` location (see `alr help settings`), or
- when you already have a msys2 installation in your PATH (more precisely, if `pacman`
  is found in your PATH.)
  - In this case, `alr` will reuse your existing installation.

Alire provides GNAT toolchains hosted on x86-64 for Windows. Those toolchains
should work for all cases; if not, let us know.

## `alr` on macOS

On macOS, `Alire` is simply provided in an archive.

Once the archive is extracted you have to add `alr` in the environment `PATH`:
```bash
$ export PATH="<PATH_TO_EXTRACTED>/bin/:$PATH"
```

If you try to run it on recent versions of macOS, you will get a popup saying
`“alr” cannot be opened because the developer cannot be verified.` and inviting
you to move it to the bin. The way round this is to remove the quarantine attribute,
```console
$ xattr -d com.apple.quarantine bin/alr
```

Alire provides GNAT toolchains hosted on x86-64 for macOS. If those toolchains do not
work for you, or if you are on another host architecture like the Apple M1, you have
the option to look at the GNAT toolchains from the community.

## `alr` for other platforms

If `alr` is not available on your platform, you can try to build it from
sources. Go to the [GitHub repository](https://github.com/alire-project/alire/)
for more information.

## First steps

The following miniguide shows how to obtain and build already packaged
projects, and create your own. First, create or enter into some folder where
you don't mind that new project folders are created by the `alr` tool

Run `alr` without arguments to get a summary of available commands.

Run `alr --help` for global options about verbosity.

Run `alr help <command>` for more details about a command.

### Downloading, compiling and running an executable crate

Obtaining an executable project already cataloged in Alire is straightforward.
We'll demonstrate it with the `hello` project which is a plain "Hello, world!"
application (or you can use the `hangman` or `eagle_lander` projects as funnier
alternatives).

Follow these steps:

1. Issue `alr get hello`
1. Enter the new folder you'll find under your current directory: `cd hello*`
1. Build and run the project with `alr run`. This will build and then launch
   the resulting executable.

   The first time you run this command, the `toolchain selection assistant`
   will ask you to select your preferred default toolchains (GNAT compiler and
   GPRbuild). For this getting started example, we recommend to just press
   enter twice to select the defaults.

As a shorthand, you can use `alr get --build hello` to get and build the
program in one step.

### Creating a new crate

Alire allows you to initialize an empty binary or library crate with ease:

1. Issue `alr init --bin myproj` (you can use `--lib` for a library project)

   The first time you run this command, `alr` will ask a couple of questions to
   automatically fill-in information about the crate:
    - `GitHub login`: is used to identify the maintainer of the crate when
      contributed to the community index.
    - `Full name`: Name of the author of the crate
    - `Email address`: Point of contact to author of the crate

    All the questions are optional, you can just press enter to use the default
    values.

    The `alr init` command will create a basic `crate` structure in the `myproj`
    directory.

1. Enter the folder: `cd myproj`
1. Build the crate: `alr build`
1. Run the program: `alr run`

We can now edit the sources of this executable in the `src/` directory.
For instance, add a "Hello world" to `src/myproj.adb`:
```ada
with Ada.Text_IO;
procedure Myproj is
begin
   Ada.Text_IO.Put_Line ("Hello, world!");
end Myproj;
```

Use `alr run` to build and run the program again:
```console
$ alr run
# Building myproj/myproj.gpr...
Compile
   [Ada]          myproj.adb
Bind
   [gprbind]      myproj.bexch
   [Ada]          myproj.ali
Link
   [link]         myproj.adb
Build finished successfully in 0.35 seconds.
Hello, world!
```

### The Alire manifest

Besides the `alr` command, the main interface with Alire is the `manifest`.

The manifest is a text file named `alire.toml` in the root directory of the
crate. It contains all sorts of information about the crate, some mandatory
such as the `name` and `version`, others optional like the `licenses`. Alire
manifests are written in [TOML](https://toml.io) format.

You can have a look at the manifest to get a idea of its content, but nothing
has to be edited by hand so far.


## Dependencies and upgrading

Alire keeps track of dependencies in the manifest (`alire.toml`) of your crate.

Adding dependencies can be done with the `alr with` command:

* `alr with crate_name` adds a dependency. You can immediately 'with' its
  packages in your code.
* `alr with --del crate_name` removes a dependency.

Alternatively you can edit the file to add dependencies and then issue:

* `alr update`, which will fetch any modified dependencies in your project.

Using `alr with` without arguments will show the current dependencies of your
project. Using one of `--solve`, `--tree`, `--versions`, `--graph` will show
different details about the complete solution needed to fulfill dependencies.

### Add a dependency

Let's add a dependency to the `libhello` crate.

```console
$ alr with libhello
Requested changes:

   # libhello ^1.0.0 (add)

Changes to dependency solution:

   + libhello 1.0.0 (new)

Do you want to proceed?
[Y] Yes  [N] No  (default is Yes)
```

`alr` is showing the new dependency solution, i.e. all the crates in the
dependency graph and their version.

Press enter to accept the new solution.

`alr` will then download the sources of the `libhello` crate.

### Use the dependency

You can now edit the `src/myproj.adb` source file again, and write this piece
of code to call a function from the `libhello` crate:
```ada
with Libhello;
procedure Myproj is
begin
   libhello.Hello_World;
end Myproj;
```

Run `alr run` to build and run the new executable:
```console
$ alr run
# Building myproj/myproj.gpr...
Setup
   [mkdir]        object directory for project Libhello
   [mkdir]        library directory for project Libhello
Compile
   [Ada]          myproj.adb
   [Ada]          libhello.adb
Build Libraries
   [gprlib]       hello.lexch
   [archive]      libhello.a
   [index]        libhello.a
Bind
   [gprbind]      myproj.bexch
   [Ada]          myproj.ali
Link
   [link]         myproj.adb
Build finished successfully in 0.34 seconds.
Hello, world!
```

As you can see, the `libhello` library sources are automatically built and
linked in your program.

## Finding available projects

For quick listing of crates and their descriptions you can use the `search`
command with the `--crates` switch:

* `alr search --crates [substring]`

Otherwise, `search` will look into releases, providing more details about specific
releases:

* `alr search <substring>` will look for `substring` in crate names.
* `alr search --list` will list the latest release of every crate.
* `alr search --list --full` will list all releases in the catalog.

Even more details are obtained with:

* `alr show <crate>`

This last command will show generic information. To see the one that
specifically applies to your platform:

* `alr show --system <crate>`

The list of projects and their descriptions are also available on the Alire
website:

* [alire.ada.dev](https://alire.ada.dev)

### Using Alire with other indexes

So far in this guide we have been using the community index, a central catalog
of publicly available crates, but it is possible to host your own index for
crates which you do not wish to make generally available. For more information,
see [using Alire with private crates](private-crates).

## Build environment

To create a build environment, `alr` sets environment variables such as
`GPR_PROJECT_PATH` before running `gprbuild`. If you want to run `gprbuild`
yourself or inside an editor (GNAT Studio), you can use the `printenv` command
to print the build environment:

* `alr printenv`

## Troubleshooting

If you hit any problem, increasing verbosity (`-v` or even `-vv`) is usually
enough to get an idea of the root of the problem. Additionally, `-d` will show
tracebacks of exceptions.

Subprocess output is shown by default (you can silence it, and anything else
not an error) with `-q`, which enables quiet mode. Any subprocess that exits
abnormally will be reported, including its invocation arguments.

If you suspect your settings may be the source of some problem, please check
our section on [Settings](settings), and in particular how to use a [default
pristine settings](settings#relocating-your-settings)

## Running tests

`alr` comes with a test suite for self-checks. See the instructions in the
[README](https://github.com/alire-project/alire/blob/master/testsuite/README.md)
of the `testsuite` folder.

Additionally, you can test in batch the building of crates in your platform
with the `alr test` command. (See `alr test --help` output for instructions.)

## Migration of an existing Ada/SPARK project to Alire

First you have to decide on a crate name for your project, this name will have
to follow the naming rules of Alire. You can find those rules using the
command:
```bash
$ alr help identifiers
```

Avoid using `ada` as a prefix for your crate name, this will make the project
harder to find in a list. `ada` suffix is ok when the project is a binding for
an existing library (e.g. `sdlada`, `gtkada`).

We will use the name `my_crate` as an example, and consider that the repository
uses the same name.

Clone your project repository and enter the directory:
```bash
$ git clone https://github.com/github_login/my_crate.git
$ cd my_crate
```

At this point you have a choice:
 1. Let Alire generate a new GPR project file for you. This is recommended for
   most projects, and in particular if your project has simple code
   organization and GPR project file. One of the advantages is that Alire will
   create a GPR project file “standardized” for best integration in the
   ecosystem.

 1. Keep your existing GPR project file. This is recommended for projects with
    complex GPR project file(s).

### 1: Using Alire GPR project file

If you want Alire to generate a project you first have to delete the existing
GPR project file:

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

If this is your first time using `alr init`, you will have to provide some
information like your name and GitHub login.

You can ignore the warnings such as `Cannot create
'[...]/my_crate/src/my_crate.ads'`, Alire is trying to create a root package
for your crate but you probably already have one.

From this point you can edit the GPR project file to change the source dir or
compilation flags, if needed. And then try to compile your crate with:
```bash
$ alr build
```

### 2: Using your own GPR project file(s)

If you want to keep the existing GPR project file, use `alr init` with the
`--no-skel` option to skip the project skeleton creation:

For a library:
```bash
$ alr init --in-place --no-skel --lib my_crate
```
For an application:
```bash
$ alr init --in-place --no-skel --bin my_crate
```
If this is your first time using `alr init`, you will have to provide some
information like your name and GitHub login.

If your GPR project file does not match the crate name (i.e. `my_crate.gpr`),
you have to add a
`project-files` field in your `alire.toml` manifest. For instance:
```toml
project-files = ["project_file.gpr"]
```

Although this is not recommended (see
[best practices](policies#best-practices)), you can have multiple GPR project files:

```toml
project-files = ["project_file_1.gpr", "project_file_2.gpr"]
```

You can now compile your crate with:
```bash
$ alr build
```
