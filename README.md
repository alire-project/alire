# README #
Alire project: Experimental Ada library management

A catalog of ready-to-use Ada libraries plus a command-line tool (`alr`) to obtain, compile, and incorporate them into your own projects.

## Design goals ##

alr is a tool tailored to userspace. It will store its information in the user configuration and cache folders. The current user project is created/downloaded wherever the user wants it; any dependencies it uses will be stored in a caching folder where they are reused by any dependent projects, thus saving space and compilation times.

The only exception is when a native package is needed by a project, in which case the user will be requested to authorize it with the plaform package manager (apt is the only one supported at this time) by granting sudo privileges.

Dependencies of a project are managed through an Ada specification file that must be compiled with `alr`. More information on this point coming soon.

## Supported platforms ##
Alire has been tested on the stock GNAT compiler of Debian testing and Ubuntu 17.10, as well as with GNAT GPL 2017 edition.

Alire is known _not_ to work in current Debian 9 stable or any earlier versions of Ubuntu.

Note that, for projects that require platform-provided Ada libraries (e.g., GtkAda), the compiler in use must be the platform-provided one too (at the time of writing, GNAT 7.2 from Debian Testing or Ubuntu 17.10).

## Installation ##
Copy, paste and execute in a terminal as a regular user the following command:

    curl https://raw.githubusercontent.com/alire-project/alr/master/install/alr-bootstrap.sh -o ./alr-bootstrap.sh && bash ./alr-bootstrap.sh && rm -f ./alr-bootstrap.sh || echo Installation failed

Or, alternatively, clone the repository and launch the installation script:

1. `git clone --recursive https://github.com/alire-project/alr.git`
2. `cd alr`
3. `bash install/alr-bootstrap.sh`
    
## First steps ##
First, create or enter into some folder where you don't mind that new project folders are created by the `alr` tool

Run `alr` without arguments to get a summary of available commands.

Run `alr help <command>` for more details about a command.

### Downloading, compiling and running an executable project ###
Obtaining an executable project already cataloged in Alire is straightforward. We'll demonstrate it with the `hello` project which is a plain "Hello, world!" application (or you can use the `hangman` project as a funnier alternative).

Follow these steps:

1. Issue `alr get hello`
2. Enter the new folder you'll find under your current directory: `cd hello*`
3. Build and run the project with `alr run`. This will compile and then launch the resulting executable.

As a shorthand, you can use `alr get --compile hello` to get and compile the program in one step.

### Creating a new project ###
Alire allows you to initialize an empty GNAT binary or library project with ease:

1. Issue `alr init --bin myproj` (you can use --lib for a library project).
2. Enter the folder: `cd myproj`
3. Check that it builds: `alr compile`
4. Run it: `alr run`

## Dependencies and upgrading ##
Alire keeps track of a project dependencies by compiling the file `myproj_alr.ads` file in the root folder of your project. You may check the one just created in the previous example.

If you need to add dependencies you must edit the file and then issue one of:

* `alr update`, which will fetch any additional dependencies in your project; or
* `alr update --online`, which will previously update the Alire catalog to obtain newly available releases.

As a shorthand, you can also use `alr build` to both update and compile in a single command.

### Finding available projects ###
For quick listing of projects and its description you can use the `list` command:

* `alr list [substring]`

There's also a search command which provides more details:

* `alr search <substring>` will look for `substring` in project names.
* `alr search --list` will list the whole catalog.

### Further reading ###

More comprehensive documentation is forthcoming, so stay tuned! You can check the draft paper in the `doc` folder for more details about `alr` internals in the meanwhile, or inspect index file to get an idea of how packages are indexed.
