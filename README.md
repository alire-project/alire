# README #
Alire project: Experimental Ada library management

A catalog of ready-to-use Ada libraries plus a command-line tool (`alr`) to obtain, compile, and incorporate them into your own projects.

## Supported platforms ##
Alire has been tested on the stock GNAT compiler of Debian testing and Ubuntu 17.10, as well as with GNAT GPL 2017 edition.
Alire is known _not_ to work in current Debian 9 stable or any earlier versions of Ubuntu.

## Installation ##
Copy, paste and execute in a terminal as a regular user the following command:

    curl https://bitbucket.org/aleteolabs/alr/raw/master/install/alr-bootstrap.sh -o ./alr-bootstrap.sh && bash ./alr-bootstrap.sh && rm -f ./alr-bootstrap.sh || echo Installation failed

Or, alternatively, clone the repository and launch the installation script:

1. `git clone --recursive https://bitbucket.org/aleteolabs/alr.git`
2. `cd alr`
3. `bash install/alr-bootstrap.sh`
    
## First steps ##
First, create or enter into some folder where you don't mind that new folders are created by the `alr` tool

Run `alr` without arguments to get a summary of available commands.

### Downloading, compiling and running an executable project ###
Obtaining an executable project already cataloged in Alire is straightforward. We'll demonstrate it with the `hello` project which is a plain "Hello, world!" application.

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
* `alr update --online`, which will previously update the Alire catalog to have any new release available.

As a shorthand, you can also use `alr build` to both update and compile in a single command.

### Finding available projects ###
For now there's a basic search functionality in alire to search the catalog:

* `alr search <substring>` will look for `substring` in project names.
* `alr search --list` will list the whole catalog.

That's all there is to it for now. Have fun! 
