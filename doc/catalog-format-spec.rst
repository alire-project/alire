Catalog format specification
============================

Big picture
-----------

Each project is described as a TOML file. For instance: ``aaa.toml`` for the
AAA project, ``gnatcoll.toml`` for the package corresponding to GNATCOLL.

Each TOML description contains an object that has a ``general`` table and then
one table per release.

.. code-block:: toml

   [general]
   ...

   ['1.0']
   ...

   ['1.1']
   ...

   ['2.0']
   ...

``general`` contains information common to all releases, then each release
provides sources (``origin``) and can refine common information.


Information encoding
--------------------

This section describes the various encodings used in this format to encode
information.

First, there are two kinds of data: atomic and composite data.

Atomic data designates values that cannot be decomposed. There are only two
atomic data types:

* mere strings (``"Hello, world!"``);
* booleans (``true``, ``false``);

We can then split composite data in two kinds: lists (TOML's arrays) and
mappings (JSON's tables). Lists are just sequences of other values; for
instance a list of strings:

.. code-block:: toml

   ["A", "B"]

Mapping are the traditional sets of associations from keys (here, always
strings) to other values. For instance, the following represents a set of
dependencies, with version constraints:

.. code-block:: toml

   libfoo = "^1.2"
   libbar = "^2.0"

In some contexts, information can be dynamic: special encodings can be used to
make data vary depending on the environment (OS, architecture, …). The
environment is represented as a set of specific variables which can have
a specific set of values: see the Parameters section below for a comprehensive
list.

To create atomic values in an environment-dependent way, use the following
construct (here to create a boolean):

.. code-block:: toml

   {'case(distribution)' = {
       'debian|ubuntu': true,
       '...': false
   }}

   # Or in a more idiomatic TOML syntay
   ['case(distribution)']
   'debian|ubuntu' = true
   '...' = false

Depending on the value of the ``distribution`` environment variable, this will
return ``true`` (its value is ``debian`` or ``ubuntu``) or ``false`` (for other
values).

A little variation allows to build environment-dependent composite data. For
instance, to make the dependency on ``libbar`` above dynamic:

.. code-block:: toml

   {
       "libfoo": "^1.2",
       "case(os)": {
           "linux": {"libbar": "^2.0"},
           "windows": {"libwinbar": "^3.0"},
           "...": {}
       }
   }

   # Or in a more idiomatic TOML syntay
   libfoo = "^1.2"

   ['case(os)'.linux]
   libbar = "^2.0"

   ['case(os)'.windows]
   libwinbar = "^3.0"

   ['case(os)'.'...']

If the ``os`` environment variable contains ``linux``, this will create the
following dependencies:

.. code-block:: toml

   libfoo = "^1.2"
   libbar = "^2.0"

The ``case(os)`` part selects dependencies depending on the value of the ``os``
environment variable.

.. code-block:: toml

   libfoo = "^1.2"
   libwinbar = "^3.0"

And finally for other ``os`` values:

.. code-block:: toml

   libfoo = "^1.2"


General info
------------

Unless specified, all the entries must be static, i.e. they cannot depend on
the context.

The ``general`` entry must contain an object. It contains itself the following
entries:

* ``description``: mandatory string. One-line description about the package.
  For instance:

  .. code-block:: toml

   description = "Library to handle foobars"

* ``authors``: optional array of strings. Flat list of human-readable names for
  the authors, i.e. the people that wrote the software that is packaged. For
  instance:

  .. code-block:: toml

   authors = ["Alice Example",
              "Bob For Instance <bob@example.com>"]

* ``maintainers``: mandatory array of strings. Flat list of human-readable names
  for the maintainers, i.e. the people that maintain the package description in
  Alire. For instance:

  .. code-block:: toml

   maintainers = ["Alice Example",
                  "Bob For Instance <bob@example.com>"]

* ``licenses``: mandatory array of strings. Flat list of licenses for the
  software that is packaged. The following licenses are allowed:

    * ``GMGPL 2.0`` and ``GMGPL 3.0`` for the corresponding GNAT Modified
      General Public Licenses.

    * ``GPL 1.0``, ``GPL 2.0`` and ``GPL 3.0`` for the corresponding GNU General Public
      Licenses.

    * ``LGPL 3.0`` for the corresponding Lesser General Public License.

    * ``MIT`` for the MIT License.

    * ``Public Domain`` for code in the public domain.

    * ``zlib`` fo the zlib License.

    * ``custom:XXX``, to be used for licenses unlisted here, where ``XXX`` is the
      body of the license itself.

  If the license is unknown, leave an empty array.

  .. code-block:: toml

   licenses = []

  For a double license:

  .. code-block:: toml

   licenses = ["GPL 3.0", "MIT"]

  Custom license example:

  .. code-block:: toml

   licenses = """
      "THE BEER-WARE LICENSE" (Revision 42):
      <phk@FreeBSD.ORG> wrote this file.  As long as you retain this notice you
      can do whatever you want with this stuff. If we meet some day, and you
      think this stuff is worth it, you can buy me a beer in return.
      Poul-Henning Kamp
   """

* ``website``: optional string. URL to the original project's website. For
  instance:

  .. code-block:: toml

     website = "https://myproject.example.org/"

* ``available``: optional dynamic boolean expression.  Determines whether the
  package is available for the current platform (true) or not (false). For
  instance:

  .. code-block:: toml

   [available.'case(distribution)']
   'debian|ubuntu' = true
   '...' = false

* ``comments``: free form optional string to provide information about this
  package, in addition to ``description``.

* ``depends-on``: optional dynamic dependencies expression common to all
  releases. For instance:

  .. code-block:: toml

   [depends-on]
   libfoo = "^1.2"

   [depends-on.'case(os)'.linux]
   libbar = "^2.0"

   [depends-on.'case(os)'.windows]
   libwinbar = "^3.0"

  Available constraint operators are the usual Ada ones (=, /=, >, >=, <, <=) 
  plus caret (^, any upwards version within the same major point) and tilde 
  (~, any upwards version within the same minor point).

* ``project-files``: optional list of strings. Each is a path, relative to the
  root of the source directory, to a project file to be made available. 
  Expressions are accepted. For instance:

  .. code-block:: toml

   project-files = ["my_project.gpr", "utils/utils_for_my_project.gpr"]

   [project-files.'case(word-size)']
   bits-64 = ["my_project.gpr"]
   bits-32 = ["my_project32.gpr"]

* ``gpr-externals``: optional table, giving a mapping from the name of external
  variables in the project files to sets of possible values (as array of
  strings), or an empty string if this set is infinite. For instance:

  .. code-block:: toml

   [gpr-externals]
   BUILD_MODE = ["debug", "profile", "release"]
   TAG = ""

* ``gpr-set-externals``: optional table, giving a mapping from the name of
  external variables to the values to use by default when building the project.  
  Expressions are accepted before the mapping. For instance:

  .. code-block:: toml

   [gpr-set-externals]
   BUILD_MODE = "release"

   [gpr-set-externals.'case(os)']
   linux   = { OS = "gnu-linux" } # Compact table syntax is convenient in this case
   windows = { OS = "ms-linux" }  # to see all enumeration values, one per row.

* ``executables``: optional list of strings. Each is a path, relative to the root
  of the source directory, to an executable provided by the package. For
  instance:

  .. code-block:: toml

   executables = ["bin/my_main"]

* ``actions``: optional list of actions to perform when installing this package. 
  The general action syntax is:

  .. code-block:: toml

   [[actions]]
   type = <kind>
   command = <command>

  ``<command>`` is a string for a shell command to run in the source directory.
  ``<kind>`` can be either:
 
  * ``post-fetch``: the command is to be run right after getting the package
    sources;
  * ``post-compile``: the command is to be run right after GPRbuild has been
    run.

  Actions accept dynamic expressions. For example:

  .. code-block:: toml

   [[general.actions.'case(os)'.linux]]
   type = "post-fetch"
   command = "make"

   [[general.actions.'case(os)'.windows]]
   type = "post-fetch"
   command = "cmd build"

   [[general.actions.'case(os)'.'...']]
   # An explicit empty case alternative, which is not mandatory

Release-specific info
---------------------

Each release is materialized as an entry in the top-level object. The key is a
string for the version number for the release, while the value is an object to
contain the release-specific information. This object can contains the
following entries:

* ``origin``: mandatory dynamic string expression. URL used to fetch the
  sources to build. For instance:

  .. code-block:: toml

   # Clone a git repository
   origin = "git+https://github.com/example-user/example-project"

   # Download and extract a source archive
   origin = "https://example.org/archive.tar.gz"

  If the package only maps a package from the system package manager, (for
  instance ``make``), run:

  .. code-block:: json

   origin = "native:make"

  Make the expression evaluate to an empty string to mean that the package is
  not available, or just leave the alternative out. For instance, to state that
  ``make`` is available on Debian/Ubuntu and not on the other platforms:

  .. code-block:: json

   [origin.'case(distribution)']
   'debian|ubuntu' = "native:make"

* ``archive-name``: optional string. If ``origin`` points to a source archive,
  this can specifiy the name of the file to download, which is needed in order
  to properly extract the sources. For instance:

  .. code-block:: json

   origin = "https://example.org/0123456789"
   archive-name = "archive.tar.gz"

* ``available``: optional dynamic boolean expression. It is used the following
  way:

  1. If it evaluates to ``false``, the package is not available for the current
     platform.
  2. Otherwise, the availability is determined by the ``available`` entry in
     the ``general`` section.

* ``notes``: optional string. Provides miscellanous information about this
  release. For instance:

  .. code-block:: json

   "notes": "Experimental version"

It can also contain the following entries: ``depends-on``, ``project-files``,
``gpr-externals``, ``gpr-set-externals``, ``executables``, ``actions``. These
are optional. For atomic values, these override the ones from ``general``, and
for lists/mappings, they are interpreted as additions. In the latter case,
conflicting entries are considered as errors.

Parameters
----------

* ``os``: name of the OS. Currently supported values are: ``linux``, ``macos``
  and ``windows``.
* ``distribution``: name of the Linux distribution, or ``none`` if running on a
  different OS. Currently supported values are: ``debian``, ``ubuntu``.
* ``compiler``: name of the current compiler. Currently supported values are:
  ``gnat-unknown``, ``gnat-fsf-old``, ``gnat-fsf-7.2``, ``gnat-fsf-7.3``,
  ``gnat-gpl-old``, ``gnat-gpl-2017``, ``gnat-community-2018``.
* ``word-size``: architecture word size. Currently supported values are: ``bits-32``, ``bits-64``, ``bits-unknown``
