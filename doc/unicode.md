# Unicode guidelines

Unicode is the solution to work with character sets other than ASCII or, in the
case of Ada, beyond Latin-1, which was the character set chosen for the
`Character` type. There are a number of considerations for the use of Unicode
in Ada and in Alire crates, detailed in this section.

## TL;DR

Alire v2.0 and onwards expects sources to be UTF-8 (through the `-gnatW8`
switch). Input/Output must be done using specialized crates (e.g. `vss`,
`uxstrings`) or with `Wide_Wide_Text_IO` (for unencoded/UTF-32 strings) or with
`Streams_IO/GNAT.IO` (for UTF-8-encoded strings). Avoid `Text_IO` unless you
are sure of what you're doing (using Latin-1 strings). Some friction may happen
with older sources not using UTF-8.

Read on for the gory details.
## Alire assumptions and configuration

By default, any crate initialized via `alr init` will have the `-gnatW8` switch
in its build configuration, which presumes UTF-8 encoding of sources, and
tweaks some internals of the compiler accordingly.

This means that source files **must** use UTF-8 encoding when not using plain
ASCII.

Since GNAT compiles specifications and generic bodies in the context of the
client project, once internalization is enabled in some parts of a build, it
becomes necessary for all parts of the build to use the same `-gnatW8` setting.
Otherwise, a file containing non-ASCII literals could be interpreted
differently depending on the compilation context (as a standalone library or
from a client of such a library).

Most old GNAT projects are likely not to have `-gnatW8` enabled (although an
UTF-8 file with BOM marker will have the same effect). For crates that do not
contain string literals outside of ASCII or engage on I/O of such strings, this
should not make any difference. For those for which internationalization
matters, however, there is no sensible way forward but to embrace Unicode, with
UTF-8 being the standard encoding nowadays for files and terminals.

This means that a certain amount of breakage might happen for 'legacy'
libraries not yet adapted to `-gnatW8`. From the Alire project we are trying to
get in front of this problem with early detection of such libraries in the
Alire ecosystem, and universally using `-gnatW8` from version 2.0 on.

## Recommendations

You can use a library designed to shield you from Unicode details, such as
`vss` or `uxstrings`.

Otherwise, these recommendations should keep you safe:

1. Use `-gnatW8` to compile (this is Alire's default) and save your sources in UTF-8 encoding.
1. Use Wide_Wide_String to store any text that may exceed the ASCII character set.
    1. Alternatively, use `Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode` to get UTF-8 strings from Unicode literals.
1. Convert if necessary at the time of output to the desired encoding for display/storage.
    1. By default, Ada.Wide_Wide_Text_IO will use UTF-8 encoding if `-gnatW8` is in effect.
    1. Most modern plain text formats expect UTF-8 encoding.
    1. On Linux terminals this is the usual default.
    1. On Windows 10:
        1. You can enable UTF-8 once by running `chcp 65001` in your terminal.
        1. You can enable UTF-8 in terminals system-wide.
            - This will break programs assuming the default Windows 10 encoding.
    1. On Windows 11, UTF-8 terminals are the default.
1. You can use `GNAT.IO` or `Ada.Streams.Stream_IO` to output properly encoded strings, as they don't manipulate the bytes.
1. You **cannot** use `Ada.Text_IO` to output UTF-8-encoded `String` variables, as Latin-1 encoding is expected.

You can experiment with the `utf8test` crate at
https://github.com/mosteo/utf8test to check how your environment behaves in
regard to UTF-8 output.

## Ada and GNAT string encoding basics

Properly working with Unicode in Ada relies on these bits of info:

- The Ada Standard designates the full Unicode character for its repertoire [1], but:
- The default encoding used by GNAT, unless a BOM is found, is Latin-1 [2].
- The standard `Character` type is Latin-1 [3], so:
- `Ada.Text_IO` expects Latin-1-encoded strings (superset of ASCII), incompatible with UTF-8.
- `Ada.Wide_Wide_Text_IO` expects UTF-32 strings, that is, regular `Wide_Wide_Strings` in practice.

It is common to use `String` to store byte sequences in different encodings.
This is a source of problems when going out of ASCII, as depending on how these
strings are populated, they may easily end being either Latin-1 or UTF-8 or
something else.

UTF-8-encoded strings are more memory-efficient, but cannot be used to iterate
over characters without help of support libraries. `Wide_Wide_Strings` retain
the 1:1 index-to-character ratio, so they are efficient for such iterations.
`Wide_Strings`, which are defined to hold 2-byte Unicode code points (although
conceivably they could also be used with UTF-16), are probably a middle-ground
not useful in general anymore.

## Migrating 'old' code

1. Save all source files as UTF-8 (see e.g. `iconv`).
1. Add `-gnatW8` to your compiler switches.
1. Fix any errors that arise due to `String` literals containing characters outside of the Latin-1 range (see next section).
1. Look for suspect uses of `Ada.Text_IO` and replace it with `Ada.Wide_Wide_Text_IO` or `GNAT.IO` or `Ada.Streams.Stream_IO`.
    - Use `Ada.Characters.Conversions.To_Wide_Wide_String` to convert Latin-1 strings to UTF-32.
    - Use `Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode` to convert UTF-8 strings to UTF-32.
    - Use `Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode` to convert UTF-32 strings to UTF-8.

## Common pitfalls

A particularity of `-gnatW8` is that it may affect compilation units in other
projects, for example when specifications or generic bodies are 'withed', as
these are compiled also in the context of the client project. This may cause
issues when not all projects use `-gnatW8`, even if projects in isolation work
properly.

Trouble may thus arise from inconsistencies between source file encoding and
`-gnatW8` being in effect, or even simply by saving Latin-1 files as UTF-8 in
order to enable `-gnatW8`:

- Latin-1 (or other non UTF-8) sources compiled with -gnatW8 will likely cause
"illegal wide character" errors, as Latin-1 literals will be invalid UTF-8
sequences.
- UTF-8 sources compiled without -gnatW8 will result in mangled output as the
UTF-8 sequences are misinterpreted as Latin-1, where most bytes are a valid
character code.
- Latin-1 sources saved to UTF-8 and compiled with -gnatW8 may result in
"literal out of range of type Standard.Character" errors when characters
outside of Latin-1 are assigned to a regular `Character` instead of
`Wide_Wide_Character`.
    - Example: `X : String := "€";`
        - Will work without `-gnatW8`, resulting in a three-byte string.
        - Will err with `-gnatW8`, as '€' is out of `Character` range.
        - In this case, the code has to be adapted to use `Wide_Wide_String`/`Wide_Wide_Character`, or the string manually encoded as UTF-8 (which will **not** be apt for use with `Ada.Text_IO`):
        `subtype UTF8_String is String;`
        `X : UTF8_String := Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode ("€");`


See a few more examples:

```
O_Acute : constant Character := 'ó';
--  With Latin-1 source and -gnatW8, it will fail as 'ó' won't be a proper UTF-8 sequence in the input file.
--  With UTF-8 source and -gnatW8, it will work **but** it will be stored in Latin-1 encoding in memory.
--  It can be converted to a Wide_Wide_String (that will be in UTF-32)
--  with `Ada.Characters.Conversions.To_Wide_Wide_String`. In this case,
--  using `Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode` would be wrong,
--  as the String is Latin-1 and not UTF-8 despite the source file encoding being UTF-8.

Bye : constant String := "Adiós";
--  Without -gnatW8 but UTF-8 sources without a BOM marker, this will end being an UTF-8 byte sequence,
--  as it is interpreted literally as Latin-1 characters.
--  With -gnatW8, the UTF-8 will be properly loaded and converted to Latin-1 for the in-memory representation.

Bye : constant String (1 .. 5) := "Adiós";
--  This is proper for a Latin-1-encoded file and compiling without -gnatW8.
--  With -gnatW8 and an UTF-8 file, it will work properly and also end as a 5-byte Latin-1 string.
--  With -gnatW8 but with Latin-1-encoded file, it will fail as the sequence won't be proper UTF-8.
--  Without -gnatW8 but with an UTF-8 file it will fail as the sequence will be 6 bytes long.

Euro : constant String := "€";
--  Without -gnatW8 and a UTF-8-encoded file, this results in a 3-byte UTF-8 string.
--  With -gnatW8 and a UTF-8-encoded file, this results in an error as '€' is outside of Character.
```

## References

[1] http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-2-1.html
[2] https://docs.adacore.com/live/wave/gnat_ugn/html/gnat_ugn/gnat_ugn/building_executable_programs_with_gnat.html#character-set-control
[3] http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-3-5-2.html