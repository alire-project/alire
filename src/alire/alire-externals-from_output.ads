private with Alire.Utils;
private with GNAT.Regpat;

package Alire.Externals.From_Output is

   --  This kind of external runs a command known to provide the tool version,
   --  which is then extracted with a regular expression.

   type External is new Externals.External with private;

   overriding
   function Detect (This : External;
                    Name : Crate_Name) return Containers.Release_Set;

   overriding
   function Image (This : External) return String;

   overriding
   function Detail (This          : External;
                    Unused_Distro : Platforms.Distributions)
                    return Utils.String_Vector;

   overriding
   function Kind (This : External) return String is ("Executable");

   function From_TOML (From : TOML_Adapters.Key_Queue) return External;

private

   use type GNAT.Regpat.Program_Size;

   type External is new Externals.External with record
      Command : Utils.String_Vector;
      Regexp  : GNAT.Regpat.Pattern_Matcher (GNAT.Regpat.Max_Program_Size - 1);
      --  There's a off-by-one bug when using Max_Program_Size that results in
      --  freezes/storage errors.
      Regstr  : UString; -- Original regexp for Detail output
   end record;

end Alire.Externals.From_Output;
