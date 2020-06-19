with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Alire.OS_Lib;
with Alire.Directories;
with Alire.Platform;
with Alire.Paths;

package body Alire.Environment.Formatting is

   ----------------
   -- Find_Start --
   ----------------

   function Find_Start (Str  : Unbounded_String;
                        From : Positive)
                        return Natural
   is
      Loc : Natural := From;
   begin

      loop
         Loc := Index (Str, "${", Loc);

         exit when Loc = 0;

         --  Check for escape character before the pattern
         if Loc = 1 or else Element (Str, Loc - 1) /= '\' then
            return Loc;
         else

            --  Pattern found but with an escape character
            Loc := Loc + 2;
         end if;
      end loop;

      return Loc;
   end Find_Start;

   --------------
   -- Find_End --
   --------------

   function Find_End (Str  : Unbounded_String;
                      From : Positive)
                      return Natural
   is
   begin
      --  There is not possible escape character between the start and
      --  the end of the formating pattern.
      return Index (Str, "}", From);
   end Find_End;

   ------------
   -- Format --
   ------------

   function Format (Rel             : Releases.Release;
                    Value           : String;
                    Is_Root_Release : Boolean)
                    return String
   is
      -------------
      -- Replace --
      -------------

      procedure Replace (Str : in out Unbounded_String;
                         From, To : Positive)
      is
         use Alire.OS_Lib;

         Id : constant String := Slice (Str, From + 2, To - 1);

         Working_Folder : constant Alire.Absolute_Path :=
           Alire.Directories.Current;
      begin

         if Id = "DISTRIB_ROOT" then
            Replace_Slice (Str, From, To, Platform.Distribution_Root);

         elsif Id = "CRATE_ROOT" then
            Replace_Slice
              (Str, From, To,
               Working_Folder /
                 (if Is_Root_Release
                  then ".."
                  else Alire.Paths.Working_Folder_Inside_Root
                  / Alire.Paths.Dependency_Dir_Inside_Working_Folder
                  / Rel.Unique_Folder));

         elsif Id = "_ALIRE_TEST_" then
            --  This is used to test the env var formatting feature
            Replace_Slice (Str, From, To, "TEST");
         else
            raise Unknown_Formatting_Key;
         end if;
      end Replace;

      Result : Unbounded_String := To_Unbounded_String (Value);
      From   : Natural := 1;
      To     : Natural;
   begin
      loop
         From := Find_Start (Result, From);

         if From = 0 then
            --  All patterns are replaced, if any
            exit;
         end if;

         To := Find_End (Result, From);

         if To = 0 then
            --  All patterns are replaced, if any
            exit;
         end if;

         --  A pattern is found
         Replace (Result, From, To);

         --  Start again from the beginning of the string
         From := 1;
      end loop;

      return To_String (Result);
   end Format;

end Alire.Environment.Formatting;
