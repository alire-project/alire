with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Alire.OS_Lib;
with Alire.Platforms.Current;

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
      --  the end of the formatting pattern.
      return Index (Str, "}", From);
   end Find_End;

   ------------
   -- Format --
   ------------

   function Format (Release_Dir : Any_Path;
                    Value       : String)
                    return String
   is
      -------------
      -- Replace --
      -------------

      procedure Replace (Str : in out Unbounded_String;
                         From, To : Positive)
      is
         Id : constant String := Slice (Str, From + 2, To - 1);
      begin

         if Id = "DISTRIB_ROOT" then
            Replace_Slice (Str, From, To, Platforms.Current.Distribution_Root);

         elsif Id = "CRATE_ROOT" then
            Replace_Slice
              (Str, From, To,
               Release_Dir);

         elsif Id = "_ALIRE_TEST_" then
            --  This is used to test the env var formatting feature
            Replace_Slice (Str, From, To, "TEST");
         else
            raise Unknown_Formatting_Key;
         end if;
      end Replace;

      ---------------
      -- To_Native --
      ---------------
      --  Replace forward slashes with native slashes on Windows, unless they
      --  are an escape sequence.
      function To_Native (S : String) return String is
      begin
         case OS_Lib.Dir_Separator is
            when '/' => return S;
            when '\' => null;
            when others => raise Unimplemented with
                 "Unknown OS with dir separator: " & OS_Lib.Dir_Separator;
         end case;

         return AAA.Strings.Replace (S, "/", "" & OS_Lib.Dir_Separator);
      end To_Native;

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

      --  For final usage, we use the native separator
      return To_Native (+Result);
   end Format;

end Alire.Environment.Formatting;
