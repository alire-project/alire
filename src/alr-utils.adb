with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with GNAT.SHA1;

package body Alr.Utils is

   Indicator : constant String := ".oOo";

   -------------------
   -- Busy_Activity --
   -------------------

   function Busy_Activity (Activity : String) return Busy_Prompt is
   begin
      return Busy : Busy_Prompt (Activity'Length) do
         Busy.Activity := Activity;
         Busy.Step;
      end return;
   end Busy_Activity;

   --------------
   -- Contains --
   --------------

   function Contains (V : String_Vector; Subst : String) return Boolean is
   begin
      for Str of V loop
         if Contains (Str, Subst) then
            return True;
         end if;
      end loop;

      return False;
   end Contains;

   function Crunch (Text : String) return String is
      Result : String (Text'Range);
      Src    : Natural := Text'First;
      Dst    : Natural := Result'First;
   begin
      while Src <= Text'Last loop
         if Src = Text'First or else Text (Src) /= ' ' or else Text (Src - 1) /= ' ' then
            Result (Dst) := Text (Src);
            Dst := Dst + 1;
         end if;
         Src := Src + 1;
      end loop;

      return Result (Result'First .. Dst - 1);
   end Crunch;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Busy_Prompt) is
   begin
      if Trace.Level = Info then
         Ada.Text_IO.Put (ASCII.CR & (1 .. This.Activity'Length + 1 => ' ') & ASCII.CR);
         Ada.Text_IO.Flush;
      end if;
   exception
      when others =>
         null;
   end Finalize;

   ---------------
   -- Hash_File --
   ---------------

   function Hash_File (Path : String) return String is
      use Ada.Streams;

      Buffer  : Stream_Element_Array (1 .. 1024);
      File    : Stream_IO.File_Type;
      Context : aliased GNAT.SHA1.Context;
      Hasher  : GNAT.SHA1.Hash_Stream (Context'Access);
      Last    : Stream_Element_Offset;
   begin
      Stream_IO.Open (File, Stream_IO.In_File, Path);

      while not Stream_IO.End_Of_File (File) loop
         Stream_IO.Read (File, Buffer, Last);
         Hasher.Write (Buffer (Buffer'First .. Last));
      end loop;

      Stream_IO.Close (File);

      return GNAT.SHA1.Digest (Context);
   end Hash_File;

   -----------------
   -- Hash_String --
   -----------------

   function Hash_String (Str : String) return String is
   begin
      return GNAT.SHA1.Digest (Str);
   end Hash_String;

   -------------
   -- Replace --
   -------------

   function Replace (Text : String; Match : String; Subst : String) return String is
      use Ada.Strings.Fixed;
      First : Natural;
   begin
      First := Index (Text, Match);
      if First = 0 then
         return Text;
      else
         return Replace (Replace_Slice (Text, First, First + Match'Length - 1, Subst), Match, Subst);
      end if;
   end Replace;

   ----------
   -- Step --
   ----------

   procedure Step (This : in out Busy_Prompt) is
      use Ada.Calendar;
   begin
      if Trace.Level = Info and then Clock - This.Last >= 0.1 then
         Ada.Text_IO.Put (ASCII.CR & This.Activity & " " & Indicator (This.Pos));
         Ada.Text_IO.Flush;

         This.Last := Clock;
         This.Pos  := This.Pos + 1;
         if This.Pos > Indicator'Last then
            This.Pos := Indicator'First;
         end if;
      end if;
   end Step;

end Alr.Utils;
