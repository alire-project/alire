with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with GNAT.Case_Util;
with GNAT.OS_Lib;

package body Alire.Utils is

   -------------------------
   -- Append_To_Last_Line --
   -------------------------

   function Append_To_Last_Line (V : String_Vector; S : String) return String_Vector is
   begin
      if V.Is_Empty then
         return To_Vector (S);
      else
         return R : String_Vector := V do
            R.Delete_Last;
            R.Append_Line (V.Last_Element & S);
         end return;
      end if;
   end Append_To_Last_Line;

   --------------
   -- Contains --
   --------------

   function Contains (Text : String; Sub : String) return Boolean is
     (Ada.Strings.Fixed.Count (Text, Sub) > 0);

   ----------
   -- Head --
   ----------

   function Head (Str : String; Separator : Character) return String is
   begin
      for I in Str'Range loop
         if Str (I) = Separator then
            return Str (Str'First .. I - 1);
         end if;
      end loop;

      return Str;
   end Head;

   -------------
   -- Flatten --
   -------------

   function Flatten (V : String_Vector; Separator : String := " ") return String is

      function Flatten (Pos : Positive; V : String_Vector) return String is
        (if Pos > V.Count
         then ""
         else V (Pos) & Separator & Flatten (Pos + 1, V));

   begin
      return Flatten (1, V);
   end Flatten;

   ------------
   -- Indent --
   ------------

   function Indent (V      : String_Vector;
                    Spaces : String := "   ")
                    return   String_Vector is
   begin
      return R : String_Vector do
         for Line of V loop
            R.Append (String'(Spaces & Line));
         end loop;
      end return;
   end Indent;

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

   -----------
   -- Split --
   -----------

   function Split (Text      : String;
                   Separator : Character;
                   Side      : Halves := Head;
                   From      : Halves := Head;
                   Count     : Positive := 1;
                   Raises    : Boolean  := True) return String
   is
      Seen : Natural := 0;
      Pos  : Integer := (if From = Head then Text'First else Text'Last);
      Inc  : constant Integer := (if From = Head then 1 else -1);
   begin
      loop
         if Text (Pos) = Separator then
            Seen := Seen + 1;

            if Seen = Count then
               if Side = Head then
                  return Text (Text'First .. Pos - 1);
               else
                  return Text (Pos + 1 .. Text'Last);
               end if;
            end if;
         end if;

         Pos := Pos + Inc;

         exit when Pos not in Text'Range;
      end loop;

      if Raises then
         raise Constraint_Error with "Not enought separators found";
      else
         return Text;
      end if;
   end Split;

   ----------
   -- Tail --
   ----------

   function Tail (Str : String; Separator : Character) return String is
   begin
      for I in Str'Range loop
         if Str (I) = Separator then
            return Str (I + 1 .. Str'Last);
         end if;
      end loop;

      return "";
   end Tail;

   -------------------
   -- To_Lower_Case --
   -------------------

   function To_Lower_Case (S : String) return String is
   begin
      return SLC : String := S do
         GNAT.Case_Util.To_Lower (SLC);
      end return;
   end To_Lower_Case;

   -------------------
   -- To_Mixed_Case --
   -------------------

   function To_Mixed_Case (S : String) return String is
   begin
      return SMC : String := S do
         GNAT.Case_Util.To_Mixed (SMC);
      end return;
   end To_Mixed_Case;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (S : String) return String_Vector is
   begin
      return V : String_Vector do
         V.Append (S);
      end return;
   end To_Vector;

   --------------------
   -- Image_One_Line --
   --------------------

   function Image_One_Line (V : Vector) return String is

      use all type Vectors.Index_Type;

      function Image (V : Vector; Pos : Vectors.Index_Type) return String is
        (Image (V.Element (Pos)) &
         (if Pos = V.Last_Index
          then ""
          else Separator & Image (V, Pos + 1)));

   begin
      if V.Is_Empty then
         return When_Empty;
      else
         return Image (V, V.First_Index);
      end if;
   end Image_One_Line;

   ---------------
   -- To_Native --
   ---------------

   function To_Native (Path : Platform_Independent_Path) return String is
      Dir_Seps : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("/\");
      use Ada.Strings.Maps;
   begin
      return Native : String := Path do
         for I in Native'Range loop
            if Is_In (Path (I), Dir_Seps) then
               Native (I) := GNAT.OS_Lib.Directory_Separator;
            end if;
         end loop;
      end return;
   end To_Native;

   -----------
   -- Write --
   -----------

   procedure Write (V         : String_Vector;
                    Filename  : Platform_Independent_Path;
                    Separator : String := ASCII.LF & "")
   is
      use Ada.Streams.Stream_IO;
      F : File_Type;
   begin
      Create (F, Out_File, Filename);

      for Line of V loop
         String'Write (Stream (F), Line);
         String'Write (Stream (F), Separator);
      end loop;

      Close (F);
   end Write;

end Alire.Utils;
