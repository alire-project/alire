with Ada.Streams.Stream_IO;
with Ada.Strings.Maps;

with GNAT.Case_Util;
with GNAT.OS_Lib;
with GNAT.Regpat;

package body Alire.Utils is

   ------------
   -- Append --
   ------------

   function Append (V : String_Vector;
                    S : String) return String_Vector is
   begin
      return R : String_Vector := V do
         R.Append (S);
      end return;
   end Append;

   ------------
   -- Append --
   ------------

   function Append (L, R : String_Vector) return String_Vector is
   begin
      return Result : String_Vector := L do
         Result.Append (R);
      end return;
   end Append;

   -------------------------
   -- Append_To_Last_Line --
   -------------------------

   function Append_To_Last_Line (V : String_Vector;
                                 S : String)
                                 return String_Vector
   is
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

   -------------
   -- Convert --
   -------------

   function Convert (V : Vector) return Other_Vector is
      OV : Other_Vector := Initial_Other_Vector;
   begin
      for E of V loop
         Append (OV, To_New_Value (E));
      end loop;

      return OV;
   end Convert;

   -----------------------
   -- Could_Be_An_Email --
   -----------------------

   type Matcher_Access is access GNAT.Regpat.Pattern_Matcher;

   Email_Matcher : Matcher_Access;
   --  Holds the email pattern matcher, compiled on first use.

   Email_With_Name_Matcher : Matcher_Access;
   --  Likewise, with a name and email enclosed in '<...>'.

   function Could_Be_An_Email (Str       : String;
                               With_Name : Boolean) return Boolean
   is
      Pat_Printable : constant String := "[!-~]";
      --  Anything printable (ASCII 32-126).

      Pat_Printable_But_Dot : constant String := "[!--/-~]";
      --  Anything printable but a dot

      Pat_User      : constant String :=
                        Pat_Printable_But_Dot &
                        Pat_Printable & "*";
      --  Part before '@', anything printable goes except starting with  '.'
      --  (ending is valid).

      Pat_Subdomain : constant String :=
                        "([[:alnum:]]([[:alnum:]]|-){0,61}[[:alnum:]])";
      --  Subdomain parts; alphanumeric plus dash sequences, not
      --  starting/ending with a dash. Length in 2..63 (RFC 1035).

      Pat_Domain    : constant String :=
                        Pat_Subdomain & "(\." & Pat_Subdomain & "){1,85}";
      --  A domain is at least two subdomains separated by dots. A domain can
      --  be at worst 255 chars in length, but subs are already 2 + dot.

      Pat_Email     : constant String := Pat_User & "@" & Pat_Domain;
      --  user@do.ma.in

      Pat_Only_Email : constant String := "^" & Pat_Email & "$";
      --  An email without anything before or after.

      Pat_Named_Email : constant String :=
                          "^[^<]+ <" & Pat_Email & ">$";
      --  A name plus a <...> quoted email.

      Pat_With_Or_Without_Name : constant String :=
                                   "(" &
                                   Pat_Only_Email & ")|("
                                   & Pat_Named_Email & ")";
      --  Accept either of the two.

      use GNAT.Regpat;
   begin
      --  Initialize matchers on first call:

      if Email_Matcher = null then
         Trace.Debug ("Compiling email pattern...: " & Pat_Only_Email);
         Email_Matcher :=
           new Pattern_Matcher'(Compile (Pat_Only_Email));

         Trace.Debug ("Compiling named email pattern...: " & Pat_Named_Email);
         Email_With_Name_Matcher :=
           new Pattern_Matcher'(Compile (Pat_With_Or_Without_Name));
      end if;

      --  Do the matching:

      return Match (Self => (if With_Name
                             then Email_With_Name_Matcher.all
                             else Email_Matcher.all),
                    Data => Str);
   end Could_Be_An_Email;

   ------------
   -- Crunch --
   ------------

   function Crunch (Text : String) return String is
      Result : String (Text'Range);
      Src    : Natural := Text'First;
      Dst    : Natural := Result'First;
   begin
      --  Trim initial spaces:
      while Src <= Text'Last and then Text (Src) = ' ' loop
         Src := Src + 1;
      end loop;

      --  Remove excess spaces:
      while Src <= Text'Last loop
         if Src = Text'First
           or else
            Text (Src) /= ' '
           or else
            Text (Src - 1) /= ' '
         then
            Result (Dst) := Text (Src);
            Dst := Dst + 1;
         end if;
         Src := Src + 1;
      end loop;

      return Result (Result'First .. Dst - 1);
   end Crunch;

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

   function Flatten (V         : String_Vector;
                     Separator : String := " ")
                     return String
   is

      function Flatten (Pos : Positive; V : String_Vector) return String;

      -------------
      -- Flatten --
      -------------

      function Flatten (Pos : Positive; V : String_Vector) return String is
        (if Pos = V.Count
         then V (Pos)
         else V (Pos) & Separator & Flatten (Pos + 1, V));

   begin
      if V.Is_Empty then
         return "";
      else
         return Flatten (1, V);
      end if;
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

   ------------------------------
   -- Is_Valid_GitHub_Username --
   ------------------------------

   function Is_Valid_GitHub_Username (User : String) return Boolean is
     ((for all C of User => C in '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-')
      and then User'Length in 1 .. 39
      and then User (User'First) /= '-'
      and then User (User'Last) /= '-'
      and then not Contains (User, "--"));

   ------------------
   -- Is_Valid_Tag --
   ------------------

   function Is_Valid_Tag (Tag : String) return Boolean is
     ((for all C of Tag => C in '0' .. '9' | 'a' .. 'z' | '-')
      and then Tag (Tag'First) /= '-'
      and then Tag (Tag'Last) /= '-'
      and then not Contains (Tag, "--"));

   --------------
   -- New_Line --
   --------------

   function New_Line (V : String_Vector) return String_Vector
   is (V.Append (""));

   -------------
   -- Replace --
   -------------

   function Replace (Text  : String;
                     Match : String;
                     Subst : String)
                     return String
   is
      use Ada.Strings.Fixed;
      First : Natural;
   begin
      First := Index (Text, Match);
      if First = 0 then
         return Text;
      else
         return Replace
           (Replace_Slice (Text, First, First + Match'Length - 1, Subst),
            Match,
            Subst);
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

   -----------
   -- Split --
   -----------

   function Split (S : String; Separator : Character) return String_Vector is
      Prev : Integer := S'First - 1;
   begin
      return V : String_Vector do
         for I in S'Range loop
            if S (I) = Separator then
               V.Append (S (Prev + 1 .. I - 1));
               Prev := I;
            end if;
         end loop;
         V.Append (S (Prev + 1 .. S'Last));
      end return;
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

   ----------
   -- Tail --
   ----------

   function Tail (V : String_Vector) return String_Vector is
   begin
      return Result : String_Vector := V do
         Result.Delete_First;
      end return;
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

   function To_Native (Path : Any_Path) return String is
      Dir_Seps : constant Ada.Strings.Maps.Character_Set :=
        Ada.Strings.Maps.To_Set ("/\");

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
                    Filename  : Any_Path;
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
