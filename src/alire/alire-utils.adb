with Ada.Command_Line;

with AAA.Strings; use AAA.Strings;
with Ada.Strings.Maps;

with GNAT.OS_Lib;
with GNAT.Regpat;

package body Alire.Utils is

   ---------------------------
   -- Command_Line_Contains --
   ---------------------------

   function Command_Line_Contains (Prefix : String) return Boolean is
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         if Has_Prefix (Ada.Command_Line.Argument (I), Prefix) then
            return True;
         end if;
      end loop;

      return False;
   end Command_Line_Contains;

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

   ----------------
   -- Count_True --
   ----------------

   function Count_True (Booleans : Boolean_Array) return Natural is
   begin
      return Set : Natural := 0 do
         for Bool of Booleans loop
            if Bool then
               Set := Set + 1;
            end if;
         end loop;
      end return;
   end Count_True;

   -----------------
   -- First_Match --
   -----------------

   function First_Match (Regex : String; Text : String) return String is

      -----------------------
      -- Count_Parentheses --
      -----------------------

      function Count_Parentheses return Positive is
         Count : Natural := 0;
      begin
         for Char of Regex loop
            if Char = '(' then
               Count := Count + 1;
            end if;
         end loop;
         return Count;
      end Count_Parentheses;

      use GNAT.Regpat;
      Matches : Match_Array (1 .. Count_Parentheses);
      --  This is a safe estimation, as some '(' may not be part of a capture

   begin
      Match (Regex, Text, Matches);

      for I in Matches'Range loop
         if Matches (I) /= No_Match then
            return Text (Matches (I).First .. Matches (I).Last);
         end if;
      end loop;

      return "";
   end First_Match;

   -------------------------------
   -- Is_Valid_Full_Person_Name --
   -------------------------------

   function Is_Valid_Full_Person_Name (Name : String) return Boolean
   is (for all C of Name =>
          C not in Character'Val (0) .. Character'Val (31) | '\');

   ------------------------------
   -- Is_Valid_GitHub_Username --
   ------------------------------

   function Is_Valid_GitHub_Username (User : String) return Boolean is
     ((for all C of User => C in '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-')
      and then User'Length in 1 .. 39
      and then User (User'First) /= '-'
      and then User (User'Last) /= '-'
      and then not AAA.Strings.Contains (User, "--"));

   ------------------
   -- Is_Valid_Tag --
   ------------------

   function Is_Valid_Tag (Tag : String) return Boolean is
     ((for all C of Tag => C in '0' .. '9' | 'a' .. 'z' | '-')
      and then Tag'Length in 1 .. Max_Tag_Length
      and then Tag (Tag'First) /= '-'
      and then Tag (Tag'Last) /= '-'
      and then not AAA.Strings.Contains (Tag, "--"));

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

   -------------------------
   -- Image_Keys_One_Line --
   -------------------------

   function Image_Keys_One_Line (M : Maps.Map) return String is
   begin
      if M.Is_Empty then
         return When_Empty;
      else
         declare
            use Ada.Strings.Unbounded;
            US : Unbounded_String;
            First : Boolean := True;
         begin
            for C in M.Iterate loop
               if First then
                  Append (US, Maps.Key (C));
                  First := False;
               else
                  Append (US, Separator & Maps.Key (C));
               end if;
            end loop;

            return To_String (US);
         end;
      end if;
   end Image_Keys_One_Line;

end Alire.Utils;
