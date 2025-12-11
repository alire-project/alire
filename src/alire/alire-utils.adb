with Ada.Command_Line;

with AAA.Strings; use AAA.Strings;
with Ada.Strings.Maps;

with GNAT.OS_Lib;
with GNAT.Regpat;

with Alire.Utils.TTY;

package body Alire.Utils is

   ---------------------------
   -- Command_Line_Contains --
   ---------------------------

   function Command_Line_Contains (Prefix : String) return Boolean is
   begin
      return
        (for some I in 1 .. Ada.Command_Line.Argument_Count =>
           Has_Prefix (Ada.Command_Line.Argument (I), Prefix));
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
   -- Error_In_Tag --
   ------------------

   function Error_In_Tag (Tag : String) return String
   is
      Err : UString;
      use type UString;
   begin
      if Tag'Length < 1 then
         Err := +"Tag too short (Min " & Min_Tag_Length'Img & ").";
      elsif Tag'Length > Max_Tag_Length then
         Err := +"Tag too long (Max " & Max_Tag_Length'Img & ").";
      elsif Tag (Tag'First) = '-' or else Tag (Tag'Last) = '-' then
         Err := +"Tags must not begin/end with an hyphen.";
      elsif AAA.Strings.Contains (Tag, "--") then
         Err := +"Tags cannot have two consecutive hyphens.";
      elsif (for some C of Tag => C not in Tag_Character) then
         Err := +"Tags must be lowercase ASCII alphanumerical" &
           " with optional hyphens.";
      end if;

      if Err /= "" then
         return "Invalid Tag '" & Utils.TTY.Name (Tag) & "': " & (+Err);
      else
         return "";
      end if;
   end Error_In_Tag;

   ------------------
   -- Is_Valid_Tag --
   ------------------

   function Is_Valid_Tag (Tag : String) return Boolean
   is (Error_In_Tag (Tag) = "");

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

   ------------------------
   -- Finalize_Exception --
   ------------------------

   procedure Finalize_Exception (E : Ada.Exceptions.Exception_Occurrence) is

      --  Import a Last_Chance_Handler procedure that will either be the one
      --  declared by Alr, or the default GNAT last chance handler.

      procedure Last_Chance_Handler (E : Ada.Exceptions.Exception_Occurrence);
      pragma Import (C,
                     Last_Chance_Handler,
                     "__gnat_last_chance_handler");
      pragma No_Return (Last_Chance_Handler);

   begin
      Last_Chance_Handler (E);
   end Finalize_Exception;

   --------------------
   -- Has_Duplicates --
   --------------------

   function Has_Duplicates
     (V         : AAA.Strings.Vector;
      Transform : access function (S : String) return String := null)
      return Boolean is
      Seen : AAA.Strings.Set := AAA.Strings.Empty_Set;
   begin
      for Elt of V loop
         declare
            Transformed : constant String := (if Transform /= null
                                                then Transform (Elt)
                                                else Elt);
         begin
            if Seen.Contains (Transformed) then
               return True;
            else
               Seen.Insert (Transformed);
            end if;
         end;
      end loop;
      return False;
   end Has_Duplicates;

end Alire.Utils;
