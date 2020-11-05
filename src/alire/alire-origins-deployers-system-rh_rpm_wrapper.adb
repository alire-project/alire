with Alire.OS_Lib.Subprocess;
with Alire.Utils;             use Alire.Utils;
with Alire.Errors;

with GNAT.Regpat;

package body Alire.Origins.Deployers.System.Rh_Rpm_Wrapper is

   package Subprocess renames Alire.OS_Lib.Subprocess;

   ---------------------
   -- Wrapper_Command --
   ---------------------

   function Wrapper_Command (This : Deployer) return String is
     (case This.Wrapper is
         when Dnf => "dnf",
         when Yum => "yum");

   ------------------
   -- Rh_To_Semver --
   ------------------
   --  Transform a Redhat version scheme into a semver one
   function Rh_To_Semver (Version : String) return String is

      use GNAT.Regpat;

      --  Used to detect a 1 digit version
      --  Detect all following strings:
      --  1
      --  1-1
      --  1a
      --  1a-1
      --  1a1
      --  1a1-1
      Regexp_1 : constant String :=
        "^(\d+)(([a-z]+)(\d+)?)?(?:-(\d+))?";

      --  Used to detect a 2 digits version
      --  Detect all following strings:
      --  1.2
      --  1.2-1
      --  1.2a
      --  1.2a-1
      --  1.2a1
      --  1.2a1-1
      Regexp_2 : constant String :=
        "^(\d+)\.(\d+)(([a-z]+)(\d+)?)?(?:-(\d+))?";

      --  Used to detect a 3 digits version
      --  Detect all following strings:
      --  1.2.3
      --  1.2.3-1
      --  1.2.3a
      --  1.2.3a-1
      --  1.2.3a1
      --  1.2.3a1-1
      Regexp_3 : constant String :=
        "^(\d+)\.(\d+)\.(\d+)(([a-z]+)(\d+)?)?(?:-(\d+))?";

      --  Used to detect a 4 digits version
      --  Detect all following strings:
      --  1.2.3.4
      --  1.2.3.4-1
      --  1.2.3.4a
      --  1.2.3.4a-1
      --  1.2.3.4a1
      --  1.2.3.4a1-1
      Regexp_4 : constant String :=
        "^(\d+)\.(\d+)\.(\d+)\.(\d+)(([a-z]+)(\d+)?)?(?:-(\d+))?";

      Matches : Match_Array (0 .. 8);

      Major_Num : Natural := 0;
      Minor_Num : Natural := 0;
      Patch_Num : Natural := 0;
      Seq_Num   : Natural := 0;
      Pre_Num   : Natural := 0;
      Suffix    : UString := +"";

      procedure Extract_Components (
                                    Major_Idx  : Natural;
                                    Minor_Idx  : Natural := 0;
                                    Patch_Idx  : Natural := 0;
                                    Seq_Idx    : Natural := 0;
                                    Suffix_Idx : Natural := 0;
                                    Pre_Idx    : Natural := 0) is
      begin
         if Major_Idx /= 0 and then Matches (Major_Idx) /= No_Match then
            Major_Num := Natural'Value (
              Version (Matches (Major_Idx).First .. Matches (Major_Idx).Last));
         end if;

         if Minor_Idx /= 0 and then Matches (Minor_Idx) /= No_Match then
            Minor_Num := Natural'Value (
              Version (Matches (Minor_Idx).First .. Matches (Minor_Idx).Last));
         end if;

         if Patch_Idx /= 0 and then Matches (Patch_Idx) /= No_Match then
            Patch_Num := Natural'Value (
              Version (Matches (Patch_Idx).First .. Matches (Patch_Idx).Last));
         end if;

         if Seq_Idx /= 0 and then Matches (Seq_Idx) /= No_Match then
            Seq_Num := Natural'Value (
              Version (Matches (Seq_Idx).First .. Matches (Seq_Idx).Last));
         end if;

         if Suffix_Idx /= 0 and then Matches (Suffix_Idx) /= No_Match then
            Suffix := +Version (
               Matches (Suffix_Idx).First .. Matches (Suffix_Idx).Last);
         end if;

         if Pre_Idx /= 0 and then Matches (Pre_Idx) /= No_Match then
            Pre_Num := Natural'Value (
              Version (Matches (Pre_Idx).First .. Matches (Pre_Idx).Last));
         end if;

      end Extract_Components;

   begin
      --  Check wether there are 4 digits
      Match (Regexp_4, Version, Matches);

      if Matches (0) /= No_Match then
         Extract_Components (Major_Idx => 1,
                             Minor_Idx => 2,
                             Patch_Idx => 3,
                             Seq_Idx => 4,
                             Suffix_Idx => 5,
                             Pre_Idx => 8);

      else
         --  Check wether there are 3 digits
         Match (Regexp_3, Version, Matches);

         if Matches (0) /= No_Match then
            Extract_Components (Major_Idx => 1,
                                Minor_Idx => 2,
                                Patch_Idx => 3,
                                Suffix_Idx => 4,
                                Pre_Idx => 7);

         else
            --  Check wether there are 2 digits
            Match (Regexp_2, Version, Matches);

            if Matches (0) /= No_Match then
               Extract_Components (Major_Idx => 1,
                                   Minor_Idx => 2,
                                   Suffix_Idx => 3,
                                   Pre_Idx => 6);

            else
               --  Check wether there is 1 digit
               Match (Regexp_1, Version, Matches);

               if Matches (0) /= No_Match then
                  Extract_Components (Major_Idx => 1,
                                      Suffix_Idx => 2,
                                      Pre_Idx => 5);

               end if;

            end if;

         end if;

      end if;

      --  We want to transform a RedHat version like this:
      --  1.4.1.1-4
      --  into this:
      --  1.4.11004

      Patch_Num := (Patch_Num * 10000) + (Seq_Num * 1000);

      if Pre_Num > 0 then
         Patch_Num := Patch_Num + Pre_Num;
      end if;

      return Replace (Major_Num'Img &
               "." & Minor_Num'Img &
               "." & Patch_Num'Img &
               (if Length (Suffix) > 0 then "+" & (+Suffix) else ""),
               " ", "");

   end Rh_To_Semver;

   -----------------------
   -- Already_Installed --
   -----------------------

   overriding
   function Already_Installed (This : Deployer) return Boolean is
      Pck    : String renames This.Base.Package_Name;

      Wrapper : constant String := Wrapper_Command (This);

      Output : constant Utils.String_Vector :=
                 Subprocess.Checked_Spawn_And_Capture
                   (Wrapper,
                    Empty_Vector &
                      "-y" &
                      "list" &
                      "--installed" &
                      This.Base.Package_Name,
                    Valid_Exit_Codes => (0, 1), -- Returned when not found
                    Err_To_Out     => True);
   begin
      if not Output.Is_Empty
        and then
          Output.First_Element = Pck
      then
         return True;
      else
         Trace.Detail ("Cannot find package '" & Pck &
                         "' in " & Wrapper & " installed list: '" &
                         Output.Flatten & "'");
         return False;
      end if;
   end Already_Installed;

   ------------
   -- Detect --
   ------------

   overriding
   function Detect (This : Deployer) return Version_Outcomes.Outcome is

      Regexp : constant String := "^" & This.Base.Package_Name &
        "[^\s]+\s+([^\s]+)";

      Wrapper : constant String := Wrapper_Command (This);

      Output    : constant Utils.String_Vector :=
        Subprocess.Checked_Spawn_And_Capture
          (Wrapper,
           Empty_Vector &
             "-y" &
             "list" &
             This.Base.Package_Name,
           Valid_Exit_Codes => (0, 1), -- Returned when not found
           Err_To_Out       => True);

      use GNAT.Regpat;
      Matches : Match_Array (1 .. 1);
   begin
      for Line of Output loop
         Trace.Debug ("Extracting native version from " & Wrapper &
                        " output: " & Line);
         Match (Regexp, Line, Matches);
         if Matches (1) /= No_Match then
            Trace.Debug ("Candidate version string: "
                         & Line (Matches (1).First .. Matches (1).Last));
            return
              Version_Outcomes.New_Result
                (Semantic_Versioning.Parse
                   (Rh_To_Semver
                      (Line (Matches (1).First .. Matches (1).Last)),
                       Relaxed => True)); --  Relaxed because some Fedora
                                          --  versions have extra version pts,
                                          --  e.g. 5.8-3.fc33
         else
            Trace.Debug
              ("Unexpected version format, could not identify version");
         end if;
      end loop;

      return Version_Outcomes.Outcome_Failure ("could not be detected");
   end Detect;

   -------------
   -- Install --
   -------------

   overriding
   function Install (This : Deployer) return Outcome is

      Wrapper : constant String := Wrapper_Command (This);

   begin
      Subprocess.Checked_Spawn
        ("sudo", Empty_Vector &
           Wrapper &
           "-y" &
           "install" &
           This.Base.Package_Name);

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Install;

end Alire.Origins.Deployers.System.Rh_Rpm_Wrapper;
