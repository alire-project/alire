with Ada.Characters.Latin_1;
with AAA.Strings; use AAA.Strings;

with Alire.Errors;
with Alire.OS_Lib.Subprocess;
--  with Alire.Utils.Regex;

package body Alire.Origins.Deployers.System.Portage is

   package L1         renames Ada.Characters.Latin_1;
   package Subprocess renames Alire.OS_Lib.Subprocess;

   -----------------------
   -- Already_Installed --
   -----------------------

   overriding function Already_Installed (This : Deployer) return Boolean
   is
      --  The following call is faster than using emerge.
      Output : constant AAA.Strings.Vector :=
        Subprocess.Checked_Spawn_And_Capture
          ("eix",
           Empty_Vector & "-ce" & This.Base.Package_Name,
           --  Check the category (A),
           --  whether it is installed (I) and
           --  match against the exact string (e).
           --  i.e. "app-editors/zed"
           Valid_Exit_Codes => (1, 0), -- returns 1 when not found
           Err_To_Out       => True);

      Indicator : constant String := "[I]";
      Line      : constant String := AAA.Strings.First_Element (Output);
   begin
      Trace.Info ("Already_Installed: " & This.Base.Package_Name);

      return (if Line (Line'First .. Line'First + Indicator'Length) = Indicator
              then True else False);
   end Already_Installed;

   function To_Semantic_Version (Gentoo_Version : String) return String is
      Tmp : String := Gentoo_Version;
   begin
      for C in Tmp'Range loop
         --  Format: 0.0.0_release-rc9
         if Tmp (C) = L1.Low_Line then
            Tmp (C) := L1.Hyphen;

            Trace.Info ("To_Semantic_Version: " & Tmp);

            return Tmp;
         end if;
      end loop;

      Trace.Info ("To_Semantic_Version: " & Tmp);

      return Tmp;
   end To_Semantic_Version;

   ------------
   -- Detect --
   ------------

   overriding
   function Detect (This : Deployer) return Version_Outcomes.Outcome
   is
      function Split_Version (Gentoo_Package_Version : String) return String is
      begin
         for Index in Gentoo_Package_Version'Range loop
            if Gentoo_Package_Version (Index) = L1.Hyphen then
               return Gentoo_Package_Version
                 (Index + 1 .. Gentoo_Package_Version'Last);
            end if;
         end loop;

         return "";
      end Split_Version;
      pragma Unreferenced (Split_Version);

      PortageQ_Output : constant AAA.Strings.Vector :=
                    Subprocess.Checked_Spawn_And_Capture
                      ("portageq",
                       Empty_Vector & "best_visible" & "/" &
                         This.Base.Package_Name,
                       Valid_Exit_Codes => (0, 1), -- Returned when not found
                       Err_To_Out       => True);
      --  portageq should *always* produce zero or one line result.

      Output    : constant AAA.Strings.Vector :=
                    Subprocess.Checked_Spawn_And_Capture
                      ("qatom",
                       Empty_Vector & "-F%{PVR}" &
                         First_Element (PortageQ_Output),
                       Valid_Exit_Codes => (0, 1), -- Returned when not found
                       Err_To_Out       => True);
      Gentoo_Version : constant String := First_Element (Output);

      --  Regexp : constant String :=
      --    "[0-9]+(\.[0-9]+)*[a-z](_[a-z]+[0-9]?)*(-r[0-9]*)*";
      --  From the pms.pdf.
   begin
      if Gentoo_Version /= "" then
         Trace.Info ("Detect: " & This.Base.Package_Name & " - " &
           Gentoo_Version & " - " & To_Semantic_Version (Gentoo_Version) &
           " => " & Semantic_Versioning.Parse
                         (To_Semantic_Version (Gentoo_Version),
                            Relaxed => True).Image);

         return
            Version_Outcomes.New_Result
               (Semantic_Versioning.Parse
                 (To_Semantic_Version (Gentoo_Version), Relaxed => True));
      end if;

      Trace.Debug ("System deployer could not detect: " & This.Base.Image);
      return Version_Outcomes.Outcome_Failure ("could not be detected",
                                               Report => False);
   end Detect;

   -------------
   -- Install --
   -------------

   overriding
   function Install (This : Deployer) return Outcome is
   begin
      Trace.Info ("Install: " & This.Base.Package_Name);

      Subprocess.Checked_Spawn
        ("sudo", Empty_Vector &
           "emerge" &
           "-av" &
           This.Base.Package_Name);

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Install;

end Alire.Origins.Deployers.System.Portage;
