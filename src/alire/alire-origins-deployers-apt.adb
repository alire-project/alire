with Alire.OS_Lib.Subprocess;
with Alire.Platform;
with Alire.Utils;             use Alire.Utils;
with Alire.Errors;

package body Alire.Origins.Deployers.APT is

   package Subprocess renames Alire.OS_Lib.Subprocess;

   -----------------------
   -- Already_Installed --
   -----------------------

   overriding function Already_Installed (This : Deployer) return Boolean is
      Output : constant Utils.String_Vector :=
                 Subprocess.Checked_Spawn_And_Capture
                   ("apt-cache",
                    Empty_Vector & "policy"
                    & This.Base.Package_Name (Platform.Distribution));
   begin
      for Line of Output loop
         if Utils.Contains (Line, "Installed")
           and then
            not Utils.Contains (Line, "none")
         then
            return True;
         end if;
      end loop;

      return False;
   end Already_Installed;

   ------------
   -- Deploy --
   ------------

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is
      pragma Unreferenced (Folder);
   begin
      Subprocess.Checked_Spawn
        ("sudo", Empty_Vector &
           "apt-get" &
           "install" &
           "--no-remove" &
           "-q" &
           "-q" &
           "-y" &
           This.Base.Package_Name (Platform.Distribution));

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Deploy;

   ------------
   -- Exists --
   ------------

   overriding function Exists (This : Deployer) return Boolean is
      Output : constant String_Vector :=
                 Subprocess.Checked_Spawn_And_Capture
                   ("apt-cache",
                    Empty_Vector &
                      "-q" &
                      "policy" &
                      This.Base.Package_Name (Platform.Distribution));
   begin
      for Line of Output loop
         if Contains (To_Lower_Case (Line), "candidate:")
           and then
            not Contains (To_Lower_Case (Line), "none")
         then
            return True;
         end if;
      end loop;

      return False;
   end Exists;

   -----------
   -- Fetch --
   -----------

   overriding
   function Fetch (This   : Deployer; Folder : String) return Outcome is
     (Outcome_Success);

   --------------------
   -- Native_Version --
   --------------------

   function Native_Version (Name : String) return String is
      Output : constant Utils.String_Vector :=
                 Subprocess.Checked_Spawn_And_Capture
                   ("apt-cache",
                    Empty_Vector &
                      "-q" &
                      "policy" &
                      Name);
   begin
      for Line of Output loop
         if Contains (To_Lower_Case (Line), "candidate:")
           and then
            not Contains (To_Lower_Case (Line), "none")
         then
            return Trim (Tail (Line, ':'));
         end if;
      end loop;

      return "";
   end Native_Version;

   --------------------
   -- Native_Version --
   --------------------

   overriding function Native_Version (This : Deployer) return String is
      (Native_Version (This.Base.Package_Name (Platform.Distribution)));

end Alire.Origins.Deployers.APT;
