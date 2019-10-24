with Alire.OS_Lib.Subprocess;
with Alire.Platform;
with Alire.Utils;

package body Alire.Origins.Deployers.APT is

   package Subprocess renames Alire.OS_Lib.Subprocess;

   -----------------------
   -- Already_Installed --
   -----------------------

   overriding function Already_Installed (This : Deployer) return Boolean is
      Exit_Code : Integer;
      Output    : Utils.String_Vector;
   begin
      Exit_Code := Subprocess.Spawn_And_Capture
        (Output, "apt-cache", "policy " &
           This.Base.Package_Name (Platform.Distribution));

      if Exit_Code /= 0 then
         Trace.Error ("apt-cache policy exited with error:" & Exit_Code'Img);
         raise Internal_Error;
      end if;

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
      Exit_Code : Integer;
   begin
      Exit_Code :=
        Subprocess.Spawn ("sudo",
                          "apt-get install --no-remove -q -q -y " &
                            This.Base.Package_Name (Platform.Distribution));

      if Exit_Code /= 0 then
         return Outcome_Failure ("apt-get install exited with code:"
                                 & Exit_Code'Img);
      end if;

      return Outcome_Success;
   end Deploy;

   ------------
   -- Exists --
   ------------

   overriding function Exists (This : Deployer) return Boolean is
      Exit_Code : Integer;
      Output    : Utils.String_Vector;
      use Utils;
   begin
      Exit_Code := Subprocess.Spawn_And_Capture
        (Output, "apt-cache", "-q policy " &
           This.Base.Package_Name (Platform.Distribution));

      if Exit_Code /= 0 then
         Trace.Error ("apt-cache policy exited with error:" & Exit_Code'Img);
         raise Internal_Error;
      end if;

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
      Exit_Code : Integer;
      Output    : Utils.String_Vector;
      use Utils;
   begin
      Exit_Code := Subprocess.Spawn_And_Capture
        (Output, "apt-cache", "-q policy " & Name);
      if Exit_Code /= 0 then
         Uncontained_Error
           ("apt-cache policy exited with code:" & Exit_Code'Img);
      end if;

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
