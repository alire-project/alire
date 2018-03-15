with Alr.OS;
with Alr.OS_Lib;
with Alr.Utils;

package body Alr.Origins.Apt is

   -----------------------
   -- Already_Installed --
   -----------------------

   function Already_Installed (This : Origin) return Boolean is
      Output : Utils.String_Vector;
   begin
      Output := OS_Lib.Spawn_And_Capture ("apt-cache", "policy " &
                                            This.Base.Package_Name (OS.Distribution));
      for Line of Output loop
         if Utils.Contains (Line, "Installed") and then not Utils.Contains (Line, "none") then
            return True;
         end if;
      end loop;

      return False;
   end Already_Installed;

   ------------
   -- Exists --
   ------------

   overriding function Exists (This : Origin) return Boolean is
      Output : constant Utils.String_Vector :=
                 OS_Lib.Spawn_And_Capture ("apt-cache", "-q policy " &
                                                    This.Base.Package_Name (OS.Distribution));
      use Utils;
   begin
      for Line of Output loop
         if Contains (To_Lower_Case (Line), "candidate:") and then
           not Contains (To_Lower_Case (Line), "none") then
            return True;
         end if;
      end loop;

      return False;
   end Exists;

   -----------
   -- Fetch --
   -----------

   overriding procedure Fetch (This : Origin; Folder : String) is
   begin
      raise Program_Error;
   end Fetch;

   -------------
   -- Install --
   -------------

   overriding procedure Install (This : Origin) is
   begin
      OS_Lib.Spawn_Raw ("sudo", "apt-get install -q -q -y " &  This.Base.Package_Name (OS.Distribution));
   end Install;

   --------------------
   -- Native_Version --
   --------------------

   overriding function Native_Version (This : Origin) return String is
      Output : constant Utils.String_Vector :=
                 OS_Lib.Spawn_And_Capture ("apt-cache", "-q policy " &
                                                    This.Base.Package_Name (OS.Distribution));
      use Utils;
   begin
      for Line of Output loop
         if Contains (To_Lower_Case (Line), "candidate:") and then
           not Contains (To_Lower_Case (Line), "none") then
            return Trim (Tail (Line, ':'));
         end if;
      end loop;

      return "";
   end Native_Version;

end Alr.Origins.Apt;
