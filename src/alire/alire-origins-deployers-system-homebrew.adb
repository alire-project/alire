with AAA.Strings; use AAA.Strings;

with Alire.OS_Lib.Subprocess;
with Alire.Errors;

with LML.Input.JSON;

package body Alire.Origins.Deployers.System.Homebrew is

   --  Ada.Strings.Unbounded is use-visible via Alire.Origins.

   package Subprocess renames Alire.OS_Lib.Subprocess;

   procedure Get_Info (Package_Name : String;
                       Available_Version : out Unbounded_String;
                       Installed_Version : out Unbounded_String);
   --  Queries the versions using 'brew info'.

   procedure Get_Info (Package_Name : String;
                       Available_Version : out Unbounded_String;
                       Installed_Version : out Unbounded_String)
   is

      --  The format of the JSON returned by 'brew info --json=v1 {pkg}' is
      --  [
      --      {
      --          "stuff": {},
      --          "versions": {
      --              "stable": "1.2.4_0",
      --              "stuff": {}
      --          },
      --          "stuff": {},
      --          "installed": [
      --              {
      --                  "version": "1.2.3_1",
      --                  "stuff": {}
      --              }
      --          ],
      --          "stuff": {}
      --      }
      --  ]

      Info : AAA.Strings.Vector;
      JSON_Issue : exception;
   begin
      if Subprocess.Unchecked_Spawn_And_Capture
        ("brew",
         Empty_Vector & "info" & "--json=v1" & Package_Name,
         Output     => Info,
         Err_To_Out => True) /= 0
      then
         --  failed.
         Trace.Debug ("brew failed to find " & Package_Name);
         Available_Version := Null_Unbounded_String;
         Installed_Version := Null_Unbounded_String;
         return;
      end if;

      Homebrew_Found_Required_Package :
      declare
         package Yeison renames LML.Yeison;
         use all type Yeison.Kinds;

         Data : constant Yeison.Any :=
                  LML.Input.JSON.From_UTF_8 (AAA.Strings.Flatten (Info));
      begin
         if Data.Kind /= Vec_Kind then
            raise JSON_Issue with "JSON info not array";
         end if;
         if Data.Length /= 1 then
            raise JSON_Issue with "JSON info length /= 1";
         end if;
         if Data (1).Kind /= Map_Kind then
            raise JSON_Issue with "JSON info (1) not object";
         end if;

         if Data (1).Has_Field ("versions") then
            if Data (1) ("versions").Kind /= Map_Kind then
               raise JSON_Issue with "JSON 'versions' not object";
            end if;
            Available_Version := To_Unbounded_String
              (Data (1) ("versions") ("stable").As_UTF_8);
            Trace.Debug ("available: " & To_String (Available_Version));
         end if;

         if Data (1).Has_Field ("installed") then
            if Data (1) ("installed").Kind /= Vec_Kind then
               raise JSON_Issue with "JSON 'installed' not array";
            end if;
            if not Data (1) ("installed").Is_Empty
              and then Data (1) ("installed") (1).Has_Field ("version")
            then
               Installed_Version := To_Unbounded_String
                 (Data (1) ("installed") (1) ("version").As_UTF_8);
            end if;
            Trace.Debug ("installed: " & To_String (Installed_Version));
         end if;
      end Homebrew_Found_Required_Package;

   end Get_Info;

   -----------------------
   -- Already_Installed --
   -----------------------

   overriding function Already_Installed (This : Deployer) return Boolean
   is
      Installed_Version : Unbounded_String;
      Available_Version : Unbounded_String;
   begin
      Trace.Debug ("already_installed? " & This.Base.Package_Name);

      Get_Info (Package_Name => This.Base.Package_Name,
                Available_Version => Available_Version,
                Installed_Version => Installed_Version);
      return Length (Installed_Version) > 0;
   end Already_Installed;

   ------------
   -- Detect --
   ------------

   overriding
   function Detect (This : Deployer) return Version_Outcomes.Outcome
   is

      function Get_Version_From_String (Candidate : String)
                                       return Version_Outcomes.Outcome
      is (Version_Outcomes.New_Result (Semantic_Versioning.Parse
                                         (Candidate,
                                          Relaxed => True)));

      Installed_Version : Unbounded_String;
      Available_Version : Unbounded_String;
   begin
      Trace.Debug ("detect? " & This.Base.Package_Name);

      Get_Info (Package_Name => This.Base.Package_Name,
                Available_Version => Available_Version,
                Installed_Version => Installed_Version);

      if Length (Installed_Version) > 0 then
         return Get_Version_From_String (To_String (Installed_Version));
      elsif Length (Available_Version) > 0 then
         return Get_Version_From_String (To_String (Available_Version));
      else
         return Version_Outcomes.Outcome_Failure
           ("no candidate version found",
            Report => False);
      end if;

   end Detect;

   -------------
   -- Install --
   -------------

   overriding
   function Install (This : Deployer) return Outcome is
   begin
      Trace.Debug ("hoping to install: " & This.Base.Image);
      Subprocess.Checked_Spawn
        ("brew",
         Empty_Vector & "install" & This.Base.Package_Name);

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Install;

end Alire.Origins.Deployers.System.Homebrew;
