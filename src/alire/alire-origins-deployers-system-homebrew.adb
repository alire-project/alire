with AAA.Strings; use AAA.Strings;

with Alire.OS_Lib.Subprocess;
with Alire.Errors;

with GNATCOLL.JSON;

package body Alire.Origins.Deployers.System.Homebrew is

   --  Ada.Strings.Unbounded is use-visible via Alire.Origins.
   use GNATCOLL.JSON;

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
         Data : constant JSON_Value := Read (AAA.Strings.Flatten (Info));

         procedure Get_Available_Version
           (From    :     JSON_Value;
            Version : out Ada.Strings.Unbounded.Unbounded_String)
         with Pre => Kind (From) = JSON_Object_Type;
         procedure Get_Installed_Version
           (From    :     JSON_Array;
            Version : out Ada.Strings.Unbounded.Unbounded_String);
         procedure Info_Callback (Name : UTF8_String; Value : JSON_Value);
         --  Called for the elements of the JSON data to find the
         --  relevant sections and use Get_Available_Version,
         --  Get_Installed_Version to extract the actual versions (if
         --  any).

         procedure Get_Available_Version
           (From    :     JSON_Value;
            Version : out Ada.Strings.Unbounded.Unbounded_String)
         is
         begin
            Version := Ada.Strings.Unbounded.To_Unbounded_String
              (String'(Get (From, "stable")));
         end Get_Available_Version;

         procedure Get_Installed_Version
           (From    :     GNATCOLL.JSON.JSON_Array;
            Version : out Ada.Strings.Unbounded.Unbounded_String)
         is
            Result : Ada.Strings.Unbounded.Unbounded_String;
            procedure Installed_Callback (Name : UTF8_String;
                                          Value : JSON_Value);
            procedure Installed_Callback (Name : UTF8_String;
                                          Value : JSON_Value)
            is
            begin
               if Name = "version" then
                  Result := Ada.Strings.Unbounded.To_Unbounded_String
                    (String'(Get (Value)));
               end if;
            end Installed_Callback;
         begin
            if Length (From) /= 0 then
               Map_JSON_Object (Get (From, 1), Installed_Callback'Access);
            end if;
            Version := Result;
         end Get_Installed_Version;

         procedure Info_Callback (Name : UTF8_String; Value : JSON_Value) is
            function "+"
              (L : Ada.Strings.Unbounded.Unbounded_String) return String
              renames Ada.Strings.Unbounded.To_String;
         begin
            if Name = "versions" then
               if Kind (Value) /= JSON_Object_Type then
                  raise JSON_Issue with "JSON 'versions' not JSON_Object";
               end if;
               Get_Available_Version (Value, Available_Version);
               Trace.Debug ("available: " & (+Available_Version));
            elsif Name = "installed" then
               if Kind (Value) /= JSON_Array_Type then
                  raise JSON_Issue with "JSON 'installed' not JSON_Array";
               end if;
               Get_Installed_Version (JSON_Array'(Get (Value)),
                                      Installed_Version);
               Trace.Debug ("installed: " & (+Installed_Version));
            end if;
         end Info_Callback;

         Arr : JSON_Array;
      begin
         if Kind (Data) /= JSON_Array_Type then
            raise JSON_Issue with "JSON info not JSON_Array";
         end if;
         Arr := Get (Data);
         if Length (Arr) /= 1 then
            raise JSON_Issue with "JSON info length /= 1";
         end if;
         if Kind (Get (Arr, 1)) /= JSON_Object_Type then
            raise JSON_Issue with "JSON info (1) not JSON_Object";
         end if;
         Map_JSON_Object (Get (Arr, 1), Info_Callback'Access);
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
