with TOML;

with Alire.Utils.Switches;
with Alire.Properties.Configurations;
limited with Alire.Roots;

private with Ada.Strings.Unbounded;
private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;
private with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Ordered_Maps;

package Alire.Crate_Configuration is

   --  Types used to store build profiles/switches and declared variables

   subtype Profile_Kind is Utils.Switches.Profile_Kind;

   use all type Profile_Kind;

   package Profile_Maps
   is new Ada.Containers.Indefinite_Ordered_Maps (Crate_Name, Profile_Kind);

   Default_Root_Build_Profile : constant Utils.Switches.Profile_Kind :=
     Utils.Switches.Development;

   Default_Deps_Build_Profile : constant Utils.Switches.Profile_Kind :=
     Utils.Switches.Release;

   type Global_Config is tagged private;

   function Is_Valid (This : Global_Config) return Boolean;
   --  False until Load is called

   procedure Ensure_Complete (This : Global_Config);
   --  Verify all variables have a value, or report and raise

   function Build_Profile (This  : Global_Config;
                           Crate : Crate_Name)
                           return Utils.Switches.Profile_Kind
     with Pre => This.Is_Valid;

   function Is_Default_Profile (This  : Global_Config;
                                Crate : Crate_Name)
                                return Boolean;
   --  Say if the current profile for the crate is a default one or not

   procedure Set_Build_Profile (This    : in out Global_Config;
                                Crate   : Crate_Name;
                                Profile : Profile_Kind)
     with Pre => This.Is_Valid;

   procedure Load (This : in out Global_Config;
                   Root : in out Alire.Roots.Root);

   procedure Generate_Config_Files (This : Global_Config;
                                    Root : in out Alire.Roots.Root);

   procedure Save_Last_Build_Profiles (This : Global_Config);
   --  Record in local user configuration the last profiles used in crate
   --  configuration (the ones currently in the given configuration).

   function Last_Build_Profiles return Profile_Maps.Map;
   --  Get the last profile used from user configuration. Note that we can have
   --  more/fewer crates in a new run if dependencies have changed.

   function Must_Regenerate (This : Global_Config) return Boolean;
   --  Say if some profile has changed so config files must be regenerated

   type Profile_Wildcards is (To_None,    --  No wildcard given
                              To_Unset, --  '%' (not set otherwise)
                              To_All);  --  '*' (total override)

   type Parsed_Profiles is record
      Profiles : Profile_Maps.Map;

      Default_Apply   : Profile_Wildcards := To_None;
      Default_Profile : Profile_Kind      := Development;
   end record;

   function Parse_Profiles (Img              : String;
                            Accept_Wildcards : Boolean) return Parsed_Profiles;
   --  Convert a string "crate1=profile1,crate2=profile2,[*|%=profile3]" to
   --  its proper type. Only one of the wildcards may appear, and only when
   --  Accept_Wildcards (since we shouldn't see them in our internally stored
   --  last profiles.

private

   use Alire.Properties.Configurations;
   use type Alire.Utils.Switches.Switch_List;

   package Config_Type_Definition_Holder
   is new Ada.Containers.Indefinite_Holders (Config_Type_Definition);

   type Setters is (Default,  -- Set by alire to a default value
                    Manifest, -- Set by a crate manifest
                    User);    -- Set by the alire user through API

   type Config_Setting is record
      Type_Def  : Config_Type_Definition_Holder.Holder;
      Value     : TOML.TOML_Value;
      Set_By    : Ada.Strings.Unbounded.Unbounded_String;
      --  Free-form text as this can be any crate name and other things
   end record;

   package Config_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Config_Setting,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

   function Name (C : Config_Maps.Cursor) return Crate_Name;

   package Profile_Setter_Maps
   is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name, Setters);

   package Switches_Maps
   is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name, Alire.Utils.Switches.Switch_List);

   type Global_Config is tagged record
      Var_Map : Config_Maps.Map;
      --  Mapping "crate.var" --> setting

      Profile_Map  : Profile_Maps.Map;
      --  Mapping crate -> profile, exists for all crates in solution

      Setter_Map   : Profile_Setter_Maps.Map;
      --  Mapping crate -> setter, exists for all crates in solution

      Switches_Map : Switches_Maps.Map;
   end record;

   procedure Use_Default_Values (Conf : in out Global_Config);
   --  Use default value for unset variable, raise Checked_Error if a variable
   --  has no default value.

   procedure Add_Definition (This     : in out Global_Config;
                             Crate    : Crate_Name;
                             Type_Def : Config_Type_Definition);

   procedure Load_Definitions (This  : in out Global_Config;
                               Root  : in out Roots.Root;
                               Crate : Crate_Name);

   procedure Set_Value (This  : in out Global_Config;
                        Crate : Crate_Name;
                        Val   : Assignment);

   procedure Load_Settings (This  : in out Global_Config;
                            Root  : in out Roots.Root;
                            Crate : Crate_Name);

   procedure Generate_Ada_Config (This     : Global_Config;
                                  Crate    : Crate_Name;
                                  Filepath : Absolute_Path;
                                  Version  : String);

   procedure Generate_GPR_Config (This        : Global_Config;
                                  Crate       : Crate_Name;
                                  Filepath    : Absolute_Path;
                                  Withs       : AAA.Strings.Set;
                                  Version     : String);

   procedure Generate_C_Config (This     : Global_Config;
                                Crate    : Crate_Name;
                                Filepath : Absolute_Path;
                                Version  : String);

end Alire.Crate_Configuration;
