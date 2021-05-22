with TOML;

with Alire.Releases;
with Alire.Properties.Configurations;
limited with Alire.Roots;

private with Ada.Strings.Unbounded;
private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;
private with Ada.Containers.Indefinite_Holders;
private with Alire.Utils;

package Alire.Crate_Configuration is

   type Global_Config is tagged limited private;

   procedure Load (This : in out Global_Config;
                   Root : in out Alire.Roots.Root);

   procedure Generate_Config_Files (This : Global_Config;
                                    Root : in out Alire.Roots.Root);

private

   use Alire.Properties.Configurations;

   package Config_Type_Definition_Holder
   is new Ada.Containers.Indefinite_Holders (Config_Type_Definition);

   type Config_Setting is record
      Type_Def  : Config_Type_Definition_Holder.Holder;
      Value     : TOML.TOML_Value;
      Set_By    : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Config_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Config_Setting,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

   type Global_Config is tagged limited record
      Map : Config_Maps.Map;
   end record;

   procedure Use_Default_Values (Conf : in out Global_Config);
   --  Use default value for unset variable, raise Checked_Error if a variable
   --  has no default value.

   procedure Load_Definitions (This  : in out Global_Config;
                               Root  : in out Roots.Root;
                               Crate : Crate_Name);

   procedure Load_Settings (This  : in out Global_Config;
                            Root  : in out Roots.Root;
                            Crate : Crate_Name);

   procedure Generate_Ada_Config (This     : Global_Config;
                                  Crate    : Crate_Name;
                                  Filepath : Absolute_Path);

   procedure Generate_GPR_Config (This     : Global_Config;
                                  Crate    : Crate_Name;
                                  Filepath : Absolute_Path;
                                  Withs    : Alire.Utils.String_Set);

   procedure Generate_C_Config (This     : Global_Config;
                                Crate    : Crate_Name;
                                Filepath : Absolute_Path);

end Alire.Crate_Configuration;
