with Ada.Text_IO; use Ada.Text_IO;

with Libcrate_Config_Config; use Libcrate_Config_Config;

with Host_Specific;

package body Plop is

   type My_Int_Type is range Var_Int_First .. Var_Int_Last;
   My_Int : constant My_Int_Type := Var_Int;

   type My_Real_Type is digits 10 range Var_Real_First .. Var_Real_Last;
   My_Real : constant My_Real_Type := Var_Real;

   procedure Print is
      procedure Test_C_Print;
      pragma Import(C, Test_C_Print, "test_c_print");

   begin
      Put_Line ("Ada -> Crate_Version: " & Libcrate_Config_Config.Crate_Version);
      Put_Line ("Ada -> Crate_Name: " & Libcrate_Config_Config.Crate_Name);
      Put_Line ("Ada -> Alire_Host_OS: " & Libcrate_Config_Config.Alire_Host_OS);
      Put_Line ("Ada -> Alire_Host_Arch: " & Libcrate_Config_Config.Alire_Host_Arch);
      Put_Line ("Ada -> Alire_Host_Distro: " & Libcrate_Config_Config.Alire_Host_Distro);
      Put_Line ("Ada -> Var_Bool: " & Libcrate_Config_Config.Var_Bool'Img);
      Put_Line ("Ada -> Var_String: '" & Libcrate_Config_Config.Var_String & "'");
      Put_Line ("Ada -> Var_Int'First: " & My_Int_Type'First'Img);
      Put_Line ("Ada -> Var_Int'Last: " & My_Int_Type'Last'Img);
      Put_Line ("Ada -> Var_Int: " & My_Int'Img);
      Put_Line ("Ada -> Var_Real'First: " & My_Real_Type'First'Img);
      Put_Line ("Ada -> Var_Real'Last: " & My_Real_Type'Last'Img);
      Put_Line ("Ada -> Var_Real: " & My_Real'Img);
      Put_Line ("Ada -> Var_Enum_Kind'First: " & Libcrate_Config_Config.Var_Enum_Kind'First'Img);
      Put_Line ("Ada -> Var_Enum_Kind'Last: " & Libcrate_Config_Config.Var_Enum_Kind'Last'Img);
      Put_Line ("Ada -> Var_Enum: " & Libcrate_Config_Config.Var_Enum'Img);

      Put_Line ("Host_Specific -> " & Host_Specific);

      Test_C_Print;
   end Print;

end Plop;
