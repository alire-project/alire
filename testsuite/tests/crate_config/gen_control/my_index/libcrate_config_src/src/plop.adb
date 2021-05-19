with Ada.Text_IO; use Ada.Text_IO;

with Libcrate_Config_Config; use Libcrate_Config_Config;

package body Plop is

   type My_Int_Type is range Var_Int_First .. Var_Int_Last;
   My_Int : constant My_Int_Type := Var_Int;

   type My_Real_Type is digits 10 range Var_Real_First .. Var_Real_Last;
   My_Real : constant My_Real_Type := Var_Real;

   procedure Print is
   begin
      Put_Line ("Ada -> Var_Bool: " & Libcrate_Config_Config.Var_Bool'Img);
      Put_Line ("Ada -> Var_String: '" & Libcrate_Config_Config.Var_String & "'");
      Put_Line ("Ada -> Var_Int: " & My_Int'Img);
      Put_Line ("Ada -> Var_Real: " & My_Real'Img);
      Put_Line ("Ada -> Var_Enum: " & Libcrate_Config_Config.Var_Enum'Img);
   end Print;

end Plop;
