with Ada.Text_IO;

with Hello_World_Config;

procedure Hello_World is
begin
   Ada.Text_IO.Put_Line (Hello_World_Config.Crate_Version);
end Hello_World;
