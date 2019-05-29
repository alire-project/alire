with Ada.Finalization;

package Alr.Utils.Temp_File is

   --  Scopes a unique name that could be used for a file.
   --  At finalization will delete the file, if it exists

   type File (<>) is new Ada.Finalization.Limited_Controlled with private;

   function New_File return File;

   function Name (This : File) return String;

private

   type File (Len : Natural) is new Ada.Finalization.Limited_Controlled
     with record
      Name : String (1 .. Len);
   end record;

   overriding procedure Finalize (This : in out File);

   function Name (This : File) return String is (This.Name);

end Alr.Utils.Temp_File;
