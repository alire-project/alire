with Ada.Containers.Indefinite_Holders;

package body Alire.Platform is

   package Holders is new Ada.Containers.Indefinite_Holders (Supported_Platform'Class);

   This : Holders.Holder;


   type Unsupported_Platform is new Supported_Platform with null record;

   overriding function Package_Version (P : Unsupported_Platform; Origin : Origins.Origin) return String is ("");

   -------------
   -- Current --
   -------------

   function Current return Supported_Platform'Class is
     (if This.Is_Empty
      then Unsupported_Platform'(Supported_Platform with null record)
      else This.Element);

   ------------------
   -- Set_Platform --
   ------------------

   procedure Set (P : Supported_Platform'Class) is
   begin
      This.Replace_Element (P);
   end Set;

end Alire.Platform;
