with Alire.Utils;

package body Alire.Warnings is

   Already_Emitted : Utils.String_Set;

   ---------------
   -- Warn_Once --
   ---------------

   procedure Warn_Once (Text  : String;
                        Id    : Warning_Id := "";
                        Level : Trace.Levels := Trace.Warning)
   is
   begin
      if Id = "" then
         Warn_Once (Text, Warning_Id (Text), Level);
      elsif not Already_Emitted.Contains (String (Id)) then
         Already_Emitted.Include (String (Id));
         Trace.Log (Text, Level);
      end if;
   end Warn_Once;

   --------------------
   -- Already_Warned --
   --------------------

   function Already_Warned (Id : Warning_Id) return Boolean
   is (Already_Emitted.Contains (String (Id)));

end Alire.Warnings;
