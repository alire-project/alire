with Alire.Utils;

package body Alire.Warnings is

   Already_Emitted : Utils.String_Set;

   ---------------
   -- Warn_Once --
   ---------------

   procedure Warn_Once (Text : String;
                        ID   : String := "";
                        Level : Trace.Levels := Trace.Warning)
   is
   begin
      if ID = "" then
         Warn_Once (Text, Text, Level);
      elsif not Already_Emitted.Contains (ID) then
         Already_Emitted.Include (ID);
         Trace.Log (Text, Level);
      end if;
   end Warn_Once;

end Alire.Warnings;
