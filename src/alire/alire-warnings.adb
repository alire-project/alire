package body Alire.Warnings is

   Already_Emitted : AAA.Strings.Set;

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

   ----------------------
   -- Warn_With_Result --
   ----------------------

   function Warn_With_Result (Text   : String;
                  Result : Returned;
                  Level  : Trace.Levels := Trace.Warning)
                  return Returned
   is
   begin
      Trace.Log (Text, Level);
      return Result;
   end Warn_With_Result;

end Alire.Warnings;
