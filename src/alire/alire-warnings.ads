package Alire.Warnings with Preelaborate is

   --  TODO: thread-unsafe

   procedure Warn_Once (Text : String;
                        ID   : String := "";
                        Level : Trace.Levels := Trace.Warning);
   --  Emit a warning just once. If it has been already seen, do not warn
   --  again. ID is used to determine if a warning has already been emitted
   --  or, when not given, the actual warning text.

   function Already_Warned (ID : String) return Boolean;
   --  Says if a warning has been already emitted in the current run

end Alire.Warnings;
