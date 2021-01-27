package Alire.Warnings with Preelaborate is

   procedure Warn_Once (Text : String;
                        ID   : String := "";
                        Level : Trace.Levels := Trace.Warning);
   --  Emit a warning just once. If it has been already seen, do not warn
   --  again. ID is used to determine if a warning has already been emitted
   --  or, when not given, the actual warning text.

end Alire.Warnings;
