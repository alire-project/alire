package Alire.Warnings with Preelaborate is

   --  TODO: thread-unsafe

   type Warning_Id is new String;

   procedure Warn_Once (Text  : String;
                        Id    : Warning_Id := "";
                        Level : Trace.Levels := Trace.Warning);
   --  Emit a warning just once. If it has been already seen, do not warn
   --  again. ID is used to determine if a warning has already been emitted
   --  or, when not given, the actual warning text serves as the ID.

   function Already_Warned (Id : Warning_Id) return Boolean;
   --  Says if a warning has been already emitted in the current run

   generic
      type Returned (<>) is private;
   function Warn_With_Result (Text   : String;
                              Result : Returned;
                              Level  : Trace.Levels := Trace.Warning)
                              return Returned;
   --  Return Result after printing Text; for use in expressions. See instances
   --  in Alire.Warnings.Typed.

   ------------------
   --  Defined Ids --
   ------------------

   Caret_Or_Tilde : constant Warning_Id := "caret or tilde";

end Alire.Warnings;
