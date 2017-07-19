
 with Ada.Command_Line;

package body Alire.Commands is

   Usage_Error : exception;

   -------------------
   -- Unimplemented --
   -------------------

   procedure Unimplemented is
   begin
      raise Usage_Error with "The requested action is not implemeted";
   end Unimplemented;

   type Action_Names is (Get, Version);

   type Action_Executer is access procedure;

   The_Actions : constant array (Action_Names) of Action_Executer :=
                   (Get     => Unimplemented'Access,
                    Version => Unimplemented'Access);

   -------------
   -- Execute --
   -------------

   procedure Execute is
      use Ada.Command_Line;
      Action : Action_Names;
   begin
      if Argument_Count < 1 then
         raise Usage_Error with "Insufficient arguments"; -- TODO: nicer output
      else
         begin
            Action := Action_Names'Value (Argument (1));
         exception
            when Constraint_Error =>
               raise Usage_Error with "Unrecognized option: " & Argument (1);
         end;

         The_Actions(Action).all;
      end if;
   end Execute;

end Alire.Commands;
