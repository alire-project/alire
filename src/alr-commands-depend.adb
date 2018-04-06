with Alire.Releases;

package body Alr.Commands.Depend is

   --------------------------
   -- Display_Help_Details --
   --------------------------

   overriding procedure Display_Help_Details (Cmd : Command) is
   begin
      New_Line;
      Print_Project_Version_Sets;
   end Display_Help_Details;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      Requires_Project;

      if Num_Arguments < 1 then
         Reportaise_Wrong_Arguments ("Specify add/del subcommand and dependency");
      elsif Num_Arguments < 2 then
         Reportaise_Wrong_Arguments ("No project dependency given");
      end if;

      for I in 2 .. Num_Arguments loop
         if not Query.Exists (Argument (I)) then
            Reportaise_Command_Failed ("The requested dependency was not found in the catalog: " & Argument (I));
         end if;

         declare
            Requested : constant Alire.Releases.Release := Query.Find (Argument (I), Query_Policy);
         begin
            INCLUDE THE RELEASE (RELEASE_MAP.INCLUDE) -- NO HACÍA FALTA, LELO!!! VIENE POR DEFECTO
         end;
      end loop;
   end Execute;

end Alr.Commands.Depend;
