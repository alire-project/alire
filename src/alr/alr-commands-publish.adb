with Alire.Publish;

package body Alr.Commands.Publish is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd : in out Command) is
   begin
      if Cmd.Prepare then
         if Num_Arguments < 1 then
            Reportaise_Wrong_Arguments ("Origin URL argument required.");
         elsif Num_Arguments > 2 then
            Reportaise_Wrong_Arguments
              ("Unknown extra arguments, only a mandatory URL"
               & " and optional revision are expected");
         else
            Alire.Publish.Verify_And_Create_Index_Manifest
              (Origin => Argument (1),
               Commit => (if Num_Arguments = 2
                          then Argument (2)
                          else ""));
         end if;

      elsif Cmd.Print_Trusted then
         Alire.Publish.Print_Trusted_Sites;

      else
         Trace.Warning
           ("No publishing subcommand given, defaulting to "
            & Switch_Prepare & " ...");
         Cmd.Prepare := True;
         Execute (Cmd);
      end if;
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch
        (Config,
         Cmd.Print_Trusted'Access,
         "", "--trusted-sites",
         "Print a list of trusted git repository sites");

      Define_Switch
        (Config,
         Cmd.Prepare'Access,
         "", Switch_Prepare,
         "Start the publishing assistant using a ready remote origin");
   end Setup_Switches;

end Alr.Commands.Publish;
