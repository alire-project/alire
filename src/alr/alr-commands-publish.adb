with Alire.Features.Index;
with Alire.Hashes;
with Alire.Publish;

package body Alr.Commands.Publish is

   procedure Hash;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd : in out Command) is
   begin
      if Cmd.Hash then
         Hash;
         return;

      elsif Cmd.Prepare then
         if Num_Arguments /= 1 then
            Reportaise_Wrong_Arguments ("Origin URL argument required.");
         else
            Alire.Publish.Verify_And_Create_Index_Manifest
              (Origin => Argument (1),
               Commit => Cmd.Commit.all);
         end if;

      else
         Trace.Warning
           ("No publishing subcommand given, defaulting to "
            & Switch_Prepare & " ...");
         Cmd.Prepare := True;
         Execute (Cmd);
      end if;
   end Execute;

   ----------
   -- Hash --
   ----------

   procedure Hash is
   begin
      if Num_Arguments /= 1 then
         Reportaise_Wrong_Arguments ("hash subcommand expects one argument");
      end if;

      Trace.Info
        (String
           (Alire.Features.Index.Hash_Origin
                (Alire.Hashes.Default,
                 Argument (1)).Value.Ptr.all));
   end Hash;

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
         Cmd.Commit'Access,
         "", "--commit=",
         "Revision in a remote repository denoting the sources of a release",
         Argument => "ID");

      Define_Switch (Config,
                     Cmd.Hash'Access,
                     "", "--hash",
                     "Compute hash of given origin");

      Define_Switch
        (Config,
         Cmd.Hash'Access,
         "", Switch_Prepare,
         "Start the publishing assistant using a ready remote origin");
   end Setup_Switches;

end Alr.Commands.Publish;
