with Alire.Features.Index;
with Alire.Hashes;

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
      else
         Trace.Error ("No publishing subcommand given.");
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
      Define_Switch (Config,
                     Cmd.Hash'Access,
                     "", "--hash",
                     "Compute hash of given origin");
   end Setup_Switches;

end Alr.Commands.Publish;
