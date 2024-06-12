with AAA.Table_IO;

with Alire.Settings.Edit;
with Alire.Index;
with Alire.Index_On_Disk.Loading;
with Alire.Index_On_Disk.Updates;
with Alire.Utils;

package body Alr.Commands.Index is

   package Index_Load    renames Alire.Index_On_Disk.Loading;
   package Index_Updates renames Alire.Index_On_Disk.Updates;

   --  Forward declarations

   procedure Add (Cmd : Command);

   procedure Check (Cmd : in out Command);

   procedure List;

   procedure Reset_Community;

   ---------
   -- Add --
   ---------

   procedure Add (Cmd : Command) is
      Before : constant String := Cmd.Bfr.all;

      Result : constant Alire.Outcome :=
                 Alire.Index_On_Disk.Loading.Add
                   (Origin => Cmd.Add.all,
                    Name   => Cmd.Name.all,
                    Under  => Alire.Settings.Edit.Indexes_Directory,
                    Before => Before);
   begin
      Trace.Debug ("Index before ID = " & Before);

      if not Result.Success then
         Reportaise_Command_Failed (Alire.Message (Result));
      end if;
   end Add;

   ------------
   -- Delete --
   ------------

   procedure Delete (Name : String) is
      Result  : Alire.Outcome;
      Indexes : constant Index_Load.Set :=
                  Index_Load.Find_All
                    (Alire.Settings.Edit.Indexes_Directory, Result);
      Found   : Boolean := False;
   begin
      if not Result.Success then
         Reportaise_Command_Failed (Alire.Message (Result));
      end if;

      --  Find matching index and delete
      for Index of Indexes loop
         if Index.Name = Name then
            Found := True;
            declare
               Result : constant Alire.Outcome := Index.Delete;
            begin
               if Result.Success then
                  exit;
               else
                  Reportaise_Command_Failed (Alire.Message (Result));
               end if;
            end;
         end if;
      end loop;

      if not Found then
         Reportaise_Command_Failed ("Given index not found: " & Name);
      end if;
   end Delete;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
   begin
      if Args.Count /= 0 then
         Reportaise_Wrong_Arguments (Name (Cmd) & " doesn't take arguments");
      end if;

      --  Check no multi-action
      case Alire.Utils.Count_True
        ((Cmd.Add.all /= "",
          Cmd.Del.all /= "",
          Cmd.Check,
          Cmd.List,
          Cmd.Rset,
          Cmd.Update_All))
      is
         when 0 =>
            --  Use --list as the default
            Cmd.List := True;
         when 1 =>
            null; -- Usual case, just fall through
         when others =>
            Reportaise_Wrong_Arguments
              ("Specify exactly one index subcommand");
      end case;

      --  Dispatch to selected action
      if Cmd.Add.all /= "" then
         if Cmd.Name.all = "" then
            Reportaise_Wrong_Arguments
              ("Must provide a local name for new index");
         end if;
         Add (Cmd);
      elsif Cmd.Del.all /= "" then
         Delete (Cmd.Del.all);
      elsif Cmd.Check then
         Check (Cmd);
      elsif Cmd.List then
         List;
      elsif Cmd.Update_All then
         Update_All;
      elsif Cmd.Rset then
         Reset_Community;
      else
         Reportaise_Wrong_Arguments ("Specify an index subcommand");
      end if;
   end Execute;

   -----------
   -- Check --
   -----------

   procedure Check (Cmd : in out Command) is
   begin
      Cmd.Requires_Full_Index (Strict => True);
      Alire.Index.Check_Contents;
      Alire.Put_Success ("No issues found in index contents.");
   end Check;

   ----------
   -- List --
   ----------

   procedure List is
      use Alire;

      Result  : Alire.Outcome;
      Indexes : constant Index_Load.Set :=
                  Index_Load.Find_All
                    (Alire.Settings.Edit.Indexes_Directory, Result);

      Table : AAA.Table_IO.Table;
      Count : Natural := 0;
   begin
      if not Result.Success then
         Reportaise_Command_Failed (Alire.Message (Result));
      end if;

      Table
        .Append (TTY.Emph ("#"))
        .Append (TTY.Emph ("NAME"))
        .Append (TTY.Emph ("URL"))
        .Append (TTY.Emph ("PATH"));

      if Alire.Log_Level = Alire.Trace.Debug then
         Table.Append (TTY.Emph ("PRIORITY"));
      end if;

      for Index of Indexes loop
         Count := Count + 1;
         Table.New_Row;
         Table
           .Append (AAA.Strings.Trim (Count'Img))
           .Append (Index.Name)
           .Append (Index.Origin)
           .Append (Index.Index_Directory);

         if Alire.Log_Level = Alire.Trace.Debug then
            Table.Append (AAA.Strings.Trim (Index.Priority'Img));
         end if;
      end loop;

      if Count > 0 then
         Table.Print;
      else
         Trace.Info ("No index configured.");
      end if;
   end List;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
      .Append ("Add, remove, list and update indexes used by the current"
               & " alr configuration.")
      .New_Line
      .Append ("Updating applies only to repository-stored indexes, in which"
               & " case a pull operation will be performed on them."
               & " An index initially set up with a specific commit will"
               & " not be updated.")
      .New_Line
      .Append ("URL can be one of:")
      .Append ("- Plain absolute path: /path/to/index")
      .Append ("- Explicit path:       file://path/to/index")
      .Append ("- git over HTTP/HTTPS: git+https://github.com/org/repo")
      .Append ("- git over SSH:        git+ssh://user@host.com:/path/to/repo")
      .Append ("- git user over SSH:   git@github.com:/org/repo")
     );

   ---------------------
   -- Reset_Community --
   ---------------------

   procedure Reset_Community is
      Result : constant Alire.Outcome := Index_Load.Add_Or_Reset_Community;
   begin
      if not Result.Success then
         Reportaise_Command_Failed (Result.Message);
      end if;
   end Reset_Community;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch
        (Config      => Config,
         Output      => Cmd.Add'Access,
         Long_Switch => "--add=",
         Argument    => "URL",
         Help        => "Add an index");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Bfr'Access,
         Long_Switch => "--before=",
         Argument    => "NAME",
         Help        => "Priority order (defaults to last)");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Check'Access,
         Long_Switch => "--check",
         Help        =>
           "Runs diagnostics on index contents (unknown values, hosts, etc.)");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Del'Access,
         Long_Switch => "--del=",
         Argument    => "NAME",
         Help        => "Remove an index");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.List'Access,
         Long_Switch => "--list",
         Help        => "List configured indexes (default)");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Name'Access,
         Long_Switch => "--name=",
         Argument    => "NAME",
         Help        => "User given name for the index");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Update_All'Access,
         Long_Switch => "--update-all",
         Help        => "Update configured indexes");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Rset'Access,
         Long_Switch => "--reset-community",
         Help        => "Add the community index, or reset any local changes");
   end Setup_Switches;

   ----------------
   -- Update_All --
   ----------------

   procedure Update_All is
      Result : constant Alire.Outcome :=
                 Index_Updates.Update_All
                   (Alire.Settings.Edit.Indexes_Directory);
   begin
      if not Result.Success then
         Reportaise_Command_Failed (Alire.Message (Result));
      end if;
   end Update_All;

end Alr.Commands.Index;
