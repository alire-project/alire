with Ada.Strings.Fixed;

with Alire.Index;
with Alire.Releases;

with Alr.Utils;

with Semantic_Versioning;

package body Alr.Commands.Search is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      use Ada.Strings.Fixed;

      Found   : Natural := 0;

      ------------------
      -- List_Release --
      ------------------

      procedure List_Release (R : Alire.Releases.Release) is
      begin
         Trace.Always (R.Project & ASCII.HT & Semantic_Versioning.Image (R.Version) & ASCII.HT & R.Description);
      end List_Release;

   begin
      Requires_No_Bootstrap;

      if Num_Arguments = 0 and then not Cmd.List then -- no search term, nor --list
         Trace.Error ("Please provide a search term, or use --list to show all available releases");
         raise Wrong_Command_Arguments;
      end if;

      if Cmd.List and then Num_Arguments /= 0 then
         Trace.Error ("Listing is incompatible with searching");
         raise Wrong_Command_Arguments;
      end if;

      --  End of option verification, start of search

      if Cmd.List then
         Found := Natural (Alire.Index.Releases.Length);
         for R of Alire.Index.Releases loop
            List_Release (R);
         end loop;
      else
         declare
            Pattern : constant String := Argument (1);
         begin
            if not Cmd.List then
               Log ("Searching " & Utils.Quote (Pattern) & "...", Detail);
            end if;

            for R of Alire.Index.Releases loop
               if Count (R.Project, Pattern) > 0 then
                  Found := Found + 1;
                  List_Release (R);
               end if;
            end loop;
         end;
      end if;

      if Found = 0 then
         Log ("Search term not found");
      end if;
   end Execute;

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin

      Define_Switch (Config,
                     Cmd.List'Access,
                     "", "--list",
                     "List all available releases");
   end Setup_Switches;

end Alr.Commands.Search;
