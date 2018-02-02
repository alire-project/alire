with Ada.Command_Line;
with Ada.Strings.Fixed;

with Alire.Index;

with Alr.Utils;

with Semantic_Versioning;

package body Alr.Commands.Search is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      use Ada.Command_Line;
      use Ada.Strings.Fixed;

      Pattern : constant String := Last_Argument;
      Found   : Natural := 0;
   begin
      if Argument_Count = 1 then -- no search term
         Log ("Please provide a search term, or use --list to show all available releases");
         raise Wrong_Command_Arguments;
      end if;

      if Cmd.List and then Pattern /= "--list" then
         Log ("Listing is incompatible with searching");
         raise Wrong_Command_Arguments;
      end if;

      if not Cmd.List and then Pattern = "" then
         Cmd.List := True;
      end if;

      --  End of option verification, start of search

      if not Cmd.List then
         Log ("Searching " & Utils.Quote (Pattern) & "...");
      end if;

      for R of Alire.Index.Releases loop
         if Cmd.List or else Count (R.Project, Pattern) > 0 then
            Found := Found + 1;
            Log (R.Project & " " & Semantic_Versioning.Image (R.Version));
         end if;
      end loop;

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
