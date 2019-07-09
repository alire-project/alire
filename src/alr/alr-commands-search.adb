with AAA.Table_IO;

with Ada.Strings.Fixed;

with Alire.Containers;
with Alire.Index;
with Alire.Origins.Deployers;
with Alire.Projects;
with Alire.Releases;

with Alr.Platform;
with Alr.Query;
with Alr.Utils;

with Semantic_Versioning;

package body Alr.Commands.Search is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      use Ada.Strings.Fixed;

      Found   : Natural := 0;

      Tab : AAA.Table_IO.Table;

      ------------------
      -- List_Release --
      ------------------

      procedure List_Release (R : Alire.Releases.Release) is
      begin
         if (Cmd.Prop.all = ""
             or else
             R.Property_Contains (Cmd.Prop.all)
             or else
             Utils.Contains (R.Notes, Cmd.Prop.all)
             or else
             Utils.Contains (Alire.Projects.Descriptions (R.Project),
                             Cmd.Prop.all))
           and then
             (Cmd.Native or else not R.Origin.Is_Native)
         then
            Found := Found + 1;
            Tab.New_Row;
            Tab.Append (+R.Project);
            Tab.Append ((if R.Origin.Is_Native then "N" else " ") &
                        (if Query.Is_Available (R) then " " else "U") &
                        (if Query.Is_Resolvable
                             (R.Depends (Platform.Properties))
                           then " "
                           else "X"));
            Tab.Append (Semantic_Versioning.Image
                        (R.Version) &
                        (if R.Origin.Is_Native
                           and then
                           Alire.Origins.Deployers.New_Deployer
                             (R.Origin).Native_Version /= ""
                           then "+" & Alire.Origins.Deployers.New_Deployer
                             (R.Origin).Native_Version
                           else ""));
            Tab.Append (Alire.Projects.Descriptions (R.Project));
            Tab.Append (R.Notes);
         end if;
      end List_Release;

      use Alire.Containers.Release_Sets;
   begin
      if Num_Arguments = 0
        and then
         not Cmd.List
        and then
         Cmd.Prop.all = ""
      then
         --  no search term, nor --list, nor --prop
         Trace.Error ("Please provide a search term, --property, or use" &
                        " --list to show all available releases");
         raise Wrong_Command_Arguments;
      end if;

      if Num_Arguments = 0 and then Cmd.Prop.all /= "" then
         Cmd.List := True;
      end if;

      if Cmd.List and then Num_Arguments /= 0 then
         Trace.Error ("Listing is incompatible with searching");
         raise Wrong_Command_Arguments;
      end if;

      --  End of option verification, start of search. First load the index,
      --  required to look at its entries.

      Requires_Full_Index;

      Tab.Append ("NAME");
      Tab.Append ("STATUS");
      Tab.Append ("VERSION");
      Tab.Append ("DESCRIPTION");
      Tab.Append ("NOTES");

      declare
         Busy : Utils.Busy_Prompt := Utils.Busy_Activity ("Searching...");
      begin
         if Cmd.List then
            Trace.Detail ("Searching...");
            for I in Alire.Index.Catalog.Iterate loop
               if Cmd.Full
                 or else
                  I = Alire.Index.Catalog.Last
                 or else
                  Alire.Index.Catalog (I).Project /= Alire.Index.Catalog
                                                       (Next (I)).Project
               then
                  List_Release (Alire.Index.Catalog (I));
                  Busy.Step;
               end if;
            end loop;
         else
            declare
               Pattern : constant String := Argument (1);
            begin
               Trace.Detail ("Searching " & Utils.Quote (Pattern) & "...");

               for I in Alire.Index.Catalog.Iterate loop
                  if Count (+Alire.Index.Catalog (I).Project, Pattern) > 0 then
                     if Cmd.Full or else I = Alire.Index.Catalog.Last or else
                       Alire.Index.Catalog (I).Project /= Alire.Index.Catalog
                                                            (Next (I)).Project
                     then
                        List_Release (Alire.Index.Catalog (I));
                     end if;
                  end if;
                  Busy.Step;
               end loop;
            end;
         end if;
      end;

      if Found = 0 then
         Log ("No hits              ");
      else
         Tab.Print (Separator => "  ");
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Searches the given substring in crate names (or properties"
               & " with --property), and shows the most recent release"
               & " of matching crates (unless --full is specified).")
      .New_Line
      .Append ("Besides version, description and release notes, a status"
               & " column with the following status flags is provided:")
      .New_Line
      .Append ("N: the release is native and provided by a system package.")
      .Append ("U: the release is not available in the current platform.")
      .Append ("X: the release has dependencies that cannot be resolved.")
      .New_Line
      .Append ("The reasons for unavalaibility (U) can be ascertained with"
               & " 'alr show <crate>=<version>'.")
      .New_Line
      .Append ("Unresolvable releases (X) should not happen in platforms"
               & " with assigned maintainers. Common reasons are missing"
               & " native dependencies that have been phased out by the"
               & " platform without being updated yet in the community"
               & " index.")
     );

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Full'Access,
                     "", "--full",
                     "Show all versions of a project (newest only otherwise)");

      Define_Switch (Config,
                     Cmd.List'Access,
                     "", "--list",
                     "List all available releases");

      Define_Switch (Config,
                     Cmd.Native'Access,
                     "", "--native",
                     "Include platform-provided native packages in search");

      Define_Switch (Config,
                     Cmd.Prop'Access,
                     "", "--property=",
                     "Search TEXT in property values",
                     Argument => "TEXT");
   end Setup_Switches;

end Alr.Commands.Search;
