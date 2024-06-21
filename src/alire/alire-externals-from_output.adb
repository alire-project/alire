with AAA.Strings; use AAA.Strings;

with Alire.Directories;
with Alire.Index;
with Alire.Origins;
with Alire.OS_Lib.Subprocess;
with Alire.Paths;
with Alire.Releases;
with Alire.TOML_Keys;
with Alire.Utils.Regex;
with Alire.Utils.TTY;

with Semantic_Versioning;

with TOML;

package body Alire.Externals.From_Output is

   ------------
   -- Detect --
   ------------

   overriding
   function Detect (This : External;
                    Name : Crate_Name) return Releases.Containers.Release_Set
   is
      use Directories.Operators;
      Location : GNAT.OS_Lib.String_Access :=
                   GNAT.OS_Lib.Locate_Exec_On_Path
                     (This.Command.First_Element);
      Result : Alire.Releases.Containers.Release_Set;
   begin
      if Location in null then
         Trace.Debug
           ("External not detected because executable is not in PATH: "
            & This.Command.First_Element);
         return (Releases.Containers.Release_Sets.Empty_Set with null record);
      elsif Contains (Location.all,
                      Paths.Working_Folder_Inside_Root
                      / Paths.Cache_Folder_Inside_Working_Folder
                      / Paths.Deps_Folder_Inside_Cache_Folder)
      then
         Trace.Debug
           ("External skipped because executable is deployed by Alire: "
            & Location.all);
         return (Releases.Containers.Release_Sets.Empty_Set with null record);
      else
         GNAT.OS_Lib.Free (Location);
      end if;

      declare
         Lines   : AAA.Strings.Vector;
         Status  : constant Integer :=
                     OS_Lib.Subprocess.Unchecked_Spawn_And_Capture
                       (This.Command.First_Element,
                        This.Command.Tail,
                        Lines);
         Output  : constant String := Lines.Flatten ("" & ASCII.LF);
         --  ASCII.LF is used by Regpat for new lines
      begin
         if Status /= 0 then
            Trace.Debug
              ("External command [" & This.Command.First_Element
               & "] erred with code: " & AAA.Strings.Trim (Status'Image));
            return Releases.Containers.Empty_Release_Set;
         end if;

         Trace.Debug
           ("Looking for external in version string '" & Output & "'");

         declare
            Version : constant String :=
                      Utils.Regex.First_Match (+This.Regstr, Output);
         begin
            if Version /= "" then
               declare
                  Path    : constant Any_Path :=
                              OS_Lib.Subprocess.Locate_In_Path
                                (This.Command.First_Element);
               begin
                  Trace.Debug ("Identified external from version: "
                               & Version);

                  Result.Insert
                    (Index.Crate (Name, Index.Query_Mem_Only).Base
                     .Retagging (Semantic_Versioning.Parse
                       (Version, Relaxed => True))
                     .Providing (This.Provides)
                     .Replacing (Origins.New_External ("path " & Path))
                     .Replacing (Notes => "Detected at " -- length is 12
                                 & Shorten
                                   (String (Path),
                                    Max_Description_Length - 12)));
               end;
            else
               Trace.Debug
                 ("Failed to match a version using regexp '"
                  & (+This.Regstr) & "' on output: " & Version);
            end if;
         end;
      end;

      return Result;
   exception
      when E : others =>
         Trace.Debug ("Unexpected exception while attempting detection of "
                      & "external crate " & Utils.TTY.Name (Name));
         Log_Exception (E);
         return Result;
   end Detect;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue) return External is
      Regexp : constant String :=
                 From.Checked_Pop (TOML_Keys.Version_Regexp,
                                   TOML.TOML_String).As_String;
      use GNAT.Regpat;
   begin
      From.Assert_Key (TOML_Keys.Version_Cmd, TOML.TOML_Array);
      if From.Unwrap.Get (TOML_Keys.Version_Cmd).Length = 0 then
         From.Checked_Error ("version command cannot be empty");
      end if;

      return This : External do
         This.Regstr  := +Regexp;
         This.Command :=
           TOML_Adapters.To_Vector
             (From.Checked_Pop (TOML_Keys.Version_Cmd, TOML.TOML_Array));

         Compile (This.Regexp, Regexp, Single_Line + Multiple_Lines);

         Trace.Debug ("Loaded external with regexp: " & Regexp);
      exception
         when E : GNAT.Regpat.Expression_Error =>
            Log_Exception (E);
            From.Checked_Error ("invalid regular expression: " & Regexp);
      end return;
   end From_TOML;

   -----------
   -- Image --
   -----------

   overriding
   function Image (This : External) return String is
     (This.Command.Flatten);

   ------------
   -- Detail --
   ------------

   overriding
   function Detail (This          : External;
                    Unused_Distro : Platforms.Distributions)
                    return AAA.Strings.Vector is
      (AAA.Strings.Empty_Vector.Append (+This.Regstr));

end Alire.Externals.From_Output;
