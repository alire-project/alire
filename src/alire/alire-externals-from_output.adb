with AAA.Enum_Tools;
with AAA.Strings; use AAA.Strings;

with Alire.Directories;
with Alire.Index;
with Alire.Origins;
with Alire.OS_Lib.Subprocess;
with Alire.Paths;
with Alire.Releases;
with Alire.Settings.Builtins;
with Alire.Settings.Version_Output;
with Alire.TOML_Keys;
with Alire.Utils.Regex;
with Alire.Utils.TTY;
with Alire.Warnings;

with Semantic_Versioning;

with TOML;

package body Alire.Externals.From_Output is

   ------------
   -- Policy --
   ------------

   function Policy return Settings.Version_Output.Policies is
      use Settings.Version_Output;
      function Is_Valid is new AAA.Enum_Tools.Is_Valid (Policies);
      Text : constant String :=
               Settings.Builtins.Externals_Version_Output_Policy.Get;
   begin
      if Is_Valid (Prefix & Text) then
         return Policies'Value (Prefix & Text);
      end if;

      Warnings.Warn_Once
        ("Unknown value '" & Text & "' for setting "
         & "externals.version_output.policy, using '"
         & Image (Run_Allowlist) & "' instead");
      return Run_Allowlist;
   end Policy;
   --  The configured policy. Unknown values, which can only come from editing
   --  the settings file by hand, are taken as Run_Allowlist with a warning.

   -------------
   -- May_Run --
   -------------

   function May_Run (Command : AAA.Strings.Vector;
                     Name    : Crate_Name)
                     return Boolean
   is
      use Settings.Version_Output;
   begin
      case Policy is
         when Run_All =>
            return True;

         when Run_None =>
            Trace.Debug ("Not running version command [" & Command.Flatten
                         & "] of external " & Utils.TTY.Name (Name)
                         & " as externals.version_output.policy is '"
                         & Image (Run_None) & "'");
            return False;

         when Run_Allowlist =>
            if Is_Allowed
              (Command,
               Settings.Builtins.Externals_Version_Output_Allowlist.Get)
            then
               return True;
            else
               Warnings.Warn_Once
                 ("Not running version command [" & Command.Flatten
                  & "] of external " & Utils.TTY.Name (Name)
                  & " as it is not in externals.version_output.allowlist"
                  & " (see 'alr help settings')");
               return False;
            end if;
      end case;
   end May_Run;

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
      if not May_Run (This.Command, Name) then
         GNAT.OS_Lib.Free (Location);
         return (Releases.Containers.Release_Sets.Empty_Set with null record);
      elsif Location in null then
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
         Output  : constant String := Lines.Flatten ("" & Latin_1.LF);
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

   ----------------
   -- Is_Allowed --
   ----------------

   function Is_Allowed (Command   : AAA.Strings.Vector;
                        Allowlist : String)
                        return Boolean
   is
      function Words (Text : String) return AAA.Strings.Vector is
      begin
         return Result : AAA.Strings.Vector do
            for Word of Split (Text, ' ', Trim => True) loop
               if Word /= "" then
                  Result.Append (Word);
               end if;
            end loop;
         end return;
      end Words;

      function Is_Wildcard_Argument (Arg : String) return Boolean is
        (Arg /= "" and then Arg (Arg'First) /= '-' and then
           (for all C of Arg =>
              C in 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'
                 | '.' | '_' | '+' | '-'));

      function Matches (Allowed : AAA.Strings.Vector) return Boolean is
        (Natural (Allowed.Length) = Natural (Command.Length)
         and then
           (for all I in Allowed.First_Index .. Allowed.Last_Index =>
              Allowed (I) = Command (I)
              or else (I > Allowed.First_Index
                       and then Allowed (I) = "*"
                       and then Is_Wildcard_Argument (Command (I)))));

      Entries : constant AAA.Strings.Vector := Split (Allowlist, ';');
   begin
      return (for some Entry_Text of Entries =>
                Matches (Words (Entry_Text)));
   end Is_Allowed;

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
