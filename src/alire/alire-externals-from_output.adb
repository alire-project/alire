with Alire.Index;
with Alire.Origins;
with Alire.OS_Lib.Subprocess;
with Alire.Releases;
with Alire.Requisites;
with Alire.TOML_Keys;

with Semantic_Versioning;

with TOML;

package body Alire.Externals.From_Output is

   ------------
   -- Detect --
   ------------

   overriding
   function Detect (This : External;
                    Name : Crate_Name) return Containers.Release_Set
   is
      use GNAT.Regpat;
      Matches : Match_Array (1 .. Match_Count'Last);
      Output  : constant String :=
                  OS_Lib.Subprocess.Checked_Spawn_And_Capture
                    (This.Command.First_Element,
                     This.Command.Tail).Flatten ("" & ASCII.LF);
      --  ASCII.LF is used by Regpat for new lines
   begin
      return Releases : Containers.Release_Set do
         Trace.Debug ("Looking for external in version string: " & Output);
         Match (This.Regexp, Output, Matches);

         for I in Matches'Range loop
            if Matches (I) /= No_Match then
               declare
                  Version : constant String :=
                              Output (Matches (I).First .. Matches (I).Last);
                  Path    : constant Any_Path :=
                              OS_Lib.Subprocess.Locate_In_Path
                                (This.Command.First_Element);
               begin
                  Trace.Debug ("Identified external from version: " & Version);

                  Releases.Insert
                    (Index.Crate (Name).Base
                     .Retagging (Semantic_Versioning.Parse (Version))
                     .Replacing (Origins.New_External ("path " & Path))
                     .Replacing (Notes => "Detected at " -- prefix of length 12
                                 & Utils.Shorten
                                   (String (Path),
                                    Max_Description_Length - 12)));
               end;
            end if;
         end loop;
      end return;
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
                    return Utils.String_Vector is
      (Utils.Empty_Vector.Append (+This.Regstr));

end Alire.Externals.From_Output;
