private with Alire.Directories;
with Alire.Milestones;
private with Alire.Origins;
with Alire.Roots.Optional;
with Alire.URI;

package Alire.Publish is

   type All_Options is private;

   function New_Options (Skip_Build        : Boolean := False;
                         Skip_Submit       : Boolean := False;
                         For_Private_Index : Boolean := False;
                         Manifest          : String  := Roots.Crate_File_Name)
                         return All_Options;

   procedure Directory_Tar (Path     : Any_Path := ".";
                            Revision : String   := "HEAD";
                            Options  : All_Options := New_Options);
   --  Publish the release at the given directory, by creating a source archive
   --  to be uploaded somewhere. Then proceed with Remote_Origin using the
   --  uploaded archive. If a git repo is at Path, `git archive` will be
   --  used; otherwise `tar` will be used.

   procedure Local_Repository (Path     : Any_Path := ".";
                               Revision : String   := "HEAD";
                               Options  : All_Options := New_Options) with
     Pre => URI.URI_Kind (Path) in URI.Bare_Path;
   --  Check that given Path is a git repo with a remote configured. If so,
   --  check that Revision (tag, branch, commit) is suitable for publishing,
   --  then proceed using Remote_Origin.
   --
   --  If Revision is "" or "HEAD", use the repo's current HEAD.

   procedure Remote_Origin (URL     : Alire.URL;
                            Commit  : String := "";
                            Subdir  : Relative_Path := "";
                            Options : All_Options := New_Options);
   --  Requires a remote URL to a source file or a git repository. Commit is
   --  mandatory in the latter case. If Subdir is /= "", it is a relative path
   --  inside a repository with the actual location of a nested crate. Produces
   --  a file `crate-version.toml` in the current directory or raises
   --  Checked_Error with the appropriate error message set.

   function Branch_Name (M : Milestones.Milestone) return String
   is ("release/"
       & M.Crate.As_String & "-"
       & M.Version.Image);

   procedure Print_Trusted_Sites (For_Community : Boolean);
   --  Print our list of allowed sites to host git releases.
   --
   --  If For_Community is True, the list is the hardcoded
   --  Community_Trusted_Sites list, otherwise it is that configured with the
   --  'origins.git.trusted_sites' setting.

   function Is_Trusted (URL : Alire.URL; For_Community : Boolean)
                        return Boolean;
   --  According to the 'origins.git.trusted_sites' setting, or the hardcoded
   --  Community_Trusted_Sites if For_Community is True.

   type Data is tagged limited private;

private

   type All_Options is tagged record
      Manifest_File     : UString;
      Skip_Build        : Boolean := False;
      Skip_Submit       : Boolean := False;
      For_Private_Index : Boolean := False;
   end record;

   function Manifest (Options : All_Options) return Any_Path
   is (+Options.Manifest_File);

   function Nonstandard_Manifest (Options : All_Options) return Boolean
   is (Options.Manifest /= Roots.Crate_File_Name);

   --  Data shared across publishing steps, needs to be visible to children

   type Data is tagged limited record
      Options : All_Options;

      Origin : Origins.Origin := Origins.New_External ("undefined");
      --  We use external as "undefined" until a proper origin is provided.

      Path   : UString := +".";
      --  Where to find the local workspace

      Subdir : Unbounded_Relative_Path;
      --  Subdir inside the root repo, for monorepo crates

      Revision : UString := +"HEAD";
      --  A particular revision for publishing from a git repo

      Tmp_Deploy_Dir : Directories.Temp_File;
      --  Place to check the sources

      Root   : Roots.Optional.Root;
      --  Required valid by the submit steps

      Token  : UString;
      --  GitHub Personal Access token, required to fork/create PR
   end record;

   -----------------
   -- Branch_Name --
   -----------------

   function Branch_Name (This : in out Data) return String
   is (Branch_Name (This.Root.Value.Release.Milestone));

   -------------
   -- PR_Name --
   -------------

   function PR_Name (This : in out Data) return String
   is (This.Root.Value.Name.As_String & " "
       & This.Root.Value.Release.Version.Image);

   ------------------------
   -- Generated_Filename --
   ------------------------

   function Generated_Filename (This : in out Data) return String;

   ------------------------
   -- Generated_Manifest --
   ------------------------

   function Generated_Manifest (This : in out Data) return Absolute_Path;

end Alire.Publish;
