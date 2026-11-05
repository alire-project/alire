with Ada.Unchecked_Deallocation;

with Alire.Cache.Locks;
with Alire.Directories;
with Alire.Environment;
with Alire.OS_Lib;
with Alire.Paths;
with Alire.Roots;
with Alire.Settings.Builtins;

with GNAT.OS_Lib;

package body Alire.Builds.Locks is

   use Directories.Operators;

   Held : AAA.Strings.Set;
   --  Locks held by this process, so they're not taken twice (e.g. when
   --  configuring a folder while syncing it).

   Separator : constant String := (1 => GNAT.OS_Lib.Path_Separator);

   -------------
   -- Enabled --
   -------------

   function Enabled return Boolean
   is (Settings.Builtins.Dependencies_Shared_Locking.Get);

   ---------------
   -- Lock_Name --
   ---------------

   function Lock_Name (Root    : in out Roots.Root;
                       Release : Releases.Release)
                       return Relative_Path
   is
      Hash : constant String := Root.Build_Hash (Release.Name);
   begin
      return Paths.Build_Folder_Inside_Working_Folder
        / (Release.Deployment_Folder & "_"
           & Hash (Hash'First .. Hash'First + 7) & ".lock");
   end Lock_Name;

   ------------
   -- Export --
   ------------

   procedure Export is
   begin
      OS_Lib.Setenv (Environment.Build_Locks,
                     Held.To_Vector.Flatten (Separator));
   end Export;

   -------------
   -- Acquire --
   -------------

   function Acquire (Wanted : AAA.Strings.Map) return Holder
   --  Wanted maps lock names to the milestone being built, and is sorted by
   --  name, which gives the order in which locks are taken.
   is
      Inherited_List : constant AAA.Strings.Vector :=
                         AAA.Strings.Split
                           (OS_Lib.Getenv (Environment.Build_Locks),
                            GNAT.OS_Lib.Path_Separator);
      Inherited : constant AAA.Strings.Set :=
                    AAA.Strings.To_Set (Inherited_List);
      --  Held by the alr process that started this one, which is waiting for
      --  this one to finish.
   begin
      return This : Holder := (Ada.Finalization.Limited_Controlled with
                               others => <>)
      do
         if not Enabled then
            return;
         end if;

         --  Get locks for not yet locked ones only

         for I in Wanted.Iterate loop
            declare
               Name : constant String := AAA.Strings.Maps.Key (I);
               Lock : Lock_Access;
            begin
               if not Held.Contains (Name)
                 and then not Inherited.Contains (Name)
               then
                  Lock := new AAA.Filesystem.Locks.Lock;
                  Cache.Locks.Acquire
                    (Lock.all,
                     Name  => Name,
                     Doing => "building " & AAA.Strings.Maps.Element (I));
                  This.Locks.Append (Lock);
                  This.Names.Append (Name);
                  Held.Include (Name);
               end if;
            end;
         end loop;

         --  Export our locks in case we invoke another alr via actions or
         --  some other means (?)

         if not This.Names.Is_Empty then
            Export;
         end if;
      end return;
   end Acquire;

   ----------
   -- None --
   ----------

   function None return Holder
   is (Ada.Finalization.Limited_Controlled with others => <>);

   ------------
   -- Folder --
   ------------

   function Folder (Root    : in out Roots.Root;
                    Release : Releases.Release)
                    return Holder
   is
      Wanted : AAA.Strings.Map;
   begin
      if Is_Shared (Root, Release) then
         Wanted.Insert (Lock_Name (Root, Release),
                        Release.Milestone.TTY_Image);
      end if;

      return Acquire (Wanted);
   end Folder;

   -------------------
   -- For_Compiling --
   -------------------

   function For_Compiling (Root    : in out Roots.Root;
                           Release : Releases.Release)
                           return Holder
   is
      Wanted : AAA.Strings.Map;
   begin
      if Is_Shared (Root, Release) then
         Wanted.Include (Lock_Name (Root, Release),
                         Release.Milestone.TTY_Image);
      end if;

      for Rel of Root.Solution.Releases loop
         if Is_Shared (Root, Rel) and then not Is_Built (Root, Rel) then
            Wanted.Include (Lock_Name (Root, Rel), Rel.Milestone.TTY_Image);
         end if;
      end loop;

      return Acquire (Wanted);
   end For_Compiling;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Holder) is
      procedure Free is
        new Ada.Unchecked_Deallocation (AAA.Filesystem.Locks.Lock,
                                        Lock_Access);
   begin
      for Lock of This.Locks loop
         Free (Lock); -- Finalizing it releases it
      end loop;
      This.Locks.Clear;

      for Name of This.Names loop
         Held.Exclude (Name);
      end loop;

      if not This.Names.Is_Empty then
         This.Names.Clear;
         Export;
      end if;
   end Finalize;

end Alire.Builds.Locks;
