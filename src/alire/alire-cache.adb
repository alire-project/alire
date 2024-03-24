with Ada.Calendar;

with Alire.Directories;
with Alire.Paths;
with Alire.Platforms.Folders;
with Alire.Settings.Builtins;
with Alire.Settings.Edit;

with Ncdu;

package body Alire.Cache is

   use Alire.Directories.Operators;

   package Adirs renames Ada.Directories;
   package Du is new Ncdu;

   ----------
   -- Path --
   ----------

   function Path return Absolute_Path
   is (if Settings.Builtins.Cache_Dir.Get /= "" then
          Settings.Builtins.Cache_Dir.Get
       elsif not Settings.Edit.Is_At_Default_Dir then
          Settings.Edit.Path / Paths.Cache_Folder_Inside_Working_Folder
       else
          Platforms.Folders.Cache);

   -----------
   -- Usage --
   -----------

   function Usage return Usages is

      Busy_Top : Simple_Logging.Ongoing :=
                   Simple_Logging.Activity ("Listing");

      Busy : Simple_Logging.Ongoing := Simple_Logging.Activity ("");

      Last_Check : Ada.Calendar.Time := Ada.Calendar.Clock;

      --------------
      -- Progress --
      --------------

      procedure Progress (Path : String) is
         use Ada.Calendar;
      begin
         if Clock - Last_Check >= 0.1
           and then Directories.Is_File (Path / Alire.Paths.Crate_File_Name)
         then
            Busy_Top.Step;
            Busy.Step (Adirs.Simple_Name (Path));
            Last_Check := Clock;
         end if;
      end Progress;

      Tree  : constant Du.Tree := Du.List (Path,
                                           Progress => Progress'Access);

      ----------------
      -- Usage_Wrap --
      ----------------

      procedure Usage_Wrap (Parent   : in out Usages;
                            Children : Du.Tree;
                            Depth    : Depths;
                            Branch   : String := ""
                              --  Says if toolchains, releases, or builds
                           )
      is
      begin
         for Child of Children loop
            declare
               Branch           : constant String
                 := (if Usage_Wrap.Branch /= ""
                     then Usage_Wrap.Branch
                     else Adirs.Simple_Name (Child.Element.Path));
               Wrapped_Children : Usages;
            begin

               --  Wrap the children if we still have room to go down

               if Depth < Release or else
                 (Depth < Build
                  and then Branch = Paths.Build_Folder_Inside_Working_Folder)
               then
                  Usage_Wrap (Wrapped_Children,
                              Child.Element.Children,
                              Depth  => Depths'Succ (Depth),
                              Branch => Branch);
               end if;

               --  Create the wrapped node at the current depth

               Parent.Insert
                 (Item'
                    (Depth    => Depth,
                     Name     => +Adirs.Simple_Name (Child.Element.Path),
                     Path     => +Child.Element.Path,
                     Size     => Child.Tree_Size,
                     Children => Wrapped_Children));
            end;
         end loop;
      end Usage_Wrap;

   begin
      --  The root node should be the cache dir itself, unless there is still
      --  no cache at all.
      if Tree.Is_Empty then
         return Item_Sets.Empty_Set;
      elsif Tree.Length not in 1 then
         raise Program_Error
           with "Cache tree root length /= 1:" & Tree.Length'Image;
      end if;

      --  Iterate the obtained tree wrapping contents as our usage type
      return Result : Usages do
         Usage_Wrap (Result,
                     Tree.First_Element.Element.Children,
                     Depths'First);
      end return;
   end Usage;

end Alire.Cache;
