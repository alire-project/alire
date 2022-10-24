with Ada.Directories;

with GNAT.OS_Lib;

with Alire.VFS;

with CLIC.User_Input; use CLIC.User_Input;

package body Alire.Utils.User_Input is

   -----------------
   -- Approve_Dir --
   -----------------

   function Approve_Dir (Dir   : Any_Path;
                         Force : Boolean := Alire.Force)
                         return Boolean
   is
   begin
      if not GNAT.OS_Lib.Is_Directory (Dir) then
         return Query
           (Question => TTY.Error (if TTY.Color_Enabled then U ("⚠") else "!")
                        & " Given path does not exist: " & TTY.URL (Dir)
                        & ASCII.LF & "Do you want to continue anyway?",
            Valid    => (Yes | No => True, others => False),
            Default  => (if Force then Yes else No))
           = Yes;
      end if;

      return True;
   end Approve_Dir;

   ------------------------------
   -- Confirm_Solution_Changes --
   ------------------------------

   function Confirm_Solution_Changes
     (Changes        : Alire.Solutions.Diffs.Diff;
      Changed_Only   : Boolean            := not Alire.Detailed;
      Level          : Alire.Trace.Levels := Info)
      return Boolean
   is
      package UI renames CLIC.User_Input;
   begin
      Trace.Log ("", Level);

      if Changes.Contains_Changes then
         Trace.Log ("Changes to dependency solution:", Level);
         Changes.Print (Changed_Only => Changed_Only);

         Trace.Log ("", Level);

         return UI.Query
           (Question => "Do you want to proceed?",
            Valid    => (Yes | No => True,
                         others   => False),
            Default  => (if Changes.Latter_Is_Complete or else Alire.Force
                         then Yes
                         else No)) = Yes;
      else
         Trace.Log
           ("There are no changes between the former and new solution.",
            Level);
         return True;
      end if;
   end Confirm_Solution_Changes;

   -------------------------------
   -- To_Absolute_From_Portable --
   -------------------------------

   function To_Absolute_From_Portable
     (User_Path                  : String;
      Error_When_Relative_Native : String :=
        "relative paths must use forward slashes to be portable")
      return Absolute_Path
   is
   begin
      if not Check_Absolute_Path (User_Path) and then
        not VFS.Is_Portable (User_Path)
      then
         Recoverable_Error
           (Error_When_Relative_Native & ": " & TTY.URL (User_Path));
      end if;

      --  Make the path absolute if not already, and store it

      return
        Ada.Directories.Full_Name
          (if VFS.Is_Portable (User_Path)
           then VFS.To_Native (Portable_Path (User_Path))
           else User_Path);

   end To_Absolute_From_Portable;

end Alire.Utils.User_Input;
