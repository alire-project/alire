with Alire.Solutions.Diffs;

package Alire.Utils.User_Input is

   function Confirm_Solution_Changes
     (Changes        : Solutions.Diffs.Diff;
      Changed_Only   : Boolean            := not Alire.Detailed;
      Level          : Alire.Trace.Levels := Info)
      return Boolean;
   --  Present a summary of changes and ask the user for confirmation. Returns
   --  True when the user answers positively. Defaults to Yes when the new
   --  solution is complete, or when Alire.Force.

   function Approve_Dir (Dir   : Any_Path;
                         Force : Boolean := Alire.Force)
                         return Boolean;
   --  Some commands receive a path from the user (e.g., pinning). If such path
   --  does not exist, we allow to continue only after user confirmation (or
   --  forcing). Returns whether to proceed.

   ----------------
   -- VALIDATION --
   ----------------

   function To_Absolute_From_Portable
     (User_Path                  : String;
      Error_When_Relative_Native : String :=
        "relative paths must use forward slashes to be portable")
      return Absolute_Path;
   --  Paths given by the user in the manifest have to be vetted for
   --  portability. If they are absolute there is nothing to do; but if they
   --  are relative they may be native or portable. Here we check if a relative
   --  path is portable (which is desirable so a manifest/lockfile can work
   --  across OSes) and, for internal processing, we convert it in any case
   --  to a native absolute path.

end Alire.Utils.User_Input;
