with Alire.Solutions.Diffs;

package Alire.Utils.User_Input is

   -------------------
   -- Interactivity --
   -------------------

   Not_Interactive : aliased Boolean := False;
   --  When not Interactive, instead of asking the user something, use default.
   --  Currently only used before the first call to `sudo apt` to ask for
   --  confirmation.
   --  TODO: remove global eventually

   type Answer_Kind is (Yes, No, Always);

   type Answer_Set is array (Answer_Kind) of Boolean;

   function Query (Question : String;
                   Valid    : Answer_Set;
                   Default  : Answer_Kind)
                   return Answer_Kind;
   --  If interactive, ask the user for one of the valid answer.
   --  Otherwise return the Default answer.

   type Answer_With_Input (Length : Natural) is record
      Input  : String (1 .. Length);
      Answer : Answer_Kind;
   end record;

   function Validated_Input
     (Question : String;
      Prompt   : String;
      Valid    : Answer_Set;
      Default  : access function (User_Input : String) return Answer_Kind;
      Confirm  : String := "Is this information correct?";
      Is_Valid : access function (User_Input : String) return Boolean)

      return Answer_With_Input;
   --  Interactive prompt for information from the user, with confirmation:
   --  Put_Line (Question)
   --  loop
   --     Put (Prompt); Get_Line (User_Input);
   --     if Is_Valid (User_Input) then
   --        exit when Query (Confirm, Valid, Default (User_Input)) /= No;
   --     end if;
   --  end loop

   function Img (Kind : Answer_Kind) return String;

   type String_Validation_Access is
     access function (Str : String) return Boolean;

   function Query_String (Question   : String;
                          Default    : String;
                          Validation : String_Validation_Access)
                          return String
     with Pre => Validation = null or else Validation (Default);
   --  If interactive, ask the user to provide a valid string.
   --  Otherwise return the Default value.
   --
   --  If Validation is null, any input is accepted.
   --
   --  The Default value has to be a valid input.

   procedure Continue_Or_Abort;
   --  If interactive, ask the user to press Enter or Ctrl-C to stop.
   --  Output a log trace otherwise and continue.

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
