with AAA.Strings;

package CLIC.User_Input is

   -------------------
   -- Interactivity --
   -------------------

   Not_Interactive : aliased Boolean := False;
   --  When not Interactive, instead of asking the user something, use default.
   --  Currently only used before the first call to `sudo apt` to ask for
   --  confirmation.
   --  TODO: remove global eventually

   User_Interrupt : exception;
   --  Raised when the user hits Ctrl-D and no further input can be obtained as
   --  stdin is closed.

   type Answer_Kind is (Yes, No, Always);

   type Answer_Set is array (Answer_Kind) of Boolean;

   function Query (Question : String;
                   Valid    : Answer_Set;
                   Default  : Answer_Kind)
                   return Answer_Kind;
   --  If interactive, ask the user for one of the valid answer.
   --  Otherwise return the Default answer.

   function Query_Multi (Question         : String;
                         Choices          : AAA.Strings.Vector;
                         Page_Size        : Positive := 10)
                         return Positive
     with Pre => Page_Size >= 2 and then Page_Size < 36;
   --  Present the Choices in a numbered list 1-9-0-a-z, with paging if
   --  Choices.Length > Page_Size. Default is always First of Choices.

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

end CLIC.User_Input;
