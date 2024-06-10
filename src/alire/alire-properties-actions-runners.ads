with Alire.Utils;

package Alire.Properties.Actions.Runners with Preelaborate is

   --  A Run action executes custom commands

   type Run (<>) is new Action with private;
   --  Encapsulates the execution of an external command

   function Command_Line   (This : Run) return AAA.Strings.Vector;
   function Working_Folder (This : Run) return String;

   overriding
   function To_TOML (This : Run) return TOML.TOML_Value;

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties;

private

   subtype Action_Name is String with Dynamic_Predicate =>
     (for all Char of Action_Name =>
        Char in 'a' .. 'z' | '0' .. '9' | '-')
     and then Action_Name'Length > 1
     and then Action_Name (Action_Name'First) in 'a' .. 'z'
     and then Action_Name (Action_Name'Last) /= '-'
     and then (for all I in Action_Name'First .. Action_Name'Last - 1 =>
                 (if Action_Name (I) = '-'
                  then Action_Name (I + 1) /= '-'));

   type Run (Moment : Moments; Folder_Len : Natural; Name_Len : Natural)
   is new Action (Moment) with record
      Name                  : String (1 .. Name_Len);
      --  Optional, except for custom actions, which require a name.

      Relative_Command_Line : AAA.Strings.Vector;
      Working_Folder        : Any_Path (1 .. Folder_Len);
   end record
     with Type_Invariant =>
       (Name = "" or else Name in Action_Name)
     and then
       (if Moment = On_Demand then Name /= "");

   overriding
   function Image (This : Run) return String
   is (AAA.Strings.To_Mixed_Case (This.Moment'Img)
       & (if This.Name /= "" then " (" & This.Name & ")" else "")
       & " run: " & This.Relative_Command_Line.Flatten
       & " (from ${CRATE_ROOT}/" & This.Working_Folder & ")");

   overriding
   function To_YAML (This : Run) return String
   is (AAA.Strings.To_Mixed_Case (This.Moment'Img) & " run: <project>" &
        (if This.Working_Folder /= "" then "/" else "") &
        This.Working_Folder & "/" & This.Relative_Command_Line.Flatten);

   function New_Run (Moment                : Moments;
                     Name                  : String;
                     Relative_Command_Line : AAA.Strings.Vector;
                     Working_Folder        : Any_Path)
                     return Run'Class
   --  Working folder will be entered for execution
   --  Relative command-line must consider being in working folder
   is
     (Run'
        (Moment,
         Working_Folder'Length,
         Name'Length,
         Name,
         Relative_Command_Line,
         Utils.To_Native (Working_Folder)));

   function Command_Line (This : Run) return AAA.Strings.Vector
   is (This.Relative_Command_Line);

   function Working_Folder (This : Run) return String
   is (This.Working_Folder);

end Alire.Properties.Actions.Runners;
