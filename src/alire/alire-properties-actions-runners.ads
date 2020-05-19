with Alire.Utils;

package Alire.Properties.Actions.Runners with Preelaborate is

   --  A Run action executes custom commands

   type Run (<>) is new Action with private;
   --  Encapsulates the execution of an external command

   function New_Run (Moment                : Moments;
                     Relative_Command_Line : Utils.String_Vector;
                     Working_Folder        : Any_Path)
                     return Run;
   --  Working folder will be entered for execution
   --  Relative command-line must consider being in working folder

   function Command_Line   (This : Run) return Utils.String_Vector;
   function Working_Folder (This : Run) return String;

   overriding
   function To_TOML (This : Run) return TOML.TOML_Value;

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties;

private

   type Run (Moment : Moments; Folder_Len : Natural)
   is new Action (Moment) with record
      Relative_Command_Line : Utils.String_Vector;
      Working_Folder        : Any_Path (1 .. Folder_Len);
   end record;

   overriding
   function Image (This : Run) return String
   is (Utils.To_Mixed_Case (This.Moment'Img) & " run: <project>" &
        (if This.Working_Folder /= "" then "/" else "") &
        This.Working_Folder & "/" & This.Relative_Command_Line.Flatten);

   overriding
   function To_YAML (This : Run) return String
   is (Utils.To_Mixed_Case (This.Moment'Img) & " run: <project>" &
        (if This.Working_Folder /= "" then "/" else "") &
        This.Working_Folder & "/" & This.Relative_Command_Line.Flatten);

   function New_Run (Moment                : Moments;
                     Relative_Command_Line : Utils.String_Vector;
                     Working_Folder        : Any_Path)
                     return Run
   is
     (Moment,
      Working_Folder'Length,
      Relative_Command_Line,
      Utils.To_Native (Working_Folder));

   function Command_Line (This : Run) return Utils.String_Vector
   is (This.Relative_Command_Line);

   function Working_Folder (This : Run) return String
   is (This.Working_Folder);

end Alire.Properties.Actions.Runners;
