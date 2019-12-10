with Alire.Conditional;
with Alire.Properties;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;
with Alire.Utils;

with TOML;

package Alire.Actions with Preelaborate is

   --  TODO: probably should be a child of Alire.Properties for consistency.

   type Moments is (
                    Post_Fetch,   -- After being downloaded
                    Post_Compile  -- After being compiled as the main project
                   );

   --  It's probable that there'll be a need to pre-compile every dependency
   --  after being downloaded, and then we will have the possibility of having
   --  another moment post THAT compilation. But that compilation may depend on
   --  configuration set by the main project... -_-'. We'll cross that bridge
   --  once it proves necessary.

   type Action (<>) is abstract new Properties.Property with private;

   overriding function Key (This : Action) return String is (TOML_Keys.Action);

   function Moment (This : Action) return Moments;

   procedure Execute (This : Action;
                      Implementer : access procedure (This : Action'Class));

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

   overriding function To_TOML (This : Run) return TOML.TOML_Value;

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties;

private

   type Action (Moment : Moments)
   is abstract new Properties.Property with null record;

   function Moment (This : Action) return Moments is (This.Moment);

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

end Alire.Actions;
