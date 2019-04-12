with Alire.Properties;
with Alire.Utils;

private with TOML;

package Alire.Actions with Preelaborate is

   type Moments is (
                    Post_Fetch,   -- After being downloaded
                    Post_Compile  -- After being compiled as the main project
                   );

   --  It's probable that there'll be a need to pre-compile every dependency after being downloaded,
   --  and then we will have the possibility of having another moment post THAT compilation
   --  But that compilation may depend on configuration set by the main project... -_-'
   --  We'll cross that bridge once it proves necessary

   type Action (<>) is abstract new Properties.Property with private;

   function Moment (This : Action) return Moments;

   procedure Execute (This : Action; Implementer : access procedure (This : Action'Class));


   type Run (<>) is new Action with private;
   --  Encapsulates the execution of an external command

   function New_Run (Moment                : Moments;
                     Relative_Command_Line : Platform_Independent_Path;
                     Working_Folder        : Platform_Independent_Path) return Run;
   --  Working folder will be entered for execution
   --  Relative command-line must consider being in working folder

   function Command_Line   (This : Run) return String;
   function Working_Folder (This : Run) return String;

private

   type Action (Moment : Moments) is abstract new Properties.Property with null record;

   function Moment (This : Action) return Moments is (This.Moment);

   type Run (Moment : Moments; Cmd_Len, Folder_Len : Natural) is new Action (Moment) with record
      Relative_Command_Line : Platform_Independent_Path (1 .. Cmd_Len);
      Working_Folder        : Platform_Independent_Path (1 .. Folder_Len);
   end record;

   overriding function Image (This : Run) return String is
     (Utils.To_Mixed_Case (This.Moment'Img) & " run: <project>" &
        (if This.Working_Folder /= "" then "/" else "") &
        This.Working_Folder & "/" & This.Relative_Command_Line);

   overriding function To_TOML (This : Run) return TOML.TOML_Value is (raise Program_Error with "TODO: implement");

   function New_Run (Moment                : Moments;
                     Relative_Command_Line : Platform_Independent_Path;
                     Working_Folder        : Platform_Independent_Path) return Run is
     (Moment,
      Relative_Command_Line'Length,
      Working_Folder'Length,
      Utils.To_Native (Relative_Command_Line),
      Utils.To_Native (Working_Folder));

   function Command_Line (This : Run) return String is (This.Relative_Command_Line);

   function Working_Folder (This : Run) return String is (This.Working_Folder);

end Alire.Actions;
