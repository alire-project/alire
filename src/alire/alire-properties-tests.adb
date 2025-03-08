package body Alire.Properties.Tests is

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (S : Settings) return TOML.TOML_Value is
      use TOML;
      Res : constant TOML_Value := Create_Table;
   begin
      case S.Runner.Kind is
         when Alire_Runner =>
            Res.Set ("runner", Create_String ("alire"));

         when External =>
            declare
               Arr : constant TOML_Value := Create_Array;
            begin
               for E of S.Runner.Command loop
                  Arr.Append (Create_String (E));
               end loop;
               Res.Set ("runner", Arr);
            end;
      end case;

      Res.Set ("directory", Create_String (S.Directory));
      Res.Set ("jobs", Create_Integer (Any_Integer (S.Jobs)));

      --  To emit an array of tables, we must return an array of 1 element

      return Arr : constant TOML_Value := Create_Array do
         Arr.Append (Res);
      end return;
   end To_TOML;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML
     (From : TOML_Adapters.Key_Queue) return Conditional.Properties
   is
      use type Conditional.Properties;
      use TOML;

      Raw : TOML_Value;
   begin
      if From.Unwrap.Kind /= TOML_Table then
         From.Checked_Error
           ("test: table with assignments expected, but got: "
            & From.Unwrap.Kind'Img);
      end if;

      if From.Pop_Single_Table (Raw, TOML_Table) /= TOML_Keys.Test then
         raise Program_Error;
         --  Can't happen, unless the dispatch to us itself was erroneous

      end if;

      return Props : Conditional.Properties do
         declare
            Local          : constant TOML_Adapters.Key_Queue :=
              From.Descend (Raw, "values");
            Res            : Settings := Default;
            Val            : TOML_Value;
            Runner_Visited : Boolean := False;
         begin
            if Local.Pop (TOML_Keys.Test_Runner, Val) then
               Runner_Visited := True;
               if Val.Kind = TOML_String and then Val.As_String = "alire" then
                  Res.Runner := (Kind => Alire_Runner);
               else
                  Local.Checked_Error
                    ("invalid builtin runner (accepted values: 'alire'). Use "
                     & "the 'command' field with an array to configure an "
                     & "external test runner.");
               end if;
            end if;

            if Local.Pop (TOML_Keys.Test_Command, Val) then
               if Runner_Visited then
                  Local.Checked_Error
                    ("the 'runner' and 'command' fields cannot be present at "
                     & "the same time");
               elsif Val.Kind = TOML_Array then
                  declare
                     Cmd : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
                  begin
                     for I in 1 .. Val.Length loop
                        if Val.Item (I).Kind /= TOML_String then
                           Local.Checked_Error
                             ("the test command must be an array of strings");
                        end if;
                        Cmd.Append (Val.Item (I).As_String);
                     end loop;
                     Res.Runner := (Kind => External, Command => Cmd);
                  end;
               else
                  Local.Checked_Error
                    ("the 'command' field must be an array of strings. Use "
                     & "the 'runner' field to configure a builtin runner.");
               end if;
            elsif not Runner_Visited then
               Local.Checked_Error
                 ("one of 'runner' or 'command' is required");
            end if;

            if Local.Pop (TOML_Keys.Test_Folder, Val) then
               if Val.Kind /= TOML_String then
                  Local.Checked_Error ("directory must be a string");
               elsif not (Val.As_Unbounded_String in Unbounded_Relative_Path)
               then
                  Local.Checked_Error
                    ("the 'directory' field must be a relative path from the "
                     & "crate root.");
               end if;
               Res.Directory := Val.As_Unbounded_String;
            end if;

            if Local.Pop (TOML_Keys.Test_Jobs, Val) then
               if Res.Runner.Kind /= Alire_Runner then
                  Local.Checked_Error
                    ("cannot have a jobs setting when using a custom runner");
               end if;

               if Val.Kind /= TOML_Integer
                 or else not (Val.As_Integer
                              in 0 .. Any_Integer (Natural'Last))
               then
                  Local.Checked_Error ("jobs must be a non negative integer");
               end if;
               Res.Jobs := Natural (Val.As_Integer);
            end if;
            Local.Report_Extra_Keys;

            Props := Props and Res;
         end;
      end return;
   end From_TOML;
end Alire.Properties.Tests;
