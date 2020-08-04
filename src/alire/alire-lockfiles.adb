with Alire.Manifest;
with Alire.TOML_Keys;
with Alire.TOML_Load;

with GNATCOLL.Email.Utils;

package body Alire.Lockfiles is

   ----------
   -- Keys --
   ----------

   package Keys is

      --  Key used internally for TOML serialization

      Solution : constant String := "solution";

   end Keys;

   ---------------
   -- From_TOML --
   ---------------

   overriding
   function From_TOML (This : in out Contents;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome
   is
   begin
      --  Choose between plain/base64 encodings:
      if From.Unwrap.Has ("data") then -- encoded
         declare
            use GNATCOLL.Email.Utils;
            Data : constant String :=
                     From.Checked_Pop ("data", TOML.TOML_String).As_String;
            Decoded : UString;
            Value   : TOML.TOML_Value;
         begin
            Base64_Decode
              (Str    => Utils.Split (Data, ASCII.LF).Flatten (""),
               Result => Decoded);
            Value := TOML.Load_String (+Decoded).Value;
            This.Solution :=
              Solutions.From_TOML
                (From.Descend
                   (Value.Get (Keys.Solution),
                    Keys.Solution));
         end;
      else
         This.Solution :=
           Solutions.From_TOML
             (From.Descend
                (From.Checked_Pop
                   (Key  => Keys.Solution,
                    Kind => TOML.TOML_Table),
                 Keys.Solution));
      end if;

      From.Report_Extra_Keys;

      return Outcome_Success;
   end From_TOML;

   ----------
   -- Read --
   ----------

   function Read (Filename : Any_Path) return Contents is
   begin
      Trace.Debug ("Reading persistent contents from " & Filename);

      return This : Contents do
         declare
            TOML_Contents : constant TOML_Adapters.Key_Queue :=
                              TOML_Adapters.From
                                (TOML_Load.Load_File (Filename),
                                 Context => Filename);
         begin
            Assert
              (This.From_TOML
                 (TOML_Contents.Descend
                      (Value   => TOML_Contents.Checked_Pop
                           (TOML_Keys.Privat, TOML.TOML_Table),
                       Context => TOML_Keys.Privat)));
         end;
      end return;
   end Read;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Contents) return TOML.TOML_Value
   is
   begin
      return Table : constant TOML.TOML_Value := TOML.Create_Table do
         Table.Set (Keys.Solution, This.Solution.To_TOML);
      end return;
   end To_TOML;

   --------------
   -- Validity --
   --------------

   function Validity (File : Any_Path) return Validities is
   begin
      if not GNAT.OS_Lib.Is_Read_Accessible_File (File) then
         return Missing;
      end if;

      --  Try to load to assess validity

      declare
         Unused : constant Contents := Read (File);
      begin
         return Valid;
      end;

   exception
      when E : others =>
         Trace.Debug ("Exception while loading lockfile is: ");
         Log_Exception (E, Debug);
         return Invalid;
   end Validity;

   -----------
   -- Write --
   -----------

   procedure Write (Contents : Lockfiles.Contents;
                    Filename : Any_Path)
   is
   begin
      Trace.Debug ("Dumping lockfile contents to " & Filename);
      Manifest.Replace_Private (Filename, Contents.To_TOML);
   end Write;

end Alire.Lockfiles;
