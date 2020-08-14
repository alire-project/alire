with Alire.Errors;
with Alire.Properties.From_TOML;
with Alire.TOML_Expressions.Cases;
with Alire.TOML_Keys;
with Alire.Utils;

with TOML.File_IO;

package body Alire.TOML_Load is

   --  The following are entries in the manifest that are not loaded as
   --  properties, but stored separately as complex types.

   type Tables is (Available,
                   Dependencies);

   Allowed_Tables : constant array (Crates.Sections, Tables) of Boolean :=
                      (Crates.Release_Section          =>
                         (others => True),
                       Crates.External_Shared_Section  =>
                         (others => False),
                       Crates.External_Private_Section =>
                         (Available    => True,
                          Dependencies => False));

   ------------------
   -- Format_Error --
   ------------------

   function Format_Error (File   : Any_Path;
                          Result : TOML.Read_Result) return String
   is ((+Result.Message) & " at "
        & File & ":"
        & Utils.Trim (Result.Location.Line'Img) & ":"
        & Utils.Trim (Result.Location.Column'Img));

   ------------------------
   -- Load_Crate_Section --
   ------------------------

   procedure Load_Crate_Section (Section : Crates.Sections;
                                 From    : TOML_Adapters.Key_Queue;
                                 Props   : in out Conditional.Properties;
                                 Deps    : in out Conditional.Dependencies;
                                 Avail   : in out Requisites.Tree)
   is
      use TOML;
      use type Conditional.Dependencies;
      use type Conditional.Properties;
      use type Requisites.Tree;

      TOML_Avail : TOML.TOML_Value;
      TOML_Deps  : TOML.TOML_Value;

   begin

      --  Check mandatory fields existence

      for Ada_Key in Properties.From_TOML.Mandatory'Range (2) loop
         declare
            TOML_Key : constant String := TOML_Adapters.Tomify (Ada_Key'Img);
         begin
            if Properties.From_TOML.Mandatory (Section, Ada_Key) then
               if not From.Unwrap.Has (TOML_Key) then
                  From.Checked_Error
                    ("mandatory property missing: " & TOML_Key);
               end if;
            end if;
         end;
      end loop;

      --  Process Dependencies

      if Allowed_Tables (Section, Dependencies) then
         if From.Pop (TOML_Keys.Depends_On, TOML_Deps) then
            From.Assert (TOML_Deps.Kind = TOML_Array,
                         "dependencies must be specified as array of tables");

            for I in 1 .. TOML_Deps.Length loop
               Deps := Deps and
                 TOML_Expressions.Cases.Load_Dependencies
                   (TOML_Adapters.From
                      (TOML_Deps.Item (I),
                       From.Message (TOML_Keys.Depends_On)
                       & "(group" & I'Img & ")"));
            end loop;
         end if;
      elsif From.Unwrap.Has (TOML_Keys.Depends_On) then
         From.Checked_Error ("found field not allowed in manifest section: "
                             & TOML_Keys.Depends_On);
      end if;

      --  TODO: Process Forbidden

      --  Process Available

      if Allowed_Tables (Section, Available) then
         if From.Pop (TOML_Keys.Available, TOML_Avail) then
            Avail := Avail and
              TOML_Expressions.Cases.Load_Requisites
                (TOML_Adapters.From (TOML_Avail,
                 From.Message (TOML_Keys.Available)));
         end if;
      elsif From.Unwrap.Has (TOML_Keys.Available) then
         From.Checked_Error ("found field not allowed in manifest section: "
                             & TOML_Keys.Available);
      end if;

      --  Process remaining keys, which must be properties

      Props := Props and
        Properties.From_TOML.Section_Loaders (Section) (From);

   end Load_Crate_Section;

   ---------------
   -- Load_File --
   ---------------

   function Load_File (File_Name : Any_Path) return TOML.TOML_Value is
      TOML_Result : constant TOML.Read_Result :=
                      TOML.File_IO.Load_File (File_Name);
   begin
      if TOML_Result.Success then
         return TOML_Result.Value;
      else
         Raise_Checked_Error
           (Errors.Wrap ("Invalid TOML contents in file",
                         Format_Error (File_Name, TOML_Result)));
      end if;
   end Load_File;

end Alire.TOML_Load;
