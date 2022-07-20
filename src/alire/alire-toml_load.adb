with Alire.Conditional_Trees.TOML_Load;
with Alire.Expressions.Enums;
with Alire.Errors;
with Alire.Platforms;
with Alire.Properties.From_TOML;
with Alire.TOML_Keys;

with TOML.File_IO;

package body Alire.TOML_Load is

   use AAA.Strings;

   --  Instantiate loaders at library level

   package Available_Loader  is new Conditional.For_Available.TOML_Load;
   package Dependency_Loader is new Conditional.For_Dependencies.TOML_Load;

   --  Register predefined environment variables so they're recognized on load

   package Distro_Expressions is new Expressions.Enums
     (Key      => TOML_Keys.Distribution,
      Ada_Enum => Platforms.Distributions) with Unreferenced;

   package OS_Expressions is new Expressions.Enums
     (Key      => TOML_Keys.OS,
      Name     => "OS",
      Ada_Enum => Platforms.Operating_Systems) with Unreferenced;

   package Toolchain_Expressions is new Expressions.Enums
     (Key      => TOML_Keys.Toolchain,
      Ada_Enum => Platforms.Toolchains) with Unreferenced;

   package Word_Size_Expressions is new Expressions.Enums
     (Key      => TOML_Keys.Word_Size,
      Ada_Enum => Platforms.Word_Sizes) with Unreferenced;

   package Host_Arch_Expressions is new Expressions.Enums
     (Key      => TOML_Keys.Host_Arch,
      Ada_Enum => Platforms.Architectures) with Unreferenced;

   --  The following are entries in the manifest that are not loaded as
   --  properties, but stored separately as complex types.

   type Tables is (Available,
                   Dependencies,
                   Forbids,
                   Provides,
                   Origin);

   Allowed_Tables : constant array (Crates.Sections, Tables) of Boolean :=
                      (Crates.Index_Release            =>
                         (others => True),
                       Crates.Local_Release            =>
                         (Origin => False,
                          others => True),
                       Crates.External_Shared_Section  =>
                         (others => False),
                       Crates.External_Private_Section =>
                         (Available => True,
                          others    => False));

   ------------------
   -- Format_Error --
   ------------------

   function Format_Error (File   : Any_Path;
                          Result : TOML.Read_Result) return String
   is ((+Result.Message) & " at "
        & File & ":"
        & Trim (Result.Location.Line'Img) & ":"
        & Trim (Result.Location.Column'Img));

   ------------------------
   -- Load_Crate_Section --
   ------------------------

   procedure Load_Crate_Section
     (Strict  : Boolean;
      Section : Crates.Sections;
      From    : TOML_Adapters.Key_Queue;
      Props   : in out Conditional.Properties;
      Deps    : in out Conditional.Dependencies;
      Equiv   : in out Alire.Provides.Equivalences;
      Forbids : in out Conditional.Forbidden_Dependencies;
      Pins    : in out User_Pins.Maps.Map;
      Avail   : in out Conditional.Availability)
   is
      use TOML;
      use type Conditional.Dependencies;
      use type Conditional.Properties;

      TOML_Avail   : TOML.TOML_Value;
      TOML_Deps    : TOML.TOML_Value;
      TOML_Equiv   : TOML.TOML_Value;
      TOML_Forbids : TOML.TOML_Value;

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
            From.Assert
              (TOML_Deps.Kind = TOML_Array,
               "dependencies must be specified as array of tables");

            for I in 1 .. TOML_Deps.Length loop
               Deps := Deps and
                 Dependency_Loader.Load
                   (From    => From.Descend
                      (Key     => TOML_Keys.Depends_On,
                       Value   => TOML_Deps.Item (I),
                       Context => "(group" & I'Img & ")"),
                    Loader  => Conditional.Deps_From_TOML'Access,
                    Resolve => True,
                    Strict  => Strict);
            end loop;
         end if;
      elsif From.Unwrap.Has (TOML_Keys.Depends_On) then
         From.Checked_Error ("found field not allowed in manifest section: "
                             & TOML_Keys.Depends_On);
      end if;

      --  Process Forbids

      if Allowed_Tables (Section, TOML_Load.Forbids) then
         if From.Pop (TOML_Keys.Forbidden, TOML_Forbids) then
            From.Assert
              (TOML_Forbids.Kind = TOML_Array,
               "dependencies must be specified as array of tables");

            for I in 1 .. TOML_Forbids.Length loop
               Forbids := Forbids and
                 Dependency_Loader.Load
                   (From    => From.Descend
                      (Key     => TOML_Keys.Forbidden,
                       Value   => TOML_Forbids.Item (I),
                       Context => "(group" & I'Img & ")"),
                    Loader  => Conditional.Deps_From_TOML'Access,
                    Resolve => True,
                    Strict  => Strict);
            end loop;
         end if;
      elsif From.Unwrap.Has (TOML_Keys.Forbidden) then
         From.Checked_Error ("found field not allowed in manifest section: "
                             & TOML_Keys.Forbidden);
      end if;

      --  Process Provides

      if Allowed_Tables (Section, Provides) then
         if From.Pop (TOML_Keys.Provides, TOML_Equiv) then
            From.Assert
              (TOML_Equiv.Kind = TOML_Array,
               "provides must be an array of strings describing milestones");

            Equiv := Alire.Provides.From_TOML
              (From.Descend (TOML_Equiv, TOML_Keys.Provides));
         end if;
      end if;

      --  Process user pins

      if From.Contains (TOML_Keys.Pins) then
         Pins := User_Pins.Maps.From_TOML
           (From.Descend
              (From.Checked_Pop (TOML_Keys.Pins, TOML_Array),
               Context => TOML_Keys.Pins));
      end if;

      --  Process Available

      if Allowed_Tables (Section, Available) then
         if From.Pop (TOML_Keys.Available, TOML_Avail) then
            Avail.Append
              (Conditional.Availability'
                 (Available_Loader.Load
                      (From    => From.Descend
                           (Key     => TOML_Keys.Available,
                            Value   => TOML_Avail,
                            Context => TOML_Keys.Available),
                       Loader  => Conditional.Available_From_TOML'Access,
                       Resolve => True,
                       Strict  => Strict) with null record));
         end if;
      elsif From.Unwrap.Has (TOML_Keys.Available) then
         From.Checked_Error ("found field not allowed in manifest section: "
                             & TOML_Keys.Available);
      end if;

      --  Process remaining keys, which must be properties

      Props := Props and
        Properties.From_TOML.Section_Loaders (Section) (From, Strict);

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
