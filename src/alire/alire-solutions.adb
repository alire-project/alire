with Alire.Crates.With_Releases;
with Alire.Dependencies;
with Alire.Releases;
with Alire.Solutions.Diffs;

package body Alire.Solutions is

   -------------
   -- Changes --
   -------------

   function Changes (Former, Latter : Solution) return Diffs.Diff is
     (Diffs.Between (Former, Latter));

   --------------
   -- Required --
   --------------

   function Required (This : Solution) return Containers.Crate_Name_Sets.Set is
   begin
      if not This.Valid then
         return Containers.Crate_Name_Sets.Empty_Set;
      end if;

      --  Merge release and hint crates

      return Set : Containers.Crate_Name_Sets.Set do
         for Dep of This.Hints loop
            Set.Include (Dep.Crate);
         end loop;

         for Rel of This.Releases loop
            Set.Include (Rel.Name);
         end loop;
      end return;
   end Required;

   ----------
   -- Keys --
   ----------

   package Keys is

      --  TOML keys used locally for loading and saving of solutions

      Advisory     : constant String := "advisory";
      Context      : constant String := "context";
      Dependencies : constant String := "dependency";
      Externals    : constant String := "externals";
      Valid        : constant String := "valid";

   end Keys;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Solution
   is
      --  We are parsing an internally generated structure, so any errors in it
      --  are unexpected.
   begin
      Trace.Debug ("Reading solution from TOML...");
      if From.Unwrap.Get (Keys.Context).Get (Keys.Valid).As_Boolean then
         return This : Solution (Valid => True) do
            Assert (From_TOML (This, From));
         end return;
      else
         Trace.Debug ("Read invalid solution from TOML");
         return (Valid => False);
      end if;
   end From_TOML;

   ---------------
   -- From_TOML --
   ---------------

   overriding
   function From_TOML (This : in out Solution;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome
   is
      use TOML;

      ------------------
      -- Read_Release --
      ------------------
      --  Load a single release. From points to the crate name, which contains
      --  crate.general and crate.version tables.
      function Read_Release (From : TOML_Value) return Alire.Releases.Release
      is
         Name  : constant String := +From.Keys (1);
         Crate : Crates.With_Releases.Crate :=
                   Crates.With_Releases.New_Crate (+Name);

         --  We can proceed loading the crate normally
         OK    : constant Outcome :=
                   Crate.From_TOML
                     (From_TOML.From.Descend
                        (Value   => From.Get (Name),
                         Context => "crate"));
      begin

         --  Double checks

         if From.Keys'Length /= 1 then
            From_TOML.From.Checked_Error ("too many keys in stored crate");
         end if;

         OK.Assert;

         if Crate.Releases.Length not in 1 then
            From_TOML.From.Checked_Error
              ("expected a single release, but found"
               & Crate.Releases.Length'Img);
         end if;

         return Crate.Releases.First_Element;
      end Read_Release;

   begin
      if not From.Unwrap.Get (Keys.Context).Get (Keys.Valid).As_Boolean then
         From.Checked_Error ("cannot load invalid solution");
      end if;

      Trace.Debug ("Reading valid solution from TOML...");

      --  Load proper releases, stored as a crate with a single release

      declare
         Releases     : TOML_Value;
         Has_Releases : constant Boolean :=
                          From.Pop (Keys.Dependencies, Releases);
      begin
         if Has_Releases then -- must be an array
            for I in 1 .. Releases.Length loop
               This.Releases.Insert (Read_Release (Releases.Item (I)));
            end loop;
         end if;
      end;

      --  Load external dependencies

      declare
         Externals     : TOML_Value;
         Has_Externals : constant Boolean :=
                           From.Pop (Keys.Externals, Externals);
      begin
         if Has_Externals then -- It's a table containing dependencies
            for I in 1 .. Externals.Keys'Length loop
               This.Hints.Merge
                 (Dependencies.From_TOML
                    (Key   => +Externals.Keys (I),
                     Value => Externals.Get (Externals.Keys (I))));
            end loop;
         end if;
      end;

      return Outcome_Success;
   end From_TOML;

   -------------
   -- To_TOML --
   -------------

   function To_TOML (This : Solution;
                     Props : Properties.Vector) return TOML.TOML_Value
   is
      Static_Solution : Solution := This;
   begin
      if This.Valid then
         Static_Solution.Releases := This.Releases.Whenever (Props);
      end if;

      return To_TOML (Static_Solution);
   end To_TOML;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Solution) return TOML.TOML_Value is
      use TOML;
   begin

      --  The structure used to store a solution is:
      --
      --  [context]
      --  Validity, advisory
      --
      --  [[dependency.crate_name.version]]
      --  Dependency release description
      --
      --  [externals]
      --  crate_name = "version set"
      --  ...

      return Root : constant TOML_Value := Create_Table do

         --  Output advisory and validity

         declare
            Context : constant TOML_Value := Create_Table;
         begin
            Root.Set (Keys.Context, Context);
            Context.Set
              (Keys.Advisory,
               Create_String
                 ("THIS IS AN AUTOGENERATED FILE. DO NOT EDIT MANUALLY"));
            Context.Set (Keys.Valid, Create_Boolean (This.Valid));
         end;

         --  Early exit when the solution is invalid

         if not This.Valid then
            return;
         end if;

         --  Output proper releases (except detected externals, which will be
         --  output as external hints)

         declare
            Deps : constant TOML_Value := Create_Array (TOML_Table);
         begin
            for Dep of This.Releases loop
               declare
                  Release : constant TOML_Value := Create_Table;
               begin
                  Deps.Append (Release);
                  Release.Set (Dep.Name_Str, Dep.To_TOML);
               end;
            end loop;

            Root.Set (Keys.Dependencies, Deps);
         end;

         --  Output external releases

         declare
            Externals : constant TOML_Value := Create_Table;
         begin
            if not This.Hints.Is_Empty then
               for Dep of This.Hints loop
                  Externals.Set (+Dep.Crate, Dep.To_TOML);
               end loop;

               Root.Set (Keys.Externals, Externals);
            end if;
         end;

      end return;
   end To_TOML;

end Alire.Solutions;
