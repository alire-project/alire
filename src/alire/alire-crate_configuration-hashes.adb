with Alire.Errors;

package body Alire.Crate_Configuration.Hashes is

   procedure Add_From (Config : Global_Config;
                       Rel    : Releases.Release;
                       Add    : access procedure (Kind, Key, Value : String))
   is
   begin
      --  We could iterate over Config contents, but by relying on the release
      --  reported variables we ensure nothing is amiss during hashing.

      for Untyped of Rel.Config_Variables loop
         declare
            Def : constant Properties.Configurations.Config_Type_Definition :=
                    Properties.Configurations.Config_Type_Definition (Untyped);
            Key : constant String :=
                    Crate_Configuration.Key (Rel.Name, Def.Name);
         begin
            if not Config.Var_Map.Contains (+Key) then
               raise Program_Error
                 with Errors.Set
                   ("Incomplete configuration during hashing, missing value "
                    & "for: " & Key);
            end if;

            if not Def.Valid (Config.Var_Map (+Key).Value) then
               raise Program_Error
                 with Errors.Set ("Invalid config value during hashing: "
                                  & Def.Image);
            end if;

            Add (Kind  => "config",
                 Key   => Key,
                 Value =>
                   Properties.Configurations.Image
                     (Config.Var_Map (+Key).Value));
         end;
      end loop;
   end Add_From;

end Alire.Crate_Configuration.Hashes;
