package body Alire.Dependencies.States.Maps is

   use TOML;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue) return Map is
      --  Read from an array of key:
      --  [[state]]

      States : constant TOML_Value := From.Unwrap; -- The TOML array

   begin
      return This : Map do
         for I in 1 .. States.Length loop
            declare
               Status : constant State :=
                          Dependencies.States.From_TOML
                            (From.Descend
                               (States.Item (I),
                                "state " & I'Img));
            begin
               This.Insert (Status.Crate, Status);
            end;
         end loop;
      end return;
   end From_TOML;

   --------------------
   -- Image_One_Line --
   --------------------

   function Image_One_Line (This : Map) return String is
      Result : AAA.Strings.Vector;
   begin
      if This.Is_Empty then
         return "(empty)";
      end if;

      for I in This.Iterate loop
         Result.Append (State_Maps.Key (I).As_String
                        & "->"
                        & (if This (I).Has_Release
                          then This (I).Milestone_Image
                          else This (I).Fulfilment'Image));
      end loop;

      return Result.Flatten (", ");
   end Image_One_Line;

   ---------------
   -- Including --
   ---------------

   function Including (Base  : Map;
                       State : States.State)
                       return Map
   is
   begin
      return Result : Map := Base do
         Result.Include (State.Crate, State);
      end return;
   end Including;

   -------------
   -- Merging --
   -------------

   function Merging (Base : Map;
                     Dep  : Dependencies.Dependency)
                     return Map
   is
      New_Dep : constant State :=
                  (if Base.Contains (Dep.Crate)
                   then Base (Dep.Crate).Merging (Dep.Versions)
                   else States.New_State (Dep));
   begin
      return Result : Map := Base do
         Result.Include (Dep.Crate, New_Dep);
      end return;
   end Merging;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Map) return TOML.TOML_Value is
      --  Stored as an array of individual states:
      --  [[state]]
   begin
      return Arr : constant TOML_Value := Create_Array do
         for Dep of This loop
            Arr.Append (Dep.To_TOML);
         end loop;
      end return;
   end To_TOML;

end Alire.Dependencies.States.Maps;
