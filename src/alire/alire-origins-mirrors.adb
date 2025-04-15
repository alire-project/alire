package body Alire.Origins.Mirrors is

   ----------
   -- Keys --
   ----------

   package Keys is
      --  The keys used to access the fields of the TOML object.
      Binary : constant String := "binary";
   end Keys;

   ----------
   -- Kind --
   ----------

   function Kind (This : Mirror) return Mirror_Kinds is
   begin
      return This.Data.Kind;
   end Kind;

   ---------
   -- URL --
   ---------

   function URL (This : Mirror) return Alire.URL is
   begin
      case This.Data.Kind is
         when Alire.Origins.Binary_Archive =>
            return +This.Data.Binary_Mirror.As_Data.URL;
         when VCS_Kinds =>
            return +This.Data.Repo_URL;
         when Alire.Origins.Source_Archive =>
            return +This.Data.Src_URL;
      end case;
   end URL;

   --------------
   -- Whenever --
   --------------

   function Whenever (This : Mirror; Env : Alire.Properties.Vector)
                      return Mirror
   is
   begin
      if This.Data.Kind = Alire.Origins.Binary_Archive then
         return Result : Mirror := This do
            Result.Data.Binary_Mirror :=
               This.Data.Binary_Mirror.Evaluate (Env);
         end return;
      else
         return This;
      end if;
   end Whenever;

   ------------------
   -- Is_Available --
   ------------------

   function Is_Available (This : Mirror; Env : Alire.Properties.Vector)
                          return Boolean
   is (This.Kind /= Binary_Archive
       or else
       not This.Data.Binary_Mirror.Evaluate (Env).Is_Empty);

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : Alire.TOML_Adapters.Key_Queue) return Mirror is
      Result : Mirror;
   begin
      --  Check if we are seeing a conditional binary origin, or a regular
      --  static one. If the former, divert to the dynamic loader; else
      --  continue loading normally.

      if From.Contains_Expression
        or else
          (From.Unwrap.Has (Keys.Binary) and then
           From.Unwrap.Get (Keys.Binary).As_Boolean)
      then
         Result := (Obj  => From.Unwrap.Clone,
                  Data => Mirror_Data'
                    (Kind          => Binary_Archive,
                     Binary_Mirror => (Binary_Loader.Load
                       (From => From.Descend
                            (Keys.Origin,
                             Table.Unwrap,
                             Context => "binary archive"),
                        Loader  => Binary_From_TOML'Access,
                        Resolve => True,
                        Strict  => False) with null record)));

         return Result;
      end if;

      --  Static loading
      raise Unimplemented;
   end From_TOML;

   -------------
   -- To_TOML --
   -------------

   function To_TOML (This : Mirror) return TOML.TOML_Value is (This.Obj);

end Alire.Origins.Mirrors;
