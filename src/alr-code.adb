with Alire.Index;

with Semantic_Versioning;

package body Alr.Code is

   use all type Semantic_Versioning.Conditions;

   function Condition_To_Code (C : Semantic_Versioning.Conditions) return String is
     (case C is
         when At_Least => " >= ",
         when At_Most => " <= ",
         when Exactly => " = ",
         when Except => " /= ",
         when Within_Major => ".Within_Major ",
         when Within_Minor => ".Within_Minor ");

   Need_Parenth : constant array (Semantic_Versioning.Conditions) of Boolean :=
                    (Within_Minor | Within_Major => True,
                     others                      => False);

   function Q (S : String; With_P : Boolean := False) return String is
     ((if With_P then "(" else "") &
        """" & S & """" &
      (if With_P then ")" else ""));

   --------------
   -- Generate --
   --------------

   function Generate (Deps : Alire.Dependencies.Vector)
                      return Utils.String_Vector
   is
      Result : Utils.String_Vector;
      use all type Alire.Dependencies.Dependency;
      use Semantic_Versioning;
   begin
      for Dep of Deps loop
         if Dep /= Deps.First_Element then
            Result.Append ("and");
         end if;

         declare
            Cat  : constant Alire.Index.Catalog_Entry :=
                    Alire.Index.Get (Dep.Project);
            Vers : constant Semantic_Versioning.Version_Set := Dep.Versions;
         begin
            if Length (Vers) = 0 then -- Any version
               Result.Append (Cat.Ada_Identifier & ".Current");
            else
               for I in 1 .. Length (Vers) loop
                  declare
                     Cond : constant Conditions := Condition (Element (Vers, I));
                     Ver  : constant Version    := On_Version (Element (Vers, I));
                  begin
                     Result.Append (Cat.Ada_Identifier &
                                      Condition_To_Code (Cond) &
                                      Q (Image (Ver), Need_Parenth (Cond)));
                  end;
               end loop;
            end if;
         end;
      end loop;

      return Result;
   end Generate;

end Alr.Code;
