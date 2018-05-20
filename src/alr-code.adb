with Alire.Conditional;
with Alire.Dependencies;
with Alire.Index;

with Alr.Platform;

with Semantic_Versioning;

package body Alr.Code is

   use all type Alire.Conditional.For_Dependencies.Conjunctions;
   use all type Semantic_Versioning.Conditions;

   function Condition_To_Code (C : Semantic_Versioning.Conditions) return String is
     (case C is
         when At_Least => " >= ",
         when At_Most => " <= ",
         when Exactly => " = ",
         when Except => " /= ",
         when Within_Major => ".Within_Major ",
         when Within_Minor => ".Within_Minor ");

   function Conj_To_Code (C : Alire.Conditional.For_Dependencies.Conjunctions)
                          return String is
     (case C is
         when Anded => "and",
         when Ored => "or");

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

   function Generate (R : Types.Release) return Utils.String_Vector is
      Code : Utils.String_Vector;
      Tab  : constant String := "   ";
   begin
      Code.Append ("New_Working_Release (");

      Code.Append (Tab & "Project => " & Utils.Quote (+R.Project) & ",");

      Code.Append (Tab & "Origin =>");
      Code.Append (R.Origin.To_Code.Indent (Tab & Tab));

      if not R.Dependencies.Is_Empty then
         Code.Append (Tab & "Dependendies =>");
         Code.Append (Generate (R.Dependencies (Platform.Properties))
                        .Indent (Tab & Tab));
      end if;

      declare
         use all type Utils.String_Vectors.Cursor;
         P : constant Utils.String_Vector :=
               R.Project_Files (Platform.Properties, With_Path => True);
      begin
         if not P.Is_Empty then
            Code.Append (Tab & "Properties =>");
            for I in P.Iterate loop
               Code.Append (Tab & Tab & "Project_File " & Q (P (I)));
               if I /= P.Last then
                  Code.Append (Tab & Tab & "and");
               end if;
            end loop;
         end if;
      end;

      Code.Append (")");

      return Code;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (Deps : Types.Platform_Dependencies)
                      return Utils.String_Vector
   is
      Result : Utils.String_Vector;
      use all type Alire.Dependencies.Dependency;
      use Semantic_Versioning;

      -----------
      -- Visit --
      -----------

      procedure Visit (Dep : Types.Platform_Dependencies; Prefix : String := "") is
         Tab : constant String := "   ";

         --------------------
         -- Add_Dependency --
         --------------------

         procedure Add_Dependency (Dep : Types.Dependency) is
            Cat  : constant Alire.Index.Catalog_Entry :=
                    Alire.Index.Get (Dep.Project);
            Vers : constant Semantic_Versioning.Version_Set := Dep.Versions;
         begin
            if Length (Vers) = 0 then -- Any version
               Result.Append (Prefix & Cat.Ada_Identifier & ".Current");
            else
               for I in 1 .. Length (Vers) loop
                  declare
                     Cond : constant Conditions := Condition (Element (Vers, I));
                     Ver  : constant Version    := On_Version (Element (Vers, I));
                  begin
                     Result.Append (Prefix & Cat.Ada_Identifier &
                                      Condition_To_Code (Cond) &
                                      Q (Image (Ver), Need_Parenth (Cond)));
                  end;
               end loop;
            end if;
         end Add_Dependency;

         use Alire.Conditional.For_Dependencies;

      begin
         case Dep.Kind is
            when Value =>
               Add_Dependency (Dep.Value);
            when Vector =>
               Result.Append (Prefix & "(");
               for I in Dep.Iterate loop -- "of" bugs out
                  Visit (Dep (I), Prefix & Tab);
                  if Has_Element (Next (I)) then -- and/or
                     Result.Append (Prefix & Tab & Conj_To_Code (Dep.Conjunction));
                  end if;
               end loop;
               Result.Append (Prefix & ")");
            when Condition =>
               raise Program_Error with "Requisites should be already evaluated";
         end case;
      end Visit;
   begin
      if Deps.Is_Empty then
         Result.Append ("No_Dependencies");
      else
         Visit (Deps);
      end if;

      return Result;
   end Generate;

end Alr.Code;
