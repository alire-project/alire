------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ R E S U L T S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2011, AdaCore                   --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

with AUnit.Memory.Utils;

--  Record test results.

package body AUnit.Test_Results is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Alloc_Failure is new AUnit.Memory.Utils.Gen_Alloc
     (Test_Failure, Test_Failure_Access);

   function Alloc_Error is new AUnit.Memory.Utils.Gen_Alloc
     (Test_Error, Test_Error_Access);

   E_Count : Count_Type;
   F_Count : Count_Type;
   S_Count : Count_Type;

   procedure Iterate_Error (Position : Result_Lists.Cursor);
   procedure Iterate_Failure (Position : Result_Lists.Cursor);
   procedure Iterate_Success (Position : Result_Lists.Cursor);

   function Is_Error (Position : Result_Lists.Cursor) return Boolean;
   function Is_Failure (Position : Result_Lists.Cursor) return Boolean;
   function Is_Success (Position : Result_Lists.Cursor) return Boolean;

   generic
      with function Test (Position : Result_Lists.Cursor) return Boolean;
   procedure Gen_Extract (R : in out Result;
                          E : in out Result_Lists.List);

   -------------------
   -- Iterate_Error --
   -------------------

   procedure Iterate_Error (Position : Result_Lists.Cursor) is
   begin
      if Result_Lists.Element (Position).Error /= null then
         E_Count := E_Count + 1;
      end if;
   end Iterate_Error;

   ---------------------
   -- Iterate_Failure --
   ---------------------

   procedure Iterate_Failure (Position : Result_Lists.Cursor) is
   begin
      if Result_Lists.Element (Position).Failure /= null then
         F_Count := F_Count + 1;
      end if;
   end Iterate_Failure;

   ---------------------
   -- Iterate_Success --
   ---------------------

   procedure Iterate_Success (Position : Result_Lists.Cursor) is
   begin
      if Result_Lists.Element (Position).Error = null
        and then Result_Lists.Element (Position).Failure = null
      then
         S_Count := S_Count + 1;
      end if;
   end Iterate_Success;

   -----------------
   -- Gen_Extract --
   -----------------

   procedure Gen_Extract
     (R : in out Result;
      E : in out Result_Lists.List)
   is
      C : Result_Lists.Cursor;
      Prev : Result_Lists.Cursor;
      use Result_Lists;
   begin
      C := First (R.Result_List);
      Prev := No_Element;

      while Has_Element (C) loop
         if Test (C) then
            Splice (Target   => E,
                    Before   => No_Element,
                    Source   => R.Result_List,
                    Position => C);

            if Prev = No_Element then
               C := First (R.Result_List);
            else
               C := Next (Prev);
            end if;
         else
            Prev := C;
            Next (C);
         end if;
      end loop;
   end Gen_Extract;

   --------------
   -- Is_Error --
   --------------

   function Is_Error (Position : Result_Lists.Cursor) return Boolean is
   begin
      return Result_Lists.Element (Position).Error /= null;
   end Is_Error;

   ----------------
   -- Is_Failure --
   ----------------

   function Is_Failure (Position : Result_Lists.Cursor) return Boolean is
   begin
      return Result_Lists.Element (Position).Failure /= null;
   end Is_Failure;

   ----------------
   -- Is_Success --
   ----------------

   function Is_Success (Position : Result_Lists.Cursor) return Boolean is
   begin
      return not Is_Error (Position) and then not Is_Failure (Position);
   end Is_Success;

   ---------------
   -- Add_Error --
   ---------------

   procedure Add_Error
     (R            : in out Result;
      Test_Name    : Message_String;
      Routine_Name : Message_String;
      Error        : Test_Error;
      Elapsed      : Time)
   is
      Val : constant Test_Result := (Test_Name, Routine_Name,
                                     Failure => null,
                                     Error   => Alloc_Error,
                                     Elapsed => Elapsed);
      use Result_Lists;
   begin

      Val.Error.all := Error;
      Append (R.Result_List, Val);
   end Add_Error;

   -----------------
   -- Add_Failure --
   -----------------

   procedure Add_Failure
     (R            : in out Result;
      Test_Name    : Message_String;
      Routine_Name : Message_String;
      Failure      : Test_Failure;
      Elapsed      : Time) is

      Val : constant Test_Result := (Test_Name, Routine_Name,
                                     Failure => Alloc_Failure,
                                     Error   => null,
                                     Elapsed => Elapsed);
      use Result_Lists;
   begin

      Val.Failure.all := Failure;
      Append (R.Result_List, Val);
   end Add_Failure;

   -----------------
   -- Add_Success --
   -----------------

   procedure Add_Success
     (R                       : in out Result;
      Test_Name               : Message_String;
      Routine_Name            : Message_String;
      Elapsed                 : Time) is

      Val : constant Test_Result :=
              (Test_Name, Routine_Name, null, null, Elapsed);
      use Result_Lists;

   begin
      Append (R.Result_List, Val);
   end Add_Success;

   -----------------
   -- Set_Elapsed --
   -----------------

   procedure Set_Elapsed (R : in out Result;
                          T : Time_Measure.Time) is
   begin
      R.Elapsed_Time := T;
   end Set_Elapsed;

   -----------------
   -- Error_Count --
   -----------------

   function Error_Count (R : Result) return Count_Type is
      use Result_Lists;
   begin
      E_Count := 0;
      Iterate (R.Result_List, Iterate_Error'Access);
      return E_Count;
   end Error_Count;

   ------------
   -- Errors --
   ------------

   procedure Errors (R : in out Result;
                     E : in out Result_Lists.List) is
      procedure Extract is new Gen_Extract (Is_Error);
   begin
      Extract (R, E);
   end Errors;

   -------------------
   -- Failure_Count --
   -------------------

   function Failure_Count (R : Result) return Count_Type is
      use Result_Lists;
   begin
      F_Count := 0;
      Iterate (R.Result_List, Iterate_Failure'Access);
      return F_Count;
   end Failure_Count;

   --------------
   -- Failures --
   --------------

   procedure Failures (R : in out Result;
                       F : in out Result_Lists.List) is
      procedure Extract is new Gen_Extract (Is_Failure);
   begin
      Extract (R, F);
   end Failures;

   -------------
   -- Elapsed --
   -------------

   function Elapsed (R : Result) return Time_Measure.Time is
   begin
      return R.Elapsed_Time;
   end Elapsed;

   ----------------
   -- Start_Test --
   ----------------

   procedure Start_Test (R : in out Result; Subtest_Count : Count_Type) is
   begin
      R.Tests_Run := R.Tests_Run + Subtest_Count;
   end Start_Test;

   -------------------
   -- Success_Count --
   -------------------

   function Success_Count (R : Result)  return Count_Type is
   begin
      S_Count := 0;
      Result_Lists.Iterate (R.Result_List, Iterate_Success'Access);
      return S_Count;
   end Success_Count;

   ---------------
   -- Successes --
   ---------------

   procedure Successes (R : in out Result;
                        S : in out Result_Lists.List) is
      procedure Extract is new Gen_Extract (Is_Success);
   begin
      Extract (R, S);
   end Successes;

   ----------------
   -- Successful --
   ----------------

   function Successful (R : Result) return Boolean is
   begin
      return Success_Count (R) = Test_Count (R);
   end Successful;

   ----------------
   -- Test_Count --
   ----------------

   function Test_Count (R : Result) return Ada_Containers.Count_Type is
   begin
      return R.Tests_Run;
   end Test_Count;

   -----------
   -- Clear --
   -----------

   procedure Clear (R : in out Result) is
   begin
      R.Tests_Run    := 0;
      R.Elapsed_Time := Time_Measure.Null_Time;
      Result_Lists.Clear (R.Result_List);
   end Clear;

end AUnit.Test_Results;
