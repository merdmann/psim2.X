with Parallel.Recursion.Stack_Safe_Work_Seeking_Functional_Reduction;
use Parallel;
with Parallel_Test_Harness;
with System;

function Integrate_Recursive_Stack_Safe_Work_Seeking
  (Integrand : access function (Value : Real) return Real;
   Lower, Upper : Real)
   return Real
is
   Other_Workers : aliased Parallel.Work_Seeking_State;

   type Bounds_Type is
      record
         Lower : Real;
         Upper : Real;
      end record;

   package Recursive_Area is new
     Parallel.Recursion.Stack_Safe_Work_Seeking_Functional_Reduction
       (Work_Type => Bounds_Type,
        Result_Type => Real,
        Reducer => "+",
        Identity_Value => 0.0);

   Dispatcher : aliased Recursive_Area.Recursion_Dispatcher_Access := null;

   function Integration (A, B : Real; Samples : Positive) return Real;
   function Parallel_Area
     (Bounds : Bounds_Type; Stack_Limit : System.Address) return Real;

   function Integration
     (A, B : Real;
      Samples : Positive) return Real
   is
      Increment : constant Real := (B - A) / Real (Samples);
      Sum : Real := 0.5 * Increment * (Integrand (A) + Integrand (B));
   begin
      for I in 0 .. Samples - 1 loop
         Sum := Sum + Increment * Integrand (A + Increment * Real (I));
      end loop;
      return Sum;
   end Integration;

   function Parallel_Area
     (Bounds : Bounds_Type;
      Stack_Limit : System.Address) return Real
   is
      Result : Real := Integration (Bounds.Lower, Bounds.Upper, 1000);
      Check : constant Real := Integration (Bounds.Lower, Bounds.Upper, 5);
   begin
      if abs (Result - Check) > 0.0000000001 then

         declare
            M : constant Real := (Bounds.Upper + Bounds.Lower) / 2.0;
            use type System.Address;
         begin

            if Other_Workers.Seeking_Work then

               Result :=
                 Dispatcher.Recurse
                   (Item => (Bounds.Lower, M),
                    Stack_Limit => Stack_Limit) +
                 Parallel_Area ((M, Bounds.Upper), Stack_Limit);

            elsif Bounds'Address <= Stack_Limit then

               Result :=
                 Dispatcher.Recurse
                   (Item => (Bounds.Lower, M),
                    Stack_Limit => Stack_Limit) +
                 Dispatcher.Recurse
                   (Item => (M, Bounds.Upper),
                    Stack_Limit => Stack_Limit);
            else
               Result :=
                 Parallel_Area ((Bounds.Lower, M), Stack_Limit) +
                 Parallel_Area ((M, Bounds.Upper), Stack_Limit);
            end if;
         end;
      end if;
      return Result;
   end Parallel_Area;

   Stack_Deferrals : aliased Parallel.Recursion.Stack_Limit_Count;
begin
   return Recursive_Area.Execute
     (Item  => (Lower, Upper),
      Other_Workers => Other_Workers'Access,
      Dispatcher => Dispatcher'Access,
      Max_Depth => Parallel_Test_Harness.Maximum_Recursive_Stack_Depth,
      Storage_Size => Parallel_Test_Harness.Worker_Storage_Size,
      Process => Parallel_Area'Access,
      Stack_Deferrals => Stack_Deferrals'Access);
end Integrate_Recursive_Stack_Safe_Work_Seeking;
