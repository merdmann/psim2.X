with Parallel.Recursion.Work_Seeking_Functional_Reduction; use Parallel;

function Integrate_Recursive_Work_Seeking
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
     Parallel.Recursion.Work_Seeking_Functional_Reduction
       (Work_Type => Bounds_Type,
        Result_Type => Real,
        Reducer => "+",
        Identity_Value => 0.0);

   function Integration (A, B : Real; Samples : Positive) return Real;
   function Parallel_Area (Bounds : Bounds_Type) return Real;

   Dispatcher : aliased Recursive_Area.Recursion_Dispatcher_Access := null;

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

   function Parallel_Area (Bounds : Bounds_Type) return Real
   is
      Result : Real := Integration (Bounds.Lower, Bounds.Upper, 1000);
      Check : constant Real := Integration (Bounds.Lower, Bounds.Upper, 5);
   begin
      if abs (Result - Check) > 0.0000000001 then

         declare
            M : constant Real := (Bounds.Upper + Bounds.Lower) / 2.0;
         begin

            if Other_Workers.Seeking_Work then
               Result :=
                 Dispatcher.Recurse
                   (Item => (Bounds.Lower, M)) +
                 Parallel_Area ((M, Bounds.Upper));
            else
               Result :=
                 Parallel_Area ((Bounds.Lower, M)) +
                 Parallel_Area ((M, Bounds.Upper));
            end if;
         end;
      end if;
      return Result;
   end Parallel_Area;

begin
   return Recursive_Area.Execute
     (Item  => (Lower, Upper),
      Other_Workers => Other_Workers'Access,
      Dispatcher => Dispatcher'Access,
      Process => Parallel_Area'Access);
end Integrate_Recursive_Work_Seeking;
