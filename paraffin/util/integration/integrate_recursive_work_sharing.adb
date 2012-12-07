with Parallel.Recursion.Work_Sharing_Functional_Reduction; use Parallel;

function Integrate_Recursive_Work_Sharing
  (Integrand : access function (Value : Real) return Real;
   Lower, Upper : Real)
   return Real
is

   type Bounds_Type is
      record
         Lower : Real;
         Upper : Real;
      end record;

   package Recursive_Area is new
     Parallel.Recursion.Work_Sharing_Functional_Reduction
       (Work_Type => Bounds_Type,
        Result_Type => Real,
        Reducer => "+",
        Identity_Value => 0.0);

   Dispatcher : aliased Recursive_Area.Recursion_Dispatcher_Access := null;

   ------------------  L O C A L   F U N C T I O N S -----------------------

   function Integration (A, B : Real; Samples : Positive) return Real;
   function Parallel_Area (Bounds : Bounds_Type;
                           Subcontractors : Worker_Count_Type) return Real;
   function Sequential_Area (Bounds : Bounds_Type) return Real;

   --------------------------------------------------------------------------

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

   function Parallel_Area (Bounds : Bounds_Type;
                           Subcontractors : Worker_Count_Type) return Real
   is
      Result : Real := Integration (Bounds.Lower, Bounds.Upper, 1000);
      Check : constant Real := Integration (Bounds.Lower, Bounds.Upper, 5);
   begin
      if abs (Result - Check) > 0.0000000001 then
         declare
            M : constant Real := (Bounds.Upper + Bounds.Lower) / 2.0;
         begin

            Result :=
              Dispatcher.Recurse
                (Item => (Bounds.Lower, M),
                 Split => 1,
                 Of_Splits => 2,
                 Subcontractors => Subcontractors) +
              Dispatcher.Recurse
                (Item => (M, Bounds.Upper),
                 Split => 2,
                 Of_Splits => 2,
                 Subcontractors => Subcontractors);
         end;
      end if;

      return Result;
   end Parallel_Area;

   --  Recursively determine area
   function Sequential_Area (Bounds : Bounds_Type) return Real
   is
      Result : Real := Integration (Bounds.Lower, Bounds.Upper, 1000);
      Check : constant Real := Integration (Bounds.Lower, Bounds.Upper, 5);
   begin
      if abs (Result - Check) > 0.0000000001 then
         declare
            M : constant Real := (Bounds.Upper + Bounds.Lower) / 2.0;
         begin
            Result :=
              Sequential_Area ((Bounds.Lower, M)) +
              Sequential_Area ((M, Bounds.Upper));
         end;
      end if;
      return Result;
   end Sequential_Area;

begin
   return Recursive_Area.Execute
     (Item  => (Lower, Upper),
      Dispatcher => Dispatcher'Access,
      Parallel_Process => Parallel_Area'Access,
      Process => Sequential_Area'Access);
end Integrate_Recursive_Work_Sharing;
