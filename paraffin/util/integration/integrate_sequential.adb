function Integrate_Sequential
  (Integrand : access function (Value : Real) return Real;
   Lower, Upper : Real)
   return Real
is
   function Integration (A, B : Real; Samples : Positive) return Real;
   function Area (Lower, Upper : Real) return Real;

   function Area (Lower, Upper : Real) return Real
   is
      Result : Real := Integration (Lower, Upper, 1000);
      Check : constant Real := Integration (Lower, Upper, 5);
   begin
      if abs (Result - Check) > 0.0000000001 then
         declare
            M : constant Real := (Upper + Lower) / 2.0;
         begin
            Result := Area (Lower, M) + Area (M, Upper);
         end;
      end if;
      return Result;
   end Area;

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

begin
   return Area (Lower, Upper);
end Integrate_Sequential;
