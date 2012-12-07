generic
  type Real is digits <>;
function Integrate_Sequential
  (Integrand : access function (Value : Real) return Real;
   Lower, Upper : Real) return Real;
pragma Preelaborate (Integrate_Sequential);
