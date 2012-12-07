generic
  type Real is digits <>;
function Integrate
  (Integrand : access function (Value : Real) return Real;
   Lower, Upper : Real) return Real;
pragma Preelaborate (Integrate);
