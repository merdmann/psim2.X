generic
  type Real is digits <>;
function Integrate_Recursive_Work_Seeking
  (Integrand : access function (Value : Real) return Real;
   Lower, Upper : Real) return Real;
pragma Preelaborate (Integrate_Recursive_Work_Seeking);
