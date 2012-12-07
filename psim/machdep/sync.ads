with System;                                 use System;

package Sync is

   function Safe_Increment( Counter : in Address ) return Natural;
   function Safe_Decrement( Counter : in Address ) return Natural;
   function CAS ( Location : in Address; Expected : in Natural; Value : in Natural  ) return Boolean;
end ;



