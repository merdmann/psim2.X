------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                   F F T _ W O R K _ S E E K I N G                  --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                  Copyright (C) 2011, Bradley J. Moore                    --
--                                                                          --
--  Paraffin is free software;  you can  redistribute it  and/or modify it  --
--  under  terms of the  GNU General Public License  as  published  by the  --
--  Free Software  Foundation;  either version 2,  or (at your option) any  --
--  later  version.  Paraffin is  distributed in the hope that it  will be  --
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  --
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU  --
--  General Public License for  more details.  You should have  received a  --
--  copy of the GNU General Public License distributed with Paraffin;  see  --
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,   --
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.         --
--                                                                          --
--  As a  special exception, if other files  instantiate generics from      --
--  this unit,  or you link this  unit with other files  to produce an      --
--  executable,  this unit  does  not by  itself  cause the  resulting      --
--  executable to be covered by  the GNU General Public License.  This      --
--  exception does  not however invalidate  any other reasons  why the      --
--  executable file might be covered by the GNU Public License.             --
------------------------------------------------------------------------------

with Parallel;
with System.Storage_Elements;

generic
   type Index_Type is range <>;
   type Element_Type is digits <>;
   type Array_Type is array (Index_Type range <>) of Element_Type;
package FFT_Work_Seeking is

   Min_Sequential_Length : constant := 2**7;
   --  The minimum length of the data that must be processed sequentially
   --  NOTE: This constant should be edited to suit the usage.
   --  In general, the higher the value the better the performance,
   --  up to a point. Powers of two greater than 6 or 7 appear to give
   --  little benefit, (on an AMD quadcore processor at least)
   --  and may start to detract performance.
   --  Also note that the value must be a power of two and note that the
   --  data length / this value must be greater than the number of available
   --  CPU's in order for the parallelism to work. This needs to be a universal
   --  integer, because it affects the inclusion of code at compile time.

   procedure FFT
     (Data : in out Array_Type;
      Log2_N : Natural;
      Forward : Boolean := True;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Parallel.Default_Worker_Storage_Size);
   pragma Precondition
     (Data'First = 0 and then
        Is_A_Power_Of_Two (Data'Length) and then
         Data'Length / Min_Sequential_Length >
        Natural (Parallel.Available_CPUs));
--  pragma Postcondition (Correct_Results (Data'Old, Data, Forward));
   --  Compute the Fast Fourier Transform of a series of data in Parallel
   --  If 'Forward' is true, then the Fast Fourier Transform is applied.
   --  Otherwise, the inverse transform is applied.

   type Array_Length is mod 2 ** 32;
   for Array_Length'Size use 32;

   function Is_A_Power_Of_Two (Value : Array_Length) return Boolean;

   function Correct_Results
     (Input, Output : Array_Type;
      Log2_N : Natural;
      Forward : Boolean) return Boolean;
   --  Useful as a postcondition check to determine if the FFT is working
   --  properly. Returns True if the results are as expected, False otherwise.
   --  The value of 'Forward' should be opposite of the value used to generate
   --  the output.
   --  NOTE: This check is expensive. Internally it reruns the transform on
   --  the output to return the input in reverse order. Also, this cannot
   --  be left on as a postcondition, because for large arrays, the 'Old
   --  generates a storage error, but it is useful for checking output for
   --  arrays whose length is 2**n or smaller, where N is 15 or thereabouts.

private

   --  This is equivalent to Array_Length, except it is a non-modular type
   --  and is only used internally.

   pragma Assert (Is_A_Power_Of_Two (Min_Sequential_Length));
   --  Invariant for Min_Sequential_Length

   pragma Inline (Is_A_Power_Of_Two);

end FFT_Work_Seeking;
