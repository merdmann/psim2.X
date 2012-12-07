----------------------------------------------------------------------------------------------------------
--  Based on an implementation of the Cooley-Tukey Fast Fourier transform
--  program, four1, from "Numerical Recipes in C" (Cambridge Univ. Press)
--  by W.H. Press, S.A. Teukolsky, W.T. Vetterling, and B.P. Flannery
--
--  This source is further based on implementation of the Danielson Lanczos
--  approach which implements the Cooley-Tukey algorithm recursively as
--  presented in an article from Dr. Dobb's entitled;
--  A Simple and Efficient FFT Implementation in C++, Part I
--  by Vlodymyr Myrnyy, May 10, 2007
--  http://drdobbs.com/cpp/199500857

------------------------------------------------------------------------------
--
--                 Paraffin - Parallelism Generics for Ada
--
--       F F T _ U N O P T I M I Z E D _ S E Q U E N T I A L
--
--                                B o d y
--
--                  Copyright (C) 2011, Bradley J. Moore
--
--  Paraffin is free software;  you can  redistribute it  and/or modify it
--  under  terms of the  GNU General Public License  as  published  by the
--  Free Software  Foundation;  either version 2,  or (at your option) any
--  later  version.  Paraffin is  distributed in the hope that it  will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU
--  General Public License for  more details.  You should have  received a
--  copy of the GNU General Public License distributed with Paraffin;  see
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.
--
--  As a  special exception, if other files  instantiate generics from
--  this unit,  or you link this  unit with other files  to produce an
--  executable,  this unit  does  not by  itself  cause the  resulting
--  executable to be covered by  the GNU General Public License.  This
--  exception does  not however invalidate  any other reasons  why the
--  executable file might be covered by the GNU Public License.
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Elementary_Functions; use Ada.Numerics;

package body FFT_Unoptimized_Sequential is

   procedure FFT_Internal
     (Data : in out Array_Type;
      Forward : Boolean := True);

   function Correct_Results
     (Input, Output : Array_Type;
      Forward : Boolean) return Boolean
   is
      type Array_Access is access Array_Type;
      pragma Warnings (Off, "*could be declared constant*");
      Check_Results : Array_Access := new Array_Type'(Output);
      pragma Warnings (On, "*could be declared constant*");
   begin

      FFT_Internal (Check_Results.all, not Forward);

      --  Check Results

      if Element_Type'Rounding
        (Check_Results (Check_Results'First) * 1000.0) / 1000.0 /=
        Element_Type'Rounding
          (Input (Input'First) * 1000.0) / 1000.0 then
         return False;
      end if;

      if Element_Type'Rounding
        (Check_Results (Check_Results'First + 1) * 1000.0) / 1000.0 /=
        Element_Type'Rounding
          (Input (Input'First + 1) * 1000.0) / 1000.0 then
         return False;
      end if;

      for I in 1 .. Output'Length / 2 - 1 loop
         if Element_Type'Rounding
           (Check_Results
              (Check_Results'Last - 1 - (Index_Type (I) - 1) * 2) * 1000.0)
           / 1000.0 /=
           Element_Type'Rounding (Input (Index_Type (I) * 2) * 1000.0)
           / 1000.0 then
            return False;
         end if;

         if Element_Type'Rounding
           (Check_Results
              (Check_Results'Last - (Index_Type (I) - 1) * 2) * 1000.0)
           / 1000.0 /=
           Element_Type'Rounding
             (Input (Index_Type (I) * 2 + 1) * 1000.0) / 1000.0 then
            return False;
         end if;

      end loop;

      return True;
   end Correct_Results;

   -----------------------------------------------------------

   procedure FFT_Internal
     (Data : in out Array_Type;
      Forward : Boolean := True)
   is

      subtype Count_Type is Index_Type'Base;

      N : constant Count_Type := Data'Length / 2;

      package Elementary_Functions is new
        Ada.Numerics.Generic_Elementary_Functions (Float_Type => Element_Type);
      use Elementary_Functions;

      procedure Danielson_Lanczos (Offset : Index_Type; N : Count_Type) is
         Wtemp, Tempr, Tempi, Wr, Wi, Wpr, Wpi : Element_Type;
         I : Index_Type := 0;
      begin
         if N = 1 then
            return;
         end if;

         Danielson_Lanczos (Offset, N / 2);
         Danielson_Lanczos (Offset + N, N / 2);

         Wtemp := Sin (Pi / Element_Type (N));
         Wpr   := -2.0 * Wtemp * Wtemp;
         Wpi   := -Sin (2.0 * Pi / Element_Type (N));
         Wr    := 1.0;
         Wi    := 0.0;

         while I < N loop

            Tempr := Data (Offset + I + N) * Wr -
                       Data (Offset + I + N + 1) * Wi;
            Tempi := Data (Offset + I + N) * Wi +
                       Data (Offset + I + N + 1) * Wr;

            Data (Offset + I + N)     := Data (Offset + I) - Tempr;
            Data (Offset + I + N + 1) := Data (Offset + I + 1) - Tempi;
            Data (Offset + I)         := Data (Offset + I) + Tempr;
            Data (Offset + I + 1)     := Data (Offset + I + 1) + Tempi;

            Wtemp := Wr;
            Wr    := Wr + Wr * Wpr - Wi * Wpi;
            Wi    := Wi + Wi * Wpr + Wtemp * Wpi;

            I := I + 2;

         end loop;

      end Danielson_Lanczos;

      procedure Reverse_Binary_Reindex is

         procedure Swap (L, R : Index_Type);
         pragma Inline (Swap);

         procedure Swap (L, R : Index_Type) is
            Temp : constant Element_Type := Data (L);
         begin
            Data (L) := Data (R);
            Data (R) := Temp;
         end Swap;

         M : Count_Type;
         I : Index_Type := 1;
         J : Index_Type := 1;

      begin -- Reverse_Binary_Index

         while I < Data'Last loop

            if J > I then
               Swap (J - 1, I - 1);
               Swap (J, I);
            end if;

            M := Data'Length / 2;

            while M >= 2 and then J > M loop
               J := J - M;
               M := M / 2;
            end loop;

            J := J + M;
            I := I + 2;

         end loop;

      end Reverse_Binary_Reindex;

   begin --  FFT
      Reverse_Binary_Reindex;

      Danielson_Lanczos (Data'First, Data'Length / 2);

      if Forward then
         --  Scaling for forward transform
         for I in 0 .. N - 1 loop
            Data (I * 2)     := Data (I * 2) / Element_Type (N);
            Data (I * 2 + 1) := Data (I * 2 + 1) / Element_Type (N);
         end loop;
      end if;

   end FFT_Internal;

   procedure FFT (Data : in out Array_Type; Forward : Boolean := True)
     renames FFT_Internal;

   --------------------------------------------------------------------------

   function Is_A_Power_Of_Two (Value : Array_Length) return Boolean is
   begin
      return ((Value and (Value - 1)) = 0);
   end Is_A_Power_Of_Two;

end FFT_Unoptimized_Sequential;
