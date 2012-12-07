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
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                     F F T _ S E Q U E N T I A L                        --
--                                                                          --
--                                B o d y                                   --
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
with Ada.Numerics.Generic_Elementary_Functions; use Ada.Numerics;

package body FFT_Sequential is

   procedure FFT_Internal
     (Data : in out Array_Type;
      Forward : Boolean := True);

   --  Do the trigonometry at compile time, instead of run time
   --  sin x = x - x3/3! + x5/5! - x7/7! + ...,

   --  N = 8
   N3 : constant := Pi / 2**3;
   Wi_N3 : constant := 2.0 * N3;
   Wtemp_N3 : constant := N3 - (N3**3)/(3.0 * 2.0) +
     (N3**5)/(5.0*4.0*3.0*2.0) - (N3**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N3**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N3**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N3 : constant := -2.0 * (Wtemp_N3 ** 2);
   Wpi_N3 : constant := -1.0 *
     (Wi_N3 - (Wi_N3**3)/(3.0 * 2.0) + (Wi_N3**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N3**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N3**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N3**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 16
   N4 : constant := Pi / 2**4;
   Wi_N4 : constant := 2.0 * N4;
   Wtemp_N4 : constant := N4 - (N4**3)/(3.0 * 2.0) +
     (N4**5)/(5.0*4.0*3.0*2.0) - (N4**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N4**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N4**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N4 : constant := -2.0 * (Wtemp_N4 ** 2);
   Wpi_N4 : constant := -1.0 *
     (Wi_N4 - (Wi_N4**3)/(3.0 * 2.0) + (Wi_N4**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N4**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N4**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N4**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 32
   N5 : constant := Pi / 2**5;
   Wi_N5 : constant := 2.0 * N5;
   Wtemp_N5 : constant := N5 - (N5**3)/(3.0 * 2.0) +
     (N5**5)/(5.0*4.0*3.0*2.0) - (N5**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N5**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N5**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N5 : constant := -2.0 * (Wtemp_N5 ** 2);
   Wpi_N5 : constant := -1.0 *
     (Wi_N5 - (Wi_N5**3)/(3.0 * 2.0) + (Wi_N5**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N5**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N5**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N5**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 64
   N6 : constant := Pi / 2**6;
   Wi_N6 : constant := 2.0 * N6;
   Wtemp_N6 : constant := N6 - (N6**3)/(3.0 * 2.0) +
     (N6**5)/(5.0*4.0*3.0*2.0) - (N6**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N6**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N6**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N6 : constant := -2.0 * (Wtemp_N6 ** 2);
   Wpi_N6 : constant := -1.0 *
     (Wi_N6 - (Wi_N6**3)/(3.0 * 2.0) + (Wi_N6**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N6**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N6**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N6**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 128
   N7 : constant := Pi / 2**7;
   Wi_N7 : constant := 2.0 * N7;
   Wtemp_N7 : constant := N7 - (N7**3)/(3.0 * 2.0) +
     (N7**5)/(5.0*4.0*3.0*2.0) - (N7**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N7**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N7**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N7 : constant := -2.0 * (Wtemp_N7 ** 2);
   Wpi_N7 : constant := -1.0 *
     (Wi_N7 - (Wi_N7**3)/(3.0 * 2.0) + (Wi_N7**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N7**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N7**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N7**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 256
   N8 : constant := Pi / 2**8;
   Wi_N8 : constant := 2.0 * N8;
   Wtemp_N8 : constant := N8 - (N8**3)/(3.0 * 2.0) +
     (N8**5)/(5.0*4.0*3.0*2.0) - (N8**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N8**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N8**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N8 : constant := -2.0 * (Wtemp_N8 ** 2);
   Wpi_N8 : constant := -1.0 *
     (Wi_N8 - (Wi_N8**3)/(3.0 * 2.0) + (Wi_N8**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N8**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N8**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N8**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

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

      --  Check Results

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

      generic
         N_Value  : Count_Type;
         Initial_Wtemp : Element_Type;
         Wpr : Element_Type;
         Wpi : Element_Type;
         with procedure DL (Offset : Index_Type);
      procedure DL_N (Offset  : Index_Type);
      pragma Inline (DL_N);

      procedure DL_N1 (Offset  : Index_Type);
      procedure DL_N2 (Offset  : Index_Type);
      pragma Inline (DL_N1, DL_N2);

      -----------------------------------------------------------

      procedure DL_N (Offset : Index_Type) is
         Wtemp, Tempr, Tempi, Wr, Wi : Element_Type;
         I  : Index_Type := 0;
      begin

         DL (Offset);
         DL (Offset + N_Value);

         Wtemp := Initial_Wtemp;
         Wr := 1.0;
         Wi := 0.0;

         while I < N_Value loop

            Tempr  := Data (Offset + I + N_Value) * Wr -
              Data (Offset + I + N_Value + 1) * Wi;
            Tempi  := Data (Offset + I + N_Value) * Wi +
              Data (Offset + I + N_Value + 1) * Wr;
            Data (Offset + I + N_Value) := Data (Offset + I) - Tempr;
            Data (Offset + I + N_Value + 1) := Data (Offset + I + 1) - Tempi;
            Data (Offset + I)           := Data (Offset + I) + Tempr;
            Data (Offset + I + 1)       := Data (Offset + I + 1) + Tempi;

            Wtemp := Wr;
            Wr    := Wr + Wr * Wpr - Wi * Wpi;
            Wi    := Wi + Wi * Wpr + Wtemp * Wpi;

            I := I + 2;

         end loop;
      end DL_N;

      -----------------------------------------------------------

      procedure DL_N1 (Offset  : Index_Type)
      is
         Tr : constant Element_Type := Data (Offset + 2);
         Ti : constant Element_Type := Data (Offset + 3);
      begin
         Data (Offset + 2) := Data (Offset) - Tr;
         Data (Offset + 3) := Data (Offset + 1) - Ti;
         Data (Offset) := Data (Offset) + Tr;
         Data (Offset + 1) := Data (Offset + 1) + Ti;
      end DL_N1;

      -----------------------------------------------------------

      procedure DL_N2 (Offset  : Index_Type)
      is
         Tr : Element_Type := Data (Offset + 2);
         Ti : Element_Type := Data (Offset + 3);
      begin
         Data (Offset + 2) := Data (Offset) - Tr;
         Data (Offset + 3) := Data (Offset + 1) - Ti;
         Data (Offset) := Data (Offset) + Tr;
         Data (Offset + 1) := Data (Offset + 1) + Ti;

         Tr := Data (Offset + 6);
         Ti := Data (Offset + 7);
         Data (Offset + 6) := Data (Offset + 5) - Ti;
         Data (Offset + 7) := Tr - Data (Offset + 4);
         Data (Offset + 4) := Data (Offset + 4) + Tr;
         Data (Offset + 5) := Data (Offset + 5) + Ti;

         Tr := Data (Offset + 4);
         Ti := Data (Offset + 5);
         Data (Offset + 4) := Data (Offset) - Tr;
         Data (Offset + 5) := Data (Offset + 1) - Ti;
         Data (Offset) := Data (Offset) + Tr;
         Data (Offset + 1) := Data (Offset + 1) + Ti;

         Tr := Data (Offset + 6);
         Ti := Data (Offset + 7);
         Data (Offset + 6) := Data (Offset + 2) - Tr;
         Data (Offset + 7) := Data (Offset + 3) - Ti;
         Data (Offset + 2) := Data (Offset + 2) + Tr;
         Data (Offset + 3) := Data (Offset + 3) + Ti;

      end DL_N2;

      procedure DL_N3 is new
        DL_N
          (N_Value       => 2**3,
           Initial_Wtemp => Wtemp_N3,
           Wpr           => Wpr_N3,
           Wpi           => Wpi_N3,
           DL            => DL_N2);

      procedure DL_N4 is new
        DL_N
          (N_Value       => 2**4,
           Initial_Wtemp => Wtemp_N4,
           Wpr           => Wpr_N4,
           Wpi           => Wpi_N4,
           DL            => DL_N3);

      procedure DL_N5 is new
        DL_N
          (N_Value       => 2**5,
           Initial_Wtemp => Wtemp_N5,
           Wpr           => Wpr_N5,
           Wpi           => Wpi_N5,
           DL            => DL_N4);

      procedure DL_N6 is new
        DL_N
          (N_Value       => 2**6,
           Initial_Wtemp => Wtemp_N6,
           Wpr           => Wpr_N6,
           Wpi           => Wpi_N6,
           DL            => DL_N5);

      procedure DL_N7 is new
        DL_N
          (N_Value       => 2**7,
           Initial_Wtemp => Wtemp_N7,
           Wpr           => Wpr_N7,
           Wpi           => Wpi_N7,
           DL            => DL_N6);

      procedure DL_N8 is new
        DL_N
          (N_Value       => 2**8,
           Initial_Wtemp => Wtemp_N8,
           Wpr           => Wpr_N8,
           Wpi           => Wpi_N8,
           DL            => DL_N7);

      -----------------------------------------------------------

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

      -----------------------------------------------------------

      procedure Sequential_Danielson_Lanczos
        (Offset : Index_Type;
         N : Count_Type)
      is

         Wtemp, Tempr, Tempi, Wr, Wi, Wpr, Wpi : Element_Type;
         I : Index_Type := 0;

      begin  --  Sequential_Danielson_Lanczois
         pragma Warnings (Off, "*condition is always*");
         if Min_Sequential_Length = 2**0 then
            if N = 2**0 then
               return;
            end if;
         elsif Min_Sequential_Length = 2**1 then
            if N = 2**1 then
               DL_N1 (Offset);
               return;
            end if;
         elsif Min_Sequential_Length = 2**2 then
            if N = 2**2 then
               DL_N2 (Offset);
               return;
            end if;
         elsif Min_Sequential_Length = 2**3 then
            if N = 2**3 then
               DL_N3 (Offset);
               return;
            end if;
         elsif Min_Sequential_Length = 2**4 then
            if N = 2**4 then
               DL_N4 (Offset);
               return;
            end if;
         elsif Min_Sequential_Length = 2**5 then
            if N = 2**5 then
               DL_N5 (Offset);
               return;
            end if;
         elsif Min_Sequential_Length = 2**6 then
            if N = 2**6 then
               DL_N6 (Offset);
               return;
            end if;
         elsif Min_Sequential_Length = 2**7 then
            if N = 2**7 then
               DL_N7 (Offset);
               return;
            end if;
         else
            if N = 2**8 then
               DL_N8 (Offset);
               return;
            end if;
         end if;
         pragma Warnings (On, "*condition is always*");

         Sequential_Danielson_Lanczos (Offset, N / 2);
         Sequential_Danielson_Lanczos (Offset + N, N / 2);

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

      end Sequential_Danielson_Lanczos;

   begin --  FFT_Sequential

      Reverse_Binary_Reindex;

      Sequential_Danielson_Lanczos (Data'First, Data'Length / 2);

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

end FFT_Sequential;
