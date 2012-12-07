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
--                   F F T _ W O R K _ S H A R I N G
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
with Parallel.Iteration.Work_Seeking;
with Parallel.Recursion.Work_Sharing;
with Parallel.Synchronous_Barriers; use Parallel.Synchronous_Barriers;
use Parallel;

with Ada.Numerics.Generic_Elementary_Functions;
use Ada.Numerics;

package body FFT_Work_Sharing is

   subtype Count_Type is Index_Type'Base;

   procedure FFT_Internal
     (Data : in out Array_Type;
      Log2_N : Natural;
      Forward : Boolean := True;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Parallel.Default_Worker_Storage_Size);

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

   --  N = 512
   N9 : constant := Pi / 2**9;
   Wi_N9 : constant := 2.0 * N9;
   Wtemp_N9 : constant := N9 - (N9**3)/(3.0 * 2.0) +
     (N9**5)/(5.0*4.0*3.0*2.0) - (N9**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N9**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N9**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N9 : constant := -2.0 * (Wtemp_N9 ** 2);
   Wpi_N9 : constant := -1.0 *
     (Wi_N9 - (Wi_N9**3)/(3.0 * 2.0) + (Wi_N9**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N9**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N9**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N9**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 1024
   N10 : constant := Pi / 2**10;
   Wi_N10 : constant := 2.0 * N10;
   Wtemp_N10 : constant := N10 - (N10**3)/(3.0 * 2.0) +
     (N10**5)/(5.0*4.0*3.0*2.0) - (N10**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N10**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N10**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N10 : constant := -2.0 * (Wtemp_N10 ** 2);
   Wpi_N10 : constant := -1.0 *
     (Wi_N10 - (Wi_N10**3)/(3.0 * 2.0) + (Wi_N10**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N10**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N10**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N10**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 2048
   N11 : constant := Pi / 2**11;
   Wi_N11 : constant := 2.0 * N11;
   Wtemp_N11 : constant := N11 - (N11**3)/(3.0 * 2.0) +
     (N11**5)/(5.0*4.0*3.0*2.0) - (N11**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N11**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N11**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N11 : constant := -2.0 * (Wtemp_N11 ** 2);
   Wpi_N11 : constant := -1.0 *
     (Wi_N11 - (Wi_N11**3)/(3.0 * 2.0) + (Wi_N11**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N11**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N11**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N11**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 4096
   N12 : constant := Pi / 2**12;
   Wi_N12 : constant := 2.0 * N12;
   Wtemp_N12 : constant := N12 - (N12**3)/(3.0 * 2.0) +
     (N12**5)/(5.0*4.0*3.0*2.0) - (N12**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N12**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N12**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N12 : constant := -2.0 * (Wtemp_N12 ** 2);
   Wpi_N12 : constant := -1.0 *
     (Wi_N12 - (Wi_N12**3)/(3.0 * 2.0) + (Wi_N12**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N12**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N12**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N12**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 8192
   N13 : constant := Pi / 2**13;
   Wi_N13 : constant := 2.0 * N13;
   Wtemp_N13 : constant := N13 - (N13**3)/(3.0 * 2.0) +
     (N13**5)/(5.0*4.0*3.0*2.0) - (N13**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N13**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N13**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N13 : constant := -2.0 * (Wtemp_N13 ** 2);
   Wpi_N13 : constant := -1.0 *
     (Wi_N13 - (Wi_N13**3)/(3.0 * 2.0) + (Wi_N13**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N13**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N13**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N13**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 16384
   N14 : constant := Pi / 2**14;
   Wi_N14 : constant := 2.0 * N14;
   Wtemp_N14 : constant := N14 - (N14**3)/(3.0 * 2.0) +
     (N14**5)/(5.0*4.0*3.0*2.0) - (N14**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N14**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N14**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N14 : constant := -2.0 * (Wtemp_N14 ** 2);
   Wpi_N14 : constant := -1.0 *
     (Wi_N14 - (Wi_N14**3)/(3.0 * 2.0) + (Wi_N14**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N14**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N14**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N14**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 32767
   N15 : constant := Pi / 2**15;
   Wi_N15 : constant := 2.0 * N15;
   Wtemp_N15 : constant := N15 - (N15**3)/(3.0 * 2.0) +
     (N15**5)/(5.0*4.0*3.0*2.0) - (N15**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N15**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N15**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N15 : constant := -2.0 * (Wtemp_N15 ** 2);
   Wpi_N15 : constant := -1.0 *
     (Wi_N15 - (Wi_N15**3)/(3.0 * 2.0) + (Wi_N15**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N15**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N15**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N15**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 65536
   N16 : constant := Pi / 2**16;
   Wi_N16 : constant := 2.0 * N16;
   Wtemp_N16 : constant := N16 - (N16**3)/(3.0 * 2.0) +
     (N16**5)/(5.0*4.0*3.0*2.0) - (N16**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N16**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N16**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N16 : constant := -2.0 * (Wtemp_N16 ** 2);
   Wpi_N16 : constant := -1.0 *
     (Wi_N16 - (Wi_N16**3)/(3.0 * 2.0) + (Wi_N16**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N16**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N16**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N16**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 131072
   N17 : constant := Pi / 2**17;
   Wi_N17 : constant := 2.0 * N17;
   Wtemp_N17 : constant := N17 - (N17**3)/(3.0 * 2.0) +
     (N17**5)/(5.0*4.0*3.0*2.0) - (N17**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N17**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N17**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N17 : constant := -2.0 * (Wtemp_N17 ** 2);
   Wpi_N17 : constant := -1.0 *
     (Wi_N17 - (Wi_N17**3)/(3.0 * 2.0) + (Wi_N17**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N17**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N17**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N17**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 262144
   N18 : constant := Pi / 2**18;
   Wi_N18 : constant := 2.0 * N18;
   Wtemp_N18 : constant := N18 - (N18**3)/(3.0 * 2.0) +
     (N18**5)/(5.0*4.0*3.0*2.0) - (N18**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N18**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N18**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N18 : constant := -2.0 * (Wtemp_N18 ** 2);
   Wpi_N18 : constant := -1.0 *
     (Wi_N18 - (Wi_N18**3)/(3.0 * 2.0) + (Wi_N18**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N18**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N18**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N18**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 524288
   N19 : constant := Pi / 2**19;
   Wi_N19 : constant := 2.0 * N19;
   Wtemp_N19 : constant := N19 - (N19**3)/(3.0 * 2.0) +
     (N19**5)/(5.0*4.0*3.0*2.0) - (N19**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N19**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N19**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N19 : constant := -2.0 * (Wtemp_N19 ** 2);
   Wpi_N19 : constant := -1.0 *
     (Wi_N19 - (Wi_N19**3)/(3.0 * 2.0) + (Wi_N19**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N19**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N19**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N19**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 1048576
   N20 : constant := Pi / 2**20;
   Wi_N20 : constant := 2.0 * N20;
   Wtemp_N20 : constant := N20 - (N20**3)/(3.0 * 2.0) +
     (N20**5)/(5.0*4.0*3.0*2.0) - (N20**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N20**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N20**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N20 : constant := -2.0 * (Wtemp_N20 ** 2);
   Wpi_N20 : constant := -1.0 *
     (Wi_N20 - (Wi_N20**3)/(3.0 * 2.0) + (Wi_N20**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N20**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N20**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N20**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 2097152
   N21 : constant := Pi / 2**21;
   Wi_N21 : constant := 2.0 * N21;
   Wtemp_N21 : constant := N21 - (N21**3)/(3.0 * 2.0) +
     (N21**5)/(5.0*4.0*3.0*2.0) - (N21**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N21**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N21**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N21 : constant := -2.0 * (Wtemp_N21 ** 2);
   Wpi_N21 : constant := -1.0 *
     (Wi_N21 - (Wi_N21**3)/(3.0 * 2.0) + (Wi_N21**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N21**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N21**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N21**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 4194304
   N22 : constant := Pi / 2**22;
   Wi_N22 : constant := 2.0 * N22;
   Wtemp_N22 : constant := N22 - (N22**3)/(3.0 * 2.0) +
     (N22**5)/(5.0*4.0*3.0*2.0) - (N22**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N22**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N22**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N22 : constant := -2.0 * (Wtemp_N22 ** 2);
   Wpi_N22 : constant := -1.0 *
     (Wi_N22 - (Wi_N22**3)/(3.0 * 2.0) + (Wi_N22**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N22**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N22**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N22**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 8388608
   N23 : constant := Pi / 2**23;
   Wi_N23 : constant := 2.0 * N23;
   Wtemp_N23 : constant := N23 - (N23**3)/(3.0 * 2.0) +
     (N23**5)/(5.0*4.0*3.0*2.0) - (N23**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N23**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N23**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N23 : constant := -2.0 * (Wtemp_N23 ** 2);
   Wpi_N23 : constant := -1.0 *
     (Wi_N23 - (Wi_N23**3)/(3.0 * 2.0) + (Wi_N23**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N23**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N23**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N23**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   --  N = 16777216
   N24 : constant := Pi / 2**24;
   Wi_N24 : constant := 2.0 * N24;
   Wtemp_N24 : constant := N24 - (N24**3)/(3.0 * 2.0) +
     (N24**5)/(5.0*4.0*3.0*2.0) - (N24**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (N24**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
   (N24**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0);
   Wpr_N24 : constant := -2.0 * (Wtemp_N24 ** 2);
   Wpi_N24 : constant := -1.0 *
     (Wi_N24 - (Wi_N24**3)/(3.0 * 2.0) + (Wi_N24**5)/(5.0*4.0*3.0*2.0) -
      (Wi_N24**7)/(7.0*6.0*5.0*4.0*3.0*2.0) +
     (Wi_N24**9)/(9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0) -
      (Wi_N24**11)/(11.0*10.0*9.0*8.0*7.0*6.0*5.0*4.0*3.0*2.0));

   type FFT_Constants is
      record
         Initial_Wtemp : Element_Type;
         Wpr : Element_Type;
         Wpi : Element_Type;
      end record;

   FFT_Constants_Array : constant array (Natural range 3 .. 24)
     of FFT_Constants :=
     (3 => (Wtemp_N3, Wpr_N3, Wpi_N3),
      4 => (Wtemp_N4, Wpr_N4, Wpi_N4),
      5 => (Wtemp_N5, Wpr_N5, Wpi_N5),
      6 => (Wtemp_N6, Wpr_N6, Wpi_N6),
      7 => (Wtemp_N7, Wpr_N7, Wpi_N7),
      8 => (Wtemp_N8, Wpr_N8, Wpi_N8),
      9 => (Wtemp_N9, Wpr_N9, Wpi_N9),
      10 => (Wtemp_N10, Wpr_N10, Wpi_N10),
      11 => (Wtemp_N11, Wpr_N11, Wpi_N11),
      12 => (Wtemp_N12, Wpr_N12, Wpi_N12),
      13 => (Wtemp_N13, Wpr_N13, Wpi_N13),
      14 => (Wtemp_N14, Wpr_N14, Wpi_N14),
      15 => (Wtemp_N15, Wpr_N15, Wpi_N15),
      16 => (Wtemp_N16, Wpr_N16, Wpi_N16),
      17 => (Wtemp_N17, Wpr_N17, Wpi_N17),
      18 => (Wtemp_N18, Wpr_N18, Wpi_N18),
      19 => (Wtemp_N19, Wpr_N19, Wpi_N19),
      20 => (Wtemp_N20, Wpr_N20, Wpi_N20),
      21 => (Wtemp_N21, Wpr_N21, Wpi_N21),
      22 => (Wtemp_N22, Wpr_N22, Wpi_N22),
      23 => (Wtemp_N23, Wpr_N23, Wpi_N23),
      24 => (Wtemp_N24, Wpr_N24, Wpi_N24));

   function Correct_Results
     (Input, Output : Array_Type;
      Log2_N : Natural;
      Forward : Boolean) return Boolean
   is
      type Array_Access is access Array_Type;
      pragma Warnings (Off, "*could be declared constant*");
      Check_Results : Array_Access := new Array_Type'(Output);
      pragma Warnings (On, "*could be declared constant*");
   begin

      FFT_Internal (Check_Results.all, Log2_N, not Forward);

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
      Log2_N : Natural;
      Forward : Boolean := True;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Parallel.Default_Worker_Storage_Size)
   is

      N : constant Count_Type := Data'Length / 2;

      type Work_Info_Type is record
         Offset  : Index_Type;
         N       : Count_Type;
         Log2_N : Natural;
         Barrier : access Synchronous_Barriers.Synchronous_Barrier := null;
      end record;

      package Recursive_FFT is new Parallel.Recursion.Work_Sharing
        (Work_Type => Work_Info_Type);

      Dispatcher : aliased Recursive_FFT.Recursion_Dispatcher_Access := null;

      package Elementary_Functions is new
        Ada.Numerics.Generic_Elementary_Functions (Float_Type => Element_Type);
      use Elementary_Functions;

      generic
         N  : Count_Type;
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
         DL (Offset + N);

         Wtemp := Initial_Wtemp;
         Wr := 1.0;
         Wi := 0.0;

         while I < N loop

            Tempr  := Data (Offset + I + N) * Wr -
              Data (Offset + I + N + 1) * Wi;
            Tempi  := Data (Offset + I + N) * Wi +
              Data (Offset + I + N + 1) * Wr;
            Data (Offset + I + N)     := Data (Offset + I) - Tempr;
            Data (Offset + I + N + 1) := Data (Offset + I + 1) - Tempi;
            Data (Offset + I)            := Data (Offset + I) + Tempr;
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
          (N             => 2**3,
           Initial_Wtemp => Wtemp_N3,
           Wpr           => Wpr_N3,
           Wpi           => Wpi_N3,
           DL            => DL_N2);

      procedure DL_N4 is new
        DL_N
          (N             => 2**4,
           Initial_Wtemp => Wtemp_N4,
           Wpr           => Wpr_N4,
           Wpi           => Wpi_N4,
           DL            => DL_N3);

      procedure DL_N5 is new
        DL_N
          (N             => 2**5,
           Initial_Wtemp => Wtemp_N5,
           Wpr           => Wpr_N5,
           Wpi           => Wpi_N5,
           DL            => DL_N4);

      procedure DL_N6 is new
        DL_N
          (N             => 2**6,
           Initial_Wtemp => Wtemp_N6,
           Wpr           => Wpr_N6,
           Wpi           => Wpi_N6,
           DL            => DL_N5);

      procedure DL_N7 is new
        DL_N
          (N             => 2**7,
           Initial_Wtemp => Wtemp_N7,
           Wpr           => Wpr_N7,
           Wpi           => Wpi_N7,
           DL            => DL_N6);

      procedure DL_N8 is new
        DL_N
          (N             => 2**8,
           Initial_Wtemp => Wtemp_N8,
           Wpr           => Wpr_N8,
           Wpi           => Wpi_N8,
           DL            => DL_N7);

      -----------------------------------------------------------

      procedure Parallel_Danielson_Lanczos
        (Info : Work_Info_Type;
         Subcontractors : Worker_Count_Type)
      is
         N      : Count_Type renames Info.N;
         Log2_N : Natural renames Info.Log2_N;
         Offset  : Index_Type renames Info.Offset;
         Wtemp, Tempr, Tempi, Wr, Wi, Wpr, Wpi : Element_Type;
         I  : Index_Type := 0;
         Barrier : aliased Synchronous_Barrier (Number_Waiting => 2);

      begin
         --  Note we do not need to check for ending the recursion, since that
         --  will happen in the sequential version of the code

         Dispatcher.Recurse
           ((Offset, N / 2, Log2_N - 1, Barrier'Unchecked_Access),
            Split     => 1,
            Of_Splits => 2,
            Subcontractors => Subcontractors);

         Dispatcher.Recurse
           ((Offset + N, N / 2, Log2_N - 1, Barrier'Unchecked_Access),
            Split     => 2,
            Of_Splits => 2,
            Subcontractors => Subcontractors);

         Wtemp := FFT_Constants_Array (Log2_N).Initial_Wtemp;
         Wpr   := FFT_Constants_Array (Log2_N).Wpr;
         Wpi   := FFT_Constants_Array (Log2_N).Wpi;
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

         --  If both sides of the recursion were executed in parallel, we need
         --  a barrier to ensure that both workers have finished their
         --  recursion before returning to the caller, since the code in the
         --  while loop above assumes that both recursive calls have completed
         --  (since it iterates over the full range processed by both sides of
         --  the recursion)
         --  Sequential execution (and the top level call) passes null for the
         --  barrier, since there is no need to synchronize workers.

         if Info.Barrier /= null then
            Synchronous_Barriers.Wait_For_Release (Info.Barrier.all);
         end if;

      end Parallel_Danielson_Lanczos;

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

      procedure Scale_Results is
         Parallel_Scaling : constant Boolean := True;
      begin
         if not Parallel_Scaling then  -- Dead code gets eliminated
            for I in 0 .. N - 1 loop
               Data (I * 2)     := Data (I * 2) / Element_Type (N);
               Data (I * 2 + 1) := Data (I * 2 + 1) / Element_Type (N);
            end loop;
         else
            --  Some slight performance gains made by parallelizing the
            --  above loop. Most performance gains however are by
            --  parallellizing the Daniel_Lanczos routine.
            declare
               procedure Parallel_Iterative_Scaling is new
                 Parallel.Iteration.Work_Seeking
                   (Iteration_Index_Type => Index_Type);

               procedure Iterate
                 (Start, Finish : Index_Type) is
               begin
                  for I in Start .. Finish loop
                     Data (I * 2)     := Data (I * 2) / Element_Type (N);
                     Data (I * 2 + 1) := Data (I * 2 + 1) / Element_Type (N);
                  end loop;
               end Iterate;
            begin
               Parallel_Iterative_Scaling
                 (From => 0,
                  To => N - 1,
                  Worker_Count => Parallel.Default_Worker_Count,
              --  Minimum_Seek => Select_Minimum_Seek,
              --  Storage_Size   => Parallel.Default_Worker_Storage_Size,
              --  Work_Budget   => Parallel.Unlimited_Work_Budget,
              --  Priority        => Dynamic_Priorities.Get_Priority,
              --  Use_Affinity     => False,
                  Process  => Iterate'Access);
            end;
         end if;
      end Scale_Results;

      -----------------------------------------------------------

      procedure Sequential_Danielson_Lanczos
        (Offset : Index_Type;
         N : Count_Type;
         Log2_N : Natural)
      is
         Wtemp, Tempr, Tempi, Wr, Wi, Wpr, Wpi : Element_Type;
         I  : Index_Type := 0;
      begin

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
         elsif Min_Sequential_Length = 2**8 then
            if N = 2**8 then
               DL_N8 (Offset);
               return;
            end if;
         end if;
         pragma Warnings (On, "*condition is always*");

         Sequential_Danielson_Lanczos (Offset, N / 2, Log2_N - 1);
         Sequential_Danielson_Lanczos (Offset + N, N / 2, Log2_N - 1);

         Wtemp := FFT_Constants_Array (Log2_N).Initial_Wtemp;
         Wpr   := FFT_Constants_Array (Log2_N).Wpr;
         Wpi   := FFT_Constants_Array (Log2_N).Wpi;
         Wr    := 1.0;
         Wi    := 0.0;

         while I < N loop

            Tempr  := Data (Offset + I + N) * Wr -
                        Data (Offset + I + N + 1) * Wi;
            Tempi  := Data (Offset + I + N) * Wi +
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

      ------------------------------------------------------------------------

      procedure Transitional_Danielson_Lanczos (Info : Work_Info_Type) is
         N       : Count_Type renames Info.N;
         Log2_N : Natural renames Info.Log2_N;
         Offset   : Index_Type renames Info.Offset;

         Wtemp, Tempr, Tempi, Wr, Wi, Wpr, Wpi : Element_Type;
         I  : Index_Type := 0;

      begin

         Sequential_Danielson_Lanczos (Offset, N / 2, Log2_N - 1);
         Sequential_Danielson_Lanczos (Offset + N, N / 2, Log2_N - 1);

         Wtemp := FFT_Constants_Array (Log2_N).Initial_Wtemp;
         Wpr   := FFT_Constants_Array (Log2_N).Wpr;
         Wpi   := FFT_Constants_Array (Log2_N).Wpi;
         Wr    := 1.0;
         Wi    := 0.0;

         while I < N loop

            Tempr  := Data (Offset + I + N) * Wr -
                        Data (Offset + I + N + 1) * Wi;
            Tempi  := Data (Offset + I + N) * Wi +
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

         --  If both sides of the recursion were executed in parallel, we need
         --  a barrier to ensure that both workers have finished their
         --  recursion before returning to the caller, since the code in the
         --  while loop above assumes that both recursive calls have completed
         --  (since it iterates over the full range processed by both sides of
         --  the recursion)
         --  Sequential execution (and the top level call) passes null for the
         --  barrier, since there is no need to synchronize workers.
         if Info.Barrier /= null then
            Synchronous_Barriers.Wait_For_Release (Info.Barrier.all);
         end if;
      end Transitional_Danielson_Lanczos;

   begin --  FFT_Work_Sharing
      Reverse_Binary_Reindex;

      Recursive_FFT.Execute
        (Item                => (Data'First, Data'Length / 2, Log2_N, null),
         Dispatcher          => Dispatcher'Access,
         Parallel_Process    => Parallel_Danielson_Lanczos'Access,
         Sequential_Process => Transitional_Danielson_Lanczos'Access,
         Storage_Size        => Storage_Size
         --  Priority          => Dynamic_Priorities.Get_Priority;
         --  Worker_Count    => Default_Worker_Count;
         --  Use_Affinity       => False
        );

      if Forward then
            --  Scaling for forward transform
            Scale_Results;
      end if;
   end FFT_Internal;

   procedure FFT
     (Data : in out Array_Type;
      Log2_N : Natural;
      Forward : Boolean := True;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Parallel.Default_Worker_Storage_Size)
      renames FFT_Internal;

   --------------------------------------------------------------------------

   function Is_A_Power_Of_Two (Value : Array_Length) return Boolean is
   begin
      return ((Value and (Value - 1)) = 0);
   end Is_A_Power_Of_Two;

end FFT_Work_Sharing;
