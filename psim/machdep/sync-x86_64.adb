with System.Machine_Code;                    use System.Machine_Code;
with Ada.Characters.Latin_1;                 use Ada.Characters.Latin_1;
use Ada;

package body Sync is

   NL : constant String := "" & LF & HT ;

   --------------------
   -- Safe_Increment --
   --------------------
   function Safe_Increment( Counter : in Address ) return Natural is
      Result : Natural := 0;
   begin
      Asm(
          "movl $1, %%eax" & LF & HT &
          "lock; xaddl %%eax,(%%rcx)",

          Outputs => Natural'Asm_Output( "=a", Result),
          Inputs => Address'Asm_Input( "c", Counter ),
          Volatile => True
         );
      return Result;
   end Safe_Increment;

   function Safe_Decrement( Counter : in Address ) return Natural is
      Result : Natural := 0;
   begin
      Asm(
          "movl $-1, %%eax" & LF & HT &
          "lock; xaddl %%eax, (%%rcx)",

          Outputs => Natural'Asm_Output( "=a", Result),
          Inputs => Address'Asm_Input( "c", Counter ),
          Volatile => True
         );
      return Result;
   end Safe_Decrement;

   ---------
   -- CAS --
   ---------
   function CAS ( Location : in Address; Expected : in Natural; Value : in Natural  ) return Boolean is
      Result : Natural := 0;
   begin
      Asm( "lock; cmpxchgl %%ebx, (%%rdx)" & NL &
            "  jz success" & NL &
            "  movl $0,%%eax" & NL &
            "  jmp exit" & NL &
            "success: movl $1,%%eax" & NL &
            "exit: nop",
        Inputs => ( Address'Asm_Input( "d", Location ),
                   Natural'Asm_Input( "a", Expected ),
                   Natural'Asm_Input( "b", Value )),
          Outputs => Natural'Asm_Output("=a", Result ),
          Volatile => true
         );
      return Result > 0 ;
   end CAS;

end Sync;



