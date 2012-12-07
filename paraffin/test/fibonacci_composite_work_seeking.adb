with Parallel.Recursion.Work_Seeking_Procedural_Reduction;
use Parallel;

with Parallel_Test_Harness;

--  function Fibonacci (Number : Natural) return Natural is
--  begin
--     if Number < 2 then
--        return Number;
--     else
--        return Fibonacci (Number - 2) + Fibonacci (Number - 1);
--     end if;
--  end Fibonacci;
--
--  pragma Parallel_Recursion
--    (Fibonacci, Two_Way_Recursive_Integer_Addition.Parallel_Recursion);

function Fibonacci_Composite_Work_Seeking (Value : Natural) return Natural
is
   Other_Workers : aliased Parallel.Work_Seeking_State;

   procedure Add (Left, Right : in out Natural) is
   begin
      Left := Left + Right;
   end Add;

   package Fibonacci_Recursion is new
     Parallel.Recursion.Work_Seeking_Procedural_Reduction
       (Work_Type => Natural,
        Result_Type => Natural,
        Reducer => Add,
        Identity_Value => 0);

   Dispatcher : aliased Fibonacci_Recursion.Recursion_Dispatcher_Access
     := null;

   procedure Parallel_Fibonacci
     (Number : Natural;
      Result : out Natural) is
   begin

      if Number < 2 then
         Result := Number;
      elsif Other_Workers.Seeking_Work and then
         Number > Parallel_Test_Harness.Fibonacci_Offer_Limit then
         declare
            Temp : Natural;
         begin
            Dispatcher.Recurse
             (Item => Number - 1,
              Result => Result);
            Parallel_Fibonacci (Number - 2, Temp);
            Result := Result + Temp;
         end;
      else
         declare
            Temp : Natural;
         begin
            Parallel_Fibonacci (Number - 2, Result);
            Parallel_Fibonacci (Number - 1, Temp);
            Result := Result + Temp;
         end;
      end if;
   end Parallel_Fibonacci;

   Result : Natural;
begin
   Fibonacci_Recursion.Execute
     (Item  => Value,
      Other_Workers => Other_Workers'Access,
      Dispatcher => Dispatcher'Access,
      Process => Parallel_Fibonacci'Access,
      --  Ceiling      => ,
      --  Worker_Count => ,
      Work_Budget  => Parallel_Test_Harness.Work_Budget_For_Even_Loads,
      Use_Affinity => Parallel_Test_Harness.Use_Affinity,
      Result => Result);
   return Result;
end Fibonacci_Composite_Work_Seeking;
