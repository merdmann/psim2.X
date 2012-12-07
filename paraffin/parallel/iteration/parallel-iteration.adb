package body Parallel.Iteration is

   function "<=" (L, R : Atomic_Loop_Index_Type) return Boolean is
   begin
      return L.Value <= R.Value;
   end "<=";

   function Effective_Worker_Count
     (Workers : Worker_Count_Type;
      Iterations : Positive) return Positive_Worker_Count is
   begin
      if Workers = Use_Optimal_Worker_Count then
         return Optimal_Worker_Count (Iterations);
      else
         return Positive_Worker_Count
           (Positive'Min
              (Iterations, Positive (Workers)));
      end if;
   end Effective_Worker_Count;

   ----------------------------------------------------------------------

   procedure Next (Item : in out Atomic_Loop_Index_Type) is
   begin
      Item.Value := Item.Value + 1;
   end Next;

   ----------------------------------------------------------------

   function Optimal_Worker_Count
     (Iterations : Positive)
      return Worker_Count_Type is
   begin
      return Worker_Count_Type
        (Positive'Min
           (Iterations,
            Positive (Available_CPUs) +
            (Iterations mod Positive (Available_CPUs))));
   end Optimal_Worker_Count;

   ----------------------------------------------------------------

   function Select_Minimum_Steal_Value
     (Workers : Worker_Count_Type;
      Iterations : Positive;
      Minimum_Steal : Natural) return Positive is
   begin
      if Minimum_Steal = Select_Minimum_Steal then
         return Natural'Max
           (1,
            Natural'Min (Iterations / Positive (Workers) / 4, 10000));
      else
         -- Client provided a value, so use that.
         return Minimum_Steal;
      end if;
   end Select_Minimum_Steal_Value;

end Parallel.Iteration;
