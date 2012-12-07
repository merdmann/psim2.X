with Ada.Text_IO;       use Ada.Text_IO;
with Sync;              use Sync;

procedure Main is

   N : constant Natural := 4;

   ---------------
   -- My_Worker --
   ---------------
   task type My_Worker_Task(Id : Natural);

   type My_Worker_Task_Access is access all My_Worker_Task;

   X : aliased Natural := 0;

   function Increment return Natural is
      T : Natural := X;
      F : Boolean;
   begin
      while not CAS( X'Address, T, T+1) loop
         T := X;
      end loop;

      return T+1;
   end Increment;

   --------------------
   -- My_Worker_Task --
   --------------------
   task body My_Worker_Task is
   begin
      for i in 1..20000 loop
         declare
            X : Natural;
         begin
            X := Increment;
         end ;
      end loop;

      Put_Line( "Task " & Natural'Image(Id) & " done " & Natural'Image(X) );
   end My_Worker_Task;

   Result : Natural := 0;

   w : array( 1..N ) of My_Worker_Task_Access;
begin
   for i in W'Range loop
      W(I) := new My_Worker_Task( I );
   end loop;

end Main;
