with Lock;                 use Lock;

procedure Main is
begin
   Lock.Initialize;

   Lock.Fetch(0);
   Lock.Release(0);

end;

