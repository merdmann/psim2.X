------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                            P A R A L L E L                               --
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

with Ada.Environment_Variables; use Ada;
with Ada.Characters.Latin_1; use Ada.Characters;
with Interfaces.C; use Interfaces;
with Parallel.Config;

package body Parallel is

   Worker_Count_Initialized : Boolean := False;
   Default_Workers : Positive_Worker_Count := 1;

   Available_CPUs_Initialized : Boolean := False;
   Available_CPU_Count : CPU_Count'Base := 0;

   Debug_Initialized : Boolean := False;
   Debug_Mode : Boolean := False;

   ----------------------------------------------------------------------

   function Available_CPUs return CPU_Count is

      Default_CPU_Count : constant := 2;

   begin
      pragma Warnings (Off, "*code*has been deleted*");

      case Config.Target is

      when Config.Any =>
         return Default_CPU_Count;

      when Config.MacOS_X |
           Config.HPUX |
           Config.IRIX |
           Config.BSD =>

         raise Program_Error with
           "This configuration does not currently support determining the" &
         "CPU count. Worker_Count will need to be explicitly specified" &
         " by client";

      when Config.Linux =>
         declare
            O_RDONLY : constant := 0;
            function open
              (path : C.char_array;
               oflag : C.int) return C.int;
            pragma Import (Convention => C,
                           Entity => open,
                           External_Name => "open");

            subtype ssize_t is C.int;

            function read
              (filedes : C.int;
               buf : C.char_array;
               nbyte : C.size_t) return ssize_t;
            pragma Import (Convention => C,
                           Entity => read,
                           External_Name => "read");

            function close
              (filedes : C.int) return C.int;
            pragma Import (Convention => C,
                           Entity => close,
                           External_Name => "close");

            cpu_info : C.int;
            pragma Warnings (Off, "*buf* never assigned*");
            buf : C.char_array (1 .. 4096);
            res : ssize_t;
            pos : C.size_t := buf'First;
            use type C.int;
            use type C.size_t;
            use type C.char;
            use type C.char_array;

            --  text to count occurrences of in proc file
            scan_item : constant String := "processor";

            --  special file containing info on processors
            --  note compiler bug prevents calling the function To_C
            --  here, so we had to call the To_C procedure, which is
            --  quite a bit messier here, but at least it works.
            cpu_info_file : constant String := "/proc/cpuinfo";
            C_file : C.char_array
              (1 .. C.size_t (cpu_info_file'Last) + 1);
            C_file_len : C.size_t;

         begin
            if not Available_CPUs_Initialized then

               Available_CPUs_Initialized := True;

               --  Ideally we could use the function version, but that call
               --  crashes the compiler when compiling for Windows. (Weird)
               --  It actually does compile for Linux that way
               C.To_C (Item       => cpu_info_file,
                       Target     => C_file,
                       Count      => C_file_len,
                       Append_Nul => True);

               cpu_info := open
                 (path => C_file,
                  oflag => O_RDONLY);

               pragma Assert (Check => cpu_info > 0);

               Scan_Loop : loop

                  pos := buf'First;

                  Get_Line_Loop : loop
                     res := read (filedes => cpu_info,
                                  buf     => buf (pos .. pos),
                                  nbyte   => 1);
                     exit Scan_Loop when res = 0 and then pos = buf'First;
                     exit Get_Line_Loop when
                       buf (pos) = C.To_C (Item => Latin_1.LF) or else res = 0;
                     pos := pos + 1;
                  end loop Get_Line_Loop;

                  if pos > scan_item'Length and then
                    buf (buf'First .. buf'First + scan_item'Length - 1)
                    = "processor" then

                     Available_CPU_Count := Available_CPU_Count + 1;
                  end if;

               end loop Scan_Loop;

               res := close (cpu_info);
               pragma Assert (res = 0);

            end if;

            return Available_CPU_Count;
         end;

         pragma Warnings (On, "*buf* never assigned*");

         when Config.Windows =>

            declare

               subtype DWORD is Interfaces.Unsigned_32;
               subtype LPVOID is System.Address;
               subtype DWORD_PTR is System.Address;
               subtype WORD is Interfaces.Unsigned_16;

               type SYSTEM_INFO is
                  record
                     dwOemId : DWORD;
                     dwPageSize : DWORD;
                     lpMinimumApplicationAddress : LPVOID;
                     lpMaximumApplicationAddress : LPVOID;
                     dwActiveProcessorMask : DWORD_PTR;
                     dwNumberOfProcessors : DWORD;
                     dwProcessorType : DWORD;
                     dwAllocationGranularity : DWORD;
                     wProcessorLevel : WORD;
                     wProcessorRevision : WORD;
                  end record;
               pragma Convention (C, SYSTEM_INFO);

               type LPSYSTEM_INFO is access all SYSTEM_INFO;

               procedure GetSystemInfo
                 (lpSystemInfo : LPSYSTEM_INFO);
               pragma Import (Convention => Stdcall,
                              Entity => GetSystemInfo,
                              External_Name => "GetSystemInfo");

               Info : aliased SYSTEM_INFO;

            begin
               GetSystemInfo (Info'Access);
               return CPU_Count (Info.dwNumberOfProcessors);
            end;

      end case;

      pragma Warnings (On, "*code*has been deleted*");
   end Available_CPUs;

   ----------------------------------------------------------------------

   function Debug_Logging return Boolean is
      Debugging_Enabled : constant String :=
        "PARALLEL_DEBUG_ENABLED";
   begin

      if not Debug_Initialized and then
        Environment_Variables.Exists
          (Name => Debugging_Enabled) then

         Debug_Mode := Boolean'Value
           (Environment_Variables.Value (Name => Debugging_Enabled));
         Debug_Initialized := True;
      end if;

      return Debug_Mode;

   end Debug_Logging;

   ----------------------------------------------------------------------

   function Default_Worker_Count return Positive_Worker_Count is
      Worker_Count : constant String := "DEFAULT_WORKER_COUNT";
   begin
      if not Worker_Count_Initialized then
         if Environment_Variables.Exists
           (Name => Worker_Count) then

            Default_Workers := Positive_Worker_Count'Value
              (Environment_Variables.Value (Name => Worker_Count));
            Worker_Count_Initialized := True;
         else
            Default_Workers := Positive_Worker_Count (Available_CPUs);
         end if;

         Worker_Count_Initialized := True;
      end if;
      return Default_Workers;
   end Default_Worker_Count;

   ----------------------------------------------------------------------

   procedure Set_Affinity (Affinity : Affinity_Type)
   is
      use type Config.Target_Kinds;

      Local_Affinity : aliased Affinity_Type;
   begin
      Local_Affinity := Affinity;

      pragma Warnings (Off, "*code*has been deleted*");

      case Config.Target is

      when Config.Any =>
         raise Program_Error with
           "This configuration requires Use_Affinity => False.";

      when Config.MacOS_X |
           Config.HPUX |
           Config.IRIX =>

         pragma Compile_Time_Warning
           (True, "OS-X, HP-UX, and IRIX affinity support unimplemented." &
           "Use_Affinity => False and specify worker count");

         raise Program_Error with
           "This configuration does not currently support setting affinity." &
         " Try setting Use_Affinity to False. It usually work just as well";

      when Config.Linux |
           Config.BSD =>
            declare

               subtype pthread_t is System.Address;

               function pthread_self return pthread_t;
               pragma Import (Convention => C,
                              Entity => pthread_self,
                              External_Name => "pthread_self");

               function pthread_setaffinity_np
                 (thread : pthread_t;
                  cpusetsize : C.size_t;
                  cpuset : access constant Affinity_Type) return C.int;
               pragma Import (Convention => C,
                              Entity => pthread_setaffinity_np,
                              External_Name => "pthread_setaffinity_np");

               Status : C.int := 0;
               use type C.size_t;
               use type C.int;
            begin
               Status := pthread_setaffinity_np
                 (thread => pthread_self,
                  cpusetsize => Local_Affinity'Size / C.char'Size,
                  cpuset => Local_Affinity'Access);

               pragma Assert (Status = 0);
            end;

         when Config.Windows =>
            declare

               subtype DWORD is Affinity_Type;
               type DWORD_PTR is access all DWORD;
               subtype HANDLE is C.int;

               function GetCurrentThread return HANDLE;
               pragma Import (Convention => Stdcall,
                              Entity => GetCurrentThread,
                              External_Name => "GetCurrentThread");

               function SetThreadAffinityMask
                 (hThread : HANDLE;
                  dwThreadAffinityMask : DWORD_PTR) return DWORD_PTR;
               pragma Import (Convention => Stdcall,
                              Entity => SetThreadAffinityMask,
                              External_Name => "SetThreadAffinityMask");

               Result : DWORD_PTR;
            begin
               Result := SetThreadAffinityMask
                 (hThread              => GetCurrentThread,
                  dwThreadAffinityMask => Local_Affinity'Access);
               pragma Assert (Result /= null);
            end;

      end case;

      pragma Warnings (On, "*code*has been deleted*");

   end Set_Affinity;

end Parallel;
