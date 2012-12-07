------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      P A R A L L E L . C O N F I G                       --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                   Copyright (C) 2011, Bradley J. Moore                   --
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
--  This package provides configuration needed to allow the generics to
--  compile on the Linux platform.
--

package Parallel.Config is

   pragma Preelaborate;
   pragma Remote_Types;

   type Target_Kinds is (Any, Linux, BSD, MacOS_X, HPUX, IRIX, Windows);

   --  Note: This setting only affects the implementation of setting affinity
   --  and determining the number of processors on the system.
   --  If the intended target does not operate correctly, (for example
   --  raises Program_Error), then it may make sense to;
   --     1) Always specify Use_Affinity as False (which is the default).
   --        Use_Affinity is usually very subtle and doesn't tend to improve
   --        performance except in very special situations. In other
   --        situations, it can actually degrade performance.
   --
   --     2) Always specify the desired number of workers explicitly. Otherwise
   --        the default will be 2 rather than based on the number of present
   --        processors.
   --
   --    'Any' is a special configuration that requires Use_Affinity => False,
   --    and that the default number of workers is 2. This configuration should
   --    be very portable, and should be used when other configurations

   Target : constant Target_Kinds := Any;

end Parallel.Config;
