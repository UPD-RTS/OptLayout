-----------------------------------------------------------------------
--         OptLayout - A Cache-aware Memory Layout Optimizer         --
--                                                                   --
--               Copyright (c) 2011 Enrico Mezzetti                  --
--                     University of Padua                           --
--                  <emezzett@math.unipd.it>                         --
--                                                                   --
-- This file is part of OptLayout.                                   --
--                                                                   --
-- OptLayout is free software: you can redistribute it and/or modify --
-- it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation, either version 3 of the License, --
-- or (at your option) any later version.                            --
--                                                                   --
-- OptLayout is distributed in the hope that it will be useful,      --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public License --
-- along with OptLayout. If not, see <http://www.gnu.org/licenses/>. --
--                                                                   --
--                                                                   --
--                                                                   --
-----------------------------------------------------------------------
-- FILE NAME      : cache.ads
-- PACKAGE        : CACHE spec
-- PURPOSE        : Representation of the current cache configuration
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Global; use Global;

package Cache is

   -- Set size type declaration
   subtype Set_Dim is Integer range 1..256;
   -- Line size type declaration
   subtype Line_Dim is Integer range 8..32;

   -- Actual cache type declaration
   type Cache_Type is private;
   type Cache_Ptr is access all Cache_Type;

   -- Initialize the cache configuration
   procedure Init_Cache (l : Line_Dim; s : Set_Dim; w : Positive );

   -- Returns the number of ways in the cache
   function Get_Ways return Positive;

   -- Returns the cache line size
   function Get_Line_Size return Positive;

   -- Returns the number of cache lines per set
   function Get_Lines_X_Set return Positive;

   -- Returns the size of each cache set
   function Get_Set_Size return Integer;

   -- Returns end of a cache set starting at address "start"
   function End_Of_Set (start : Address) return Integer;



private

   type Cache_Type is
      record
         ways        : Positive;
         line        : Line_Dim;
         lines_X_set : Set_Dim;
      end record;

   -- Sinlge instantiation of cache type
   cacheSingleton: Cache_Ptr;

end Cache;
