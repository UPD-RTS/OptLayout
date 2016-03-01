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
-- FILE NAME      : utilities.ads
-- PACKAGE        : UTILITIES spec
-- PURPOSE        : (Mostly) String-related utilities
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Utilities is

   -- Trim an unbounded string
   function UTrim (s : Unbounded_String) return String;

   -- Trim a string
   function Trim (s : String) return String;

   -- Trim the string representation of an integer (remove leading space)
   function Trim (i : Integer) return String;

   -- Return the first occurrence of a character in a string
   function First_Index (Src : in String; Ch  : in Character) return Natural;

   -- Return the last occurrence of a character in a string
   function Last_Index (Src : in String; Ch  : in Character) return Natural;

end Utilities;
