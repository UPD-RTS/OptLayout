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
-- FILE NAME      : utilities.adb
-- PACKAGE        : UTILITIES body
-- PURPOSE        : (Mostly) String-related utilities
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Ada.Strings.Unbounded, Ada.Strings.Fixed;
use Ada.Strings.Unbounded, Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;


package body Utilities is


   function UTrim (s : Unbounded_String) return String is
   begin
      return Ada.Strings.Fixed.Trim (To_String(s), Ada.Strings.Right);
   end UTrim;

   function Trim (s : String) return String is
   begin
      return Ada.Strings.Fixed.Trim (s, Ada.Strings.Right);
   end Trim;

   function Trim (i : Integer) return String is
     begin
      return Ada.Strings.Fixed.Trim (Integer'Image (I), Ada.Strings.Left);
   end Trim;


   function First_Index (Src : in String; Ch  : in Character) return Natural is
   begin
      for I in Src'Range loop
         if Src (I) = Ch then return I; end if;
      end loop;
      return 0;
   end First_Index;

   function Last_Index (Src : in String; Ch  : in Character) return Natural is
   begin
      for I in reverse Src'Range loop
         if Src (I) = Ch then return I; end if;
      end loop;
      return 0;
   end Last_Index;



end Utilities;
