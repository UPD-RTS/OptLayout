--  Representation of the current RAM memory
with Global; use Global;
with Ada.Unchecked_Deallocation; --use Ada.Unchecked_Deallocation;

package body RAM.Pool is

--     type Proc_Pool is
--        record
--           proc : Contents;
--           size : Integer;
--           num : Natural;
--           sharing : Boolean := False;
--        end record;
--
--     type Proc_Pool_ptr is access ProcPool;
--
--     procedure Free_Pool is new
--       Ada.Unchecked_Deallocation (Proc_Pool,
--                                   Proc_Pool_ptr);

   --  Return the overall size of the pool
   -- i.e. size = SUM{ size of all procedures }
   function Size (p : Proc_Pool_ptr) return Integer is
   begin
      return p.size;
   end Size;

   function Size (p : Proc_Pool) return Integer is
   begin
      return p.size;
   end Size;

   --  Return the number of procedures in the pool
   function Num (p : Proc_Pool_ptr) return Integer is
   begin
      return p.num;
   end Num;

   function Num (p : Proc_Pool) return Integer is
   begin
      return p.num;
   end Num;

   --  Add a procedure to the pool
   procedure Add_Procedure (p : in out Proc_Pool_ptr;
                            proc: Descriptor;
                            size : Integer) is
   begin
      p.num := p.num + 1;
      p.proc (p.num+1) := proc;
      p.size := p.size + proc.size;
   end Add_Procedure;


   --  Get the n-th procedure from pool
   function Get_Procedure (p : Proc_Pool_ptr; n : Integer) return Descriptor is
   begin
      return Get_Procedure (p.all, n);
   end Get_Procedure;

   function Get_Procedure (p : Proc_Pool; n : Integer) return Descriptor is
   begin
      return p.proc (n);
   end Get_Procedure;



end RAM.Pool;
