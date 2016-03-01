--  Representation of the current RAM memory
with Global; use Global;
with Ada.Unchecked_Deallocation; --use Ada.Unchecked_Deallocation;

package RAM.Pool is

   type Proc_Pool is
      record
         proc : Contents;
         size : Integer;
         num : Natural;
         sharing : Boolean := False;
      end record;

   type Proc_Pool_ptr is access Proc_Pool;

    -- Unchecked deallocation of a procedure pool
   procedure Free_Pool is new
     Ada.Unchecked_Deallocation (Proc_Pool,
                                 Proc_Pool_ptr);

   --  Return the overall size of the pool
   -- i.e. size = SUM{ size of all procedures }
   function Size (p : Proc_Pool_ptr) return Integer;
   function Size (p : Proc_Pool) return Integer;

   --  Return the number of procedures in the pool
   function Num (p : Proc_Pool_ptr) return Integer;
   function Num (p : Proc_Pool) return Integer;

   --  Add a procedure to the pool
   procedure Add_Procedure (p : in out Proc_Pool_ptr; proc: Descriptor;
                            size : Integer);
--     procedure Add_Procedure (p : in out Proc_Pool; procName: String;
--                              size : Integer);

   --  Get the n-th procedure from pool
   function Get_Procedure (p : Proc_Pool_ptr; n : Integer) return Descriptor;
   function Get_Procedure (p : Proc_Pool; n : Integer) return Descriptor;


end RAM.Pool;
