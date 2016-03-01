
-- Retrieve program arguments and options by parsing the command line
package Argument_Parser is

   -- Illegal argument xception
   Argument_Error:  exception;

   -- Retrieve command line arguments
   procedure Get_Arguments;

private

   -- Files that can be passed as command line arguments
   type FileInput is (PROGRAM, ROOT, LOOPS, CONSTRAINTS, CACHE, NOTAFILE);

   -- Prints this program usage info
   procedure Usage;

end Argument_Parser;
