with Ada.Strings.Unbounded;

with Types;

package Printer is
   package SU renames Ada.Strings.Unbounded;

   --
   --  Prints mal value on a single line.
   --
   function Pr_Str (V              : Types.Ast;
                    Print_Readably : Boolean := True) return String;

   --
   --  Appends stringified representation of a mal value to S.
   --
   procedure Append (S              : in out SU.Unbounded_String;
                     V              : Types.Ast;
                     Print_Readably : Boolean := True);

private
   Print_Error : exception;
end Printer;