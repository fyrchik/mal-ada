pragma Ada_2012;

with Ada.IO_Exceptions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Readline;

with Printer;
with Reader;
with Types;

procedure Step1_Read_Print is
   package RL renames GNATCOLL.Readline;

   function Eval (V : Types.Ast) return Types.Ast is (V);
begin
   loop
      declare
         R : Reader.Instance;
         E : Types.Ast;
         V : Types.Ast;
      begin
         R.Init (RL.Get_Line ("user> "));
         E := R.Read_Form;
         V := Eval (E);
         Put_Line (Printer.Pr_Str (V));
         New_Line;
      exception
         when Ada.IO_Exceptions.End_Error =>
            return;
         when E : others =>
            Put_Line (Exception_Name (E) & ": " &
                      Exception_Message (E));
      end;
   end loop;
end Step1_Read_Print;
