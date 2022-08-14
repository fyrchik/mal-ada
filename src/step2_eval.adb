pragma Ada_2012;

with Ada.IO_Exceptions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Readline;

with Printer;
with Reader;
with Types;
with Eval;

procedure Step2_Eval is
   package RL renames GNATCOLL.Readline;
begin
   loop
      declare
         R : Reader.Instance;
         F : Types.Ast;
         V : Types.Ast;
         E : constant Types.Env_Ptr := Eval.New_Env;
      begin
         R.Init (RL.Get_Line ("user> "));
         F := R.Read_Form;
         V := Eval.Eval (F, E);
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
end Step2_Eval;
