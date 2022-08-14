pragma Ada_2012;

with Ada.IO_Exceptions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;

with GNATCOLL.Readline;

with Printer;
with Reader;
with Types;
with Eval;

procedure Step6_File is
   package CL renames Ada.Command_Line;
   package RL renames GNATCOLL.Readline;

   E : constant Types.Env_Ptr := Eval.New_Env;
begin
   pragma Warnings (Off, """V""");
   declare
      R : Reader.Instance;
      V : Types.Ast;
      L : Types.List;
   begin
      R.Init ("(def! load-file (fn* (f) " &
              "(eval (read-string (str ""(do "" (slurp f) ""\nnil)"")))))");
      V := Eval.Eval (R.Read_Form, E);

      if CL.Argument_Count >= 2 then
         for I in 2 .. CL.Argument_Count loop
            L.Append (Types.New_String (CL.Argument (I)));
         end loop;
      end if;
      Eval.Set (E, "*ARGV*", Types.New_List (L));

      if CL.Argument_Count >= 1 then
         R.Init ("(load-file """ & CL.Argument (1) & """)");
         V := Eval.Eval (R.Read_Form, E);
         return;
      end if;
   end;
   pragma Warnings (On, """V""");

   loop
      declare
         R : Reader.Instance;
         F : Types.Ast;
         V : Types.Ast;
      begin
         R.Init (RL.Get_Line ("user> "));
         F := R.Read_Form;
         V := Eval.Eval (F, E);
         Put_Line (Printer.Pr_Str (V));
         New_Line;
      exception
         when Ada.IO_Exceptions.End_Error =>
            return;
         when E : Eval.Mal_Error =>
            Eval.Consume_Mal_Error (V);
            Put_Line ("Mal error: " & Printer.Pr_Str (V));   
         when E : others =>
            Put_Line (Exception_Name (E) & ": " &
                      Exception_Message (E));
      end;
   end loop;
end Step6_File;
