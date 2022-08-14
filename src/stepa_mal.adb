pragma Ada_2012;
pragma Assertion_Policy (Check);

with Ada.IO_Exceptions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;

with GNATCOLL.Readline;

with Printer;
with Reader;
with Types;
with Eval;

procedure StepA_Mal is
   package CL renames Ada.Command_Line;
   package RL renames GNATCOLL.Readline;

   E : constant Types.Env_Ptr := Eval.New_Env;
   R : Reader.Instance;
   V : Types.Ast;
begin
   pragma Warnings (Off, """V""");
   declare
      L : Types.List;
   begin
      Eval.Set (E, "*host-language*", Types.New_String ("ada"));

      R.Init ("(def! load-file (fn* (f) " &
              "(eval (read-string (str ""(do "" (slurp f) ""\nnil)"")))))");
      V := Eval.Eval (R.Read_Form, E);

      R.Init ("(defmacro! cond                                             " &
              "   (fn* (& xs)                                              " &
              "      (if (> (count xs) 0)                                  " &
              "          (list 'if (first xs)                              " &
              "                (if (> (count xs) 1)                        " &
              "                    (nth xs 1)                              " &
              "                    (throw ""odd number of forms to cond""))" &
              "          (cons 'cond (rest (rest xs)))))))");
      V := Eval.Eval (R.Read_Form, E);

      if CL.Argument_Count >= 2 then
         for I in 2 .. CL.Argument_Count loop
            L.Append (Types.New_String (CL.Argument (I)));
         end loop;
      end if;
      Eval.Set (E, "*ARGV*", Types.New_List (L));

      if CL.Argument_Count >= 1 then
         R.Init ("(load-file """ & CL.Argument (1) & """)");
         begin
            V := Eval.Eval (R.Read_Form, E);
         exception
            when Eval.Mal_Error =>
               Eval.Consume_Mal_Error (V);
               Put_Line ("Mal error: " & Printer.Pr_Str (V));
         end;
         return;
      end if;
   end;
   pragma Warnings (On, """V""");

   R.Init ("(println (str ""Mal ["" *host-language* ""]""))");
   V := Eval.Eval (R.Read_Form, E);

   loop
      declare
         F : Types.Ast;
      begin
         R.Init (RL.Get_Line ("user> "));
         F := R.Read_Form;
         V := Eval.Eval (F, E);
         Put_Line (Printer.Pr_Str (V));
      exception
         when Ada.IO_Exceptions.End_Error =>
            return;
         when Eval.Mal_Error =>
            Eval.Consume_Mal_Error (V);
            Put_Line ("Mal error: " & Printer.Pr_Str (V));
         when E : others =>
            Put_Line (Exception_Name (E) & ": " &
                      Exception_Message (E));
      end;
   end loop;
end StepA_Mal;
