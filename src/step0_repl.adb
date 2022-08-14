pragma Ada_2012;

with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.Readline;

procedure Step0_REPL is
   package RL renames GNATCOLL.Readline;
   package SU renames Ada.Strings.Unbounded;


   procedure Print (S : String) is
   begin
      Ada.Text_IO.Put_Line (S);
   end Print;

   function Read (S : String) return String is (S);
   function Eval (S : String) return String is (S);

   L : SU.Unbounded_String;
   E : SU.Unbounded_String;
   R : SU.Unbounded_String;
begin
   loop
      begin
         L := SU.To_Unbounded_String (RL.Get_Line ("user> "));
         E := SU.To_Unbounded_String (Read (SU.To_String (L)));
         R := SU.To_Unbounded_String (Eval (SU.To_String (E)));
         Print (SU.To_String (R));
      exception
         when E : Ada.IO_Exceptions.End_Error =>
            return;
      end;
   end loop;
end Step0_REPL;
