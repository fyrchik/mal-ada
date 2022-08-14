with Ada.Characters.Latin_1;

with Types; use Types;

package body Printer is
   package Latin_1 renames Ada.Characters.Latin_1;

   function Pr_Str (V               : Types.Ast;
                     Print_Readably : Boolean := True) return String is
      S : SU.Unbounded_String;
   begin
      Append (S, V, Print_Readably);
      return SU.To_String (S);
   end Pr_Str;

   procedure Append (S              : in out SU.Unbounded_String;
                     V              : Types.Ast;
                     Print_Readably : Boolean := True) is
   begin
      case V.K is
         when Kind_List | Kind_Vector =>
            SU.Append (S, (if V.K = Kind_List then '(' else '['));

            for I in V.List.First_Index .. V.List.Last_Index loop
               if I /= V.List.First_Index then
                  SU.Append (S, ' ');
               end if;
               SU.Append (S, Pr_Str (V.List.Element (I), Print_Readably));
            end loop;

            SU.Append (S, (if V.K = Kind_List then ')' else ']'));
         when Kind_Map =>
            SU.Append (S, '{');
            declare
               Not_First : Boolean := False;
            begin
               for C in V.HMap.all.Iterate loop
                  if Not_First then
                     SU.Append (S, ' ');
                  end if;
                  Not_First := True;

                  SU.Append (S, Pr_Str (IM.Key (C), Print_Readably));
                  SU.Append (S, ' ');
                  SU.Append (S, Pr_Str (IM.Element (C), Print_Readably));
               end loop;
            end;
            SU.Append (S, '}');
         when Kind_Number =>
            declare
               Img : constant String := V.Number'Image;
            begin
               SU.Append (S, (if V.Number < 0
                              then Img
                              else Img (2 .. Img'Last)));
            end;
         when Kind_Symbol =>
            SU.Append (S, V.Str.all);
         when Kind_Keyword =>
            SU.Append (S, ':' & V.Str.all);
         when Kind_String =>
            if not Print_Readably then
               SU.Append (S, V.Str.all);
            else
               SU.Append (S, '"');

               for C of V.Str.all loop
                     SU.Append (S, (case C is
                                    when Latin_1.LF => "\n",
                                    when Latin_1.HT => "\t",
                                    when '"' => "\""",
                                    when '\' => "\\",
                                    when others => "" & C));
               end loop;

               SU.Append (S, '"');
            end if;
         when Kind_Boolean =>
            SU.Append (S, (if V.Bool then "true" else "false"));
         when Kind_Nil =>
            SU.Append (S, "nil");
         when Kind_Function | Kind_Lambda =>
            SU.Append (S, "#<function>");
         when Kind_Atom =>
            SU.Append (S, "(atom ");
            Append (S, V.Ptr.all, Print_Readably);
            SU.Append (S, ')');
         when others => raise Print_Error;
      end case;
   end Append;
end Printer;