with Ada.Characters.Latin_1;

with Types; use Types;

package body Reader is
   package Latin_1 renames Ada.Characters.Latin_1;

   procedure Init (R : in out Instance; S : String) is
   begin
      R := (S => SU.To_Unbounded_String (S), Index => 1);
   end Init;

   function Read_Form (R : in out Instance) return Ast is
      function Read_List (R       : in out Instance;
                          Opening : Character) return Ast is
         V       : List;
         Closing : constant Character :=
            (case Opening is
             when '(' => ')',
             when '[' => ']',
             when '{' => '}',
             when others => raise Read_Error with "unreachable");
      begin
         loop
            R.Skip_Whitespace;

            case R.Peek is
               when ')' | ']' | '}' =>
                  if R.Peek /= Closing then
                     raise Read_Error with "unmatched closing bracket";
                  end if;

                  R.Next;
                  return (
                     case Opening is
                     when '(' => New_List (V),
                     when '[' => New_Vector (V),
                     when '{' => New_Map (V),
                     when others => raise Read_Error with "unreachable");
               when others =>
                  V.Append (R.Read_Form);
            end case;
         end loop;
      end Read_List;

      function Try_Consume (R : in out Instance; S : String) return Boolean is
         L : constant Natural := S'Length - 1;
      begin
         if R.Has (L) and then
            SU.Slice (R.S, R.Index, R.Index + L) = S and then
            (R.Index + L + 1 > SU.Length (R.S) or else
             not Is_Symbol_Char (SU.Element (R.S, R.Index + L + 1)))
         then
            R.Index := R.Index + L + 1;
            return True;
         end if;

         return False;
      end Try_Consume;

      function Read_Number (R : in out Instance) return Ast is
         Neg : constant Boolean := R.Peek = '-';
         Acc : Integer := 0;
      begin
         if Neg then
            R.Next;
         end if;

         while not R.EOF and then R.Peek in '0' .. '9' loop
            Acc := Acc * 10 +
                   Character'Pos (R.Peek) -
                   Character'Pos ('0');
            R.Next;
         end loop;

         return New_Number (if Neg then -Acc else Acc);
      end Read_Number;

      function Read_Symbol (R : in out Instance) return String is
         Start : constant Integer := R.Index;
      begin
         while not R.EOF and then
            (Is_Symbol_Char (R.Peek) or else
             Is_Digit (R.Peek))
         loop
            R.Next;
         end loop;

         if Start = R.Index then
            raise Read_Error with "empty symbol";
         end if;

         return SU.Slice (R.S, Start, R.Index - 1);
      end Read_Symbol;

      function Read_String (R : in out Instance) return Ast is
         Acc : SU.Unbounded_String;
      begin
         loop
            case R.Peek is
               when '"' =>
                  R.Next;
                  return New_String (SU.To_String (Acc));
               when '\' =>
                  R.Next;
                  SU.Append (Acc,
                             (case R.Peek is
                              when 't' => Latin_1.HT,
                              when 'n' => Latin_1.LF,
                              when '\' => '\',
                              when others => '"'));
               when others =>
                  SU.Append (Acc, R.Peek);
            end case;
            R.Next;
         end loop;
      end Read_String;

      function Read_Atom (R : in out Instance) return Ast is
         C : Character;
      begin
         R.Skip_Whitespace;

         C := R.Peek;
         if C in '0' .. '9' or else
            (C = '-' and then
             R.Has (1) and then
             R.Peek (1) in '0' .. '9')
         then
            return Read_Number (R);
         end if;

         if Is_Symbol_Char (C) then
            if C = 't' and then Try_Consume (R, "true") then
               return Bool_True;
            elsif C = 'f' and then Try_Consume (R, "false") then
               return Bool_False;
            elsif C = 'n' and then Try_Consume (R, "nil") then
               return Nil;
            elsif C = ':' then
               R.Next;
               return New_Keyword (Read_Symbol (R));
            else
               return New_Symbol (Read_Symbol (R));
            end if;
         end if;

         R.Next;
         if C = '"' then
            return Read_String (R);
         end if;

         declare
            L : List;
         begin
            case C is
               when ''' => L.Append (New_Symbol ("quote"));
               when '`' => L.Append (New_Symbol ("quasiquote"));
               when '@' => L.Append (New_Symbol ("deref"));
               when '~' =>
                  if not R.EOF and then R.Peek = '@' then
                     R.Next;
                     L.Append (New_Symbol ("splice-unquote"));
                  else
                     L.Append (New_Symbol ("unquote"));
                  end if;
               when others => raise Read_Error with "unexpected character: " & C'Image;
            end case;

            L.Append (R.Read_Form);
            return New_List (L);
         end;
      end Read_Atom;

      C        : Character;
      Result   : Ast;
      L        : List;
      Meta     : Ast;
   begin
      R.Skip_Whitespace;

      C := R.Peek;
      if C = '^' then
         R.Next;
         Meta := R.Read_Form;
         R.Skip_Whitespace;
         C := R.Peek;

         L.Append (New_Symbol ("with-meta"));
      end if;

      case C is
         when '(' | '[' | '{' =>
            R.Next;
            Result := Read_List (R, C);
         when others =>
            Result :=  Read_Atom (R);
      end case;

      if Meta.K = Kind_None then
         return Result;
      end if;

      L.Append (Result);
      L.Append (Meta);
      return New_List (L);
   end Read_Form;

   procedure Skip_Whitespace (R : in out Instance) is
   begin
      while not R.EOF and then Is_Whitespace (R.Peek) loop
         R.Next;
      end loop;

      --  Comments span to the end of the line.
      if not R.EOF and then R.Peek = ';' then
         while not R.EOF and then R.Peek /= Latin_1.LF loop
            R.Next;
         end loop;

         --  Take care of multi-line comments.
         R.Skip_Whitespace;
      end if;
   end Skip_Whitespace;

   function EOF (R : Instance) return Boolean is
      (R.Index > SU.Length (R.S));

   function Has (R : Instance; Offset : Natural) return Boolean is
      (R.Index + Offset <= SU.Length (R.S));

   function Peek (R : Instance;
                  Offset : Natural := 0) return Character is
      (if R.Has (Offset)
       then SU.Element (R.S, R.Index + Offset)
       else raise Read_Error with "unexpected end of input");

   procedure Next (R : in out Instance) is
   begin
      R.Index := R.Index + 1;
   end Next;

   function Is_Whitespace (C : Character) return Boolean is
      (C in ' ' | ',' | Latin_1.HT | Latin_1.LF);
end Reader;