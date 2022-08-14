with Ada.Containers; use Ada.Containers;

package body Types is
   overriding
   function "=" (Left, Right : Ast) return Boolean is
   begin
      case Left.K is
         when Kind_Map =>
            if Right.K /= Kind_Map or else Left.HMap.Length /= Right.HMap.Length then
               return False;
            end if;

            declare
               RC : IM.Cursor;
            begin
               for LC in Left.HMap.Iterate loop
                  RC := Right.HMap.Find (IM.Key (LC));
                  if not IM.Has_Element (RC) or else
                     IM.Element (LC) /= IM.Element (RC)
                  then
                     return False;
                  end if;
               end loop;
               return True;
            end;
         when Kind_List | Kind_Vector =>
            if Right.K not in Kind_List | Kind_Vector or else
               Left.List.Length /= Right.List.Length
            then
               return False;
            end if;

            for I in Left.List.First_Index .. Left.List.Last_Index loop
               if not (Left.List.Element (I) = Right.List.Element (I)) then
                  return False;
               end if;
            end loop;
            return True;
         when Kind_Number =>
            return Right.K = Kind_Number and then Left.Number = Right.Number;
         when Kind_Boolean =>
            return Right.K = Kind_Boolean and then Left.Bool = Right.Bool;
         when Kind_Symbol | Kind_String | Kind_Keyword =>
            return Right.K = Left.K and then Left.Str.all = Right.Str.all;
         when Kind_Nil => return Right.K = Kind_Nil;
         when Kind_Function => return False;
         when Kind_Lambda => return Right.K = Kind_Lambda and then
                                    Left.Fn = Right.Fn;
         when Kind_Atom => return Right.K = Kind_Atom and then
                                  Left.Ptr = Right.Ptr;
         when Kind_None => raise Invalid_Type;
      end case;
   end "=";

   function New_List (Sym : String; V : Ast) return Ast is
      L : List;
   begin
      L.Append (New_Symbol (Sym));
      L.Append (V);
      return New_List (L);
   end New_List;

   function New_List (V : Ast) return Ast is
      L : List;
   begin
      L.Append (V);
      return New_List (L);
   end New_List;

   function New_Map (L : List) return Ast is
      M    : Map;
      K, V : Ast;
      C    : IV.Cursor := L.First;
   begin
      while IV.Has_Element (C) loop
         K := IV.Element (C); IV.Next (C);
         V := IV.Element (C); IV.Next (C);
         M.Include (K, V);
      end loop;
      return New_Map (M);
   end New_Map;
end Types;