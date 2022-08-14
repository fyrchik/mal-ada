with Ada.Containers; use Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Real_Time;
with Ada.Calendar;

with GNATCOLL.Readline;

with Printer;
with Reader;

package body Eval.Builtin is
   package SU renames Ada.Strings.Unbounded;

   use type IV.Cursor;

   function Add (Args : List; E : Env_Ptr) return Call_Result is
      (Eval_Arith (Args, Op_Add));

   function Sub (Args : List; E : Env_Ptr) return Call_Result is
      (Eval_Arith (Args, Op_Sub));

   function Mul (Args : List; E : Env_Ptr) return Call_Result is
      (Eval_Arith (Args, Op_Mul));

   function Div (Args : List; E : Env_Ptr) return Call_Result is
      (Eval_Arith (Args, Op_Div));

   function Def (Args : List; E : Env_Ptr) return Call_Result is
      V : Ast;
   begin
      Check (Args.Length = 2, "def!: expected 2 arguments");
      Check (Args.First_Element.K = Kind_Symbol,
             "def!: first argument must be a symbol");

      V := Eval (Args.Last_Element, E);
      Set (E, Args.First_Element.Str.all, V);
      return New_Result (V);
   end Def;

   function Defmacro (Args : List; E : Env_Ptr) return Call_Result is
      V : Ast;
   begin
      Check (Args.Length = 2, "defmacro!: expected 2 arguments");
      Check (Args.First_Element.K = Kind_Symbol,
             "defmacro!: first argument must be a symbol");

      V := Eval (Args.Last_Element, E);
      Check (V.K = Kind_Lambda,
             "defmacro!: second argument must be a function");

      V := New_Lambda (V.Fn.Params, V.Fn.Expr, V.Fn.Env, True);
      Set (E, Args.First_Element.Str.all, V);
      return New_Result (V);
   end Defmacro;

   function Is_Macro_Call_Helper (A : Ast; E : Env_Ptr) return Boolean is
      F : Ast;
   begin
      return A.K = Kind_List and then
             not A.List.Is_Empty and then
             A.List.First_Element.K = Kind_Symbol and then
             Find (E, A.List.First_Element.Str.all, F) and then
             F.K = Kind_Lambda and then
             F.Fn.Is_Macro;
   end Is_Macro_Call_Helper;

   function Is_Macro_Call (Args : List; E : Env_Ptr) return Call_Result is
      (New_Result (
         New_Boolean (
            not Args.Is_Empty and then
            Is_Macro_Call_Helper (Args.First_Element, E))));

   function Macroexpand (Args : List; E : Env_Ptr) return Call_Result is
      (New_Result (if Args.Is_Empty
                   then Nil
                   else Macroexpand_Helper (Args.First_Element, E)));

   function Macroexpand_Helper (Arg : Ast; E : Env_Ptr) return Ast is
      F, A  : Ast;
      L     : List;
      EE    : Env_Ptr := E;
   begin
      A := Arg;
      while Is_Macro_Call_Helper (A, E) loop
         if not Find (E, A.List.First_Element.Str.all, F) then
            raise Eval_Error with "unreachable";
         end if;

         L.Clear;
         for I in A.List.First_Index + 1 .. A.List.Last_Index loop
            L.Append (A.List.Element (I));
         end loop;

         EE := New_Env (F.Fn.Env);
         Bind (EE, F.Fn.Params, L);
         A := Eval (F.Fn.Expr, EE);
      end loop;
      return A;
   end Macroexpand_Helper;

   function Let (Args : List; E : Env_Ptr) return Call_Result is
      A     : Ast renames Args.First_Element;
      C     : IV.Cursor;
      N, V  : Ast;
      Child : constant Env_Ptr := New_Env (E);
   begin
      Check (Args.Length = 2, "let*: expected 2 arguments");
      Check (Args.First_Element.K in Kind_Seq,
             "let*: first arguments must be a sequence");

      C := A.List.First;
      while IV.Has_Element (C) loop
         N := IV.Element (C); IV.Next (C);
         V := IV.Element (C); IV.Next (C);
         Set (Child, N.Str.all, Eval (V, Child));
      end loop;

      return (K => Call_Tail, A => Args.Last_Element, E => Child);
   end Let;

   function Eval_If (Args : List; E : Env_Ptr) return Call_Result is
   begin
      Check (Args.Length in 2 | 3, "if: expected 2 or 3 arguments");

      return (if Is_True (Eval (Args.First_Element, E))
              then (K => Call_Tail,
                    A => Args.Element (Args.First_Index + 1),
                    E => E)
              elsif Args.Length = 2 then New_Result (Nil)
              else (K => Call_Tail, A => Args.Last_Element, E => E));
   end Eval_If;

   function Eval_Fn (Args : List; E : Env_Ptr) return Call_Result is
      Params : List;
      Expr   : Ast;
   begin
      Check (Args.Length = 2, "fn*: expected 2 arguments");
      Check (Args.First_Element.K in Kind_Seq,
             "fn*: first argument must be a sequence");

      Params := Args.First_Element.List.all;
      Expr := Args.Last_Element;

      return New_Result (New_Lambda (Params, Expr, E));
   end Eval_Fn;

   function Quote (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length = 1, "quote: expected 1 argument");
      return New_Result (Args.First_Element);
   end Quote;

   function Quasiquote_Helper (Args : List; E : Env_Ptr; Name : String)
      return Ast
   is
      L   : List;

      procedure Callback (C : IV.Cursor) is
         Elt : Ast := IV.Element (C);
      begin
         if Elt.K = Kind_List and then
            Elt.List.Length > 0 and then
            Elt.List.First_Element.K = Kind_Symbol and then
            Elt.List.First_Element.Str.all = "splice-unquote"
         then
            Elt := Elt.List.Element (Elt.List.First_Index + 1);
            Elt := New_List ("concat", Elt);
         else
            Elt := Quasiquote_Expand (New_List (Elt).List.all, E).A;
            Elt := New_List ("cons", Elt);
         end if;

         Elt.List.Append (New_List (L));
         L := Elt.List.all;
      end Callback;
   begin
      Check (Args.Length = 1, Name & ": expected 1 argument");

      declare
         A : Ast renames Args.First_Element;
      begin
         case A.K is
            when Kind_Vector =>
               if A.List.Is_Empty then
                  L := A.List.all;
               else
                  A.List.Reverse_Iterate (Callback'Access);
               end if;
               return New_List ("vec", New_List (L));
            when Kind_List =>
               if A.List.Is_Empty then
                  return A;
               elsif A.K = Kind_List and then
                     A.List.First_Element.K = Kind_Symbol and then
                     A.List.First_Element.Str.all = "unquote"
               then
                  return A.List.Element (A.List.First_Index + 1);
               end if;

               A.List.Reverse_Iterate (Callback'Access);
               return New_List (L);
            when Kind_Nil | Kind_Number | Kind_Boolean | Kind_String =>
               return A;
            when others =>
               return New_List ("quote", A);
         end case;
      end;
   end Quasiquote_Helper;

   function Quasiquote (Args : List; E : Env_Ptr) return Call_Result is
      (New_Result (Eval (Quasiquote_Helper (Args, E, "quasiquote"), E)));

   function Quasiquote_Expand (Args : List; E : Env_Ptr) return Call_Result is
      (New_Result (Quasiquote_Helper (Args, E, "quasiquoteexpand")));

   function Try (Args : List; E : Env_Ptr) return Call_Result is
      Catch_Form : Ast;
   begin
      if Args.Length < 2 then
         return (K => Call_Tail, A => Args.First_Element, E => E);
      end if;

      Catch_Form := Args.Last_Element;
      Check (Catch_Form.K = Kind_List and then
              Catch_Form.List.Length = 3 and then
              Catch_Form.List.First_Element.K = Kind_Symbol and then
              Catch_Form.List.First_Element.Str.all = "catch*" and then
              Catch_Form.List.Element
                (Catch_Form.List.First_Index + 1).K = Kind_Symbol,
             "try*: catch form must be (catch* <SYMBOL> <EXPR>)");

      begin
         --  Can't do TCO here because of the need to catch possible exception.
         return New_Result (Eval (Args.First_Element, E));
      exception
         when Mal_Error =>
            return (K => Call_Tail, A => Catch_Form, E => E);
      end;
   end Try;

   function Catch (Args : List; E : Env_Ptr) return Call_Result is
      A  : Ast;
   begin
      --  Do not perform argument checks, this is done in try*.
      Consume_Mal_Error (A);

      declare
         EE : constant Env_Ptr := New_Env (E);
      begin
         Set (EE, Args.First_Element.Str.all, A);
         return (K => Call_Tail, A => Args.Last_Element, E => EE);
      end;
   end Catch;

   function Throw (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length = 1, "throw: expected 1 argument");

      Throwed := Args.First_Element;
      Is_Throwed := True;
      raise Mal_Error;

      --  functions can't have No_Return aspect, so satisfy the compiler
      return New_Result (Nil);
   end Throw;

   function Time_Ms (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (Args, E);

      package CAL renames Ada.Calendar;
      package RT renames Ada.Real_Time;

      use type CAL.Time;

      Now : constant Duration := CAL.Clock -
                                 CAL.Time_Of (1970, 1, 1, 0.0);
   begin
      return New_Result (
         New_Number (Long_Integer (Now / RT.To_Duration (RT.Milliseconds (1)))));
   end Time_Ms;

   function Meta (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length = 1, "meta: expected 1 arguments");
      return New_Result (if Args.First_Element.Meta = null
                         then Nil
                         else Args.First_Element.Meta.all);
   end Meta;

   function With_Meta (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);

      A : Ast := Args.First_Element;
   begin
      Check (Args.Length = 2, "with-meta: expected 2 arguments");

      A.Meta := new Ast'(Args.Last_Element);
      return New_Result (A);
   end With_Meta;

   function Seq (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length = 1, "seq: expected 1 argument");

      declare
         A : Ast renames Args.First_Element;
         L : List;
      begin
         case A.K is
         when Kind_Seq => L := A.List.all;
         when Kind_Nil => null;
         when Kind_String =>
            L.Reserve_Capacity (A.Str.all'Length);

            for I in A.Str.all'Range loop
               L.Append (New_String ("" & A.Str.all (I)));
            end loop;
         when others =>
            Check (False, "seq: expected list, vector, nil or string");
         end case;

         return New_Result (if L.Is_Empty
                            then Nil
                            else New_List (L));
      end;
   end Seq;

   function Conj (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length >= 1, "conj: expected at least 1 arguments");
      Check (Args.First_Element.K in Kind_Seq,
             "conj: first argument must be a sequence");

      declare
         A : Ast renames Args.First_Element;
         L : List := A.List.Copy;
         C : IV.Cursor := IV.Next (Args.First);
      begin
         while IV.Has_Element (C) loop
            if A.K = Kind_List then
               L.Prepend (IV.Element (C));
            else
               L.Append (IV.Element (C));
            end if;
            IV.Next (C);
         end loop;

         return New_Result (if A.K = Kind_List
                            then New_List (L)
                            else New_Vector (L));
      end;
   end Conj;

   function Eq (Args : List; E : Env_Ptr) return Call_Result is
      (Eval_Bool (Args, Op_Eq));

   function Ne (Args : List; E : Env_Ptr) return Call_Result is
      (Eval_Bool (Args, Op_Ne));

   function Lt (Args : List; E : Env_Ptr) return Call_Result is
      (Eval_Bool (Args, Op_Lt));

   function Gt (Args : List; E : Env_Ptr) return Call_Result is
      (Eval_Bool (Args, Op_Gt));

   function Le (Args : List; E : Env_Ptr) return Call_Result is
      (Eval_Bool (Args, Op_Le));

   function Ge (Args : List; E : Env_Ptr) return Call_Result is
      (Eval_Bool (Args, Op_Ge));

   function Eval_Not (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length = 1, "not: expected 1 argument");

      return New_Result (New_Boolean (not Is_True (Args.First_Element)));
   end Eval_Not;

   function Is_True (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length = 1, "true?: expected 1 argument");

      return New_Result
         (New_Boolean
            (Args.First_Element.K = Kind_Boolean and then
             Args.First_Element.Bool));
   end Is_True;

   function Is_False (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length = 1, "false?: expected 1 argument");

      return New_Result
         (New_Boolean
            (Args.First_Element.K = Kind_Boolean and then
             not Args.First_Element.Bool));
   end Is_False;

   function Is_Of_Type (Args : List;
                        K    : Kind;
                        Name : String) return Call_Result
      with Inline is
   begin
      Check (Args.Length = 1, Name & ": expected 1 argument");
      return New_Result (New_Boolean (Args.First_Element.K = K));
   end Is_Of_Type;

   function Is_Nil (Args : List; E : Env_Ptr) return Call_Result is
      (Is_Of_Type (Args, Kind_Nil, "nil?"));

   function Is_Keyword (Args : List; E : Env_Ptr) return Call_Result is
      (Is_Of_Type (Args, Kind_Keyword, "keyword?"));

   function Is_Symbol (Args : List; E : Env_Ptr) return Call_Result is
      (Is_Of_Type (Args, Kind_Symbol, "symbol?"));

   function Is_Vector (Args : List; E : Env_Ptr) return Call_Result is
      (Is_Of_Type (Args, Kind_Vector, "vector?"));

   function Is_Map (Args : List; E : Env_Ptr) return Call_Result is
      (Is_Of_Type (Args, Kind_Map, "map?"));

   function Is_Sequential (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length = 1, "sequential?: expected 1 argument");

      return New_Result
         (New_Boolean
            (Args.First_Element.K in Kind_Seq | Kind_Map));
   end Is_Sequential;

   function Count (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
      A : Ast renames Args.First_Element;
      N : constant Integer := (if A.K = Kind_Nil
                               then 0
                               else Integer (A.List.Length));
   begin
      return New_Result (New_Number (N));
   end Count;

   function Is_Fn (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length = 1, "fn?: expected 1 argument");

      declare
         F : Ast renames Args.First_Element;
      begin
         return New_Result (
            New_Boolean ((F.K = Kind_Function and then not F.Special) or
                         (F.K = Kind_Lambda and then not F.Fn.Is_Macro)));
      end;
   end Is_Fn;

   function Is_Macro (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length = 1, "macro?: expected 1 argument");

      return New_Result
         (New_Boolean (Args.First_Element.K = Kind_Lambda and then
                       Args.First_Element.Fn.Is_Macro));
   end Is_Macro;

   function Is_String (Args : List; E : Env_Ptr) return Call_Result is
      (Is_Of_Type (Args, Kind_String, "string?"));

   function Is_Number (Args : List; E : Env_Ptr) return Call_Result is
      (Is_Of_Type (Args, Kind_Number, "number?"));

   function Is_List (Args : List; E : Env_Ptr) return Call_Result is
      (Is_Of_Type (Args, Kind_List, "list?"));

   function Is_Empty (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length = 1, "empty?: expected 1 argument");

      declare
         A      : constant Ast := Args.First_Element;
         Is_Seq : constant Boolean := A.K in Kind_Seq | Kind_Map;
      begin
         return New_Result (New_Boolean (Is_Seq and then A.List.Is_Empty));
      end;
   end Is_Empty;

   function Lst (Args : List; E : Env_Ptr) return Call_Result is
      (New_Result (New_List (Args)));

   function Eval_Do (Args : List; E : Env_Ptr) return Call_Result is
      A : Ast := Nil;
   begin
      for C in Args.Iterate loop
         if C = Args.Last then
            return (K => Call_Tail, A => IV.Element (C), E => E);
         end if;

         A := Eval (IV.Element (C), E);
      end loop;

      return New_Result (A);
   end Eval_Do;

   function Cons (Args : List; E : Env_Ptr) return Call_Result is
   begin
      Check (Args.Length = 2, "cons: expected 2 arguments");

      declare
         L   : List;
         Seq : Ast renames Args.Last_Element;
      begin
         Check (Seq.K in Kind_Seq, "cons: last argument must be a sequence");

         L.Append (Args.First_Element);
         L.Append (Seq.List.all);

         return New_Result (New_List (L));
      end;
   end Cons;

   function Concat (Args : List; E : Env_Ptr) return Call_Result is
      L : List;
   begin
      for Elem of Args loop
         L.Append (Elem.List.all);
      end loop;
      return New_Result (New_List (L));
   end Concat;

   function Vec (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length = 1, "vec: expected 1 argument");
      Check (Args.First_Element.K in Kind_Seq,
             "vec: first argument must be a sequence");

      return New_Result (New_Vector (Args.First_Element.List.all));
   end Vec;

   function Vector (Args : List; E : Env_Ptr) return Call_Result is
      (New_Result (New_Vector (Args.Copy)));

   function Nth (Args : List; E : Env_Ptr) return Call_Result is
   begin
      Check (Args.Length = 2, "nth: expected 2 arguments");

      declare
         Lst : Ast renames Args.First_Element;
         Num : Ast renames Args.Last_Element;
      begin
         Check (Lst.K in Kind_Seq, "nth: first argument must be a sequence");
         Check (Num.K = Kind_Number, "nth: second argument must be a number");
         Check (Num.Number in 0 .. Long_Integer (Lst.List.Length) - 1,
                "nth: index out of range");

         return New_Result
            (Lst.List.Element
               (Lst.List.First_Index + Integer (Num.Number)));
      end;
   end Nth;

   function First (Args : List; E : Env_Ptr) return Call_Result is
   begin
      Check (Args.Length = 1, "first: expected 1 argument");

      declare
         A : Ast renames Args.First_Element;
      begin
         return New_Result (if A.K in Kind_Seq and then not A.List.Is_Empty
                            then A.List.First_Element
                            else Nil);
      end;
   end First;

   function Rest (Args : List; E : Env_Ptr) return Call_Result is
   begin
      Check (Args.Length = 1, "rest: expected 1 argument");

      declare
         A : constant Ast := Args.First_Element;
         L : List;
      begin
         if A.K in Kind_Seq and then not A.List.Is_Empty then
            for I in A.List.First_Index + 1 .. A.List.Last_Index loop
               L.Append (A.List.Element (I));
            end loop;
         end if;

         return New_Result (New_List (L));
      end;
   end Rest;

   function Hash_Map (Args : List; E : Env_Ptr) return Call_Result is
   begin
      Check (Args.Length mod 2 = 0,
             "hash-map: expected even number of arguments");

      return New_Result (New_Map (Args));
   end Hash_Map;

   function Keys (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
      L : List;
   begin
      Check (Args.Length = 1, "keys: expected 1 argument");
      Check (Args.First_Element.K = Kind_Map,
             "keys: first argument must be a map");

      for C in Args.First_Element.HMap.Iterate loop
         L.Append (IM.Key (C));
      end loop;

      return New_Result (New_List (L));
   end Keys;

   function Values (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
      L : List;
   begin
      Check (Args.Length = 1, "vals: expected 1 argument");
      Check (Args.First_Element.K = Kind_Map,
             "vals: first argument must be a map");

      for Elem of Args.First_Element.HMap.all loop
         L.Append (Elem);
      end loop;

      return New_Result (New_List (L));
   end Values;

   function Assoc (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
   begin
      Check (Args.Length >= 1, "assoc: expected at least 1 argument");
      Check (Args.Length mod 2 = 1,
             "assoc: must have an odd number of arguments");

      declare
         A       : Ast renames Args.First_Element;
         M, K, V : Ast;
         C       : IV.Cursor := IV.Next (Args.First);
      begin
         Check (A.K in Kind_Map, "assoc: first argument must be a map");

         M := New_Map (A.HMap.Copy);
         while IV.Has_Element (C) loop
            K := IV.Element (C); IV.Next (C);
            V := IV.Element (C); IV.Next (C);
            M.HMap.Include (K, V);
         end loop;

         return New_Result (M);
      end;
   end Assoc;

   function Dissoc (Args : List; E : Env_Ptr) return Call_Result is
   begin
      Check (Args.Length >= 1, "dissoc: expected at least 1 argument");

      declare
         A    : Ast renames Args.First_Element;
         M, K : Ast;
         C    : IV.Cursor := IV.Next (Args.First);
      begin
         Check (A.K in Kind_Map, "dissoc: first argument must be a map");

         M := New_Map (A.HMap.Copy);
         while IV.Has_Element (C) loop
            K := IV.Element (C);
            M.HMap.Exclude (K);
            IV.Next (C);
         end loop;

         return New_Result (M);
      end;
   end Dissoc;

   function Eval_Map (Args : List; E : Env_Ptr) return Call_Result is
   begin
      Check (Args.Length = 2, "map: expected 2 arguments");

      declare
         L, A : List;
         F    : Ast renames Args.First_Element;
         Seq  : Ast renames Args.Last_Element;
      begin
         Check (F.K in Kind_Fn, "map: first argument must be a function");
         Check (Seq.K in Kind_Seq, "map: last argument must be a sequence");

         --  We want to have TCO without reimplementing it here.
         --  Thus, Eval is called directly.
         A.Append (F);
         A.Append (Nil); --  preallocate storage for the argument

         for Elem of Seq.List.all loop
            A.Replace_Element (A.Last, New_List ("quote", Elem));
            L.Append (Eval (New_List (A), E));
         end loop;

         return New_Result (New_List (L));
      end;
   end Eval_Map;

   function Map_Get (Args : List; E : Env_Ptr) return Call_Result is
   begin
      Check (Args.Length = 2, "get: expected 2 arguments");

      declare
         M : Ast renames Args.First_Element;
         K : Ast renames Args.Last_Element;
         C : IM.Cursor;
      begin
         if M.K = Kind_Nil then
            return New_Result (Nil);
         end if;

         Check (M.K = Kind_Map, "get: first argument must be a map");

         C := M.HMap.Find (K);
         return New_Result (if IM.Has_Element (C)
                            then IM.Element (C)
                            else Nil);
      end;
   end Map_Get;

   function Map_Contains (Args : List; E : Env_Ptr) return Call_Result is
      M   : Ast renames Args.First_Element;
      K   : Ast renames Args.Last_Element;
      Has : Boolean;
   begin
      Check (Args.Length = 2, "contains?: expected 2 arguments");

      Has := IM.Has_Element (M.HMap.Find (K));
      return New_Result (New_Boolean (Has));
   end Map_Contains;

   function Apply (Args : List; E : Env_Ptr) return Call_Result is
   begin
      Check (Args.Length >= 2, "apply: expected at least 2 arguments");
      Check (Args.First_Element.K in Kind_Fn,
             "apply: first argument must be a function");
      if Args.Length > 2 then
         Check (Args.Last_Element.K in Kind_Seq,
                "apply: last argument must be a sequence");
      end if;

      declare
         L : List;
         C : IV.Cursor := IV.Next (Args.First);
      begin
         if IV.Has_Element (C) then
            while C /= Args.Last loop
               L.Append (IV.Element (C));
               IV.Next (C);
            end loop;

            L.Append (IV.Element (C).List.all);
         end if;

         return (K => Call_Tail_Fn,
                 A => Args.First_Element,
                 Args => L,
                 EE => E);
      end;
   end Apply;

   function Prn (Args : List; E : Env_Ptr) return Call_Result is
      (Prn_Helper (Args, E, Op_Prn));

   function Println (Args : List; E : Env_Ptr) return Call_Result is
      (Prn_Helper (Args, E, Op_Println));

   function Str (Args : List; E : Env_Ptr) return Call_Result is
      (Prn_Helper (Args, E, Op_Str));

   function PrStr (Args : List; E : Env_Ptr) return Call_Result is
      (Prn_Helper (Args, E, Op_PrStr));

   function Slurp (Args : List; E : Env_Ptr) return Call_Result is
   begin
      Check (Args.Length = 1, "slurp: expected 1 argument");
      Check (Args.First_Element.K = Kind_String,
             "slurp: argument must be a string");

      return New_Result (New_String (Read_File (Args.First_Element.Str.all)));
   end Slurp;

   function Read_String (Args : List; E : Env_Ptr) return Call_Result is
      R : Reader.Instance;
   begin
      Check (Args.Length = 1, "read-string: expected 1 argument");
      Check (Args.First_Element.K = Kind_String,
             "read-string: argument must be a string");

      begin
         R.Init (Args.First_Element.Str.all);
         return New_Result (R.Read_Form);
      exception
         when E : Reader.Read_Error =>
            Raise_Mal_Error (
               New_String (Ada.Exceptions.Exception_Message (E)));
      end;
      return New_Result (Nil);
   end Read_String;

   function Readline (Args : List; E : Env_Ptr) return Call_Result is
   begin
      Check (Args.Length = 1, "readline: expected 1 argument");
      Check (Args.First_Element.K = Kind_String,
             "readline: argument must be a string");

      return New_Result
         (New_String
            (GNATCOLL.Readline.Get_Line
               (Args.First_Element.Str.all)));
   end Readline;

   function Eval_Eval (Args : List; E : Env_Ptr) return Call_Result is
      EE : Env_Ptr := E;
   begin
      --  Eval executes in the REPL environment.
      while EE.Outer /= null loop
         EE := EE.Outer;
      end loop;
      return (Call_Tail, Args.First_Element, EE);
   end Eval_Eval;

   function Symbol (Args : List; E : Env_Ptr) return Call_Result is
      (New_Result (New_Symbol (Args.First_Element.Str.all)));

   function Keyword (Args : List; E : Env_Ptr) return Call_Result is
      A : Ast renames Args.First_Element;
   begin
      if A.K = Kind_Keyword then
         return New_Result (A);
      end if;

      return New_Result (New_Keyword (A.Str.all));
   end Keyword;

   function Atom (Args : List; E : Env_Ptr) return Call_Result is
      (New_Result (New_Atom (Args.First_Element)));

   function Is_Atom (Args : List; E : Env_Ptr) return Call_Result is
      (Is_Of_Type (Args, Kind_Atom, "atom?"));

   function Deref (Args : List; E : Env_Ptr) return Call_Result is
      (New_Result (Args.First_Element.Ptr.all));

   function Reset (Args : List; E : Env_Ptr) return Call_Result is
      pragma Unreferenced (E);
      Atom : constant Ast := Args.First_Element;
      Val  : constant Ast := Args.Last_Element;
   begin
      Check (Args.Length = 2, "reset!: expected 2 arguments");

      Atom.Ptr.all := Val;
      return New_Result (Val);
   end Reset;

   function Swap (Args : List; E : Env_Ptr) return Call_Result is
   begin
      Check (Args.Length >= 2, "swap!: expected at least 2 arguments");

      declare
         Atom : constant Ast := Args.First_Element;
         C    : IV.Cursor := IV.Next (Args.First);
         Fn   : constant Ast := IV.Element (C);
         A    : List;
      begin
         Check (Atom.K = Kind_Atom, "swap!: first argument must be an atom");
         Check (Fn.K in Kind_Fn, "swap!: second argument must be a function");

         A.Append (Fn);
         A.Append (New_List ("quote", Atom.Ptr.all));

         IV.Next (C);
         while IV.Has_Element (C) loop
            A.Append (New_List ("quote", IV.Element (C)));
            IV.Next (C);
         end loop;

         Atom.Ptr.all := Eval (New_List (A), E);
         return New_Result (Atom.Ptr.all);
      end;
   end Swap;

   --
   --  Private functions.
   --
   function Eval_Arith (Args : List;
                        Op   : Arith_Op) return Call_Result is
      Acc : Long_Integer := (if Op = Op_Mul then 1 else 0);
   begin
      for C in Args.Iterate loop
         declare
            Elt : constant Ast := IV.Element (C);
         begin
            Check (Elt.K = Kind_Number,
                   Arith_Op_Name (Op) & ": argument must be a number");

            if C = Args.First then
               Acc := Elt.Number;
            else
               Acc := (case Op is
                       when Op_Add => Acc + Elt.Number,
                       when Op_Sub => Acc - Elt.Number,
                       when Op_Mul => Acc * Elt.Number,
                       when Op_Div => Acc / Elt.Number);
            end if;
         end;
      end loop;

      return New_Result (New_Number (Acc));
   end Eval_Arith;

   function Eval_Bool (Args : List;
                       Op   : Bool_Op) return Call_Result is
      A, B : Ast;
      R    : Boolean;
   begin
      Check (Args.Length = 2, Bool_Op_Name (Op) & ": expected 2 arguments");

      A := Args.First_Element;
      B := Args.Last_Element;
      if Op in Op_Lt .. Op_Ge then
         Check (A.K = Kind_Number and B.K = Kind_Number,
                Bool_Op_Name (Op) & ": argument must be a number");
      end if;

      R := (case Op is
         when Op_Eq => A = B,
         when Op_Ne => A /= B,
         when Op_Lt => A.Number < B.Number,
         when Op_Gt => A.Number > B.Number,
         when Op_Le => A.Number <= B.Number,
         when Op_Ge => A.Number >= B.Number);

      return New_Result (New_Boolean (R));
   end Eval_Bool;

   function Prn_Helper (Args      : List;
                        E         : Env_Ptr;
                        Op        : Str_Op) return Call_Result is
      S         : SU.Unbounded_String;
      Readably  : constant Boolean := Op in Op_PrStr | Op_Prn;
      Separator : constant String := (if Op = Op_Str then "" else " ");
      Print     : constant Boolean := Op in Op_Prn | Op_Println;
   begin
      if not Args.Is_Empty then
         for C in Args.Iterate loop
            Printer.Append (S, IV.Element (C), Readably);
            if C /= Args.Last then
               SU.Append (S, Separator);
            end if;
         end loop;
      end if;

      if Print then
         Put_Line (SU.To_String (S));
         return New_Result (Nil);
      end if;

      return New_Result (New_String (SU.To_String (S)));
   end Prn_Helper;

   function Read_File (File_Name : String) return String is
      File_Size : constant Natural :=
         Natural (Ada.Directories.Size (File_Name));

      subtype File_String    is String (1 .. File_Size);
      package SIO is new Ada.Direct_IO (File_String);

      File      : SIO.File_Type;
      Contents  : File_String;
   begin
      SIO.Open  (File, Mode => SIO.In_File,
                       Name => File_Name);
      SIO.Read  (File, Item => Contents);
      SIO.Close (File);

      return Contents;
   end Read_File;

   function Is_True (A : Ast) return Boolean is
   begin
      return (if A.K = Kind_Boolean then A.Bool else A.K /= Kind_Nil);
   end Is_True;
end Eval.Builtin;