with Ada.Containers;

with Eval.Builtin; use Eval.Builtin;

package body Eval is
   function Find (E : Env_Ptr; Name : String; R : out Ast) return Boolean is
      C : HM.Cursor;
   begin
      C := E.Data.Find (Name);
      if HM.Has_Element (C) then
         R := HM.Element (C);
         return True;
      end if;

      return E.Outer /= null and then Find (E.Outer, Name, R);
   end Find;

   procedure Set (E : Env_Ptr; Name : String; V : Ast) is
   begin
      E.Data.Include (Name, V);
   end Set;

   function Eval_Function (F : Ast; Args : List; E : Env_Ptr) return Call_Result is
      EE : Env_Ptr := E;
   begin
      case F.K is
         when Kind_Function =>
            return F.Func.all (Args, EE);
         when Kind_Lambda =>
            EE := New_Env (F.Fn.Env);
            Bind (EE, F.Fn.Params, Args);
            return (Call_Tail, F.Fn.Expr, EE);
         when others =>
            raise Eval_Error with "Eval_Fn: not a function";
      end case;
   end Eval_Function;

   function Eval (V : Ast; E : Env_Ptr) return Ast is
      R : Call_Result := (Call_Tail, V, E);
   begin
      loop
         case R.K is
            when Call_Normal => return R.A;
            when Call_Tail_Fn =>
               R := Eval_Function (R.A, R.Args, R.EE);
            when Call_Tail =>
               if R.A.K = Kind_List then
                  R.A := Macroexpand_Helper (R.A, R.E);
               end if;

               if R.A.K /= Kind_List then
                  return Eval_Ast (R.A, R.E);
               elsif R.A.List.Is_Empty then
                  return R.A;
               end if;

               declare
                  F         : constant Ast := Eval (R.A.List.First_Element, R.E);
                  L         : List;
                  C         : IV.Cursor := IV.Next (R.A.List.First);
               begin
                  while IV.Has_Element (C) loop
                     if F.K = Kind_Function and then F.Special then
                        L.Append (IV.Element (C));
                     else
                        L.Append (Eval (IV.Element (C), R.E));
                     end if;
                     IV.Next (C);
                  end loop;

                  R := Eval_Function (F, L, R.E);
               end;
         end case;
      end loop;
   end Eval;

   procedure Bind (E : Env_Ptr; Binds : List; Exprs : List) is
      J : Integer;
      L : List;
   begin
      for I in Binds.First_Index .. Binds.Last_Index loop
         J := Exprs.First_Index + I - Binds.First_Index;
         if Binds (I).Str.all = "&" then
            for K in J .. Exprs.Last_Index loop
               L.Append (Exprs (K));
            end loop;
            Set (E, Binds (I + 1).Str.all, New_List (L));
            return;
         end if;

         Set (E, Binds (I).Str.all, Exprs (J));
      end loop;
   end Bind;

   function Eval_Ast (V : Ast; E : Env_Ptr) return Ast is
      R : Ast;
      L : List;
   begin
      case V.K is
         when Kind_Symbol =>
            if not Find (E, V.Str.all, R) then
               Raise_Mal_Error (New_String ("'" & V.Str.all & "' not found"));
            end if;
            return R;
         when Kind_Map =>
            --  Always copy the map so that assoc and dissoc
            --  don't need to perform additional allocations.
            declare
               M : Map;
               C : IM.Cursor := V.HMap.First;
            begin
               while IM.Has_Element (C) loop
                  M.Include (IM.Key (C), Eval (IM.Element (C), E));
                  IM.Next (C);
               end loop;
               return New_Map (M);
            end;
         when Kind_List | Kind_Vector =>
            for Elem of V.List.all loop
               L.Append (Eval (Elem, E));
            end loop;
            return (case V.K is
                    when Kind_List => New_List (L),
                    when Kind_Vector => New_Vector (L),
                    when others => raise Eval_Error with "unreachable");
         when others => return V;
      end case;
   end Eval_Ast;

   procedure Check (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         Raise_Mal_Error (New_String (Message));
      end if;
   end Check;

   procedure Raise_Mal_Error (V : Ast) is
   begin
      Is_Throwed := True;
      Throwed := V;
      raise Mal_Error;
   end Raise_Mal_Error;

   procedure Consume_Mal_Error (V : out Ast) is
   begin
      Is_Throwed := False;
      V := Throwed;
      Throwed := Nil;
   end Consume_Mal_Error;

   function New_Env (Outer : Env_Ptr_Null := null) return Env_Ptr is
      E : constant Env_Ptr := new Env;
   begin
      if Outer /= null then
         E.Outer := Outer;
         return E;
      end if;

      E.Data.Include ("def!", New_Special (Def'Access));
      E.Data.Include ("defmacro!", New_Special (Defmacro'Access));
      E.Data.Include ("is_macro_call", New_Special (Is_Macro_Call'Access));
      E.Data.Include ("macroexpand", New_Special (Macroexpand'Access));
      E.Data.Include ("let*", New_Special (Let'Access));
      E.Data.Include ("if", New_Special (Eval_If'Access));
      E.Data.Include ("fn*", New_Special (Eval_Fn'Access));
      E.Data.Include ("quote", New_Special (Quote'Access));
      E.Data.Include ("quasiquote", New_Special (Quasiquote'Access));
      E.Data.Include ("quasiquoteexpand",
                      New_Special (Quasiquote_Expand'Access));
      E.Data.Include ("try*", New_Special (Try'Access));
      E.Data.Include ("catch*", New_Special (Catch'Access));
      E.Data.Include ("do", New_Special (Eval_Do'Access));

      E.Data.Include ("eval", New_Function (Eval_Eval'Access));

      E.Data.Include ("time-ms", New_Function (Time_Ms'Access));
      E.Data.Include ("meta", New_Function (Meta'Access));
      E.Data.Include ("with-meta", New_Function (With_Meta'Access));
      E.Data.Include ("string?", New_Function (Is_String'Access));
      E.Data.Include ("number?", New_Function (Is_Number'Access));
      E.Data.Include ("seq", New_Function (Seq'Access));
      E.Data.Include ("conj", New_Function (Conj'Access));

      E.Data.Include ("apply", New_Function (Apply'Access));
      E.Data.Include ("+", New_Function (Add'Access));
      E.Data.Include ("-", New_Function (Sub'Access));
      E.Data.Include ("*", New_Function (Mul'Access));
      E.Data.Include ("/", New_Function (Div'Access));
      E.Data.Include ("throw", New_Function (Throw'Access));
      E.Data.Include ("=", New_Function (Eq'Access));
      E.Data.Include ("not=", New_Function (Ne'Access));
      E.Data.Include ("<", New_Function (Lt'Access));
      E.Data.Include (">", New_Function (Gt'Access));
      E.Data.Include ("<=", New_Function (Le'Access));
      E.Data.Include (">=", New_Function (Ge'Access));
      E.Data.Include ("not", New_Function (Eval_Not'Access));
      E.Data.Include ("true?", New_Function (Is_True'Access));
      E.Data.Include ("false?", New_Function (Is_False'Access));
      E.Data.Include ("nil?", New_Function (Is_Nil'Access));
      E.Data.Include ("keyword?", New_Function (Is_Keyword'Access));
      E.Data.Include ("symbol?", New_Function (Is_Symbol'Access));
      E.Data.Include ("vector?", New_Function (Is_Vector'Access));
      E.Data.Include ("map?", New_Function (Is_Map'Access));
      E.Data.Include ("fn?", New_Function (Is_Fn'Access));
      E.Data.Include ("macro?", New_Function (Is_Macro'Access));
      E.Data.Include ("sequential?", New_Function (Is_Sequential'Access));
      E.Data.Include ("count", New_Function (Count'Access));
      E.Data.Include ("list?", New_Function (Is_List'Access));
      E.Data.Include ("empty?", New_Function (Is_Empty'Access));
      E.Data.Include ("list", New_Function (Lst'Access));
      E.Data.Include ("cons", New_Function (Cons'Access));
      E.Data.Include ("concat", New_Function (Concat'Access));
      E.Data.Include ("vec", New_Function (Vec'Access));
      E.Data.Include ("vector", New_Function (Vector'Access));
      E.Data.Include ("nth", New_Function (Nth'Access));
      E.Data.Include ("first", New_Function (First'Access));
      E.Data.Include ("rest", New_Function (Rest'Access));
      E.Data.Include ("hash-map", New_Function (Hash_Map'Access));
      E.Data.Include ("keys", New_Function (Keys'Access));
      E.Data.Include ("vals", New_Function (Values'Access));
      E.Data.Include ("assoc", New_Function (Assoc'Access));
      E.Data.Include ("dissoc", New_Function (Dissoc'Access));
      E.Data.Include ("map", New_Function (Eval_Map'Access));
      E.Data.Include ("get", New_Function (Map_Get'Access));
      E.Data.Include ("contains?", New_Function (Map_Contains'Access));
      E.Data.Include ("prn", New_Function (Prn'Access));
      E.Data.Include ("println", New_Function (Println'Access));
      E.Data.Include ("str", New_Function (Str'Access));
      E.Data.Include ("pr-str", New_Function (PrStr'Access));
      E.Data.Include ("slurp", New_Function (Slurp'Access));
      E.Data.Include ("read-string", New_Function (Read_String'Access));
      E.Data.Include ("readline", New_Function (Readline'Access));
      E.Data.Include ("symbol", New_Function (Symbol'Access));
      E.Data.Include ("keyword", New_Function (Keyword'Access));
      E.Data.Include ("atom", New_Function (Atom'Access));
      E.Data.Include ("atom?", New_Function (Is_Atom'Access));
      E.Data.Include ("deref", New_Function (Deref'Access));
      E.Data.Include ("reset!", New_Function (Reset'Access));
      E.Data.Include ("swap!", New_Function (Swap'Access));
      return E;
   end New_Env;

end Eval;