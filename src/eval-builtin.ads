package Eval.Builtin is
   function Macroexpand_Helper (Arg : Ast; E : Env_Ptr) return Ast;

   --
   --  Math builtins.
   --
   function Add (Args : List; E : Env_Ptr) return Call_Result;
   function Sub (Args : List; E : Env_Ptr) return Call_Result;
   function Mul (Args : List; E : Env_Ptr) return Call_Result;
   function Div (Args : List; E : Env_Ptr) return Call_Result;

   --  Special forms.
   function Def (Args : List; E : Env_Ptr) return Call_Result;
   function Defmacro (Args : List; E : Env_Ptr) return Call_Result;
   function Is_Macro_Call (Args : List; E : Env_Ptr) return Call_Result;
   function Macroexpand (Args : List; E : Env_Ptr) return Call_Result;
   function Let (Args : List; E : Env_Ptr) return Call_Result;
   function Eval_If (Args : List; E : Env_Ptr) return Call_Result;
   function Eval_Fn (Args : List; E : Env_Ptr) return Call_Result;
   function Quote (Args : List; E : Env_Ptr) return Call_Result;
   function Quasiquote (Args : List; E : Env_Ptr) return Call_Result;
   function Quasiquote_Expand (Args : List; E : Env_Ptr) return Call_Result;
   function Eval_Do (Args : List; E : Env_Ptr) return Call_Result;
   function Try (Args : List; E : Env_Ptr) return Call_Result;
   function Catch (Args : List; E : Env_Ptr) return Call_Result;
   function Apply (Args : List; E : Env_Ptr) return Call_Result;

   --  Not a special, but goes near try/catch.
   function Throw (Args : List; E : Env_Ptr) return Call_Result;

   function Time_Ms (Args : List; E : Env_Ptr) return Call_Result;
   function Meta (Args : List; E : Env_Ptr) return Call_Result;
   function With_Meta (Args : List; E : Env_Ptr) return Call_Result;
   function Seq (Args : List; E : Env_Ptr) return Call_Result;
   function Conj (Args : List; E : Env_Ptr) return Call_Result;

   --  Boolean builtins.
   function Eq (Args : List; E : Env_Ptr) return Call_Result;
   function Ne (Args : List; E : Env_Ptr) return Call_Result;
   function Lt (Args : List; E : Env_Ptr) return Call_Result;
   function Gt (Args : List; E : Env_Ptr) return Call_Result;
   function Le (Args : List; E : Env_Ptr) return Call_Result;
   function Ge (Args : List; E : Env_Ptr) return Call_Result;
   function Eval_Not (Args : List; E : Env_Ptr) return Call_Result;
   function Is_True (Args : List; E : Env_Ptr) return Call_Result;
   function Is_False (Args : List; E : Env_Ptr) return Call_Result;
   function Is_Nil (Args : List; E : Env_Ptr) return Call_Result;
   function Is_Keyword (Args : List; E : Env_Ptr) return Call_Result;
   function Is_Symbol (Args : List; E : Env_Ptr) return Call_Result;
   function Is_Vector (Args : List; E : Env_Ptr) return Call_Result;
   function Is_Map (Args : List; E : Env_Ptr) return Call_Result;
   function Is_Sequential (Args : List; E : Env_Ptr) return Call_Result;
   function Is_Fn (Args : List; E : Env_Ptr) return Call_Result;
   function Is_Macro (Args : List; E : Env_Ptr) return Call_Result;
   function Is_String (Args : List; E : Env_Ptr) return Call_Result;
   function Is_Number (Args : List; E : Env_Ptr) return Call_Result;

   --  Collection builtins.
   function Count (Args : List; E : Env_Ptr) return Call_Result;
   function Is_List (Args : List; E : Env_Ptr) return Call_Result;
   function Is_Empty (Args : List; E : Env_Ptr) return Call_Result;
   function Lst (Args : List; E : Env_Ptr) return Call_Result;
   function Cons (Args : List; E : Env_Ptr) return Call_Result;
   function Concat (Args : List; E : Env_Ptr) return Call_Result;
   function Vec (Args : List; E : Env_Ptr) return Call_Result;
   function Vector (Args : List; E : Env_Ptr) return Call_Result;
   function Nth (Args : List; E : Env_Ptr) return Call_Result;
   function First (Args : List; E : Env_Ptr) return Call_Result;
   function Rest (Args : List; E : Env_Ptr) return Call_Result;
   function Hash_Map (Args : List; E : Env_Ptr) return Call_Result;
   function Keys (Args : List; E : Env_Ptr) return Call_Result;
   function Values (Args : List; E : Env_Ptr) return Call_Result;
   function Assoc (Args : List; E : Env_Ptr) return Call_Result;
   function Dissoc (Args : List; E : Env_Ptr) return Call_Result;
   function Eval_Map (Args : List; E : Env_Ptr) return Call_Result;
   function Map_Get (Args : List; E : Env_Ptr) return Call_Result;
   function Map_Contains (Args : List; E : Env_Ptr) return Call_Result;

   --  IO builtins.
   function Prn (Args : List; E : Env_Ptr) return Call_Result;
   function Println (Args : List; E : Env_Ptr) return Call_Result;
   function Str (Args : List; E : Env_Ptr) return Call_Result;
   function PrStr (Args : List; E : Env_Ptr) return Call_Result;
   function Slurp (Args : List; E : Env_Ptr) return Call_Result;
   function Read_String (Args : List; E : Env_Ptr) return Call_Result;
   function Readline (Args : List; E : Env_Ptr) return Call_Result;

   --  Atom builtins.
   function Atom (Args : List; E : Env_Ptr) return Call_Result;
   function Is_Atom (Args : List; E : Env_Ptr) return Call_Result;
   function Deref (Args : List; E : Env_Ptr) return Call_Result;
   function Reset (Args : List; E : Env_Ptr) return Call_Result;
   function Swap (Args : List; E : Env_Ptr) return Call_Result;

   --  Other builtins.
   function Eval_Eval (Args : List; E : Env_Ptr) return Call_Result;
   function Symbol (Args : List; E : Env_Ptr) return Call_Result;
   function Keyword (Args : List; E : Env_Ptr) return Call_Result;

private
   type Arith_Op is (Op_Add, Op_Sub, Op_Mul, Op_Div);

   function Arith_Op_Name (Op : Arith_Op) return String is
      (case Op is
       when Op_Add => "+",
       when Op_Sub => "-",
       when Op_Mul => "*",
       when Op_Div => "/")
      with Inline;
   pragma Pure_Function (Arith_Op_Name);

   function Eval_Arith (Args : List;
                        Op   : Arith_Op) return Call_Result
      with Inline;

   type Bool_Op is (Op_Eq, Op_Ne, Op_Lt, Op_Gt, Op_Le, Op_Ge);

   function Bool_Op_Name (Op : Bool_Op) return String is
      (case Op is
       when Op_Eq => "=",
       when Op_Ne => "not=",
       when Op_Lt => "<",
       when Op_Gt => ">",
       when Op_Le => "<=",
       when Op_Ge => ">=")
      with Inline;
   pragma Pure_Function (Bool_Op_Name);

   function Eval_Bool (Args : List;
                       Op   : Bool_Op) return Call_Result
      with Inline;

   type Str_Op is (Op_Str, Op_Prn, Op_Println, Op_PrStr);
   function Prn_Helper (Args : List;
                        E    : Env_Ptr;
                        Op   : Str_Op) return Call_Result;

   function Read_File (File_Name : String) return String;

   function Is_True (A : Ast) return Boolean with Inline;
   pragma Pure_Function (Is_True);
end Eval.Builtin;