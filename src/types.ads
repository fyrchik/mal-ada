with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;

limited with Eval;

package Types is
   type Kind is (
      Kind_Nil,
      Kind_Boolean,
      Kind_Number,
      Kind_String,
      Kind_Symbol,
      Kind_Keyword,
      Kind_List,
      Kind_Vector,
      Kind_Map,
      Kind_Function,
      Kind_Lambda,
      Kind_Atom,
      Kind_None
   );

   subtype Kind_Seq is Kind
      with Static_Predicate => Kind_Seq in Kind_List | Kind_Vector;

   subtype Kind_Fn is Kind
      with Static_Predicate => Kind_Fn in Kind_Function | Kind_Lambda;

   type Ast;
   type Ast_Ptr_Null is access Ast;
   subtype Ast_Ptr is not null Ast_Ptr_Null;

   overriding
   function "=" (Left, Right : Ast) return Boolean;

   type List;
   type List_Ptr is not null access List;
   type Map;
   type Map_Ptr is not null access Map;
   type String_Ptr is not null access String;
   type Env_Ptr_Null is access all Eval.Env;
   subtype Env_Ptr is not null Env_Ptr_Null;
   type Call_Result;
   type Builtin_Ptr is not null
      access function (Args : List;
                       E    : Env_Ptr) return Call_Result;
   type Lambda;
   type Lambda_Ptr is not null access Lambda;

   --  Nested case is somewhat ugly but it seems like the only option to share
   --  a field between objects with different discriminant.
   type Ast (K : Kind := Kind_None) is record
      case K is
      when Kind_Function | Kind_Lambda |
           Kind_List | Kind_Vector | Kind_Map =>
         Meta : Ast_Ptr_Null;

         case K is
         when Kind_List | Kind_Vector =>
            List : List_Ptr;
         when Kind_Map =>
            HMap : Map_Ptr;
         when Kind_Function =>
            Func : Builtin_Ptr;
            Special : Boolean;
         when Kind_Lambda =>
            Fn : Lambda_Ptr;
         when others => null;
         end case;
      when Kind_Number =>
         Number : Long_Integer;
      when Kind_Symbol | Kind_String | Kind_Keyword =>
         Str : String_Ptr;
      when Kind_Boolean =>
         Bool : Boolean;
      when Kind_Atom =>
         Ptr : Ast_Ptr;
      when Kind_None | Kind_Nil =>
         null;
      end case;
   end record;

   type Ast_Array is array (Positive range <>) of Ast;

   --
   --  Public constants for convenience;
   --
   Nil        : constant Ast := (K => Kind_Nil);
   Bool_True  : constant Ast := (K => Kind_Boolean, Bool => True);
   Bool_False : constant Ast := (K => Kind_Boolean, Bool => False);

   package IV is new Ada.Containers.Vectors (
      Index_Type => Positive,
      Element_Type => Ast
   );
   type List is new IV.Vector with null record;
   type Lambda is record
      Params   : List;
      Expr     : Ast;
      Env      : Env_Ptr;
      Is_Macro : Boolean := False;
   end record;

   type Call_Result_Kind is (Call_Normal, Call_Tail, Call_Tail_Fn);
   type Call_Result (K : Call_Result_Kind := Call_Normal) is record
      A : Ast;
      case K is
         when Call_Tail =>
            E : Env_Ptr;
         when Call_Tail_Fn =>
            Args : List;
            EE   : Env_Ptr;
         when others => null;
      end case;
   end record;

   function Hash (A : Ast) return Ada.Containers.Hash_Type is
      (case A.K is
       when Kind_Nil => 0,
       when Kind_Number => Ada.Containers.Hash_Type (A.Number mod 2 ** 16),
       when Kind_String | Kind_Keyword =>
         Ada.Strings.Hash (A.Str.all),
       when others => 0);

   package IM is new Ada.Containers.Hashed_Maps (
      Key_Type => Ast,
      Element_Type => Ast,
      Hash => Hash,
      Equivalent_Keys => "="
   );
   type Map is new IM.Map with null record;

   --
   --  Basic operations.
   --
   function New_Number (N : Integer) return Ast is
      (K => Kind_Number, Number => Long_Integer (N));

   function New_Number (N : Long_Integer) return Ast is
      (K => Kind_Number, Number => N);

   function New_Boolean (V : Boolean) return Ast is
      (K => Kind_Boolean, Bool => V);

   function New_Symbol (S : String) return Ast is
      (K => Kind_Symbol, Str => new String'(S));

   function New_String (S : String) return Ast is
      (K => Kind_String, Str => new String'(S));

   function New_Keyword (S : String) return Ast is
      (K => Kind_Keyword, Str => new String'(S));

   function New_List (L : List) return Ast is
      (K => Kind_List, List => new List'(L), Meta => null);

   --  Useful helpers.
   function New_List (Sym : String; V : Ast) return Ast;
   function New_List (V : Ast) return Ast;

   function New_Vector (L : List) return Ast is
      (K => Kind_Vector, List => new List'(L), Meta => null);

   function New_Map (M : Map) return Ast is
      (K => Kind_Map, HMap => new Map'(M), Meta => null);

   function New_Map (L : List) return Ast;

   function New_Function (P : Builtin_Ptr) return Ast is
      (K => Kind_Function, Func => P, Special => False, Meta => null);

   function New_Special (P : Builtin_Ptr) return Ast is
      (K => Kind_Function, Func => P, Special => True, Meta => null);

   function New_Lambda (Params   : List;
                        Expr     : Ast;
                        E        : Env_Ptr;
                        Is_Macro : Boolean := False) return Ast is
      (K => Kind_Lambda,
       Meta => null,
       Fn => new Lambda'(Params => Params,
                         Expr => Expr,
                         Env => E,
                         Is_Macro => Is_Macro));

   function New_Atom (A : Ast) return Ast is
      (K => Kind_Atom, Ptr => new Ast'(A));

   function New_Result (A : Ast) return Call_Result is
      (K => Call_Normal, A => A);
private
   Invalid_Type : exception;
end Types;