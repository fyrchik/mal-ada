with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Types; use Types;

package Eval is
   --
   --  Exceptions in target language.
   --  Because mal is single-threaded, global variables suffice.
   --
   Is_Throwed : Boolean := False;
   Throwed    : Ast;
   Mal_Error  : exception;

   type Env;

   package HM is new Ada.Containers.Indefinite_Hashed_Maps (
      Key_Type => String,
      Element_Type => Ast,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "="
   );

   type Env is record
      Data  : HM.Map;
      Outer : Env_Ptr_Null;
   end record;

   function Find (E : Env_Ptr; Name : String; R : out Ast) return Boolean;
   procedure Set (E : Env_Ptr; Name : String; V : Ast);
   function Eval (V : Types.Ast; E : Env_Ptr) return Types.Ast;
   procedure Bind (E : Env_Ptr; Binds : List; Exprs : List);

   function New_Env (Outer : Env_Ptr_Null := null) return Env_Ptr;

   pragma Warnings (Off, "postcondition does not check the outcome of calling ""Check"""); 
   procedure Check (Condition : Boolean; Message : String)
      with Inline,
           Post => Condition;
   pragma Warnings (Off, "postcondition does not check the outcome of calling ""Check""");

   procedure Raise_Mal_Error (V : Ast)
      with No_Return,
           Inline;
   procedure Consume_Mal_Error (V : out Ast)
      with Pre => Is_Throwed,
           Post => not Is_Throwed;

private
   Eval_Error : exception;

   function Eval_Ast (V : Ast; E : Env_Ptr) return Ast;
end Eval;