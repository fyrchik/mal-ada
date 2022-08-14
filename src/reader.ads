with Ada.Strings.Unbounded;

with Types;

package Reader is
   package SU renames Ada.Strings.Unbounded;

   --
   --  Represents single reader instance.
   --
   type Instance is tagged private;

   --
   --  Creates new reader instance from string.
   --
   procedure Init (R : in out Instance; S : String);

   --
   --  Reads a single form from the reader.
   --
   function Read_Form (R : in out Instance) return Types.Ast;

   Read_Error : exception;
private

   type Instance is tagged record
      S     : SU.Unbounded_String;
      Index : Natural;
   end record;

   procedure Skip_Whitespace (R : in out Instance);

   function Peek (R : Instance; Offset : Natural := 0) return Character
      with Inline;
   pragma Pure_Function (Peek);

   function EOF (R : Instance) return Boolean
      with Inline;
   pragma Pure_Function (EOF);

   function Has (R : Instance; Offset : Natural) return Boolean
      with Inline;
   pragma Pure_Function (Has);

   procedure Next (R : in out Instance);

   function Is_Digit (C : Character) return Boolean is
      (C in '0' .. '9')
      with Inline;
   pragma Pure_Function (Is_Digit);

   function Is_Whitespace (C : Character) return Boolean
      with Inline;
   pragma Pure_Function (Is_Whitespace);

   function Is_Symbol_Char (C : Character) return Boolean is
      (C in 'a' .. 'z' | 'A' .. 'Z' | '_' |
            '+' | '*' | '/' | '-' | '%' | '&' |
            '>' | '<' | '?' | '!' | ':' | '=');
   pragma Pure_Function (Is_Symbol_Char);
end Reader;