with Ada.Strings.Fixed;

with Interfaces; use Interfaces;

package body Mac_Address_Parser is

   --------------------
   -- To_Mac_Address --
   --------------------

   function To_Mac_Address (S : String) return App_Global.Mac_Address is
      use Ada.Strings.Fixed;

      Result     : App_Global.Mac_Address := (others => 0);
      Clean      : String (1 .. 12);
      Pos        : Natural := Clean'First;
      Input      : constant String := Trim (S, Ada.Strings.Both);
   begin
      --  Nur Hex-Zeichen sammeln, : - und Leerzeichen ignorieren
      for C of Input loop
         if Ada.Strings.Maps.Is_In (C, Hex_Digits) then
            Clean (Pos) := C;
            Pos := Pos + 1;
         elsif C /= ':' and then C /= '-' and then C /= ' ' then
            raise Constraint_Error with "Ungültiges Zeichen in MAC-Adresse";
         end if;
      end loop;

      if Pos - 1 /= 12 then
         raise Constraint_Error with
           "MAC-Adresse hat nicht genau 12 Hex-Ziffern";
      end if;

      --  Je zwei Zeichen zu einem Byte machen
      for I in 1 .. 6 loop
         declare
            Hex_Pair : constant String :=
              Clean ((I - 1) * 2 + 1 .. (I - 1) * 2 + 2);
         begin
            Result (I) := Unsigned_8'Value ("16#" & Hex_Pair & "#");
         exception
            when Constraint_Error =>
               raise Constraint_Error with "Ungültiges Hex-Paar: " & Hex_Pair;
         end;
      end loop;

      return Result;
   end To_Mac_Address;

end Mac_Address_Parser;
