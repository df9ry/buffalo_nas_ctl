with Ada.Strings.Maps;
with Interfaces;

package Mac_Address_Parser is

   type Mac_Address is array (1 .. 6) of Interfaces.Unsigned_8;

   function To_Mac_Address (S : String) return Mac_Address
     with
       Pre  => S'Length = 17 or else S'Length = 12;

private

   Hex_Digits : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set ("0123456789ABCDEFabcdef");

end Mac_Address_Parser;
