with Ada.Strings.Maps;
with App_Global;

package Mac_Address_Parser is

   function To_Mac_Address (S : String) return App_Global.Mac_Address
     with
       Pre  => S'Length = 17 or else S'Length = 12;

private

   Hex_Digits : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set ("0123456789ABCDEFabcdef");

end Mac_Address_Parser;
