with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Config_File is

   type Configuration is private;

   --  Load configuration from file
   procedure Load (Config : in out Configuration; Filename : String);

   --  Get string value (returns empty string if not found)
   function Get (Config : Configuration;
                 Section, Key : String) return String;

   --  Get value with default
   function Get (Config : Configuration;
                 Section, Key : String;
                 Default : String) return String;

   --  Get integer value
   function Get_Int (Config : Configuration;
                     Section, Key : String;
                     Default : Integer := 0) return Integer;

   --  Get boolean value
   function Get_Bool (Config : Configuration;
                      Section, Key : String;
                      Default : Boolean := False) return Boolean;

   --  Check if key exists
   function Has_Key (Config : Configuration;
                     Section, Key : String) return Boolean;

private

   package Config_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Configuration is record
      Data : Config_Map.Map;
   end record;

   function Make_Key (Section, Key : String) return String;

end Config_File;
