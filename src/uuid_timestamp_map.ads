pragma Ada_2022;

with Ada.Containers.Hashed_Maps;
with Ada.Calendar;
with Ada.Strings.Hash;
with UUIDs;

package UUID_Timestamp_Map is

   use type UUIDs.UUID;
   use type Ada.Calendar.Time;

   function Hash_UUID (Key : UUIDs.UUID) return Ada.Containers.Hash_Type
   is (Ada.Strings.Hash (Key'Image));

   function Equivalent_UUIDs (A, B : UUIDs.UUID) return Boolean
   is (A = B);

   package Timestamp_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => UUIDs.UUID,
        Element_Type    => Ada.Calendar.Time,
        Hash            => Hash_UUID,
        Equivalent_Keys => Equivalent_UUIDs);

   protected type Protected_UUID_Timestamp_Map is
      procedure Put (Key : UUIDs.UUID);
      procedure Put (Key : UUIDs.UUID; Timestamp : Ada.Calendar.Time);
      function Get (Key : UUIDs.UUID) return Ada.Calendar.Time;
      function Contains (Key : UUIDs.UUID) return Boolean;
      procedure Remove (Key : UUIDs.UUID);
      procedure Clear;
      function Length return Natural;
      procedure Cleanup_Dead (T : Ada.Calendar.Time);
      function Has_Entries return Boolean;
   private
      Map : Timestamp_Maps.Map;
   end Protected_UUID_Timestamp_Map;

   Status_Map : Protected_UUID_Timestamp_Map;

end UUID_Timestamp_Map;
