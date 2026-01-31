with Ada.Containers.Vectors;

package body UUID_Timestamp_Map is

   protected body Protected_UUID_Timestamp_Map is

      procedure Put (Key : UUIDs.UUID) is
      begin
         Map.Include (Key, Ada.Calendar.Clock);
      end Put;

      procedure Put (Key : UUIDs.UUID; Timestamp : Ada.Calendar.Time) is
      begin
         Map.Include (Key, Timestamp);
      end Put;

      function Get (Key : UUIDs.UUID) return Ada.Calendar.Time is
      begin
         return Map.Element (Key);
      exception
         when Constraint_Error =>
            raise Constraint_Error with "UUID not found in map";
      end Get;

      function Contains (Key : UUIDs.UUID) return Boolean is
      begin
         return Map.Contains (Key);
      end Contains;

      procedure Remove (Key : UUIDs.UUID) is
      begin
         Map.Delete (Key);
      end Remove;

      procedure Clear is
      begin
         Map.Clear;
      end Clear;

      function Length return Natural is
      begin
         return Natural (Map.Length);
      end Length;

      procedure Cleanup_Dead (T : Ada.Calendar.Time) is
         package UUID_Vecs is new
           Ada.Containers.Vectors (Positive, UUIDs.UUID);
         To_Remove : UUID_Vecs.Vector;
      begin
         for Pos in Map.Iterate loop
            if Timestamp_Maps.Element (Pos) <= T then
               To_Remove.Append (Timestamp_Maps.Key (Pos));
            end if;
         end loop;

         for K of To_Remove loop
            Map.Delete (K);
         end loop;
      end Cleanup_Dead;

      function Has_Entries return Boolean
      is (Integer (Map.Length) > 0);

   end Protected_UUID_Timestamp_Map;

end UUID_Timestamp_Map;
