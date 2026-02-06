with Ada.Containers.Vectors;
with Ada.Exceptions;
with App_Global;
with WoL_Task;
with Log;

package body UUID_Timestamp_Map is

   protected body Protected_UUID_Timestamp_Map is

      procedure Cleanup_Dead is
         use Ada.Calendar;
         package UUID_Vecs is new
           Ada.Containers.Vectors (Positive, UUIDs.UUID);
         To_Remove : UUID_Vecs.Vector;
         T : constant Time := Clock;
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

      procedure Remove (Id : UUIDs.UUID) is
      begin
         Map.Delete (Id);
      end Remove;

      procedure Try_Put
        (Id : UUIDs.UUID; T : Ada.Calendar.Time; Result : out Boolean)
      is
         use WoL_Task;
         use Ada.Exceptions;
      begin
         if Status_Monitor.Get_Status = NAS_SHUTDOWN then
            Result := False;
         else
            Map.Include (Id, T);
            Result := True;
         end if;
      exception
         when E : others =>
            Log.Error ("Error: " & Exception_Message (E));
      end Try_Put;

   end Protected_UUID_Timestamp_Map;

   function Try_Put (Id : UUIDs.UUID) return Boolean
   is
      use Ada.Calendar;
      T : constant Time := Clock + Duration (2 * App_Global.WoL_Interval);
      Result : Boolean;
   begin
      Status_Map.Try_Put (Id, T, Result);
      return Result;
   end Try_Put;

end UUID_Timestamp_Map;
