with Ada.Real_Time;
with Log;
with App_Global;
with Wake_On_Lan;
with UUID_Timestamp_Map; use UUID_Timestamp_Map;

package body WoL_Task is
   use Ada.Real_Time;

   protected body Status_Monitor is
      function Get_Status return NAS_Status is
      begin
         return Current_Status;
      end Get_Status;

      procedure Set_Status (New_Status : NAS_Status) is
      begin
         Current_Status := New_Status;
      end Set_Status;
   end Status_Monitor;

   task body Worker is
      WoL_Interval : Time_Span := Seconds (3600);
      Block_Time   : Time_Span := Seconds (3600);
      Is_Running   : Boolean := True;
      Status       : NAS_Status := NOT_STARTED;

      function To_Dur (TS : Time_Span) return Duration is
      begin
         return To_Duration (TS);
      end To_Dur;

   begin
      Status_Monitor.Set_Status (Status);
      Log.Debug ("Task started, initial status: NAS_OFFLINE");

      while Is_Running loop

         case Status is
            when NOT_STARTED =>
               Log.Debug ("NOT_STARTED");
               select
                  accept Start do
                     Log.Debug ("Received Start");
                     WoL_Interval := Seconds (App_Global.WoL_Interval);
                     Block_Time := Seconds (App_Global.NAS_Shutdown);
                     Log.Debug ("WoL_Interval is " & WoL_Interval'Image);
                     Log.Debug ("Block_Time is " & Block_Time'Image);
                     Status := NAS_OFFLINE;
                     Status_Monitor.Set_Status (Status);
                  end Start;
               or
                  accept Shutdown do
                     Log.Debug ("Received Shutdown in NOT_STARTED");
                     Is_Running := False;
                  end Shutdown;
               end select;

            when NAS_OFFLINE =>
               Log.Debug ("NAS_OFFLINE");
               Status_Map.Cleanup_Dead;
               select
                  accept Shutdown do
                     Log.Debug ("Received Shutdown in NAS_OFFLINE");
                     Is_Running := False;
                  end Shutdown;
               or
                  accept Pause do
                     Log.Error ("Received Pause in NAS_SHUTDOWN");
                  end Pause;
               or
                  accept Continue do
                     Log.Debug ("Received Continue in NAS_OFFLINE");
                     if Status = NAS_ONLINE then
                        Log.Debug ("Sending WoL");
                        Wake_On_Lan.Send;
                     else
                        Log.Warning ("Spurious Continue");
                     end if;
                  end Continue;
               end select;

            when NAS_ONLINE =>
               Log.Debug ("NAS_ONLINE");
               Status_Map.Cleanup_Dead;
               select
                  accept Shutdown do
                     Log.Debug ("Shutdown received");
                     Status := NAS_OFFLINE;
                     Status_Monitor.Set_Status (Status);
                     Is_Running := False;
                  end Shutdown;
               or
                  accept Pause do
                     Log.Debug ("Pause received");
                     Status := NAS_SHUTDOWN;
                     Status_Monitor.Set_Status (Status);
                  end Pause;
               or
                  accept Continue do
                     Log.Error ("Received Continue in NAS_SHUTDOWN");
                  end Continue;
               else
                  Log.Debug ("Delay {1}s for next WoL", WoL_Interval'Image);
                  delay To_Dur (WoL_Interval);
                  Log.Debug ("Delay finished");
                  if Status = NAS_ONLINE then
                     Wake_On_Lan.Send;
                  end if;
               end select;

            when NAS_SHUTDOWN =>
               Status_Map.Cleanup_Dead;
               select
                  accept Shutdown do
                     Log.Debug ("Emergency shutdown during block");
                     Status := NAS_OFFLINE;
                     Status_Monitor.Set_Status (Status);
                     Is_Running := False;
                  end Shutdown;
               or
                  accept Pause do
                     Log.Error ("Received Pause in NAS_SHUTDOWN");
                  end Pause;
               or
                  accept Continue do
                     Log.Error ("Received Continue in NAS_SHUTDOWN");
                  end Continue;
               else
                  Log.Debug ("Delay {1}s for NAS shutdown", Block_Time'Image);
                  delay To_Dur (Block_Time);
                  Log.Debug ("Block period completed");
               end select;
         end case;
      end loop;

      Log.Info ("WoL_Task terminated");
   end Worker;

   procedure Start is
   begin
      Worker.Start;
   end Start;

   procedure Pause is
   begin
      Worker.Pause;
   end Pause;

   procedure Continue is
   begin
      Worker.Continue;
   end Continue;

   procedure Shutdown is
   begin
      Worker.Shutdown;
   end Shutdown;

   function Status return NAS_Status is
   begin
      return Status_Monitor.Get_Status;
   end Status;

end WoL_Task;
