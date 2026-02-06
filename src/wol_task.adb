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
         Log.Debug ("Set status to " & New_Status'Image);
         Current_Status := New_Status;
      end Set_Status;
   end Status_Monitor;

   task body Worker is
      WoL_Interval : Time_Span := Seconds (3600);
      Block_Time   : Time_Span := Seconds (3600);
      Is_Running   : Boolean := True;

      function To_Dur (TS : Time_Span) return Duration is
      begin
         return To_Duration (TS);
      end To_Dur;

   begin
      Status_Monitor.Set_Status (NOT_STARTED);
      Log.Debug ("Task started, initial status: NAS_OFFLINE");

      while Is_Running loop
         Log.Debug ("In loop: Status is " & Status_Monitor.Get_Status'Image);
         case Status_Monitor.Get_Status is
            when NOT_STARTED =>
               select
                  accept Start do
                     Log.Debug ("Received Start");
                     WoL_Interval := Seconds (App_Global.WoL_Interval);
                     Block_Time := Seconds (App_Global.NAS_Shutdown);
                     Log.Debug ("WoL_Interval is " & WoL_Interval'Image);
                     Log.Debug ("Block_Time is " & Block_Time'Image);
                     Status_Monitor.Set_Status (NAS_OFFLINE);
                  end Start;
               or
                  accept Shutdown do
                     Log.Debug ("Received Shutdown in NOT_STARTED");
                     Is_Running := False;
                  end Shutdown;
               end select;

            when NAS_OFFLINE =>
               select
                  accept Shutdown do
                     Log.Debug ("Received Shutdown");
                     Is_Running := False;
                  end Shutdown;
               or
                  accept Pause do
                     Log.Error ("Received Pause in NAS_OFFLINE, ignored");
                  end Pause;
               or
                  accept Continue do
                     Log.Debug ("Received Continue in NAS_OFFLINE");
                     if Have_Active_Clients then
                        Status_Monitor.Set_Status (NAS_ONLINE);
                        Log.Debug ("Sending WoL");
                        Wake_On_Lan.Send;
                     else
                        Log.Warning ("Spurious Continue, ignored");
                     end if;
                  end Continue;
               end select;

            when NAS_ONLINE =>
               select
                  accept Shutdown do
                     Log.Debug ("Shutdown received");
                     Is_Running := False;
                  end Shutdown;
               or
                  accept Pause do
                     Log.Debug ("Pause received");
                     if not Have_Active_Clients then
                        Status_Monitor.Set_Status (NAS_SHUTDOWN);
                     else
                        Log.Warning ("Received spurious Pause, ignored");
                     end if;
                  end Pause;
               or
                  accept Continue do
                     Log.Warning ("Received Continue in NAS_ONLINE, ignored");
                  end Continue;
               else
                  Log.Debug ("Delay " & WoL_Interval'Image & "s for next WoL");
                  delay To_Dur (WoL_Interval);
                  Log.Debug ("Delay finished");
                  if Have_Active_Clients then
                     Wake_On_Lan.Send;
                  else
                     Status_Monitor.Set_Status (NAS_OFFLINE);
                  end if;
               end select;

            when NAS_SHUTDOWN =>
               select
                  accept Shutdown do
                     Log.Debug ("Shutdown during block");
                     Is_Running := False;
                  end Shutdown;
               or
                  accept Pause do
                     Log.Error ("Received Pause in NAS_SHUTDOWN, ignored");
                  end Pause;
               or
                  accept Continue do
                     Log.Error ("Received Continue in NAS_SHUTDOWN, ignored");
                  end Continue;
               else
                  Log.Debug ("Delay " & Block_Time'Image &
                               "s for NAS shutdown");
                  delay To_Dur (Block_Time);
                  Log.Debug ("Block period completed");
                  Status_Monitor.Set_Status (NAS_OFFLINE);
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

end WoL_Task;
