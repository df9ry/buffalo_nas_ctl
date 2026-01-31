with Ada.Real_Time;
with Log;
with App_Global;
with Wake_On_Lan;

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
      WoL_Interval  : Time_Span := Seconds (3600);
      Block_Time    : Time_Span := Seconds (3600);
      Next_WoL_Time : Time;
      Is_Running    : Boolean := True;
      Status        : NAS_Status := NOT_STARTED;

      function To_Dur (TS : Time_Span) return Duration is
      begin
         return To_Duration (TS);
      end To_Dur;

      procedure Log_Debug (Msg : String) is
      begin
         Log.Debug ("WoL_Task: " & Msg);
      end Log_Debug;

   begin
      Status_Monitor.Set_Status (Status);
      Log_Debug ("Task started, initial status: NAS_OFFLINE");

      while Is_Running loop

         case Status is
            when NOT_STARTED =>
               Log_Debug ("NOT_STARTED");
               select
                  accept Start do
                     Log_Debug ("Received Start");
                     WoL_Interval := Seconds (App_Global.WoL_Interval);
                     Block_Time := Seconds (App_Global.NAS_Shutdown);
                     Log.Debug ("WoL_Interval is " & WoL_Interval'Image);
                     Log.Debug ("Block_Time is " & Block_Time'Image);
                     Status := NAS_OFFLINE;
                     Status_Monitor.Set_Status (Status);
                  end Start;
               or
                  accept Shutdown do
                     Log_Debug ("Received Shutdown in NOT_STARTED");
                     Is_Running := False;
                  end Shutdown;
               end select;

            when NAS_OFFLINE =>
               Log_Debug ("NAS_OFFLINE");
               select
                  accept Shutdown do
                     Log_Debug ("Received Shutdown in NAS_OFFLINE");
                     Is_Running := False;
                  end Shutdown;
               end select;

            when NAS_ONLINE =>
               Log_Debug ("NAS_ONLINE - waiting until Next_WoL_Time");
               select
                  delay until Next_WoL_Time;
                  Log_Debug ("Delay finished, sending WoL");
                  Wake_On_Lan.Send;
                  Next_WoL_Time := Clock + WoL_Interval;
                  Log_Debug
                    ("Next_WoL_Time set to: "
                     & To_Duration (Next_WoL_Time - Clock)'Image
                     & "s from now");
               or
                  accept Shutdown do
                     Log_Debug ("Shutdown received");
                     Status := NAS_OFFLINE;
                     Status_Monitor.Set_Status (Status);
                     Is_Running := False;
                  end Shutdown;
               or
                  accept Pause do
                     Log_Debug ("Pause received");
                     Status := NAS_SHUTDOWN;
                     Status_Monitor.Set_Status (Status);
                  end Pause;
               end select;

            when NAS_SHUTDOWN =>
               Log_Debug
                 ("NAS_SHUTDOWN - starting "
                  & App_Global.NAS_Shutdown'Image
                  & "s block");
               select
                  delay To_Dur (Block_Time);
                  Log_Debug ("Block period completed");

                  Wait_Loop :
                  loop
                     Log_Debug ("Waiting for Continue or Shutdown");
                     select
                        accept Continue do
                           Log_Debug
                             ("Continue received, switching to NAS_ONLINE");
                           Status := NAS_ONLINE;
                           Status_Monitor.Set_Status (Status);
                           Next_WoL_Time :=
                             Clock
                             + WoL_Interval;  --  WICHTIG: In der Zukunft!
                           Log_Debug
                             ("Next_WoL_Time set to "
                              & To_Duration (Next_WoL_Time - Clock)'Image
                              & "s from now");
                        end Continue;
                        exit Wait_Loop;
                     or
                        accept Shutdown do
                           Log_Debug ("Shutdown received in wait loop");
                           Status := NAS_OFFLINE;
                           Status_Monitor.Set_Status (Status);
                           Is_Running := False;
                        end Shutdown;
                        exit Wait_Loop;
                     or
                        delay 1.0;
                     end select;
                  end loop Wait_Loop;

               or
                  accept Shutdown do
                     Log_Debug ("Emergency shutdown during block");
                     Status := NAS_OFFLINE;
                     Status_Monitor.Set_Status (Status);
                     Is_Running := False;
                  end Shutdown;
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
