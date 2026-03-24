with App_Global;
with Wake_On_Lan;
with Log;
with ISO_Time; use ISO_Time;

package body WoL_Task is

   protected body Task_Monitor is

      function Get_Next_Execution_Time return Time is
      begin
         return Next_Execution_Time;
      end Get_Next_Execution_Time;

      procedure Set_Next_Execution_Time (New_Time : Time) is
      begin
         Log.Debug ("Set Next_Execution_Time to " & Image (New_Time));
         Next_Execution_Time := New_Time;
      end Set_Next_Execution_Time;

      function Get_Last_Shutdown_Time return Time is
      begin
         return Last_Shutdown_Time;
      end Get_Last_Shutdown_Time;

      procedure Set_Last_Shutdown_Time (New_Time : Time) is
      begin
         Log.Debug ("Set Last_Shutdown_Time to " & Image (New_Time));
         Last_Shutdown_Time := New_Time;
      end Set_Last_Shutdown_Time;

      function Get_Last_Poll_Time return Time is
      begin
         return Last_Poll_Time;
      end Get_Last_Poll_Time;

      procedure Set_Last_Poll_Time (New_Time : Time) is
      begin
         Log.Debug ("Set Last_Poll_Time to " & Image (New_Time));
         Last_Poll_Time := New_Time;
      end Set_Last_Poll_Time;

      function Is_NAS_Online return Boolean is
      begin
         return NAS_Online;
      end Is_NAS_Online;

      procedure Set_NAS_Online (Online : Boolean) is
      begin
         Log.Debug ("Set NAS_Online to " & Online'Image);
         NAS_Online := Online;
      end Set_NAS_Online;

   end Task_Monitor;

   task body Worker is
      Quit      : Boolean := False;
      Interval  : Duration;
      Grace     : Duration;
   begin
      Log.Debug ("WoL_Task enter");
      select
         accept Start do
            Log.Debug ("WoL_Task received initial Start");
            Interval := Duration (App_Global.WoL_Interval);
            Grace    := Duration (App_Global.Svc_Grace);
         end Start;
      or
         accept Shutdown do
            Log.Debug ("WoL_Task received initial Shutdown");
            Quit := True;
         end Shutdown;
      end select;
      Log.Info ("WoL_Task start");
      while App_Global.Run_Guard.Is_Running and then not Quit loop
         Log.Debug ("WoL_Task loop with NAS_Online " &
                      Task_Monitor.Is_NAS_Online'Image &
                      " and next WoL at " &
                      Image (Task_Monitor.Get_Next_Execution_Time));
         select
            accept Shutdown do
               Log.Debug ("WoL_Task received Shutdown");
               Quit := True;
            end Shutdown;
         or
            accept Start do
               Log.Debug ("WoL_Task received Start");
               if not Task_Monitor.Is_NAS_Online then
                  Task_Monitor.Set_Next_Execution_Time (Clock + Interval);
                  Wake_On_Lan.Send;
                  Task_Monitor.Set_NAS_Online (True);
               end if;
            end Start;
         or
            delay until Task_Monitor.Get_Next_Execution_Time;
            Log.Debug ("WoL_Task timer event");
            if Task_Monitor.Get_Last_Poll_Time + Grace > Clock then
               Task_Monitor.Set_Next_Execution_Time (Clock + Interval);
               Wake_On_Lan.Send;
            else
               Task_Monitor.Set_Next_Execution_Time (Eternal);
               Task_Monitor.Set_Last_Shutdown_Time (Clock);
               Task_Monitor.Set_NAS_Online (False);
            end if;
         end select;
      end loop;
      Log.Info ("WoL_Task loop exited");
   end Worker;

   procedure Start is
   begin
      Worker.Start;
   end Start;

   procedure Shutdown is
   begin
      Worker.Shutdown;
   end Shutdown;

end WoL_Task;
