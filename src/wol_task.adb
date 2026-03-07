with App_Global;
with Wake_On_Lan;
with Log;

package body WoL_Task is

   protected body Task_Monitor is

      function Get_Next_Execution_Time return Time is
      begin
         return Next_Execution_Time;
      end Get_Next_Execution_Time;

      procedure Set_Next_Execution_Time (New_Time : Time) is
      begin
         Log.Debug ("Set next execution to " & Next_Execution_Time'Image);
         Next_Execution_Time := New_Time;
      end Set_Next_Execution_Time;

   end Task_Monitor;

   task body Worker is
      Quit      : Boolean := False;
      Interval  : Duration;
      Now, Last : Time;
   begin
      select
         accept Start do
            Interval := Duration (App_Global.WoL_Interval);
         end Start;
         accept Shutdown do
            return;
         end Shutdown;
      end select;
      while not Quit loop
         Log.Debug ("WoL_Task loop begin");
         select
            accept Shutdown do
               Log.Debug ("WoL_Task received Shutdown");
               Quit := True;
            end Shutdown;
         or
            accept Start do
               Log.Debug ("WoL_Task received Start");
               Now := Clock;
               if (Now > Task_Monitor.Get_Next_Execution_Time) then
                  Task_Monitor.Set_Next_Execution_Time (Now + Interval);
                  Wake_On_Lan.Send;
               end if;
               Log.Debug
                 ("Next_Execution_Time is "
                  & Task_Monitor.Get_Next_Execution_Time'Image);
            end Start;
         or
            accept Stop do
               Log.Debug ("WoL_Task received Stop");
               Task_Monitor.Set_Next_Execution_Time (Eternal);
               Log.Debug
                 ("Next_Execution_Time is "
                  & Task_Monitor.Get_Next_Execution_Time'Image);
            end Stop;
         or
            delay until Task_Monitor.Get_Next_Execution_Time;
            Now := Clock;
            Last := Task_Monitor.Get_Next_Execution_Time;
            if (Now >= Last) then
               Task_Monitor.Set_Next_Execution_Time (Last + Interval);
               Wake_On_Lan.Send;
               Log.Debug
                 ("Next_Execution_Time is "
                  & Task_Monitor.Get_Next_Execution_Time'Image);
            else
               Log.Debug ("No Execution scheduled");
            end if;
         end select;
      end loop;
      Log.Info ("WoL_Task loop exited");
   end Worker;

   procedure Start is
   begin
      Worker.Start;
   end Start;

   procedure Stop is
   begin
      Worker.Stop;
   end Stop;

   procedure Shutdown is
   begin
      Worker.Shutdown;
   end Shutdown;

end WoL_Task;
