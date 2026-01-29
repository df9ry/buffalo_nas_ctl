with Ada.Real_Time;
with Log;

package body WoL_Task is
   use Ada.Real_Time;

   procedure Execute_WoL is
   begin
      Log.Info ("WoL_Task execute");
   end Execute_WoL;

   task body Worker is
      Interval    : constant Time_Span := Seconds(30);
      Next_Time   : Time;
      Is_Active   : Boolean := False;
      Is_Running  : Boolean := True;
   begin
      Log.Debug ("WoL_Task Enter");
      while Is_Running loop
         select
            accept Start do
               Log.Info ("WoL_Task Start");
               Is_Active := True;
               Next_Time := Clock + Interval;
            end Start;
         or when Is_Active =>
            accept Pause do
               Log.Info ("WoL_Task Pause");
               Is_Active := False;
            end Pause;
         or when not Is_Active =>
            accept Continue do
               Log.Info ("WoL_Task Continue");
               Is_Active := True;
               Next_Time := Clock + Interval;
            end Continue;
         or
            accept Shutdown do
               Log.Info ("WoL_Task Shutdown");
               Is_Active := False;
               Is_Running := False;
            end Shutdown;
         or when Is_Active =>
            delay until Next_Time;
            Execute_WoL;
            Next_Time := Next_Time + Interval;
         end select;
      end loop;
      Log.Info ("WoL_Task Exit");
   end Worker;

end WoL_Task;
