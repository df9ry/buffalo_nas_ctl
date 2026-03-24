with Ada.Calendar; use Ada.Calendar;

package WoL_Task is

   Eternal       : constant Time := Time_Of (2200,  1,  1);
   Long_Long_Ago : constant Time := Time_Of (1999, 12, 31);

   --  Protected Object für Status-Abfrage von außen
   protected Task_Monitor is
      function  Get_Next_Execution_Time return Time;
      procedure Set_Next_Execution_Time (New_Time : Time);
      function  Get_Last_Shutdown_Time return Time;
      procedure Set_Last_Shutdown_Time (New_Time : Time);
      function  Get_Last_Poll_Time return Time;
      procedure Set_Last_Poll_Time (New_Time : Time);
      function  Is_NAS_Online return Boolean;
      procedure Set_NAS_Online (Online : Boolean);
   private
      Next_Execution_Time : Time := Eternal;
      Last_Shutdown_Time  : Time := Long_Long_Ago;
      Last_Poll_Time      : Time := Long_Long_Ago;
      Last_Error_Time     : Time := Long_Long_Ago;
      NAS_Online          : Boolean := False;
   end Task_Monitor;

   task Worker is
      entry Start;
      entry Shutdown;
   end Worker;

   --  Task-Steuerung
   procedure Start;
   procedure Shutdown;

end WoL_Task;
