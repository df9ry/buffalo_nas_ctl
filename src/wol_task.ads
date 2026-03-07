with Ada.Calendar; use Ada.Calendar;

package WoL_Task is

   Eternal       : constant Time := Time_Of (2100,  1,  1);
   Long_Long_Ago : constant Time := Time_Of (1999, 12, 31);

   --  Protected Object für Status-Abfrage von außen
   protected Task_Monitor is
      function Get_Next_Execution_Time return Time;
      procedure Set_Next_Execution_Time (New_Time : Time);
   private
      Next_Execution_Time : Time := Eternal;
   end Task_Monitor;

   task Worker is
      entry Start;
      entry Stop;
      entry Shutdown;
   end Worker;

   --  Task-Steuerung
   procedure Start;
   procedure Stop;
   procedure Shutdown;

end WoL_Task;
