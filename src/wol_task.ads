package WoL_Task is

   type NAS_Status is (NOT_STARTED, NAS_OFFLINE, NAS_SHUTDOWN, NAS_ONLINE);

   --  Protected Object für Status-Abfrage von außen
   protected Status_Monitor is
      function Get_Status return NAS_Status;
      procedure Set_Status (New_Status : NAS_Status);
   private
      Current_Status : NAS_Status := NAS_OFFLINE;
   end Status_Monitor;

   task Worker is
      entry Start;
      entry Pause;
      entry Continue;
      entry Shutdown;
   end Worker;

   --  Task-Steuerung
   procedure Start;
   procedure Pause;
   procedure Continue;
   procedure Shutdown;

   function Status return NAS_Status;

end WoL_Task;
