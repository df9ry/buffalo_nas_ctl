package Script is

   --  Protected Object für Skript Ausführung
   protected Script_Monitor is
      function Is_Running return Boolean;
      procedure Set_Running (Flag : Boolean := True);
      function Get_Result return Integer;
      procedure Set_Result (Value : Integer);
   private
      Running : Boolean := False;
      Result  : Integer := 0;
   end Script_Monitor;

   task Worker is
      entry Start;
      entry Shutdown;
   end Worker;

   --  Task-Steuerung
   procedure Start;
   procedure Shutdown;

end Script;
