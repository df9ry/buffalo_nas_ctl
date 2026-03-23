with App_Global;
with Log;

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Directories;              use Ada.Directories;
with Ada.Exceptions;               use Ada.Exceptions;

with Spawn;                        use Spawn;
with Spawn.String_Vectors;         use Spawn.String_Vectors;
with Spawn.Processes.Monitor_Loop;
with Spawn_Listeners;

package body Script is

   procedure Execute_Script (Command_Line : String; Result : out Integer) is

      L      : aliased Spawn_Listeners.Listener;
      Args   : UTF_8_String_Vector;

   begin
      Log.Info ("Execute """ & Command_Line & """");
      L.Initialize;
      L.Set_Program ("/usr/bin/bash");
      Args.Append ("-c");
      Args.Append (Command_Line);
      L.Set_Arguments (Args);
      L.Set_Working_Directory (Current_Directory);
      L.Start;
      --  Warte auf Prozessende mit regelmäßigen Monitor‑Aufrufen
      while not L.Stopped loop
          Spawn.Processes.Monitor_Loop (0.1);   -- 100 ms warten
      end loop;
      Result := Integer (L.Result);
      Log.Info ("Finished with exit code" & Result'Image);
   exception
      when E : others =>
         Log.Warning ("Spawn Exception:" & Exception_Message (E));
         Result := -1;
   end Execute_Script;

   protected body Script_Monitor is

      function Is_Running return Boolean is
      begin
         return Running;
      end Is_Running;

      procedure Set_Running (Flag : Boolean := True) is
      begin
         Running := Flag;
      end Set_Running;

      function Get_Result return Integer is
      begin
         return Result;
      end Get_Result;

      procedure Set_Result (Value : Integer) is
      begin
         Result := Value;
      end Set_Result;

   end Script_Monitor;

   task body Worker is
      Quit   : Boolean := False;
      Result : Integer := -1;
   begin
      Log.Info ("Script_Task enter");
      while not Quit loop
         if Script_Monitor.Is_Running then
            Log.Debug ("Script_Task start execute");
            Execute_Script (To_String (App_Global.NAS_Script), Result);
            Log.Debug
              ("Script_Task finished execute with result" & Result'Image);
            Script_Monitor.Set_Result (Result);
            Script_Monitor.Set_Running (False);
         else
            Log.Debug ("Script_Task enter select");
            select
               accept Shutdown do
                  Log.Debug ("Script_Task received Shutdown");
                  Quit := True;
               end Shutdown;
            or
               accept Start do
                  Log.Debug ("Script_Task received Start");
                  Script_Monitor.Set_Running;
               end Start;
            end select;
         end if;
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

end Script;
