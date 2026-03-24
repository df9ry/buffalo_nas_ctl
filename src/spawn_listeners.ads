
with Spawn;
with Spawn.Processes;
with Spawn.String_Vectors;
with Ada.Strings.Unbounded;

package Spawn_Listeners is

   type Listener is limited new Spawn.Processes.Process_Listener with record
      P       : aliased Spawn.Processes.Process;
      Stdin   : Ada.Strings.Unbounded.Unbounded_String;
      Stdout  : Ada.Strings.Unbounded.Unbounded_String;
      Stderr  : Ada.Strings.Unbounded.Unbounded_String;
      Result  : Spawn.Processes.Process_Exit_Code := 0;
      Started : Boolean := False;
      Stopped : Boolean := False;
   end record;

   --  Muss aufgerufen werden bevor irgendeine andere Funktion verwendet wird!
   procedure Initialize (Self : in out Listener);

   procedure Set_Program
     (Self : in out Listener; Program : String);

   procedure Set_Arguments
     (Self : in out Listener;
      Arguments : Spawn.String_Vectors.UTF_8_String_Vector);

   procedure Set_Working_Directory
     (Self : in out Listener; Directory : String);

   procedure Start
     (Self : in out Listener);

   procedure Shutdown
     (Self : in out Listener);

   --  Hier kommen die Callbacks, die vom Monitor aufgerufen werden
   overriding
   procedure Standard_Output_Available (Self : in out Listener);

   overriding
   procedure Standard_Error_Available (Self : in out Listener);

   overriding
   procedure Started (Self : in out Listener);

   overriding
   procedure Finished
     (Self        : in out Listener;
      Exit_Status : Spawn.Processes.Process_Exit_Status;
      Exit_Code   : Spawn.Processes.Process_Exit_Code);

   overriding
   procedure Error_Occurred (Self : in out Listener; Process_Error : Integer);

private
   type Process_Access is access all Spawn.Processes.Process;
   P : Process_Access;
end Spawn_Listeners;
