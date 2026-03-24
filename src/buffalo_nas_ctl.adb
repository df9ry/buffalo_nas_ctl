with App_Global; use App_Global;
with Config_File;

with GNAT.Command_Line; use GNAT.Command_Line;

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;       use Ada.Directories;

with Mac_Address_Parser;
with Web_Server;
with WoL_Task;
with Script;
with AWS.Server;
with Log;

procedure Buffalo_Nas_Ctl is

   Config_File_Name : Unbounded_String := To_Unbounded_String ("");

   procedure Parse_Command_Line is

      Config : Command_Line_Configuration;

      --  Helper procedure to handle string switches
      procedure Parse_String_Switch
        (Switch_Short : String;
         Switch_Long  : String;
         Value        : in out Unbounded_String) is
      begin
         for I in 1 .. Ada.Command_Line.Argument_Count loop
            declare
               Arg : constant String := Ada.Command_Line.Argument (I);
            begin
               if Arg'Length > Switch_Short'Length
                 and then Arg (1 .. Switch_Short'Length) = Switch_Short
               then
                  Value :=
                    To_Unbounded_String
                      (Arg (Switch_Short'Length + 1 .. Arg'Last));
               elsif Arg'Length > Switch_Long'Length
                 and then Arg (1 .. Switch_Long'Length) = Switch_Long
               then
                  Value :=
                    To_Unbounded_String
                      (Arg (Switch_Long'Length + 1 .. Arg'Last));
               end if;
            end;
         end loop;
      end Parse_String_Switch;

   begin
      Set_Usage
        (Config => Config,
         Usage  => "[options]",
         Help   => "Buffalo NAS control service");
      Define_Switch
        (Config      => Config,
         Switch      => "-c=",
         Long_Switch => "--conf=",
         Argument    => "FILE",
         Help        => "Configuration file");
      Define_Switch
        (Config      => Config,
         Switch      => "-v=",
         Long_Switch => "--verbose=",
         Argument    => "OFF|DEBUG|INFO|WARN|ERROR|FATAL",
         Help        => "Verbosity");
      Getopt (Config => Config);
      Parse_String_Switch ("-c=", "--conf=", Config_File_Name);
      Parse_String_Switch ("-v=", "--verbose=", App_Log_Level);
   end Parse_Command_Line;

   procedure When_Not_Set_Use_Default is
      Config : Config_File.Configuration;
   begin
      Log.Info ("Using config file " & To_String (Config_File_Name));
      Config_File.Load (Config, To_String (Config_File_Name));
      --  LOG LEVEL  --
      if Length (App_Log_Level) = 0 then
         App_Log_Level :=
           To_Unbounded_String
             (Config_File.Get
                (Config, "App", "Verbosity", App_Log_Level_Default));
      end if;
      --  WOL  TARGET  --
      WoL_Target :=
        To_Unbounded_String
          (Config_File.Get (Config, "WoL", "Target", WoL_Target_Default));
      --  WOL PORT --
      WoL_Port :=
        Config_File.Get_Int (Config, "WoL", "Port", WoL_Port_Default);
      --  WOL  MAC  --
      WoL_Mac :=
        To_Unbounded_String
          (Config_File.Get (Config, "WoL", "Mac", WoL_Mac_Default));
      --  WOL INTERVAL  --
      WoL_Interval :=
           Config_File.Get_Int
             (Config, "WoL", "Interval", WoL_Interval_Default);
      --  NAS SHUTDOWN  --
      NAS_Shutdown :=
        Config_File.Get_Int
          (Config, "NAS", "Shutdown", NAS_Shutdown_Default);
      --  NAS SCRIPT  --
      NAS_Script :=
        To_Unbounded_String
          (Config_File.Get
             (Config, "NAS", "Script", NAS_Script_Default));
      --  SVC INTERFACE  --
      Svc_Interface :=
        To_Unbounded_String
          (Config_File.Get
             (Config, "Service", "Interface", Svc_Interface_Default));
      --  SVC PORT  --
      Svc_Port :=
        Config_File.Get_Int (Config, "Service", "Port", Svc_Port_Default);
      --  SVC GRACE  --
      Svc_Grace :=
        Config_File.Get_Int (Config, "Service", "Grace", Svc_Grace_Default);
   end When_Not_Set_Use_Default;

begin
   Log.Set_Level (App_Log_Level_Default);
   --  Log.Set_Level ("DEBUG");
   Parse_Command_Line;
   if Length (Config_File_Name) = 0 then
      Config_File_Name :=
        To_Unbounded_String (Current_Directory & Config_File_Default);
   end if;
   When_Not_Set_Use_Default;

   Log.Set_Level (App_Log_Level);
   Log.Info
     ("This is "
      & App_Name
      & " version "
      & App_Version
      & " - Copyright (C) Reiner Hagn, 2026");
   --  Parse MAC to internal format:
   NAS_Mac := Mac_Address_Parser.To_Mac_Address (To_String (WoL_Mac));

   WoL_Task.Start;
   Web_Server.Run;
   App_Global.Run_Guard.Stop;
   Script.Shutdown;
   WoL_Task.Shutdown;

   Set_Exit_Status (Success);
   Log.Info ("Clean program termination");
   return;

exception
   when Exit_From_Command_Line =>
      Set_Exit_Status (Failure);  -- Help was displayed, exit with failure
   when Invalid_Switch =>
      Log.Error ("Error: Invalid option");
      Try_Help;
   when Invalid_Parameter =>
      Log.Error ("Error: Invalid parameter value");
      Try_Help;
   when E : others =>
      Log.Error ("Error: " & Exception_Message (E));
      AWS.Server.Shutdown (Web_Server.Server);
      AWS.Server.Wait;
      Run_Guard.Kill (Integer (Failure));
end Buffalo_Nas_Ctl;
