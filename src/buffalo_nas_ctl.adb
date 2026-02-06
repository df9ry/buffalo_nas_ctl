
with App_Global;              use App_Global;
with Config_File;

with GNAT.Command_Line;       use GNAT.Command_Line;

with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Mac_Address_Parser;
with Web_Server;
with WoL_Task;
with Log;

procedure Buffalo_Nas_Ctl is

   Config_File_Name : Unbounded_String := To_Unbounded_String ("");

   procedure Parse_Command_Line is

      Config : Command_Line_Configuration;

      --  Helper procedure to handle string switches
      procedure Parse_String_Switch
        (Switch_Short  : String;
         Switch_Long   : String;
         Value         : in out Unbounded_String)
      is
      begin
         for I in 1 .. Ada.Command_Line.Argument_Count loop
            declare
               Arg : constant String := Ada.Command_Line.Argument (I);
            begin
               if Arg'Length > Switch_Short'Length and then
                 Arg (1 .. Switch_Short'Length) = Switch_Short
               then
                  Value := To_Unbounded_String
                    (Arg (Switch_Short'Length + 1 .. Arg'Last));
               elsif Arg'Length > Switch_Long'Length and then
                 Arg (1 .. Switch_Long'Length) = Switch_Long
               then
                  Value := To_Unbounded_String
                    (Arg (Switch_Long'Length + 1 .. Arg'Last));
               end if;
            end;
         end loop;
      end Parse_String_Switch;

   begin
      Set_Usage (Config => Config,
                 Usage  => "[options]",
                 Help   => "Buffalo NAS control service");
      Define_Switch (Config      => Config,
                     Switch      => "-c=",
                     Long_Switch => "--conf=",
                     Argument    => "FILE",
                     Help        => "Configuration file");
      Define_Switch (Config      => Config,
                     Switch      => "-v=",
                     Long_Switch => "--verbose=",
                     Argument    => "DEBUG|INFO|WARN|ERROR|FATAL",
                     Help        => "Verbosity");
      Define_Switch (Config      => Config,
                     Switch      => "-a=",
                     Long_Switch => "--addr=",
                     Argument    => "IP-ADDR",
                     Help        => "NAS IP broadcast address");
      Define_Switch (Config      => Config,
                     Output      => WoL_Port'Access,
                     Initial     => -1,
                     Switch      => "-p=",
                     Long_Switch => "--port=",
                     Argument    => "NUM",
                     Help        => "WoL port number (default 9)");
      Define_Switch (Config      => Config,
                     Switch      => "-m=",
                     Long_Switch => "--mac=",
                     Argument    => "MAC",
                     Help        => "NAS MAC address");
      Define_Switch (Config      => Config,
                     Output      => WoL_Interval'Access,
                     Initial     => -1,
                     Switch      => "-i=",
                     Long_Switch => "--interval=",
                     Argument    => "NUM",
                     Help        => "WoL interval (default 30)");
      Define_Switch (Config      => Config,
                     Output      => NAS_Shutdown'Access,
                     Initial     => -1,
                     Switch      => "-s=",
                     Long_Switch => "--shutdown=",
                     Argument    => "NUM",
                     Help        => "NAS shutdown time (default 300)");
      Define_Switch (Config      => Config,
                     Switch      => "-l=",
                     Long_Switch => "--listen=",
                     Argument    => "IP-ADDR",
                     Help        => "Service listen interface");
      Define_Switch (Config      => Config,
                     Output      => Svc_Port'Access,
                     Initial     => -1,
                     Switch      => "-w=",
                     Long_Switch => "--web=",
                     Argument    => "NUM",
                     Help        => "Service port number (default 8080)");
      Getopt (Config => Config);
      Parse_String_Switch ("-c=", "--conf=",    Config_File_Name);
      Parse_String_Switch ("-v=", "--verbose=", App_Log_Level);
      Parse_String_Switch ("-a=", "--addr=",    WoL_Target);
      Parse_String_Switch ("-m=", "--mac=",     WoL_Mac);
      Parse_String_Switch ("-l=", "--listen=",  Svc_Interface);
   end Parse_Command_Line;

begin
   Log.Set_Level (Log.Debug);
   Log.Debug ("Debug is on");
   Parse_Command_Line;
   if Length (Config_File_Name) > 0 then
      declare
         Config : Config_File.Configuration;
      begin
         Log.Info ("Using config file " & To_String (Config_File_Name));
         Config_File.Load (Config, To_String (Config_File_Name));
         if Length (App_Log_Level) = 0 then
            App_Log_Level :=
              To_Unbounded_String (
              Config_File.Get (Config, "App", "Verbosity",
                               App_Log_Level_Default));
         end if;
         if Length (WoL_Target) = 0 then
            WoL_Target :=
              To_Unbounded_String (
              Config_File.Get (Config, "WoL", "Target",
                               WoL_Target_Default));
         end if;
         if WoL_Port = -1 then
            WoL_Port :=
              Config_File.Get_Int (Config, "WoL", "Port",
                                   WoL_Port_Default);
         end if;
         if Length (WoL_Mac) = 0 then
            WoL_Mac :=
              To_Unbounded_String (
              Config_File.Get (Config, "WoL", "Mac",
                               WoL_Mac_Default));
         end if;
         if WoL_Interval = -1 then
            WoL_Interval :=
              Config_File.Get_Int (Config, "WoL", "Interval",
                                   WoL_Interval_Default);
         end if;
         if NAS_Shutdown = -1 then
            NAS_Shutdown :=
              Config_File.Get_Int (Config, "NAS", "Shutdown",
                                   NAS_Shutdown_Default);
         end if;
         if Length (Svc_Interface) = 0 then
            App_Global.Svc_Interface :=
              To_Unbounded_String (
              Config_File.Get (Config, "Service", "Interface",
                               Svc_Interface_Default));
         end if;
         if Svc_Port = -1 then
            Svc_Port :=
              Config_File.Get_Int (Config, "Service", "Port",
                                   Svc_Port_Default);
         end if;
      end;
   else
      Log.Debug ("No config file");
      if Length (App_Log_Level) = 0 then
         App_Log_Level := To_Unbounded_String (App_Log_Level_Default);
      end if;
      if Length (WoL_Target) = 0 then
         WoL_Target := To_Unbounded_String (WoL_Target_Default);
      end if;
      if WoL_Port = -1 then
         WoL_Port := WoL_Port_Default;
      end if;
      if Length (WoL_Mac) = 0 then
         WoL_Mac := To_Unbounded_String (WoL_Mac_Default);
      end if;
      if WoL_Interval = -1 then
         WoL_Interval := WoL_Interval_Default;
      end if;
      if NAS_Shutdown = -1 then
         NAS_Shutdown := NAS_Shutdown_Default;
      end if;
      if Length (Svc_Interface) = 0 then
         Svc_Interface := To_Unbounded_String (Svc_Interface_Default);
      end if;
      if Svc_Port = -1 then
         Svc_Port := Svc_Port_Default;
      end if;
   end if;
   Log.Set_Level (Log.Debug);
   Log.Info ("This is "  & App_Name &
             " version " & App_Version &
             " - Copyright (C) Reiner Hagn, 2026");
   --  Parse MAC to internal format:
   NAS_Mac := Mac_Address_Parser.To_Mac_Address (To_String (WoL_Mac));

   WoL_Task.Start;
   Web_Server.Run;
   WoL_Task.Shutdown;

   Set_Exit_Status (Success);

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
      Set_Exit_Status (Failure);
end Buffalo_Nas_Ctl;
