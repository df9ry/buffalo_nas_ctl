
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;

package App_Global is

   App_Name              : constant String  := "Buffalo_NAS_Ctl";
   App_Version           : constant String  := "0.1.0";

   Config_File_Default   : constant String  := "/etc/" & App_Name & ".conf";

   App_Log_Level_Default : constant String  := "INFO";
   WoL_Target_Default    : constant String  := "127.0.0.255";
   WoL_Port_Default      : constant Integer := 9;
   WoL_Mac_Default       : constant String  := "ff:ff:ff:ff:ff:ff";
   WoL_Interval_Default  : constant Integer := 30;
   NAS_Shutdown_Default  : constant Integer := 300;
   NAS_Script_Default    : constant String  := "/etc/nas_mount.sh";
   Svc_Interface_Default : constant String  := "0.0.0.0";
   Svc_Port_Default      : constant Integer := 8080;
   Svc_Grace_Default     : constant Integer := 60;

   App_Log_Level : Unbounded_String := Null_Unbounded_String;
   WoL_Target    : Unbounded_String := Null_Unbounded_String;
   WoL_Port      : aliased Integer  := -1;
   WoL_Mac       : Unbounded_String := Null_Unbounded_String;
   WoL_Interval  : aliased Integer  := -1;
   NAS_Shutdown  : aliased Integer  := -1;
   NAS_Script    : Unbounded_String := Null_Unbounded_String;
   Svc_Interface : Unbounded_String := Null_Unbounded_String;
   Svc_Port      : aliased Integer  := -1;
   Svc_Grace     : aliased Integer  := -1;

   subtype MAC_Byte is Stream_Element range 0 .. 255;
   type MAC_Address is array (1 .. 6) of MAC_Byte;
   NAS_Mac : Mac_Address := [others => 0];

end App_Global;
