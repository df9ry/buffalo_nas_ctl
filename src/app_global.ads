
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces;

package App_Global is

   WoL_Target_Default    : constant String  := "127.0.0.255";
   WoL_Port_Default      : constant Integer := 9;
   WoL_Mac_Default       : constant String  := "ff:ff:ff:ff:ff:ff";
   WoL_Interval_Default  : constant Integer := 30;
   NAS_Shutdown_Default  : constant Integer := 300;
   Svc_Interface_Default : constant String  := "localhost";
   Svc_Port_Default      : constant Integer := 8080;

   WoL_Target    : Unbounded_String := To_Unbounded_String ("");
   WoL_Port      : aliased Integer  := -1;
   WoL_Mac       : Unbounded_String := To_Unbounded_String ("");
   WoL_Interval  : aliased Integer  := -1;
   NAS_Shutdown  : aliased Integer  := -1;
   Svc_Interface : Unbounded_String := To_Unbounded_String ("");
   Svc_Port      : aliased Integer  := -1;

   type Mac_Address is array (1 .. 6) of Interfaces.Unsigned_8;
   NAS_Mac : Mac_Address := (others => 0);

end App_Global;
