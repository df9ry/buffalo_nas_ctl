with Ada.Exceptions;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;

with GNAT.Sockets; use GNAT.Sockets;

with App_Global; use App_Global;
with Log;

package body Wake_On_Lan is

   procedure Send is
      Packet  : Stream_Element_Array (1 .. 102);
      Socket  : Socket_Type;
      Address : Sock_Addr_Type;
      Last    : Stream_Element_Offset;

   begin
      --  Magic Packet aufbauen: FF:FF:FF:FF:FF:FF + 16×MAC
      for I in 1 .. 6 loop
         Packet (Stream_Element_Offset (I)) := Stream_Element'(16#FF#);
      end loop;

      for I in 1 .. 16 loop
         Packet (Stream_Element_Offset (6 + (I - 1) * 6 + 1) ..
                   Stream_Element_Offset (6 + I * 6)) :=
           Stream_Element_Array (NAS_Mac);
      end loop;

      --  UDP Socket vorbereiten
      Create_Socket (Socket, Family_Inet, Socket_Datagram);
      --  Create_Socket (Socket);
      Set_Socket_Option
        (Socket, Socket_Level, (Name => Broadcast, Enabled => True));

      Address.Addr := Inet_Addr (To_String (WoL_Target));
      Address.Port := Port_Type (App_Global.WoL_Port);

      --  Send three times
      for I in 1 .. 3 loop
         Send_Socket (Socket, Packet, Last, Address);
      end loop;

      Close_Socket (Socket);

      Log.Info ("Wake-on-LAN packet sent to [" & To_String (WoL_Mac) &
                  "] -> Broadcast " & To_String (WoL_Target) & ":" &
                  Trim (WoL_Port'Image, Both));
   exception
      when E : others =>
         Log.Error ("Error sending the WoL packet: " &
                    Ada.Exceptions.Exception_Information (E));
   end Send;

end Wake_On_Lan;
