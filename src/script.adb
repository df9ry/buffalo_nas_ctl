with App_Global;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Expect; use GNAT.Expect;
with GNAT.Regpat; use GNAT.Regpat;
with GNAT.Strings;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with Log;

package body Script is

   subtype My_Argument_List is GNAT.OS_Lib.Argument_List;
   type My_Argument_List_Access is access My_Argument_List;

   --  Pattern      : constant Pattern_Matcher := Compile (".*" & ASCII.LF);

   procedure Free_Argument_List is new
     Ada.Unchecked_Deallocation (My_Argument_List, My_Argument_List_Access);

   procedure Execute_Script (Command_Line : String; Result : out Integer) is

      Command_Error : exception;

      --  Parser: zerlegt eine Shell‑kompatible Zeile in eine Argumentliste
      function Parse_Command_Line (Cmd : String) return My_Argument_List_Access
      is
         type State_Type is (Normal, Single_Quote, Double_Quote);
         State       : State_Type := Normal;
         Escape      : Boolean := False;
         Current_Arg : Unbounded_String := Null_Unbounded_String;
         Arg_Array   : My_Argument_List_Access := null;
         Arg_Count   : Natural := 0;

         procedure Add_Current_Arg is
         begin
            if Length (Current_Arg) > 0 then
               Arg_Count := Arg_Count + 1;
               declare
                  New_Array : constant My_Argument_List_Access :=
                    new My_Argument_List (1 .. Arg_Count);
               begin
                  if Arg_Array /= null then
                     New_Array (1 .. Arg_Count - 1) := Arg_Array.all;
                     Free_Argument_List (Arg_Array);
                  end if;
                  New_Array (Arg_Count) :=
                    new String'(To_String (Current_Arg));
                  Arg_Array := New_Array;
               end;
               Set_Unbounded_String (Current_Arg, "");
            end if;
         end Add_Current_Arg;

         procedure Append_Char (C : Character) is
         begin
            Append (Current_Arg, C);
         end Append_Char;

      begin
         for I in Cmd'Range loop
            declare
               C : constant Character := Cmd (I);
            begin
               if Escape then
                  Append_Char (C);
                  Escape := False;
               else
                  case State is
                     when Normal =>
                        if C = ' ' or else C = ASCII.HT then
                           Add_Current_Arg;
                        elsif C = ''' then
                           State := Single_Quote;
                        elsif C = '"' then
                           State := Double_Quote;
                        elsif C = '\' then
                           Escape := True;
                        else
                           Append_Char (C);
                        end if;

                     when Single_Quote =>
                        if C = ''' then
                           State := Normal;
                        else
                           Append_Char (C);
                        end if;

                     when Double_Quote =>
                        if C = '"' then
                           State := Normal;
                        elsif C = '\' then
                           Escape := True;
                        else
                           Append_Char (C);
                        end if;
                  end case;
               end if;
            end;
         end loop;

         Add_Current_Arg;   --  letztes Argument

         if State /= Normal or else Escape then
            if Arg_Array /= null then
               for A of Arg_Array.all loop
                  Free (A);
               end loop;
               Free_Argument_List (Arg_Array);
            end if;
            raise Command_Error with "Unbalanced quotes or trailing backslash";
         end if;

         return Arg_Array;
      end Parse_Command_Line;

      --  ----------------------------------------------------------------------
      --  Variablen der Hauptfunktion
      --  ----------------------------------------------------------------------
      Script_Path  : GNAT.Strings.String_Access := null;
      Args         : My_Argument_List_Access := null;
      Process      : Process_Descriptor;
      Exit_Code    : Integer;
      Match_Result : Expect_Match;
      Matched      : Match_Array (0 .. 1);

   begin
      Result := -1;
      Log.Info ("Execute """ & Command_Line & """");
      --  1. Kommandozeile zerlegen
      Args := Parse_Command_Line (Command_Line);
      if Args = null or else Args'Length = 0 then
         Log.Error ("Empty command line");
         return;
      end if;
      Script_Path := Args (Args'First);

      --  2. Prozess starten (stdout+stderr zusammen)
      Log.Debug ("Start process """ & Script_Path.all & """");
      Non_Blocking_Spawn
        (Process,
         Script_Path.all,
         Args.all (Args'First + 1 .. Args'Last),
         Err_To_Out => True);
      if Process.Get_Pid = -1 then
         Log.Error ("Spawn failed");
         Result := 99;
      end if;

      --  3. Ausgabe zeilenweise lesen und loggen
      loop
         begin
            --  Expect (Process, Match_Result, ".*" & ASCII.LF, Matched);
            Expect
              (Process,
               Match_Result,
               "[^" & ASCII.LF & "]*" & ASCII.LF,
               Matched);
            Log.Debug ("Expect returned" & Match_Result'Image);
            if Match_Result = 1 then
               declare
                  Line : constant String := Expect_Out_Match (Process);
               begin
                  if Line'Length > 0 and then Line (Line'Last) = ASCII.LF then
                     Log.Info ("|" & Line (Line'First .. Line'Last - 1) & "|");
                  else
                     Log.Info ("|" & Line & "|");
                  end if;
               end;
            end if;
         exception
            when Process_Died =>
               Log.Debug ("Process vanished");
               exit;
            when E : others =>
               Log.Warning ("Exception: " & Exception_Message (E));
               --  raise;
               exit;
         end;
      end loop;

      --  4. Exit‑Code holen und Prozess‑Deskriptor schließen
      Log.Debug ("Close process");
      GNAT.Expect.Close (Process, Exit_Code);
      Log.Info ("Process exited with code" & Exit_Code'Image);
      Log.Debug ("Close filr descriptor");
      Close (Get_Output_Fd (Process));

      --  5. Aufräumen (alle allokierten Strings freigeben)
      --     Wichtig: Script_Path ist bereits Teil von Args, darf also nicht
      --     separat freigegeben werden, da es sonst doppelt freigegeben
      --     würde.
      --     Wir durchlaufen Args und geben jedes Element frei, dann das
      --     Array.
      Log.Debug ("Cleaning up");
      if Args /= null then
         for A of Args.all loop
            Free (A);
         end loop;
         Free_Argument_List (Args);
      end if;
      Log.Debug ("Set exit code" & Exit_Code'Image);
      Result := Exit_Code;

   exception
      when E : others =>
         Log.Error ("Error: " & Exception_Message (E));
         --  Bei jedem Fehler alle bereits allokierten Ressourcen freigeben
         if Args /= null then
            for A of Args.all loop
               Free (A);
            end loop;
            Free_Argument_List (Args);
         end if;
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
