with Ada.Streams;
with Ada.Exceptions;
with Log;

package body Spawn_Listeners is

   Buffer_Size : constant Ada.Streams.Stream_Element_Offset := 256;

   procedure Initialize (Self : in out Listener) is
   begin
      Self.P.Set_Listener (Self'Unchecked_Access);
   end Initialize;

   procedure Set_Program (Self : in out Listener; Program : String) is
   begin
      Self.P.Set_Program (Program);
   end Set_Program;

   procedure Set_Arguments
     (Self : in out Listener;
      Arguments : Spawn.String_Vectors.UTF_8_String_Vector) is
   begin
      Self.P.Set_Arguments (Arguments);
   end Set_Arguments;

   procedure Set_Working_Directory
     (Self : in out Listener; Directory : String) is
   begin
      Self.P.Set_Working_Directory (Directory);
   end Set_Working_Directory;

   procedure Start
     (Self : in out Listener) is
   begin
      Self.P.Start;
   end Start;

   overriding
   procedure Standard_Output_Available (Self : in out Listener) is
      use type Ada.Streams.Stream_Element_Count;
      use Ada.Strings.Unbounded;
      use Ada.Exceptions;
      use Spawn.Processes;
   begin
      loop
         declare
            Data    : Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);
            Last    : Ada.Streams.Stream_Element_Count;
            Success : Boolean := True;

         begin
            Self.P.Read_Standard_Output (Data, Last, Success);

            exit when Last < Data'First;

            for Char of Data (1 .. Last) loop
               if Char not in 16#0D# | 16#0A# then
                  Ada.Strings.Unbounded.Append
                    (Self.Stdout, Character'Val (Char));
               elsif Length (Self.Stdout) > 0 then
                  Log.Info ("O:|" & To_String (Self.Stdout) & "|");
                  Self.Stdout := Null_Unbounded_String;
               end if;
            end loop;
         end;
      end loop;
   exception
      when E : others =>
         Log.Warning ("Spawn read stdout exception " & Exception_Message (E));
         Self.Stopped := True;
   end Standard_Output_Available;

   overriding
   procedure Standard_Error_Available (Self : in out Listener) is
      use type Ada.Streams.Stream_Element_Count;
      use Ada.Strings.Unbounded;
      use Ada.Exceptions;
   begin
      loop
         declare
            Data    : Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);
            Last    : Ada.Streams.Stream_Element_Count;
            Success : Boolean := True;

         begin
            Self.P.Read_Standard_Error (Data, Last, Success);

            exit when Last < Data'First;

            for Char of Data (1 .. Last) loop
               if Char not in 16#0D# | 16#0A# then
                  Ada.Strings.Unbounded.Append
                    (Self.Stderr, Character'Val (Char));
               elsif Length (Self.Stderr) > 0 then
                  Log.Info ("E:|" & To_String (Self.Stderr) & "|");
                  Self.Stderr := Null_Unbounded_String;
               end if;
            end loop;
         end;
      end loop;
   exception
      when E : others =>
         Log.Warning ("Spawn read stderr exception " & Exception_Message (E));
         Self.Stopped := True;
   end Standard_Error_Available;

   overriding
   procedure Started (Self : in out Listener) is
      use Ada.Exceptions;
   begin
      Log.Debug ("Spawn started");
      Self.Started := True;
      Self.P.Close_Standard_Input;
   exception
      when E : others =>
         Log.Warning ("Spawn started exception " & Exception_Message (E));
         Self.Stopped := True;
   end Started;

   overriding
   procedure Finished
     (Self        : in out Listener;
      Exit_Status : Spawn.Processes.Process_Exit_Status;
      Exit_Code   : Spawn.Processes.Process_Exit_Code)
   is
      use Ada.Strings.Unbounded;
      use Ada.Exceptions;
   begin
      Log.Debug ("Spawn Finished has been called with Exit_Status " &
                Exit_Status'Image);
      Self.Result := Exit_Code;
      Self.Stopped := True;
      --  Restliche Buffer ausgeben, wenn noch was da ist
      if Length (Self.Stdout) > 0 then
         Log.Info ("O:|" & To_String (Self.Stdout) & "|");
         Self.Stdout := Null_Unbounded_String;
      end if;
      if Length (Self.Stderr) > 0 then
         Log.Info ("E:|" & To_String (Self.Stderr) & "|");
         Self.Stderr := Null_Unbounded_String;
      end if;
   exception
      when E : others =>
         Log.Warning ("Spawn finished exception " & Exception_Message (E));
         Self.Stopped := True;
   end Finished;

   overriding
   procedure Error_Occurred (Self : in out Listener; Process_Error : Integer)
   is
      use Spawn.Processes;
      use Ada.Exceptions;
   begin
      Log.Info ("Error_Occurred:" & (Process_Error'Img));
      Self.Result := Process_Exit_Code (Process_Error);
      Self.Stopped := True;
   exception
      when E : others =>
         Log.Warning ("Spawn error exception " & Exception_Message (E));
         Self.Stopped := True;
   end Error_Occurred;

end Spawn_Listeners;
