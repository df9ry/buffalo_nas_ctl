with Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Characters.Handling;
with Ada.Environment_Variables;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Log is

   package Unbounded_String_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Unbounded_String);
   use Unbounded_String_Vectors;

   use Ada.Text_IO;

   protected Protected_Log is
      procedure Log (Lvl : Log_Level; Msg : String);
      procedure Set_Level (Lvl : Log_Level);
      function Get_Level return Log_Level;
      procedure Set_Output (To_Stderr : Boolean);
   private
      Current_Level    : Log_Level := Info;
      Output_To_Stderr : Boolean := True;
      Initialized      : Boolean := False;
   end Protected_Log;

   protected body Protected_Log is

      procedure Log (Lvl : Log_Level; Msg : String) is
         Level_Str : constant String :=
           (case Lvl is
              when Trace => "[TRACE]",
              when Debug => "[DEBUG]",
              when Info => "[INFO ]",
              when Warning => "[WARN ]",
              when Error => "[ERROR]",
              when Fatal => "[FATAL]",
              when Off => "[OFF  ]");

         Timestamp : constant String :=
           Ada.Calendar.Formatting.Image
             (Date                  => Ada.Calendar.Clock,
              Include_Time_Fraction => True,
              Time_Zone             => 0);
      begin
         if not Initialized then
            if Ada.Environment_Variables.Exists ("LOG_LEVEL") then
               declare
                  Env : constant String :=
                    Ada.Environment_Variables.Value ("LOG_LEVEL");
                  Upper_Env : constant String :=
                    Ada.Characters.Handling.To_Upper (Env);
               begin
                  if Upper_Env = "TRACE" then
                     Current_Level := Trace;
                  elsif Upper_Env = "DEBUG" then
                     Current_Level := Debug;
                  elsif Upper_Env = "INFO" then
                     Current_Level := Info;
                  elsif Upper_Env = "WARN" or else Upper_Env = "WARNING" then
                     Current_Level := Warning;
                  elsif Upper_Env = "ERROR" then
                     Current_Level := Error;
                  elsif Upper_Env = "FATAL" then
                     Current_Level := Fatal;
                  elsif Upper_Env = "OFF" then
                     Current_Level := Off;
                  end if;
               exception
                  when others =>
                     null;
               end;
            end if;
            Initialized := True;
         end if;

         if Lvl in Off .. Current_Level then
            declare
               Full : constant String :=
                 Timestamp & " " & Level_Str & " " & Msg;
            begin
               if Output_To_Stderr then
                  Put_Line (Standard_Error, Full);
                  Flush (Standard_Error);
               else
                  Put_Line (Standard_Output, Full);
                  Flush (Standard_Output);
               end if;
            end;
         end if;
      end Log;

      procedure Set_Level (Lvl : Log_Level) is
      begin
         Current_Level := Lvl;
      end Set_Level;

      function Get_Level return Log_Level
      is (Current_Level);

      procedure Set_Output (To_Stderr : Boolean) is
      begin
         Output_To_Stderr := To_Stderr;
      end Set_Output;

   end Protected_Log;

   function Replace (Template : String; Args : Vector) return String is
      Result : Unbounded_String := To_Unbounded_String (Template);
      Pos    : Natural;
   begin
      for I in 1 .. Natural (Args.Length) loop
         declare
            Ph  : constant String := "{" & Positive'Image (I) & "}";
            Val : constant String := To_String (Args.Element (I));
         begin
            loop
               Pos := Index (Result, Ph);
               exit when Pos = 0;
               Replace_Slice
                 (Result, Pos, Pos + Ph'Length - 1, Val);
            end loop;
         end;
      end loop;
      return To_String (Result);
   end Replace;

   procedure Set_Level (Level : Log_Level) is
   begin
      Protected_Log.Set_Level (Level);
   end Set_Level;

   procedure Set_Level (S : String) is
      Upper_S : constant String := Ada.Characters.Handling.To_Upper (S);
   begin
      if Upper_S = "TRACE" then
         Protected_Log.Set_Level (Trace);
      elsif Upper_S = "DEBUG" then
         Protected_Log.Set_Level (Debug);
      elsif Upper_S = "INFO" then
         Protected_Log.Set_Level (Info);
      elsif Upper_S = "WARN" or else Upper_S = "WARNING" then
         Protected_Log.Set_Level (Warning);
      elsif Upper_S = "ERROR" then
         Protected_Log.Set_Level (Error);
      elsif Upper_S = "FATAL" then
         Protected_Log.Set_Level (Fatal);
      elsif Upper_S = "OFF" then
         Protected_Log.Set_Level (Off);
      else
         raise Constraint_Error with "Ungültiger Log-Level: '" & S & "'";
      end if;
      Protected_Log.Log (Info, "Log-Level changed to " & Upper_S);
   end Set_Level;

   function Current_Level return Log_Level
   is (Protected_Log.Get_Level);

   procedure Initialize
     (Default_Level : Log_Level := Info; Use_Stderr : Boolean := True) is
   begin
      Protected_Log.Set_Level (Default_Level);
      Protected_Log.Set_Output (Use_Stderr);
   end Initialize;

   procedure Trace (Message : String) is
   begin
      Protected_Log.Log (Trace, Message);
   end Trace;

   procedure Debug (Message : String) is
   begin
      Protected_Log.Log (Debug, Message);
   end Debug;

   procedure Info (Message : String) is
   begin
      Protected_Log.Log (Info, Message);
   end Info;

   procedure Warning (Message : String) is
   begin
      Protected_Log.Log (Warning, Message);
   end Warning;

   procedure Error (Message : String) is
   begin
      Protected_Log.Log (Error, Message);
   end Error;

   procedure Fatal (Message : String) is
   begin
      Protected_Log.Log (Fatal, Message);
   end Fatal;

   procedure Log_One (Lvl : Log_Level; Template : String; Arg : String) is
      V : Vector;
   begin
      V.Append (To_Unbounded_String (Arg));
      Protected_Log.Log (Lvl, Replace (Template, V));
   end Log_One;

   procedure Log_One (Lvl : Log_Level; Template : String;
                      Arg : Unbounded_String) is
      V : Vector;
   begin
      V.Append (Arg);
      Protected_Log.Log (Lvl, Replace (Template, V));
   end Log_One;

   procedure Log_One (Lvl : Log_Level; Template : String; Arg : Integer) is
      V : Vector;
   begin
      V.Append (To_Unbounded_String (Integer'Image (Arg)));
      Protected_Log.Log (Lvl, Replace (Template, V));
   end Log_One;

   procedure Log_One (Lvl : Log_Level; Template : String; Arg : Float) is
      V : Vector;
   begin
      V.Append (To_Unbounded_String (Float'Image (Arg)));
      Protected_Log.Log (Lvl, Replace (Template, V));
   end Log_One;

   procedure Log_One (Lvl : Log_Level; Template : String; Arg : Boolean) is
      V : Vector;
      S : constant String := (if Arg then "true" else "false");
   begin
      V.Append (To_Unbounded_String (S));
      Protected_Log.Log (Lvl, Replace (Template, V));
   end Log_One;

   procedure Trace (Template : String; Arg : String) is
   begin
      Log_One (Trace, Template, Arg);
   end Trace;

   procedure Trace (Template : String; Arg : Unbounded_String) is
   begin
      Log_One (Trace, Template, Arg);
   end Trace;

   procedure Trace (Template : String; Arg : Integer) is
   begin
      Log_One (Trace, Template, Arg);
   end Trace;

   procedure Trace (Template : String; Arg : Float) is
   begin
      Log_One (Trace, Template, Arg);
   end Trace;

   procedure Trace (Template : String; Arg : Boolean) is
   begin
      Log_One (Trace, Template, Arg);
   end Trace;

   procedure Debug (Template : String; Arg : String) is
   begin
      Log_One (Debug, Template, Arg);
   end Debug;

   procedure Debug (Template : String; Arg : Unbounded_String) is
   begin
      Log_One (Debug, Template, Arg);
   end Debug;

   procedure Debug (Template : String; Arg : Integer) is
   begin
      Log_One (Debug, Template, Arg);
   end Debug;

   procedure Debug (Template : String; Arg : Float) is
   begin
      Log_One (Debug, Template, Arg);
   end Debug;

   procedure Debug (Template : String; Arg : Boolean) is
   begin
      Log_One (Debug, Template, Arg);
   end Debug;

   procedure Info (Template : String; Arg : String) is
   begin
      Log_One (Info, Template, Arg);
   end Info;

   procedure Info (Template : String; Arg : Unbounded_String) is
   begin
      Log_One (Info, Template, Arg);
   end Info;

   procedure Info (Template : String; Arg : Integer) is
   begin
      Log_One (Info, Template, Arg);
   end Info;

   procedure Info (Template : String; Arg : Float) is
   begin
      Log_One (Info, Template, Arg);
   end Info;

   procedure Info (Template : String; Arg : Boolean) is
   begin
      Log_One (Info, Template, Arg);
   end Info;

   procedure Warning (Template : String; Arg : String) is
   begin
      Log_One (Warning, Template, Arg);
   end Warning;

   procedure Warning (Template : String; Arg : Unbounded_String) is
   begin
      Log_One (Warning, Template, Arg);
   end Warning;

   procedure Warning (Template : String; Arg : Integer) is
   begin
      Log_One (Warning, Template, Arg);
   end Warning;

   procedure Warning (Template : String; Arg : Float) is
   begin
      Log_One (Warning, Template, Arg);
   end Warning;

   procedure Warning (Template : String; Arg : Boolean) is
   begin
      Log_One (Warning, Template, Arg);
   end Warning;

   procedure Error (Template : String; Arg : String) is
   begin
      Log_One (Error, Template, Arg);
   end Error;

   procedure Error (Template : String; Arg : Unbounded_String) is
   begin
      Log_One (Error, Template, Arg);
   end Error;

   procedure Error (Template : String; Arg : Integer) is
   begin
      Log_One (Error, Template, Arg);
   end Error;

   procedure Error (Template : String; Arg : Float) is
   begin
      Log_One (Error, Template, Arg);
   end Error;

   procedure Error (Template : String; Arg : Boolean) is
   begin
      Log_One (Error, Template, Arg);
   end Error;

   procedure Fatal (Template : String; Arg : String) is
   begin
      Log_One (Fatal, Template, Arg);
   end Fatal;

   procedure Fatal (Template : String; Arg : Unbounded_String) is
   begin
      Log_One (Fatal, Template, Arg);
   end Fatal;

   procedure Fatal (Template : String; Arg : Integer) is
   begin
      Log_One (Fatal, Template, Arg);
   end Fatal;

   procedure Fatal (Template : String; Arg : Float) is
   begin
      Log_One (Fatal, Template, Arg);
   end Fatal;

   procedure Fatal (Template : String; Arg : Boolean) is
   begin
      Log_One (Fatal, Template, Arg);
   end Fatal;

   procedure Debug
     (Template : String;
      Arg1     : Unbounded_String;
      Arg2     : Unbounded_String := Null_Unbounded_String;
      Arg3     : Unbounded_String := Null_Unbounded_String;
      Arg4     : Unbounded_String := Null_Unbounded_String)
   is
      V : Vector;
   begin
      V.Append (Arg1);
      if Arg2 /= Null_Unbounded_String then
         V.Append (Arg2);
      end if;
      if Arg3 /= Null_Unbounded_String then
         V.Append (Arg3);
      end if;
      if Arg4 /= Null_Unbounded_String then
         V.Append (Arg4);
      end if;
      Protected_Log.Log (Debug, Replace (Template, V));
   end Debug;

end Log;
