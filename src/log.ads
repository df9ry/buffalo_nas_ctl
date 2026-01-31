with Ada.Strings.Unbounded;

package Log is

   type Log_Level is (Off, Fatal, Error, Warning, Info, Debug, Trace);

   --  Setzt den globalen Log-Level (wirkt sofort in allen Tasks)
   procedure Set_Level (Level : Log_Level);
   procedure Set_Level (S : String);

   --  Aktueller Log-Level
   function Current_Level return Log_Level;

   --  Einfache Log-Methoden ohne Formatierung
   procedure Trace   (Message : String);
   procedure Debug   (Message : String);
   procedure Info    (Message : String);
   procedure Warning (Message : String);
   procedure Error   (Message : String);
   procedure Fatal   (Message : String);

   --  Formatierte Logs mit einem Argument
   procedure Trace   (Template : String; Arg : String);
   procedure Trace   (Template : String;
                      Arg : Ada.Strings.Unbounded.Unbounded_String);
   procedure Trace   (Template : String; Arg : Integer);
   procedure Trace   (Template : String; Arg : Float);
   procedure Trace   (Template : String; Arg : Boolean);

   procedure Debug   (Template : String; Arg : String);
   procedure Debug   (Template : String;
                      Arg : Ada.Strings.Unbounded.Unbounded_String);
   procedure Debug   (Template : String; Arg : Integer);
   procedure Debug   (Template : String; Arg : Float);
   procedure Debug   (Template : String; Arg : Boolean);

   procedure Info    (Template : String; Arg : String);
   procedure Info    (Template : String;
                      Arg : Ada.Strings.Unbounded.Unbounded_String);
   procedure Info    (Template : String; Arg : Integer);
   procedure Info    (Template : String; Arg : Float);
   procedure Info    (Template : String; Arg : Boolean);

   procedure Warning (Template : String; Arg : String);
   procedure Warning (Template : String;
                      Arg : Ada.Strings.Unbounded.Unbounded_String);
   procedure Warning (Template : String; Arg : Integer);
   procedure Warning (Template : String; Arg : Float);
   procedure Warning (Template : String; Arg : Boolean);

   procedure Error   (Template : String; Arg : String);
   procedure Error   (Template : String;
                      Arg : Ada.Strings.Unbounded.Unbounded_String);
   procedure Error   (Template : String; Arg : Integer);
   procedure Error   (Template : String; Arg : Float);
   procedure Error   (Template : String; Arg : Boolean);

   procedure Fatal   (Template : String; Arg : String);
   procedure Fatal   (Template : String;
                      Arg : Ada.Strings.Unbounded.Unbounded_String);
   procedure Fatal   (Template : String; Arg : Integer);
   procedure Fatal   (Template : String; Arg : Float);
   procedure Fatal   (Template : String; Arg : Boolean);

   --  Formatierte Logs mit bis zu 4 Argumenten (alle als Unbounded_String)
   procedure Debug (Template : String;
                    Arg1 : Ada.Strings.Unbounded.Unbounded_String;
                    Arg2 : Ada.Strings.Unbounded.Unbounded_String :=
                      Ada.Strings.Unbounded.Null_Unbounded_String;
                    Arg3 : Ada.Strings.Unbounded.Unbounded_String :=
                      Ada.Strings.Unbounded.Null_Unbounded_String;
                    Arg4 : Ada.Strings.Unbounded.Unbounded_String :=
                      Ada.Strings.Unbounded.Null_Unbounded_String);

   procedure Info  (Template : String;
                    Arg1 : Ada.Strings.Unbounded.Unbounded_String;
                    Arg2 : Ada.Strings.Unbounded.Unbounded_String :=
                      Ada.Strings.Unbounded.Null_Unbounded_String;
                    Arg3 : Ada.Strings.Unbounded.Unbounded_String :=
                      Ada.Strings.Unbounded.Null_Unbounded_String;
                    Arg4 : Ada.Strings.Unbounded.Unbounded_String :=
                      Ada.Strings.Unbounded.Null_Unbounded_String);

   --  Einmalige Initialisierung (optional – wird sonst lazy beim ersten Log
   --  gemacht)
   procedure Initialize
     (Default_Level : Log_Level := Info;
      Use_Stderr    : Boolean   := True);

end Log;
