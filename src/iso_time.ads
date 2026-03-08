-- iso_time.ads
with Ada.Calendar;

package ISO_Time is
   
   --  Gibt das Datum im ISO-Format zurück: YYYY-MM-DD HH:MM:SS
   --  Für Logging-Zwecke mit führenden Nullen
   function Image (T : Ada.Calendar.Time) return String;
   
   --  Zusätzliche Variante mit Millisekunden
   function Image_ms (T : Ada.Calendar.Time) return String;
   
   --  Zusätzliche Variante für Dateinamen (keine Leerzeichen)
   function Image_Filename (T : Ada.Calendar.Time) return String;
   
end ISO_Time;
