--  iso_time.adb

package body ISO_Time is

   --  Hilfsfunktion für zweistellige Zahlen mit führender Null
   function Zwei_Ziffern (Wert : Integer) return String is
      Bild : constant String := Integer'Image (Wert);
   begin
      if Wert < 10 then
         return "0" & Bild (2 .. Bild'Last);  --  Führendes Leerzeichen
      else
         return Bild (2 .. Bild'Last);        --  Leerzeichen entfernen
      end if;
   end Zwei_Ziffern;

   --  Hilfsfunktion für vierstellige Jahreszahl
   function Vier_Ziffern (Wert : Integer) return String is
      Bild : constant String := Integer'Image (Wert);
   begin
      if Wert < 10 then
         return "000" & Bild (2 .. Bild'Last);
      elsif Wert < 100 then
         return "00" & Bild (2 .. Bild'Last);
      elsif Wert < 1000 then
         return "0" & Bild (2 .. Bild'Last);
      else
         return Bild (2 .. Bild'Last);
      end if;
   end Vier_Ziffern;

   --  Hauptfunktion: ISO-Format YYYY-MM-DD HH:MM:SS
   function Image (T : Ada.Calendar.Time) return String is
      use Ada.Calendar;

      Jahr : Year_Number;
      Monat : Month_Number;
      Tag : Day_Number;
      Sekunden : Day_Duration;
      Std, Min, Sek : Integer;

   begin
      Split (T, Jahr, Monat, Tag, Sekunden);

      Std := Integer (Sekunden) / 3600;
      Min := (Integer (Sekunden) mod 3600) / 60;
      Sek := Integer (Sekunden) mod 60;

      return Vier_Ziffern (Jahr) & "-" &
             Zwei_Ziffern (Monat) & "-" &
             Zwei_Ziffern (Tag) & " " &
             Zwei_Ziffern (Std) & ":" &
             Zwei_Ziffern (Min) & ":" &
             Zwei_Ziffern (Sek);
   end Image;

   --  Mit Millisekunden: YYYY-MM-DD HH:MM:SS.sss
   function Image_ms (T : Ada.Calendar.Time) return String is
      use Ada.Calendar;

      Jahr : Year_Number;
      Monat : Month_Number;
      Tag : Day_Number;
      Sekunden : Day_Duration;
      Std, Min, Sek, Ms : Integer;

   begin
      Split (T, Jahr, Monat, Tag, Sekunden);

      Std := Integer (Sekunden) / 3600;
      Min := (Integer (Sekunden) mod 3600) / 60;
      Sek := Integer (Sekunden) mod 60;
      Ms  := Integer ((Sekunden - Day_Duration (Sek)) * 1000.0);

      return Vier_Ziffern (Jahr) & "-" &
             Zwei_Ziffern (Monat) & "-" &
             Zwei_Ziffern (Tag) & " " &
             Zwei_Ziffern (Std) & ":" &
             Zwei_Ziffern (Min) & ":" &
             Zwei_Ziffern (Sek) & "." &
             Zwei_Ziffern (Ms / 10) & Zwei_Ziffern (Ms mod 100);  -- 3-stellig
   end Image_ms;

   --  Für  Dateinamen: YYYY-MM-DD_HH-MM-SS (keine Sonderzeichen)
   function Image_Filename (T : Ada.Calendar.Time) return String is
      Bild : constant String := Image (T);
   begin
      --  Ersetze ':' durch '-' und Leerzeichen durch '_'
      declare
         Ergebnis : String (Bild'Range) := Bild;
      begin
         for I in Ergebnis'Range loop
            if Ergebnis (I) = ':' then
               Ergebnis (I) := '-';
            elsif Ergebnis (I) = ' ' then
               Ergebnis (I) := '_';
            end if;
         end loop;
         return Ergebnis;
      end;
   end Image_Filename;

end ISO_Time;
