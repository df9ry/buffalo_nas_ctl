with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

package body Config_File is

   use Ada.Strings;
   use Ada.Strings.Fixed;

   function Make_Key (Section, Key : String) return String is
   begin
      if Section = "" then
         return Key;
      else
         return Section & "." & Key;
      end if;
   end Make_Key;

   procedure Load (Config : in out Configuration; Filename : String) is
      File         : Ada.Text_IO.File_Type;
      Line_Buffer  : String (1 .. 1024);
      Last         : Natural;
      Current_Section : String := "";
   begin
      --  Clear existing data
      Config.Data.Clear;

      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Filename);

      while not Ada.Text_IO.End_Of_File (File) loop
         Ada.Text_IO.Get_Line (File, Line_Buffer, Last);

         if Last > 0 then
            declare
               Line : constant String := Line_Buffer (1 .. Last);
               Trimmed : constant String := Trim (Line, Both);
            begin
               --  Skip empty lines and comments
               if Trimmed'Length = 0
                 or else Trimmed (Trimmed'First) = '#'
                 or else Trimmed (Trimmed'First) = ';'
               then
                  null;

               --  Check for section header
               elsif Trimmed (Trimmed'First) = '['
                 and then Trimmed (Trimmed'Last) = ']'
               then
                  --  Extract section name (remove brackets)
                  Current_Section :=
                    Trim (Trimmed (Trimmed'First + 1 .. Trimmed'Last - 1),
                          Both);

               --  Key-value pair
               else
                  declare
                     Pos : Natural;
                     Full_Line : constant String := Trimmed;
                  begin
                     Pos := Index (Full_Line, "=");
                     if Pos > 0 then
                        declare
                           Key : constant String :=
                             Trim (Full_Line (Full_Line'First .. Pos - 1),
                                   Both);
                           Value : constant String :=
                             Trim (Full_Line (Pos + 1 .. Full_Line'Last),
                                   Both);
                        begin
                           if Key'Length > 0 then
                              Config.Data.Include
                                (Make_Key (Current_Section, Key),
                                 Value);
                           end if;
                        end;
                     end if;
                  end;
               end if;
            end;
         end if;
      end loop;

      Ada.Text_IO.Close (File);

   exception
      when Ada.Text_IO.Name_Error =>
         raise;
      when others =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
         raise;
   end Load;

   function Get (Config : Configuration;
                 Section, Key : String) return String is
      Full_Key : constant String := Make_Key (Section, Key);
   begin
      if Config.Data.Contains (Full_Key) then
         return Config.Data.Element (Full_Key);
      else
         return "";
      end if;
   end Get;

   function Get (Config : Configuration;
                 Section, Key : String;
                 Default : String) return String is
      Result : constant String := Get (Config, Section, Key);
   begin
      if Result'Length > 0 then
         return Result;
      else
         return Default;
      end if;
   end Get;

   function Get_Int (Config : Configuration;
                     Section, Key : String;
                     Default : Integer := 0) return Integer is
      Value : constant String := Get (Config, Section, Key);
   begin
      if Value'Length = 0 then
         return Default;
      end if;

      return Integer'Value (Value);
   exception
      when Constraint_Error =>
         return Default;
   end Get_Int;

   function Get_Bool (Config : Configuration;
                      Section, Key : String;
                      Default : Boolean := False) return Boolean is
      Value : constant String := Get (Config, Section, Key);
   begin
      if Value'Length = 0 then
         return Default;
      end if;

      declare
         use Ada.Strings.Maps;
         use Ada.Strings.Maps.Constants;
         Lower_Value : constant String :=
           Translate (Value, Lower_Case_Map);
      begin
         return Lower_Value = "true"
           or else Lower_Value = "yes"
           or else Lower_Value = "1"
           or else Lower_Value = "on";
      end;
   end Get_Bool;

   function Has_Key (Config : Configuration;
                     Section, Key : String) return Boolean is
   begin
      return Config.Data.Contains (Make_Key (Section, Key));
   end Has_Key;

end Config_File;
