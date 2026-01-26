with App_Global;            use App_Global;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Interrupts;
with Ada.Interrupts.Names;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with AWS.Config.Set;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Parameters;
with AWS.MIME;
with AWS.Config;
with AWS.Messages;

package body Web_Server is

   protected type Signal_Receiver is
      function Should_Shutdown return Boolean;
   private
      procedure Handler;
      pragma Interrupt_Handler (Handler);
      pragma Attach_Handler (Handler, Ada.Interrupts.Names.SIGTERM);
      pragma Attach_Handler (Handler, Ada.Interrupts.Names.SIGQUIT);

      Shutdown_Flag : Boolean := False;
   end Signal_Receiver;

   protected body Signal_Receiver is
      function Should_Shutdown return Boolean is
      begin
         return Shutdown_Flag;
      end Should_Shutdown;

      procedure Handler is
      begin
         Shutdown_Flag := True;
      end Handler;
   end Signal_Receiver;

   --  Globales Instance
   Receiver : Signal_Receiver;

   package Timestamp_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   NAS_Map : Timestamp_Maps.Map;

   --  Request Handler
   function Request_Handler
     (Request : AWS.Status.Data) return AWS.Response.Data
   is

      function Get_Param
        (Params : AWS.Parameters.List; Param : String) return String is
      begin
         if AWS.Parameters.Exist (Params, Param) then
            return Aws.Parameters.Get (Params, Param);
         else
            return "";
         end if;
      end Get_Param;

      URI    : constant String := AWS.Status.URI (Request);
      Method : constant String := AWS.Status.Method (Request);
      Params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);

      Guid      : constant String := Get_Param (Params, "guid");
      Timestamp : constant String := Get_Param (Params, "timestamp");

      function JSON_Response
        (Status_Code : AWS.Messages.Status_Code; The_Body : String)
         return AWS.Response.Data is
      begin
         return
           AWS.Response.Build
             (Content_Type => AWS.MIME.Application_JSON,
              Message_Body => The_Body,
              Status_Code  => Status_Code);
      end JSON_Response;

   begin
      --  OPTIONS für CORS Preflight
      if Method = "OPTIONS" then
         --  Einfache leere Response mit Status 200
         return
           AWS.Response.Build
             (Content_Type => AWS.MIME.Text_HTML,
              Message_Body => "",
              Status_Code  => AWS.Messages.S200);
      end if;

      --  POST /nas - Neue/Update GUID
      if Method = "POST" and then URI = "/nas" then
         if Guid = "" or else Timestamp = "" then
            return
              JSON_Response
                (AWS.Messages.S400,
                 "{""error"":""guid and timestamp parameters required""}");
         end if;

         NAS_Map.Include (Guid, Timestamp);

         return
           JSON_Response
             (AWS.Messages.S201,
              "{""status"":""created/updated"","
              & """guid"":"""
              & Guid
              & """,""timestamp"":"""
              & Timestamp
              & """}");

      --  GET /nas - Alle GUIDs
      elsif Method = "GET" and then URI = "/nas" then
         declare
            Result     : Unbounded_String := To_Unbounded_String ("[");
            First_Item : Boolean := True;
         begin
            for C in NAS_Map.Iterate loop
               if not First_Item then
                  Append (Result, ",");
               else
                  First_Item := False;
               end if;
               Append (Result, "{""guid"":""");
               Append (Result, Timestamp_Maps.Key (C));
               Append (Result, """,""timestamp"":""");
               Append (Result, Timestamp_Maps.Element (C));
               Append (Result, """}");
            end loop;

            Append (Result, "]");

            return JSON_Response (AWS.Messages.S200, To_String (Result));
         end;

      --  GET /nas?guid=... - Spezifische GUID
      elsif Method = "GET" and then URI = "/nas" and then Guid /= "" then
         if NAS_Map.Contains (Guid) then
            return
              JSON_Response
                (AWS.Messages.S200,
                 "{""guid"":"""
                 & Guid
                 & """,""timestamp"":"""
                 & NAS_Map.Element (Guid)
                 & """}");
         else
            return
              JSON_Response
                (AWS.Messages.S404, "{""error"":""GUID not found""}");
         end if;

      --  DELETE /nas?guid=... - GUID löschen
      elsif Method = "DELETE" and then URI = "/nas" and then Guid /= "" then
         if NAS_Map.Contains (Guid) then
            NAS_Map.Delete (Guid);
            return
              JSON_Response
                (AWS.Messages.S200,
                 "{""status"":""deleted""," & """guid"":""" & Guid & """}");
         else
            return
              JSON_Response
                (AWS.Messages.S404, "{""error"":""GUID not found""}");
         end if;

      --  GET /stats - Statistik
      elsif Method = "GET" and then URI = "/stats" then
         return
           JSON_Response
             (AWS.Messages.S200,
              "{""total_guids"":"
              & Ada.Containers.Count_Type'Image (NAS_Map.Length)
              & "}");

      else
         --  Fallback / Hilfe
         declare
            Help_Text : constant String :=
              "<html><body>"
              & "<h1>NAS API Server</h1>"
              & "<h2>Endpoints:</h2>"
              & "<ul>"
              & "<li>POST /nas?guid={uuid}&timestamp={iso8601} "
              & "- Add/Update</li>"
              & "<li>GET /nas - All GUIDs</li>"
              & "<li>GET /nas?guid={uuid} - Specific GUID</li>"
              & "<li>DELETE /nas?guid={uuid} - Delete GUID</li>"
              & "<li>GET /stats - Statistics</li>"
              & "</ul>"
              & "</body></html>";
         begin
            return AWS.Response.Build (AWS.MIME.Text_HTML, Help_Text);
         end;
      end if;

   exception
      when others =>
         return
           JSON_Response
             (AWS.Messages.S500, "{""error"":""Internal server error""}");
   end Request_Handler;

   procedure Run is

      Server : AWS.Server.HTTP;
      Config : AWS.Config.Object;

   begin
      --  Server konfigurieren
      Config := AWS.Config.Default_Config;
      AWS.Config.Set.Server_Port (Config, Svc_Port);
      AWS.Config.Set.Server_Host (Config, To_String (Svc_Interface));
      AWS.Config.Set.Max_Connection (Config, 10);
      AWS.Config.Set.Accept_Queue_Size (Config, 5);

      Log.Info ("Start server listen on " & To_String (Svc_Interface) &
                  " Port " & Integer'Image (Svc_Port));
      AWS.Server.Start (Server,
         Name     => App_Name,
         Callback => Request_Handler'Access);

      while not Receiver.Should_Shutdown loop
         delay 0.5;
      end loop;

      --  Server stoppen
      AWS.Server.Shutdown (Server);
      Log.Info ("Server stopped.");

   end Run;

end Web_Server;
