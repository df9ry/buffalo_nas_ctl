with App_Global;            use App_Global;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Interrupts;
with Ada.Interrupts.Names;

with AWS.Config.Set;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Parameters;
with AWS.MIME;
with AWS.Config;
with AWS.Messages;

with Log;
with UUIDs;
with UUID_Timestamp_Map;

package body Web_Server is

   protected Shutdown_Control is
      entry Wait_Until_Shutdown;

      procedure On_Term_Signal;
      pragma Attach_Handler (On_Term_Signal, Ada.Interrupts.Names.SIGTERM);
      pragma Attach_Handler (On_Term_Signal, Ada.Interrupts.Names.SIGQUIT);
      pragma Attach_Handler (On_Term_Signal, Ada.Interrupts.Names.SIGHUP);

   private
      Done : Boolean := False;
   end Shutdown_Control;

   protected body Shutdown_Control is

      entry Wait_Until_Shutdown when Done is
      begin
         null;
      end Wait_Until_Shutdown;

      procedure On_Term_Signal is
      begin
         Done := True;
      end On_Term_Signal;

   end Shutdown_Control;

   --  Request Handler
   function Request_Handler
     (Request : AWS.Status.Data) return AWS.Response.Data
   is
      use UUID_Timestamp_Map;

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

      Guid_S : constant String := Get_Param (Params, "guid");
      Guid   : constant UUIDs.UUID := UUIDs.From_String (Guid_S);

      function Text_Response
        (Status_Code : AWS.Messages.Status_Code; The_Body : String)
         return AWS.Response.Data is
      begin
         return
           AWS.Response.Build
             (Content_Type => AWS.MIME.Text_Plain,
              Message_Body => The_Body,
              Status_Code  => Status_Code);
      end Text_Response;

      function OK_Response return AWS.Response.Data is
      begin
         return Text_Response (AWS.Messages.S200, "OK");
      end OK_Response;

   begin
      --  OPTIONS für CORS Preflight
      if Method = "OPTIONS" then
         return Text_Response (AWS.Messages.S200, "");
      end if;

      --  POST /nas - Neue/Update GUID
      if Method = "POST" then
         if URI = "/new" then
            if not Try_Put (Guid) then
               return Text_Response (AWS.Messages.S403, "NAS in shutdown");
            end if;
         elsif URI = "/del" then
            Status_Map.Remove (Guid);
         else
            return Text_Response (AWS.Messages.S404, "Not found: " & URI);
         end if;
         return OK_Response;
      end if;

      --  Other method:
      return Text_Response (AWS.Messages.S404, "Invalid method: " & Method);

   exception
      when others =>
         return Text_Response (AWS.Messages.S500, "Internal server error");
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

      Log.Info
        ("Start server listen on "
         & To_String (Svc_Interface)
         & " Port "
         & Integer'Image (Svc_Port));

      AWS.Server.Start
        (Server, Name => App_Name, Callback => Request_Handler'Access);

      Shutdown_Control.Wait_Until_Shutdown;

      --  Server stoppen
      AWS.Server.Shutdown (Server);
      Log.Info ("Server stopped.");

   end Run;

end Web_Server;
