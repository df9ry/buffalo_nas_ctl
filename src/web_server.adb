with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Interrupts;
with Ada.Interrupts.Names;
with App_Global;            use App_Global;
with AWS.Config.Set;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Parameters;
with AWS.MIME;
with AWS.Config;
with AWS.Messages;
with AWS.Client;
with AWS.Dispatchers.Callback;

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
   end Shutdown_Control;   --  Request Handler

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
      Log.Debug ("Received Method:" & Method & " with URI """ & URI & """");
      if Method = "OPTIONS" then
         return Text_Response (AWS.Messages.S200, "");
      elsif Method = "GET" then
         if URI = "/health" then
            return OK_Response;
         else
            return Text_Response (AWS.Messages.S404, "Not found: " & URI);
         end if;
      elsif Method = "POST" then
         declare
            Guid_S : constant String := Get_Param (Params, "guid");
            Guid   : constant UUIDs.UUID := UUIDs.From_String (Guid_S);
         begin
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
         end;
      end if;

      --  Other method:
      return Text_Response (AWS.Messages.S404, "Invalid method: " & Method);
   exception
      when others =>
         return Text_Response (AWS.Messages.S500, "Internal server error");
   end Request_Handler;

   procedure Run is
      Server        : AWS.Server.HTTP;
      My_Config     : AWS.Config.Object;
      My_Dispatcher : AWS.Dispatchers.Callback.Handler;

      function Self_Check return Boolean is
         use AWS.Messages;

         R         : AWS.Response.Data;
         Host_Part : constant String :=
           (if To_String (Svc_Interface) in "" | "0.0.0.0" | "::" | "::1"
            then "localhost"
            else To_String (Svc_Interface));
         URL_Str   : constant String :=
           "http://"
           & Host_Part
           & ":"
           & Trim (Integer'Image (Svc_Port), Left)
           & "/health";

      begin
         Log.Info ("Performing self check to " & URL_Str);
         R :=
           AWS.Client.Get
             (URL => URL_Str, Timeouts => AWS.Client.Timeouts (Each => 2.0));
         declare
            Status : constant AWS.Messages.Status_Code :=
              AWS.Response.Status_Code (R);
         begin
            if Status = S200 then
               Log.Debug ("Self test OK");
               return True;
            else
               Log.Error
                 ("Server self test failed with status code " & Status'Image);
               return False;
            end if;
         end;
      end Self_Check;

   begin
      My_Config := AWS.Config.Default_Config;
      AWS.Config.Set.Server_Port (My_Config, Svc_Port);
      AWS.Config.Set.Server_Host (My_Config, To_String (Svc_Interface));
      AWS.Config.Set.Server_Name (My_Config, App_Name);
      AWS.Config.Set.Max_Connection (My_Config, 10);
      AWS.Config.Set.Accept_Queue_Size (My_Config, 5);
      My_Dispatcher :=
        AWS.Dispatchers.Callback.Create (Request_Handler'Access);

      Log.Debug ("AWS Version is " & AWS.Version);

      Log.Info
        ("Start server listen on "
         & To_String (Svc_Interface)
         & " Port"
         & Integer'Image (Svc_Port));

      AWS.Server.Start
        (Server, Config => My_Config, Dispatcher => My_Dispatcher);

      delay 0.5;

      if Self_Check then
         Shutdown_Control.Wait_Until_Shutdown;
      --  AWS.Server.Wait;
      end if;

      AWS.Server.Shutdown (Server);
      Log.Info ("Server stopped.");
   end Run;
end Web_Server;
