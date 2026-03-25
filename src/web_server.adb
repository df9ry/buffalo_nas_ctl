with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Calendar;          use Ada.Calendar;
with Ada.Interrupts;
with Ada.Interrupts.Names;
with App_Global;            use App_Global;
with WoL_Task;              use WoL_Task;
with ISO_Time;              use ISO_Time;
with AWS.Config.Set;
with AWS.Response;
with AWS.Status;
with AWS.MIME;
with AWS.Config;
with AWS.Messages;
with AWS.Client;
with AWS.Dispatchers.Callback;

with Log;
with Script;

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
         Log.Info ("Interrupt signal received.");
      end Wait_Until_Shutdown;

      procedure On_Term_Signal is
      begin
         Done := True;
      end On_Term_Signal;
   end Shutdown_Control;

   function Request_Handler
     (Request : AWS.Status.Data) return AWS.Response.Data
   is
      function Json_Response
        (Status_Code : AWS.Messages.Status_Code; The_Body : String)
         return AWS.Response.Data is
      begin
         return
           AWS.Response.Build
             (Content_Type => AWS.MIME.Application_JSON,
              Message_Body => The_Body,
              Status_Code  => Status_Code);
      end Json_Response;

      function Retry_After return Time is
      begin
         return Task_Monitor.Get_Last_Shutdown_Time + Duration (NAS_Shutdown);
      end Retry_After;

      Method : constant String := AWS.Status.Method (Request);
      URI    : constant String := AWS.Status.URI (Request);
      --  Params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);

   begin
      Log.Debug ("Received Method:" & Method & " with URI """ & URI & """");
      if Method = "OPTIONS" then
         return Json_Response (AWS.Messages.S200, "{}");
      elsif Method = "GET" then
         --  Check for health request
         if URI = "/api/health" then
            return Json_Response (AWS.Messages.S200, "{""state"":""ok""}");
         end if;
         --  Check for poll request
         if URI = "/api/poll" then
            Task_Monitor.Set_Last_Poll_Time (Clock);
            if Task_Monitor.Is_NAS_Online then
               if Script.Script_Monitor.Is_Running then
                  --  NAS online, mount running
                  return
                    Json_Response
                      (AWS.Messages.S201, "{""state"":""pending""}");
               end if;
               if Script.Script_Monitor.Get_Result = 0 then
                  --  NAS online, mount good
                  return
                    Json_Response (AWS.Messages.S200, "{""state"":""ok""}");
               end if;
               --  NAS online, script failed
               if Task_Monitor.Get_Last_Error_Time + Duration (NAS_Shutdown)
                 < Clock
               then
                  --  Healing time is not over. Continue sending error state.
                  return
                    Json_Response
                      (AWS.Messages.S401, "{""state"":""failed""}");
               end if;
               --  Healing time is over - Script can start for next try
               Script.Start;
               return
                 Json_Response (AWS.Messages.S201, "{""state"":""pending""}");
            end if;
            --  NAS offline
            if Task_Monitor.Get_Last_Shutdown_Time + Duration (NAS_Shutdown)
              < Clock
            then
               --  NAS can start
               WoL_Task.Start;
               Script.Start;
               return
                 Json_Response (AWS.Messages.S201, "{""state"":""pending""}");
            end if;
            --  NAS offline and in shutdown
            return
              Json_Response
                (AWS.Messages.S503,
                 "{""state"":""shutdown"","
                 & """retry_after"":"""
                 & Image (Retry_After)
                 & """}");
         else
            --  Undefined URI
            return
              Json_Response
                (AWS.Messages.S404, "{""error"":""Not found: " & URI & """}");
         end if;
      end if;
      --  Invalid method
      return
        Json_Response
          (AWS.Messages.S404,
           "{""error"":""Invalid method: " & Method & """}");
   exception
      when others =>
         return
           Json_Response
             (AWS.Messages.S500, "{""error"":""Internal server error""}");
   end Request_Handler;

   procedure Run is
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
           & "/api/health";

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
      AWS.Config.Set.Reuse_Address (My_Config, True);
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
      end if;

      AWS.Server.Shutdown (Server);
      AWS.Server.Wait;

      Log.Info ("Web server stopped.");
   end Run;
end Web_Server;
