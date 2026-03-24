
with Interfaces.C;
with Log;

package body App_Global is

   protected body Run_Guard is

      procedure Stop is
      begin
         Running := False;
      end Stop;

      function Is_Running return Boolean is
      begin
         return Running;
      end;

      procedure Kill (Exit_Status : Integer) is
         use Interfaces.C;
         procedure kill (status : int);
         pragma Import (C, kill, "_exit");
      begin
         Log.Fatal ("Program KILL with " & Exit_Status'Image);
         kill (int (Exit_Status));
      end Kill;

   end Run_Guard;

end App_Global;
