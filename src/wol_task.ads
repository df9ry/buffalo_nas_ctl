package WoL_Task is

   task Worker is
      entry Start;
      entry Pause;
      entry Continue;
      entry Shutdown;
   end Worker;

end WoL_Task;
