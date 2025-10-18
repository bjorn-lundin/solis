
------------------------------------------------------------------------------
package body Binary_Semaphores is
   protected body Semaphore_Type is
      ---------------------------
      procedure Release is
      begin
         In_Use := False;
      end Release;
      ---------------------------
      entry Seize when not In_Use is
      begin
         In_Use := True;
      end Seize;
      ---------------------------
   end Semaphore_Type;
end Binary_Semaphores;
