

------------------------------------------------------------------------------
package Binary_Semaphores is
   pragma Pure;
   protected type Semaphore_Type is
      ---------------------------
      procedure Release;
      ---------------------------
      entry Seize;
      ---------------------------
   private
      In_Use : Boolean := False;
   end Semaphore_Type;
end Binary_Semaphores;
