------------------------------------------------------------------------------
--
-- COPYRIGHT     Consafe Logistics AB
-- FILE NAME     Binary_Semaphores-Controls.adb
-- RESPONSIBLE	
-- DESCRIPTION   Body for package Binary_Semaphores.Controls
--               This package embeds the Binary_Semaphores with standard
--               Ada procedure initialize and finalize, thus making 
--               take and release of a semaphore, hidden for the caller.
--               Can be used for making package task safe.
--
------------------------------------------------------------------------------
--Vers.     Author    Date         Description
------------------------------------------------------------------------------
--9.5-10276 AXO/SNE   09-Oct-2006  Original version
------------------------------------------------------------------------------
package body Binary_Semaphores.Controls is

   procedure Initialize (Control : in out Semaphore_Control) is
   begin
      Control.Semaphore.Seize;
   end;

   procedure Finalize (Control : in out Semaphore_Control) is
   begin
      Control.Semaphore.Release;
   end;

end Binary_Semaphores.Controls;
