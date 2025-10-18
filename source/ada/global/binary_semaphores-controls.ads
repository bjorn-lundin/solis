------------------------------------------------------------------------------
--
-- COPYRIGHT     Consafe Logistics AB
-- FILE NAME     Binary_Semaphores-Controls.ads
-- RESPONSIBLE	
-- DESCRIPTION   Specification for package Binary_Semaphores.Controls
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
with Ada.Finalization;

package Binary_Semaphores.Controls is

   pragma Preelaborate;

   type Semaphore_Control (Semaphore : access Semaphore_Type) is
      limited private;

  -- Example ------------------------------------------
  -- with Binary_Semaphores.Controls;
  -- with TEXT_IO;
  -- 
  -- package body MESSAGE_TRACE is
  --   use Binary_Semaphores;
  --   use Binary_Semaphores.Controls;
  -- 
  --   Semaphore : aliased Semaphore_Type;
  --   procedure Enable is
  --     Control : Semaphore_Control (Semaphore'Access);
  --   begin
  --     Enabled := TRUE;
  --   end Enable;
  --   .
  --   .
  --   .
  -- end MESSAGE_TRACE
  -- 
  -- When procedure Enable is called the Initialize procedure is executed
  -- When procedure Enable reached the end statement, the Finalize procedure 
  -- will be executed.
  -----------------------------------------------------


private

   use Ada.Finalization;

   type Semaphore_Control (Semaphore : access Semaphore_Type) is
     new Limited_Controlled with null record;

   procedure Initialize (Control : in out Semaphore_Control);

   procedure Finalize (Control : in out Semaphore_Control);

end Binary_Semaphores.Controls;
