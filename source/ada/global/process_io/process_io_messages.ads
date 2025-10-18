
----------------------------------------

--  BNL 2011-09-21  pio messages

--------------------------------------------------------------------------------

with Process_IO; 
pragma Elaborate(Process_IO);
with Process_Io.Pipe;

package Process_Io_Messages is


  package Pio_Enquire_Status_Package is new Process_Io.Generic_Io
          (Identity        => Process_Io.Pipe.Pio_Enquire_Status_Message,
           Data_Type       => Process_Io.Pipe.Pio_Enquire_Status_Record,
           Data_Descriptor => (1 => Process_IO.Integer_4_Type));

  function Data  (Message   : Process_Io.Message_Type)
                  return      Process_Io.Pipe.Pio_Enquire_Status_Record
                  renames     Pio_Enquire_Status_Package.Data;

  procedure Send (Receiver  : Process_Io.Process_Type;
                  Data      : Process_Io.Pipe.Pio_Enquire_Status_Record;
                  Connection: Process_Io.Connection_Type:=Process_Io.Permanent)
                  renames     Pio_Enquire_Status_Package.Send;
  --------------------------------------------------------------------  
  package Pio_Status_Package is new Process_Io.Generic_Io
          (Identity        => Process_Io.Pipe.Pio_Status_Message,
           Data_Type       => Process_Io.Pipe.Pio_Status_Record,
           Data_Descriptor => (1 .. 7  => Process_Io.Integer_2_Type,
		                       8 .. 12 => Process_IO.Integer_4_Type));

  function Data  (Message   : Process_Io.Message_Type)
                  return      Process_Io.Pipe.Pio_Status_Record
                  renames     Pio_Status_Package.Data;

  procedure Send (Receiver  : Process_Io.Process_Type;
                  Data      : Process_Io.Pipe.Pio_Status_Record;
                  Connection: Process_Io.Connection_Type:=Process_Io.Permanent)
                  renames     Pio_Status_Package.Send;
-------------------------------------------------------------------------------
  package Pio_Queue_Package is new Process_Io.Generic_Io
          (Identity        => Process_Io.Pipe.Pio_Queue_Message,
           Data_Type       => Process_Io.Pipe.Pio_Queue_Record,
           Data_Descriptor => (1 .. 2 => Process_Io.String_Type(15),
		                       3      => Process_IO.Integer_4_Type));

  function Data  (Message   : Process_Io.Message_Type)
                  return      Process_Io.Pipe.Pio_Queue_Record
                  renames     Pio_Queue_Package.Data;

  procedure Send (Receiver  : Process_Io.Process_Type;
                  Data      : Process_Io.Pipe.Pio_Queue_Record;
                  Connection: Process_Io.Connection_Type:=Process_Io.Permanent)
                  renames     Pio_Queue_Package.Send;
  --------------------------------------------------------------------  

end Process_Io_Messages;
