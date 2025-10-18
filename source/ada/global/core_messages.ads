--------------------------------------------------------------------------------


with Types;  use Types;
with Process_Io;
--with Calendar2;  -- v9.6

pragma Elaborate_All(Process_Io);--9.3-0028

package Core_Messages is

-- Definition of message identities

  subtype Core_Messages is Process_Io.Identity_Type range 1000..1099;

-- The EXIT message is used only to perform a controlled exit of a process.
  Exit_Message                    : constant Core_Messages := 1091;

-- The ENTER_CONSOLE_MODE_MESSAGE is used to connect a process to a console
  Enter_Console_Mode_Message      : constant Core_Messages := 1092;

-- The READ_CONFIG_MESSAGE is used to re-read config, if any
  Read_Config_Message             : constant Core_Messages := 1093;

  -----------------------------------------
  type Exit_Record is record
      Dummy : Integer_4 := 0;
  end record;
  for Exit_Record'alignment use 4;
  for Exit_Record use record
      Dummy at 0 range 0..8*4-1;					-- V6.5b
  end record;								-- V6.5b
  for Exit_Record'Size use 8*4;				-- V6.5b
  -----------------------------------------
  type Enter_Console_Mode_Record is record
      Device : String(1..80);
  end record;
  for Enter_Console_Mode_Record'alignment use 4;
  for Enter_Console_Mode_Record use record
      Device at 0 range 0..8*80-1;					-- V6.5b
  end record;								-- V6.5b
  for Enter_Console_Mode_Record'Size use 8*80;				-- V6.5b
  -----------------------------------------
  type Read_Config_Record is record
      Dummy : Integer_4 := 0;
  end record;
  for Read_Config_Record'alignment use 4;
  for Read_Config_Record use record
      Dummy at 0 range 0..8*4-1;					-- V6.5b
  end record;								-- V6.5b
  for Read_Config_Record'Size use 8*4;				-- V6.5b

--------------------------------------------------------------------------------
package Exit_Package is new Process_Io.Generic_Io
        (Identity        => Exit_Message,
         Data_Type       => Exit_Record,
         Data_Descriptor => (1 => Process_Io.Integer_4_Type));
--
function  Data   (Message: Process_Io.Message_Type)
          return  Exit_Record
          renames Exit_Package.Data;
--
procedure Send   (Receiver  : Process_Io.Process_Type;
                  Data      : Exit_Record;
                  Connection: Process_Io.Connection_Type:=Process_Io.Permanent)
          renames Exit_Package.Send;

--------------------------------------------------------------------------------

package Enter_Console_Mode_Package is new Process_Io.Generic_Io
        (Identity        => Enter_Console_Mode_Message,
         Data_Type       => Enter_Console_Mode_Record,
         Data_Descriptor => (1 => Process_Io.String_Type(80)));
--
function  Data   (Message: Process_Io.Message_Type)
          return  Enter_Console_Mode_Record
          renames Enter_Console_Mode_Package.Data;
--
procedure Send   (Receiver  : Process_Io.Process_Type;
                  Data      : Enter_Console_Mode_Record;
                  Connection: Process_Io.Connection_Type:=Process_Io.Permanent)
          renames Enter_Console_Mode_Package.Send;

--------------------------------------------------------------------------------
package Read_Config_Package is new Process_Io.Generic_Io
        (Identity        => Read_Config_Message,
         Data_Type       => Read_Config_Record,
         Data_Descriptor => (1 => Process_Io.Integer_4_Type));
--
function  Data   (Message: Process_Io.Message_Type)
          return  Read_Config_Record
          renames Read_Config_Package.Data;
--
procedure Send   (Receiver  : Process_Io.Process_Type;
                  Data      : Read_Config_Record;
                  Connection: Process_Io.Connection_Type:=Process_Io.Permanent)
          renames Read_Config_Package.Send;

--------------------------------------------------------------------------------

end Core_Messages;
