--------------------------------------------------------------------------------
--
--	COPYRIGHT	SattControl AB, Malmo
--
--	FILE NAME	process_io_spec.a
--
--	RESPONSIBLE	Ingvar Hedgarde
--
--	DESCRIPTION	This files contains the package specification
--			of the package PROCESS_IO.
--			NOTE! This is the UNIX version designed to run on
--			AIX V3.2 and INTERACTIVE UNIX 386 V2.2.1.
--
--			The package contains procedures for sending and
--			receiving data messages between processes.
--			Messages are queued to the receiving process.
--
--			Implementation information:
--			On a UNIX system shared memory are used for data
--			transfer. A shared memory contains the queue of
--			messages to a process.
--
--------------------------------------------------------------------------------
--
--	VERSION		5.0
--	AUTHOR		Ingvar Hedg{rde
--	VERIFIED BY
--	DESCRIPTION	Original version
--
--------------------------------------------------------------------------------
--
--	VERSION		5.0a
--	AUTHOR		Henrik Dannberg	18-jul-1991
--	VERIFIED BY
--	DESCRIPTION	Parameter SOPS in POSIX.SEMBUF not a pointer any more
--
--	VERSION		5.0b
--	AUTHOR		Ingvar Hedgarde	29-oct-1991
--	VERIFIED BY
--	DESCRIPTION	Modified to correct process restart deadlock.
--
--	VERSION		5.0c
--	AUTHOR		Henrik Dannberg	27-JAN-1992
--	VERIFIED BY
--	DESCRIPTION	Package POSIX renamed to SVR3
--
--------------------------------------------------------------------------------
--
--	VERSION		5.1a
--	AUTHOR		Henrik Dannberg	6-AUG-1992
--	VERIFIED BY
--	DESCRIPTION	Representation clauses added to make the code more
--			portable.
--
--	VERSION		5.1b
--	AUTHOR		Ingvar Hedgarde 18-AUG-1992
--	VERIFIED BY
--	DESCRIPTION	Attach and detach error corrected.
--
--	VERSION		5.1c
--	AUTHOR		Ingvar Hedgarde 31-AUG-1992
--	VERIFIED BY
--	DESCRIPTION	A possibility of using polling in the receive call
--			has been added.
--
--	VERSION		5.2a
--	AUTHOR		Ingvar Hedgarde 03-NOV-1993
--	VERIFIED BY
--	DESCRIPTION	A modification made to get around internal compiler
--			bug in Verdix ADA 6.1.0i.
--
--	VERSION		6.0
--	AUTHOR		Ingvar Hedgarde 12-Sep-1994
--	VERIFIED BY
--	DESCRIPTION	Integer_2 => Integer_4
--
--------------------------------------------------------------------------------
--
--	VERSION		8.2
--	AUTHOR		Ann-Charlotte Andersson	 15-Apr-1999
--	VERIFIED BY
--	DESCRIPTION	a) Added pio routines
--
--------------------------------------------------------------------------------
--	VERSION		8.2b
--	AUTHOR		Irene Olsson		19-Apr-1999
--	VERIFIED BY
--	DESCRIPTION	Added procedure SHOW_ATTACHED_PROCESSES used in pio
--------------------------------------------------------------------------------
--	VERSION		8.2-1
--	AUTHOR		Irene Olsson
--	DATE		30-May-2000
--	VERIFIED BY
--	DESCRIPTION	Increased MAX_NUMBER_OF_MESSAGES from 10 to 30
--------------------------------------------------------------------------------
--      VERSION         9.2-0143
--      AUTHOR          SNE 16-May-2002
--      VERIFIED BY     ?
--      DESCRIPTION     Added function RECEIVER
--------------------------------------------------------------------------------
--9.6-10618
--  BNL 2006-11-30  Added SET_THIS_PROCESS
--  IEO 2007-07-11  Corrected SET_THIS_PROCESS
--9.6-10649 AXO/SNE       05-Dec-2006 Process_IO improvements (taken from process I/O v2.11 made by Alex van Oorschot)
--------------------------------------------------------------------------------
with SYSTEM;
with Ada.Finalization; use Ada.Finalization;

package PROCESS_IO is

-- The MAX_DATA_LENGTH is the maximum length in bytes for a message.
-- MAX_NUMBER_OF_MESSAGES is the number of messages of maximum length that
-- a queue can hold. The constants should be optimized for the application.
-- A queue can hold more number of messages than MAX_NUMBER_OF_MESSAGES as
-- long as the sum of their lengths are less or equal to the product of
-- MAX_DATA_LENGTH and MAX_NUMBER_OF_MESSAGES.
--
-- On a VMS system the size of a mailbox that is allocated to a process, is
-- the product of these two constants. A mailbox is allocated inside a process
-- when:
--       The PROCESS_IO package is included in the process with a with clause.
--       The process sends a message to another process that has not yet
--       allocated a mailbox.

  MAX_DATA_LENGTH        : constant := 1036;
  MAX_NUMBER_OF_MESSAGES : constant := 300;	-- v8.2-1

-- The PROCESS_TYPE should be used when defining all processes that use
-- the process communication in the package CORE_DEFINITION. The node component
-- is now used in this implementation of the PROCESS_IO.
-- The total range of IDENTITY_TYPE is for all processes. The numbers should
-- be grouped in series and declared in CORE_DEFINITION for each process that
-- define messsages.

  subtype NAME_TYPE is STRING (1..15);

  type PROCESS_TYPE is
    record
      NAME  : NAME_TYPE := (others => ' ');
      NODE  : NAME_TYPE := (others => ' ');
    end record;
  for PROCESS_TYPE'Alignment use 2; --9.4.1
  for PROCESS_TYPE use record					-- V5.1a
--9.4.1    record at mod 2;					-- V5.1a
      NAME at  0  range 0..15*8-1;			-- V5.1a
      NODE at  15 range 0..15*8-1;			-- V5.1a
    end record;						-- V5.1a

  for PROCESS_TYPE'SIZE use 30*8;				-- V5.1a

  type     IDENTITY_TYPE        is range -10..32000;
  for      IDENTITY_TYPE'SIZE   use 32;                           --V6.0

  type     MESSAGE_TYPE  is private;

  type CONNECTION_TYPE is (PERMANENT, TEMPORARY);

-- CONNECTION_TYPE (see comments for the SEND procedure).
-- The functions INTEGER_2_TYPE, INTEGER_4_TYPE, ENUMERATION_TYPE and
-- STRING_TYPE are used to describe the data types in a message. They are used
-- to form a DATA_DESCRIPTOR for input to the generic package GENERIC_IO.

  type COMPONENT is private;
  type COMPONENT_DESCRIPTOR is access COMPONENT;
  type DATA_DESCRIPTOR_TYPE is array (INTEGER range <>) of COMPONENT_DESCRIPTOR;
  overriding function "&" (LEFT,RIGHT : DATA_DESCRIPTOR_TYPE) return DATA_DESCRIPTOR_TYPE;

  function INTEGER_2_TYPE                  return COMPONENT_DESCRIPTOR;
  function INTEGER_4_TYPE                  return COMPONENT_DESCRIPTOR;
  function ENUMERATION_TYPE                return COMPONENT_DESCRIPTOR;
  function STRING_TYPE (LENGTH : POSITIVE) return COMPONENT_DESCRIPTOR;

-- The RECEIVE procedure is used to receive messages from other processes.
-- It reads the message which than can be decoded with the functions
-- IDENTITY, SENDER and DATA.
-- If there is no message to receive, the RECEIVE is waiting
-- for a message to come. A TIME_OUT time may be specified when calling
-- RECEIVE. The RECEIVE waits at least the specified TIME_OUT time. But it may
-- wait longer. Note: A call without a TIME_OUT time is normally more
-- efficient.

  procedure RECEIVE  (MESSAGE  : out MESSAGE_TYPE;
                      TIME_OUT : in  DURATION := 0.0);

  function  IDENTITY (MESSAGE  : MESSAGE_TYPE) return IDENTITY_TYPE;
  function  SENDER   (MESSAGE  : MESSAGE_TYPE) return PROCESS_TYPE;
  function  RECEIVER (MESSAGE  : MESSAGE_TYPE) return PROCESS_TYPE;  --v9.2-0143

  function  THIS_PROCESS return PROCESS_TYPE;
--9.6-10618
  procedure SET_THIS_PROCESS(PROCESS: in PROCESS_TYPE);

-- SIGNAL should be called when there is no data to be sent but one process
-- wants to notify another process.
-- The SEND procedure is used to send messages to another process.
-- A SEND does not wait for the receiver to receive the message. But if the
-- receivers queue is full, then SEND will wait for the neccesary space in
-- the queue to be free.
-- If CONNECTION is set to PERMANENT the SEND and SIGNAL routines will
-- internally allocate memory resources in your process (one internal channel
-- will be permanently allocated to the RECEIVER) in order to improve run time
-- performance. If CONNECTION is set to TEMPORARY the internal channel will
-- be deallocated after the data transfer has been performed. Therefore -
-- if you intend to send several data packages to the RECEIVER (during the
-- lifetime of your process) you should use a permanent connection.

  procedure SIGNAL (RECEIVER   : in  PROCESS_TYPE;
                    IDENTITY   : in  IDENTITY_TYPE;
                    CONNECTION : in  CONNECTION_TYPE := PERMANENT);

  procedure SEND   (RECEIVER   : in  PROCESS_TYPE;
                    MESSAGE    : in  MESSAGE_TYPE;
                    CONNECTION : in  CONNECTION_TYPE := PERMANENT);

--The following procedures are added to serve the pio display program
--v8.2 start

--  procedure SHOW_MESSAGE_QUEUE (PROCESS : PROCESS_TYPE);

--  procedure SHOW_PROCESS_LIST;

--  procedure SET_VALUE (S1 : STRING;
--                       S2 : STRING;
--                       V1 : STRING);
--v8.2 end

--  procedure SHOW_ATTACHED_PROCESSES (PROCESS : PROCESS_TYPE);  --8.2b

-- The package GENERIC_IO is instansiated with DATA, which is the message
-- record, the IDENTITY and a DESCRIPTOR, which describes the
-- types of the elements in the message record.
-- The DATA function is used to decode the message data.
-- The SEND procedure is used to send messages of DATA_TYPE.
-- The function UNPACK is equivalent to the function DATA, use UNPACK because
-- the function DATA will be removed from the PROCESS_IO package in the future.
-- The function PACK is to encode the message data to a message of
-- MESSAGE_TYPE which can be sent with the procedure SEND above.

  generic
    Identity        : Identity_Type;
    type Data_Type is private;
    Data_Descriptor : Data_Descriptor_Type;
	pragma Warnings(Off,DATA_DESCRIPTOR);
  package Generic_Io is

    function  Data (Message    :     Message_Type) return Data_Type;

    procedure Send (Receiver   : in  Process_Type;
                    Data       : in  Data_Type;
                    Connection : in  Connection_Type := Permanent);

    function  Unpack (Message  : in  Message_Type) return Data_Type;

    function  Pack (Data       : in  Data_Type) return Message_Type;

  end Generic_Io;


-- The following exceptions may be raised during elaboration.

  Illegal_Data_Descriptor : exception; -- The sum of the lengths of the
                                       -- components in the DATA_DESCRIPTOR
                                       -- for the message does not match the
                                       -- length of the object of the type
                                       -- DATA_TYPE

-- The following exeptions may be raised during execution.

  ILLEGAL_DATA_LENGTH     : exception; -- A message with illegal length has
                                       -- been received or the length of the
                                       -- data stored in the MESSSAGE parameter
                                       -- does not match the generic data type.

  ILLEGAL_IDENTITY        : exception; -- The identity is not in the allowed
                                       -- range or the identity of the data
                                       -- in MESSAGE does not match the identity
                                       -- of the generic data.

  TIMEOUT                 : exception; -- RECEIVE has not received a message
                                       -- within the specified TIME_OUT_TIME.

  SYSTEM_ERROR            : exception; -- This exception will be raised on
                                       -- system specific errors.


  function ERROR_MESSAGE return STRING;

-- The ERROR_MESSAGE function may be called after an exception has been raised.
-- The function will then return a system dependant error message text, which
-- will give more detailed information on the error cause than the exception
-- itself.

  --v9.6-10649 ---->
  function SET_DEFAULT_MAILBOX_NAME return BOOLEAN;

  -- Only for compability reason towards Win32.  Will always return true.


  function To_Process_Type(S : String ) return Process_Io.Process_Type ;


private

  type BYTE is range 0..255;
  for  BYTE'SIZE use 8;

  type BYTE_ARRAY is array (POSITIVE range <>) of BYTE;

  type WORD is range -32_768..32_767;
  for  WORD'SIZE use 16;



  subtype Byte_Array_4 is Byte_Array(1..4);
  subtype Payload_Array_Type is Byte_Array(1..Max_Data_Length);

  type Header_Type is record
      Endianess      : Byte         := 0;
      Message_Length : Natural      := 0;
      Identity       : Identity_Type  := 0;
      Sender         : Process_Type := ((others => ' '),(others => ' '));
      Receiver       : Process_Type := ((others => ' '),(others => ' '));
  end record;
  for Header_Type'Size use 72*8;
  for Header_Type use record
      Endianess      at   0 range 0..31;
      Message_Length at   4 range 0..31;
      Identity       at   8 range 0..31;
      Sender         at  12 range 0..8*30-1;
      Receiver       at  42 range 0..8*30-1;
  end record;

  type Payload_Type is record
      Data        : Payload_Array_Type := (others => 0);
      Length      : Natural            := 0;
  end record;
  for Payload_Type'Size use 1040*8;
  for Payload_Type use record
      Data      at    0 range 0..8*1036-1;
      Length    at 1036 range 0..31;
  end record;

  type Local_Message_Type is new Controlled with record
      Header  : Header_Type;
      Payload : Payload_Type;
  end record;
--  for Local_Message_Type'Size use  Header_Type'Size + Payload_Type'Size;
--  for Local_Message_Type'Size use 8*(72+1040);

--  overriding procedure Finalize (Msg : in out Local_Message_Type);
  overriding procedure Initialize (Msg : in out Local_Message_Type) ;

  procedure Write_To_Pipe(Msg : in out Local_Message_Type) ;
  procedure Check_Message_Record (Msg : in out Local_Message_Type) ;
  procedure Set_Identity(Msg : in out Local_Message_Type; Identity : Identity_Type);
  function  Get_Identity(Msg : Local_Message_Type) return  Identity_Type;
  procedure Set_Receiver(Msg : in out Local_Message_Type; Receiver : Process_Type);
  function  Get_Receiver(Msg : Local_Message_Type) return  Process_Type;
  procedure Set_Sender(Msg : in out Local_Message_Type; Sender : Process_Type);
  procedure Set_Message(Msg : in out Local_Message_Type; Message : Message_Type);
  function  Get_Payload_length(Msg : Local_Message_Type) return Natural;
  procedure Print_Header(Msg : Local_Message_Type; What : in String);

  type Message_Type is record
     Msg : Local_Message_Type;
  end record;

  Message_Length : constant Natural := Local_Message_Type'Size/System.Storage_Unit;

  type Component_Type is (An_Integer_2, An_Integer_4, A_String, Enumeration);

  type Component is
    record
      Component   : Component_Type;
      Length      : Positive;
    end record;

end PROCESS_IO;

