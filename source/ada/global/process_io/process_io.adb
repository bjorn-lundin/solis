--------------------------------------------------------------------------------
--	COPYRIGHT	Consafe Logistics AB, Lund
--	FILE NAME	process_io.adb
--	RESPONSIBLE	Björn Lundin
--	DESCRIPTION	This files contains the package body
--			of the package PROCESS_IO.
--------------------------------------------------------------------------------
--  Total rewrite, using pipes - same body for all os:es, different child body
--------------------------------------------------------------------------------
with Ada.Characters.Handling;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Text_Io;
pragma Warnings(Off,Text_Io);
with Posix;
--with System;
with Unchecked_Conversion;
with Process_Io.Pipe;
pragma Elaborate_All(Posix);
with Ada.Environment_Variables;
with Binary_Semaphores;

package body Process_Io is

  Global_Process_Name_Is_Set : Boolean := False;

  Sem : Binary_Semaphores.Semaphore_Type;

  My_Process        : Process_Type    := ((others => ' '), (others => ' '));
  My_Own_Read_Pipe  : Process_io.Pipe.Pipe_Type; -- This is my mailbox
  --My_Own_Write_Pipe : Process_io.Pipe.Pipe_Type; -- This is also my mailbox, but
  -- never used. It is opened so we do not get EOF when other processes closes my pipe

  Time_Out_Identity        : constant Identity_Type := -1;
--  Enquiry_Identity         : constant Identity_Type := -2;
--  START_LOG_FILE_IDENTITY  : constant WORD := -2;
--  STOP_LOG_FILE_IDENTITY   : constant WORD := -3;
--  START_LOG_IDENTITY       : constant WORD := -4;
--  STOP_LOG_IDENTITY        : constant WORD := -5;
--  LIST_IDENTITY            : constant WORD := -6;
--  LOG_LIST_IDENTITY        : constant WORD := -7;
--  LOG_MESSAGE_IDENTITY     : constant Identity_Type := -8;
--  LOG_HEAD_MESSAGE_IDENTITY: constant WORD := -9;
--  INT2                     : constant BYTE := 2;
--  INT2_ARRAY               : constant BYTE := 3;
--  INT4                     : constant BYTE := 4;
--  INT4_ARRAY               : constant BYTE := 5;
--  ENUM                     : constant BYTE := 6;
--  ENUM_ARRAY               : constant BYTE := 7;
--  STRI                     : constant BYTE := 8;

  Receiving_Mailbox_Full : exception ;
  Wait_For_Mailbox_Time  : constant Duration := 0.05;

--  procedure Here(Num : Positive) is
--  begin
--    Process_io.Pipe.Log("Here:" & Num'Img);
--  end Here;
  ---------------------------------------------------------------------

--  overriding procedure Finalize (Msg : in out Local_Message_Type);
  ---------------------------------------------------------------------
  overriding procedure Initialize (Msg : in out Local_Message_Type) is
    use system;
  begin
--   type Bit_Order is (High_Order_First, Low_Order_First);
    case Default_Bit_Order is
      when High_Order_First => Msg.Header.Endianess := 0;
      when Low_Order_First  => Msg.Header.Endianess := 1;
    end case;
    Msg.Header.Message_Length := Message_Length; -- constant in private part of spec
--    text_io.put_line("Initialize - Message_Length:" & Msg.Header.Message_Length'img);
  end Initialize;
  ---------------------------------------------------------------------
  procedure Write_To_Pipe(Msg : in out Local_Message_Type) is
    Receivers_Pipe : Process_io.Pipe.Pipe_Type ;
--    Receivers_Pipe_Ro : Process_io.Pipe.Pipe_Type ; -- make sure we do not hang on write b/c no reader

    Dummy : Message_Type;
  begin
  	Sem.Seize;
    Process_io.Pipe.Log("Write_To_Pipe - Start");
    Receivers_Pipe := Process_io.Pipe.Create(Msg.Get_Receiver);
    Process_io.Pipe.Log("Write_To_Pipe - writing to " & To_String(Receivers_Pipe.Name));
    Process_io.Pipe.Log("Write_To_Pipe - identity " & Msg.Header.Identity'Img);
    if Receivers_Pipe.Exists then
--      Process_io.Pipe.Log("Write_To_Pipe - Before Open");
      Receivers_Pipe.Open(Process_io.Pipe.O_RDWR);
      Dummy.Msg := Msg;
--      Process_io.Pipe.Log("Write_To_Pipe - Before write");
      Receivers_Pipe.Write(Dummy);
--      Process_io.Pipe.Log("Write_To_Pipe - Before close");
      Receivers_Pipe.Close;
	  case Msg.Get_Identity is
	    when Time_Out_Identity                  |
		     Process_io.Pipe.Pio_Status_Message |
		     Process_io.Pipe.Pio_Queue_Message  => null;
        when others                             =>  My_Own_Read_Pipe.Increase_Sent;	-- for pio stats
	  end case;
	else
      My_Own_Read_Pipe.Increase_Failed_Sent;	-- for pio stats
    end if;
    Process_io.Pipe.Log("Write_To_Pipe - Done");
	Sem.Release;
  end Write_To_Pipe;
  ---------------------------------------------------------------------
  procedure Check_Message_Record(Msg : in out Local_Message_Type) is
  begin
   null;
  end Check_Message_Record;
  ---------------------------------------------------------------------
  procedure Set_Identity(Msg : in out Local_Message_Type; Identity : Identity_Type) is
  begin
    Msg.Header.Identity := Identity;
--    Process_io.Pipe.Log("Set_Identity -" & Identity'Img);
  end  Set_Identity;
  ---------------------------------------------------------------------
  procedure Set_Receiver(Msg : in out Local_Message_Type; Receiver : Process_Type) is
  begin
    Msg.Header.Receiver := Receiver;
--    Process_io.Pipe.Log("Set_Receiver - " & Receiver.Name);
  end Set_Receiver;
  ---------------------------------------------------------------------
  function Get_Receiver(Msg : Local_Message_Type) return  Process_Type is
  begin
--    Process_io.Pipe.Log("get_Receiver - " & Msg.Header.Receiver.Name);
    return Msg.Header.Receiver;
  end Get_Receiver;
  ---------------------------------------------------------------------
  procedure Set_Sender(Msg : in out Local_Message_Type; Sender : Process_Type) is
  begin
    Msg.Header.Sender := Sender;
  end Set_Sender;
  ---------------------------------------------------------------------
  procedure Set_Message(Msg : in out Local_Message_Type; Message : Message_Type) is
  begin
    null;
  end Set_Message;
  ---------------------------------------------------------------------
  function Get_Identity(Msg : Local_Message_Type) return  Identity_Type is
  begin
 --   Process_io.Pipe.Log("get_Receiver -" & Msg.Header.Identity'Img);
    return Msg.Header.Identity;
  end Get_Identity;
  ---------------------------------------------------------------------
  function Get_Payload_length(Msg : Local_Message_Type) return NAtural is
  begin
 --   Process_io.Pipe.Log("get_Receiver -" & Msg.Payload.Length'Img);
    return Msg.Payload.Length;
  end Get_Payload_length ;
  ---------------------------------------------------------------------
  procedure Print_Header(Msg : Local_Message_Type; What : in String) is
    use Text_Io;
  begin
--    put_line("Endianess     :" & Msg.Header.Endianess'img);
--    put_line("Message_Length:" & Msg.Header.Message_Length'img);
    Process_io.Pipe.Log("Identity " & What & Msg.Header.Identity'Img);
    Process_io.Pipe.Log("Sender        : '" & Msg.Header.Sender.Name & "'.'" & Msg.Header.Sender.Node & "'");
    Process_io.Pipe.Log("Receiver      : '" & Msg.Header.Receiver.Name & "'.'" & Msg.Header.Receiver.Node & "'");
  end Print_Header;
  ---------------------------------------------------------------------
  function Get_Process return Process_Type is
    Process : Process_Type;
    use Ada.Environment_Variables;
  begin
    if Exists("BOT_NAME") then
      Move(Ada.Characters.Handling.To_Lower(Value("BOT_NAME")),Process.Name,Drop => Right);
    else
      Move(Trim(Posix.Pid_T'Image(Posix.Getpid),Both),Process.Name,Drop => Right);
    end if;

    if Exists("BOT_NODE") then
      Move(Ada.Characters.Handling.To_Lower(Value("BOT_NODE")),Process.Node,Drop => Right);
    end if;
    return Process;
  end Get_Process;
  ---------------------------------------------------
--  procedure Check_For_Processes_To_Remove is
--  begin
--    null;
--  end Check_For_Processes_To_Remove;
  ----------------------------------------------------------
  overriding function "&" (Left,Right: Data_Descriptor_Type) return Data_Descriptor_Type is
    Result: Data_Descriptor_Type(1..Left'Length+Right'Length);
  begin
    Result(1..Left'Length)             := Left;
    Result(Left'Length+1..Result'Last) := Right;
    return Result;
  end "&";
  ----------------------------------------------------------
  function Integer_2_Type return Component_Descriptor is
  begin
    return new Component'(An_Integer_2,2);
  end Integer_2_Type;
  ----------------------------------------------------------
  function Integer_4_Type return Component_Descriptor is
  begin
    return new Component'(An_Integer_4,4);
  end Integer_4_Type;
  ----------------------------------------------------------
  function String_Type (Length : Positive) return Component_Descriptor is
  begin
    return new Component'(A_String,Length);
  end String_Type;
  ----------------------------------------------------------
  function Enumeration_Type return Component_Descriptor is
  begin
    return new Component'(Enumeration,1);
  end Enumeration_Type;
  ----------------------------------------------------------
  function This_Process return Process_Type is
  begin
    if not Global_Process_Name_Is_Set then
      My_Process := Get_Process;
      Global_Process_Name_Is_Set := True;
    end if;
    return My_Process;
  end This_Process;
  ----------------------------------------------------------
  procedure Check_Message_Record (Data_Descriptor : Data_Descriptor_Type;
                                  Data_Length     : Natural;
                                  Identity        : Identity_Type) is

   begin
    null;
  end Check_Message_Record;
  ----------------------------------------------------------
   task type Timer_Task is
     entry Start_Timer (Time : in  Duration);
     entry Stop_Timer;
   end Timer_Task;
  --------------------------------------------------------
  task body Timer_Task is
    Time_Out_Time : Duration;
    Timeout_Message : Local_Message_Type;
  begin
    accept Start_Timer (Time : in Duration) do
      Time_Out_Time := Time;
      Timeout_Message.Header.Identity := Time_Out_Identity;
      Timeout_Message.Set_Receiver(This_Process);
      Timeout_Message.Set_Sender(This_Process);
    end Start_Timer;
    select
      accept Stop_Timer do
        null;
      end Stop_Timer;
    or
      delay Time_Out_Time;
      loop
        begin
          Process_io.Pipe.Log("Timer_Task - start write timeout to " & Timeout_Message.Get_Receiver.Name);
          Process_io.Pipe.Log("Timer_Task - identity " & Timeout_Message.Header.Identity'Img);
          Timeout_Message.Write_To_Pipe ;
          Process_io.Pipe.Log("Timer_Task - stop write");
          exit;
        exception
          when Receiving_Mailbox_Full => delay Wait_For_Mailbox_Time;
        end;
      end loop;
      accept Stop_Timer do
        null;
      end Stop_Timer;
    end select;
  end Timer_Task;
  -----------------------------------------------------------
  procedure Receive (Message          : out Message_Type;
                     Time_Out         : in  Duration := 0.0) is
  begin
    Process_io.Pipe.Log("Receive - start",0 );
    Process_io.Pipe.Increase_Indent(2);
    loop
      Sem.Seize;
      if not My_Own_Read_Pipe.Is_Initialized then
	    My_Own_Read_Pipe.Do_Initialize;
      end if;
      Sem.Release;
      if Time_Out > 0.0 then
          declare
            Timer  : Timer_Task;
          begin
            Timer.Start_Timer (Time_Out);
            My_Own_Read_Pipe.Read(Message);
            begin
              Timer.Stop_Timer;
            exception
              when Tasking_Error => null;
            end ;
            loop
              exit when Timer'Terminated;
              delay 0.0_001;
            end loop;
          end;
      else
            My_Own_Read_Pipe.Read(Message);
      end if;

      case Identity(Message) is
          when Time_Out_Identity =>
              My_Own_Read_Pipe.Increase_Timeout;
              raise Timeout;
          when Process_Io.Pipe.Pio_Enquire_Status_Message =>
            My_Own_Read_Pipe.Send_Statistics(Message);
          when others            =>
              My_Own_Read_Pipe.Increase_Received;
			  exit;
      end case;
    end loop;
    Process_io.Pipe.Increase_Indent(-2);
    Process_io.Pipe.Log("Receive - stop",0);
  end Receive;

  ----------------------------------------------------------
  function Identity(Message : Message_Type) return Identity_Type is
  begin
    return Message.Msg.Header.Identity;
  end Identity;
  ----------------------------------------------------------
  function Sender(Message : Message_Type) return Process_Type is
  begin
    return Message.Msg.Header.Sender;
  end Sender;
  ----------------------------------------------------------
  --v9.2-0143
  function Receiver(Message: Message_Type) return Process_Type is
  begin
    return Message.Msg.Header.Receiver;
  end Receiver;
  ----------------------------------------------------------
  procedure Signal(Receiver     : in  Process_Type;
                   Identity     : in  Identity_Type;
                   Connection   : in  Connection_Type := Permanent) is
    pragma Unreferenced(Connection);
    Msg : Local_message_type;
  begin
    Msg.Set_Receiver(Receiver);
    Msg.Set_Identity(Identity);
    Msg.Set_Sender(This_Process);
    loop
      begin
        Msg.Write_To_Pipe;
        exit;
      exception
        when Receiving_Mailbox_Full => delay Wait_For_Mailbox_Time;
      end;
    end loop;
  end Signal;
  ----------------------------------------------------------
  procedure Send   (Receiver   : in  Process_Type;
                    Message    : in  Message_Type;
                    Connection : in  Connection_Type := Permanent) is
    pragma Unreferenced(Connection);
    Msg : Local_Message_Type := Message.Msg;
  begin
	Process_io.Pipe.Increase_Indent(2);
    Sem.Seize;
    if not My_Own_Read_Pipe.Is_Initialized then
       My_Own_Read_Pipe.Do_Initialize;
    end if;
    Sem.Release;
    Msg.Set_Receiver(Receiver);
    Msg.Set_Sender(This_Process);
    loop
      begin
        Msg.Write_To_Pipe;
        exit;
      exception
        when Receiving_Mailbox_Full => delay Wait_For_Mailbox_Time;
      end;
    end loop;
	Process_io.Pipe.Increase_Indent(-2);
  end Send;
  ----------------------------------------------------------
  package body Generic_Io is
    Data_Length    : constant Natural := Data_Type'Size/System.Storage_Unit;
    subtype Byte_Array_Message is Byte_Array (1..Data_Length);
    function Data_To_Byte_Array is new
             Unchecked_Conversion(Data_Type, Byte_Array_Message);

    function Convert (D : Data_Type) return Byte_Array_Message is
      BAM : Byte_Array_Message;
       --gnat-018 A silly work-around, just to save the
       -- result in a variable!
    begin
      BAM := Data_To_Byte_Array(D);
      return BAM;
    end Convert;
    ----------------------------------------------------------
    procedure Send (Receiver        : in  Process_Type;
                    Data            : in  Data_Type;
                    Connection      : in  Connection_Type := Permanent) is
      pragma Unreferenced(Connection);
      Msg : Local_Message_Type;
    begin
      Process_io.Pipe.Increase_Indent(2);
      Sem.Seize;
      if not My_Own_Read_Pipe.Is_Initialized then
	    My_Own_Read_Pipe.Do_Initialize;
      end if;
      Sem.Release;
      Process_io.Pipe.Log("Generice Send - start identity:" & Identity'Img);
      Msg.Set_Receiver(Receiver) ;
      Msg.Set_Sender(This_Process);
      Msg.Set_Identity(Identity);
      Msg.Payload.Data(1..Data_Length) := Convert(Data);
      Msg.Payload.Length := Data_Length;
      Process_io.Pipe.Log("Generice Send - start Data_Length:" & Data_Length'Img);
      loop
        begin
          Msg.Write_To_Pipe;
          exit;
        exception
          when Receiving_Mailbox_Full => delay Wait_For_Mailbox_Time;
          Process_io.Pipe.Log("Generice Send - Wait_For_Mailbox");
        end;
      end loop;
      Process_io.Pipe.Log("Generice Send - stop");
	Process_io.Pipe.Increase_Indent(-2);
    end Send;
    ----------------------------------------------------------
    function  Data (Message : Message_Type) return Data_Type is
      function Message_To_Data is new Unchecked_Conversion(Byte_Array_Message, Data_Type);
      Data  : Data_Type;
    begin
      Process_io.Pipe.Increase_Indent(2);
      Message.Msg.Print_Header("Data");
      if Message.Msg.Get_Payload_Length /= Data_Length  then
        raise Illegal_Data_Length with " Message record length = " &
                            Natural'Image (Data_Length) &
                          " Received message length = " &
                            Natural'Image (Message.Msg.Get_Payload_Length);
      elsif Message.Msg.Get_Identity /= Identity then
        raise Illegal_Identity with " Expected identity =" & Identity_Type'image(Identity) &
                          " Received identity = " &  Identity_Type'image(Message.Msg.Get_Identity);
      end if;
      Data := Message_To_Data (Message.Msg.Payload.Data (1..Data_Length));
      Process_io.Pipe.Increase_Indent(-2);
      return Data;
    end Data;
    ----------------------------------------------------------
    function  Unpack (Message : in  Message_Type) return Data_Type is
    begin
      return Data(Message);
    end Unpack;
    ----------------------------------------------------------
    function  Pack (Data : in  Data_Type) return Message_Type is
      Message : Message_Type;
    begin
      Process_io.Pipe.Increase_Indent(2);
      Message.Msg.Set_Identity(Identity);
      Message.Msg.Payload.Data := (others => 0);
      Message.Msg.Payload.Data (1..Integer(Data_Length)) := Data_To_Byte_Array (Data);
      Message.Msg.Payload.Length := Data_Length;
      Message.Msg.Print_Header("Pack");
      Process_io.Pipe.Increase_Indent(-2);
      return Message;
    end Pack;
    ----------------------------------------------------------
  begin
    Check_Message_Record (Data_Descriptor, Data_Length, Identity);
  end Generic_Io;
  ----------------------------------------------------------
  --v9.6-10649 ---->
  function Set_Default_Mailbox_Name return Boolean is
  begin
    return True;
  end Set_Default_Mailbox_Name;
  --v9.6-10649 <----
  ----------------------------------------------------------
  --9.6-10618
  procedure Set_This_Process(Process: in Process_Type) is
  begin
    My_Process.Name := Process.Name;
    My_Process.Node := Process.Node;
  end Set_This_Process;
  ----------------------------------------------------------
  function ERROR_MESSAGE return STRING is
  begin
    return "pio.Errror_Message is NOT implemented :-(";
  end Error_Message;
  pragma obsolescent(Error_Message,"Print error when occurs instead, not later");
  ----------------------------------------------------------

  --function To_Pio_Name(S : String ) return Process_Io.Name_Type is
  --  P :  Process_Io.Name_Type := (others => ' ');
  --begin
  --  Move(S,P);
  --  return P;
  --end To_Pio_Name;
  ------------------------------------------------------------


  function To_Process_Type(S : String ) return Process_Io.Process_Type is
    P :  Process_Io.Process_Type := ((others => ' '), (others => ' '));
  begin
    Move(S,P.Name);
    return P;
  end To_Process_Type;
  ----------------------------------------------------------



--begin
--  My_Process := Get_Process;
end Process_Io;
