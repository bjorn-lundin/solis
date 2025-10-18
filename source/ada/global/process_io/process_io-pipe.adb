--------------------------------------------------------------------------------
--    COPYRIGHT    Consafe Logistics AB, Lund
--    RESPONSIBLE    Björn Lundin
--    DESCRIPTION    This files contains the pipe object
--------------------------------------------------------------------------------
with Ada.Strings;        use Ada.Strings;
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Directories;
with Posix;
--with System_Services;
with Text_io; use Text_io;
--with Types; use Types;
--with Calendar2;

with Ada.Environment_Variables;

pragma Warnings(Off);
-- This is probably platform specific.... AIX seems to be ok without...
-- We REALLY want these, or the pipe stuff does not work...
-- We get a SIGPIPE if we write to a pipe with no reader,
-- and that, we do not want...
with Ada.Interrupts; use Ada.Interrupts;
with Ada.Interrupts.Names;
pragma Warnings(On);

--pragma Elaborate_All(System_Services);
with Process_Io_Messages;

package body Process_Io.Pipe is
  package EV renames Ada.Environment_Variables;
--  Tmp : Integer := 0;
  Global_Logging : Boolean := False;
  Global_Indent : Integer := 0;
--  protected SignalHandler is
--    procedure Sigpipe_Handler;
--    pragma Unreserve_All_Interrupts;
--    pragma Attach_Handler(Sigpipe_Handler, Names.SIGPIPE );
--    -- If we write to a pipe with noone on the other side,
--    -- we get a SIGPIPE, and normally, that will terminate the program.
--    -- We do NOT want that...
--  end SignalHandler;
--  -- The Signal Handler

--  protected body SignalHandler is
--    procedure Sigpipe_Handler is
--    begin
--      Tmp := Tmp+1;
--    end Sigpipe_Handler;
--  end SignalHandler;

  Global_Pipe_Directory : String := EV.Value("BOT_HOME") & "/pipes/";

  type Mode_T is new Integer;
  type Size_T is new Long_Integer;
  subtype Ssize_T is Size_T;


  --------------------------------------------------------
  function O_RDONLY return Interfaces.C.Int is
  begin
    return Interfaces.C.Int(Posix.O_RDONLY);
  end O_RDONLY;

  --------------------------------------------------------
  function O_WRONLY return Interfaces.C.Int is
  begin
    return Interfaces.C.Int(Posix.O_WRONLY);
  end O_WRONLY;

  --------------------------------------------------------
  function O_RDWR return Interfaces.C.Int is
  begin
    return Interfaces.C.Int(Posix.O_RDWR);
  end O_RDWR;

  --------------------------------------------------------
  procedure Increase_Indent(Delta_Indent : Integer) is
  begin
      Global_Indent := Global_Indent + Delta_Indent;
  end Increase_Indent;
  --------------------------------------------------------
  procedure Log(What : String; Indent : Integer := -1) is
  begin
    if Indent > -1 then
      Global_Indent := Indent;
    end if;
    if Global_Logging then
      if Global_Indent > 0 then
        declare
          I_S : String(1..Global_Indent) := (others => ' ');
        begin
          Text_Io.Put_Line(I_S & Calendar2.String_Time(Milliseconds => True) & "-" & What);
        end;
      else
        Text_Io.Put_Line(Calendar2.String_Time(Milliseconds => True) & "-" & What);
      end if;
    end if;
  end Log;
  --------------------------------------------------------
  package C renames Interfaces.C;


  --------------------------------------------------------
  procedure Perror (Msg : String ) is
    subtype Msg_String is String(1 .. Msg'length +1);
    procedure cPerror( Message : access Msg_String);
    pragma Import( C, Cperror, "perror" );
    My_Msg : aliased Msg_String := Msg & Ascii.NUL;
  begin
    cPerror(My_Msg'access);
  end Perror;
  ------------------------------------------------------
  function Create(Receiver : Process_Type) return Pipe_Type is
    use Ada.Characters.Handling;
    use Ada.Directories;
    aPipe : Pipe_Type;
  begin
    if not Exists(Global_Pipe_Directory) then
      Create_Path(Global_Pipe_Directory);
    end if;
    aPipe.Name := To_Unbounded_String(Trim(To_Lower(Receiver.Name),Both));
    aPipe.Device := To_Unbounded_String(Global_Pipe_Directory) & aPipe.Name ;
    aPipe.Device_Length := To_String(aPipe.Device)'Length;
    aPipe.C_Device := To_Unbounded_String(To_String(aPipe.Device) & Ascii.Nul);
    aPipe.C_Device_Length := To_String(aPipe.C_Device)'Length;
    aPipe.Status.Created  := Calendar2.Clock;
    aPipe.Is_Initialized  := True;
    return aPipe;
  end Create;
  ------------------------------------------------------
  function  Exists(aPipe : Pipe_Type) return Boolean is
    use Ada.Directories;
    Is_Existing : Boolean;
  begin
    Is_Existing := Exists(To_String(aPipe.Device));
    return Is_Existing;
  end Exists;
  ------------------------------------------------------
  procedure Create_Pipe_On_Disk(aPipe : in out Pipe_Type) is
   -- int mkfifo(const char *pathname, mode_t mode);
    subtype Path_Name_Type is String(1..aPipe.C_Device_Length);
    My_Path : aliased Path_Name_Type := To_String(aPipe.C_Device);
    function Mkfifo(Path : access Path_Name_Type; Permission : Mode_t) return C.Int;
    pragma   Import(C, mkfifo, "mkfifo");
    use type C.Int;
    Result : C.Int := -1;
  begin
    Result := Mkfifo(My_Path'access, 8#660# );
    if Result < 0 then
      Perror("Mkfifo:");
    end if;
  end Create_Pipe_On_Disk;
  -------------------------------------------------
  procedure Remove_Pipe_From_Disk(aPipe : in out Pipe_Type) is
  -- int unlink(const char *pathname);
    subtype Path_Name_Type is String(1..aPipe.C_Device_Length);
    My_Path : aliased Path_Name_Type := To_String(aPipe.C_Device);
    function Unlink(Path : access Path_Name_Type) return C.Int;
    pragma   Import(C, Unlink, "unlink");
    use type C.Int;
    Result : C.Int := -1;
  begin
    Result := Unlink(My_Path'access);
    if Result < 0 then
      Perror("Unlink:");
    end if;
  end Remove_Pipe_From_Disk;

  ------------------------------------------------------
  procedure Open(aPipe : in out Pipe_Type; Flags : Interfaces.C.Int) is
    subtype Path_Name_Type is String(1..aPipe.C_Device_Length);
    My_Path : aliased Path_Name_Type := To_String(aPipe.C_Device);
    function cOpen(Path  : access Path_Name_Type;
                   Flags : Interfaces.C.Int;
                   Mode  : Mode_T ) return File_Id;
    pragma Import(C, cOpen, "open" );
  begin
    aPipe.Id := cOpen(My_Path'access,Flags, 8#660#);
    if aPipe.Id < 0 then
      Perror("'" & My_Path & "'");
    end if;
  end Open;
  ------------------------------------------------------
  procedure Close(aPipe : in out Pipe_Type) is
    function cClose( File : File_Id ) return C.Int;
    pragma import( C, cClose ,"close" );
    use type C.Int;
    Result : C.int := -1;
  begin
    Result := cClose(aPipe.Id);
    if Result < 0 then
      Perror("Close:");
    end if;
  end Close;
  ------------------------------------------------------
  procedure Read(aPipe : in out Pipe_Type; Msg : in out Message_Type) is
  -- ssize_t read(int fd, void *buf, size_t count);
  -- Read bytes from the specified file ,fd, into a buffer.
  -- BUF is any type of destination for the bytes read,
  -- with COUNT being the size of the buffer in bytes.
  -- The number of bytes read is returned, or -1 on an error.
    --use type C.Int;
    type LM_Type is record -- this type is here only because tagged types maps bad to C
      Header  : Header_Type;
      Payload : Payload_Type;
    end record;

    Message_Buffer : aliased LM_Type;
    function cRead(Fd    : File_Id;
                   Buf   : access LM_Type;
                   Count : Size_T ) return Ssize_T;
    pragma import( C, cRead , "read");
    Num_Bytes : Ssize_T := -1;
  begin
    loop
      Num_Bytes := cRead(aPipe.Id, Message_Buffer'access, Size_T(Message_Length));
      if Num_Bytes < 0 then
        Perror("Read:");
      end if;
      exit when Num_Bytes > 0;
      Log("read-Got EOF, reread" );
    end loop;

    Msg.Msg.Header  :=  Message_Buffer.Header;
    Msg.Msg.Payload :=  Message_Buffer.Payload;
--    Log("Read-Identity" & Msg.Msg.Header.Identity'img);
--    Log("Read-Sender '" & Msg.Msg.Header.Sender.Name & "'.'" & Msg.Msg.Header.Sender.Node & "'");
--    Log("Read-Receiver '" & Msg.Msg.Header.Receiver.Name & "'.'" & Msg.Msg.Header.Receiver.Node & "'");
  end Read;
  ------------------------------------------------------
  procedure Write(aPipe : in out Pipe_Type; Msg : in Message_Type) is
    --ssize_t write(int fd, const void *buf, size_t count);
    --write() writes up to count bytes to the file
    --referenced by the file descriptor fd from the
    --buffer starting at buf. POSIX requires that a read()
    --which can be proved to occur after a write()
    --has returned returns the new data.
    --Note that not all file systems are POSIX conforming.

    -- On success, the number of bytes written are returned
    --(zero indicates nothing was written).
    -- On error, -1 is returned, and errno is set appropriately.

    type LM_Type is record
      Header  : Header_Type;
      Payload : Payload_Type;
    end record;
    Message_Buffer : aliased LM_Type;
    function cWrite( Fd    : File_Id;
                     Buf   : access LM_Type;
                     Count : Size_T ) return Ssize_T;
    pragma import( C, cWrite , "write");
    Num_Bytes : Ssize_T;
  begin
    Message_Buffer.Header  := Msg.Msg.Header;
    Message_Buffer.Payload := Msg.Msg.Payload;
--    Log("Write-Identity" & Message_Buffer.Header.Identity'img);
--    Log("Write-Sender '" & Message_Buffer.Header.Sender.Name & "'.'" & Message_Buffer.Header.Sender.Node & "'");
--    Log("Write-Receiver '" & Message_Buffer.Header.Receiver.Name & "'.'" & Message_Buffer.Header.Receiver.Node & "'");

    Num_Bytes := cWrite(aPipe.Id, Message_Buffer'access, Size_T(Message_Length));
    if Num_Bytes < 0 then
      Perror("Write:");
    end if;
  end Write;
  ------------------------------------------------------
  overriding procedure Finalize (aPipe : in out Pipe_Type) is
  begin
    if aPipe.Delete_On_Exit then
      Log("Finalize - removing pipe" );
      aPipe.Remove_Pipe_From_Disk;
    end if;
  end Finalize;
  ------------------------------------------------------

  procedure Fill_Pipe_List(Pipe_List : in out Msg_List_Pack.List) is
    use Ada.Directories;
    Dir_Ent     : Directory_Entry_Type;
    The_Search  : Search_Type with Warnings => Off;
    Pipe_Data : Process_Io.Process_Type := ((others => ' '),(others => ' '));
  begin
      -- Search the directory in config file for table_*.xml
    Start_Search(Search    => The_Search,
                 Directory => Global_Pipe_Directory,
                 Pattern   => "",
                 Filter    => (Directory     => False,
                               Ordinary_File => False,
                               Special_File  => True));
    loop
      exit when not More_Entries(Search => The_Search);
      Get_Next_Entry(Search          => The_Search,
                     Directory_Entry => Dir_Ent);
      Move(Simple_Name(Dir_Ent),Pipe_Data.Name);
      Pipe_List.Append(Pipe_Data);
    end loop;
    End_Search (Search => The_Search);
  end Fill_Pipe_List;

  ------------------------------------------------------
  procedure Do_Initialize(aPipe : in out Pipe_Type) is
  begin
      if not aPipe.Is_Initialized then
          aPipe := Process_io.Pipe.Create(This_Process);
          if not aPipe.Exists then
            aPipe.Create_Pipe_On_Disk;
          end if;
          aPipe.Open(Process_io.Pipe.O_RDWR); --O_RDONLY
          aPipe.Delete_On_Exit := True;
      end if;
  end Do_Initialize;
  ------------------------------------------------------
  procedure Increase_Sent(aPipe : in out Pipe_Type) is
  begin
    aPipe.Status.Num_Sent  := aPipe.Status.Num_Sent+1;
    aPipe.Status.Last_Sent := Calendar2.Clock;
  end Increase_Sent;
  ------------------------------------------------------
  procedure Increase_Failed_Sent(aPipe : in out Pipe_Type) is
  begin
    aPipe.Status.Num_Fail_Sent := aPipe.Status.Num_Fail_Sent+1;
  end Increase_Failed_Sent;
  ------------------------------------------------------
  procedure Increase_Timeout(aPipe : in out Pipe_Type) is
  begin
    aPipe.Status.Num_Timeout := aPipe.Status.Num_Timeout+1;
  end Increase_Timeout;
  ------------------------------------------------------
  procedure Increase_Received(aPipe : in out Pipe_Type) is
  begin
    aPipe.Status.Num_Received  := aPipe.Status.Num_Received+1;
    aPipe.Status.Last_Received := Calendar2.Clock;
  end Increase_Received;
  ------------------------------------------------------
  procedure Send_Statistics(aPipe : in out Pipe_Type; Message : Process_Io.Message_Type) is
    Enquire : Pio_Enquire_Status_Record := Process_Io_Messages.Data(Message);
  begin
     case Enquire.Mode is
       when 0 =>  Process_Io_Messages.Send(Sender(Message), aPipe.Status);
       when 1 =>  -- find out que stat and queue
         -- here, loop over all untreated msgs and send back sender + identity
         -- Process_Io.Msg_Pack.Get_First(Process_Io.Msg_List, Msg,Eol);
         --loop
         --  exit when Eol;
         --  Msg_Data.Sender := Sender(Msg);
         --  Msg_Data.Identity := Identity(Msg);
         --  Process_Io_Messages.Send(Sender(Message),Msg_Data);
         --  Process_Io.Msg_Pack.Get_Next(Process_Io.Msg_List, Msg,Eol);
         --end loop;
         Process_Io_Messages.Send(Sender(Message), aPipe.Status);  -- first send que stat.

       when others => null;
     end case;
  end Send_Statistics;
  ------------------------------------------------------
  ------------------------------------------------------
end Process_Io.Pipe;
