
--------------------------------------------------------------------------------
--	COPYRIGHT	Consafe Logistics AB, Lund
--	RESPONSIBLE	Björn Lundin
--	DESCRIPTION	This files contains the pipe object
--------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Ada.Containers.Doubly_Linked_Lists;

with Calendar2;

package Process_Io.Pipe is
  -- pio utility stuff below
  ------------------------------------------------
  Pio_Enquire_Status_Message : constant  := 32_000; --Process_Io_Id_Type but don't 'with' core_definition....
  Pio_Status_Message         : constant  := 32_000-1; --Process_Io_Id_Type
  Pio_Queue_Message          : constant  := 32_000-2; --Process_Io_Id_Type

  ------------------------------------------------------------
  type Pio_Status_Record is record
      Num_Sent      : Integer                     := 0;
      Num_Received  : Integer                     := 0;
      Num_Untreated : Integer                     := 0;
      Num_Fail_Sent : Integer                     := 0;
      Num_Timeout   : Integer                     := 0;
      Created       : Calendar2.Time_Type := Calendar2.Time_Type_First;
      Last_Sent     : Calendar2.Time_Type := Calendar2.Time_Type_First;
      Last_Received : Calendar2.Time_Type := Calendar2.Time_Type_First;
    end record;

--  for Pio_Status_Record'alignment use 4;
--
--  for Pio_Status_Record use record
--      Num_Sent      at    0 range 0..4*8-1;
--      Num_Received  at    4 range 0..4*8-1;
--      Num_Untreated at    8 range 0..4*8-1;
--      Num_Fail_Sent at   12 range 0..4*8-1;
--      Num_Timeout   at   16 range 0..4*8-1;
--      Created       at   20 range 0..7*2*8-1;  -- 7 integer_2
--      Last_Sent     at   36 range 0..7*2*8-1;
--      Last_Received at   52 range 0..7*2*8-1;
--
--  end record;
--                                 -- I4   Fillers time_type
--  for Pio_Status_Record'size use 8*(5*4+ 2*2 + 3*7*2);

  ------------------------------------------------------------
  type Pio_Enquire_Status_Record is record
      Mode      : Integer                   := 0;
  end record;
--  for Pio_Enquire_Status_Record'alignment use 4;
--
--  for Pio_Enquire_Status_Record use record
--      Mode      at   0 range 0..31;
--  end record;
--
--  for Pio_Enquire_Status_Record'size use 8*4;

  ------------------------------------------------------------
  type Pio_Queue_Record is record
      Name     : Process_Io.Name_Type := (others => ' ');
      Node     : Process_Io.Name_Type := (others => ' ');
	  Identity : Process_Io.Identity_Type := Process_Io.Identity_Type'First;
  end record;
--  for Pio_Queue_Record'alignment use 4;
--
--  for Pio_Queue_Record use record
--      Name      at   0  range 0..15*8-1;
--      Node      at   16 range 0..15*8-1;
--      Identity  at   32 range 0..4*8-1;
--  end record;
--                                 --strings pads i4
--  for Pio_Queue_Record'size use 8*(2*15 + 2*1 +1*4);


  -- pio utility stuff above
  ------------------------------------------------

  type File_Id is new Integer;


  type Pipe_Type is new Controlled with record
    Is_Initialized  : Boolean := False;
    Name            : Unbounded_String := Null_Unbounded_String;
    Device          : Unbounded_String := Null_Unbounded_String;
    Device_Length   : Integer          := 0;
    C_Device        : Unbounded_String := Null_Unbounded_String;
    C_Device_Length : Integer          := 0;
    Id              : File_Id          := File_Id'First;
	Delete_On_Exit  : Boolean          := False; -- to kill my pipe on finalize, but keep others
	Status          : Pio_Status_Record;
  end record;


  overriding procedure Finalize (aPipe : in out Pipe_Type) ;
  function  Create(Receiver : Process_Type) return Pipe_Type;
  function  Exists(aPipe : Pipe_Type) return Boolean;
  procedure Open(aPipe : in out Pipe_Type; Flags : Interfaces.C.Int);
  procedure Close(aPipe : in out Pipe_Type);
  procedure Read(aPipe : in out Pipe_Type; Msg : in out Message_Type) ;
  procedure Write(aPipe : in out Pipe_Type; Msg : in Message_Type);
  procedure Create_Pipe_On_Disk(aPipe : in out Pipe_Type);
  procedure Remove_Pipe_From_Disk(aPipe : in out Pipe_Type);
  procedure Increase_Sent(aPipe : in out Pipe_Type);
  procedure Increase_Failed_Sent(aPipe : in out Pipe_Type);
  procedure Increase_Timeout(aPipe : in out Pipe_Type);
  procedure Increase_Received(aPipe : in out Pipe_Type);
  procedure Send_Statistics(aPipe : in out Pipe_Type; Message : Process_Io.Message_Type);
  procedure Do_Initialize(aPipe : in out Pipe_Type);


  function O_RDONLY return Interfaces.C.Int;
  function O_WRONLY return Interfaces.C.Int;
  function O_RDWR   return Interfaces.C.Int;

--  O_RDONLY   : constant Integer := Integer(Posix1.O_RDONLY); -- open for reading only
--  O_WRONLY   : constant Integer := Integer(Posix1.O_WRONLY); -- open for writing only
--  O_RDWR     : constant Integer := Integer(Posix1.O_RDWR);   -- open for reading and writing

  procedure Log(What : String; Indent : Integer := -1) ;
  procedure Increase_Indent(Delta_Indent : Integer) ;


  package Msg_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Process_Io.Process_Type);
  --will fill the supplied list with the names of the pipes on disk for this system
  procedure Fill_Pipe_List(Pipe_List : in out Msg_List_Pack.List);


end Process_Io.Pipe;
