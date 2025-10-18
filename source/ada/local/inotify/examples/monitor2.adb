with Ada.Command_Line;
with Ada.Streams;
with Ada.Text_IO;
--with Ada.Directories;
with Inotify.Recursive;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--with Prices;


procedure Monitor2 is
  --package AD renames Ada.Directories;
  Instance : Inotify.Recursive.Recursive_Instance;
  W    : Inotify.Watch;


--  package List_Handler is new Ada.Containers.Doubly_Linked_Lists (Prices.Lists.List);
  package String_Handler is new Ada.Containers.Doubly_Linked_Lists (Unbounded_String);


 procedure Write_Content (Filename : String; US : in Unbounded_String) is
      File         : Ada.Streams.Stream_IO.File_Type with Warnings => Off;
      Stream       : Ada.Streams.Stream_IO.Stream_Access;
      -- File_On_Disk : String := Path & Filename;
      File_On_Disk : String :=  Filename;
      --Service      : constant String := "Read_From_Disk";
    begin
      -- Log(Object & Service, "read from file '" & Filename & "'");
      Ada.Text_IO.Put_Line ("Write_Content: Filename='" & Filename & "'");
      Ada.Streams.Stream_IO.Open
        (File => File,
         Name => File_On_Disk,
         Mode => Ada.Streams.Stream_IO.Out_File);
      Ada.Text_IO.Put_Line ("Write_Content: 2");
      Stream := Ada.Streams.Stream_IO.Stream (File);
      Ada.Text_IO.Put_Line ("Write_Content: 3");
      Unbounded_String'Write (Stream, US);
      Ada.Text_IO.Put_Line ("Write_Content: 4");
      Ada.Streams.Stream_IO.Close (File);
      Ada.Text_IO.Put_Line ("Write_Content: Data=' " & To_String(US) & "'");

    end Write_Content;



  protected List_Protector is
    procedure Put (Data : Unbounded_String);
    entry Get (US  : out Unbounded_String) ;
  private
    Cnt  : Natural := 0;
    List : String_Handler.List;
  end List_Protector;

  -- Path : String := "/dev/shm/bot/poller/";
  protected body List_Protector is
    procedure Put (Data : Unbounded_String) is
    begin
      List.Append (Data);
    end Put;

    entry Get (US  : out Unbounded_String) when Cnt > 0 is
      Tmp_List : String_Handler.List := List.Copy;
      First    : Boolean := True;
    begin
      List.Clear;
      for O of Tmp_List loop
        if First then
          US := O;
        else
          First := False;
          List.Append (O);
        end if;
      end loop;
    end Get;
  end List_Protector;


  procedure Handle_Event
    (Subject      : Inotify.Watch;
     Event        : Inotify.Event_Kind;
     Is_Directory : Boolean;
     Name         : String)
  is
    Kind : constant String := (if Is_Directory then "directory" else "file");
    use type Inotify.Event_Kind;
    Data :  Unbounded_String;

    procedure Read_Content (Filename : String; US : out Unbounded_String) is
      File         : Ada.Streams.Stream_IO.File_Type with Warnings => Off;
      Stream       : Ada.Streams.Stream_IO.Stream_Access;
      -- File_On_Disk : String := Path & Filename;
      File_On_Disk : String :=  Filename;
      --Service      : constant String := "Read_From_Disk";
    begin
      -- Log(Object & Service, "read from file '" & Filename & "'");
      Ada.Text_IO.Put_Line ("Read_Content: Filename='" & Filename & "'");
      Ada.Streams.Stream_IO.Open
        (File => File,
         Name => File_On_Disk,
         Mode => Ada.Streams.Stream_IO.In_File);
      Ada.Text_IO.Put_Line ("Read_Content: 2");
      Stream := Ada.Streams.Stream_IO.Stream (File);
      Ada.Text_IO.Put_Line ("Read_Content: 3");
      Unbounded_String'Read (Stream, US);
      Ada.Text_IO.Put_Line ("Read_Content: 4");
      Ada.Streams.Stream_IO.Close (File);
      Ada.Text_IO.Put_Line ("Read_Content: Data=' " & To_String(Data) & "'");

    end Read_Content;



  begin
    if Event = Inotify.CLOSED_WRITE then
      Ada.Text_IO.Put_Line (Event'Img & " " & Instance.Name (Subject) & " ->  [" & Kind & "] '" & Name & "'");

      Read_Content (Name, Data);
      Ada.Text_IO.Put_Line ("Handle_Event: Data=' " & To_String(Data) & "'");

      List_Protector.Put (Data);



      --      if Name = "/dev/shm/bot/do_rename" then
      --        begin
      --          Ad.Rename (Name & ".notthere" , Name & ".done");
      --        exception
      --          when Ada.IO_Exceptions.Use_Error =>
      --            Ada.Text_IO.Put_Line ("cannot rename '" & Name & "' target exists already");
      --          when Ada.IO_Exceptions.Name_Error =>
      --            Ada.Text_IO.Put_Line ("cannot rename '" & Name & "' source does not exist");
      --        end;
      --      end if;
    end if;
  end Handle_Event;

  task Consumer is
    entry Start;
  end Consumer;

  task body Consumer is
    Us : Unbounded_String;
  begin
    Ada.Text_IO.Put_Line ("wait for start");
    select
      accept Start do
        delay 1.0;
      end Start;

    end select;

    Write_Content("/dev/shm/bot/poller/poll1", To_Unbounded_String("test string"));


    loop
      Ada.Text_IO.Put_Line ("wait for data");
      List_Protector.Get (Us);
      Ada.Text_IO.Put_Line ("consumer : " & To_String (Us));
      Instance.Remove_Watch (W);
      Ada.Text_IO.Put_Line ("consumer watch removed");
    end loop;

  end Consumer;


begin

  W := Instance.Add_Watch
    (Path => Ada.Command_Line.Argument (1),
     Mask => (Closed_Write => True, others => False));
  Consumer.Start;

  Instance.Process_Events (Handle_Event'Access);

end Monitor2;

