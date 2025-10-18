with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Streams;
with Ada.Streams.Stream_Io;

with Types ; use Types;
with Bot_types ; use Bot_types;
with Ini;
with Stacktrace;
with Sql;
with Bets;
with Calendar2; use Calendar2;
with Logging; use Logging;
--with General_Routines; use General_Routines;
with Text_Io;
with Utils; use Utils;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;


procedure Check_Greed is



  package Ad renames Ada.Directories;
  package Ev renames Ada.Environment_Variables;


  generic
    type Data_Type is private;
    Animal : Animal_Type ;
  package Disk_Serializer is
    function File_Exists(Filename : String) return Boolean ;
    procedure Write_To_Disk (Container : in Data_Type; Filename : in String);
    procedure Read_From_Disk (Container : in out Data_Type; Filename : in String);
  end Disk_Serializer;

  ---------------------------------------------------------
  procedure  Normalize_To(Size : Fixed_Type ;List: in out Bets.Lists.List) is
    Factor : Fixed_Type := 1.0;
  begin
    for B of List loop
      Factor        := Size / B.Size;
      B.Size        := Factor * B.Size;
      B.Sizematched := Factor * B.Sizematched;
      B.Profit      := Factor * B.Profit;
    end loop;
  end Normalize_To;
  ---------------------------------------------------------


  package body Disk_Serializer is
    --------------------------------------------------------
    Ani : String := Lower_Case(Animal'Img);
    Path : String := Ev.Value("BOT_HISTORY") & "/data/streamed_objects/" & Ani & "/misc/";
    --Path : String := "/mnt/samsung1gb/data/streamed_objects/";

    function File_Exists(Filename : String) return Boolean is
     -- Service : constant String := "File_Exists";
      File_On_Disk : String := Path & Filename;
      File_Exists  : Boolean := AD.Exists(File_On_Disk) ;
      Dir          : String := Ad.Containing_Directory(File_On_Disk);
      Dir_Exists   : Boolean := AD.Exists(Dir) ;
      use type AD.File_Size;
    begin
      if not Dir_Exists then
        Ad.Create_Directory(Dir);
      end if;
    --  Log(Object & Service, "Exists: " & Exists'Img);
      if File_Exists then
        File_Exists := AD.Size (File_On_Disk) > 5;
      end if;
      return File_Exists;
    end File_Exists;
    ---------------------------------------------------------------
    procedure Write_To_Disk (Container : in Data_Type; Filename : in String) is
      File   : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_IO.Stream_Access;
      File_On_Disk : String := Path & Filename;
    --  Service : constant String := "Write_To_Disk";
    begin
    --  Log(Object & Service, "write to file '" & Filename & "'");
      Ada.Streams.Stream_IO.Create
          (File => File,
           Name => File_On_Disk,
           Mode => Ada.Streams.Stream_IO.Out_File);
      Stream := Ada.Streams.Stream_IO.Stream (File);
      Data_Type'Write(Stream, Container);
      Ada.Streams.Stream_IO.Close(File);
    --  Log(Object & Service, "Stream written to file " & Filename);
    end Write_To_Disk;
    --------------------------------------------------------
    procedure Read_From_Disk (Container : in out Data_Type; Filename : in String) is
      File   : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_IO.Stream_Access;
      File_On_Disk : String := Path & Filename;
    --  Service : constant String := "Read_From_Disk";
    begin
     -- Log(Object & Service, "read from file '" & Filename & "'");
      Ada.Streams.Stream_IO.Open
          (File => File,
           Name => File_On_Disk,
           Mode => Ada.Streams.Stream_IO.In_File);
      Stream := Ada.Streams.Stream_IO.Stream (File);
      Data_Type'Read(Stream, Container);
      Ada.Streams.Stream_IO.Close(File);
    --  Log(Object & Service, "Stream read from file " & Filename);
    end Read_From_Disk;
    --------------------------------------------------------
  end Disk_Serializer;
  ----------------------------------------------------------

  package Serializer is new Disk_Serializer(Bets.Lists.List, Horse);


  Bet_List            : Bets.Lists.List ;
  Stm                 : Sql.Statement_Type;
  T                   : Sql.Transaction_Type;
  Total_Profit        : Fixed_Type := 0.0;
  Todays_Profit       : Fixed_Type := 0.0;
  Ts                  : Calendar2.Time_Type := (2018,05,01,0,0,0,0);
  Current_Date        : Calendar2.Time_Type ; --:= (2018,04,30,0,0,0,0);
  Max_Daily_Profit    : Fixed_Type := 100.0;
  Min_Daily_Profit    : Fixed_Type := -100.0;
  Tmp_I, Tmp_J        : String_Object;
  Cmd_Line            :          Command_Line_Configuration;
  Sa_Betname          : aliased  Gnat.Strings.String_Access;
  Sa_Min_Pricematched : aliased  Gnat.Strings.String_Access;

begin
  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");

  Define_Switch
    (Cmd_Line,
     Sa_Betname'Access,
     Long_Switch => "--betname=",
     Help        => "name of bet");

  Define_Switch
    (Cmd_Line,
     Sa_Min_Pricematched'Access,
     Long_Switch => "--min_pricematched=",
     Help        => "min price matched");


  Getopt (Cmd_Line);  -- process the command line

  declare
    Filename : String_Object;
  begin
    Filename.Set(Sa_Betname.all & "_" & Sa_Min_Pricematched.all & ".dat");
    if not Serializer.File_Exists(Filename.Lower_Case) then
      Log ("Connect db");
      Sql.Connect
        (Host     => Ini.Get_Value("database", "host", ""),
         Port     => Ini.Get_Value("database", "port", 5432),
         Db_Name  => Ini.Get_Value("database", "name", ""),
         Login    => Ini.Get_Value("database", "username", ""),
         Password => Ini.Get_Value("database", "password", ""));

      --                  "'HORSE_BACK_1_50_01_1_2_PLC_1_06'," &
      --                  "'HORSE_BACK_1_30_01_1_2_PLC_1_01'," &
      --                  "'HORSE_BACK_1_36_01_1_2_PLC_1_01'," &
      --                  "'HORSE_BACK_1_17_01_1_2_PLC_1_01') " &


      T.Start;
      Stm.Prepare("select * from ABETS " &
                    "where true " &
                    "and STARTTS >= :DATE " &
                    "and BETNAME = :BETNAME " &
                    "and PRICEMATCHED >= :PRICEMATCHED " &
                    "and STATUS = 'SETTLED' " &
                    "order by BETPLACED");
      Stm.Set_Timestamp("DATE", Ts);
      Stm.Set("BETNAME", Sa_Betname.all);
      Stm.Set("PRICEMATCHED", Fixed_Type'Value(Sa_Min_Pricematched.all));

      Bets.Read_List(Stm, Bet_List);

      T.Commit ;
      Sql.Close_Session;
      Normalize_To(100.0,Bet_List);

      Serializer.Write_To_Disk(Bet_List,Filename.Lower_Case);
    else
      Serializer.Read_From_Disk(Bet_List,Filename.Lower_Case);
    end if;
  end;

  Log("num bets" & Bet_List.Length'Img);
--    for Bet of Bet_List loop
--          Log("size " & F8_Image(Bet.Size) & " ; " &
--              "sizemateched " & F8_Image(Bet.Sizematched) & " ; " &
--              "Profit " & F8_Image(Bet.profit) & " ; " &
--               Bet.Betplaced.String_Date_ISO);
--    end loop;
--
--    return;

  Loss_Loop : for J in  1..1500 loop
    Min_Daily_Profit := Fixed_Type( -J);

    Greed_Loop : for I in 1 .. 1500 loop
      Total_Profit := 0.0;
      Current_Date := (2018,5,1,0,0,0,0);
      Max_Daily_Profit := Fixed_Type(I);

      Date_Loop     : loop
        Todays_Profit := 0.0;
        --Last_Marketid  := (others => ' ');

--          Log("Todays profit " & F8_Image(Todays_Profit) & " ; " &
--              "Min_Daily_Profit " & F8_Image(Min_Daily_Profit) & " ; " &
--              "Max_Daily_Profit " & F8_Image(Max_Daily_Profit) & " ; " &
--               Current_Date.String_Date_ISO);


        List_Loop : for Bet of Bet_List loop
          if Min_Daily_Profit < Todays_Profit and then Todays_Profit < Max_Daily_Profit then

            if Bet.Startts.Year = Current_Date.Year and then
              Bet.Startts.Month = Current_Date.Month and then
              Bet.Startts.Day = Current_Date.Day then
--             Log("Todays profit " & F8_Image(Todays_Profit) & " ; " & Current_Date.String_Date_ISO);

              if Bet.Sizematched > Fixed_Type(0.0) then
                Todays_Profit := Todays_Profit + Bet.Profit;
              end if;

            end if; -- current_date
          end if;
        end loop List_Loop;
        --Log("Todays profit  " & Tmp.Fix_String(Length => 4) & " ; " & F8_Image(Todays_Profit) & " ; " & Current_Date.String_Date_ISO);

        Current_Date := Current_Date + (1,0,0,0,0);
        exit Date_Loop when Current_Date >=  (2018,6,18,0,0,0,0);
        Total_Profit := Total_Profit + Todays_Profit;
      end loop Date_Loop;
      Tmp_I.Set(Integer(Max_Daily_Profit)'Img);
      Tmp_J.Set(Integer(Min_Daily_Profit)'Img);
      Text_Io.Put_Line(Tmp_I.Fix_String(Length => 4) & " | " & Tmp_J.Fix_String(Length => 4) & " | " & F8_Image(Total_Profit));

      if I = 1500 then
        Text_Io.New_Line;
      end if;

    end loop Greed_Loop;
  end loop Loss_Loop;


exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
end Check_Greed;
