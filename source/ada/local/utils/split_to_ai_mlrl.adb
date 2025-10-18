
with Types; use Types;
with Calendar2; use Calendar2;

with GNAT; use GNAT;
with Text_Io; use Text_Io;
with Ada.Directories;
with Stacktrace;
--with Ada.Containers.Doubly_Linked_Lists;
with Price_Histories;
with GNAT.Command_Line; use GNAT.Command_Line;
with Gnat.Strings;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ini;
with Logging; use Logging;
with Sql;
with Sim;
with Bot_Types;

procedure Split_To_Ai_MLRL is
  package AD renames Ada.Directories;
  package Ev renames Ada.Environment_Variables;

--    function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
--    begin
--      return Left.Backprice < Right.Backprice;
--    end "<";
--    package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");

  type Best_Runners_Array_Type is array (1..16) of Price_Histories.Price_History_Type;

  procedure To_Array(List : in out Price_Histories.Lists.List ;
                       Bra  : in out Best_Runners_Array_Type ) is

    Price             : Price_Histories.Price_History_Type := Price_Histories.Empty_Data;
  begin
    Price.Backprice := 0.0;
    Bra := (others => Price);

    declare
      Idx : Integer := 0;
    begin
      for Tmp of List loop
        if Tmp.Status(1..6) = "ACTIVE" then
          Idx := Idx +1;
          exit when Idx > Bra'Last;
          Bra(Idx) := Tmp;
        end if;
      end loop;
    end ;

  end To_Array;

  -----------------------------------------------------
  function To_Time(S2: String) return Calendar2.Time_Type is
    Tmp : Time_Type;
    S   : String (1 .. S2'Last - S2'First + 1) := S2;
  begin
    -- '22-01-2013 11:09:06'

    Tmp.Year  := Year_Type'Value (S (7 .. 10));
    Tmp.Month := Month_Type'Value (S (4 .. 5));
    Tmp.Day   := Day_Type'Value (S (1 .. 2));

    Tmp.Hour   := Hour_Type'Value (S (12 .. 13));
    Tmp.Minute := Minute_Type'Value (S (15 .. 16));
    if S'Length = 19 then
      Tmp.Second := Second_Type'Value (S (18 .. 19));
    else
      Tmp.Second := 0;
    end if;
    return Tmp;
  end To_Time;
  pragma Unreferenced(To_Time);
  -----------------------------------------------------

  function Create_Marketname(S : String) return String is
    Tmp : String := Trim(S,Both);
  begin
    for I in Tmp'Range loop
      case Tmp(I) is
        when ' ' => Tmp(I) := '_';
        when others => null;
      end case;
    end loop;
    return Tmp;
  end Create_Marketname;

  -------------------------------------------------------

  procedure Put (F : Text_Io.File_Type; S : String) is
  begin
    Text_Io.Put(F,S);
  end Put;

  -------------------------------------------------------

  procedure Put_Line (F : Text_Io.File_Type; S : String) is
  begin
    Text_Io.Put_Line(F,S);
  end Put_Line;

  -------------------------------------------

  procedure Check_Odds (B : in Best_Runners_Array_Type; Seen_1_0x, More_Than_Limit : in out Boolean) is
    Some_Under_Limit : Boolean := False;
  begin
    for Runner of B loop
      if not Seen_1_0x and then Runner.Backprice <= Fixed_Type(1.05) then
        Seen_1_0x := True;
      end if;
    end loop;

    if Seen_1_0x then
      for Runner of B loop
        -- is any runners within 1.01 - 50 ? then All_more_than limit is false
        if Fixed_Type (1.0) < Runner.Backprice and then Runner.Backprice < Fixed_Type (25.0) then
          Some_Under_Limit := True;
        end if;
      end loop;
      More_Than_Limit := not Some_Under_Limit;
    end if;
  end Check_Odds;

  -------------------------------------------------------

  Sa_Logfilename      : aliased  Gnat.Strings.String_Access;
  Race                :          Text_IO.File_Type;
 -- Start_Date          : constant Calendar2.Time_Type := (2016,03,16,0,0,0,0);
  Start_Date          : constant Calendar2.Time_Type := (2018,8,1,0,0,0,0);
  One_Day             : constant Calendar2.Interval_Type := (1,0,0,0,0);
  Current_Date        :          Calendar2.Time_Type := Start_Date;
 -- Stop_Date           : constant Calendar2.Time_Type := (2018,08,01,0,0,0,0);
  Stop_Date           : constant Calendar2.Time_Type := (2019,08,01,0,0,0,0);
  Cmd_Line            :          Command_Line_Configuration;
  Marketname          :          String_Object;
  T                   :          Sql.Transaction_Type;
begin

  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","aiml2");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
--  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);
  Log("main", "params start");
  Log("main", "params stop");


    Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
    Log("main", "Connect Db " &
          Ini.Get_Value("database_home", "host", "")  & " " &
          Ini.Get_Value("database_home", "port", 5432)'Img & " " &
          Ini.Get_Value("database_home", "name", "") & " " &
          Ini.Get_Value("database_home", "username", "") & " " &
          Ini.Get_Value("database_home", "password", "")
       );
    Sql.Connect
      (Host     => Ini.Get_Value("database_home", "host", ""),
       Port     => Ini.Get_Value("database_home", "port", 5432),
       Db_Name  => Ini.Get_Value("database_home", "name", ""),
       Login    => Ini.Get_Value("database_home", "username", ""),
       Password => Ini.Get_Value("database_home", "password", ""));
    Log("main", "db Connected");

  Date_Loop : loop
    T.Start;
    Log("start fill maps");
    Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse, Rewards => False, Racetimes => False);
    Log("start process maps");
    T.Commit;

    declare
      Cnt       : Integer := 0;
      First     : Boolean := True;
      Race_Type : String := "win";
    begin
      Market_Loop : for Market of Sim.Market_With_Data_List loop

--          if Market.Markettype(1..3) = "WIN" and then
--            Market.Marketname_Ok2 and then
--            Market.Numactiverunners < 8 then
--            Text_Io.Put(Market.Marketid & "|");
--            Text_Io.Put_Line(Market.To_String);
--          end if;
        if Market.Markettype (1 .. 3) = "PLA" then
          Race_Type := "plc";
        elsif Market.Markettype (1 .. 3) = "WIN" then
          Race_Type := "win";
        end if;


    --    if Market.Markettype (1 .. 3) = "WIN"
        if True
--            Market.Markettype (1 .. 3) = "PLA" and then
--            (Market.Marketid = "1.123631657" or Market.Marketid = "1.131837740")

        --  8 <= Market.Numrunners and then
        --  Market.Numrunners <= 16  --and then
         -- Market.Marketname_Ok2
        then
          First := True;

          Cnt := Cnt + 1;

          Marketname.Set(Create_Marketname(Market.Marketname));

          declare
            Path : String := Ev.Value("BOT_HISTORY") & "/data/ai/" & Race_Type & "/races/";
          begin

          if not Ad.Exists(Path & Marketname.Fix_String) then
            Ad.Create_Directory(Path & Marketname.Fix_String);
          end if;

      --    Text_Io.Put_Line("marketid='" & Market.Marketid & "'");

          Text_IO.Create(File => Race,
                         Mode => Text_IO.Out_File,
                         Name => Path & Marketname.Fix_String & "/" & Market.Marketid & ".dat");


          end;

          -- list of timestamps in this market
          declare
            Timestamp_To_Prices_History_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                Sim.Marketid_Timestamp_To_Prices_History_Map (Market.Marketid);

            Have_Seen_1_0x                  : Boolean := False;
            All_More_Than_Limit             : Boolean := True;
            Last_Poll                       : Calendar2.Time_Type := Calendar2.Time_Type_First;
          begin
            Loop_Ts : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
              declare
                List                : Price_Histories.Lists.List := Timestamp_To_Prices_History_Map (Timestamp.To_String);
                Bra                 : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);
                Delta_Time          : Calendar2.Interval_Type := (0, 0, 0, 0, 0);
              begin
                To_Array(List => List, Bra => Bra);

                if First then
                  Put (Race, "Timestamp|");
                  for I in Bra'Range loop
                    Put(Race,Bra(I).Selectionid'Img);
                    if I < Bra'Last then
                      Put(Race,"|");
                    else
                      Put_Line(Race,"");
                    end if;
                  end loop;
                  First := False;
                else
                  Delta_Time := Bra(1).Pricets - Last_Poll;
                  Check_Odds (Bra, Have_Seen_1_0x, All_More_Than_Limit);
                  exit Loop_Ts when Have_Seen_1_0x and then All_More_Than_Limit;

                  if Delta_Time < (0, 0, 0, 2, 0) then -- don't bother when race not started
                    Put (Race, Calendar2.String_Time (Date => Bra(1).Pricets, Milliseconds => True ) & "|");
                    for I in Bra'Range loop
                      Put (Race, Bra (I).Backprice'Img);
                      if I < Bra'Last then
                        Put (Race, "|");
                      else
                        Put_Line (Race, "");
                      end if;
                    end loop;
                  end if;
                end if;
                Last_Poll := Bra (1).Pricets;
              end;
            end loop Loop_Ts; --  Timestamp
          end;
          Text_IO.Close(Race);
        end if; -- Market_type(1..3) = WIN
      end loop Market_Loop;
    end;

    Current_Date := Current_Date + One_Day;
    exit when Current_Date = Stop_Date;

  end loop Date_Loop;
  Sql.Close_Session;
exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);

end Split_To_AI_MLRL;
