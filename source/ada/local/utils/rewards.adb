
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Text_Io; use Text_Io;

with Gnat; use Gnat;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Types; use Types;
with Calendar2; use Calendar2;

with Stacktrace;
--with Ada.Containers.Doubly_Linked_Lists;
with Price_Histories;
with Ini;
with Logging; use Logging;
with Sql;
with Sim;
with Bot_Types; --use Bot_Types;
with Ada.IO_Exceptions;

procedure Rewards is
  package Ad renames Ada.Directories;
  package Ev renames Ada.Environment_Variables;

  --    function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
  --    begin
  --      return Left.Backprice < Right.Backprice;
  --    end "<";
  --    package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");
  Global_Size : constant Bot_Types.Bet_Size_Type := 100.0;
  Commission  : constant Fixed_Type := 2.0/100.0;

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
  pragma Unreferenced(Create_Marketname);
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
  function Check_Profit (Runner : Price_Histories.Price_History_Type; Ttphm :  Sim.Timestamp_To_Prices_History_Maps.Map) return Bot_Types.Profit_Type is
    Is_Winner                       : Boolean := False;
    use Bot_Types;
    Profit                          : Profit_Type := 0.0;
  begin

    for W of Sim.Winners_Map (Runner.Marketid) loop
      if W.Selectionid = Runner.Selectionid then
        Is_Winner := True;
        exit;
      end if;
    end loop;

    --simplification, but ok. betting on the wrong horse should be penalized, even if not matched
    if not Is_Winner then
      return - Profit_Type(Global_Size);
    end if;

    Loop_Ts_Check_Profit : for Timestamp of Sim.Marketid_Pricets_Map (Runner.Marketid) loop
      declare
        List                : Price_Histories.Lists.List := Ttphm (Timestamp.To_String);
        Delta_Time          : Calendar2.Interval_Type := Runner.Pricets - Timestamp;
        Tmp                 : Profit_Type := 0.0;
      begin

        for J of List loop
          if J.Selectionid = Runner.Selectionid and then
            Delta_Time >= (0, 0, 0, 1, 0) then -- check time +1s

            if J.Backprice >= Runner.Backprice then --match
              if Is_Winner then
                if  Runner.Backprice < 1.01 then
                  return 0.0;
                else
                  Tmp := Profit_Type(Global_Size) * Profit_Type (Runner.Backprice - 1.0) ;
                  Profit := Tmp * Profit_Type (1.0 - Commission);
                end if;
              else
                return - Profit_Type(Global_Size);
              end if;
            end if;

          end if;
        end loop;
      end;
    end loop Loop_Ts_Check_Profit;
    return Profit;
  end Check_Profit;

  -------------------------------------------------------
  function Check_Profit_Lay (Runner : Price_Histories.Price_History_Type; Ttphm :  Sim.Timestamp_To_Prices_History_Maps.Map) return Bot_Types.Profit_Type is
    Is_Winner                       : Boolean := False;
    use Bot_Types;
    Profit                          : Profit_Type := 0.0;
  begin

    for W of Sim.Winners_Map (Runner.Marketid) loop
      if W.Selectionid = Runner.Selectionid then
        Is_Winner := True;
        exit;
      end if;
    end loop;

    if Is_Winner then
      return - Profit_Type(Global_Size) * Profit_Type (Runner.Layprice - 1.0);
    end if;

    Loop_Ts_Check_Profit : for Timestamp of Sim.Marketid_Pricets_Map (Runner.Marketid) loop
      declare
        List                : Price_Histories.Lists.List := Ttphm (Timestamp.To_String);
        Delta_Time          : Calendar2.Interval_Type := Runner.Pricets - Timestamp;
      begin

        for J of List loop
          if J.Selectionid = Runner.Selectionid and then
            Delta_Time >= (0, 0, 0, 1, 0) then -- check time +1s

            -- if here , we are not winner
            if J.Layprice <= Runner.Layprice then --match
              Profit := Profit_Type(Global_Size);
            end if;

          end if;
        end loop;
      end;
    end loop Loop_Ts_Check_Profit;
    return Profit;
  end Check_Profit_Lay;
  ------------------------------------------------------


  Sa_Side                         : aliased  Gnat.Strings.String_Access;
  Sa_Start_Date                   : aliased  Gnat.Strings.String_Access;
  Sa_Stop_Date                    : aliased  Gnat.Strings.String_Access;
  Sa_Logfilename                  : aliased  Gnat.Strings.String_Access;
  --Sa_Markettype                   : aliased  Gnat.Strings.String_Access;
  --Path                            :          String := Ev.Value("BOT_HISTORY") & "/data/ai/plc/rewards";
  Race                            :          Text_Io.File_Type;
  --Start_Date                      :          Calendar2.Time_Type := (2016,03,16,0,0,0,0);
  Start_Date                      :          Calendar2.Time_Type := (2018,11,1,0,0,0,0);
  One_Day                         : constant Calendar2.Interval_Type := (1,0,0,0,0);
  Current_Date                    :          Calendar2.Time_Type := Start_Date;
  Stop_Date                       :          Calendar2.Time_Type := (2021,3,31,23,59,59,999);
  Cmd_Line                        :          Command_Line_Configuration;
  T                               :          Sql.Transaction_Type;
  Side                            : Bot_Types.Bet_Side_Type := Bot_Types.Back;
begin


  Define_Switch
    (Cmd_Line,
     Sa_Start_Date'Access,
     Long_Switch => "--startdate=",
     Help        => "start date eg 2019-02-25");

  Define_Switch
    (Cmd_Line,
     Sa_Stop_Date'Access,
     Long_Switch => "--stopdate=",
     Help        => "stop date eg 2019-12-21");

  --    Define_Switch
  --       (Cmd_Line,
  --       Sa_Markettype'Access,
  --       Long_Switch => "--markettype=",
  --       Help        => "PLC/WIN");

  Define_Switch
    (Cmd_Line,
     Sa_Side'Access,
     Long_Switch => "--side=",
     Help        => "back/lay, back is default");

  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","rewards");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
  --  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

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


  if Sa_Start_Date.all /= "" then
    Start_Date := Calendar2.To_Time_Type(Sa_Start_Date.all,"");
  end if;

  if Sa_Stop_Date.all /= "" then
    Stop_Date := Calendar2.To_Time_Type(Sa_Stop_Date.all,"");
  end if;

  if Sa_Side.all /= "" and then Ada.Characters.Handling.To_Lower(Sa_Side.all) = "lay" then
    Side := Bot_Types.Lay;
  else
    Side := Bot_Types.Back;
  end if;

  Log("main", "params start");
  Log("main", "start_date '" & Sa_Start_Date.all & "'");
  Log("main", "stop_date  '" & Sa_Stop_Date.all & "'");
  Log("main", "logfile    '" & Sa_Logfilename.all & "'");
  Log("main", "side       '" & Sa_Side.all & "'");
  Log("main", "params stop");

  Current_Date := Start_Date;

  Date_Loop : loop
    T.Start;
    Log("start fill maps");
    --    Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse);
    Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse, Rewards => False, Racetimes => False);
    Log("start process maps");
    T.Commit;

    declare
      Cnt       : Integer := 0;
      First     : Boolean := True;
      Race_Type : String := "win";
    begin
      Market_Loop : for Market of Sim.Market_With_Data_List loop

        if Market.Markettype (1 .. 3) = "PLA" then
          Race_Type := "plc";
        elsif Market.Markettype (1 .. 3) = "WIN" then
          Race_Type := "win";
        end if;

        if True
        --market.Markettype(1..3) = Mtype and then

        --if Market.Markettype(1..3) = "PLA" and then
        -- 8 <= Market.Numactiverunners and then
        --  Market.Numactiverunners <= 16 --and then
        --   Market.Marketname_Ok2
        then
          declare
            File_Already_Exists : exception;
          begin
            First := True;
            Cnt := Cnt + 1;

            declare
              Path     : String := Ev.Value("BOT_HISTORY") & "/data/ai/" & Race_Type & "/rewards/" & Ada.Characters.Handling.To_Lower(Side'Img);
              Filename : String := Path & "/" & Market.Marketid & ".dat";
            begin
              if not Ad.Exists(Path) then
                Ad.Create_Directory(Path);
              end if;
              --   Text_Io.Put_Line("marketid='" & Market.Marketid & "' " & Market.Startts.String_Date_Time_Iso & " " & Calendar2.Clock.String_Date_Time_Iso);

              if Ad.Exists(Filename) then
                raise File_Already_Exists with Filename;
              end if;


              Text_Io.Create(File => Race,
                             Mode => Text_Io.Out_File,
                             Name => Path & "/" & Market.Marketid & ".dat");
            end;
            -- list of timestamps in this market
            declare
              Timestamp_To_Prices_History_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                  Sim.Marketid_Timestamp_To_Prices_History_Map (Market.Marketid);

              Timestamp_To_Prices_History_Map2 : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                   Sim.Marketid_Timestamp_To_Prices_History_Map (Market.Marketid);
              Have_Seen_1_0x                   : Boolean := False;
              All_More_Than_Limit              : Boolean := True;
              Last_Poll                        : Calendar2.Time_Type := Calendar2.Time_Type_First;
            begin
              Loop_Ts : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
                declare
                  List                : Price_Histories.Lists.List := Timestamp_To_Prices_History_Map (Timestamp.To_String);
                  Bra                 : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);
                  Delta_Time          : Calendar2.Interval_Type := (0, 0, 0, 0, 0);
                  Profit              : Bot_Types.Profit_Type := 0.0;
                  use Bot_Types;
                begin
                  --                Text_Io.Put_Line("start loop " & Calendar2.Clock.String_Date_Time_Iso);
                  To_Array(List => List, Bra => Bra);

                  if First then
                    Put (Race, "Timestamp |");
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
                    Delta_Time := Bra (1).Pricets - Last_Poll;
                    Check_Odds (Bra, Have_Seen_1_0x, All_More_Than_Limit);
                    exit Loop_Ts when Have_Seen_1_0x and then All_More_Than_Limit;

                    if Delta_Time < (0, 0, 0, 2, 0) then -- don't bother when race not started
                      for I in Bra'Range loop

                        if I = 1 then
                          Put (Race, Calendar2.String_Time (Date => Bra(I).Pricets, Milliseconds => True ) & "|");
                          -- Put (Race, Calendar2.String_Interval (Interval => Delta_Time, Days => False , Hours => False ) & "|");
                        end if;

                        if Bra (I).Selectionid > 0 then
                          case Side is
                            when Back => Profit := Check_Profit (Bra (I), Timestamp_To_Prices_History_Map2);
                            when Lay  => Profit := Check_Profit_Lay (Bra (I), Timestamp_To_Prices_History_Map2);
                          end case;
                        else
                          Profit := 0.0;
                        end if;

                        Put (Race, Profit'Img);
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
          exception
            when File_Already_Exists =>  null;
          end;

          begin
            Text_Io.Close(Race);
          exception
            when Ada.Io_Exceptions.Status_Error =>  null;
          end;

        end if; -- Market_type(1..3) = WIN
      end loop Market_Loop;
    end;

    Current_Date := Current_Date + One_Day;
    exit Date_Loop when Current_Date = Stop_Date;

  end loop Date_Loop;
  Sql.Close_Session;
exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);

end Rewards;
