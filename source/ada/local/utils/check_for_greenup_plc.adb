
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Ada.Containers.Doubly_Linked_Lists;

with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Stacktrace;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Sql;
with Calendar2; use Calendar2;
--with Bot_Messages;
--with Rpc;
--with Lock ;
with Posix;
with Ini;
with Logging; use Logging;
--with Process_IO;
--with Core_Messages;
with Markets;
--with Table_Aevents;
--with Table_Aprices;
--with Table_Abalances;
with Table_Arunners;
with Table_Abets;
with Table_Apriceshistory;
with Bot_Svn_Info;
--with Utils; use Utils;
with Bot_System_Number;
with Tics;

procedure Check_For_Greenup_Plc is
  package EV renames Ada.Environment_Variables;
  T                   : Sql.Transaction_Type;
  Select_Cand         : Sql.Statement_Type;
  Select_Ph           : Sql.Statement_Type;
  Select_Markets      : Sql.Statement_Type;
  Select_Runner       : Sql.Statement_Type;
  Select_Timestamps   : Sql.Statement_Type;
  Select_Selectionids : Sql.Statement_Type;
  Find_Plc_Market     : Sql.Statement_Type;

  Me                  : constant String := "Check_For_Greenup_Plc.";
  Global_Bet_Name     : Betname_Type := (others => ' ');
  --Global_Max_Layprice : Fixed_Type := 0.0;
  Global_Deltatics   : Integer := 0;
  Global_Max_Backprice: Fixed_Type := 0.0;
  Cmd_Line            : Command_Line_Configuration;
  --SA_Max_Layprice     : aliased Gnat.Strings.String_Access;
  SA_Deltatics        : aliased Gnat.Strings.String_Access;
  SA_Max_Backprice    : aliased Gnat.Strings.String_Access;
  Sa_Betname          : aliased Gnat.Strings.String_Access;

  package Ts_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Calendar2.Time_Type);
  Ts_List             : Ts_List_Pack.List;

  -----------------------------------------------------
 -- procedure Place_Market(M     : in     Markets.Market_Type;
 --                        P     : in out Markets.Market_Type;
 --                        Found : in out Boolean) is
 --   P : Markets.Market_Type ;
 -- begin
 --   Find_Plc_Market.Set("WINMARKETID", M.Marketid);
 --   Find_Plc_Market.Open_Cursor;
 --   Find_Plc_Market.Fetch(Eos);
 --   if not Eos then
 --     P := Table_Amarkets.Get(Find_Plc_Market);
 --     Found := M.Startts = P.Startts;
 --   else
 --     Found := False;
 --   end if;
 --   Find_Plc_Market.Close_Cursor;
 -- end Place_Market;

  ---------------------------------------------------
  procedure Add_Bet(M : Markets.Market_Type;
                    R : Table_Arunners.Data_Type;
                    P : Table_Apriceshistory.Data_Type) is
    Bet : Table_Abets.Data_Type;
    Now : Calendar2.Time_Type := Calendar2.Clock;
    Bet_Name     : Betname_Type := Global_Bet_Name;
    Backsize       : Bet_Size_Type := 30.0;
    Backprice_Tics : Integer := Tics.Get_Tic_Index(P.Backprice);
    Zero_Layprice  : Lay_Price_Type := Lay_Price_Type(Tics.Get_Tic_Price(Backprice_Tics -Global_Deltatics)); -- 30 tics lower eg (2.0 - 1.7)
    Zero_Laysize   : Bet_Size_Type := Tics.Get_Zero_Size(Back_Price_Type(P.Backprice),Backsize,Zero_Layprice);

  begin
  
    Bet_Name(1..3) := "GRL";
    Bet_Name(11 .. 14) := "_PLC";
    Bet := (
      Betid          => Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid)),
      Marketid       => M.Marketid,
      Betmode        => Bot_Mode(Simulation),
      Powerdays      => 0,
      Selectionid    => R.Selectionid,
      Reference      => (others => '-'),
      Size           => Fixed_Type(Backsize),
      Price          => P.Backprice,
      Side           => "BACK",
      Betname        => Bet_Name,
      Betwon         => False,
      Profit         => 0.0,
      Status         => (others => ' '),
      Exestatus      => (others => ' '),
      Exeerrcode     => (others => ' '),
      Inststatus     => (others => ' '),
      Insterrcode    => (others => ' '),
      Startts        => M.Startts,
      Betplaced      => P.Pricets,
      Pricematched   => Fixed_Type(0.0),
      Sizematched    => Fixed_Type(30.0),
      Runnername     => R.Runnernamestripped,
      Fullmarketname => M.Marketname,
      Svnrevision    => Bot_Svn_Info.Revision,
      Ixxlupd        => (others => ' '), --set by insert
      Ixxluts        => Now              --set by insert
    );
    Bet.Insert;

    Bet_Name := Global_Bet_Name;
    Bet_Name(1..3) := "GRB";
    Bet_Name(11 .. 14) := "_PLC";

    Bet := (
      Betid          => Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid)),
      Marketid       => M.Marketid,
      Betmode        => Bot_Mode(Simulation),
      Powerdays      => 0,
      Selectionid    => R.Selectionid,
      Reference      => (others => '-'),
      Size           => Fixed_Type(Zero_Laysize),
      Price          => Fixed_Type(Zero_Layprice),
      Side           => "LAY ",
      Betname        => Bet_Name,
      Betwon         => False,
      Profit         => 0.0,
      Status         => (others => ' '),
      Exestatus      => (others => ' '),
      Exeerrcode     => (others => ' '),
      Inststatus     => (others => ' '),
      Insterrcode    => (others => ' '),
      Startts        => M.Startts,
      Betplaced      => P.Pricets,
      Pricematched   => Fixed_Type(0.0),
      Sizematched    => Fixed_Type(30.0),
      Runnername     => R.Runnernamestripped,
      Fullmarketname => M.Marketname,
      Svnrevision    => Bot_Svn_Info.Revision,
      Ixxlupd        => (others => ' '), --set by insert
      Ixxluts        => Now              --set by insert
    );
    Bet.Insert;

  end Add_Bet;
  --------------------------------------------------

  procedure Read_Ts(Stm  : in out Sql.Statement_Type;
                    List : in out Ts_List_Pack.List) is
    Eos : Boolean := False;
    Ts : Calendar2.Time_Type;
  begin
     Stm.Open_Cursor;
     loop
       Stm.Fetch(Eos);
       exit when Eos;
       Stm.Get("PRICETS", Ts);
       List.Append(Ts);
     end loop;
     Stm.Close_Cursor;
  end Read_Ts;
  --------------------------------------------------

begin
  --declare
  --  Backsize       : Bet_Size_Type := 30.0;
  --  Backprice_Tics : Integer := Tics.Get_Tic_Index(Fixed_Type(2.0));
  --  Zero_Layprice  : Lay_Price_Type := Lay_Price_Type(Tics.Get_Tic_Price(Backprice_Tics -30)); -- 30 tics lower eg (2.0 - 1.7)
  --  Zero_Laysize   : Bet_Size_Type := Tics.Get_Zero_Size(Back_Price_Type(2.0),Backsize,Zero_Layprice);
  --begin
  --  Log("Zero_Layprice " & F8_Image(Fixed_Type(Zero_Layprice)));
  --  Log("Zero_Laysize  " & F8_Image(Fixed_Type(Zero_Laysize)));
  --  return;
  --end ;



  if not EV.Exists("BOT_NAME") then
    EV.Set("BOT_NAME","check_for_green");
  end if;
 -- Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Define_Switch
     (Cmd_Line,
      Sa_Betname'access,
      Long_Switch => "--betname=",
      Help        => "betname for equity");

  Define_Switch
     (Cmd_Line,
      SA_Deltatics'access,
      Long_Switch => "--delta=",
      Help        => "delta tics");

  Define_Switch
     (Cmd_Line,
      SA_Max_Backprice'access,
      Long_Switch => "--max_backprice=",
      Help        => "Max backprice");

  Getopt (Cmd_Line);  -- process the command line

  Move(SA_Betname.all,Global_Bet_Name);
  --Global_Max_Layprice := Fixed_Type'Value(SA_Max_Layprice.all);
  Global_Max_Backprice := Fixed_Type'Value(SA_Max_Backprice.all);
  Global_Deltatics    := Integer'Value(SA_Deltatics.all);

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log(Me, "Connect Db");
  Sql.Connect
        (Host     => Ini.Get_Value("stats", "host", ""),
         Port     => Ini.Get_Value("stats", "port", 5432),
         Db_Name  => Ini.Get_Value("stats", "name", ""),
         Login    => Ini.Get_Value("stats", "username", ""),
         Password => Ini.Get_Value("stats", "password", ""));
  Log(Me, "db Connected");

  Select_Runner.Prepare(
      "select * from APRICESHISTORY " &
      "where MARKETID = :MARKETID " &
      "and BACKPRICE <= :BACKPRICE " &
      "order by PRICETS "
  );

  Select_Markets.Prepare(
      "select * " &
      "from " &
      "AMARKETS " &
      "where MARKETTYPE = 'PLACE' " &
      "and STARTTS::date >= '2016-03-27' " & -- we do not have good data before that time
      "order by STARTTS"
  );

  Select_Selectionids.Prepare(
      "select * " &
      "from " &
      "ARUNNERS " &
      "where MARKETID = :MARKETID " &
      "and STATUS in ('WINNER','LOSER') " &
      "order by SORTPRIO desc"
  );

  Select_Cand.Prepare(
      "select * " &
      "from APRICESHISTORY " &
      "where MARKETID = :MARKETID " &
      "and PRICETS = :PRICETS " &
      "order by BACKPRICE"
  );

  Select_Ph.Prepare(
      "select * " &
      "from " &
      "APRICESHISTORY " &
      "where MARKETID = :MARKETID " &
      "and SELECTIONID = :SELECTIONID " &
      "and PRICETS >= :PRICETS " &
      "order by PRICETS"
  );

  Select_Timestamps.Prepare(
      "select distinct(PRICETS) " &
      "from " &
      "APRICESHISTORY " &
      "where MARKETID = :MARKETID " &
      "order by PRICETS"
  );

  Find_Plc_Market.Prepare(
    "select MP.* from AMARKETS MW, AMARKETS MP " &
    "where MW.EVENTID = MP.EVENTID " &
    "and MW.STARTTS = MP.STARTTS " &
    "and MW.MARKETID = :WINMARKETID " &
    "and MP.MARKETTYPE = 'PLACE' " &
    "and MP.NUMWINNERS = 3 " &
    "and MW.MARKETTYPE = 'WIN' "
  ) ;


  T.Start;
    declare
      Ph_List     : Table_Apriceshistory.Apriceshistory_List_Pack2.List;
      Market_List : Markets.Lists.List;
      Cnt    : Natural := 0;
    begin
      Log(Me & "Main" , "read start");
      Markets.Read_List(Select_Markets, Market_List);
      Log(Me & "Main" , "read done");

      Market_Loop : for Market of Market_List loop
        Cnt := Cnt +1;
        if Cnt rem 100 = 0 then
          Log(Me & "Main" , "treat: " & Market.To_String);
        end if;
        Ts_List.Clear;
        Select_Timestamps.Set("MARKETID", Market.Marketid);
        Read_Ts(Select_Timestamps, Ts_List);
        --Has := (others => False);

        Timestamp_Loop : for Ts of Ts_List loop
          Ph_List.Clear;
          Select_Cand.Set("MARKETID",Market.Marketid);
          Select_Cand.Set("PRICETS",Ts);
          Table_Apriceshistory.Read_List(Select_Cand, Ph_List);

          declare
            Idx : Integer := 0;
            type Best_Runners_Array_Type is array (1..20) of Table_Apriceshistory.Data_Type ;
            Best_Runners : Best_Runners_Array_Type := (others => Table_Apriceshistory.Empty_Data);
            Runner       : Table_Arunners.Data_Type;
            Eos          : Boolean := False;
          begin
            Ph_Loop : for Ph of Ph_List loop
              Idx := Idx +1;
              exit Ph_Loop when Idx > Best_Runners'Last;
              Best_Runners(Idx) := Ph;
            end loop Ph_Loop;
            -- Best_Runners is sorted lowest backprice to highest, max 4 entries

            if Best_Runners(1).Backprice <= Global_Max_Backprice and then
               Best_Runners(1).Backprice >= Fixed_Type(1.01) then

                Runner.Marketid    := Best_Runners(1).Marketid;
                Runner.Selectionid := Best_Runners(1).Selectionid;
                Runner.Read(Eos);
                Add_Bet(Market, Runner, Best_Runners(1));

               exit Timestamp_Loop; -- 1 set of bets per market
            end if;
          end;
        end loop Timestamp_Loop;
      end loop Market_Loop;

    end ;
  T.Commit;


exception
  when E: others =>
    declare
      Last_Exception_Name     : constant String  := Ada.Exceptions.Exception_Name(E);
      Last_Exception_Messsage : constant String  := Ada.Exceptions.Exception_Message(E);
      Last_Exception_Info     : constant String  := Ada.Exceptions.Exception_Information(E);
    begin
      Log(Last_Exception_Name);
      Log("Message : " & Last_Exception_Messsage);
      Log(Last_Exception_Info);
      Log("addr2line" & " --functions --basenames --exe=" &
           Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump(Last_Exception_Info));
    end ;

    Log(Me, "Closed log and die");
    Logging.Close;
    Posix.Do_Exit(0); -- terminate

end Check_For_Greenup_Plc;
