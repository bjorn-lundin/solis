with Ada.Exceptions;
with Ada.Command_Line;
--with Ada.Strings; use Ada.Strings;
--with Ada.Strings.Fixed; use Ada.Strings.Fixed;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Text_io;

with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Logging; use Logging;


with Stacktrace;
with Types; use Types;
--with Bot_Types;-- use Bot_Types;
with Sql;
--with Calendar2; use Calendar2;
with Ini;
with Statistics;
--with Utils; use Utils;
with Table_Abets;
--with Table_Apriceshistory;
--with Ada.Containers;

procedure  Stat_Maker is
  package EV renames Ada.Environment_Variables;

  s : Statistics.Stats_Array_Type;
  T : Sql.Transaction_Type;

  Bet_List  : Table_Abets.Abets_List_Pack2.List;
  --Tmp_Price : Table_Apriceshistory.Data_Type;
  --Eos       : Boolean := False;

  FO: Statistics.First_Odds_Range_Type;
  SO: Statistics.Second_Odds_Range_Type;
  GMT,MT: Statistics.Market_Type;

  use type Statistics.Market_Type;

  Cmd_Line           : Command_Line_Configuration;
  Sa_Par_Market_Type : aliased Gnat.Strings.String_Access;
  Ba_Par_Quiet       : aliased Boolean := False;
  Ia_Par_Week        : aliased Integer := 0;
  Ia_Par_Month       : aliased Integer := 0;


  --Select_Untreated_Bets              : Sql.Statement_Type;
  --Select_Prices_From_Dry             : Sql.Statement_Type;
  --Select_Betnames                    : Sql.Statement_Type;
  --Select_Betnames_With_Higher_Profit : Sql.Statement_Type;
  Select_Markets_Of_Correct_MT       : Sql.Statement_Type;

  --Cnt : Natural := 0;
  Me : constant String := "Stat_Maker.Main";
  --use type Ada.Containers.Count_Type;

  subtype Week_Type is Integer_4 range 0 .. 53;
  Week  : Week_Type  := 0;
  subtype Month_Type is Integer_4 range 0 .. 12;
  Month : Month_Type := 0;
  
begin
   Define_Switch
    (Cmd_Line,
     Sa_Par_Market_Type'access,
     Long_Switch => "--market_type=",
     Help        => "win or plc");

   Define_Switch
    (Cmd_Line,
     Ba_Par_Quiet'access,
     Long_Switch => "--quiet",
     Help        => "no logging at all");
    
   Define_Switch
    (Cmd_Line,
     Ia_Par_Week'access,
     Long_Switch => "--week=",
     Help        => "week num 2016");

   Define_Switch
    (Cmd_Line,
     Ia_Par_Month'access,
     Long_Switch => "--month=",
     Help        => "month (numerical)");

  Getopt (Cmd_Line);  -- process the command line


  if Ba_Par_Quiet then
    Logging.Set_Quiet(True);
  end if;  

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");

  GMT := Statistics.Market_Type'Value(Sa_Par_Market_Type.all);
  Sql.Connect
        (Host     => Ini.Get_Value("stats", "host", ""),
         Port     => Ini.Get_Value("stats", "port", 5432),
         Db_Name  => Ini.Get_Value("stats", "name", ""),
         Login    => Ini.Get_Value("stats", "username", ""),
         Password => Ini.Get_Value("stats", "password", ""));

  T.Start;

  Week  := Week_Type(Ia_Par_Week);
  Month := Month_Type(Ia_Par_Month);

  if Week > 0 then
    Log(Me, "read all bets for week" & Ia_Par_Week'Img);
    Select_Markets_Of_Correct_MT.Prepare(
        "select B.* from ABETS B " &
        "where B.BETNAME like :MARKETTYPE " &
        "and status= 'SUCCESS' " &
        "and extract(week from B.BETPLACED) = :WEEK"
    );
    Select_Markets_Of_Correct_MT.Set("WEEK", Week);
    if GMT = Statistics.Win then
      Select_Markets_Of_Correct_MT.Set("MARKETTYPE", "%WIN%");
    else
      Select_Markets_Of_Correct_MT.Set("MARKETTYPE", "%PLC%");
    end if;
    Table_Abets.Read_List(Select_Markets_Of_Correct_MT, Bet_List);
    
  elsif Month > 0 then
    Log(Me, "read all bets for month" & Month'Img);
    Select_Markets_Of_Correct_MT.Prepare(
        "select B.* from ABETS B " &
        "where B.BETNAME like :MARKETTYPE " &
        "and status= 'SUCCESS' " &
        "and extract(month from B.BETPLACED) = :MONTH"
    );
    Select_Markets_Of_Correct_MT.Set("MONTH", Month);
    if GMT = Statistics.Win then
      Select_Markets_Of_Correct_MT.Set("MARKETTYPE", "%WIN%");
    else
      Select_Markets_Of_Correct_MT.Set("MARKETTYPE", "%PLC%");
    end if;
    Table_Abets.Read_List(Select_Markets_Of_Correct_MT, Bet_List);
  else -- all  
    Log(Me, "read all bets");
    Select_Markets_Of_Correct_MT.Prepare(
        "select B.* from ABETS B " &
        "where B.BETNAME like :MARKETTYPE " &
        "and status= 'SUCCESS' "
    );
    if GMT = Statistics.Win then
      Select_Markets_Of_Correct_MT.Set("MARKETTYPE", "%WIN%");
    else
      Select_Markets_Of_Correct_MT.Set("MARKETTYPE", "%PLC%");
    end if;
    Table_Abets.Read_List(Select_Markets_Of_Correct_MT, Bet_List);
  end if;
  Log(Me, "Listsize=" & Bet_List.Length'Img);


  T.Commit;
  Sql.Close_Session;
--  Log(Me, "logged out");

  for b of Bet_List loop
    FO := Statistics.Get_First_Odds_Range(B.Betname);
    SO := Statistics.Get_Second_Odds_Range(B.Betname);
    MT:= Statistics.Get_Market_Type(B.Betname);
   -- B.Pricematched := Statistics.Get_Avg_Odds(B.Betname);    
    S(FO,SO,MT).Treat(B);
  end loop;

  for fi in Statistics.First_Odds_Range_Type'range loop
    for sn in Statistics.Second_Odds_Range_Type'range loop
      S(Fi,Sn,GMT).Calculate_Avg_Odds;
      Statistics.Print_Result( S(Fi, Sn, GMT),Fi, Sn, GMT);
    end loop;
  end loop;

 -- Log(Me, "Done");


exception
  when E: others =>
    declare
      Last_Exception_Name     : constant String  := Ada.Exceptions.Exception_Name(E);
      Last_Exception_Messsage : constant String  := Ada.Exceptions.Exception_Message(E);
      Last_Exception_Info     : constant String  := Ada.Exceptions.Exception_Information(E);
    begin
      Text_io.Put_Line(Last_Exception_Name);
      Text_io.Put_Line("Message : " & Last_Exception_Messsage);
      Text_io.Put_Line(Last_Exception_Info);
      Text_io.Put_Line("addr2line" & " --functions --basenames --exe=" &
           Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump(Last_Exception_Info));
    end ;

end Stat_Maker;

