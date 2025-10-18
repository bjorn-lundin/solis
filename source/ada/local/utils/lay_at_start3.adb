with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Types;    use Types;
with Sql;
with Calendar2; use Calendar2;
--with Logging;               use Logging;
with Text_Io;
with Ini;
with  Ada.Environment_Variables;
--with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded;
with Bot_Types; use Bot_Types;
with Utils; use Utils;

with Ada.Strings ; use Ada.Strings;
with Ada.Strings.Fixed ; use Ada.Strings.Fixed;
with Stacktrace;
with Markets;
with Prices;
with Runners;
with Bets;



procedure Lay_At_Start3 is
  package Ev renames Ada.Environment_Variables;
  Cmd_Line              : Command_Line_Configuration;
  T                     : Sql.Transaction_Type with Warnings => Off;
  Select_Markets        : Sql.Statement_Type;

  Bet_Size           : Bet_Size_Type := 30.0;
  Gdebug             : Boolean := True;


  -------------------------------
  procedure Debug (What : String) is
  begin
    if Gdebug then
      Text_Io.Put_Line (Text_Io.Standard_Error, Calendar2.String_Date_Time_Iso (Clock, " " , "") & " " & What);
    end if;
  end Debug;
  pragma Warnings(Off, Debug);
  -------------------------------
  procedure Print (What : String) with Unreferenced is
  begin
    Text_Io.Put_Line (What);
  end Print;
  -------------------------------


  procedure Get_Market_Data(Market_List  : in out Markets.Lists.List) is
  begin
    Select_Markets.Prepare( "select M.* " &
                              "from AMARKETS M, OKMARKETS OM " &
                              "where true " &
                              "and M.MARKETID = OM.MARKETID " &  -- has more than 100 samples / runner
                              "and M.MARKETTYPE = 'WIN' " &
                              "and M.NUMRUNNERS >= 8 " &
                              "and M.NUMRUNNERS <= 16 " &
                              "order by M.STARTTS");

    Markets.Read_List(Select_Markets, Market_List);
  end Get_Market_Data;
  ------------------------------------------------------

  procedure Treat(Market   : in out Markets.Market_Type;
                  Min      : in     Fixed_Type;
                  Max      : in     Fixed_Type;
                  Tot      : in out Fixed_Type) is
    Price          : Prices.Price_Type;
    Runner         : Runners.Runner_Type;
    Prices_List    : Prices.Lists.List;
    Localname      : Betname_Type := (others => ' ');
    Bet            : Bets.Bet_Type;
  begin

    Move("LAY_AT_START_" & F8_Image(Min) & "_" & F8_Image(Max), Localname);

    Price.Marketid := Market.Marketid;
    Prices.Read_I1_Marketid(Price, Prices_List);
    Runner.Marketid := Market.Marketid;

    for P of Prices_List loop
      if Min <= P.Layprice and then P.Layprice <= Max then
        Runner.Selectionid := P.Selectionid;
        Runner.Marketid := P.Marketid;

        Bet := Bets.Create(Name   => Localname,
                           Side   => Lay,
                           Size   => Bet_Size,
                           Price  => Price_Type(P.Layprice),
                           Placed => P.Pricets,
                           Runner => Runner,
                           Market => Market);
        Bet.Pricematched := Bet.Price;
        Move("MATCHED", Bet.Status);
        Bet.Check_Outcome;
        Tot := Tot + Bet.Profit;
        Bet.Insert;
        --   Bet_List.Append(Bet);
        -- Log("Bet_laid-WIN_LAY", Bet.To_String);
      end if;

    end loop;

  end Treat;



  Mlist  :  Markets.Lists.List;
  C      : Integer_4 := 0;

  Sa_Max : aliased  Gnat.Strings.String_Access;
  Sa_Min : aliased  Gnat.Strings.String_Access;
  Gmin   : Fixed_Type := 0.0;
  Gmax   : Fixed_Type := 0.0;
  Tot    : Fixed_Type := 0.0;

begin


  Define_Switch
    (Cmd_Line,
     Sa_Max'Access,
     Long_Switch => "--max=",
     Help        => "max layprice");

  Define_Switch
    (Cmd_Line,
     Sa_Min'Access,
     Long_Switch => "--min=",
     Help        => "min layprice");


  Getopt (Cmd_Line);  -- process the command line

  Gmax := Fixed_Type'Value(Sa_Max.all);
  Gmin := Fixed_Type'Value(Sa_Min.all);

  Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");

  Debug("Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("database","host",""),
     Port     => Ini.Get_Value("database","port", 5432),
     Db_Name  => Ini.Get_Value("database","name",""),
     Login    => Ini.Get_Value("database","username",""),
     Password => Ini.Get_Value("database","password",""),
     Ssl_Mode => "prefer");
  Debug("db Connected");

  T.Start;
  Get_Market_Data(Mlist);


  for M of Mlist loop
    C := C +1;
    if C rem Integer_4(1_000) = Integer_4(0) then
      Debug(C'Img & " / " & Mlist.Length'Img & " tot=" & F8_Image(Tot));
    end if;

    Treat(Market => M, Min => Gmin, Max => Gmax, Tot => Tot);
  end loop;

  T.Commit;
  Sql.Close_Session;

exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
end Lay_At_Start3;
