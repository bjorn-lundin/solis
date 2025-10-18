
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;

with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Stacktrace;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Sql;
with Calendar2; use Calendar2;
with Posix;
with Ini;
with Logging; use Logging;
with Markets;
with Runners;
with Bets;
with Prices;
with Bot_Svn_Info;
--with Utils; use Utils;

procedure Check_Grn is
  Me                  : constant String := "Check_Grn.";
  package EV renames Ada.Environment_Variables;
  T                   : Sql.Transaction_Type;
  Select_Cand         : Sql.Statement_Type;
  Select_Markets      : Sql.Statement_Type;
  Select_Timestamps   : Sql.Statement_Type;

  Cmd_Line            : Command_Line_Configuration;

  Global_Betname   : Betname_Type := (others => ' ');
  Global_Laysize   : Bet_Size_Type := 40.0;

  SA_Max_Price     : aliased Gnat.Strings.String_Access;
  SA_Min_Price     : aliased Gnat.Strings.String_Access;
  Sa_Betname       : aliased Gnat.Strings.String_Access;

  IA_Runners_Place : aliased Integer := 0;
  IA_Addon_Odds    : aliased Integer := 0;

 -- Bad_Runners_Place : exception;


  ---------------------------------------------------

begin
  if not EV.Exists("BOT_NAME") then
    EV.Set("BOT_NAME","check_lay");
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
      SA_Max_Price'access,
      Long_Switch => "--max_price=",
      Help        => "Max price");

  Define_Switch
     (Cmd_Line,
      SA_Min_Price'access,
      Long_Switch => "--min_price=",
      Help        => "Min price");

  Define_Switch
     (Cmd_Line,
      IA_Runners_Place'access,
      Long_Switch => "--runners_place=",
      Help        => "Runners place in race (1-50)");

  Define_Switch
     (Cmd_Line,
      IA_Addon_Odds'access,
      Long_Switch => "--addon_odds=",
      Help        => "Runners place in race (1-50)");



  Getopt (Cmd_Line);  -- process the command line

  Move(SA_Betname.all,Global_Betname);
 -- Global_Max_Price := Fixed_Type'Value(SA_Max_Price.all);
 -- Global_Min_Price := Fixed_Type'Value(SA_Min_Price.all);

 -- case IA_Runners_Place is
 --   when 1 .. 50 => null;
 --   when others => raise Bad_Runners_Place with IA_Runners_Place'Img;
 -- end case;


  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log(Me, "Connect Db");
  Sql.Connect
        (Host     => Ini.Get_Value("stats", "host", ""),
         Port     => Ini.Get_Value("stats", "port", 5432),
         Db_Name  => Ini.Get_Value("stats", "name", ""),
         Login    => Ini.Get_Value("stats", "username", ""),
         Password => Ini.Get_Value("stats", "password", ""));
  Log(Me, "db Connected");

  Select_Markets.Prepare(
      "select * " &
      "from " &
      "AMARKETS " &
      "where MARKETTYPE = 'WIN' " &
      "and STARTTS::date >= '2016-03-27' " & -- we do not have good data before that time
      "order by STARTTS"
  );

  Select_Cand.Prepare(
      "select * " &
      "from ARUNNERS " &
      "where MARKETID = :MARKETID " &
      "and STATUS <> 'LAPSED'"
  );

  Select_Timestamps.Prepare(
      "select distinct(PRICETS) " &
      "from " &
      "APRICESHISTORY " &
      "where MARKETID = :MARKETID " &
      "order by PRICETS"
  );

  if Bets.Is_Existing_I7(Betname => Global_Betname) then
    Log(Me & "Main" , "bet '" & Global_Betname & "' already exists. Exiting");
    return;
  end if;


  declare
    Runner_List     : Runners.Lists.List;
    Market_List : Markets.Lists.List;
    Cnt         : Natural := 0;
    --type Has_Type is (Lay);
--    subtype Max_Runners_Type is Integer range 1 .. 50;
  begin
    T.Start;
    Log(Me & "Main" , "read start");
    Markets.Read_List(Select_Markets, Market_List);
    Log(Me & "Main" , "read done");
    T.Commit;

    Market_Loop : for Market of Market_List loop
      T.Start;
      Cnt := Cnt +1;
      if Cnt rem 100 = 0 then
        Log(Me & "Main" , "treat: " & Market.To_String);
      end if;
      Select_Cand.Set("MARKETID",Market.Marketid);
      Runners.Read_List(Select_Cand, Runner_List);

      Runner_Loop : for Runner of Runner_List loop

        declare
          Eos   : Boolean := False;
          Bet   : array (Bet_Side_Type'Range) of Bets.Bet_Type;
          Price : Prices.Price_Type;
          Backsize : Bet_Size_Type := 0.0;
        begin

          Price.Marketid := Runner.Marketid;
          Price.Selectionid := Runner.Selectionid;
          Price.Read(Eos);

          if not Eos and then
             Price.Backprice >= Fixed_Type(1.01)  and then
             Price.Layprice  <=  Fixed_Type(29.0) and then
             Price.Layprice  >=  Fixed_Type(10.0) and then
             Price.Backprice <=  Fixed_Type(30.0) and then
             Price.Backprice >=  Fixed_Type(10.0) and then
             Price.Layprice/Price.Backprice <=1.1 then -- max 10% difference Lay/Back

            Bet(Lay) := Bets.Create(Side   => Lay,
                                    Name   => Global_Betname,
                                    Size   => Global_Laysize,
                                    Price  => Price_Type(Price.Layprice),
                                    Placed => Price.Pricets,
                                    Runner => Runner,
                                    Market => Market);
            Bet(Lay).Match_Directly(False);
            Bet(Lay).Insert;
            Bet(Lay).Check_Matched;
            Bet(Lay).Check_Outcome;
            Bet(Lay).Update_Withcheck;

            --Backsize * Backprice = Laysize * Layprice -1
            Backsize := Global_Laysize * (Bet_Size_Type(Price.Layprice - Fixed_Type(1.0)) / (Price.Layprice ));
            Bet(Back) := Bets.Create(Side   => Back,
                                     Name   => Global_Betname,
                                     Size   => Backsize,
                                     Price  => Price_Type(Price.Layprice + Fixed_Type(1.0)),
                                     Placed => Price.Pricets,
                                     Runner => Runner,
                                     Market => Market);
            Bet(Back).Match_Directly(False);
            Bet(Back).Insert;
            Bet(Back).Check_Matched;
            Bet(Back).Check_Outcome;
            Bet(Back).Update_Withcheck;
          end if; --Eos
        end;
      end loop Runner_Loop;

      T.Commit;
    end loop Market_Loop;
  end ;


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

end Check_Grn;
