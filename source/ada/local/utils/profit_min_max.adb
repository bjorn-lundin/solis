with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
with Bets;
with Calendar2; use Calendar2;
with Logging; use Logging;
with Tics;
with Utils; use Utils;
with Bot_System_Number;

procedure Profit_Min_Max is
  package EV renames Ada.Environment_Variables;
  Bet_List     : Bets.Lists.List;
  Bets_A_Day   : Sql.Statement_Type;
  T : Sql.Transaction_Type;
  Daily_Profit , Global_Profit : Profit_Type := 0.0;
  Start_Date   : Calendar2.Time_Type := (2016,3,31,0,0,0,0);
  Stop_Date    : Calendar2.Time_Type := (2017,1,31,0,0,0,0);
  Current_Date : Calendar2.Time_Type := Start_Date;
  One_Day      : Calendar2.Interval_Type := (1,0,0,0,0);
  Global_Size  : Bet_Size_Type   := 30.0;

  Ba_Checkonly : aliased Boolean := False;
  Sa_Betname   : aliased Gnat.Strings.String_Access;
  Ia_Max       : aliased Integer := 0;
  Ia_Min       : aliased Integer := 0;
  Cmd_Line     : Command_Line_Configuration;
  Betname      : Betname_Type := (others => ' ');

  ------------------------------------------------------------
  procedure Profit_Per_Bet_And_Day(Bet_List : in out Bets.Lists.List;
                                   Min      : Profit_Type;
                                   Max      : Profit_Type;
                                   Profit   : out Profit_Type) is
    Local_Profit : Profit_Type := 0.0;
    Current_Marketid : Marketid_Type := (others => ' ');
    Exit_On_Next_Market : Boolean := False;
  begin -- TODO fix correct commision
    for Bet of Bet_List loop
      exit when Exit_On_Next_Market and Current_Marketid /= Bet.Marketid;
      Current_Marketid := Bet.Marketid;
      Local_Profit := Local_Profit + Profit_Type(Bet.Profit);
      Log(F8_Image(Bet.Profit) & " => " & F8_Image(Bet.Pricematched) & " = " & F8_Image(Fixed_Type(Local_Profit)));
      if Local_Profit > Max then
        Exit_On_Next_Market := True;
      elsif Local_Profit < Min then
        Exit_On_Next_Market := True;
      end if;
      if not Ba_Checkonly then
        Move (Trim (Bet.Betname) & "_" & F8_Image (Fixed_Type (Min)) & "_" & F8_Image (Fixed_Type (Max)), Bet.Betname);
        Move("Min="&F8_Image (Fixed_Type (Min)) & ", Max=" & F8_Image (Fixed_Type (Max)), Bet.Reference);
        Bet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
        Bet.Insert;
      end if;
    end loop;
    Profit := Local_Profit;
  end Profit_Per_Bet_And_Day;

  ------------------------------------------------------------
  procedure Fix_Bets_In_List(Bet_List : in out Bets.Lists.List) is
    Tic : Integer := 0;
  begin
    for Bet of Bet_List loop
      Bet.Size := Fixed_Type(Global_Size);
      if    Bet.Side(1..3) = "LAY" then
        -- to get back to original legal odds
        Tic := Tics.Get_Nearest_Higher_Tic_Index(Bet.Price);
        Bet.Pricematched := Tics.Get_Tic_Price(Tic-3); -- some small margin to get the bet
        if Bet.Betwon then
          Bet.Profit := Bet.Size;
        else
          Bet.Profit := -Bet.Size * (Bet.Pricematched -1.0);
        end if;
      elsif Bet.Side(1..4) = "BACK" then
        -- to get back to original legal odds
        Tic := Tics.Get_Nearest_Higher_Tic_Index(Bet.Price);
        Bet.Pricematched := Tics.Get_Tic_Price(Tic+3); -- some small margin to get the bet
        if Bet.Betwon then
          Bet.Profit := Bet.Size * (Bet.Pricematched -1.0);
        else
          Bet.Profit := -Bet.Size;
        end if;
      else
        Log("WTF! " & Bet.To_String);
      end if ;
    end loop;
  end Fix_Bets_In_List;
  ------------------------------------------------------------

begin

   Define_Switch
    (Cmd_Line,
     Ba_Checkonly'access,
     Long_Switch => "--checkonly",
     Help        => "no inserts - just check");

   Define_Switch
    (Cmd_Line,
     Sa_Betname'access,
     Long_Switch => "--betname=",
     Help        => "betname");

   Define_Switch
     (Cmd_Line,
      IA_Min'access,
      Long_Switch => "--min=",
      Help        => "min daily profit (ie max loss)");

   Define_Switch
     (Cmd_Line,
      Ia_Max'access,
      Long_Switch => "--max=",
      Help        => "max daily profit");

  Getopt (Cmd_Line);  -- process the command line

  Move(Sa_Betname.all & "_" & F8_Image(Fixed_Type(IA_Min)) & "_" & F8_Image(Fixed_Type(IA_Max)), Betname);

  if not Ba_Checkonly then
    Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Trim(Betname) & ".log");
  end if;

  Log ("Connect db");
  if Ba_Checkonly then
    Sql.Connect
      (Host     => "betbot.nonobet.com",
       Port     => 5432,
       Db_Name  => "bnl",
       Login    => "bnl",
       Password => "ld4BC9Q51FU9CYjC21gp");
    --    Sql.Connect
    --      (Host     => "192.168.1.20",
    --       Port     => 5432,
    --       Db_Name  => "dry",
    --       Login    => "bnl",
    --       Password => "bnl");
  else
    Sql.Connect
      (Host     => "localhost",
       Port     => 5432,
       Db_Name  => "bnl",
       Login    => "bnl",
       Password => "bnl");
  end if;

  T.Start;
  Bets_A_Day.Prepare("select * from ABETS " &
                      "where BETNAME=:BETNAME " &
                      "and STARTTS >=:START " &
                      "and STARTTS <=:STOP " &
                      "order by STARTTS,MARKETID,BETPLACED"
                    );

  Log("start ------ '" & Sa_Betname.all & "' '" & Current_Date.To_String & " --------");
  loop
    --Log("start ------ " & Current_Date.To_String & " --------");
    Bets_A_Day.Set("BETNAME",Sa_Betname.all);
    Bets_A_Day.Set("START",Current_Date);
    Bets_A_Day.Set("STOP",Current_Date+One_Day);
    Bet_List.Clear;
    Bets.Read_List(Bets_A_Day,Bet_List);
   -- Log("num bets" & Bet_List.Length'Img);
    Fix_Bets_In_List(Bet_List);
    Profit_Per_Bet_And_Day (Bet_List => Bet_List,
                            Min      => Profit_Type(IA_Min),
                            Max      => Profit_Type(IA_Max),
                            Profit   => Daily_Profit) ;

    Global_Profit := Global_Profit + Daily_Profit;
    Log("daily profit = " & Current_Date.To_String & " " & Trim(Betname) & " " & Integer_4(Daily_Profit)'Img);
    Current_Date := Current_Date + One_Day;
    exit when Current_Date > Stop_Date;
  end loop;


  T.Commit ;
  Sql.Close_Session;

  Log("Total profit = " & Integer_4(Global_Profit)'Img);
exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
end Profit_Min_Max;
