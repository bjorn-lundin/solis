--with Ada.Containers.Doubly_Linked_Lists;
with  Ada.Environment_Variables;
--with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded;
with Gnatcoll.Json; use Gnatcoll.Json;

with Ada.Strings ; use Ada.Strings;
with Ada.Strings.Fixed ; use Ada.Strings.Fixed;
with Text_Io;

with Gnat.Command_Line; use Gnat.Command_Line;
--with Gnat.Strings;

with Aws;
with Aws.Headers;
--with Aws.Headers.Set;
with Aws.Response;
with Aws.Client;
pragma Elaborate_All (Aws.Headers);


with Sql;
with Calendar2; use Calendar2;
with Logging;               use Logging;
with Ini;
with Types; use Types;
with Bot_Types;
with Stacktrace;
--with Table_Amarkets;
--with Table_Arunners;

--with Table_Apriceshistory;
with Bets;
with Runners;
with Markets;

--with Ada.Containers;

with Sim;

with Price_Histories;



procedure Ai_Nn_Diff_Db is

--    function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
--    begin
--      return Left.Backprice < Right.Backprice;
--    end "<";
--    --------------------------------------------
-- package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");

  --------------------------------------------

  --use type Ada.Containers.Count_Type;
  Price : Price_Histories.Price_History_Type;

  package Ev renames Ada.Environment_Variables;
  Cmd_Line              : Command_Line_Configuration;
  T                     : Sql.Transaction_Type;

  Ba_Layprice         : aliased Boolean := False;
  Ia_Position         : aliased Integer := 0;

  Global_Start_Date    : Time_Type := Clock;

  Gdebug : Boolean := True;

  Global_Profit : Float := 0.0;

  --  type Odds_Type is (Previous, Current, Diff);
  type Odds_Type is (Current);
  Odds          : array(Odds_Type'Range,1..16) of Float := (others => (others => 0.0));
  Selid : array(1..16) of Long_Long_Integer := (others => 0);


  type Best_Runners_Array_Type is array (1..16) of Price_Histories.Price_History_Type ;
  Best_Runners      : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);

  Day      : Time_Type := (2018,8,1,00,00,00,000);
  End_Date : Time_Type := (2020,10,1,00,00,00,000);
  One_Day  : Interval_Type := (1,0,0,0,0);




  -------------------------------
  procedure Debug (What : String) is
  begin
    if Gdebug then
      Text_Io.Put_Line (Text_Io.Standard_Error, Calendar2.String_Date_Time_Iso (Clock, " " , "") & " " & What);
    end if;
  end Debug;
  pragma Warnings(Off, Debug);
  -------------------------------
  procedure Print (What : String) -- with Unreferenced
  is
  begin
    Text_Io.Put_Line (What);
  end Print;
  -------------------------------



  procedure  Get_Json_Reply (Query : in Json_Value; Do_Bet : out Boolean) is
    Aws_Reply    : Aws.Response.Data;
    Http_Headers : Aws.Headers.List := Aws.Headers.Empty_List;
    Data         : String := Query.Write;
    Me           : String := "main.";
    Post_Timeout :  exception;
  begin
    Do_Bet := False;

    Aws.Headers.Add (Http_Headers, "Accept", "application/json");
    Aws.Headers.Add (Http_Headers, "Content-Length", Data'Length'Img);

    Log(Me & "Get_JSON_Reply", "posting: " & Data);
    Aws_Reply := Aws.Client.Post (Url          => "http://192.168.1.136:8080",
                                  Data         => Data,
                                  Content_Type => "application/json",
                                  Headers      => Http_Headers,
                                  Timeouts     => Aws.Client.Timeouts (Each => 30.0));
    --    Log(Me & "Get_JSON_Reply", "Got reply, check it ");

    declare
      Reply : String := Aws.Response.Message_Body(Aws_Reply);
    begin
      if Reply /= "Post Timeout" then
        -- Log(Me & "Get_JSON_Reply", "Got reply: " & Reply  );
        Do_Bet := Reply = "1";
        Log(Me & "Get_JSON_Reply", "Got reply: " & Do_Bet'Img & " -> " & Reply  );

      else
        Log(Me & "Get_JSON_Reply", "Post Timeout -> Give up!");
        raise Post_Timeout ;
      end if;
    exception
      when Post_Timeout => raise;
      when others =>
        Log(Me & "Get_JSON_Reply", "***********************  Bad reply start *********************************");
        Log(Me & "Get_JSON_Reply", "Bad reply" & Aws.Response.Message_Body(Aws_Reply));
        Log(Me & "Get_JSON_Reply", "***********************  Bad reply stop  ********" );
    end;

  end Get_Json_Reply;
  pragma Unreferenced (Get_Json_Reply);
  ------------------------------------------------------------------------------

begin

  Define_Switch
    (Cmd_Line,
     Ba_Layprice'Access,
     Long_Switch => "--layprice",
     Help        => "Layprices - otherwise backprices");

  Define_Switch
    (Cmd_Line,
     Ia_Position'Access,
     Long_Switch => "--position=",
     Help        => "lay/back 1=leader, 2=2nd etc");

  Getopt (Cmd_Line);  -- process the command line

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

  Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");

  Log ("Connect db");
  Sql.Connect
    (Host     => "localhost",
     Port     => 5432,
     Db_Name  => "dry",
     Login    => "bnl",
     Password => "bnl");
  Log ("Connected to db");

  Day_Loop : loop

    exit Day_Loop when Day >  End_Date;
    Sim.Fill_Data_Maps(Day, Bot_Types.Horse, Rewards => False, Racetimes => False);

    Log("start process date " & Day.To_String);

    declare
      Cnt    : Integer := 0;
      Is_Win : Boolean := True;
      --  Bet : Bets.Bet_Type;
    begin
      Log("num markets " & Day.To_String & " " & Sim.Market_With_Data_List.Length'Img);

      Loop_Market : for Market of Sim.Market_With_Data_List loop
        Is_Win := Market.Markettype(1..3) = "WIN";

        if Is_Win then
          Log("Treat market " & Market.To_String );
          T.Start;
          Cnt := Cnt + 1;
          -- list of timestamps in this market
          declare
            Timestamp_To_Apriceshistory_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                Sim.Marketid_Timestamp_To_Prices_History_Map(Market.Marketid);
            --   Bet_Placed : Boolean := False;
            First                           : Boolean := True;
            Lowest_Selid                    : Long_Long_Integer := 0;
            Lowest_Pidx                     : Long_Long_Integer := 0;
            Lowest_Odds                     : Float             := 0.0;
            Winner                          : Long_Long_Integer := 0;
            Runner                          : Runners.Runner_Type;
            Eos                             : Boolean := False;


          begin
            Loop_Timestamp : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
              --Log("Treat marketid '" & Market.Marketid & "' pricets " & Timestamp.To_String);
              declare
                List : Price_Histories.Lists.List :=
                         Timestamp_To_Apriceshistory_Map(Timestamp.To_String);
              begin
                if First then
                  First := False;

                  for I in 1..16 loop
                    --  Odds(Previous,I) := 0.0;
                    Selid(I) := 0;
                  end loop;
                  Cnt := Cnt +1;

                  for R of List loop
                    if Sim.Is_Race_Winner(Selectionid => R.Selectionid, Marketid => Market.Marketid) then
                      Winner := Long_Long_Integer(R.Selectionid);
                      exit;
                    end if;
                  end loop;
                end if;

                --Current_Market_Of_Sample := List.First_Element;
                --if Current_Market_Of_Sample.Marketid /= Old_Market_Of_Sample.Marketid then
                --  Log("Treat marketid '" & Current_Market_Of_Sample.Marketid & "' " &
                --      "pricets " & Current_Market_Of_Sample.Pricets.To_String);
                --  Old_Market_Of_Sample := Current_Market_Of_Sample;
                --end if;

              --  Backprice_Sorter.Sort(List);
                Price.Backprice := 0.0;  --10_000.0
                Best_Runners := (others => Price);

--                  declare
--                    Idx : Integer := 0;
--                  begin
--                    for Tmp of List loop
--                      Idx := Idx +1;
--                      exit when Idx > Best_Runners'Last;
--                      Best_Runners(Idx) := Tmp;
--                    end loop;
--                  end ;

                -- sort bestrunners accrding to sortprio
                for Tmp of List loop
                  Runner.Marketid := Tmp.Marketid;
                  Runner.Selectionid := Tmp.Selectionid;
                  Runner.Read(Eos);
                  if not Eos then
                    Best_Runners(Integer(Runner.Sortprio)) := Tmp;
                  end if;
                end loop;


                declare
                  Params        : Json_Value := Create_Object;
                  -- Odds_Diff     : Json_Array := Empty_Array;
                  Odds_Curr     : Json_Array := Empty_Array;
                  Selid_Curr    : Json_Array := Empty_Array;
                begin
                  for I in 1..16 loop
                    if Ba_Layprice then
                      Odds(Current,I)  := Float(Best_Runners(I).Layprice);
                    else
                      Odds(Current,I)  := Float(Best_Runners(I).Backprice);
                    end if;

                    -- race is over
                    if Odds(Current,I) > 999.9 then
                      Odds(Current,I) := 0.0;
                    end if;

                    Selid(I) := Long_Long_Integer(Best_Runners(I).Selectionid);
                  end loop;

                  for I in 1..16 loop
                    --  Append(Odds_Diff, Create(Odds(Diff,I)));
                    Append(Odds_Curr, Create(Odds(Current,I)));
                    Append(Selid_Curr,Create(Selid(I)));
                  end loop;

                  -- Params.Set_Field (Field_Name => "diff", Field => Odds_Diff);
                  Params.Set_Field (Field_Name => "curr", Field => Odds_Curr);
                  Params.Set_Field (Field_Name => "selid", Field => Selid_Curr);

                  Lowest_Selid := 0;
                  Lowest_Pidx  := 0;
                  Lowest_Odds  := 1000_000.0;

                  for I in 1..16 loop
                    if Odds(Current,I) > 1.0
                      and then Odds(Current,I) < Lowest_Odds then
                      Lowest_Selid := Selid(I);
                      Lowest_Pidx := Long_Long_Integer(I-1);
                      Lowest_Odds := Odds(Current,I);
                    end if;
                  end loop;

                  declare
                    use Bot_Types;
                    Ts              : Calendar2.Time_Type := Timestamp; -- Calendar2.To_Time_Type(Date_And_Time_Str => Awk.Field(58)) ;
                    MyBet           : Bets.Bet_Type;
                    Runner          : Runners.Runner_Type;
                    Betname         : Betname_Type      := (others => ' ');
                    Side            : Bet_Side_Type := Lay;
                    Profit          : Float   := 0.0;
                    Do_Bet          : Boolean := False;
                  begin

                    Params.Set_Field (Field_Name => "winner", Field => Create(Winner));
                    Params.Set_Field (Field_Name => "lowest_selid", Field => Create(Lowest_Selid));
                    Params.Set_Field (Field_Name => "lowest_pidx", Field => Create(Lowest_Pidx));
                    Params.Set_Field (Field_Name => "lowest_odds", Field => Create(Lowest_Odds));
                    Params.Set_Field (Field_Name => "pricets", Field => Create(Ts.To_String));

                    if Fixed_Type(Lowest_Odds) > Fixed_Type(1.0) then
                      if not First then
                        Print(Params.Write);
                        --Get_Json_Reply(Params,Do_Bet);
                        if Do_Bet then
                          case Side is
                            when Back =>
                              case Ia_Position is
                                when 1 => Move("BACK_1_AI_0.999",Betname);
                                when 2 => Move("BACK_2_AI_0.999",Betname);
                                when others => raise Program_Error with "not implemented position" & Ia_Position'Img;
                              end case;
                            when Lay =>
                              case Ia_Position is
                                when 1 =>Move("LAY_1_AI_0.999",Betname);
                                when 2 =>Move("LAY_2_AI_0.999",Betname);
                                when others => raise Program_Error with "not implemented position" & Ia_Position'Img;
                              end case;
                          end case;
                          Runner.Selectionid := Integer_4(Lowest_Selid);

                          MyBet := Bets.Create(Name   => Betname,
                                               Side   => Side,
                                               Size   => 30.0,
                                               Price  => Price_Type(Lowest_Odds),
                                               Placed => Ts,
                                               Runner => Runner,
                                               Market => Market);

                          case Side is
                            when Back =>
                              if Winner = Long_Long_Integer(Runner.Selectionid) then -- win
                                Profit :=  0.98 * 30.0 * (Lowest_Odds -1.0);
                                MyBet.Betwon := True;
                              else
                                Profit := -30.0;
                                MyBet.Betwon := False;
                              end if;

                            when Lay =>
                              if Winner = Long_Long_Integer(Runner.Selectionid) then -- loss
                                Profit :=  -30.0 * (Lowest_Odds -1.0);
                                MyBet.Betwon := False;
                              else
                                Profit := 0.98 * 30.0;
                                MyBet.Betwon := True;
                              end if;
                          end case;
                          MyBet.Profit := Fixed_Type(Profit);
                          MyBet.Insert;

                          Global_Profit := Global_Profit + Profit;
                        end if;
                      end if;
                    end if;
                  end;
                end;
                First := False;


              end;
              exit Loop_Market when False;
            end loop Loop_Timestamp; --  Timestamp
          end;
          T.Commit;
        end if; -- Is_Win
      end loop Loop_Market;  -- marketid

    end;
    --      Log("num bets laid" & Global_Bet_List.Length'Img);
    --
    --      declare
    --        --        Profit, Sum, Sum_Winners, Sum_Losers  : array (Side_Type'range) of Fixed_Type   := (others => 0.0);
    --        --        Winners, Losers, Unmatched, Strange   : array (Side_Type'range) of Integer_4 := (others => 0);
    --        T : Sql.Transaction_Type;
    --      begin
    --        T.Start;
    --        for Bet of Global_Bet_List loop
    --          --Bet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
    --          Bet.Insert;
    --
    --        end loop;
    --        T.Commit;
    --
    --        --        for i in Side_Type'range loop
    --        --          Sum(i) := Sum_Winners(i) + Sum_Losers(i) ;
    --        --          Log("RESULT day       : " & Day.To_String & " " & i'Img );
    --        --          Log("RESULT Winners   : " & Winners(i)'Img & " " & Integer_4(Sum_Winners(i))'Img );
    --        --          Log("RESULT Losers    : " & Losers(i)'Img  & " " & Integer_4(Sum_Losers(i))'Img);
    --        --          Log("RESULT Unmatched : " & Unmatched(i)'Img  & " " & Unmatched(i)'Img);
    --        --          Log("RESULT Strange   : " & Strange(i)'Img  & " " & Strange(i)'Img);
    --        --          Log("RESULT Sum       : " & Integer_4(Sum(i))'Img );
    --        --        end loop;
    --        --        Log(" Min_Backprice1:" & Global_Min_Backprice1'Img &
    --        --            " Max_Backprice1:" & Global_Max_Backprice1'Img &
    --        --            " Min_Backprice2:" & Global_Min_Backprice2'Img &
    --        --            " Max_Backprice2:" & Global_Max_Backprice2'Img);
    --        --
    --        --        Log(" GT:" &  Integer(Sum(Back) + Sum(Lay))'Img);
    --      end ;
    --
    --      Global_Bet_List.Clear;
    Day := Day + One_Day;

  end loop Day_Loop;

  Sql.Close_Session;
  Log("Started : " & Global_Start_Date.To_String);
  Log("Done : " & Calendar2.Clock.To_String);
  Logging.Close;





exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
end Ai_Nn_Diff_Db;
