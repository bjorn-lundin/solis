
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
with Posix;
with Ini;
with Logging; use Logging;
with Markets;
with Runners;
with Bets;
with Price_Histories;
with Bot_Svn_Info;
with Prices;
--with Utils; use Utils;

procedure Check_Lay2 is
   Me                  : constant String := "Check_Lay.";
   package EV renames Ada.Environment_Variables;
   T                   : Sql.Transaction_Type;
   Select_Cand         : Sql.Statement_Type;
   Select_Markets      : Sql.Statement_Type;
   Select_Timestamps   : Sql.Statement_Type;

   Cmd_Line            : Command_Line_Configuration;
  
   Global_Betname   : Betname_Type := (others => ' ');
   Global_Max_Price : Fixed_Type := 0.0;
   Global_Min_Price : Fixed_Type := 0.0;
   --Global_Min_Back_Price : Fixed_Type := 5.0;
   Global_Laysize   : Bet_Size_Type := 50.0;

   SA_Max_Price     : aliased Gnat.Strings.String_Access;
   SA_Min_Price     : aliased Gnat.Strings.String_Access;
   SA_Min_Back_Price     : aliased Gnat.Strings.String_Access;
   Sa_Betname       : aliased Gnat.Strings.String_Access;
   Ba_No_Backbet    : aliased Boolean := False;  
  -- IA_Runners_Place : aliased Integer := 0;
  -- IA_Addon_Odds    : aliased Integer := 0;
  
   --Bad_Runners_Place : exception;

   package Ts_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Calendar2.Time_Type);
   Ts_List             : Ts_List_Pack.List;

   
--     --------------------------------------------
--     function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
--     begin
--        return Left.Backprice < Right.Backprice;
--     end "<";
--     package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");
   
   
   Runners_To_Watch_List : Runners.Lists.List;   
   
   ---------------------------------------------------

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
      SA_Min_Back_Price'access,
      Long_Switch => "--min_back_price=",
      Help        => "Min back price");

   Define_Switch
     (Cmd_Line,
      Ba_No_Backbet'access,
      Long_Switch => "--no-backbet",
      Help        => "skip placing backbets");
   
   Getopt (Cmd_Line);  -- process the command line

   Move(SA_Betname.all,Global_Betname);
   Global_Max_Price := Fixed_Type'Value(SA_Max_Price.all);
   Global_Min_Price := Fixed_Type'Value(SA_Min_Price.all);
   --Global_Min_Back_Price := Fixed_Type'Value(SA_Min_Back_Price.all); 
   
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
                          "select M.* " &
                            "from " &
                            "AMARKETS M, AEVENTS E " &
                            "where M.MARKETTYPE = 'WIN' " &
                            "and M.EVENTID = E.EVENTID " &
                            "and M.NUMACTIVERUNNERS >= 8 " & 
                            "and M.TOTALMATCHED >= 1000000 " & 
                            "and E.EVENTTYPEID = 7 " & -- horses
                            "and M.STARTTS::date >= '2016-04-04' " & -- we do not have good data before that time
                            "order by M.STARTTS"
                         );

   Select_Cand.Prepare(
                       "select * " &
                         "from APRICESHISTORY " &
                         "where MARKETID = :MARKETID " &
                         "and PRICETS = :PRICETS " &
                         "order by BACKPRICE"
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
      Ph_List     : Price_Histories.Lists.List;
      Price_List  : Prices.Lists.List;
      The_Runner   : Runners.Runner_Type;
      Eos          : Boolean := False;
      The_Bet      : Bets.Bet_Type;
      Price        : Prices.Price_Type;
      Market_List : Markets.Lists.List;
      Start_Bets_OK : Boolean := False;
      Cnt         : Natural := 0;
      use type ada.Containers.Count_Type;
   begin
      T.Start;
      Log(Me & "Main" , "read start");
      Markets.Read_List(Select_Markets, Market_List);
      Log(Me & "Main" , "read done");
      T.Commit;
  
      Market_Loop : for The_Market of Market_List loop
         T.Start;
         Cnt := Cnt +1;
         if Cnt rem 100 = 0 then
            Log(Me & "Main" , "treat: " & The_Market.To_String);
         end if;
         Runners_To_Watch_List.Clear;
         Price.Marketid := The_Market.Marketid;
         Price_List.Clear;
         prices.Read_I1_Marketid(price,price_list);
         for p of price_list loop
            Start_Bets_OK := Global_Min_Price <= p.Layprice and then
              p.Layprice <= Global_Max_Price;
            if Start_Bets_OK then
               The_Runner := Runners.Empty_Data;
               The_Runner.Marketid    := p.Marketid;
               The_Runner.Selectionid := p.Selectionid;
               The_Runner.Read(Eos); 
               if not Eos then
                  if The_Runner.status(1..7) /= "REMOVED" then
                     The_Bet.Clear;
                     The_Bet := Bets.Create(Side       => Lay,
                                            Name       => Global_Betname,
                                            Size       => Global_Laysize,
                                            Price      => Price_Type(p.Layprice),  
                                            Placed     => The_Market.startts,                  
                                            Runner => The_Runner,
                                            Market => The_Market);

                     The_Bet.Match_Directly(True);
                     The_Bet.Pricematched := p.layprice;
                     Move("MATCHED",The_Bet.Status);
                     The_Bet.Insert;
                     The_Bet.Check_Outcome;
                     The_Bet.Update_Withcheck;               
                     the_bet.read(Eos);
                     the_runner.Handicap := the_bet.Pricematched;
                     Runners_To_Watch_List.Append(The_Runner);
                 -- else
                 --    Log(Me & "Main" , "runner removed: " & The_runner.To_String);                  
                 -- end if;   
               else
                  Log(Me & "Main" , "no such runner: " & The_runner.To_String);                  
               end if;   
            end if;            
          end if;            
         end loop;

         if not Ba_No_Backbet and then Runners_To_Watch_List.Length > 0 then         
            Ts_List.Clear;
            Select_Timestamps.Set("MARKETID", The_Market.Marketid);
            Read_Ts(Select_Timestamps, Ts_List);
            Timestamp_Loop : for Ts of Ts_List loop
               Ph_List.Clear;
               Select_Cand.Set("MARKETID",The_Market.Marketid);
               Select_Cand.Set("PRICETS",Ts);
               Price_Histories.Read_List(Select_Cand, Ph_List);
                 
               declare
                  The_Runner   : Runners.Runner_Type;
                  The_Bet      : Bets.Bet_Type;
                  Eos          : Boolean := False;
                  use Price_Histories;
                  back_bet_size : Bet_Size_Type := 0.0;
               begin
                  for r of Runners_To_Watch_List loop
                     for ph of Ph_List loop
                        if ph.Selectionid = R.Selectionid then
                           if ph.Backprice >= Fixed_Type(1.01) and then
                              ph.Backprice <= Fixed_Type(1000.0) and then
                             -- ph.Backprice >= r.handicap + Fixed_Type(10.0) then -- for greenup
                              ph.Backprice <= Fixed_Type(1.5) then                 -- for bail out
                              
                              back_bet_size := Global_Laysize * Bet_Size_Type(r.handicap/ph.Backprice) ;
                              The_Bet.Clear;
                              The_Runner.Marketid := ph.Marketid;
                              The_Runner.Selectionid := ph.Selectionid;
                              The_runner.read(Eos);
                              if not Eos then                              
                                 The_Bet := Bets.Create(Side       => Back,
                                                        Name       => Global_Betname,
                                                        Size       => back_bet_size,
                                                        Price      => 1.01,  
                                                        Placed     => ph.Pricets,                  
                                                        Runner => The_Runner,
                                                        Market => The_Market);
                                 The_Bet.Match_Directly(False);
                                 The_Bet.Insert;
                                 The_Bet.Check_Matched;
                                 The_Bet.Check_Outcome;
                                 The_Bet.Update_Withcheck;
                                 R.status(1..4) := "KILL";
                                 Log(Me & "Main" , "backbet: " & The_Bet.To_String);                  
                              else
                                 Log(Me & "Main" , "no such runner: " & The_runner.To_String);                  
                              end if;   
                              
                           end if; --Eos
                        end if; -- /= best_runner(i)
                     end loop; --i in best_runners
                  end loop; --r of Runners_to_Watch
                  -- remove runners we backed
                  declare
                     Tmp_List : runners.Lists.list := Runners_To_Watch_List.Copy;
                  begin
                     Runners_To_Watch_List.Clear;
                     for T of Tmp_List loop
                        if T.Status(1..4) /= "KILL" then
                           Runners_To_Watch_List.Append(T);
                        end if;                          
                     end loop;                     
                  end;
                  exit Timestamp_Loop when Runners_To_Watch_List.Length = 0;
               end;
            end loop Timestamp_Loop;
         end if; 
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

end Check_Lay2;
