
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

procedure Check_Lay is
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
   Global_Laysize   : Bet_Size_Type := 50.0;

   SA_Max_Price     : aliased Gnat.Strings.String_Access;
   SA_Min_Price     : aliased Gnat.Strings.String_Access;
   Sa_Betname       : aliased Gnat.Strings.String_Access;
  
  -- IA_Runners_Place : aliased Integer := 0;
  -- IA_Addon_Odds    : aliased Integer := 0;
  
   --Bad_Runners_Place : exception;

   package Ts_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Calendar2.Time_Type);
   Ts_List             : Ts_List_Pack.List;

   
   function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
   begin
      return Left.Backprice < Right.Backprice;
   end "<";
   --------------------------------------------
   package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");
   
   
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
      
--     Define_Switch
--       (Cmd_Line,
--        IA_Runners_Place'access,
--        Long_Switch => "--runners_place=",
--        Help        => "Runners place in race (1-50)");
--  
--     Define_Switch
--       (Cmd_Line,
--        IA_Addon_Odds'access,
--        Long_Switch => "--addon_odds=",
--        Help        => "Runners place in race (1-50)");

      
      
   Getopt (Cmd_Line);  -- process the command line

   Move(SA_Betname.all,Global_Betname);
   Global_Max_Price := Fixed_Type'Value(SA_Max_Price.all);
   Global_Min_Price := Fixed_Type'Value(SA_Min_Price.all);
  
--     case IA_Runners_Place is
--        when 1 .. 50 => null;
--        when others => raise Bad_Runners_Place with IA_Runners_Place'Img;
--     end case;  
  
 
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
      Market_List : Markets.Lists.List;
      Cnt         : Natural := 0;
      --type Has_Type is (Lay);
      subtype Max_Runners_Type is Integer range 1 .. 50;
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
         Ts_List.Clear;
         Select_Timestamps.Set("MARKETID", The_Market.Marketid);
         Read_Ts(Select_Timestamps, Ts_List);
         if Ts_List.Length > 3 then 
            Timestamp_Loop : for Ts of Ts_List loop
               Ph_List.Clear;
               Select_Cand.Set("MARKETID",The_Market.Marketid);
               Select_Cand.Set("PRICETS",Ts);
               Price_Histories.Read_List(Select_Cand, Ph_List);
                 
               declare
                  Idx : Integer := 0;
                  type Best_Runners_Array_Type is array (Max_Runners_Type'range) of Price_Histories.Price_History_Type ;
                  Best_Runners : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);
                  The_Runner   : Runners.Runner_Type;
                  Eos          : Boolean := False;
                  Eos2         : Boolean := False;
                  The_Bet      : Bets.Bet_Type;
                  use Price_Histories;
                  Price        : Prices.Price_Type;
                  Start_Bets_OK : Boolean := False;
               begin
               
                  Backprice_Sorter.Sort(Ph_List);               
               
                  Ph_Loop : for Ph of Ph_List loop
                     Idx := Idx +1;
                     exit Ph_Loop when Idx > Best_Runners'Last;
                     if idx <=3 then
                        Best_Runners(Idx) := Price_Histories.Empty_Data;
                     else   
                        Best_Runners(Idx) := Ph;
                     end if;
                  end loop Ph_Loop;
                  -- Best_Runners is sorted lowest backprice to highest, max 20 entries

                  for i in Best_Runners'range loop
                     if Best_Runners(i) /=  Price_Histories.Empty_Data then

                        The_Bet.Marketid := Best_Runners(i).Marketid;
                        The_Bet.Selectionid := Best_Runners(i).Selectionid;
                        if not The_bet.Is_Existing_Marketid_Selectionid then

                           
                           Price.Marketid    := Best_Runners(i).Marketid;
                           Price.Selectionid := Best_Runners(i).Selectionid;
                           Price.Read(Eos2);
                           if not Eos2 then
                              Start_Bets_OK := Fixed_Type(15.0) <= Price.Backprice and then
                                                       Price.Backprice <= Fixed_Type(50.0);
                           else
                              Start_Bets_OK := False;
                           end if;
                           
                           The_Runner := Runners.Empty_Data;
                           The_Runner.Marketid    := Best_Runners(i).Marketid;
                           The_Runner.Selectionid := Best_Runners(i).Selectionid;
                           The_Runner.Read(Eos);
                           The_Bet.Clear;

                           if not Eos and then
                             Start_Bets_OK and then
                             Best_Runners(i).Backprice >  Fixed_Type(1.01) and then
                             Best_Runners(i).Backprice <  Fixed_Type(1000.0) and then
                             Best_Runners(i).Layprice  <  Global_Max_Price and then
                             Best_Runners(i).Layprice  >= Global_Min_Price then
  
                              The_Bet := Bets.Create(Side       => Lay,
                                                     Name       => Global_Betname,
                                                     Size       => Global_Laysize,
                                                     Price      => Price_Type(Global_Max_Price),  
                                                     Placed     => Best_Runners(i).Pricets,                  
                                                     Runner => The_Runner,
                                                     Market => The_Market);

                              The_Bet.Match_Directly(True);
                              The_Bet.Insert;
                              The_Bet.Check_Matched;
                              The_Bet.Check_Outcome;
                              The_Bet.Update_Withcheck;
                              -- do the Backbet
                              The_Bet.Clear;
                              The_Bet := Bets.Create(Side       => Back,
                                                     Name       => Global_Betname,
                                                     Size       => Global_Laysize - 10.0,
                                                     Price      => Best_Runners(i).Backprice + 4.0,  
                                                     Placed     => Best_Runners(i).Pricets,                  
                                                     Runner => The_Runner,
                                                     Market => The_Market);

                              The_Bet.Match_Directly(False);
                              The_Bet.Insert;
                              The_Bet.Check_Matched;
                              The_Bet.Check_Outcome;
                              The_Bet.Update_Withcheck;
                              
                           end if; --is existing      
                        end if; --Eos
                     end if; -- /= best_runner(i)
                  end loop; --i in best_runners
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

end Check_Lay;
