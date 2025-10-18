
--with Ada.Characters.Handling;
--with Ada.Directories;
--with Ada.Strings; use Ada.Strings;
--with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Text_Io;
--use Text_Io;


with Types; use Types;
with Calendar2; use Calendar2;

with Stacktrace;
--with Ada.Containers.Doubly_Linked_Lists;
--with Price_Histories;
with Ini;
with Logging;
--use Logging;
with Sql;
with Sim;
with Bot_Types; use Bot_Types;
---with Ada.IO_Exceptions;
with Table_Amap;
with Price_Histories;
with Table_Okmarkets;


with Gnat; use Gnat;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;



procedure Populate_Table_Amap is
  --   package Ad renames Ada.Directories;
  package Ev renames Ada.Environment_Variables;


  T : Sql.Transaction_Type;
  Start_Date                      :          Calendar2.Time_Type := (2019,1,1,0,0,0,0);
  One_Day                         : constant Calendar2.Interval_Type := (1,0,0,0,0);
  Current_Date                    :          Calendar2.Time_Type := Start_Date;
  Stop_Date                       :          Calendar2.Time_Type := (2022,1,1,23,59,59,999);

  Place_Marketid  : Marketid_Type := (others => ' ');
  Plc_Found : Boolean := False;

  -------------------------------------------------------
  procedure Log(A : String ; B : String := "") is
  begin
    Text_IO.Put_Line(A & " , " & B);
  end Log;
  -------------------------------------------------------


  Sa_Start_Date                   : aliased  Gnat.Strings.String_Access;
  Sa_Stop_Date                    : aliased  Gnat.Strings.String_Access;

  Cmd_Line                        :          Command_Line_Configuration;
  Okmarket_Data                   : Table_Okmarkets.Data_Type;
  Eos                             : Boolean := False;


begin

  --   Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
  Logging.Open(Ev.Value("BOT_HOME") & "/log/test_old_stuff.log");
  --  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

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


  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","populate_amap");
  end if;

  if Sa_Start_Date.all /= "" then
    Start_Date := Calendar2.To_Time_Type(Sa_Start_Date.all,"");
  end if;

  if Sa_Stop_Date.all /= "" then
    Stop_Date := Calendar2.To_Time_Type(Sa_Stop_Date.all,"");
  end if;

  Log("main", "params start");
  Log("main", "start_date '" & Sa_Start_Date.all & "'");
  Log("main", "stop_date  '" & Sa_Stop_Date.all & "'");
  Log("main", "params stop");


  Current_Date := Start_Date;

  Date_Loop : loop
    T.Start;
    Log("start fill maps " & Current_Date.String_Date_Iso);
    --    Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse);
    Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse, Rewards => False, Racetimes => False);
    Log("start process maps");

    Loop_Market_Sim    : for Market of Sim.Market_With_Data_List loop

      Okmarket_Data.Marketid := Market.Marketid;
      Okmarket_Data.Read(Eos);
      if Eos then
        Market.Markettype(1..3) := "XXX";
      end if;

      Log("OK Win market: " & Market.To_String );

      if Market.Markettype(1..3) = "WIN" then
        begin
          Place_Marketid := Sim.Win_Place_Map(Market.Marketid);
          Plc_Found := Place_Marketid(1) /= ' ';
          --  Log("Fill_Wints_Placets_Map", Market.Marketid & " -> " & Place_Marketid);
        exception
          when Constraint_Error => Plc_Found := False;
        end;

        if Plc_Found then
          declare
            Amap_Data : Table_Amap.Data_Type;
          begin
            Loop_Timestamp_Sim_Win : for Timestamp_Win of Sim.Marketid_Pricets_Map(Market.Marketid) loop
              begin
              --  Log(Timestamp_Win.To_String & " -> " & Sim.Wints_Placets_Map(Timestamp_Win.To_String));

                declare
                  Ttphm_W :  Sim.Timestamp_To_Prices_History_Maps.Map := Sim.Marketid_Timestamp_To_Prices_History_Map(Market.Marketid);
                  Ttphm_P :  Sim.Timestamp_To_Prices_History_Maps.Map := Sim.Marketid_Timestamp_To_Prices_History_Map(Place_Marketid);
                  Phlw    : Price_Histories.Lists.List := Ttphm_W(Timestamp_Win.To_String);
                  Phlp    : Price_Histories.Lists.List := Ttphm_P(Sim.Wints_Placets_Map(Timestamp_Win.To_String));
                begin
                  Win_Loop: for Rw of Phlw loop
                    --Log(Rw.To_String);
                    Plc_Loop : for Rp of Phlp loop
                      --Log(Rp.To_String);

                      if Rw.Selectionid = Rp.Selectionid then
                        begin
                        Amap_Data := ( Marketidwin => Rw.Marketid,
                                       Marketidplc => Rp.Marketid,
                                       Selectionid => Rw.Selectionid,
                                       Pricetswin  => Rw.Pricets,
                                       Pricetsplc  => Rp.Pricets,
                                       Ixxlupd     => <>,
                                       Ixxluts     => <>);
                          Amap_Data.Insert;
                        exception
                          when Sql.Duplicate_Index =>
                            Log("Duplicate_Index",Amap_Data.To_String);
                           -- exit Loop_Timestamp_Sim_Win;
                         end;

                        exit PLC_Loop;
                      end if;

                    end loop Plc_Loop;


                  end loop Win_Loop;
                end ;

              exception
                when Constraint_Error => null;
              end;
            end loop Loop_Timestamp_Sim_Win;
            --          exception
            --            when Constraint_Error => null;
          end;
        end if;

      end if;
    end loop Loop_Market_Sim;

    T.Commit;
    Current_Date := Current_Date + One_Day;
    exit Date_Loop when Current_Date >= Stop_Date;

  end loop Date_Loop;

exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
end Populate_Table_Amap;
