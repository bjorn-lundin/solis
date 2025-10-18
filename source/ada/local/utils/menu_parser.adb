with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Text_io;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Gnatcoll.JSON ;use Gnatcoll.JSON;

with Utils; use Utils;
with Calendar2; use Calendar2;
with Types; use Types;
with Table_Aokmarkets;
with Sql;
with Lock ;
with Core_Messages;
with Posix;
with Logging; use Logging;
with Stacktrace;
with Ini;
with Rpc;
with Process_IO;

procedure Menu_Parser is
  package EV renames Ada.Environment_Variables;
  --use type Rpc.Result_Type;
  use type Sql.Transaction_Status_Type;
  -----------------
  --function Load_File(Filename : in String) return String is
  --   use Ada.Directories;
  --   File_Size    : constant Natural := Natural (Size (Filename));
  --   subtype JSON_String is String (1 .. File_Size);
  --   type JSON_String_Ptr is access JSON_String;
  --   Content : constant JSON_String_Ptr := new JSON_String;
  --   package File_IO is new Ada.Direct_IO(JSON_String);
  --   File : File_IO.File_Type;
  --begin
  --   File_IO.Open (File => File, Mode => File_IO.In_File, Name => Filename);
  --   File_IO.Read (File => File, Item => Content.all);
  --   File_IO.Close(File => File);
  --   return Content.all;
  --end Load_File;
  --------------------------
  Me                 : constant String := "Menu_Parser.Main";
  Msg                : Process_Io.Message_Type;
  My_Lock            : Lock.Lock_Type;
  Cmd_Line           : Command_Line_Configuration;
  Sa_Par_Bot_User    : aliased Gnat.Strings.String_Access;
  Children           : array (1..7) of JSON_Array := (others => Empty_Array);
  Ba_Daemon          : aliased Boolean := False;
  Delete_Old_Records : Sql.Statement_Type;
  Timeout            : Duration := 1.0; -- changed in main loop
  Ok,
  Is_Time_To_Exit : Boolean := False;
  Now             : Calendar2.Time_Type;

  --------------------------------------------------
  procedure Delete_Old_Markets is
    Rows : Natural := 0;
    T    : Sql.Transaction_Type;
  begin
    -- delete data older than 30 days;
    Delete_Old_Records.Prepare("delete from AOKMARKETS where IXXLUTS < (select current_date - 30)");
    T.Start;
      Delete_Old_Records.Execute(Rows);
      Log("deleted" & Rows'Img & " old rows");
    T.Commit;
  end Delete_Old_Markets;
  ---------------------------------------------------

  procedure Parse_Menu is
    T    : Sql.Transaction_Type;
    Menu : Json_Value;
  begin
    Rpc.Get_Navigation_Data(Menu);
    Log("0 type:" & Menu.Get("type") & " name:" & Menu.Get("name"));
    if Menu.Has_Field("children") then
      Children(1) := Menu.Get("children");
      for i in 1 .. Length(Children(1)) loop
        declare
          Child1 : Json_Value := Get(Children(1),i);
        begin
          if Child1.Has_Field("children") and then Child1.Get("type") = "EVENT_TYPE" and then  Child1.Get("name") ="Soccer" then
            Log("    1 type:" & Child1.Get("type") & " name:" & Child1.Get("name"));
            Children(2) := Child1.Get("children");
            for j in 1 .. Length(Children(2)) loop
              declare
                Child2 : Json_Value := Get(Children(2),j);
                UEFA : Boolean := False;
              begin
                Log(" DEBUG2 2 type:" & Child2.Get("type") & " name:" & Child2.Get("name"));
                if Child2.Has_Field("children") and
                       (
                         (
                             Child2.Get("type") = "GROUP" and
                             ( Child2.Get("name") = "Belgian Soccer" or
                               Child2.Get("name") = "Danish Soccer" or
                               Child2.Get("name") = "Dutch Soccer" or
                               Child2.Get("name") = "English Soccer" or
                               Child2.Get("name") = "German Soccer" or
                               Child2.Get("name") = "Italian Soccer" or
                               Child2.Get("name") = "French Soccer" or --?
                               Child2.Get("name") = "Portuguese Soccer" or
                               Child2.Get("name") = "Spanish Soccer"  or
                               Child2.Get("name") = "Swedish Soccer" )
                         )
                   or
                        (
                          Child2.Get("type") = "EVENT" and
                          Child2.Get("name") = "UEFA Champions League"
                        )
                      )  then
                  UEFA := Child2.Get("name") = "UEFA Champions League"; 
                  Log("        2 type:" & Child2.Get("type") & " name:" & Child2.Get("name"));
                  Children(3) := Child2.Get("children");
                  for k in 1 .. Length(Children(3)) loop
                    declare
                      Child3 : Json_Value := Get(Children(3),k);

                      B : array (1..5) of Boolean := (others => False);
                    begin
                      Log("   DEBUG3  3 type:" & Child3.Get("type") & " name:" & Child3.Get("name"));
---
                      if Child3.Has_Field("children") then
                        B(1) := True;
                        B(2) := Child3.Get("type") = "EVENT";
                        B(3) := (
                                  Child3.Get("name") = "Belgian Jupiler League" or  -- belgien
                                  Child3.Get("name") = "Danish Superliga" or        -- danmark
                                  Child3.Get("name") = "Dutch Eredivisie" or              -- holland
                                  Child3.Get("name") = "Barclays Premier League" or -- england
                                  Child3.Get("name") = "English Premier League" or -- england
                                  Child3.Get("name") = "Bundesliga 1" or            -- tyskland
                                  Child3.Get("name") = "Serie A" or                 -- italien
                                  Child3.Get("name") = "Ligue 1 Orange" or          -- frankrike
                                  Child3.Get("name") = "French Ligue 1" or          --frankrike igen
                                  Child3.Get("name") = "Primeira Liga" or           -- portugal
                                  Child3.Get("name") = "Allsvenskan" or             -- sverige
                                  Child3.Get("name") = "Primera Division"      -- spanien
                               );
                        --B(4) := Child3.Get("type") = "EVENT";
                        B(4) := Child3.Get("type") = "GROUP";
                        declare
                          S : String := Child3.Get("name");
                        begin  
                         -- B(5) := (S'length > 5 and then S(1..5) = "Group");
                          B(5) := (S'length >= 8 and then S(1..8) = "Fixtures");
                        end; 
                      end if;
---
                      if B(1) and then
                         (
                           (B(2) and B(3))
                               or
                           (B(4) and B(5))
                         ) 
                            then

                        Log("            3 type:" & Child3.Get("type") & " name:" & Child3.Get("name"));
                        Children(4) := Child3.Get("children");
                        for l in 1 .. Length(Children(4)) loop
                          declare
                            Child4 : Json_Value := Get(Children(4),l);
                            Name   : String     := Child4.Get("name");
                          begin
                            --if Child4.Has_Field("children")  then
                            --  Log("                4 type:" & Child4.Get("type") & " name:" & Name);
                            --                            
                            --   --UEFA is EVENT here
                            if Child4.Has_Field("children") and
                               (
                                 (
                                    Child4.Get("type") = "GROUP" and then
                                    Name'length >= 8 and then
                                    Name(1..8) = "Fixtures"
                                 )
                               or else
                                 (
                                   UEFA and then
                                   Child4.Get("type") = "EVENT"                               
                                 )
                               )
                            then                               
                              --Log("                4 type:" & Child4.Get("type") & " name:" & Name);
                              Children(5) := Child4.Get("children");
                              Log("                4 type:" & Child4.Get("type") & " name:" & Name & "len children" & Length(Children(5))'Img);
                              for m in 1 .. Length(Children(5)) loop
                                declare
                                  Child5    : Json_Value := Get(Children(5),m);
                                  Ok_Market : Table_Aokmarkets.Data_Type;
                                begin
                                  if Uefa then -- 1 level higher :-(
                                
                                  Log("                    5.5 type:" & Child5.Get("type") & " name:" & Child5.Get("name") & " has child " & Child5.Has_Field("children")'img);
                                  Log("                    5.5 id:" & Child5.Get("id") & 
                                                                 " exch:" & Child5.Get("exchangeId") & 
                                                                 " marketType" & Child5.Get("marketType"));
                                    begin
                                        --Log("                        6 type:" & Child6.Get("type") & " name:" & Child6.Get("name"));
                                      if Child5.Get("type") = "MARKET" and then
                                         Child5.Get("exchangeId") = "1" and then
                                         (Child5.Get("marketType") = "MATCH_ODDS" or
                                          Child5.Get("marketType") = "CORRECT_SCORE" ) then
                                        Log("                        5.5 id:" & Child5.Get("id") & " name:" & Child5.Get("name"));
                                        -- do stuff here
                                        --insert into new table
                                        Move(Child5.Get("id"),Ok_Market.Marketid);
                                        Move(Child5.Get("marketType"),Ok_Market.Markettype);
                                        declare
                                          Eos : Boolean := False;
                                        begin
                                          T.Start;
                                          Ok_Market.Read(Eos);
                                          if Eos then
                                            Log(Me,"will insert" & Ok_Market.To_String);
                                            Ok_Market.Insert;
                                            Log(Me,"did  insert" & Ok_Market.To_String);
                                          end if;
                                          T.Commit;
                                        exception
                                          when Sql.Duplicate_Index =>
                                            T.Rollback;
                                        end;
                                      end if;
                                    end;
                                  
                                  elsif Child5.Has_Field("children") then
                                    Log("                    5 type:" & Child5.Get("type") & " name:" & Child5.Get("name"));
                                    Move(Child5.Get("id"),Ok_Market.Eventid); -- eventid
                                    Children(6) := Child5.Get("children");
                                    for n in 1 .. Length(Children(6)) loop
                                      declare
                                        Child6 : Json_Value := Get(Children(6),n);
                                      begin
                                          --Log("                        6 type:" & Child6.Get("type") & " name:" & Child6.Get("name"));
                                        if Child6.Get("type") = "MARKET" and then
                                           Child6.Get("exchangeId") = "1" and then
                                           (Child6.Get("marketType") = "MATCH_ODDS" or
                                            Child6.Get("marketType") = "CORRECT_SCORE" ) then
                                          Log("                        6 id:" & Child6.Get("id") & " name:" & Child6.Get("name"));
                                          -- do stuff here
                                          --insert into new table
                                          Move(Child6.Get("id"),Ok_Market.Marketid);
                                          Move(Child6.Get("marketType"),Ok_Market.Markettype);
                                          declare
                                            Eos : Boolean := False;
                                          begin
                                            T.Start;
                                            Ok_Market.Read(Eos);
                                            if Eos then
                                              Log(Me,"will insert" & Ok_Market.To_String);
                                              Ok_Market.Insert;
                                              Log(Me,"did  insert" & Ok_Market.To_String);
                                            end if;
                                            T.Commit;
                                          exception
                                            when Sql.Duplicate_Index =>
                                              T.Rollback;
                                          end;
                                        end if;
                                      end;
                                    end loop;
                                  end if;
                                end;
                              end loop;
                            end if;
                          end;
                        end loop;
                      end if;
                    end;
                  end loop;
                end if;
              end;
            end loop;
          end if;
        end;
      end loop;
    end if;
  exception
    when Rpc.Bad_Reply =>
      Log("Parse_Menu", "caught Rpc.Bad_Reply");
  end Parse_Menu;
  -----------------------------------------------------------------------
begin

   Define_Switch
    (Cmd_Line,
     Sa_Par_Bot_User'access,
     Long_Switch => "--user=",
     Help        => "user of bot");

  Define_Switch
    (Cmd_Line,
     Ba_Daemon'access,
     "-d",
     Long_Switch => "--daemon",
     Help        => "become daemon at startup");

  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
     Ev.Set("BOT_NAME","menu_parser");
  end if;

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Logging.Open(EV.Value("BOT_HOME") & "/log/menu_parser.log");
  if Ba_Daemon then
     Posix.Daemonize;
  end if;
   --must take lock AFTER becoming a daemon ...
   --The parent pid dies, and would release the lock...
  My_Lock.Take(EV.Value("BOT_NAME"));

  Rpc.Init(
           Username   => Ini.Get_Value("betfair","username",""),
           Password   => Ini.Get_Value("betfair","password",""),
           Product_Id => Ini.Get_Value("betfair","product_id",""),
           Vendor_Id  => Ini.Get_Value("betfair","vendor_id",""),
           App_Key    => Ini.Get_Value("betfair","appkey","")
         );
  Rpc.Login;
  Log(Me, "Login betfair done");
  Sql.Connect
       (Host     => Ini.Get_Value("database","host",""),
        Port     => Ini.Get_Value("database","port",5432),
        Db_Name  => Ini.Get_Value("database","name",""),
        Login    => Ini.Get_Value("database","username",""),
        Password => Ini.Get_Value("database","password",""));

  Delete_Old_Markets;
  Timeout := 1.0;
  Main_Loop : loop
    begin
      Log(Me, "Start receive");
      if Sql.Transaction_Status /= Sql.None then
        raise Sql.Transaction_Error with "Uncommited transaction in progress !! BAD!";
      end if;
      Process_Io.Receive(Msg, Timeout);
      Log(Me, "msg : "& Process_Io.Identity(Msg)'Img & " from " & Trim(Process_Io.Sender(Msg).Name));
      case Process_Io.Identity(Msg) is
        when Core_Messages.Exit_Message                  =>
          exit Main_Loop;
        when others =>
          Log(Me, "Unhandled message identity: " & Process_Io.Identity(Msg)'Img);  --??
      end case;
    exception
      when Process_Io.Timeout =>
        Now := Calendar2.Clock;
        Timeout := 3_600.0;
        Rpc.Keep_Alive(OK);
        if not OK then
          Rpc.Login;
        end if;
        Parse_Menu;
    end;
    Now := Calendar2.Clock;
    --restart every day
    Is_Time_To_Exit := Now.Hour <= 2 ; -- timeout = 3600 s min

    exit Main_Loop when Is_Time_To_Exit;
  end loop Main_Loop;

  Rpc.Logout;
  Sql.Close_Session;

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

end Menu_Parser;
