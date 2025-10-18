with Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;

with Gnat; use Gnat;
with Gnat.Awk;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Gnat.Os_Lib;

with Logging; use Logging;
with Stacktrace;
with Calendar2;
with Ini;
with Sql;
with Lock;
with Posix;
with Utils; use Utils;
with Process_Io;
with Core_Messages;
with Types;
with Bot_Types;

with Games;
with Aliases;
with Unknowns;
with Events;


procedure Football_Live_Feed is

  package Ev renames Ada.Environment_Variables;

  Global_Filename : String := "/home/bnl/tmp/score2.dat";
  Global_Url : String := "http://www.goalserve.com/updaters/soccerupdate.aspx?ts=" & Calendar2.Clock.To_String;

  Sa_Par_Bot_User : aliased Gnat.Strings.String_Access;
  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Ba_Daemon       : aliased Boolean := False;
  Cmd_Line        : Command_Line_Configuration;

  Me              : constant String := "Poll_Market.";
  My_Lock         : Lock.Lock_Type;
  Msg             : Process_Io.Message_Type;

  Debug : Boolean := False;
  Now             : Calendar2.Time_Type;
  Is_Time_To_Exit : Boolean := False;

  Select_Event   : Sql.Statement_Type;
  Delete_Unknown : Sql.Statement_Type;
  ---------------------------------------------
  procedure D(What : String) is
  begin
    if Debug then
      Put_Line(What);
    end if;
  end D;

  function Find_Eventid(Teamname,Countrycode : String) return Bot_Types.Eventid_Type is
    Local_Event : Events.Event_Type;
    T           : Sql.Transaction_Type;
    Now         : Calendar2.Time_Type := Calendar2.Clock;
    Eos         : Boolean := False;
  begin
    Now.Hour := 1; -- make sure we get time early in the day
    T.Start;
    Select_Event.Prepare(
       "select E.* " &
       "from ARUNNERS R, AMARKETS M, AEVENTS E " &
       "where R.MARKETID = M.MARKETID " &
       "and M.EVENTID = E.EVENTID " &
       "and M.MARKETTYPE = 'MATCH_ODDS' " &
       "and E.COUNTRYCODE >= :COUNTRYCODE1 " &
       "and E.COUNTRYCODE <= :COUNTRYCODE2 " &
       "and R.RUNNERNAME in ( " &
         "select TEAMNAME from AALIASES where TEAMID in ( " &
             "select TEAMID from AALIASES where TEAMNAME = :TEAMNAME)) " &
       "and M.STARTTS >= :THISMORNING"
     );

    Select_Event.Set("TEAMNAME",Teamname);
    Select_Event.Set_Timestamp("THISMORNING",Now);
    if Countrycode = "EU" then
      Select_Event.Set("COUNTRYCODE1","AA");
      Select_Event.Set("COUNTRYCODE2","ZZ");
    else
      Select_Event.Set("COUNTRYCODE1",Countrycode);
      Select_Event.Set("COUNTRYCODE2",Countrycode);
    end if;
    Select_Event.Open_Cursor;
    Select_Event.Fetch(Eos);
    if not Eos then
      Local_Event := Events.Get(Select_Event);
    end if;
    Select_Event.Close_Cursor;

    T.Commit;

    Log("Find_Eventid","name: '" & Teamname &
                       "' CC '" & Countrycode &
                       "' Eos " & Eos'Img &
                       " returning '" & Local_Event.Eventid & "'");

    return Local_Event.Eventid;
  end Find_Eventid;



  procedure Delete_From_Unknown is
    T: Sql.Transaction_Type;
    Rows : Natural := 0;
  begin
    T.Start;
      Delete_Unknown.Prepare (
        "delete from AUNKNOWNS where exists (" &
           "select 'x' from AALIASES where AALIASES.TEAMNAME=AUNKNOWNS.TEAMNAME)"
      );
      Delete_Unknown.Execute(Rows);
    T.Commit;
    Log("Delete_From_Unknown", "deleted" & Rows'Img);
  end Delete_From_Unknown;
  --------------------------------------------

  procedure Retrieve(Url,Filename : String) is
    Arg_List : Gnat.Os_Lib.Argument_List(1..6);
    Success : Boolean := False;
    Return_Code : Integer := 0;
  begin
    Arg_List(1) := new String'("-dump");
    Arg_List(2) := new String'("--image_links=0");
    Arg_List(3) := new String'("--hiddenlinks=ignore");
    Arg_List(4) := new String'("--notitle");
    Arg_List(5) := new String'("--nolist");
    Arg_List(6) := new String'(Url);

    Gnat.Os_Lib.Spawn(Program_Name => "/usr/bin/lynx",
                      Args         => Arg_List,
                      Output_File  => Filename,
                      Success      => Success,
                      Return_Code  => Return_Code,
                      Err_To_Out   => True);

    for I in Arg_List'Range loop
      Gnat.Os_Lib.Free(X => Arg_List(I));
    end loop;


  end Retrieve;
--  http://www.goalserve.com/updaters/soccerupdate.aspx

  procedure Parse_File(Filename : String) is
    Score : AWK.Session_Type;
    use type Awk.Count;
    Football_Fields : array(1..2) of Awk.Count := (3,0);
    Score_Field : Awk.Count := 0;
    type Team_Field_Type is (Home,Away);
    Teamnames : array(Team_Field_Type'Range) of Unbounded_String;
    Scores : array(Team_Field_Type'Range) of Types.integer_4;
    Cc : String(1..2) := "  ";

    Game           : Games.Game_Type;
    Alias          : Aliases.Alias_Type;
    Unknown        : Unknowns.Unknown_Type;
    Highlight_Seen : Boolean := False;

  begin
    AWK.Set_Current (Score);
    AWK.Open (Separators => "", Filename => Filename);
    while not Awk.End_Of_File loop
      Awk.Get_Line;
      D("--------------------------------------------");
      Football_Fields(2) := 0;
      Score_Field := 0;
      for N of Teamnames loop
        N := Null_Unbounded_String;
      end loop;


      if Awk.Number_Of_Fields(Session => Score) < 2 then
        if Highlight_Seen then
          Cc := (others => ' ');
        end if;

      elsif Awk.Number_Of_Fields(Session => Score) > 3 then
         for I in 1 .. Awk.Number_Of_Fields(Session => Score) loop
           D(I'Img & ":" & Awk.Field (i));
         end loop;
         D("");

        if Awk.Field(2) = "Belgium" and then Awk.Field(4) = "Jupiler" and then Awk.Field(5) = "League" then
          Cc :="BE";
          Highlight_Seen := False;
        elsif Awk.Field(2) = "Denmark" and then Awk.Field(4) = "Superliga" then
          Cc :="DK";
          Highlight_Seen := False;
        elsif Awk.Field(2) = "England" and then Awk.Field(4) = "Premier" and then Awk.Field(5) = "League" then
          if Awk.Number_Of_Fields(Session => Score) >= 6 and then Awk.Field(6) = "2" then
            null;
          elsif Awk.Number_Of_Fields(Session => Score) >= 6 and then Awk.Field(6) /= "" then
            null;
          else
            Cc :="GB";
            Highlight_Seen := False;
          end if;
        elsif Awk.Field(2) = "Europe" and then Awk.Field(4) = "Champions" and then Awk.Field(5) = "League" then
          if Awk.Number_Of_Fields(Session => Score) >= 6 and then Awk.Field(6) = "Women" then
            null;
          else
            Cc :="EU";
            Highlight_Seen := False;
          end if;

        elsif Awk.Field(2) = "France" and then Awk.Field(4) = "Ligue" and then Awk.Field(5) = "1" then
          Cc :="FR";
          Highlight_Seen := False;
        elsif Awk.Field(2) = "Germany" and then Awk.Field(4) = "Bundesliga" then
          if Awk.Number_Of_Fields(Session => Score) >= 5 and then Awk.Field(5) = "Women" then
            null;
          else
            Cc :="DE";
            Highlight_Seen := False;
          end if;
        elsif Awk.Field(2) = "Italy" and then Awk.Field(4) = "Serie" and then Awk.Field(5) = "A" then
          Cc :="IT";
          Highlight_Seen := False;
        elsif Awk.Field(2) = "Netherlands" and then Awk.Field(4) = "Eredivisie" then
          Cc :="NL";
          Highlight_Seen := False;
        elsif Awk.Field(2) = "Portugal" and then Awk.Field(4) = "Primeira" and then Awk.Field(5) = "Liga" then
          Cc :="PT";
          Highlight_Seen := False;
        elsif Awk.Field(2) = "Scotland" and then Awk.Field(4) = "Premiership" then
          Cc :="GB";
          Highlight_Seen := False;
        elsif Awk.Field(2) = "Spain" and then Awk.Field(4) = "Laliga" then
          Cc :="ES";
          Highlight_Seen := False;
        elsif Awk.Field(2) = "Sweden" and then Awk.Field(4) = "Allsvenskan" then
          if Awk.Number_Of_Fields(Session => Score) >= 5 and then Awk.Field(5) = "Women" then
            null;
          else
            Cc :="SE";
            Highlight_Seen := False;
          end if;

        elsif Awk.Number_Of_Fields(Session => Score) >= 6 and then Awk.Field(6) = "Highlights!" then
          Highlight_Seen := True;

        elsif Awk.Field (Football_Fields(1)) = "Football" then
          for I in Football_Fields(1)+1 .. Awk.Number_Of_Fields(Session => Score) loop
            D(I'Img & "::" & Awk.Field (I) );
            if Awk.Field(I) = "Football" then
              Football_Fields(2) := I;
            end if;

            declare
              F : String := Awk.Field(I);
            begin
              for J in F'Range loop
                case F(J) is
                  when '[' =>  Score_Field := I;
                  when others => null;
                end case;
              end loop;
            end;
          end loop;

         declare
           Ftime : String := Awk.Field(2);
           Is_numeric : Boolean := True;
           use type Types.Integer_4;
         begin
           for I in Ftime'range loop
              case Ftime(I) is
                when ':' =>  -- eg 14:50 -- match start
                  Game.Minute := -1;
                  Is_Numeric  := False;
                  exit;
                when 'E' =>  -- eg ET - Extra Time
                  Game.Minute := -2;
                  Is_Numeric  := False;
                  exit;
                when 'F' =>  -- eg FT - Full Time
                  Game.Minute := -3;
                  Is_Numeric  := False;
                  exit;
                when 'H' =>  -- eg HT - Half Time
                  Game.Minute := 45;
                  Is_Numeric  := False;
                  exit;
                when '0' .. '9' => null;
                when others =>
                  Is_Numeric  := False;
                  exit;
              end case;
           end loop;
           if Is_Numeric then
             Game.Minute := Types.Integer_4'Value(Ftime);
           end if;
         end;



          if Football_Fields(2) >  Football_Fields(1) then
            D("-------------------");
            D( Awk.Field (0));
            D("-------------------");
          end if;
          if Score_Field > 0 then
            D("---- score----------");
            D( Awk.Field (Score_Field));
            D("-------------------");
            declare
              Left_Bracket,
              Dash,
              Right_Bracket : Integer := 0;
              F : String := Awk.Field (Score_Field);
            begin
              for M in F'Range loop
                case F(M) is
                  when '[' => Left_Bracket := M;
                  when '-' => Dash := M;
                  when ']' => Right_Bracket := M;
                  when others => null;
                end case;

                if Left_Bracket > 0 and then
                  Dash  > Left_Bracket and then
                  Right_Bracket > Dash then

                  if F(Left_Bracket+1 ..  Dash-1) = "?" then
                    Scores(Home) := 0;
                  else
                    Scores(Home) := Types.Integer_4'Value(F(Left_Bracket+1 .. Dash-1));
                  end if;

                  if F(Dash+1 ..  Right_Bracket-1) = "?" then
                    Scores(Away) := 0;
                  else
                    Scores(Away) := Types.Integer_4'Value(F(Dash+1 ..  Right_Bracket-1));
                  end if;
                end if;
              end loop;
              for H in Team_Field_Type'Range loop
                D("Score " & H'Img & "=" & Scores(H)'Img );
              end loop;
            end ;
          end if;

          for K in Football_Fields(1)+1 .. Score_Field-1 loop
            Append(Teamnames(Home),Awk.Field(K) & " ");
          end loop;

          for K in Score_Field+1 .. Football_Fields(2)-1 loop
            Append(Teamnames(Away),Awk.Field(K) & " ");
          end loop;

          D("---- Home----------");
          D( To_String(Teamnames(Home)));
          D("-------------------");

          D("---- Away----------");
          D( To_String(Teamnames(Away)));
          D("-------------------");

          if Cc /= "  " then
            declare
              T : Sql.Transaction_Type;
              type Eos_Type is (Aaliases,Agames,Aunknowns);
              Eos      : array (Eos_Type'Range) of Boolean := (others => False);
              Eventid  : array (Team_Field_Type'Range) of Bot_Types.Eventid_Type := (others => (others => ' '));
              use type Types.Integer_4;
              Minute : Types.Integer_4 := Game.Minute;
            begin
              T.Start;
              -- check to se if both teams are known
              for H in Team_Field_Type'Range loop
                Move(To_String(Teamnames(H)), Alias.Teamname);
                Alias.Read_Teamname(Eos(Aaliases));
                if not Eos(Aaliases) then
                  case H is
                    when Home =>
                      Game.Homeid := Alias.Teamid;
                      Game.Homescore := Scores(Home);
                      Eventid(Home) := Find_Eventid(To_String(Teamnames(Home)),Cc);
                    when Away =>
                      Game.Awayid := Alias.Teamid;
                      Game.Awayscore := Scores(Away);
                      Eventid(Away) := Find_Eventid(To_String(teamnames(Away)),Cc);
                  end case;

                  if Eventid(Home) /= Events.Empty_Data.Eventid and then Eventid(Home) = Eventid(Away) then
                    --found Event :-)
                    Game.Eventid := Eventid(Home);
                    Game.Read(Eos(Agames));
                    if not Eos(Agames) then
                      if Game.Homescore /= Scores(Home) or else
                         Game.AwayScore /= Scores(Away) or else
                         Game.Minute /= Minute then

                          Game.Homescore := Scores(Home);
                          Game.AwayScore := Scores(Away);
                          Game.Minute    := Minute;
                          Game.Update_Withcheck;
                      end if;
                    else
                      Game.Insert;
                    end if;
                  end if;
                else
                  Unknown.Countrycode := Cc;
                  Unknown.Teamname := Alias.Teamname;
                  Unknown.Read(Eos(Aunknowns));
                  if Eos(Aunknowns) then
                    Put_Line("new unknown found :" & Unknown.To_String);
                    Unknown.Insert;
                  end if;
                end if;
              end loop;

              T.Commit;
            end;
          end if;
        end if;
      end if;
    end loop;
    Awk.Close (Score);
  end Parse_File;
  -------------------------------------

  use type Sql.Transaction_Status_Type;
  use type Types.Integer_2;

begin

   Define_Switch
    (Cmd_Line,
     Sa_Par_Bot_User'access,
     Long_Switch => "--user=",
     Help        => "user of bot");

   Define_Switch
     (Cmd_Line,
      Ba_Daemon'access,
      Long_Switch => "--daemon",
      Help        => "become daemon at startup");

   Define_Switch
     (Cmd_Line,
      Sa_Par_Inifile'access,
      Long_Switch => "--inifile=",
      Help        => "use alternative inifile");

  Getopt (Cmd_Line);  -- process the command line

  if Ba_Daemon then
    Posix.Daemonize;
  end if;

  if not EV.Exists("BOT_NAME") then
    Ev.Set(Name => "BOT_NAME",Value => "live_feed");
  end if;

   --must take lock AFTER becoming a daemon ...
   --The parent pid dies, and would release the lock...
  My_Lock.Take(EV.Value("BOT_NAME"));

  Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");


  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log(Me, "Connect Db");
  Sql.Connect
        (Host     => Ini.Get_Value("database", "host", ""),
         Port     => Ini.Get_Value("database", "port", 5432),
         Db_Name  => Ini.Get_Value("database", "name", ""),
         Login    => Ini.Get_Value("database", "username", ""),
         Password =>Ini.Get_Value("database", "password", ""));
  Log(Me, "db Connected");


  Main_Loop : loop

    begin
      Log(Me, "Start receive");
      Process_Io.Receive(Msg, 10.0);
      Log(Me, "msg : "& Process_Io.Identity(Msg)'Img & " from " & Trim(Process_Io.Sender(Msg).Name));
      if Sql.Transaction_Status /= Sql.None then
        raise Sql.Transaction_Error with "Uncommited transaction in progress !! BAD!";
      end if;
      case Process_Io.Identity(Msg) is
        when Core_Messages.Exit_Message                  =>
          exit Main_Loop;
        when others =>
          Log(Me, "Unhandled message identity: " & Process_Io.Identity(Msg)'Img);  --??
      end case;
    exception
      when Process_Io.Timeout =>
         Delete_From_Unknown;
         Retrieve(Url => Global_Url, Filename => Global_Filename);
         Parse_File(Filename => Global_Filename);
    end;
    Now := Calendar2.Clock;

    --restart every day
    Is_Time_To_Exit := Now.Hour = 01 and then
                     ( Now.Minute = 00 or Now.Minute = 01) ; -- timeout = 2 min

    exit Main_Loop when Is_Time_To_Exit;

  end loop Main_Loop;

  Log(Me, "Close Db");
  Sql.Close_Session;
  Logging.Close;
  Posix.Do_Exit(0); -- terminate

exception
  when Lock.Lock_Error =>
    Log(Me, "lock error, exit");
    Logging.Close;
    Posix.Do_Exit(0); -- terminate
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


end Football_Live_Feed;
