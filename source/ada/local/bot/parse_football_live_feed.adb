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


procedure Parse_Football_Live_Feed is

  package Ev renames Ada.Environment_Variables;

  Global_Filename : String := "/Users/bnl/svn/botstart/bot-1-0/data/soccer_livescore.dat";
  Global_Url      : String := "https://www.livescore.cz/live-soccer.php?ts=" & Calendar2.Clock.To_String;

  Sa_Par_Bot_User : aliased Gnat.Strings.String_Access;

  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Ba_Daemon       : aliased Boolean := False;
  Cmd_Line        : Command_Line_Configuration;

  Me              : constant String := "parse_Football_Live_Feed.";
  My_Lock         : Lock.Lock_Type;
  Msg             : Process_Io.Message_Type;

  Debug           : Boolean := True;
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


  procedure Retrieve(Url,Filename : String) is
    Arg_List    : Gnat.Os_Lib.Argument_List(1..6);
    Success     : Boolean := False;
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
    Scores : array(Team_Field_Type'Range) of Types.Integer_4;
    Cc    : String(1..2) := "  ";

    Game           : Games.Game_Type;
    Alias          : Aliases.Alias_Type;
    Unknown        : Unknowns.Unknown_Type;
    Highlight_Seen : Boolean := False;

    type State_Type is (Undefined,
                        England_Premier_League,
                        Italy_Serie_A,
                        Italy_Serie_B,
                        Italy_Serie_C,
                        Unsupported,
                        Finished);
    State          : State_Type := Undefined;
    type Agame is record
      Home       : Unbounded_String;
      Away       : Unbounded_String;
      Home_Score : Integer := 0;
      Away_Score : Integer := 0;
      Time       : Integer := 0;
    end record;


    Procedure Determine_State ( Local_State : in out State_Type ; State_Changed : out Boolean) Is
      --Local_State : State_Type := State;
      Old_State   : State_Type := Local_State;
    begin

      if Awk.Number_Of_Fields(Session => Score) >= 5  then

        for I in 1 .. Integer(Awk.Number_Of_Fields(Session => Score)) loop
          D(I'Img & "='" & AWK.Field(Awk.Count(I)) & "'");
        end loop;


        if AWK.Field(2) = "England" and then
          AWK.Field(4) = "Premier" and then
          AWK.Field(5) = "League"  then
          Local_State := England_Premier_League;

        elsif AWK.Field(2) = "England" and then
          AWK.Field(4) = "National" and then
          AWK.Field(5) = "League"  then
          Local_State := Unsupported;

        elsif AWK.Field(2) = "Italy" and then
          AWK.Field(4) = "Serie" then

          declare
            S1 : String := AWK.Field(5);
            S  : String(1..S1'Length) := S1;
          begin
            D("In italy s(1)='" & S(1..1) & "'");
            if S(1) = 'A' then
              Local_State := Italy_Serie_A;
            elsif S(1) = 'B' then
              Local_State := Italy_Serie_B;
            elsif S(1) = 'C' then
              Local_State := Italy_Serie_C;
            else
              Local_State := Unsupported;
            end if;
          end;

          D("identified " &  Local_State'Img);


        elsif  AWK.Field(2) = "Spain"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Germany"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "France"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Netherlands"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Belgium"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Portugal"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Scotland"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Sweden"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Denmark"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Norway"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Iceland"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Slovakia"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Czech"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "South"  then --africa
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Finland"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Austria"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Switzerland"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Turkey"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Cyprus"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Luxembourg"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Russia"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Ukraine"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Poland"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Slovenia"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Romania"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Bulgaria"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Latvia"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Lithuania"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Estonia"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Moldova"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "North"  then --North Macedonia
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Israel"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Mexico"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Japan"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Singapore"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "China"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Republic"  then --Republic of Korea
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Thailand"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Vietnam"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Australia"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Egypt"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Kenya"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Morocco"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Tunisia"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Nigeria"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Niger"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Kazakhstan"  then
          Local_State := Unsupported;
        elsif  AWK.Field(2) = "Click"  then
          Local_State := Finished;
        end if;

      end if;

      if Local_State /= Old_State then
        D("state changed from " & Old_State'Img & " to " & Local_State'Img);
        State_Changed := True;
      else
        State_Changed := False;
      end if;
      D("state is " & Local_State'Img);

    end Determine_State;


    function Parse_Line return AGame is
      Tmp   : Agame;
      -- 1     7   11
      -- 14:00 68' Eintracht Norderstedt 0:2 Hannover 96 II
      -- break it down :
      S1    : String := AWK.Field(0);
      S     : String(1..S1'Length) := S1;
      St    : String := Utils.Trim(S);
      Colon : Integer :=0;
    begin

      D("start parse_line " & St);
      D("start"  & St(1..5));
      D("time " & St(7..8));

      for I in 4 .. St'Last loop
        case St(I) is
          when ':' => Colon := I; exit;
            when others => null;
        end case;
      end loop;

      D("home " & St(11 .. Colon -2 ));
      D("away " & St(Colon +2 .. St'Last ));

      D("homescore " & St(Colon-1));
      D("awayscore " & St(Colon+1));
      return Tmp;


    end Parse_Line;



    G           : Agame;

    State_Changed : Boolean := False;

  begin
    AWK.Set_Current (Score);
    AWK.Open (Separators => "", Filename => Filename);
    while not Awk.End_Of_File loop
      Awk.Get_Line;
      D("-----------------------------------") ;
      Determine_State(State, State_Changed);
      if not State_Changed then   -- when state changed -  no match info
        case State is
          when Undefined              => null;
          when England_Premier_League => G := Parse_Line;
          when Italy_Serie_A          => G := Parse_Line;
          when Italy_Serie_B          => null;
          when Italy_Serie_C          => null;
          when Unsupported            => null;
          when Finished               => exit;
        end case;
      end if;
     -- D(State'Img & " num fields " & Awk.Number_Of_Fields(Session => Score)'img & "'" & AWK.Field(0) & "'");



--        if Awk.Number_Of_Fields(Session => Score) < 2 then
--          if Highlight_Seen then
--            Cc := (others => ' ');
--          end if;
--
--        elsif Awk.Number_Of_Fields(Session => Score) > 3 then
--           for I in 1 .. Awk.Number_Of_Fields(Session => Score) loop
--             D(I'Img & ":" & Awk.Field (i));
--           end loop;
--           D("");
--
--          if Awk.Field(2) = "Belgium" and then Awk.Field(4) = "Jupiler" and then Awk.Field(5) = "League" then
--            Cc :="BE";
--            Highlight_Seen := False;
--          elsif Awk.Field(2) = "Denmark" and then Awk.Field(4) = "Superliga" then
--            Cc :="DK";
--            Highlight_Seen := False;
--          elsif Awk.Field(2) = "England" and then Awk.Field(4) = "Premier" and then Awk.Field(5) = "League" then
--            if Awk.Number_Of_Fields(Session => Score) >= 6 and then Awk.Field(6) = "2" then
--              null;
--            elsif Awk.Number_Of_Fields(Session => Score) >= 6 and then Awk.Field(6) /= "" then
--              null;
--            else
--              Cc :="GB";
--              Highlight_Seen := False;
--            end if;
--          elsif Awk.Field(2) = "Europe" and then Awk.Field(4) = "Champions" and then Awk.Field(5) = "League" then
--            if Awk.Number_Of_Fields(Session => Score) >= 6 and then Awk.Field(6) = "Women" then
--              null;
--            else
--              Cc :="EU";
--              Highlight_Seen := False;
--            end if;
--
--          elsif Awk.Field(2) = "France" and then Awk.Field(4) = "Ligue" and then Awk.Field(5) = "1" then
--            Cc :="FR";
--            Highlight_Seen := False;
--          elsif Awk.Field(2) = "Germany" and then Awk.Field(4) = "Bundesliga" then
--            if Awk.Number_Of_Fields(Session => Score) >= 5 and then Awk.Field(5) = "Women" then
--              null;
--            else
--              Cc :="DE";
--              Highlight_Seen := False;
--            end if;
--          elsif Awk.Field(2) = "Italy" and then Awk.Field(4) = "Serie" and then Awk.Field(5) = "A" then
--            Cc :="IT";
--            Highlight_Seen := False;
--          elsif Awk.Field(2) = "Netherlands" and then Awk.Field(4) = "Eredivisie" then
--            Cc :="NL";
--            Highlight_Seen := False;
--          elsif Awk.Field(2) = "Portugal" and then Awk.Field(4) = "Primeira" and then Awk.Field(5) = "Liga" then
--            Cc :="PT";
--            Highlight_Seen := False;
--          elsif Awk.Field(2) = "Scotland" and then Awk.Field(4) = "Premiership" then
--            Cc :="GB";
--            Highlight_Seen := False;
--          elsif Awk.Field(2) = "Spain" and then Awk.Field(4) = "Laliga" then
--            Cc :="ES";
--            Highlight_Seen := False;
--          elsif Awk.Field(2) = "Sweden" and then Awk.Field(4) = "Allsvenskan" then
--            if Awk.Number_Of_Fields(Session => Score) >= 5 and then Awk.Field(5) = "Women" then
--              null;
--            else
--              Cc :="SE";
--              Highlight_Seen := False;
--            end if;
--
--          elsif Awk.Number_Of_Fields(Session => Score) >= 6 and then Awk.Field(6) = "Highlights!" then
--            Highlight_Seen := True;
--
--          elsif Awk.Field (Football_Fields(1)) = "Football" then
--            for I in Football_Fields(1)+1 .. Awk.Number_Of_Fields(Session => Score) loop
--              D(I'Img & "::" & Awk.Field (I) );
--              if Awk.Field(I) = "Football" then
--                Football_Fields(2) := I;
--              end if;
--
--              declare
--                F : String := Awk.Field(I);
--              begin
--                for J in F'Range loop
--                  case F(J) is
--                    when '[' =>  Score_Field := I;
--                    when others => null;
--                  end case;
--                end loop;
--              end;
--            end loop;
--
--           declare
--             Ftime : String := Awk.Field(2);
--             Is_numeric : Boolean := True;
--             use type Types.Integer_4;
--           begin
--             for I in Ftime'range loop
--                case Ftime(I) is
--                  when ':' =>  -- eg 14:50 -- match start
--                    Game.Minute := -1;
--                    Is_Numeric  := False;
--                    exit;
--                  when 'E' =>  -- eg ET - Extra Time
--                    Game.Minute := -2;
--                    Is_Numeric  := False;
--                    exit;
--                  when 'F' =>  -- eg FT - Full Time
--                    Game.Minute := -3;
--                    Is_Numeric  := False;
--                    exit;
--                  when 'H' =>  -- eg HT - Half Time
--                    Game.Minute := 45;
--                    Is_Numeric  := False;
--                    exit;
--                  when '0' .. '9' => null;
--                  when others =>
--                    Is_Numeric  := False;
--                    exit;
--                end case;
--             end loop;
--             if Is_Numeric then
--               Game.Minute := Types.Integer_4'Value(Ftime);
--             end if;
--           end;
--
--
--
--            if Football_Fields(2) >  Football_Fields(1) then
--              D("-------------------");
--              D( Awk.Field (0));
--              D("-------------------");
--            end if;
--            if Score_Field > 0 then
--              D("---- score----------");
--              D( Awk.Field (Score_Field));
--              D("-------------------");
--              declare
--                Left_Bracket,
--                Dash,
--                Right_Bracket : Integer := 0;
--                F : String := Awk.Field (Score_Field);
--              begin
--                for          begin
            Rpc.Login;
          exception
            when Rpc.Login_Failed =>
              Log(Me, "login failed, but will try again");
          end;
 M in F'Range loop
--                  case F(M) is
--                    when '[' => Left_Bracket := M;
--                    when '-' => Dash := M;
--                    when ']' => Right_Bracket := M;
--                    when others => null;
--                  end case;
--
--                  if Left_Bracket > 0 and then
--                    Dash  > Left_Bracket and then
--                    Right_Bracket > Dash then
--
--                    if F(Left_Bracket+1 ..  Dash-1) = "?" then
--                      Scores(Home) := 0;
--                    else
--                      Scores(Home) := Types.Integer_4'Value(F(Left_Bracket+1 .. Dash-1));
--                    end if;
--
--                    if F(Dash+1 ..  Right_Bracket-1) = "?" then
--                      Scores(Away) := 0;
--                    else
--                      Scores(Away) := Types.Integer_4'Value(F(Dash+1 ..  Right_Bracket-1));
--                    end if;
--                  end if;
--                end          begin
            Rpc.Login;
          exception
            when Rpc.Login_Failed =>
              Log(Me, "login failed, but will try again");
          end;
 loop;
--                for H in Team_Field_Type'Range loop
--                  D("Score " & H'Img & "=" & Scores(H)'Img );
--                end loop;
--              end ;
--            end if;
--
--            for K in Football_Fields(1)+1 .. Score_Field-1 loop
--              Append(Teamnames(Home),Awk.Field(K) & " ");
--            end loop;
--
--            for K in Score_Field+1 .. Football_Fields(2)-1 loop
--              Append(Teamnames(Away),Awk.Field(K) & " ");
--            end loop;
--
--            D("---- Home----------");
--            D( To_String(Teamnames(Home)));
--            D("-------------------");
--
--            D("---- Away----------");
--            D( To_String(Teamnames(Away)));
--            D("-------------------");



      -- here





--            if Cc /= "  " then
--              declare
--                T : Sql.Transaction_Type;
--                type Eos_Type is (Aaliases,Agames,Aunknowns);
--                Eos      : array (Eos_Type'Range) of Boolean := (others => False);
--                Eventid  : array (Team_Field_Type'Range) of Bot_Types.Eventid_Type := (others => (others => ' '));
--                use type Types.Integer_4;
--                Minute : Types.Integer_4 := Game.Minute;
--              begin
--                T.Start;
--                -- check to se if both teams are known
--                for H in Team_Field_Type'Range loop
--                  Move(To_String(Teamnames(H)), Alias.Teamname);
--                  Alias.Read_Teamname(Eos(Aaliases));
--                  if not Eos(Aaliases) then
--                    case H is
--                      when Home =>
--                        Game.Homeid := Alias.Teamid;
--                        Game.Homescore := Scores(Home);
--                        Eventid(Home) := Find_Eventid(To_String(Teamnames(Home)),Cc);
--                      when Away =>
--                        Game.Awayid := Alias.Teamid;
--                        Game.Awayscore := Scores(Away);
--                        Eventid(Away) := Find_Eventid(To_String(teamnames(Away)),Cc);
--                    end case;
--
--                    if Eventid(Home) /= Events.Empty_Data.Eventid and then Eventid(Home) = Eventid(Away) then
--                      --found Event :-)
--                      Game.Eventid := Eventid(Home);
--                      Game.Read(Eos(Agames));
--                      if not Eos(Agames) then
--                        if Game.Homescore /= Scores(Home) or else
--                           Game.AwayScore /= Scores(Away) or else
--                           Game.Minute /= Minute then
--
--                            Game.Homescore := Scores(Home);
--                            Game.AwayScore := Scores(Away);
--                            Game.Minute    := Minute;
--                            Game.Update_Withcheck;
--                        end if;
--                      else
--                        Game.Insert;
--                      end if;
--                    end if;
--                  else
--                    Unknown.Countrycode := Cc;
--                    Unknown.Teamname := Alias.Teamname;
--                    Unknown.Read(Eos(Aunknowns));
--                    if Eos(Aunknowns) then
--                      Put_Line("new unknown found :" & Unknown.To_String);
--                      Unknown.Insert;
--                    end if;
--                  end if;
--                end loop;
--
--                T.Commit;
--              end;
--            end if;
--        end if;
--      end if;
      D("-----------------------------------") ;
    end loop;
    Awk.Close (Score);
  end Parse_File;
  -------------------------------------

  use type Sql.Transaction_Status_Type;
  use type Types.Integer_2;

begin


  Parse_File(Global_Filename);


--     Define_Switch
--      (Cmd_Line,
--       Sa_Par_Bot_User'access,
--       Long_Switch => "--user=",
--       Help        => "user of bot");
--
--     Define_Switch
--       (Cmd_Line,
--        Ba_Daemon'access,
--        Long_Switch => "--daemon",
--        Help        => "become daemon at startup");
--
--     Define_Switch
--       (Cmd_Line,
--        Sa_Par_Inifile'access,
--        Long_Switch => "--inifile=",
--        Help        => "use alternative inifile");
--
--    Getopt (Cmd_Line);  -- process the command line
--
--    if Ba_Daemon then
--      Posix.Daemonize;
--    end if;
--
--    if not EV.Exists("BOT_NAME") then
--      Ev.Set(Name => "BOT_NAME",Value => "live_feed");
--    end if;
--
--     --must take lock AFTER becoming a daemon ...
--     --The parent pid dies, and would release the lock...
--    My_Lock.Take(EV.Value("BOT_NAME"));
--
--    Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");
--
--
--    Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
--    Log(Me, "Connect Db");
--    Sql.Connect
--          (Host     => Ini.Get_Value("database", "host", ""),
--           Port     => Ini.Get_Value("database", "port", 5432),
--           Db_Name  => Ini.Get_Value("database", "name", ""),
--           Login    => Ini.Get_Value("database", "username", ""),
--           Password =>Ini.Get_Value("database", "password", ""));
--    Log(Me, "db Connected");
--
--
--    Main_Loop : loop
--
--      begin
--        Log(Me, "Start receive");
--        Process_Io.Receive(Msg, 10.0);
--        Log(Me, "msg : "& Process_Io.Identity(Msg)'Img & " from " & Trim(Process_Io.Sender(Msg).Name));
--        if Sql.Transaction_Status /= Sql.None then
--          raise Sql.Transaction_Error with "Uncommited transaction in progress !! BAD!";
--        end if;
--        case Process_Io.Identity(Msg) is
--          when Core_Messages.Exit_Message                  =>
--            exit Main_Loop;
--          when others =>
--            Log(Me, "Unhandled message identity: " & Process_Io.Identity(Msg)'Img);  --??
--        end case;
--      exception
--        when Process_Io.Timeout =>
--           Delete_From_Unknown;
--           Retrieve(Url => Global_Url, Filename => Global_Filename);
--           Parse_File(Filename => Global_Filename);
--      end;
--      Now := Calendar2.Clock;
--
--      --restart every day
--      Is_Time_To_Exit := Now.Hour = 01 and then
--                       ( Now.Minute = 00 or Now.Minute = 01) ; -- timeout = 2 min
--
--      exit Main_Loop when Is_Time_To_Exit;
--
--    end loop Main_Loop;
--
--    Log(Me, "Close Db");
--    Sql.Close_Session;
--    Logging.Close;
--    Posix.Do_Exit(0); -- terminate

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


end Parse_Football_Live_Feed;
