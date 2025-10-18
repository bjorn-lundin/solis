with Ada.Containers.Doubly_Linked_Lists;
with  Ada.Environment_Variables;
--with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded;
with Gnatcoll.Json; use Gnatcoll.Json;

--with Ada.Strings ; use Ada.Strings;
--with Ada.Strings.Fixed ; use Ada.Strings.Fixed;
with Ada.Directories;
with Text_Io;

with Gnat.Command_Line; use Gnat.Command_Line;
--with Gnat.Strings;

with Aws;
with Aws.Headers;
--with Aws.Headers.Set;
with Aws.Response;
with Aws.Client;
pragma Elaborate_All (Aws.Headers);

with Gnat; use Gnat;
with Gnat.Awk;

with Sql;
with Calendar2; use Calendar2;
with Logging;               use Logging;
with Ini;
with Types; use Types;
with Bot_Types;
with Stacktrace;
with Table_Amarkets;
with Table_Arunners;

with Table_Apriceshistory;
with Bets;
with Runners;
with Markets;



procedure Ai_Nn_Diff is
  package Ev renames Ada.Environment_Variables;
  Cmd_Line              : Command_Line_Configuration;
  T                     : Sql.Transaction_Type;

  Ba_Layprice         : aliased Boolean := False;
  Ia_Position         : aliased Integer := 0;

  Global_Start_Date    : Time_Type := Time_Type_First;
  pragma Unreferenced(Global_Start_Date);

  Gdebug : Boolean := True;

  Global_Profit : Float := 0.0;

  type R_Type is record
    Runner  : Table_Arunners.Data_Type;
    --  Price   : Table_Aprices.Data_Type;
    History : Table_Apriceshistory.Data_Type;

    Market  : Table_Amarkets.Data_Type;
  end record;

  package R_Pkg is new Ada.Containers.Doubly_Linked_Lists(R_Type);
  package String_Object_List is new Ada.Containers.Doubly_Linked_Lists(String_Object);
  File_List : String_Object_List.List;


  type Odds_Type is (Previous, Current, Diff);
  Odds : array(Odds_Type'Range,1..16) of Float := (others => (others => 0.0));

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
  ------------------------------------------------------------------------------


  procedure Get_Files(Filename_List : in out  String_Object_List.List) is
  --   Dir : String := Ev.Value("BOT_HISTORY") & "/data/ai/pong/lay/win/sample";

    Path1       : String := (if Ba_Layprice then Ev.Value("BOT_HISTORY") & "/data/ai/pong/1st/lay/win/sample" else
                               Ev.Value("BOT_HISTORY") & "/data/ai/pong/1st/back/win/sample") ;
    Path2       : String := (if Ba_Layprice then Ev.Value("BOT_HISTORY") & "/data/ai/pong/2nd/lay/win/sample" else
                               Ev.Value("BOT_HISTORY") & "/data/ai/pong/2nd/back/win/sample") ;

    use Ada.Directories;
    My_Search : Search_Type with Warnings => Off;
    My_Entry  : Directory_Entry_Type;
  begin
    Log("Get_Files", "start");

    case Ia_Position is
      when 1 =>
        Start_Search
          (Search    => My_Search,
           Directory => Path1,
           Pattern   => "*.csv",
           Filter    => (Ordinary_File => True, others => False));
      when 2 =>
        Start_Search
          (Search    => My_Search,
           Directory => Path2,
           Pattern   => "*.csv",
           Filter    => (Ordinary_File => True, others => False));
      when others =>
        raise Constraint_Error with "bad position - not supported" & Ia_Position'Img;
    end case;

    loop
      exit when not More_Entries (My_Search);
      Get_Next_Entry (My_Search, My_Entry);
      declare
        S : String_Object := Create(Full_Name(My_Entry));
      begin
        Filename_List.Append(S);
      end;
    end loop;
    Log("Get_Files", "stop");
    End_Search (My_Search);
  end Get_Files;
  ----------------------------------------


  procedure Parse_File(Filename : String) is
    Do_Bet          : Boolean := False;
    Computer_File   : Awk.Session_Type;
    First           : Boolean := True;
    Cnt             : Integer := 0;
    Profit          : Float   := 0.0;
  begin

    Log("Parse_File", Filename);
    Awk.Set_Current (Computer_File);
    Awk.Open (Separators => ",", Filename => Filename);

    while not Awk.End_Of_File loop
      Awk.Get_Line;
      --declare
      if First then
        for I in 1..16 loop
          Odds(Previous,I) := 0.0;
        end loop;
        Do_Bet := False;
      end if;
      Cnt := Cnt +1;

      declare
        Params        : Json_Value := Create_Object;
        Odds_Diff     : Json_Array := Empty_Array;
        Odds_Curr     : Json_Array := Empty_Array;
      begin

        for I in 1..16 loop
          Odds(Current,I)  := Float'Value(Awk.Field(Awk.Count(I+25)));
          Odds(Diff,I)     := Odds(Current,I) - Odds(Previous,I);
          Odds(Previous,I) := Odds(Current,I);
        end loop;

        for I in 1..16 loop
          Append(Odds_Diff, Create(Odds(Diff,I)));
          Append(Odds_Curr, Create(Odds(Current,I)));
        end loop;

        Params.Set_Field (Field_Name => "diff", Field => Odds_Diff);
        Params.Set_Field (Field_Name => "curr", Field => Odds_Curr);

        declare
          use Bot_Types;
          Winner          : Long_Long_Integer := Long_Long_Integer'Value(Awk.Field(1));
          Lowest_Selid    : Long_Long_Integer := Long_Long_Integer'Value(Awk.Field(8));
          Lowest_Pidx     : Long_Long_Integer := Long_Long_Integer'Value(Awk.Field(9));
          Lowest_Odds     : Float             := Float'Value(Awk.Field(7));
          Ts              : Calendar2.Time_Type := Calendar2.To_Time_Type(Date_And_Time_Str => Awk.Field(58)) ;
          Marketid        : Marketid_Type       := Awk.Field(6);
          Laybet          : Bets.Bet_Type;
          Runner          : Runners.Runner_Type;
          Market          : Markets.Market_Type;
          Betname         : Betname_Type      := (others => ' ');
          Side            : Bet_Side_Type := Lay;
          Eos             : Boolean := False;
        begin

          Params.Set_Field (Field_Name => "winner", Field => Create(Winner));
          Params.Set_Field (Field_Name => "lowest_selid", Field => Create(Lowest_Selid));
          Params.Set_Field (Field_Name => "lowest_pidx", Field => Create(Lowest_Pidx));
          Params.Set_Field (Field_Name => "lowest_odds", Field => Create(Lowest_Odds));
          Params.Set_Field (Field_Name => "pricets", Field => Create(Ts.To_String));

          if Fixed_Type'Value(Awk.Field(7)) > Fixed_Type(1.0) then
            if not First then
              Get_Json_Reply(Params,Do_Bet);
              if Do_Bet then
                Betname(1..23) := "LAY_2nd_AI_0.0001_0.999";
                Market.Marketid := Marketid;
                Market.Read(Eos);
                Runner.Selectionid := Integer_4(Lowest_Selid);

                Laybet := Bets.Create(Name   => Betname,
                                      Side   => Side,
                                      Size   => 30.0,
                                      Price  => Price_Type(Lowest_Odds),
                                      Placed => Ts,
                                      Runner => Runner,
                                      Market => Market);

                case Side is
                  when Back =>
                    if Winner = Lowest_Pidx then -- win
                      Profit :=  0.98 * 30.0 * (Lowest_Odds -1.0);
                      Laybet.Betwon := True;
                    else
                      Profit := -30.0;
                      Laybet.Betwon := False;
                    end if;

                  when Lay =>
                    if Winner = Lowest_Pidx then -- loss
                      Profit :=  -30.0 * (Lowest_Odds -1.0);
                      Laybet.Betwon := False;
                    else
                      Profit := 0.98 * 30.0;
                      Laybet.Betwon := True;
                    end if;
                end case;
                Laybet.Profit := Fixed_Type(Profit);
                Laybet.Insert;

                Global_Profit := Global_Profit + Profit;
              end if;
            end if;
          end if;
        end;
      end;
      First := False;

      exit when Do_Bet;
    end loop;
    Awk.Close (Computer_File);
    Log("Profit", Filename & " -> " & Profit'Img & " / " & Global_Profit'Img);

  end Parse_File;

 ----------------------------------

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

  T.Start;

  Get_Files(Filename_List => File_List);
  for F of File_List loop
    Parse_File(F.Fix_String);
  end loop;

  T.Commit;
  Sql.Close_Session;

exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
end Ai_Nn_Diff;
