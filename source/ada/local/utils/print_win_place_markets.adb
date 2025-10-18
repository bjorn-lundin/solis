
with Types; use Types;

--with Gnat; use Gnat;
with Text_Io; use Text_Io;
with Stacktrace;
--with Ada.Containers.Doubly_Linked_Lists;
--with Gnat.Command_Line; use Gnat.Command_Line;
--with Gnat.Strings;
with Ada.Environment_Variables;
with Ini;
with Logging; use Logging;
with Sql;
with Bot_Types; use Bot_Types;
with Markets;

procedure Print_Win_Place_Markets is
  package Ev renames Ada.Environment_Variables;


  --  Sa_Logfilename      : aliased  Gnat.Strings.String_Access;
  Path                :          String := Ev.Value("BOT_HISTORY") & "/data/ai/";
  Race                :          Text_Io.File_Type;
  --  Cmd_Line            :          Command_Line_Configuration;
  Marketname          :          String_Object;
  T                   :          Sql.Transaction_Type;
  Found               : Boolean := False;
  Other_Market        : Markets.Market_Type;
  All_Markets         : Markets.Lists.List;
  Win_Markets         : Sql.Statement_Type;
  Cnt : Integer := 0;
begin


  --  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","jjj");
  end if;

  --  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);
  Log("main", "params start");
  Log("main", "params stop");


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


  for I in Bot_Types.Bet_Market_Type'range loop
    Log("main", "type of bet in process " & I'Img);

    case I is
      when Place .. Winner =>
        Cnt := 0;
        All_Markets.Clear;
      when others          => exit;
    end case;
    T.Start;

    Win_Markets.Prepare("select * from AMARKETS where MARKETTYPE = :TYP");
    case I is
      when Winner => Win_Markets.Set("TYP", "WIN");
      when Place  => Win_Markets.Set("TYP", "PLACE");
      when others => raise Program_Error with "Bad type of market " & I'Img;
    end case;

    Markets.Read_List(Win_Markets, All_Markets);
    Log("main", "num in list=" & All_Markets.Length'Img);


    case I is
      when Winner =>
        Text_Io.Create(File => Race,
                       Mode => Text_Io.Out_File,
                       Name => Path & Marketname.Fix_String & "/win_place_connection.dat");
      when Place  =>
        Text_Io.Create(File => Race,
                       Mode => Text_Io.Out_File,
                       Name => Path & Marketname.Fix_String & "/place_win_connection.dat");
      when others => raise Program_Error with "Bad type of market " & I'Img;
    end case;

    Put_Line  (Race, "{");

    begin
      Market_Loop : for Market of All_Markets loop
        Cnt := Cnt +1;

        case I is
          when Winner => Market.Corresponding_Place_Market(Other_Market, Found);
          when Place  => Market.Corresponding_Win_Market(Other_Market, Found);
          when others => raise Program_Error with "Bad type of market " & I'Img;
        end case;

       -- Log (Market.To_String );
       -- Log (Found'img );
       -- Log (Other_Market.To_String );

        if Found and then (
                             (Market.Numwinners = Integer_4(1) and then Other_Market.Numwinners = Integer_4(3)) or else
                             (Market.Numwinners = Integer_4(3) and then Other_Market.Numwinners = Integer_4(1))
                          )
        then
          Put_Line(Race,  """" &  Market.Marketid & """"  & ":"  & """" & Other_Market.Marketid & """,");
        end if;

      end loop Market_Loop;
    end;

    Put_Line  (Race, "}");
    T.Commit;
    Text_Io.Close(Race);
  end loop;
  Sql.Close_Session;
  Log("main", "db Disconnected");

exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);

end Print_Win_Place_Markets;
