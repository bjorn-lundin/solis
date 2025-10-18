
with Ada.Environment_Variables;
--with Text_Io;
with Gnat;
with Gnat.Awk;

with Logging; use Logging;
with Types; use Types;
with Sql;
with Table_Amidrace;
with Ini;
with Stacktrace;
with Bot_Svn_Info;


procedure Insert_Midrace is

  package Ev renames Ada.Environment_Variables;

  procedure Insert_From_File(File : String) is
    use Gnat;
    use type Awk.Count;
    T : Sql.Transaction_Type;
    R : Table_Amidrace.Data_Type;
    Computer_File : Awk.Session_Type;

    function Fix_Index(S : String) return String is
      S2 : String(1..S'Length) := S;
    begin
      return S2;
    end Fix_Index;

  begin
    --             1                       2           3         4    5    6     7    8    9     10   11   12     13  14    15   16    17
    --2018-07-14 17:18:25.306 datapoint|1.126218360|1.126218361|0.50|2.36|FALSE|6.00|TRUE|10.00|FALSE|1.30|FALSE|1.93|TRUE|2.60|FALSE|1m2f Hcap |
    --2018-07-17 19:52:38.754 datapoint|1.134360351|1.134360352|0.60|1000.00|FALSE|1000.00|FALSE|1000.00|FALSE|2.00|FALSE|1000.00|FALSE|1000.00|FALSE|2m Hcap                                           |

Log("open :" & File);

    Awk.Set_Current(Computer_File);
    Awk.Open(Separators => "|", Filename => File);
    T.Start;
    while not Awk.End_Of_File loop
      Awk.Get_Line;
      Log(Awk.Field(0));
      Log (Awk.Number_Of_Fields'Img & " fields");
      if Awk.Number_Of_Fields >= 18 then
        R.Fraction := Fixed_Type'Value(Awk.Field(4));
        --Text_Io.Put_Line(Fix_Index(Awk.Field(2)));
        R.Marketidwin := Fix_Index(Awk.Field(2));
        R.Marketidplc := Fix_Index(Awk.Field(3));

        R.R1pricewin := Fixed_Type'Value(Awk.Field(5));
        R.R1wonwin := Boolean'Value(Awk.Field(6));
        R.R2pricewin := Fixed_Type'Value(Awk.Field(7));
        R.R2wonwin := Boolean'Value(Awk.Field(8));
        R.R3pricewin := Fixed_Type'Value(Awk.Field(9));
        R.R3wonwin := Boolean'Value(Awk.Field(10));

        R.R1priceplc := Fixed_Type'Value(Awk.Field(11));
        R.R1wonplc := Boolean'Value(Awk.Field(12));
        R.R2priceplc := Fixed_Type'Value(Awk.Field(13));
        R.R2wonplc := Boolean'Value(Awk.Field(14));
        R.R3priceplc := Fixed_Type'Value(Awk.Field(15));
        R.R3wonplc := Boolean'Value(Awk.Field(16));

        R.Marketname := Fix_Index(Awk.Field(17));

        Log(R.To_String);

        R.Insert;
      else
        Log ("to few fields");
      end if;
    end loop;
    T.Commit;
    Awk.Close (Computer_File);
    Log("close :" & File);

  end Insert_From_File;

  ---------------------------------------------------

begin


  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","insert_midrace");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/insert_midrace.log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log("main", "Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("local", "host", ""),
     Port     => Ini.Get_Value("local", "port", 5432),
     Db_Name  => Ini.Get_Value("local", "name", ""),
     Login    => Ini.Get_Value("local", "username", ""),
     Password => Ini.Get_Value("local", "password", ""));
  Log("main", "db Connected");

  Insert_From_File("/home/bnl/bnlbot/botstart/bot-1-0/history/data/data.dat");

  Sql.Close_Session;
exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);

end Insert_Midrace;
