with Ada;
with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Characters;
with Ada.Characters.Latin_1;
with Ada.Strings;
with Ada.Strings.Fixed;

with GNAT;
with GNAT.Sockets;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings;

with AWS;
with AWS.SMTP;
with AWS.SMTP.Authentication;
with AWS.SMTP.Authentication.Plain;
with AWS.SMTP.Client;

with Stacktrace;
with Types; use Types;
with Sql;

with Utils;
with Calendar2; use Calendar2;
--with Gnatcoll.Json; --use Gnatcoll.Json;

with Rpc;
with Lock ;
with Posix;
with Balances;
with Ini;
with Logging; use Logging;

--with Process_IO;
--with Core_Messages;


procedure Saldo_Fetcher is
  package EV renames Ada.Environment_Variables;
  use type Rpc.Result_Type;

  Me : constant String := "Main.";

  Sa_Par_Bot_User : aliased Gnat.Strings.String_Access;
  Ba_Daemon       : aliased Boolean := False;
  Cmd_Line        : Command_Line_Configuration;
  Betfair_Result  : Rpc.Result_Type := Rpc.Ok;
  My_Lock         : Lock.Lock_Type;
---------------------------------------------------------

  function Get_Db_Size(Db_Name : String ) return String ; -- forward declaration only


  procedure Mail_Saldo(Saldo, Old : Balances.Balance_Type) is
     T       : Calendar2.Time_Type := Calendar2.Clock;
     Subject : constant String             := "BetBot Saldo Report";
     use AWS;
  --   SMTP_Server_Name : constant String := "email-smtp.eu-north-1.amazonaws.com";
    --     SMTP_Server_Name : constant String := "mailout.telia.com"; --telia
    SMTP_Server_Name : constant String := "mailforward.nonodev.com";

     Status : SMTP.Status;
  begin
    Ada.Directories.Set_Directory(Ada.Environment_Variables.Value("BOT_CONFIG") & "/sslcert");
    declare
--      --for Telia
--      Auth : aliased constant SMTP.Authentication.Plain.Credential :=
--                                SMTP.Authentication.Plain.Initialize ("a00751796",
--                                                "c994e08b");
        --  Auth : aliased constant SMTP.Authentication.Plain.Credential :=
        --                            SMTP.Authentication.Plain.Initialize ("AKIA4CCYWRUF6WBFHS4O",
        --                                            "BOYbIW5ox8Vq9+6tUkqUpo4J7gy/a7u/tErewqGDFDWW"); -- fixed by java-tool

-- old version                  SMTP.Authentication.Plain.Initialize ("AKIAJZDDS2DVUNB76S6A",
--                                              "AhVJXW+YJRE/AMBPoUEOaCjAaWJWWRTDC8JoU039baJG");

      Auth : aliased constant SMTP.Authentication.Plain.Credential :=
               SMTP.Authentication.Plain.Initialize
                 ("betbot@nonobet.com", "rTrBJR+ADN");



      SMTP_Server : SMTP.Receiver := SMTP.Client.Initialize
                                  (SMTP_Server_Name,
                                   Port       => 587,
                                   Security   => smtp.starttls,
                                   Credential => Auth'Unchecked_Access);
      use Ada.Characters.Latin_1;
      Today     : Fixed_Type := Saldo.Balance + abs(Saldo.Exposure);
      Yesterday : Fixed_Type := Old.Balance + abs(Old.Exposure);
      Msg : constant String :=
          "Dagens saldo-rapport " & Cr & Lf &
          "konto:     " & Ini.Get_Value("betfair","username","") & Cr & Lf &
          "saldo:     " & Utils.F8_Image(Saldo.Balance) & Cr & Lf &
          "exposure:  " & Utils.F8_Image(abs(Saldo.Exposure))  & Cr & Lf &
          Cr & Lf &
          "saldo igar:     " & Utils.F8_Image(Old.Balance) & Cr & Lf &
          "exposure igar:  " & Utils.F8_Image(abs(Old.Exposure))  & Cr & Lf &
          Cr & Lf &
          "vinst idag: " & Utils.F8_Image(Today - Yesterday) &
          Cr & Lf &
          Cr & Lf &
          "Database sizes:" & Cr & Lf &
          "bnl " & Get_Db_Size("bnl")  & Cr & Lf &
          "jmb " & Get_Db_Size("jmb")  & Cr & Lf &
          "msm " & Get_Db_Size("msm")  & Cr & Lf &
          "dry " & Get_Db_Size("dry")  & Cr & Lf &
         -- "ael " & Get_Db_Size("ael")  & Cr & Lf &
         -- "ghd " & Get_Db_Size("ghd")  & Cr & Lf &
          --"ais-prod " & Get_Db_Size("ais-prod")  & Cr & Lf &
          Cr & Lf &
          "timestamp: " & Calendar2.String_Date_Time_ISO (T, " ", " ") & Cr & Lf &
          "sent from: " & GNAT.Sockets.Host_Name ;

      Receivers : constant SMTP.Recipients :=  (
                  SMTP.E_Mail("Bj=F6rn Lundin", "b.f.lundin@gmail.com"),
--                  SMTP.E_Mail("Bj=F6rn Lundin", "bjorn.lundin@consafelogistics.com")
                  SMTP.E_Mail("Joakim Birgerson", "joakim@birgerson.com"),
                  SMTP.E_Mail("Mats M=E5rtensson", "mats.g.martensson@gmail.com")
                );
    begin
      SMTP.Client.Send(Server  => SMTP_Server,
                       From    => SMTP.E_Mail ("bnl Betbot", "betbot@nonobet.com"),
                       To      => Receivers,
                       Subject => Subject,
                       Message => Msg,
                       Status  => Status);
    end;
    if not SMTP.Is_Ok (Status) then
      Log (Me & "Mail_Saldo", "Can't send message: " & SMTP.Status_Message (Status));
    end if;
  end Mail_Saldo;

---------------------------------

  procedure Insert_Saldo(S : in out Balances.Balance_Type) is
  begin
    Log(Me, "Insert_Saldo start");
    Log(Me, S.To_String);
    S.Insert;
    Log(Me, "Insert_Saldo stop");
  end Insert_Saldo;

  function Get_Old_Saldo return Balances.Balance_Type is
    Stm : Sql.Statement_Type;
    Eos : Boolean := False;
    Old_Bal : Balances.Balance_Type ;
  begin
    Stm.Prepare("select * from ABALANCES where BALDATE::date = (select current_date -1)");
    Stm.Open_Cursor;
    Stm.Fetch(Eos);
    if not Eos then
      Old_Bal := Balances.Get(Stm);
    end if;
    Stm.Close_Cursor;
    return Old_Bal;
  end Get_Old_Saldo;

  ---------------------------------------------------------------------

  procedure Balance( Betfair_Result : in out Rpc.Result_Type ; Saldo : out Balances.Balance_Type) is
    T : Sql.Transaction_Type;
    Now : Calendar2.Time_Type := Calendar2.Clock;
    Old_Saldo : Balances.Balance_Type ;
  begin

    Rpc.Get_Balance(Betfair_Result,Saldo);

    if Betfair_Result = Rpc.Ok then
      Saldo.Baldate := Now;
      T.Start;
      Insert_Saldo(Saldo);
      Old_Saldo := Get_Old_Saldo;
      T.Commit;
      Mail_Saldo(Saldo, Old_Saldo);
    end if;
  end Balance;

  -----------------------------------------------------------
  function Get_Db_Size(Db_Name : String ) return String is
    Buff           : String(1..100) := (others => ' ');
    Select_Db_Size : Sql.Statement_Type;
    Eos            : Boolean := False;
    use Ada.Strings;
    use Ada.Strings.Fixed;
    T : Sql.Transaction_Type;
  begin
    T.Start;
    Select_Db_Size.Prepare ("SELECT pg_size_pretty(pg_database_size(:DBNAME))" );
    Select_Db_Size.Set("DBNAME", Db_Name);
    Select_Db_Size.Open_Cursor;
    Select_Db_Size.Fetch(Eos);
    if not Eos then
      Select_Db_Size.Get(1, Buff);
    else
      Move("No such db: " & Db_Name, Buff);
    end if;
    Select_Db_Size.Close_Cursor;
    T.Commit;
    return Utils.Trim(Buff);
    exception
      when others =>
      T.Rollback;
      return Utils.Trim("bad db " & Db_Name);
  end Get_Db_Size;

------------------------------ main start -------------------------------------
  Saldo : Balances.Balance_Type;
  Global_Enabled : Boolean := True;
  cnt : integer := 0;
  max : integer := 120; --
begin
  Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");
  Global_Enabled := Ini.Get_Value("email","enabled",True);

  Logging.Open(EV.Value("BOT_HOME") & "/log/saldo_fetcher.log");

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

  if Ba_Daemon then
     Posix.Daemonize;
  end if;
   --must take lock AFTER becoming a daemon ...
   --The parent pid dies, and would release the lock...
  My_Lock.Take(EV.Value("BOT_NAME"));

  Log(Me, "Login betfair");
  Rpc.Init(
            Username   => Ini.Get_Value("betfair","username",""),
            Password   => Ini.Get_Value("betfair","password",""),
            Product_Id => Ini.Get_Value("betfair","product_id",""),
            Vendor_Id  => Ini.Get_Value("betfair","vendor_id",""),
            App_Key    => Ini.Get_Value("betfair","appkey","")
          );
  Rpc.Login;
  Log(Me, "Login betfair done");
  Log(Me, "Login in db");
  Sql.Connect
        (Host     => Ini.Get_Value("database","host",""),
         Port     => Ini.Get_Value("database","port",5432),
         Db_Name  => Ini.Get_Value("database","name",""),
         Login    => Ini.Get_Value("database","username",""),
         Password => Ini.Get_Value("database","password",""));


  if Global_Enabled then
    Ask : loop
      Cnt := Cnt +1;
      Balance(Betfair_Result, Saldo );
      Log(Me, "Ask_Balance result : " & Betfair_Result'Img);
      case Betfair_Result is
        when Rpc.Ok => exit Ask ;
        when Rpc.Logged_Out =>
          delay 2.0;
          Log(Me, "Logged_Out, will log in again");  --??
          begin
            Rpc.Login;
          exception
            when Rpc.Login_Failed =>
              Log(Me, "login failed, but will try again");
          end;
        when Rpc.Timeout =>  delay 5.0;
      end case;
      exit Ask when Cnt >= Max;
    end loop Ask;
  else
    Log(Me, "sending mails not enabled in [email] section of login.ini");
  end if;

  Log(Me, "Logoff from db");
  Sql.Close_Session;
  Rpc.Logout;
  Log(Me, "do_exit");
  Posix.Do_Exit(0); -- terminate

exception
  when Lock.Lock_Error =>
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
    Posix.Do_Exit(0); -- terminate
end Saldo_Fetcher;
