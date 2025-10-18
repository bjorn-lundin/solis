with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Directories;
with Logging; use Logging;
with Aws;
with Aws.Headers;
--with Aws.Headers.Set;
with Ada.Exceptions;
with Aws.Response;
with Aws.Client;
with Text_Io;
with Bot_Svn_Info;
with Utils; use Utils;
pragma Elaborate_All (Aws.Headers);
with Stacktrace;
with Ada.Calendar.Time_Zones;
--with Bot_System_Number;
--with Sql;
--with Ini;
with Process_Io;
with Bot_Messages;
with Interfaces.C;

package body Rpc is

  package Ev renames Ada.Environment_Variables;
  Me : constant String := "RPC.";

  Login_Handler : constant String := "login_handler";
  Dry           : constant String := "dry";

  No_Such_Field : exception;

  Global_Token : Token.Token_Type;

  Rpc_Tracker : Process_Io.Process_Type :=(("rpc_tracker    "),(others => ' '));
  ---------------------------------

  procedure Init(Username   : in     String;
                 Password   : in     String;
                 Product_Id : in     String;
                 Vendor_Id  : in     String;
                 App_Key    : in     String) is
  begin
    Log(Me & "Init", "start");

    Global_Token.Init(
                      Username   => Username,
                      Password   => Password,
                      Product_Id => Product_Id,
                      Vendor_Id  => Vendor_Id,
                      App_Key    => App_Key
                     );
    Log(Me & "Init", "stop");
  end Init;

  ------------------------------------------------------------------------------
  function Get_Token return Token.Token_Type is
  begin
    return Global_Token;
  end Get_Token;

  ------------------------------------------------------------------------------

  procedure Kill_Python_Server is
    use Interfaces;
    -- int kill(pid_t pid, int sig);
    type Pid_T is new C.Int;
    function Kill(Pid : Pid_T ; Sig : C.Int) return C.Int ;
    pragma Import(C, Kill, "kill");
    Dummy : C.Int;
    Sig_Term : constant C.Int := 15;
    Pid : Pid_T := 0;
    Filepath : String := (if Ev.Exists("BOT_TARGET") then Ev.Value("BOT_TARGET") else "NONAME") ;
    Filename : String := Filepath & "/befair_logon_daemon.pid" ;
    F : Text_Io.File_Type;
    Buffer : String(1..50);
    Len : Natural := 0;
  begin

    Text_Io.Open(File => F,
                 Mode => Text_Io.In_File,
                 Name => Filename);

    Text_Io.Get_Line(F,Buffer,Len);
    Text_Io.Close(F);
    Pid := Pid_T'Value(Buffer(1..Len));
    Log(Me & "Kill_Python_Server", "killing pid" & Pid'img);
    Dummy := Kill(Pid, Sig_Term);
  end Kill_Python_Server;

  -----------------------------------------------------------------------------


  procedure Login is
    Login_Http_Headers : Aws.Headers.List := Aws.Headers.Empty_List;
    Aws_Reply          : Aws.Response.Data;
    --    Header : AWS.Headers.List;
    --use Aws.Client;
    Now                : Calendar2.Time_Type := Calendar2.Clock;
    Fname              : String := Ev.Value("BOT_HOME") & "/token.dat";
    F                  : Text_Io.File_Type;
    Buffer             : String(1..100) := (others => ' ');
    Len                : Natural := 0;
    Bot_Name           : String := (if Ev.Exists("BOT_NAME") then Ev.Value("BOT_NAME") else "NONAME") ;
    Bot_User           : String := (if Ev.Exists("BOT_USER") then Ev.Value("BOT_USER") else "NOONE") ;
  begin
    if Now.Hour < 12 or else (Now.Hour = 23 and Now.Minute > 30) then
      Log(Me & "Login", "Login failed - bad time");

      if Bot_Name /= Login_Handler then
        raise Login_Failed with "Not allowed to login before 12";
      else
        if Ada.Directories.Exists(Fname)  then
          begin
            Ada.Directories.Delete_File(Fname);
            Log(Me & "Login", "deleted tokenfile");
          exception
            when others => null;
          end;
        end if;
      end if;
      return;
    end if;


    if Bot_User = Dry then
      declare
        Fname : String := Ev.Value("BOT_ROOT") & "/user/bnl/token.dat";
      begin
        Log(Me & "Login", "dry user, use bnl's token");
        if Ada.Directories.Exists(Fname) then
          Text_Io.Open(F,Text_Io.In_File,Fname);
          Text_Io.Get_Line(F,Buffer,Len);
          Text_Io.Close(F);
          Global_Token.Set(Buffer(1..Len));
          Log(Me & "Login", "use token from file (bnl) '" & Buffer(1..Len) & "'");
        else
          Log(Me & "Login", "file does not exist: '" & fname & "'");
        end if;
      end;
     return;
    else
      if Ada.Directories.Exists(Fname) and Bot_Name /= Login_Handler then
        Text_Io.Open(F,Text_Io.In_File,Fname);
        Text_Io.Get_Line(F,Buffer,Len);
        Text_Io.Close(F);
        Global_Token.Set(Buffer(1..Len));
        Log(Me & "Login", "use token from file '" & Buffer(1..Len) & "'");
        return;
      end if;
    end if;

    if Bot_Name /= Login_Handler then
      Log(Me & "Login", "'" & Bot_Name & "' -> not login_handler process - return");
      return;
    end if;

    -- ok - get a new token

    Log(Me & "Login", "login_handler process - ok - get new token");

    Aws.Headers.Add (Login_Http_Headers, "User-Agent", "AWS-BNL/1.0");

    -- curl -k -i -H "Accept: application/json" -H "X-Application: q0XW4VGRNoHuaszo" \
    --   -X POST -d 'username=bnlbnl&password=@Bf@vinst@1' \
    --   https://identitysso.betfair.se/api/login

    --HTTP/1.1 200 OK
    --Content-Type: application/json
    --Content-Length: 99
    --Date: Tue, 01 Jan 2019 12:06:48 GMT
    --{"token":"","product":"q0XW4VGRNoHuaszo","status":"FAIL","error":"TEMPORARY_BAN_TOO_MANY_REQUESTS"}

    declare
      Data        : String :=  "username=" & Global_Token.Get_Username & "&" & "password=" & Global_Token.Get_Password;
      Json_Reply  : Json_Value := Create_Object;
      Json_Reply2 : Json_Value := Create_Object;
      Login_Ok    : Boolean := False;
      Rpc_Track   : Bot_Messages.Rpc_Called_Record;
    begin
      Aws.Headers.Add (Login_Http_Headers, "Accept", "application/json");
      Aws.Headers.Add (Login_Http_Headers, "X-Application", Global_Token.Get_App_Key);

      Log(Me & "Login", "send Data   '" & Data & "'");
      Log(Me & "Login", "send appkey '" & Global_Token.Get_App_Key & "'");


      begin -- try python server first for bot-login (non-interactive)
        Aws_Reply := Aws.Client.Post (Url          => "http://localhost:12345/certlogin",
                                      Data         => Data,
                                      Content_Type => "application/x-www-form-urlencoded",
                                      Headers      => Login_Http_Headers,
                                      Timeouts     => Aws.Client.Timeouts (Each => 30.0));

        Move(Process_Io.This_Process.Name,Rpc_Track.Name);
        Rpc_Track.Typ := "I";
        Move(Data,Rpc_Track.Data, Drop => Right);
        Bot_Messages.Send(Receiver => Rpc_Tracker, Data => Rpc_Track );
        Log(Me & "Login", "reply'" & Aws.Response.Message_Body(Aws_Reply) & "'");

        declare
          Reply : String := Aws.Response.Message_Body(Aws_Reply);
        begin

          if Reply'Length >= 18 and then Reply(1..18) = "Post request error" then
            Log(Me & "Get_JSON_Reply", "Got reply: " & Reply & " raising Aws.Client.Connection_Error"  );
            raise Aws.Client.Connection_Error;
          end if;

          if Reply /= "Post Timeout" and then Reply /= "POST Timeout" then
            Json_Reply := Read (string'(Aws.Response.Message_Body(Aws_Reply)),
                                Filename => "");
            Log(Me & "Get_JSON_Reply", "Got reply: " & Json_Reply.Write  );
          else
            Log(Me & "Get_JSON_Reply", "Post Timeout -> Give up!");
            Kill_Python_Server;
            raise Post_Timeout ;
          end if;
        end;

        if Json_Reply.Has_Field("loginStatus") then
          if Json_Reply.Get("loginStatus") = "SUCCESS" then
            if Json_Reply.Has_Field("sessionToken") then
              Global_Token.Set(Trim(Json_Reply.Get("sessionToken")));
              Login_Ok := True;

              Text_Io.Create(F,Text_Io.Out_File,Fname);
              Text_Io.Put_Line(F,Global_Token.Get);
              Text_Io.Close(F);
              Log(Me & "Login", "wrote token to file");

              return;

            end if;
          end if;
        end if;

      exception
        when Aws.Client.Connection_Error =>
          Log(Me & "Login", "got Aws.Client.Connection_Error is python server up?");

      end;

      -- old way - non cert


      Aws_Reply := Aws.Client.Post (Url          => "https://identitysso.betfair.se/api/login",
                                    Data         => Data,
                                    Content_Type => "application/x-www-form-urlencoded",
                                    Headers      => Login_Http_Headers,
                                    Timeouts     => Aws.Client.Timeouts (Each => 30.0));

      Move(Process_Io.This_Process.Name,Rpc_Track.Name);
      Rpc_Track.Typ := "J";
      Move(Data,Rpc_Track.Data, Drop => Right);
      Bot_Messages.Send(Receiver => Rpc_Tracker, Data => Rpc_Track );
      Log(Me & "Login", "reply'" & Aws.Response.Message_Body(Aws_Reply) & "'");
      begin
        if String'(Aws.Response.Message_Body(Aws_Reply)) /= "Post Timeout" then
          Json_Reply2 := Read (Strm     => string'(Aws.Response.Message_Body(Aws_Reply)),
                              Filename => "");
          Log(Me & "Get_JSON_Reply", "Got reply: " & Json_Reply2.Write  );
        else
          Log(Me & "Get_JSON_Reply", "Post Timeout -> Give up!");
          raise Post_Timeout ;
        end if;
      end;

      if Json_Reply2.Has_Field("status") then
        if Json_Reply2.Get("status") = "SUCCESS" then
          if Json_Reply2.Has_Field("token") then
            Global_Token.Set(Trim(Json_Reply2.Get("token")));
            Login_Ok := True;

            Text_Io.Create(F,Text_Io.Out_File,Fname);
            Text_Io.Put_Line(F,Global_Token.Get);
            Text_Io.Close(F);
            Log(Me & "Login", "wrote token to file");
          end if;
        end if;
      end if;

      if not Login_Ok then
        Log(Me & "Login", "Login failed -> Give up!");
        raise Login_Failed;
      end if;
    end;
  end Login;

  ------------------------------------------------------------------------------

  procedure Logout is
    Logout_Http_Headers : Aws.Headers.List := Aws.Headers.Empty_List;
    Aws_Reply           : Aws.Response.Data;
    Bot_Name            : String := (if Ev.Exists("BOT_NAME") then Ev.Value("BOT_NAME") else "NONAME") ;
    Fname               : String := Ev.Value("BOT_HOME") & "/token.dat";

    Rpc_Track   : Bot_Messages.Rpc_Called_Record;
  begin

    if Bot_Name /= Login_Handler then
      Log(Me & "Logout", "only login_handler may logout, you are " & Bot_Name );
      return;
    else
      if Ada.Directories.Exists(Fname)  then
          begin
            Ada.Directories.Delete_File(Fname);
            Log(Me & "Login", "deleted tokenfile");
          exception
            when others => null;
          end;
      end if;
    end if;

    Aws.Headers.Add (Logout_Http_Headers, "User-Agent", "AWS-BNL/1.0");
    Aws.Headers.Add (Logout_Http_Headers, "Accept", "application/json");
    Aws.Headers.Add (Logout_Http_Headers, "X-Authentication", Global_Token.Get);

    Aws_Reply := Aws.Client.Post (Url          => "https://identitysso.betfair.se/api/logout",
                                  Data         => "", --Data,
                                  Content_Type => "application/x-www-form-urlencoded",
                                  Headers      => Logout_Http_Headers,
                                  Timeouts     => Aws.Client.Timeouts (Each => 30.0));
    Move(Process_Io.This_Process.Name,Rpc_Track.Name);
    Rpc_Track.Typ := "O";

    Bot_Messages.Send(Receiver => Rpc_Tracker, Data => Rpc_Track );

    Log(Me & "Logout", Aws.Response.Message_Body(Aws_Reply));

    if Position( Aws.Response.Message_Body(Aws_Reply),"""status"":""SUCCESS""") > Integer(0) then
      Global_Token.Unset;
    end if;
  end Logout;
  ------------------------------------------------------------------------------

  procedure Keep_Alive(Result : out Boolean )is
    Keep_Alive_Http_Headers : Aws.Headers.List := Aws.Headers.Empty_List;
    Aws_Reply               : Aws.Response.Data;
    Now                     : Calendar2.Time_Type := Calendar2.Clock;
    Bot_Name                : String := (if Ev.Exists("BOT_NAME") then Ev.Value("BOT_NAME") else "NONAME") ;
    Bot_User                : String := (if Ev.Exists("BOT_USER") then Ev.Value("BOT_USER") else "NOONE") ;
    Fname                   : String := (if Bot_User = Dry then Ev.Value("BOT_ROOT") & "/user/bnl/token.dat" else ev.Value("BOT_HOME") & "/token.dat");
    F                       : Text_Io.File_Type;
    Buffer                  : String(1..100) := (others => ' ');
    Len                     : Natural := 0;
    Rpc_Track               : Bot_Messages.Rpc_Called_Record;
  begin

    if Bot_Name /= Login_Handler then
      Log(Me & "Keep_Alive", "only login_handler may Keep_Alive, you are " & Bot_Name );

      if not Global_Token.Is_Set then
        Log(Me & "Keep_Alive", "no token - return false" );
        Result := False;
        return;
      end if;

      -- check file and compare with what we got
      if Ada.Directories.Exists(Fname) then
        Text_Io.Open(F,Text_Io.In_File,Fname);
        Text_Io.Get_Line(F,Buffer,Len);
        Text_Io.Close(F);
        Result := Global_Token.Get(1..5) = Buffer(1..5);
        Log(Me & "Keep_Alive", "result " & Result'Img & " start of tokens are '" & Global_Token.Get(1..5) & "' '" & Buffer(1..5) & "'");
      else
        Log(Me & "Keep_Alive", "file does not exist: '" & Fname & "'");
        Result := False;
      end if;

      return;
    end if;

    if Now.Hour < 12 or else (Now.Hour = 23 and Now.Minute > 30) then
      Log(Me & "Keep_Alive", "bad time - logout");

      if Bot_Name = Login_Handler and then Global_Token.Is_Set then
        Log(Me & "keep_alive"," logout");
        Logout;
      end if;
      Result := False;
      return;
    end if;

    Result := True;
    Aws.Headers.Add (Keep_Alive_Http_Headers, "User-Agent", "AWS-BNL/1.0");
    Aws.Headers.Add (Keep_Alive_Http_Headers, "Accept", "application/json");
    Aws.Headers.Add (Keep_Alive_Http_Headers, "X-Authentication", Global_Token.Get);

    Aws_Reply := Aws.Client.Post (Url          => "https://identitysso.betfair.se/api/keepAlive",
                                  Data         => "", --Data,
                                  Content_Type => "application/x-www-form-urlencoded",
                                  Headers      => Keep_Alive_Http_Headers,
                                  Timeouts     => Aws.Client.Timeouts (Each => 30.0));

    Move(Process_Io.This_Process.Name,Rpc_Track.Name);
    Rpc_Track.Typ := "K";
    -- no data Move(Data,Rpc_Track.Data, Drop => Right);
    Bot_Messages.Send(Receiver => Rpc_Tracker, Data => Rpc_Track );
    Log(Me & "Keep_Alive", Aws.Response.Message_Body(Aws_Reply));

    if Position( Aws.Response.Message_Body(Aws_Reply),"""status"":""FAIL""") > Integer(0) then
      Result := False;
    end if;
  end Keep_Alive;
  ------------------------------------------------------------------------------

  procedure Get_Json_Reply (Query : in     Json_Value;
                            Reply : in out Json_Value;
                            Url   : in     String) is
    Aws_Reply    : Aws.Response.Data;
    Http_Headers : Aws.Headers.List := Aws.Headers.Empty_List;
--    Rpc_Track    : Bot_Messages.Rpc_Called_Record;
  begin
    Aws.Headers.Add (Http_Headers, "X-Authentication", Global_Token.Get);
    Aws.Headers.Add (Http_Headers, "X-Application", Global_Token.Get_App_Key);
    Aws.Headers.Add (Http_Headers, "Accept", "application/json");
    Log(Me  & "Get_JSON_Reply", "posting: " & Query.Write);
    declare
      S : String := Query.Write;
    begin
      Aws_Reply := Aws.Client.Post (Url          => Url,
                                    Data         => S,
                                    Content_Type => "application/json",
                                    Headers      => Http_Headers,
                                    Timeouts     => Aws.Client.Timeouts (Each => 30.0));

--      Move(Process_Io.This_Process.Name,Rpc_Track.Name);
--      Rpc_Track.Typ := "D";
--      Move(S, Rpc_Track.Data, Drop => Right);
--      Bot_Messages.Send(Receiver => Rpc_Tracker, Data => Rpc_Track );
    end;
    Log(Me & "Get_JSON_Reply", "Got reply, check it ");

    declare
      R : String := Aws.Response.Message_Body(Aws_Reply);
    begin
      if R /= "Post Timeout" then
        Reply := Read (Strm => R, Filename => "");
        Log(Me & "Get_JSON_Reply", "Got reply: " & Reply.Write  );
      else
        Log(Me & "Get_JSON_Reply", "Post Timeout -> Give up!");
        raise Post_Timeout ;
      end if;
    end;
  exception
    when Post_Timeout => raise;
    when E: others =>
      Log(Me & "Get_JSON_Reply", "***********************  Bad reply start *********************************");
      Log(Me & "Get_JSON_Reply", "Bad reply" & Aws.Response.Message_Body(Aws_Reply));
      Log(Me & "Get_JSON_Reply", "***********************  Bad reply stop  ********" );

      declare
        Last_Exception_Name     : constant String  := Ada.Exceptions.Exception_Name(E);
        Last_Exception_Messsage : constant String  := Ada.Exceptions.Exception_Message(E);
        Last_Exception_Info     : constant String  := Ada.Exceptions.Exception_Information(E);
      begin
        Log(Last_Exception_Name);
        Log("Message : " & Last_Exception_Messsage);
        Log(Last_Exception_Info);
      end ;
      raise Bad_Reply ;
  end Get_Json_Reply;
  ------------------------------------------------------------------------------

  procedure Get_Value(Container: in     Json_Value;
                      Field    : in     String;
                      Target   : in out Boolean;
                      Found    :    out Boolean ) is
  begin
    if Container.Has_Field(Field) then
      Target := Container.Get(Field);
      Found := True;
    else
      Found := False;
    end if;
  end Get_Value;
  ------------------------------------------------------------------------------

  procedure Get_Value(Container: in    Json_Value;
                      Field    : in     String;
                      Target   : in out Fixed_Type;
                      Found    :    out Boolean ) is
    Tmp : Float := 0.0;
  begin
    if Container.Has_Field(Field) then
      Tmp := Container.Get(Field);
      Found := True;
      Target := Fixed_Type(Tmp);
    else
      Found := False;
    end if;
  end Get_Value;
  ------------------------------------------------------------------------------

  procedure Get_Value(Container: in     Json_Value;
                      Field    : in     String;
                      Target   : in out Integer_8;
                      Found    :    out Boolean ) is
    Tmp : String (1..20)  :=  (others => ' ') ;
  begin
    if Container.Has_Field(Field) then
      Move( Container.Get(Field), Tmp );
      if Tmp(2) = '.' then
        Target := Integer_8'Value(Tmp(3 .. Tmp'Last));
      else
        Target := Integer_8'Value(Tmp);
      end if;
      Found := True;
    else
      Found := False;
    end if;
  end Get_Value;
  ------------------------------------------------------------------------------

  procedure Get_Value(Container: in     Json_Value;
                      Field    : in     String;
                      Target   : in out String;
                      Found    : out    Boolean) is
  begin
    if Container.Has_Field(Field) then
      Move( Source => Container.Get(Field), Target => Target , Drop => Right);
      Found := True;
    else
      Found  := False;
    end if;
  end Get_Value;
  ------------------------------------------------------------------------------

  procedure Get_Value(Container: in     Json_Value;
                      Field    : in     String;
                      Target   : in out Json_Value;
                      Found    : out    Boolean) is
  begin
    if Container.Has_Field(Field) then
      Target := Container.Get(Field);
      Found := True;
    else
      Found  := False;
    end if;
  end Get_Value;
  ------------------------------------------------------------------------------

  procedure Get_Value(Container: in     Json_Value;
                      Field    : in     String;
                      Target   : in out Integer_4;
                      Found    :    out Boolean) is
    Tmp : Long_Long_Integer := 0 ;
  begin
    if Container.Has_Field(Field) then
      Tmp := Container.Get(Field);
      Found := True;
      Target := Integer_4(Tmp);
    else
      Found := False;
    end if;
  end Get_Value;
  ------------------------------------------------------------------------------

  procedure Get_Value(Container: in     Json_Value;
                      Field    : in     String;
                      Target   : in out Calendar2.Time_Type;
                      Found    :    out Boolean) is
  begin
    if Container.Has_Field(Field) then
      declare
        Tmp : String := Container.Get(Field);
      begin  --       "marketStartTime":"2013-06-22T17:39:00.000Z",
        Target := Calendar2.To_Time_Type(Tmp(1..10), Tmp(12..23));
      end;
      Found := True;
    else
      Found  := False;
    end if;
  end Get_Value;

  ------------------------------------------------------------------

  function Api_Exceptions_Are_Present(Reply : Json_Value) return Boolean is
    Error,
    Accountapingexception,
    Apingexception,
    Data             : Json_Value := Create_Object;
    Has_Error        : Boolean := False;
    --              {
    --                  "id": 15,
    --                  "jsonrpc": "2.0",
    --                  "error": {
    --                      "code": -32099,
    --                      "data": {
    --                          "AccountAPINGException": {
    --                              "requestUUID": "prdaan001-10091152-0001162118",
    --                              "errorCode": "NO_SESSION",
    --                              "errorDetails": "Session token is required for this operation"
    --                          },
    --                          "exceptionname": "AccountAPINGException"
    --                      },
    --                      "message": "AANGX-0010"
    --                  }
    --              }
  begin
    if Reply.Has_Field("error") then
      Has_Error := True;
      Error := Reply.Get("error");
      if Error.Has_Field("code") then
        Log(Me, "error.code " & Integer(Long_Long_Integer'(Error.Get("code")))'Img);
        if Error.Has_Field("data") then
          Data := Error.Get("data");
          if Data.Has_Field("APINGException") then
            Apingexception := Data.Get("APINGException");
            if Apingexception.Has_Field("errorCode") then
              Log(Me, "APINGException.errorCode " & Apingexception.Get("errorCode"));
              if Apingexception.Has_Field("errorDetails") then
                Log(Me, "APINGException.errorDetails " & Apingexception.Get("errorDetails"));
              end if;
              if Data.Has_Field("exceptionname") then
                Log(Me, "exceptionname " & Data.Get("exceptionname"));
              end if;
            else
              raise No_Such_Field with "APINGException - errorCode";
            end if;
          elsif Data.Has_Field("AccountAPINGException") then
            Accountapingexception := Data.Get("AccountAPINGException");
            if Accountapingexception.Has_Field("errorCode") then
              Log(Me, "APINGException.errorCode " & Accountapingexception.Get("errorCode"));
              if Accountapingexception.Get("errorCode") = "INVALID_SESSION_INFORMATION" then
                raise Invalid_Session;
              end if;
            else
              raise No_Such_Field with "AccountAPINGException - errorCode";
            end if;
          else
            raise No_Such_Field with "Data - APINGException";
          end if;
        end if;
        if Error.Has_Field("message") then
          Log(Me, "Error.Message " & Error.Get("message"));
        end if;
      else
        raise No_Such_Field with "Error - code";
      end if;
    end if;
    return Has_Error;
  end Api_Exceptions_Are_Present;
  ---------------------------------------------------------------------

  procedure Bet_Is_Matched(Betid             : Integer_8 ;
                           Is_Removed        : out Boolean;
                           Is_Matched        : out Boolean;
                           Avg_Price_Matched : out Bet_Price_Type;
                           Size_Matched      : out Bet_Size_Type
                          ) is
    Current_Order_Item,
    Result,
    Params,
    Daterange,
    Json_Reply,
    Json_Query   : Json_Value := Create_Object;


    Current_Orders,
    Bet_Ids      : Json_Array := Empty_Array;
    String_Betid : String     := Trim(Betid'Img);
  begin
    --{
    --     "jsonrpc": "2.0",
    --     "method": "SportsAPING/v1.0/listCurrentOrders",
    --     "params": {
    --          "betIds": ["31049748925"],
    --          "placedDateRange": {
    --          }
    --     },
    --     "id": 1
    --}
    Is_Matched := False;
    Avg_Price_Matched := 0.0;
    Size_Matched := 0.0;

    Append (Bet_Ids, Create(String_Betid));
    Params.Set_Field     (Field_Name => "betIds",          Field => Bet_Ids);
    Params.Set_Field     (Field_Name => "placesDateRange", Field => Daterange);
    Json_Query.Set_Field (Field_Name => "params",  Field => Params);
    Json_Query.Set_Field (Field_Name => "id",      Field => 15);   --?
    Json_Query.Set_Field (Field_Name => "method",  Field => "SportsAPING/v1.0/listCurrentOrders");
    Json_Query.Set_Field (Field_Name => "jsonrpc", Field => "2.0");

    Get_Json_Reply (Query => Json_Query,
                    Reply => Json_Reply,
                    Url   => Token.Url_Betting);

    -- ok, got a valid Json reply, check for errors
    if Api_Exceptions_Are_Present(Json_Reply) then
      return ;
    end if;

    -- ok, got a valid Json reply, parse it
    --      {
    --          "id": 15,
    --          "jsonrpc": "2.0",
    --          "result": {
    --              "moreAvailable": false,
    --              "currentOrders": [{
    --                  "marketId": "1.111593623",
    --                  "betId": "31122359882",
    --                  "handicap": 0.00000E+00,
    --                  "orderType": "LIMIT",
    --                  "sizeCancelled": 0.00000E+00,
    --                  "bspLiability": 0.00000E+00,
    --                  "selectionId": 7662169,
    --                  "sizeVoided": 0.00000E+00,
    --                  "status": "EXECUTION_COMPLETE",
    --                  "matchedDate": "2013-10-26T12:49:26.000Z",
    --                  "placedDate": "2013-10-26T11:44:54.000Z",
    --                  "sizeLapsed": 0.00000E+00,
    --                  "side": "LAY",
    --                  "priceSize": {
    --                      "size": 3.21200E+01,
    --                      "price": 1.65000E+01
    --                  },
    --                  "regulatorCode": "MALTA LOTTERIES AND GAMBLING AUTHORITY",
    --                  "sizeMatched": 3.21200E+01,
    --                  "persistenceType": "PERSIST",
    --                  "sizeRemaining": 0.00000E+00,
    --                  "averagePriceMatched": 1.65000E+01
    --              }]
    --          }
    --      }
    if Json_Reply.Has_Field("result") then
      Result := Json_Reply.Get("result");
    else
      Log(Me & "Bet_Is_Matched", "NO RESULT!!" );
      return ;
    end if;

    if Result.Has_Field("currentOrders") then
      Current_Orders := Result.Get("currentOrders");
      Log(Me & "Bet_Is_Matched", "got currentOrders, len: " & Length(Current_Orders)'Img);

      if Length(Current_Orders) > Natural(0) then
        Is_Removed := False;
        Current_Order_Item := Get(Current_Orders, 1); -- always element 1, since we only have 1
        Log(Me & "Bet_Is_Matched", "got Current_Order_Item");

        if Current_Order_Item.Has_Field("averagePriceMatched") then
          declare
            Tmp : Float := Current_Order_Item.Get("averagePriceMatched");
          begin
            Avg_Price_Matched := Bet_Price_Type(Tmp);
          end ;
        end if;

        if Current_Order_Item.Has_Field("sizeMatched") then
          declare
            Tmp : Float := Current_Order_Item.Get("sizeMatched");
          begin
            Size_Matched := Bet_Size_Type(Tmp);
          end ;
        end if;

        if Current_Order_Item.Has_Field("status") then
          Log(Me & "Bet_Is_Matched", "got Current_Order_Item.Status");
          Is_Matched := Current_Order_Item.Get("status") = "EXECUTION_COMPLETE";
        end if;
      else -- len = 0
        Is_Removed := True;
        Is_Matched := True;
      end if;
    end if;
    Log(Me & "Bet_Is_Matched", "Is_Matched: " & Is_Matched'Img & " AVG_Price_Matched: " & F8_Image(Fixed_Type(Avg_Price_Matched)))  ;
  end Bet_Is_Matched;
  -----------------------------------------------------------------
  procedure Check_Market_Result(Market_Id   : in     Marketid_Type;
                                Runner_List : in out Runners.Lists.List) is
  --{
  --     "jsonrpc": "2.0",
  --     "method": "SportsAPING/v1.0/listMarketBook",
  --     "params": {
  --          "marketIds": ["1.111572663"]
  --     },
  --     "id": 1
  --}
    Db_Runner : Runners.Runner_Type;

    Result,
    Params,
    --    Status,
    Json_Runner,
    Json_Reply,
    Json_Query          : Json_Value := Create_Object;

    Result_Array,Json_Runners_Array, Market_Ids : Json_Array := Empty_Array;
    Market_Id_Received                          : Marketid_Type := (others => ' ');
  begin

    Append (Market_Ids, Create(Market_Id));
    Params.Set_Field     (Field_Name => "marketIds", Field => Market_Ids);
    Json_Query.Set_Field (Field_Name => "params",  Field => Params);
    Json_Query.Set_Field (Field_Name => "id",      Field => 15);   --?
    Json_Query.Set_Field (Field_Name => "method",  Field => "SportsAPING/v1.0/listMarketBook");
    Json_Query.Set_Field (Field_Name => "jsonrpc", Field => "2.0");

    Get_Json_Reply (Query => Json_Query,
                    Reply => Json_Reply,
                    Url   => Token.Url_Betting);

    -- ok, got a valid Json reply, check for errors
    if Api_Exceptions_Are_Present(Json_Reply) then
      return ;
    end if;

    -- ok, got a valid Json reply, parse it
    --{
    --     "jsonrpc": "2.0",
    --     "result": [{
    --          "marketId": "1.111572663",
    --          "isMarketDataDelayed": false,
    --          "betDelay": 1,
    --          "bspReconciled": true,
    --          "complete": true,
    --          "inplay": true,
    --          "numberOfWinners": 1,
    --          "numberOfRunners": 9,
    --          "numberOfActiveRunners": 0,
    --          "totalMatched": 0.0,
    --          "totalAvailable": 0.0,
    --          "crossMatching": false,
    --          "runnersVoidable": false,
    --          "version": 624435001,
    --          "runners": [{
    --               "selectionId": 5662977,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 5.3,
    --               "removalDate": "2013-10-25T16:19:41.000Z",
    --               "status": "REMOVED"
    --          },
    --          {
    --               "selectionId": 6477571,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 52.5,
    --               "status": "LOSER"
    --          },
    --          {
    --               "selectionId": 6437577,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 12.4,
    --               "status": "LOSER"
    --          },
    --          {
    --               "selectionId": 6458897,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 10.6,
    --               "status": "LOSER"
    --          },
    --          {
    --               "selectionId": 4729721,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 9.6,
    --               "status": "WINNER"
    --          },
    --          {
    --               "selectionId": 6784150,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 4.9,
    --               "status": "LOSER"
    --          },
    --          {
    --               "selectionId": 3917956,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 6.6,
    --               "status": "LOSER"
    --          },
    --          {
    --               "selectionId": 6290196,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 2.4,
    --               "status": "LOSER"
    --          },
    --          {
    --               "selectionId": 5119099,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 1.3,
    --               "status": "LOSER"
    --          }],
    --          "status": "CLOSED"
    --     }],
    --     "id": 1
    --}

    if Json_Reply.Has_Field("result") then
      Result_Array := Json_Reply.Get("result");

      Log(Me & "Check_Market_Result", " Length(Result_Array) " &  Length(Result_Array)'Img  );

      if Length(Result_Array) > Natural(0) then
        Result := Get(Result_Array,1); -- one element in array only
      else
        Log(Me & "Check_Market_Result", "NO RESULT!! 1 " );
        return ;
      end if;
    else
      Log(Me & "Check_Market_Result", "NO RESULT!! 2" );
      return ;
    end if;


    if Result.Has_Field("marketId") then
      Market_Id_Received := Result.Get("marketId");
      Log(Me & "Check_Market_Result", "got marketId '" & Market_Id_Received & "'");
    else
      Log(Me & "Check_Market_Result", "NO marketId, return!");
      return;
    end if;

    if Result.Has_Field("status") then
      if Result.Get("status") = "CLOSED" or else
        Result.Get("status") = "SETTLED" then

        Log(Me & "Check_Market_Result", "Market IS settled , treat Market_Id_Received '" & Market_Id_Received & "' " &
              " Market_Status '" & Result.Get("status") & "'");
      else
        Log(Me & "Check_Market_Result", "Market IS NOT settled, wait/return Market_Id_Received '" & Market_Id_Received & "' " &
              " Market_Status '" & Result.Get("status") & "'");
        return;  -- market not settled yed
      end if;
    end if;

    if Result.Has_Field("runners") then
      Json_Runners_Array := Result.Get("runners");
      Log(Me & "Check_Market_Result", "got runners, len: " & Length(Json_Runners_Array)'Img);
      if Length(Json_Runners_Array) > Natural(0) then
        for I in 1 .. Length(Json_Runners_Array) loop
          Db_Runner := Runners.Empty_Data;
          Json_Runner := Get(Json_Runners_Array, I);
          Log(Me & "Check_Market_Result", "got Runner" & I'Img);

          if Json_Runner.Has_Field("selectionId") then
            declare
              I : Long_Long_Integer := Json_Runner.Get("selectionId");
            begin
              Db_Runner.Selectionid := Integer_4(I);
            end;
          else
            Log(Me & "Check_Market_Result", "no selection id!! Exit loop -  Runner" & I'Img);
            exit ;
          end if;
          Db_Runner.Marketid := Market_Id_Received ;

          if Json_Runner.Has_Field("status") then
            Move(Json_Runner.Get("status"), Db_Runner.Status);
            if Json_Runner.Get("status") = "WINNER" then
              Log(Me & "Check_Market_Result", "got a winner " & Db_Runner.To_String);
            elsif Json_Runner.Get("status") = "REMOVED" then
              Log(Me & "Check_Market_Result", "got a non-runner " & Db_Runner.To_String);
            elsif Json_Runner.Get("status") = "LOSER" then
              Log(Me & "Check_Market_Result", "got a loser " & Db_Runner.To_String);
            else
              Log(Me & "Check_Market_Result", "got something else !! " & Db_Runner.To_String);
            end if;
            Runner_List.Append(Db_Runner);
          else
            Log(Me & "Check_Market_Result", "runner is missing status, exit");
            exit;
          end if;
        end loop;
      end if;
    end if;

  end Check_Market_Result;
  ----------------------------------------------------------------

  procedure Market_Status_Is_Changed(Market     : in out Markets.Market_Type;
                                     Is_Changed :    out Boolean) is
  --{
  --     "jsonrpc": "2.0",
  --     "method": "SportsAPING/v1.0/listMarketBook",
  --     "params": {
  --          "marketIds": ["1.111572663"]
  --     },
  --     "id": 1
  --}
    Result,
    Params,
    Json_Reply,
    Json_Query               : Json_Value := Create_Object;
    Result_Array, Market_Ids : Json_Array := Empty_Array;
    Market_Id_Received       : Marketid_Type := (others => ' ');
    type Is_Changed_Type is (Status, Betdelay);
    Is_Changed_Array         : array (Is_Changed_Type'Range) of Boolean := (others => False);
  begin
    Is_Changed := False;
    Append (Market_Ids, Create(Market.Marketid));
    Params.Set_Field     (Field_Name => "marketIds", Field => Market_Ids);
    Json_Query.Set_Field (Field_Name => "params",  Field => Params);
    Json_Query.Set_Field (Field_Name => "id",      Field => 15);   --?
    Json_Query.Set_Field (Field_Name => "method",  Field => "SportsAPING/v1.0/listMarketBook");
    Json_Query.Set_Field (Field_Name => "jsonrpc", Field => "2.0");

    Get_Json_Reply (Query => Json_Query,
                    Reply => Json_Reply,
                    Url   => Token.Url_Betting);

    -- ok, got a valid Json reply, check for errors
    if Api_Exceptions_Are_Present(Json_Reply) then
      raise Json_Exception with "Bad rpc in Rpc.Market_Status_Is_Changed";
    end if;

    -- ok, got a valid Json reply, parse it
    --{
    --     "jsonrpc": "2.0",
    --     "result": [{
    --          "marketId": "1.111572663",
    --          "isMarketDataDelayed": false,
    --          "betDelay": 1,
    --          "bspReconciled": true,
    --          "complete": true,
    --          "inplay": true,
    --          "numberOfWinners": 1,
    --          "numberOfRunners": 9,
    --          "numberOfActiveRunners": 0,
    --          "totalMatched": 0.0,
    --          "totalAvailable": 0.0,
    --          "crossMatching": false,
    --          "runnersVoidable": false,
    --          "version": 624435001,
    --          "runners": [{
    --               "selectionId": 5662977,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 5.3,
    --               "removalDate": "2013-10-25T16:19:41.000Z",
    --               "status": "REMOVED"
    --          },
    --          {
    --               "selectionId": 6477571,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 52.5,
    --               "status": "LOSER"
    --          },
    --          {
    --               "selectionId": 6437577,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 12.4,
    --               "status": "LOSER"
    --          },
    --          {
    --               "selectionId": 6458897,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 10.6,
    --               "status": "LOSER"
    --          },
    --          {
    --               "selectionId": 4729721,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 9.6,
    --               "status": "WINNER"
    --          },
    --          {
    --               "selectionId": 6784150,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 4.9,
    --               "status": "LOSER"
    --          },
    --          {
    --               "selectionId": 3917956,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 6.6,
    --               "status": "LOSER"
    --          },
    --          {
    --               "selectionId": 6290196,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 2.4,
    --               "status": "LOSER"
    --          },
    --          {
    --               "selectionId": 5119099,
    --               "handicap": 0.0,
    --               "adjustmentFactor": 1.3,
    --               "status": "LOSER"
    --          }],
    --          "status": "CLOSED"
    --     }],
    --     "id": 1
    --}

    if Json_Reply.Has_Field("result") then
      Result_Array := Json_Reply.Get("result");

      if Length(Result_Array) > Natural(0) then
        Result := Get(Result_Array,1); -- one element in array only
      else
        Log(Me & "Check_Market_Result", "NO RESULT!! 3 " );
        return ;
      end if;
    else
      Log(Me & "Check_Market_Result", "NO RESULT!! 4" );
      return ;
    end if;

    if Result.Has_Field("marketId") then
      Market_Id_Received := Result.Get("marketId");
      Log(Me & "Market_Status_Is_Changed", "got marketId '" & Market_Id_Received & "'");
    else
      Log(Me & "Market_Status_Is_Changed", "NO marketId, return!");
      return;
    end if;

    if Result.Has_Field("status") then
      Is_Changed_Array(Status) := Result.Get("status")(1..3) /= Market.Status(1..3);
      if Is_Changed_Array(Status) then
        Market.Status := (others => ' ');
        Move( Result.Get("status"), Market.Status);
      end if;
      Log(Me & "Market_Status_Is_Changed",
          "Status changed for market '" & Market_Id_Received & "' " &
            Is_Changed_Array(Status)'Img & " status " & Market.Status);
    else
      Log(Me & "Market_Status_Is_Changed", "No status field found!!!");
    end if;

    if Result.Has_Field("betDelay") then
      Is_Changed_Array(Betdelay) :=  Result.Get("betDelay") /= Long_Long_Integer(Market.Betdelay);
      if Is_Changed_Array(Betdelay) then
        declare
          Bet_Delay : Long_Long_Integer := Result.Get("betDelay");
        begin
          Market.Betdelay := Integer_4(Bet_Delay);
        end;
      end if;
      Log(Me & "Market_Status_Is_Changed",
          "Bet delay changed for market '" & Market_Id_Received & "' " &
            Is_Changed_Array(Betdelay)'Img & " Betdelay " & Market.Betdelay'Img);
    else
      Log(Me & "Market_Status_Is_Changed", "No betDelay field found!!!");
    end if;

    Is_Changed := Is_Changed_Array(Status) or Is_Changed_Array(Betdelay);

  end Market_Status_Is_Changed;
  ---------------------------------------
  procedure Get_Balance(Betfair_Result : out Result_Type ; Saldo : out Balances.Balance_Type) is
    Query_Get_Account_Funds           : Json_Value := Create_Object;
    Reply_Get_Account_Funds           : Json_Value := Create_Object;
    Params                            : Json_Value := Create_Object;
    Result                            : Json_Value := Create_Object;
  begin
    Betfair_Result := Ok;
    -- params is empty ...
    Query_Get_Account_Funds.Set_Field (Field_Name => "params",  Field => Params);
    Query_Get_Account_Funds.Set_Field (Field_Name => "id",      Field => 15);          -- ???
    Query_Get_Account_Funds.Set_Field (Field_Name => "method",  Field => "AccountAPING/v1.0/getAccountFunds");
    Query_Get_Account_Funds.Set_Field (Field_Name => "jsonrpc", Field => "2.0");

    Get_Json_Reply (Query => Query_Get_Account_Funds,
                    Reply => Reply_Get_Account_Funds,
                    Url   => Token.Url_Account);

    begin
      if Api_Exceptions_Are_Present(Reply_Get_Account_Funds) then
        -- try again
        Betfair_Result := Logged_Out ;
        return;
      end if;
    exception
      when Invalid_Session =>
        Betfair_Result := Logged_Out ;
        return;
    end ;

    if Reply_Get_Account_Funds.Has_Field("result") then
      Result := Reply_Get_Account_Funds.Get("result");
      if Result.Has_Field("availableToBetBalance") then
        Saldo.Balance := Fixed_Type(Float'(Result.Get("availableToBetBalance")));
      else
        raise No_Such_Field with "Object 'Result' - Field 'availableToBetBalance'";
      end if;

      if Result.Has_Field("exposure") then
        Saldo.Exposure := Fixed_Type(Float'(Result.Get("exposure")));
      else
        raise No_Such_Field with "Object 'Result' - Field 'exposure'";
      end if;
    end if;
  end Get_Balance;

  ---------------------------------------
  procedure Get_Cleared_Bet_Info_List(Bet_Status     : in Bet_Status_Type;
                                      Settled_From   : in Calendar2.Time_Type := Calendar2.Time_Type_First;
                                      Settled_To     : in Calendar2.Time_Type := Calendar2.Time_Type_Last;
                                      Betfair_Result : out Result_Type;
                                      Bet_List       : out Bets.Lists.List) is

    Json_Query         : Json_Value := Create_Object;
    Json_Reply         : Json_Value := Create_Object;
    Params             : Json_Value := Create_Object;
    Result             : Json_Value := Create_Object;
    Settled_Date_Range : Json_Value := Create_Object;
    Cleared_Orders     : Json_Array := Empty_Array;
    Cleared_Order      : Json_Value := Create_Object;

    Local_Bet          : Bets.Bet_Type;
    Found              : Boolean := False;

  begin
    Betfair_Result := Ok;

    Settled_Date_Range.Set_Field (Field_Name => "from", Field => Calendar2.String_Date_Time_Iso(Settled_From,"T","Z"));
    Settled_Date_Range.Set_Field (Field_Name => "to",   Field => Calendar2.String_Date_Time_Iso(Settled_To,  "T","Z"));

    Params.Set_Field (Field_Name => "groupBy", Field => "BET");
    Params.Set_Field (Field_Name => "includeItemDescription", Field => False);
    Params.Set_Field (Field_Name => "settledDateRange", Field => Settled_Date_Range);
    Params.Set_Field (Field_Name => "betStatus",        Field => Bet_Status'Img);
    -- params is empty ...
    Json_Query.Set_Field (Field_Name => "params",  Field => Params);
    Json_Query.Set_Field (Field_Name => "id",      Field => 15);          -- ???
    Json_Query.Set_Field (Field_Name => "method",  Field => "SportsAPING/v1.0/listClearedOrders");
    Json_Query.Set_Field (Field_Name => "jsonrpc", Field => "2.0");

    Get_Json_Reply(Query => Json_Query,
                   Reply => Json_Reply,
                   Url   => Token.Url_Betting);

    if Api_Exceptions_Are_Present(Json_Reply) then
      Betfair_Result := Logged_Out ;
      return;
    end if;

    if Json_Reply.Has_Field("result") then
      Result := Json_Reply.Get("result");
      if Result.Has_Field("clearedOrders") then
        Cleared_Orders := Result.Get("clearedOrders");
        if Length(Cleared_Orders) > Integer(0) then
          for I in 1 .. Length (Cleared_Orders) loop
            Log(Me & "Get_Cleared_Bet_Info_List" , " we have cleared order #:" & I'Img & " with status: " & Bet_Status'Img);

            Cleared_Order := Get(Cleared_Orders, I);
            Local_Bet := Bets.Empty_Data;
            Get_Value(Container => Cleared_Order,
                      Field     => "betId",
                      Target    => Local_Bet.Betid,
                      Found     => Found);

            Get_Value(Container => Cleared_Order,
                      Field     => "priceMatched",
                      Target    => Local_Bet.Pricematched,
                      Found     => Found);

            Get_Value(Container => Cleared_Order,
                      Field     => "sizeSettled",
                      Target    => Local_Bet.Sizematched,
                      Found     => Found);

            Get_Value(Container => Cleared_Order,
                      Field     => "profit",
                      Target    => Local_Bet.Profit,
                      Found     => Found);

            Move(Bet_Status'Img, Local_Bet.Status);

            Bet_List.Append(Local_Bet);
          end loop;
        else
          Log(Me & "Get_Cleared_Bet_Info_List", "No cleared orders received with status " & Bet_Status'Img);
        end if;
      end if;
    end if;
  end Get_Cleared_Bet_Info_List;
  -----------------------------------
  function  Cancel_Bet(Bet : in Bets.Bet_Type) return Boolean is
    Json_Query   : Json_Value := Create_Object;
    Json_Reply   : Json_Value := Create_Object;
    Params       : Json_Value := Create_Object;
    Instruction  : Json_Value := Create_Object;
    Instructions : Json_Array := Empty_Array;
    Result       : Json_Value := Create_Object;
    Status       : String(1..50) := (others => ' ');
    Found        : Boolean    := False;
  begin

    Instruction.Set_Field (Field_Name => "betId", Field => Trim(Bet.Betid'Img));
    Append(Instructions, Instruction);

    Params.Set_Field (Field_Name => "marketId", Field => Bet.Marketid);
    Params.Set_Field (Field_Name => "instructions", Field => Instructions);

    Json_Query.Set_Field (Field_Name => "params",  Field => Params);
    Json_Query.Set_Field (Field_Name => "id",      Field => 15);          -- ???
    Json_Query.Set_Field (Field_Name => "method",  Field => "SportsAPING/v1.0/cancelOrders");
    Json_Query.Set_Field (Field_Name => "jsonrpc", Field => "2.0");

    Get_Json_Reply(Query => Json_Query,
                   Reply => Json_Reply,
                   Url   => Token.Url_Betting);

    if Api_Exceptions_Are_Present(Json_Reply) then
      return False;
    end if;

    --{
    --    "jsonrpc": "2.0",
    --    "result": {
    --        "status": "SUCCESS",
    --        "marketId": "1.118200318",
    --        "instructionReports": [{
    --            "status": "SUCCESS",
    --            "instruction": {
    --                "betId": "48818303652"
    --            },
    --            "sizeCancelled": 30.0
    --        }]
    --    },
    --    "id": 1
    --}
    -- or ---
    --{
    --    "jsonrpc": "2.0",
    --    "result": {
    --        "status": "FAILURE",
    --        "errorCode": "BET_ACTION_ERROR",
    --        "marketId": "1.118200318",
    --        "instructionReports": [{
    --            "status": "FAILURE",
    --            "errorCode": "BET_TAKEN_OR_LAPSED",
    --            "instruction": {
    --                "betId": "4881830132"
    --            }
    --        }]
    --    },
    --    "id": 1
    --}

    Get_Value(Container => Json_Reply,
              Field     => "result",
              Target    => Result,
              Found     => Found);
    if not Found then
      Log(Me & "Cancel_Bet", "NO RESULT!!" );
      return False;
    end if;

    Get_Value(Container => Result,
              Field     => "status",
              Target    => Status,
              Found     => Found);
    if not Found then
      Log(Me & "Cancel_Bet", "NO STATUS!!" );
      return False;
    end if;
    Log(Me & "Cancel_Bet", "status : '" & Trim(Status) & "' returning " &
          Boolean'Image(Trim(Status) = "SUCCESS"));
    return Trim(Status) = "SUCCESS" ;
  end Cancel_Bet;
  -----------------------------------


  procedure Cancel_Bet(Market_Id : in Marketid_Type;
                       Bet_Id    : in Integer_8) is
    Json_Query     : Json_Value := Create_Object;
    Json_Reply     : Json_Value := Create_Object;
    Params         : Json_Value := Create_Object;
    Instruction    : Json_Value := Create_Object;
    Instructions   : Json_Array := Empty_Array;
    Betfair_Result : Result_Type;

  begin
    Betfair_Result := Ok;

    Instruction.Set_Field (Field_Name => "betId", Field => Trim(Bet_Id'Img));
    Append(Instructions, Instruction);

    Params.Set_Field (Field_Name => "marketId", Field => Market_Id);
    Params.Set_Field (Field_Name => "instructions", Field => Instructions);

    Json_Query.Set_Field (Field_Name => "params",  Field => Params);
    Json_Query.Set_Field (Field_Name => "id",      Field => 15);          -- ???
    Json_Query.Set_Field (Field_Name => "method",  Field => "SportsAPING/v1.0/cancelOrders");
    Json_Query.Set_Field (Field_Name => "jsonrpc", Field => "2.0");

    Get_Json_Reply(Query => Json_Query,
                   Reply => Json_Reply,
                   Url   => Token.Url_Betting);

    if Api_Exceptions_Are_Present(Json_Reply) then
      Betfair_Result := Logged_Out ;
      return;
    end if;

    Log(Me & "Cancel_Bet", "Betfair_Result: " & Betfair_Result'Img);
  end Cancel_Bet;
  -----------------------------------

  procedure Parse_Prices(J_Market   : in     Json_Value;
                         Price_List : in out Prices.Lists.List ) is
    Back,
    Lay,
    Ex,
    Runner            : Json_Value := Create_Object;
    Back_Array,
    Lay_Array,
    Runner_Prices     : Json_Array := Empty_Array;
    Array_Length      : Natural;
    Array_Length_Back : Natural;
    Array_Length_Lay  : Natural;
    Now               : Calendar2.Time_Type := Calendar2.Clock;
    Found             : Boolean := False;
    Db_Runner_Price   : Prices.Price_Type;

    --        "runners": [{
    --            "handicap": 0.00000E+00,
    --            "totalMatched": 0.00000E+00,
    --            "selectionId": 7311189,
    --            "status": "ACTIVE",
    --            "ex": {
    --                "tradedVolume": [],
    --                "availableToBack": [{
    --                    "size": 1.47106E+03,
    --                    "price": 1.06000E+00
    --                },
    --                {
    --                    "size": 4.14300E+01,
    --                    "price": 1.04000E+00
    --                },
    --                {
    --                    "size": 8.28656E+03,
    --                    "price": 1.03000E+00
    --                }],
    --                "availableToLay": [{
    --                    "size": 2.07160E+02,
    --                    "price": 4.00000E+01
    --                }]
    --            }
    --        },
    --  type Data_Type is record
    --      Marketid :    String (1..11) := (others => ' ') ; -- Primary Key
    --      Selectionid :    Integer_4  := 0 ; -- Primary Key
    --      Pricets :    Time_Type  := Time_Type_First ; -- Primary Key
    --      Status :    String (1..50) := (others => ' ') ; --
    --      Totalmatched :    Fixed_Type  := 0.0 ; --
    --      Backprice :    Fixed_Type  := 0.0 ; --
    --      Layprice :    Fixed_Type  := 0.0 ; --
    --      Ixxlupd :    String (1..15) := (others => ' ') ; --
    --      Ixxluts :    Time_Type  := Time_Type_First ; --
    --  end record;


  begin
    Runner_Prices := J_Market.Get("runners");
    Array_Length  := Length (Runner_Prices);

    for J in 1 .. Array_Length loop
      Db_Runner_Price := Prices.Empty_Data;

      Runner := Get (Arr   => Runner_Prices, Index => J);

      Get_Value(Container => J_Market,
                Field     => "marketId",
                Target    => Db_Runner_Price.Marketid,
                Found     => Found);
      if not Found then
        raise No_Such_Field with "Object 'Market' - Field 'marketId'";
      end if;

      Get_Value(Container => Runner,
                Field     => "selectionId",
                Target    => Db_Runner_Price.Selectionid,
                Found     => Found);
      if not Found then
        raise No_Such_Field with "Object 'Market' - Field 'selectionId'";
      end if;

      Get_Value(Container => Runner,
                Field     => "status",
                Target    => Db_Runner_Price.Status,
                Found     => Found);
      if not Found then
        raise No_Such_Field with "Object 'Market' - Field 'status'";
      end if;

      Get_Value(Container => Runner,
                Field     => "totalMatched",
                Target    => Db_Runner_Price.Totalmatched,
                Found     => Found);

      Db_Runner_Price.Pricets := Now;

      if Runner.Has_Field("ex") then
        Ex := Runner.Get("ex");
        if Ex.Has_Field("availableToBack") then
          Back_Array := Ex.Get("availableToBack");
          Array_Length_Back := Length(Back_Array);
          if Array_Length_Back >= 1 then
            Back := Get (Arr   => Back_Array, Index => 1);
            if Back.Has_Field("price") then
              Db_Runner_Price.Backprice := Fixed_Type(Float'(Back.Get("price")));
            else
              raise No_Such_Field with "Object 'Back' - Field 'price'";
            end if;
          end if;
        else
          raise No_Such_Field with "Object 'Back' - Field 'availableToBack'";
        end if;

        if Ex.Has_Field("availableToLay") then
          Lay_Array := Ex.Get("availableToLay");
          Array_Length_Lay := Length(Lay_Array);
          if Array_Length_Lay >= 1 then
            Lay := Get (Arr   => Lay_Array, Index => 1);
            if Lay.Has_Field("price") then
              Db_Runner_Price.Layprice := Fixed_Type(Float'(Lay.Get("price")));
            else
              raise No_Such_Field with "Object 'Lay' - Field 'price'";
            end if;
          end if;
        else
          raise No_Such_Field with "Object 'Lay' - Field 'availableToLay'";
        end if;
      else -- no 'ex'
        raise No_Such_Field with "Object 'Runner' - Field 'ex'";
      end if;

      Price_List.Append(Db_Runner_Price);
      Log(Me & "Parse_Prices", Db_Runner_Price.To_String);
    end loop;
  end Parse_Prices;

  ---------------------------------

  procedure Parse_Event (J_Event, J_Event_Type : in     Json_Value ;
                         Db_Event              : in out Events.Event_Type) is
    Service : constant String := "Parse_Event";
    -- event:{"id":"27026778",
    --         "name":"Monm 22nd Jun",
    --         "countryCode":"GB",
    --         "openDate":"2013-06-22T17:39:00.000Z",
    --         "timezone":"Europe/London",
    --         "venue":"Monmore"}
    -- eventType:{"id":"7",
    --              "name":"Horse Racing"}
    --  type Data_Type is record
    --      Eventid :    String (1..11) := (others => ' ') ; -- Primary Key
    --      Eventname :    String (1..50) := (others => ' ') ; --
    --      Countrycode :    String (1..2) := (others => ' ') ; -- non unique index 2
    --      Timezone :    String (1..50) := (others => ' ') ; --
    --      Opents :    Time_Type  := Time_Type_First ; -- non unique index 3
    --      Eventtypeid :    Integer_4  := 0 ; -- non unique index 4
    --      Ixxlupd :    String (1..15) := (others => ' ') ; --
    --      Ixxluts :    Time_Type  := Time_Type_First ; --
    --  end record;
    Found   : Boolean := False;
    pragma Warnings(Off,Found);
  begin
    Log(Me & Service, "start");

    Get_Value(Container => J_Event,
              Field     => "id",
              Target    => Db_Event.Eventid,
              Found     => Found);

    Get_Value(Container => J_Event,
              Field     => "name",
              Target    => Db_Event.Eventname,
              Found     => Found);


    Get_Value(Container => J_Event,
              Field     => "countryCode",
              Target    => Db_Event.Countrycode,
              Found     => Found);
    if not Found then
      Move("XX", Db_Event.Countrycode);
    end if;

    Get_Value(Container => J_Event,
              Field     => "openDate",
              Target    => Db_Event.Opents,
              Found     => Found);

    Get_Value(Container => J_Event,
              Field     => "timezone",
              Target    => Db_Event.Timezone,
              Found     => Found);

    -- event_type !!
    --    Get_Value(Container => J_Event_Type,
    --              Field     => "id",
    --              Target    => DB_Event.Eventtypeid,
    --              Found     => Found);

    declare
      T : String(1..5) := (others => ' ');
    begin
      Get_Value(Container => J_Event_Type,
                Field     => "id",
                Target    => T,
                Found     => Found);
      Db_Event.Eventtypeid := Integer_4'Value(T);

    end;
    Log(Me & Service, Db_Event.To_String);
    Log(Me & Service, "stop");

  end Parse_Event;

  ----------------------------------------------------------------------------------
  procedure Get_Market_Prices(Market_Id  : in Marketid_Type;
                              Market     : out Markets.Market_Type;
                              Price_List : in out Prices.Lists.List;
                              In_Play    : out Boolean) is
    Market_Ids         : Json_Array := Empty_Array;
    Json_Query         : Json_Value := Create_Object;
    Json_Reply         : Json_Value := Create_Object;
    Json_Market        : Json_Value := Create_Object;
    Params             : Json_Value := Create_Object;
    Result             : Json_Array := Empty_Array;
    Price_Projection   : Json_Value := Create_Object;
    Price_Data         : Json_Array := Empty_Array;

  begin
    In_Play := False;

    Append(Market_Ids, Create(Market_Id));
    Append (Price_Data , Create("EX_BEST_OFFERS"));

    Price_Projection.Set_Field (Field_Name => "priceData", Field => Price_Data);
    Price_Projection.Set_Field (Field_Name => "virtualise", Field => True); -- bnl 2016-07-18


    Params.Set_Field (Field_Name => "priceProjection", Field => Price_Projection);
    Params.Set_Field (Field_Name => "currencyCode",    Field => "SEK");
    Params.Set_Field (Field_Name => "locale",          Field => "sv");
    Params.Set_Field (Field_Name => "marketIds",       Field => Market_Ids);

    Json_Query.Set_Field (Field_Name => "params",  Field => Params);
    Json_Query.Set_Field (Field_Name => "id",      Field => 15);   --?
    Json_Query.Set_Field (Field_Name => "method",  Field => "SportsAPING/v1.0/listMarketBook");
    Json_Query.Set_Field (Field_Name => "jsonrpc", Field => "2.0");


    Get_Json_Reply(Query => Json_Query,
                   Reply => Json_Reply,
                   Url   => Token.Url_Betting);

    if Rpc.Api_Exceptions_Are_Present(Json_Reply) then
      Log(Me & "Get_Market_Prices", "APINGException is present, return");
      return;
    end if;

    --  Iterate the Reply_List_Market_Book object.
    if Json_Reply.Has_Field("result") then
      --      Log(Me, "we have result ");
      Result := Json_Reply.Get("result");
      for I in 1 .. Length(Result) loop
        Json_Market := Get(Result, I);
        Parse_Market(Json_Market, Market, In_Play);
        if Json_Market.Has_Field("runners") then
          Parse_Prices(Json_Market, Price_List);
        end if;
      end loop;
    end if;
  end Get_Market_Prices;
  ----------------------------------------------------------------------------------
  procedure Place_Bet (Bet_Name         : in     Betname_Type;
                       Market_Id        : in     Marketid_Type;
                       Side             : in     Bet_Side_Type;
                       Runner_Name      : in     Runnername_Type;
                       Selection_Id     : in     Integer_4;
                       Size             : in     Bet_Size_Type;
                       Price            : in     Bet_Price_Type;
                       Bet_Persistence  : in     Bet_Persistence_Type;
                       Match_Directly   : in     Integer_4 := 0;
                       Fill_Or_Kill     : in     Boolean := False;
                       Bet              :    out Bets.Bet_Type ) is
    Json_Query   : Json_Value := Create_Object;
    Json_Reply   : Json_Value := Create_Object;
    Params       : Json_Value := Create_Object;
    Limit_Order  : Json_Value := Create_Object;
    Instruction  : Json_Value := Create_Object;
    Instructions : Json_Array := Empty_Array;

    Execution_Report_Status        : String (1..50)  :=  (others => ' ') ;
    Execution_Report_Error_Code    : String (1..50)  :=  (others => ' ') ;
    Instruction_Report_Status      : String (1..50)  :=  (others => ' ') ;
    Instruction_Report_Error_Code  : String (1..50)  :=  (others => ' ') ;
    Order_Status                   : String (1..50)  :=  (others => ' ') ;
    L_Size_Matched,
    Average_Price_Matched          : Fixed_Type := 0.0;
    Powerdays                      : Integer_4 := Match_Directly;

    Bet_Id : Integer_8 := 0;
    Now    : Calendar2.Time_Type := Calendar2.Clock;

    Price_String  : String         := F8_Image(Fixed_Type(Price)); -- 2 decimals only
    Local_Price   : Bet_Price_Type := Bet_Price_Type'Value(Price_String); -- to avoid INVALID_BET_PRICE

    Size_String   : String         := F8_Image(Fixed_Type(Size)); -- 2 decimals only
    Local_Size    : Bet_Size_Type  := Bet_Size_Type'Value(Size_String); -- to avoid INVALID_BET_SIZE

    Price_Matched : Bet_Price_Type := 0.0;
    Size_Matched  : Bet_Size_Type  := 0.0;

    Side_String   : Bet_Side_String_Type := (others => ' ');
    Found         : Boolean              := False;
    pragma Warnings(Off,Found);

  begin
    Move(Side'Img, Side_String);

    Limit_Order.Set_Field (Field_Name => "persistenceType", Field => Bet_Persistence'Img);
    Limit_Order.Set_Field (Field_Name => "price", Field => Float(Local_Price));
    Limit_Order.Set_Field (Field_Name => "size", Field => Float(Local_Size));
    --,"minFillSize":5.0,"timeInForce":"FILL_OR_KILL"
    if Fill_Or_Kill then
      Limit_Order.Set_Field (Field_Name => "timeInForce", Field => "FILL_OR_KILL");
      Limit_Order.Set_Field (Field_Name => "minFillSize", Field => Float(Local_Size));
    end if;
    Instruction.Set_Field (Field_Name => "limitOrder",  Field => Limit_Order);
    Instruction.Set_Field (Field_Name => "orderType",   Field => "LIMIT");
    Instruction.Set_Field (Field_Name => "side",        Field => Side'Img);
    Instruction.Set_Field (Field_Name => "handicap",    Field => 0);
    Instruction.Set_Field (Field_Name => "selectionId", Field => Long_Long_Integer(Selection_Id));



    Append (Instructions , Instruction);

    Params.Set_Field (Field_Name => "instructions", Field => Instructions);
    Params.Set_Field (Field_Name => "marketId",     Field => Trim(Market_Id));

    Json_Query.Set_Field (Field_Name => "params", Field => Params);
    Json_Query.Set_Field (Field_Name => "id", Field => 16);
    Json_Query.Set_Field (Field_Name => "method",   Field      => "SportsAPING/v1.0/placeOrders");
    Json_Query.Set_Field (Field_Name => "jsonrpc",  Field      => "2.0");

    --{
    --    "jsonrpc": "2.0",
    --    "method": "SportsAPING/v1.0/placeOrders",
    --    "params": {
    --        "marketId": "' + marketId + '",
    --        "instructions": [
    --            {
    --                "selectionId": "' + str(selectionId) + '",
    --                "handicap": "0",
    --                "side": "BACK",
    --                "orderType": "LIMIT",
    --                "limitOrder": {
    --                    "size": "0.01",
    --                    "price": "1.50",
    --                    "persistenceType": "LAPSE"
    --                }
    --            }
    --        ],
    --        "customerRef": "test12121212121"
    --    },
    --    "id": 1
    --}

    Get_Json_Reply(Query => Json_Query,
                   Reply => Json_Reply,
                   Url   => Token.Url_Betting);
    -- parse out the reply.
    -- check for API exception/Error first

    if Rpc.Api_Exceptions_Are_Present(Json_Reply) then
      Log(Me & "Make_Bet", "APINGException is present, return");
      return;
    end if;

    -- {
    --    "jsonrpc":"2.0",
    --    "result":
    --            {
    --                "status":"SUCCESS",
    --                "marketId":"1.110689758",
    --                "instructionReports":
    --                    [
    --                        {
    --                             "status":"SUCCESS",
    --                             "instruction":
    --                                {
    --                                   "orderType":"LIMIT",
    --                                   "selectionId":6644807,
    --                                   "handicap":0.0,
    --                                   "side":"BACK",
    --                                   "limitOrder":
    --                                       {
    --                                          "size":30.0,
    --                                          "price":2.3,
    --                                          "persistenceType":"LAPSE"
    --                                        }
    --                               },
    --                               "betId":"29225429632",
    --                               "placedDate":"2013-08-24T12:43:54.000Z",
    --                               "averagePriceMatched":2.3399999999999994,
    --                               "sizeMatched":30.0
    --                        }
    --                    ]
    --                },
    --        "id":15
    --}

    -- or

    --  {
    --      "id": 16,
    --      "jsonrpc": "2.0",
    --      "result": {
    --          "marketId": "1.116221480",
    --          "status": "FAILURE",
    --          "errorCode": "BET_ACTION_ERROR",
    --          "instructionReports": [{
    --              "instruction": {
    --                  "handicap": 0.00000E+00,
    --                  "limitOrder": {
    --                      "size": 1.00000E+01,
    --                      "price": 1.01000E+00,
    --                      "persistenceType": "PERSIST"
    --                  },
    --                  "orderType": "LIMIT",
    --                  "selectionId": 3586050,
    --                  "side": "BACK"
    --              },
    --              "status": "FAILURE",
    --              "errorCode": "INVALID_BET_SIZE"
    --          }]
    --      }
    --  }

    -- ok we have a parsable answer with no formal errors.
    -- lets look at it
    declare
      Result           : Json_Value := Create_Object;
      Instructionsitem : Json_Value := Create_Object;
      Instructions     : Json_Array := Empty_Array;
    begin

      Get_Value(Container => Json_Reply,
                Field     => "result",
                Target    => Result,
                Found     => Found);
      if not Found then
        Log(Me & "Make_Bet", "NO RESULT!!" );
        raise Json_Exception with "Betfair reply has no result!";
      end if;

      Get_Value(Container => Result,
                Field     => "status",
                Target    => Execution_Report_Status,
                Found     => Found);

      if Found then
        Log(Me & "Make_Bet.Place_Bet", "Execution_Report_Status : '" & Execution_Report_Status & "'");
      end if;


      Get_Value(Container => Result,
                Field     => "errorCode",
                Target    => Execution_Report_Error_Code,
                Found     => Found);

      if Found then
        Log(Me & "Make_Bet.Place_Bet", "Execution_Report_Error_Code : '" & Execution_Report_Error_Code & "'");
      end if;


      if Result.Has_Field("instructionReports") then
        Instructions := Result.Get("instructionReports");
        Log(Me & "Make_Bet", "got result.instructionReports");

        Instructionsitem  := Get(Instructions, 1); -- always element 1, since we only have 1
        Log(Me & "Make_Bet", "got InstructionsItem");

        Get_Value(Container => Instructionsitem,
                  Field     => "status",
                  Target    => Instruction_Report_Status,
                  Found     => Found);

        if Found then
          Log(Me & "Make_Bet.Place_Bet", "Instruction_Report_Status : '" & Instruction_Report_Status & "'");
        end if;

        Get_Value(Container => Instructionsitem,
                  Field     => "errorCode",
                  Target    => Instruction_Report_Error_Code,
                  Found     => Found);
        if Found then
          Log(Me & "Make_Bet.Place_Bet", "Instruction_Report_Error_Code : '" & Instruction_Report_Error_Code & "'");
        end if;
      end if;

      Get_Value(Container => Instructionsitem,
                Field     => "instruction",
                Target    => Instruction,
                Found     => Found);

      if not Found then
        Log(Me & "Make_Bet", "NO Instruction in Instructions!!" );
        raise Json_Exception with "Betfair reply has no Instruction!";
      end if;

      Get_Value(Container => Instructionsitem,
                Field     => "betId",
                Target    => Bet_Id,
                Found     => Found);


      Get_Value(Container => Instructionsitem,
                Field     => "sizeMatched",
                Target    => L_Size_Matched,
                Found     => Found);

      if Found then
        Size_Matched := Bet_Size_Type(L_Size_Matched);
      end if;

      --      if abs(L_Size_Matched - Fixed_Type(Size)) < 0.0001 then
      if L_Size_Matched = Fixed_Type(Size) then
        Move( "EXECUTION_COMPLETE", Order_Status );
      else
        Move( "EXECUTABLE", Order_Status );
      end if;

      Get_Value(Container => Instructionsitem,
                Field     => "averagePriceMatched",
                Target    => Average_Price_Matched,
                Found     => Found);

      if Found then
        Price_Matched := Bet_Price_Type(Average_Price_Matched);
      end if;
    end ;

    if Trim(Execution_Report_Status) /= "SUCCESS" then
      Log(Me & "Make_Bet", "bad bet, get fake betid in bet_checker");
      Bet_Id := 0;
      Log(Me & "Make_Bet", "bad bet, set powerdays=0 to NOT try to cancel the bet");
      Powerdays := 0;
      --if not Sql.Is_Session_Open then
      --  begin
      --    if not Ini.Is_Loaded then
      --      Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
      --    end if;
      --    Log(Me, "Connect Db");
      --    Sql.Connect
      --          (Host     => Ini.Get_Value("database", "host", ""),
      --           Port     => Ini.Get_Value("database", "port", 5432),
      --           Db_Name  => Ini.Get_Value("database", "name", ""),
      --           Login    => Ini.Get_Value("database", "username", ""),
      --           Password => Ini.Get_Value("database", "password", ""));
      --    Log(Me, "db Connected");
      --    Bet_Id := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
      --    Log(Me & "Make_Bet", "bad bet, save it for later with dr betid");
      --    Log(Me, "Close Db");
      --    Sql.Close_Session;
      --  end;
      --end if;
    end if;

    Log(Me & "Make_Bet", "create bet");
    Bet := (
            Betid          => Bet_Id,
            Marketid       => Market_Id,
            Betmode        => Bot_Mode(Real),
            Powerdays      => Powerdays,
            Selectionid    => Selection_Id,
            Reference      => (others         => '-'),
            Size           => Fixed_Type(Local_Size),
            Price          => Fixed_Type(Local_Price),
            Side           => Side_String,
            Betname        => Bet_Name,
            Betwon         => False,
            Profit         => 0.0,
            Status         => Order_Status, -- ??
            Exestatus      => Execution_Report_Status,
            Exeerrcode     => Execution_Report_Error_Code,
            Inststatus     => Instruction_Report_Status,
            Insterrcode    => Instruction_Report_Error_Code,
            Startts        => Calendar2.Time_Type_First,
            Betplaced      => Now,
            Pricematched   => Fixed_Type(Price_Matched),
            Sizematched    => Fixed_Type(Size_Matched),
            Runnername     => Runner_Name,
            Fullmarketname => (others         => ' '),
            Svnrevision    => Bot_Svn_Info.Revision,
            Ixxlupd        => (others         => ' '), --set by insert
            Ixxluts        => Now              --set by insert
           );

    Log(Me & "Make_Bet.Place_Bet", "done");
  end Place_Bet;
  ------------------------------------------

  procedure Parse_Runners(J_Market      : in     Json_Value ;
                          Runner_List   : in out Runners.Lists.List) is
    Service            : constant String := "Parse_Runners";
    Db_Runner          : Runners.Runner_Type := Runners.Empty_Data;
    Found              : Boolean := False;
    --        "runners": [{
    --            "sortPriority": 1,
    --            "handicap": 0.00000E+00,
    --            "selectionId": 6271034,
    --            "runnerName": "1. Russelena Blue"
    --        },
    --  type Data_Type is record
    --      Marketid :    String (1..11) := (others => ' ') ; -- Primary Key
    --      Selectionid :    Integer_4  := 0 ; -- Primary Key
    --      Sortprio :    Integer_4  := 0 ; --
    --      Handicap :    Fixed_Type  := 0.0 ; --
    --      Runnername :    String (1..50) := (others => ' ') ; --
    --      Runnernamestripped :    String (1..50) := (others => ' ') ; -- non unique index 3
    --      Runnernamenum :    String (1..2) := (others => ' ') ; --
    --      Ixxlupd :    String (1..15) := (others => ' ') ; --
    --      Ixxluts :    Time_Type  := Time_Type_First ; --
    --  end record;
    Json_Runners_Array : Json_Array := Empty_Array;
    Json_Runner        : Json_Value := Create_Object;
    Array_Length       : Natural ;

    Runnernamestripped : String := Db_Runner.Runnernamestripped;
    Runnernamenum      : String := Db_Runner.Runnernamenum;
    Start_Paranthesis,
    Stop_Paranthesis   : Integer := 0;

  begin
    --  Log(Me & Service, "start");
    Json_Runners_Array := J_Market.Get("runners");
    Array_Length := Length (Json_Runners_Array);

    for J in 1 .. Array_Length loop
      Db_Runner := Runners.Empty_Data;
      Json_Runner := Get (Arr   => Json_Runners_Array, Index => J);
      Log(Me & Service, "  " & Json_Runner.Write);


      Get_Value(Container => J_Market,
                Field     => "marketId",
                Target    => Db_Runner.Marketid,
                Found     => Found);

      if not Found then
        raise No_Such_Field with "Object 'Market' - Field 'marketId'";
      end if;

      Get_Value(Container => Json_Runner,
                Field     => "sortPriority",
                Target    => Db_Runner.Sortprio,
                Found     => Found);
      if not Found then
        raise No_Such_Field with "Object 'Runner' - Field 'sortPriority'";
      end if;

      Get_Value(Container => Json_Runner,
                Field     => "handicap",
                Target    => Db_Runner.Handicap,
                Found     => Found);

      if not Found then
        raise No_Such_Field with "Object 'Runner' - Field 'handicap'";
      end if;

      Get_Value(Container => Json_Runner,
                Field     => "selectionId",
                Target    => Db_Runner.Selectionid,
                Found     => Found);
      if not Found then
        raise No_Such_Field with "Object 'Runner' - Field 'selectionId'";
      end if;

      Get_Value(Container => Json_Runner,
                Field     => "runnerName",
                Target    => Db_Runner.Runnername,
                Found     => Found);
      if not Found then
        raise No_Such_Field with "Object 'Runner' - Field 'runnerName'";
      end if;

      -- fix runner name
      Runnernamestripped := (others => ' ');
      Runnernamenum := (others => ' ');

      case Db_Runner.Runnername(1) is
        when '1'..'9' =>
          if Db_Runner.Runnername(2) = '.' and then
            Db_Runner.Runnername(3) = ' ' then
            Runnernamestripped := Db_Runner.Runnername(4 .. Db_Runner.Runnername'Last) & "   ";
            Runnernamenum := Db_Runner.Runnername(1..1) & ' ';
          elsif
            Db_Runner.Runnername(3) = '.' and then
            Db_Runner.Runnername(4) = ' ' then
            Runnernamestripped := Db_Runner.Runnername(5 .. Db_Runner.Runnername'Last) & "    ";
            Runnernamenum := Db_Runner.Runnername(1..2);
          else
            null;
          end if;

        when others =>
          Runnernamestripped := Db_Runner.Runnername;
          Move(Trim(Db_Runner.Sortprio'Img), Runnernamenum);
      end case;

      Move("NOT_SET_YET", Db_Runner.Status);

      Start_Paranthesis := -1;
      Stop_Paranthesis  := -1;

      for I in Runnernamestripped'Range loop
        case Runnernamestripped(I) is
          when '('    => Start_Paranthesis := I;
          when ')'    => Stop_Paranthesis  := I;
          when others => null;
        end case;
      end loop;

      if  Start_Paranthesis > Integer(-1) and then
        Stop_Paranthesis > Integer(-1) and then
        Lower_Case(Runnernamestripped(Start_Paranthesis .. Stop_Paranthesis)) = "(res)" then
        Runnernamestripped(Start_Paranthesis .. Stop_Paranthesis) := (others => ' ');
      end if;
      Db_Runner.Runnernamestripped := Runnernamestripped;
      Db_Runner.Runnernamenum      := Runnernamenum;

      Log(Me & Service, Db_Runner.To_String);

      begin
        Runner_List.Append(Db_Runner);
      exception
        when E: others =>
          Log(Me & Service, "WHAT HAPPENED HERE?");
          Stacktrace.Tracebackinfo(E);
      end ;
    end loop;
    Log(Me & Service, "stop");
  end Parse_Runners;

  ------------------------------------------
  procedure Parse_Market (J_Market       : in     Json_Value ;
                          Db_Market      : in out Markets.Market_Type ;
                          In_Play_Market :    out Boolean) is
    Service            : constant String := "Parse_Market";
    Eos,Found          : Boolean    := False;
    pragma Warnings(Off,Found);
    Event              : Json_Value := Create_Object;
    Market_Description : Json_Value := Create_Object;
    -- this routine parses replies from both
    -- * List_Market_Catalogue
    -- * List_Market_Book

    --List_Market_Catalogue
    --      "result": [{
    --        "marketId": "1.109863141",
    --        "event": {..},
    --        "eventType": {..},
    --        "runners": [{..},{..},{..} ... ],
    --        "marketName": "A4 480m",
    --        "marketStartTime": "2013-06-24T10:19:00.000Z"
    --        }]

    -- List_Market_Book
    --    "result": [{
    --        "numberOfWinners": 2,
    --        "betDelay": 0,
    --        "marketId": "1.109863158",
    --        "totalAvailable": 6.02089E+04,
    --        "bspReconciled": false,
    --        "numberOfRunners": 6,
    --        "numberOfActiveRunners": 6,
    --        "totalMatched": 0.00000E+00,
    --        "runners": [{ ... }],
    --        "inplay": false,
    --        "status": "OPEN",
    --        "runnersVoidable": false,
    --        "version": 540333571,
    --        "isMarketDataDelayed": false,
    --        "crossMatching": true,
    --        "complete": true

    --  type Data_Type is record
    --      Marketid :    String (1..11) := (others => ' ') ; -- Primary Key
    --      Marketname :    String (1..50) := (others => ' ') ; --
    --      Startts :    Time_Type  := Time_Type_First ; --
    --      Eventid :    String (1..11) := (others => ' ') ; -- non unique index 2
    --      Markettype :    String (1..6) := (others => ' ') ; -- non unique index 3
    --      Status :    String (1..50) := (others => ' ') ; -- non unique index 4
    --      Betdelay :    Integer_4  := 0 ; --
    --      Numwinners :    Integer_4  := 0 ; -- non unique index 5
    --      Numrunners :    Integer_4  := 0 ; --
    --      Numactiverunners :    Integer_4  := 0 ; --
    --      Totalmatched :    Fixed_Type  := 0.0 ; --
    --      Totalavailable :    Fixed_Type  := 0.0 ; --
    --      Ixxlupd :    String (1..15) := (others => ' ') ; --
    --      Ixxluts :    Time_Type  := Time_Type_First ; --
    --  end record;

  begin
    --Log(Me & Service, "start");
    In_Play_Market := False;
    Get_Value(Container => J_Market,
              Field     => "marketId",
              Target    => Db_Market.Marketid,
              Found     => Found);
    if Found then
      Db_Market.Read(Eos);
    end if;

    Get_Value(Container => J_Market,
              Field     => "marketName",
              Target    => Db_Market.Marketname,
              Found     => Found);


    Get_Value(Container => J_Market,
              Field     => "description",
              Target    => Market_Description,
              Found     => Found);
    if Found then
      Get_Value(Container => Market_Description,
                Field     => "marketType",
                Target    => Db_Market.Markettype,
                Found     => Found);
    end if;

    Get_Value(Container => J_Market,
              Field     => "marketStartTime",
              Target    =>  Db_Market.Startts,
              Found     => Found);

    Get_Value(Container => J_Market,
              Field     => "event",
              Target    => Event,
              Found     => Found);
    if Found then
      Get_Value(Container => Event,
                Field     => "id",
                Target    => Db_Market.Eventid,
                Found     => Found);
    end if;

    Get_Value(Container => J_Market,
              Field     => "inplay",
              Target    => In_Play_Market,
              Found     => Found);
    -- update start, ie these fields are in Market_Book only

    Get_Value(Container => J_Market,
              Field     => "numberOfWinners",
              Target    => Db_Market.Numwinners,
              Found     => Found);

    Get_Value(Container => J_Market,
              Field     => "totalAvailable",
              Target    => Db_Market.Totalavailable,
              Found     => Found);

    Get_Value(Container => J_Market,
              Field     => "numberOfRunners",
              Target    => Db_Market.Numrunners,
              Found     => Found);


    Get_Value(Container => J_Market,
              Field     => "numberOfActiveRunners",
              Target    => Db_Market.Numactiverunners,
              Found     => Found);


    Get_Value(Container => J_Market,
              Field     => "totalMatched",
              Target    => Db_Market.Totalmatched,
              Found     => Found);


    Get_Value(Container => J_Market,
              Field     => "status",
              Target    => Db_Market.Status,
              Found     => Found);


    Get_Value(Container => J_Market,
              Field     => "betDelay",
              Target    => Db_Market.Betdelay,
              Found     => Found);


    Log(Me & Service, "In_Play_Market: " & In_Play_Market'Img & " " & Db_Market.To_String);
    --    Log(Me & Service, "stop");

  end Parse_Market;
  -----------------------------------------------

  procedure Get_Navigation_Data( Menu : in out Json_Value) is
    Aws_Reply    : Aws.Response.Data;
    Http_Headers : Aws.Headers.List := Aws.Headers.Empty_List;
  begin
    Log(Me, "Get_Navigation_Data start");
    Aws.Headers.Add (Http_Headers, "X-Authentication", Global_Token.Get);
    Aws.Headers.Add (Http_Headers, "X-Application", Global_Token.Get_App_Key);
    Aws.Headers.Add (Http_Headers, "Accept", "application/json");
    Aws_Reply := Aws.Client.Get (Url          => Token.Url_Navigation_Data,
                                 Headers      => Http_Headers,
                                 Timeouts     => Aws.Client.Timeouts (Each => 1200.0));
    Log(Me & "Get_Navigation_Data", "Got reply, check it ");

    if String'(Aws.Response.Message_Body(Aws_Reply)) = "Get Timeout" then
      Log(Me & "Get_Navigation_Data", "Get Timeout -> Give up!");
      raise Get_Timeout ;
    end if;
    Menu := Read (Strm     => String'(Aws.Response.Message_Body(Aws_Reply)),
                  Filename => "");
    ---Log(Me, Menu.Write );
    Log(Me, "Get_Navigation_Data stop");
  exception
    when Post_Timeout => raise;
    when others =>
      Log(Me & "Get_Navigation_Data", "***********************  Bad reply start *********************************");
      Log(Me & "Get_Navigation_Data", "Bad reply " & Aws.Response.Message_Body(Aws_Reply));
      Log(Me & "Get_Navigation_Data", "***********************  Bad reply stop  ********" );
      raise Bad_Reply ;
  end Get_Navigation_Data;


  --------------------------------

  procedure Get_Starttimes(List : out Table_Astarttimes.Astarttimes_List_Pack2.List) is
    Json_Query   : Json_Value := Create_Object;
    Json_Reply   : Json_Value := Create_Object;
    Result_Array : Json_Array := Empty_Array;

    Result       : Json_Value := Create_Object;
    Found        : Boolean    := False;


    Params               : Json_Value := Create_Object;
    Filter               : Json_Value := Create_Object;


    Event                : Json_Value := Create_Object;
    Market_Start_Time    : Json_Value := Create_Object;
    Market_Projection,
    Market_Countries,
    Market_Type_Codes,
    Exchange_Ids,
    Event_Type_Ids       : Json_Array := Empty_Array;

    Now       : Calendar2.Time_Type := Calendar2.Clock;
    From      : Calendar2.Time_Type := Now;
    To        : Calendar2.Time_Type := Now;
    Starttime : Calendar2.Time_Type := Now;
    List_Data : Table_Astarttimes.Data_Type;

    One_Hour           : Calendar2.Interval_Type := (0,1,0,0,0);
    Two_Hours          : Calendar2.Interval_Type := (0,2,0,0,0);
    Utc_Offset_Minutes : Ada.Calendar.Time_Zones.Time_Offset;
    use type  Calendar2.Time_Type;

    No_Such_Utc_Offset : exception;

  begin
    -- Create JSON arrays
    Append(Exchange_Ids , Create("1"));      -- Not Australia

    Append(Event_Type_Ids , Create("7"));    -- horse
    --   none for all countries
    Append(Market_Countries , Create("GB"));
    Append(Market_Countries , Create("IE"));
    Append(Market_Type_Codes , Create("WIN"));                 -- for horses/hounds
    Append(Market_Projection , Create("MARKET_START_TIME"));
    Append(Market_Projection , Create("EVENT"));

    From.Hour := 0;
    From.Minute := 0;
    From.Second := 0;
    From.Millisecond := 0;

    To.Hour := 23;
    To.Minute := 59;
    To.Second := 59;
    To.Millisecond := 999;

    Market_Start_Time.Set_Field(Field_Name => "from", Field => From.String_Date_Time_Iso);
    Market_Start_Time.Set_Field(Field_Name => "to",   Field => To.String_Date_Time_Iso);

    Filter.Set_Field (Field_Name => "exchangeIds",        Field => Exchange_Ids);
    Filter.Set_Field (Field_Name => "eventTypeIds",       Field => Event_Type_Ids);
    Filter.Set_Field (Field_Name => "marketCountries",    Field => Market_Countries);
    Filter.Set_Field (Field_Name => "marketTypeCodes",    Field => Market_Type_Codes);
    Filter.Set_Field (Field_Name => "marketStartTime",    Field => Market_Start_Time);

    Params.Set_Field (Field_Name => "filter",           Field => Filter);
    Params.Set_Field (Field_Name => "marketProjection", Field => Market_Projection);
    Params.Set_Field (Field_Name => "locale",           Field => "en"); --
    Params.Set_Field (Field_Name => "sort",             Field => "FIRST_TO_START");
    Params.Set_Field (Field_Name => "maxResults",       Field => "999");

    Json_Query.Set_Field (Field_Name => "params",  Field => Params);
    Json_Query.Set_Field (Field_Name => "id",      Field => 15);          -- ???
    Json_Query.Set_Field (Field_Name => "method",  Field => "SportsAPING/v1.0/listMarketCatalogue");
    Json_Query.Set_Field (Field_Name => "jsonrpc", Field => "2.0");


    --{
    --     "jsonrpc": "2.0",
    --     "method": "SportsAPING/v1.0/listMarketCatalogue",
    --     "params": {
    --          "filter": {
    --               "eventTypeIds": ["7"],
    --               "marketCountries": ["IE","GB"],
    --               "marketTypeCodes": ["WIN"],
    --               "marketStartTime": {
    --                    "from": "2015-08-10T00:00:00Z",
    --                    "to": "2015-08-10T21:30:00Z"
    --               }
    --          },
    --          "sort": "FIRST_TO_START",
    --          "maxResults": "100",
    --          "marketProjection": ["MARKET_START_TIME","EVENT"]
    --     },
    --     "id": 1
    --}

    Get_Json_Reply(Query => Json_Query,
                   Reply => Json_Reply,
                   Url   => Token.Url_Betting);

    if Api_Exceptions_Are_Present(Json_Reply) then
      return ;
    end if;

    --[{
    --     "jsonrpc": "2.0",
    --     "result": [{
    --          "marketId": "1.119947782",
    --          "marketName": "1m Hcap",
    --          "marketStartTime": "2015-08-10T14:45:00.000Z",
    --          "totalMatched": 2220769.95076,
    --          "event": {
    --               "id": "27508851",
    --               "name": "Wolv 10th Aug",
    --               "countryCode": "GB",
    --               "timezone": "Europe/London",
    --               "venue": "Wolverhampton",
    --               "openDate": "2015-08-10T13:15:00.000Z"
    --          }
    --     },
    --     {
    --          "marketId": "1.119947756",
    --          "marketName": "5f Hcap",
    --          "marketStartTime": "2015-08-10T15:00:00.000Z",
    --          "totalMatched": 688189.1516000001,
    --          "event": {
    --               "id": "27508850",
    --               "name": "Ayr 10th Aug",
    --               "countryCode": "GB",
    --               "timezone": "Europe/London",
    --               "venue": "Ayr",
    --               "openDate": "2015-08-10T13:00:00.000Z"
    --          }
    --     }],
    --     "id": 1
    --}]


    if Json_Reply.Has_Field("result") then
      Result_Array := Json_Reply.Get("result");

      if Length(Result_Array) > Natural(0) then
        for I in 1 .. Length(Result_Array) loop
          Result := Get(Result_Array,I);

          Get_Value(Container => Result,
                    Field     => "marketStartTime",
                    Target    => Starttime,
                    Found     => Found);
          if not Found then
            Log(Me & "Get_Starttimes", "NO starttime!!" );
          end if;

          Utc_Offset_Minutes := Ada.Calendar.Time_Zones.Utc_Time_Offset;
          case Utc_Offset_Minutes is
            when 60     => List_Data.Starttime := Starttime + One_Hour;
            when 120    => List_Data.Starttime := Starttime + Two_Hours;
            when others => raise No_Such_Utc_Offset with Utc_Offset_Minutes'Img;
          end case;

          Get_Value(Container => Result,
                    Field     => "event",
                    Target    => Event,
                    Found     => Found);
          if not Found then
            Log(Me & "Get_Starttimes", "NO event!!" );
          end if;

          Get_Value(Container => Result,
                    Field     => "marketName",
                    Target    => List_Data.Marketname,
                    Found     => Found);
          if not Found then
            Log(Me & "Get_Starttimes", "NO marketName!!" );
          end if;


          Get_Value(Container => Event,
                    Field     => "venue",
                    Target    => List_Data.Venue,
                    Found     => Found);
          if not Found then
            Log(Me & "Get_Starttimes", "NO venue!!" );
          end if;

          List.Append(List_Data);

        end loop;

      else
        Log(Me & "Get_Starttimes", "NO RESULT!! 3 " );
        return ;
      end if;
    else
      Log(Me & "Get_Starttimes", "NO RESULT!! 4" );
      return ;
    end if;



  end Get_Starttimes;

end Rpc;
