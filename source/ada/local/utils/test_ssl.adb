
with Ada.Environment_Variables;
with Logging; use Logging;
with Aws; use Aws;
with Aws.Headers;
pragma Elaborate_All (AWS.Headers);
with Rpc;
with Ini;
--with Token;

procedure Test_Ssl is
 --  Me : constant String := "RPC.";
 --  Global_Token : Token.Token_Type;
   package Ev renames Ada.Environment_Variables;
   use type Rpc.Result_Type;
begin
   Ini.Load (Ev.Value ("BOT_HOME") & "/" & "login.ini");
   Rpc.Init (
             Username   => Ini.Get_Value ("betfair", "username", ""),
             Password   => Ini.Get_Value ("betfair", "password", ""),
             Product_Id => Ini.Get_Value ("betfair", "product_id", ""),
             Vendor_Id  => Ini.Get_Value ("betfair", "vendor_id", ""),
             App_Key    => Ini.Get_Value ("betfair", "appkey", "")
            );
   Log ("Login", "start");
   Rpc.Login;
end Test_Ssl;
