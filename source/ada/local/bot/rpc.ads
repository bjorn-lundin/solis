with botcoll.Json; use botcoll.Json;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Runners;
with Markets;
with Balances;
with Bets;
with Prices;
with Events;
with Token;
with Calendar2;
with Table_Astarttimes;

package RPC is

  type Result_Type is (Ok, Timeout, Logged_Out);
  JSON_Exception,
  Post_Timeout,
  Get_Timeout,
  Invalid_Session,
  Login_Failed,
  Data_Missing,
  Bad_Reply: exception;

  procedure Login;
  procedure Logout;
  procedure Init(Username   : in String;
                 Password   : in String;
                 Product_Id : in String;
                 Vendor_Id  : in String;
                 App_Key    : in String) ;



  function Get_Token return Token.Token_Type ;
  procedure Keep_Alive (Result : out Boolean);


  function API_Exceptions_Are_Present(Reply : JSON_Value) return Boolean ;


  procedure Get_Value(Container: in     JSON_Value;
                      Field    : in     String;
                      Target   : in out Boolean;
                      Found    :    out Boolean );

  procedure Get_Value(Container: in     JSON_Value;
                      Field    : in     String;
                      Target   : in out Fixed_Type;
                      Found    :    out Boolean );

  procedure Get_Value(Container: in     JSON_Value;
                      Field    : in     String;
                      Target   : in out Integer_8;
                      Found    :    out Boolean );

  procedure Get_Value(Container: in     JSON_Value;
                      Field    : in     String;
                      Target   : in out String;
                      Found    :    out Boolean);

  procedure Get_Value(Container: in     JSON_Value;
                      Field    : in     String;
                      Target   : in out JSON_Value;
                      Found    :    out Boolean);

  procedure Get_Value(Container: in     JSON_Value;
                      Field    : in     String;
                      Target   : in out Integer_4;
                      Found    :    out Boolean);

  procedure Get_Value(Container: in     JSON_Value;
                      Field    : in     String;
                      Target   : in out Calendar2.Time_Type;
                      Found    :    out Boolean);

  procedure Bet_Is_Matched(Betid             : in  Integer_8 ;
                           Is_Removed        : out Boolean;
                           Is_Matched        : out Boolean;
                           AVG_Price_Matched : out Bet_Price_Type;
                           Size_Matched      : out Bet_Size_Type) ;

  procedure Market_Status_Is_Changed(Market     : in out Markets.Market_Type;
                                     Is_Changed :    out Boolean);

  procedure Get_Balance(Betfair_Result : out Result_Type ;
                        Saldo          : out Balances.Balance_Type) ;


  procedure Check_Market_Result(Market_Id   : in     Marketid_Type;
                                Runner_List : in out Runners.Lists.List);


  procedure Get_Cleared_Bet_Info_List(Bet_Status     : in     Bet_Status_Type;
                                      Settled_From   : in     Calendar2.Time_Type := Calendar2.Time_Type_First;
                                      Settled_To     : in     Calendar2.Time_Type := Calendar2.Time_Type_Last;
                                      Betfair_Result :    out Result_Type;
                                      Bet_List       :    out Bets.Lists.List) ;

  procedure Cancel_Bet(Market_Id : in Marketid_Type;
                       Bet_Id    : in Integer_8);

  function  Cancel_Bet(Bet : in Bets.Bet_Type) return Boolean;

  procedure Get_Market_Prices(Market_Id  : in     Marketid_Type;
                              Market     :    out Markets.Market_Type;
                              Price_List : in out Prices.Lists.List;
                              In_Play    :    out Boolean);


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
                       Bet              :    out Bets.Bet_Type);

  procedure Parse_Runners(J_Market    : in     JSON_Value ;
                          Runner_List : in out Runners.Lists.List) ;

  procedure Parse_Prices (J_Market    : in     JSON_Value ;
                          Price_List  : in out Prices.Lists.List) ;

  procedure Parse_Market (J_Market    : in     JSON_Value ;
                          DB_Market   : in out Markets.Market_Type ;
                          In_Play_Market :    out Boolean) ;

  procedure Parse_Event (J_Event, J_Event_Type : in     JSON_Value ;
                         DB_Event              : in out Events.Event_Type) ;

  procedure Get_JSON_Reply (Query : in     JSON_Value;
                            Reply : in out JSON_Value;
                            URL   : in     String) ;


  procedure Get_Navigation_Data(Menu : in out JSON_Value);



  procedure Get_Starttimes(List : out Table_Astarttimes.Astarttimes_List_Pack2.List);

end RPC;
