with Ada.Command_Line;
with Ada.Exceptions;

with Aws;
with Aws.Headers;
--with Aws.Headers.Set;
with Aws.Response;
with Aws.Client;

with botcoll.Json; use botcoll.Json;
with Text_io;
with Stacktrace;



procedure AI_Utils is

  Req    : Json_Value := Create_Object;
  Params : Json_Value := Create_Object;
  Odds   : Json_Array := Empty_Array;
  Aws_Reply    : Aws.Response.Data;
  Http_Headers : Aws.Headers.List := Aws.Headers.Empty_List;

begin

  --
  --      {
  --          "method" : "AI",
  --          "id" : 82,
  --          "jsonrpc" : "2.0",
  --          "params" : {
  --             "betType" : "place",
  --             "side" : "back",
  --             "hiddenNodes" : 100,
  --             "learningRate" : 0.5,
  --             "numFromLeader" : 0,
  --             "epochs": 6,
  --             "input" : [1.01, 2.20, 5.0, 4.0, 3.8, 2.0, 2.10, 3.10, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
  --             }
  --          }


  Req.Set_Field (Field_Name => "method", Field => "AI");
  Req.Set_Field (Field_Name => "id", Field => 15);
  Req.Set_Field (Field_Name => "jsonrpc", Field => "2.0");

  Params.Set_Field (Field_Name => "betType", Field => "place");
  Params.Set_Field (Field_Name => "side", Field => "back");
  Params.Set_Field (Field_Name => "hiddenNodes", Field => 100);
  Params.Set_Field (Field_Name => "learningRate", Field => 0.5);
  Params.Set_Field (Field_Name => "numFromLeader", Field => 0);
  Params.Set_Field (Field_Name => "epochs", Field => 6);

  Append(Odds,Create(1.01));
  Append(Odds,Create(2.20));
  Append(Odds,Create(5.0));
  Append(Odds,Create(4.0));
  Append(Odds,Create(3.8));
  Append(Odds,Create(2.0));
  Append(Odds,Create(2.10));
  Append(Odds,Create(3.10));
  Append(Odds,Create(0.0));
  Append(Odds,Create(0.0));
  Append(Odds,Create(0.0));
  Append(Odds,Create(0.0));
  Append(Odds,Create(0.0));
  Append(Odds,Create(0.0));
  Append(Odds,Create(0.0));
  Append(Odds,Create(0.0));

  Params.Set_Field (Field_Name => "input", Field => Odds);
  Req.Set_Field (Field_Name => "params", Field => Params);


  Aws.Headers.Add (Http_Headers, "Accept", "application/json");

  Aws.Client.Set_Debug(On => True);
  Aws_Reply := Aws.Client.Post (Url          => "http://127.0.0.1:12345/AI",
                                Data         => Req.Write,
                                Content_Type => "application/json",
                                Headers      => Http_Headers,
                                Timeouts     => Aws.Client.Timeouts (Each => 30.0));

  Text_Io.Put_Line(Aws.Response.Message_Body(Aws_Reply));


exception
  when E: others =>
    declare
      Last_Exception_Name     : constant String  := Ada.Exceptions.Exception_Name(E);
      Last_Exception_Messsage : constant String  := Ada.Exceptions.Exception_Message(E);
      Last_Exception_Info     : constant String  := Ada.Exceptions.Exception_Information(E);
    begin
      Text_Io.Put_Line(Last_Exception_Name);
      Text_Io.Put_Line("Message : " & Last_Exception_Messsage);
      Text_Io.Put_Line(Last_Exception_Info);
      Text_Io.Put_Line("addr2line" & " --functions --basenames --exe=" &
            Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump(Last_Exception_Info));
    end ;

end AI_Utils;
