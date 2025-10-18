

with Process_io;
with Bot_Messages;
--with bot_types; use bot_types;

procedure Test_Pio is
    PLB             : Bot_Messages.Place_Lay_Bet_Record ;


begin
 PLB.Bet_name(1..6)   := ("DR_BNL"); 
 PLB.Market_Id(1..5) := ("3.123");
 PLB.Selection_Id    := 1;
 PLB.Size(1..3) := "1.0";
 PLB.Price(1..3) := "1.0";
 Bot_Messages.Send(Process_io.To_Process_Type("bet_placer_101"), PLB);


end Test_Pio;