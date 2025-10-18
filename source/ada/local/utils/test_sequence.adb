
with Types ; use Types;
with Stacktrace;
with Sql;

--with Gnat.Command_Line; use Gnat.Command_Line;
--with GNAT.Strings;
with Logging; use Logging;
--with Calendar2; use Calendar2;
with Bot_System_Number;


procedure Test_Sequence is

Bet_id : Integer_8 := 0;

begin


   Log ("Connect db");
   Sql.Connect
     (Host     => "db.nonodev.com",
      Port     => 5432,
      Db_Name  => "prod_test_bnl",
      Login    => "bnl",
      Password => "BettingFotboll1$");
   Log ("Connected db");

   log("Test_Sequence", "before");
   Bet_id := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
   log("Test_Sequence", "after, Bet_id:" & Bet_id'Img);   
   Sql.Close_Session;

   
exception
   when E : others =>
      Stacktrace.Tracebackinfo (E);

end Test_Sequence;
