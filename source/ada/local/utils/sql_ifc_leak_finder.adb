
--with Types ; use Types;
with Stacktrace;
with Sql;

--with Gnat.Command_Line; use Gnat.Command_Line;
--with GNAT.Strings;
with Logging; use Logging;
--with Calendar2; use Calendar2;



procedure Sql_Ifc_Leak_Finder is

--   T            : Sql.Transaction_Type;
 --  Select_All   : Sql.Statement_Type;

 --  Eos          : Boolean := False;

 --  start_date   : Calendar2.time_type := Calendar2.Time_Type_First;
--   stop_date    : Calendar2.time_type := Calendar2.Time_Type_First;


--   Sa_Date      : aliased Gnat.Strings.String_Access;
--   I_Num_Days   : aliased Integer := 200;
--   Config       : Command_Line_Configuration;
--   cnt : integer := 0;

begin
--   Define_Switch
--     (Config,
--      Sa_Date'access,
--      "-d:",
--      Long_Switch => "--date=",
--      Help        => "when the data move starts yyyy-mm-dd");
--
--   Define_Switch
--     (Config,
--      I_Num_Days'access,
--      "-n:",
--      Long_Switch => "--num_days=",
--      Help        => "days to move");
--
--
--   Getopt (Config);  -- process the command line


   Log ("Connect db");
   Sql.Connect
     (Host     => "nonodev.com",
      Port     => 5432,
      Db_Name  => "bnl",
      Login    => "bnl",
      Password => "BettingFotboll1$");
   Log ("Connected db");

--   Sql.Prepare (Select_All,
--                   "select * from AMARKETS " &
--                   "where STARTTS >= :START " &
--                   "and STARTTS <= :STOP " &
--                   "and 1 =2 " &  --impossilble
--                   "");
--
--   Start_Date := Calendar2.To_Time_Type ("2011-01-01", "00:00:00:000");
--   Stop_Date  := Calendar2.To_Time_Type ("2011-03-01", "23:59:59:999");
--
--   Sql.Set_Timestamp(Select_all, "START", Start_date);
--   Sql.Set_Timestamp(Select_all, "STOP",  Stop_date);
--
--   for i in 0 .. I_Num_Days loop
--   Sql.Start_Read_Write_Transaction (T);
--         Cnt := 0;
--         Sql.Open_Cursor(Select_All);
--         loop
--           Cnt := Cnt + 1;
--           Log ("turn #" & i'img & Cnt'Img & " " & eos'img);
--           Sql.Fetch(Select_All,Eos);
--           exit when Eos;
--         end loop;
--         Sql.Close_Cursor(Select_All);
--   Sql.Commit (T);
--   end loop;
--
--   Log ("wait 25 before close");
--   delay 25.0;

   Sql.Close_Session;
--   Log ("wait 25 before die");
--   delay 25.0;

exception
   when E : others =>
      Stacktrace.Tracebackinfo (E);

end Sql_Ifc_Leak_Finder;
