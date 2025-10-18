--with Gnat.Command_Line; use Gnat.Command_Line;
with Types;    use Types;
with Sql;
with Calendar2; use Calendar2;
--with Logging;               use Logging;
with Text_Io;
with Ini;
with  Ada.Environment_Variables;
with Stacktrace;
with Table_Okmarkets;
with Table_Arunners;
with Markets;

procedure Create_Ok_Markets is
   package Ev renames Ada.Environment_Variables;
   --  Cmd_Line              : Command_Line_Configuration;
   T                     : Sql.Transaction_Type with Warnings => Off;
   Select_Markets        : Sql.Statement_Type;
   Select_Num_Samples    : Sql.Statement_Type;


   Gdebug : Boolean := True;


   -------------------------------
   procedure Debug (What : String) is
   begin
      if Gdebug then
         Text_Io.Put_Line (Text_Io.Standard_Error, Calendar2.String_Date_Time_Iso (Clock, " " , "") & " " & What);
      end if;
   end Debug;
   pragma Warnings(Off, Debug);
   -------------------------------
   procedure Print (What : String) with Unreferenced is
   begin
      Text_Io.Put_Line (What);
   end Print;
   -------------------------------

   procedure Get_Market_Data(Market_List  : in out  Markets.Lists.List) is

   begin

      Select_Markets.Prepare( "select M.* " &
                                "from AMARKETS M " &
                                "where true " &
                                "and M.NUMACTIVERUNNERS >= 8 " &
                                "and M.NUMACTIVERUNNERS <= 16 " &
                                "and M.MARKETTYPE = 'WIN' " &
                              --  "and STARTTS::date < '2017-01-01' " &
                                "order by M.STARTTS");

      Markets.Read_List(Select_Markets, Market_List);

     -- Table_Amarkets.Read_List(Select_Markets, Market_List);
   end Get_Market_Data;
   ------------------------------------------------------

   procedure Insert_Into_Ok_If_Ok(Market  : in out Markets.Market_Type) is
      Ok_Market   : Table_Okmarkets.Data_Type;
      Eos         : Boolean := False;
      Num_Samples : Integer_4 := 0;
      Winner      : Table_Arunners.Data_Type;
      Runner_List : Table_Arunners.Arunners_List_Pack2.List;
   begin
      if not Market.Marketname_Ok then
         Debug ("marketname not ok " & Market.Marketname);
         return;
      end if;

      Winner.Marketid := Market.Marketid;
      --Winner.Status(1..6) := "WINNER";


      Eos := True;
      Winner.Read_I1_Marketid(Runner_List);
      for R of Runner_List loop
         if R.Status(1..6) = "WINNER" then
            Winner := R;
            Eos := False;
            exit;
         end if;
      end loop;

      --does not WORK
      --    Winner.Status(1..6) := "WINNER";
      --    Winner.Read_One_Status( Order  => False, End_Of_Set =>  Eos);

      if Eos then
         Debug ("no winner: " & Market.To_String);
         return;
      end if;

      Select_Num_Samples.Prepare("select count('a') CNT from APRICESHISTORY " &
                                   "where MARKETID = :MARKETID and SELECTIONID = :SELECTIONID");
      Select_Num_Samples.Set("MARKETID", Market.Marketid);
      Select_Num_Samples.Set("SELECTIONID", Winner.Selectionid);

      Select_Num_Samples.Open_Cursor;
      Select_Num_Samples.Fetch(Eos);
      if not Eos then
         Select_Num_Samples.Get("CNT", Num_Samples);
         --  Debug (Market.Marketid & Winner.Selectionid'img & " -> " & num_samples'img);
      end if;
      Select_Num_Samples.Close_Cursor;

      if Num_Samples > 60 then
         Ok_Market := (
                       Marketid    => Market.Marketid,
                       Eventid     => Market.Eventid,
                       Markettype  => Market.Markettype,
                       Numwinners  => Market.Numwinners,
                       Numrunners  => Market.Numrunners,
                       Ixxlupd     => Market.Ixxlupd,
                       Ixxluts     => Market.Ixxluts);
         Ok_Market.Read(Eos);
         --  Debug (Market.Marketid &  " -> " & eos'img);
         if Eos then
            Ok_Market.Insert;
            Debug ("inserted " & Ok_Market.To_String);
         end if;
      else
         Debug ("Too few samples" & Num_Samples'Img & " eos: " & Eos'Img & " -> " & Winner.To_String);
      end if;

   end Insert_Into_Ok_If_Ok;
   -----------------------------------------

   Mlist : Markets.Lists.List;
   C     : Integer_4 := 0;
begin

   --  Getopt (Cmd_Line);  -- process the command line

   Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");

   Debug("Connect Db");
   Sql.Connect
     (Host     => Ini.Get_Value("database_home","host",""),
      Port     => Ini.Get_Value("database_home","port", 5432),
      Db_Name  => Ini.Get_Value("database_home","name",""),
      Login    => Ini.Get_Value("database_home","username",""),
      Password => Ini.Get_Value("database_home","password",""),
      Ssl_Mode => "prefer");
  Debug("db Connected");


  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","create_ok_markets");
  end if;


   T.Start;
   Get_Market_Data(Mlist);

   for M of Mlist loop
      C := C +1;
      if C rem 1000 = 0 then
         Debug(C'Img & " / " & Mlist.Length'Img);
      end if;

      Insert_Into_Ok_If_Ok(M);
   end loop;

   T.Commit;
   Sql.Close_Session;

exception
   when E: others =>
      Stacktrace.Tracebackinfo(E);
end Create_Ok_Markets;
