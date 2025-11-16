
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_Io;
with Ada.Directories;
with Ada.Exceptions;        use Ada.Exceptions;
with Types; use Types;
with Calendar2;
with Text_IO;
with Botcoll.json; use Botcoll.json;
with Table_Prices;
with Sql;

procedure Set_Times is


-- check date
-- calculate avg price for that date
-- from now until 24:00 check prices
-- if the price is below avg - add to OK_to_charge, else not
-- for each record for the remainding of the day,
-- add each 15th min seg to a list, with marking
-- not-allowed-to-charge or
-- allowed-to-charge.
--
   T : Sql.Transaction_Type;
   select_today : sql.Statement_Type;

   Prices_List : table_prices.Prices_List_Pack2.list;
   Allowed_List : table_prices.Prices_List_Pack2.list;
   Switch_List : table_prices.Prices_List_Pack2.list;
   Prices_Data : table_prices.data_type;

   Now : Calendar2.Time_Type := Calendar2.Clock;
   use type Calendar2.Time_Type;
   a_day_later : Calendar2.Time_Type := Now + (1,0,0,0,0); -- add 1 day

   avg : float := 0.0;
   count : integer := 0;

   Num_Shifts : integer := 0;
   Is_Allowed : boolean := false;
   Prev_Is_Allowed : boolean := false;
   Is_First : boolean := true;
   Start,Stop : Calendar2.Time_Type := Calendar2.Clock;

begin

   Sql.Connect(Host     => "localhost",
               Port     => 5432,
               Db_Name  => "bnl",
               Login    => "bnl",
               Password => "ld4BC9Q51FU9CYjC21gp");

   T.Start;     
   select_today.prepare("select * from PRICES where TIME_START >= :NOW and TIME_STOP <= :A_DAY_LATER order by EPOCH");

   select_today.set("NOW", Now);
   select_today.set("A_DAY_LATER", a_day_later);

   Table_Prices.Read_List (Stm => select_today, List => prices_list);
   for rec of prices_list loop
      -- process each record
      Text_IO.Put_Line (rec.to_string);
      avg := avg + rec.Sek_Per_Kwh;
      count := count + 1;
   end loop;
   
   avg := avg / float(count);
   Text_IO.Put_Line ("Average price for today is: " & avg'img);

   for Rec of Prices_List loop
      -- process each record
      if Rec.Sek_Per_Kwh <= avg then
         Rec.Exchange_Rate := 1.0;
      else
         Rec.Exchange_Rate := 0.0;
      end if;     
   end loop;

   Num_Shifts := 1;

   Prices_Data := Prices_List.First_Element;
   Prev_Is_Allowed := Prices_Data.Exchange_Rate <= 0.99;
   Start := Prices_Data.Time_Start;

   for Rec of Prices_List loop
      -- process each record
      Is_Allowed := Rec.Exchange_Rate <= 0.99;

      if Is_Allowed /= Prev_Is_Allowed then
         Text_IO.Put_Line ("  -------------------------  ");
         Text_IO.Put_Line ("  Change at shift #: " & Num_Shifts'img);
         Text_IO.Put_Line ("  start: " & Start.To_String);
         Text_IO.Put_Line ("  stop: " & Stop.To_String);
         Text_IO.Put_Line ("  charge: " & Is_Allowed'img);
         Text_IO.Put_Line ("  -------------------------  ");

         Num_Shifts := Num_Shifts + 1;  
         Stop := Rec.Time_Stop;    
         
         Prices_Data := Rec;
         Prices_Data.Time_Start := Start;
         Prices_Data.Time_Stop := Stop - (0,0,16,0,0); -- minus 16 minutes
                 
         Switch_List.Append(Prices_Data); 
         Start := Rec.Time_Start;      
      end if;
      Prev_Is_Allowed := Is_Allowed;
      exit when Num_Shifts > 5;
      Is_First := false;
   end loop;
      Text_IO.Put_Line ("");
      Text_IO.Put_Line ("-----------------------");

   for Rec of Switch_List loop
      Text_IO.put_line("Charge OK:" & Boolean'Image(Rec.Exchange_Rate >= 0.99));
      Text_IO.Put_Line ("Switch at: " & Rec.To_String);
   end loop;

   T.Commit;
   Sql.Close_Session;
end Set_Times;
