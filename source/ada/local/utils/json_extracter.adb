with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_Io;
with Ada.Containers.Vectors;
--with Ada.Environment_Variables;
with Ada.Directories;
with ada.exceptions;        use ada.exceptions;

--with bot_types; use bot_types;
with Types;
with Calendar2;
with Text_io;
with botcoll.json; use botcoll.json;
with table_solis;
with table_smhi;
with table_prices;
with sql;

procedure Json_Extracter is

   --    package Directory_List_Package is new Ada.Containers.Doubly_Linked_Lists(String_Object);
   --    package A_Sorter is new Directory_List_Package.Generic_Sorting;
   --    File_List : Directory_List_Package.List;

   procedure Log (w : string) is
   begin
      text_io.put_line (w);
   end Log;



 package unbounded_String_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => unbounded_String);

use unbounded_String_Vectors;
  ---------------------------------------------------

   procedure Split (V: in out Unbounded_String_Vectors.Vector; Source, Pattern: String) is
      Start: Positive := 1;
      Position: Natural;
      Num_Parts: Natural := 0;
   begin
       loop
         Position := Ada.Strings.Fixed.Index (Source, Pattern, Start);
         exit when Position = 0;
         V.Append (To_Unbounded_String( Source (Start .. Position - 1)));
         Start := Position + 1;
      end loop;
      Num_Parts := Num_Parts + 1;
      V.Append (To_Unbounded_String( Source(Start .. Source'Last)));
   end Split;

   -----------------------------------------------------

   procedure On_Solis_Data (Data : string) is
     V : Unbounded_String_Vectors.Vector;   
     Sep : String(1..1);
     --startline-1 = 'InverterList call success:'
     --endline+1 = 'InverterDetails call success:'
     Start_Line,
     End_Line   : Natural := 0;
     Cnt        : Natural := 0;     
     J          : Json_Value := Create;
     Data_Items : Json_Array := Empty_Array;
     Data_Item  : Json_Value := Create_Object;
     Data_Holder : Unbounded_String;
     solis_data : table_solis.data_type;
     eos : Boolean := false;
     idx : Natural := 0;  
     Treat : Boolean := false;   
   begin
     Sep(1) := ASCII.LF;
     Split (V, Data, Sep);
     for Item of V loop
        Cnt := Cnt +1;
        if Start_Line = 0
          and then To_String(Item) = "InverterList call success:"
        then
          Start_Line := Cnt ;
        end if;  
     
        if End_Line = 0
          and then To_String(Item) = "InverterDetails call success:"
        then
          End_Line := Cnt - 2;
          exit;
        end if;  
     
--        Log ("line :" & To_String (I));
--        Log ("-----------------------");
     end loop;


     Log ("Start_Line,End_Line :" & Start_Line'img & End_Line'img);
     
     for i in start_Line .. End_Line loop
       log (i'img & To_String(V(i)));
       Treat := true;
       Idx := Ada.Strings.Fixed.Index (Source => To_String(V(i)), Pattern => "stationName");
       if Idx > 0 then
           Treat := false;
       end if;  
       if Treat then
           Idx := Ada.Strings.Fixed.Index (Source => To_String(V(i)), Pattern => "timeZoneName");
           if Idx > 0 then
             Treat := false;
           end if;  
       end if; 
       if Treat then
           Idx := Ada.Strings.Fixed.Index (Source => To_String(V(i)), Pattern => "picUrl");
           if Idx > 0 then
             Treat := false;
           end if;  
       end if; 

       if Treat then 
         Append(Data_Holder, V(i));
       else
         null; -- has unicode characters we'd like to skip
       end if;
     end loop;  
     Log ("Data_Holder :" & To_String(Data_Holder));

      J := Read (Strm => To_String(Data_Holder), Filename => "");
     Log ("J :" & J.Write);
      
      Data_Items := Get (J); --convert to array
      for I in 1 .. Length (Data_Items) loop
        Data_Item := Get (Data_Items, I);
        solis_data := table_solis.Empty_Data;

        if Data_Item.Has_Field ("dataTimestampStr") then
           declare                      --2025-10-12 16:51:32 
              tm : string := data_item.Get ("dataTimestampStr");       
              use Calendar2;       
           begin
              Log ("time " & Tm);
              solis_data.time_start := Calendar2.To_Time_Type (tm(1..19) & ".000");
              solis_data.time_stop := solis_data.time_start + (0,0,5,0,0); -- + 5 minutes
           end;
        end if;
        
        if Data_Item.Has_Field ("pow1") then
           declare
              pow1 : float:= data_item.Get ("pow1");
           begin
              solis_data.power1 := pow1;
              Log ("pow1 W "& pow1'img);
           end;
        end if;
        
        if Data_Item.Has_Field ("pow2") then
           declare
              pow2 : float:= data_item.Get ("pow2");
           begin
              solis_data.power2 := pow2;
              Log ("pow2 W "& pow2'img);
           end;
        end if;
  
        if Data_Item.Has_Field ("batteryPower") then
           declare
              batteryPower : float:= data_item.Get ("batteryPower");
           begin
              solis_data.battery_power := batteryPower;
              Log ("batteryPower kW "& batteryPower'img);
           end;
        end if;
  
        if Data_Item.Has_Field ("batteryCapacitySoc") then
           declare
              batteryCapacitySoc : float:= data_item.Get ("batteryCapacitySoc");
           begin
               solis_data.Battery_Charge := batteryCapacitySoc;
              Log ("batteryCapacitySoc % "& batteryCapacitySoc'img);
           end;
        end if;

        solis_data.read(eos);
        if eos then
          solis_data.insert;
        else
          solis_data.update_withcheck;
        end if;

      end loop;
  
   end On_Solis_Data;
   -----------------------------------------------------

   --    {"createdTime":"2025-10-12T22:43:49Z",
   --    "referenceTime":"2025-10-12T22:30:00Z",
   --    "geometry":{"type":"Point",
   --    "coordinates":[13.411673, 55.777183]},
   --    "timeSeries":[
   --    {
   --       "time":"2025-10-12T23:00:00Z",
   --       "intervalParametersStartTime":"2025-10-12T22:00:00Z",
   --       "data":{"air_temperature":3.4,"wind_speed":2.1,"cloud_area_fraction":0}
   --    },
   --    {  "time":"2025-10-13T00:00:00Z",
   --       "intervalParametersStartTime":"2025-10-12T23:00:00Z",
   --       "data":{"air_temperature":2.3,"wind_speed":1.6,"cloud_area_fraction":0}}
   --    ]}

   procedure on_SMHI_Data (Data : String) is
      J                 : Json_Value := Create;
      time_Series_Array : Json_Array := Empty_Array;
      data_item         : Json_Value := Create_Object;
      smhi_data : table_smhi.data_type;
      eos : Boolean := false;
   begin
      J := Read (Strm => Data, Filename => "");
      if J.Has_Field ("timeSeries") then
         time_Series_Array := j.Get ("timeSeries");
         for I in 1 .. Length (time_Series_Array) loop
            Log ("we have result #:" & I'Img);
            data_item := Get (time_Series_Array, I);
            smhi_data := table_smhi.Empty_Data;

            if data_item.Has_Field ("time") then
               declare
                  tm : string := data_item.Get ("time");
               begin
                  smhi_data.time_start := Calendar2.To_Time_Type (tm(1..19) & ".000");
                  Log ("time " & Tm);
               end;
            end if;

            if data_item.Has_Field ("intervalParametersStartTime") then
               declare
                  ipst : string := data_item.Get ("intervalParametersStartTime");
               begin
                  smhi_data.time_stop := Calendar2.To_Time_Type (ipst(1..19) & ".000");
                  Log ("intervalParametersStartTime " & ipst);
               end;
            end if;

            if data_item.Has_Field ("data") then
               data_item := data_item.Get ("data");
               if data_item.Has_Field ("air_temperature") then
                  declare
                     air_temperature : float :=
                       data_item.Get ("air_temperature");
                  begin
                     smhi_data.air_temperature := air_temperature;
                     Log ("air_temperature: " & air_temperature'image);
                  end;
               end if;

               if data_item.Has_Field ("wind_speed") then
                  declare
                     wind_speed : float := data_item.Get ("wind_speed");
                  begin
                     smhi_data.wind_speed := wind_speed;
                     Log ("wind_speed: " & wind_speed'image);
                  end;
               end if;

               if data_item.Has_Field ("cloud_area_fraction") then
                  declare
                     Cloud_Coverage : Long_Long_Integer :=
                       data_item.Get ("cloud_area_fraction");
                  begin
                     smhi_data.Cloud_Coverage := Types.Integer_4(Cloud_Coverage);
                     Log("cloud_area_fraction: " & Cloud_Coverage'image);
                  end;
               end if;
            end if;

            smhi_data.read(eos);
            if eos then
              smhi_data.insert;
            else
              smhi_data.update_withcheck;
            end if;


         end loop;
      end if;
   end on_SMHI_Data;
   -----------------------------------------------------
   procedure On_Elpriser_Data (Data : String) is
      J                 : Json_Value := Create;
      data_items        : Json_Array := Empty_Array;
      data_item         : Json_Value := Create_Object;
      Price_Data      : Table_Prices.Data_Type;
      eos : Boolean := false;

--[
--{"SEK_per_kWh":0.05878,"EUR_per_kWh":0.00532,"EXR":11.04933,"time_start":"2025-10-12T00:00:00+02:00","time_end":"2025-10-12T00:15:00+02:00"},
--{"SEK_per_kWh":0.04751,"EUR_per_kWh":0.0043,"EXR":11.04933,"time_start":"2025-10-12T00:15:00+02:00","time_end":"2025-10-12T00:30:00+02:00"},      
-- ..
--]

   begin
      J := Read (Strm => Data, Filename => "");
      data_items := Get (J); --convert to array

      for I in 1 .. Length (data_items) loop
         data_item := Get (data_items, I);

         Price_Data := Table_Prices.Empty_Data; 
         
         if data_item.Has_Field ("SEK_per_kWh") then
            declare
               sek : float:= data_item.Get ("SEK_per_kWh");
            begin
               Price_Data.SEK_per_kWh := sek;
               Log ("SEK_per_kWh "& sek'img);
            end;
         end if;

         if data_item.Has_Field ("Eur_per_kWh") then
            declare
               eur : float:= data_item.Get ("Eur_per_kWh");
            begin
               Price_Data.Eur_per_kWh := eur;
               Log ("Eur_per_kWh "& eur'img);
            end;
         end if;

         if data_item.Has_Field ("EXR") then
            declare
               exr : float:= data_item.Get ("EXR");
            begin
               Price_Data.Exchange_Rate := exr;
               Log ("EXR "& exr'img);
            end;
         end if;

         if data_item.Has_Field ("time_start") then
            declare
               ts : string:= data_item.Get ("time_start");
            begin
               Price_Data.Time_Start := Calendar2.To_Time_Type (ts(1..19) & ".000");
               Log ("time_start" & ts);
            end;
         end if;

         if data_item.Has_Field ("time_end") then
            declare
               te : string:= data_item.Get ("time_end");
            begin
               Price_Data.Time_Stop := Calendar2.To_Time_Type (te(1..19) & ".000");
               Log ("time_end " & te);
            end;
         end if;

         Price_Data.read(eos);
         if eos then
            Price_Data.insert;
         else
            Price_Data.update_withcheck;
         end if;         

      end loop;

--      end if;

   end On_Elpriser_Data;

   ------------------------------------------------------------
   function File_Content (Filename : String) return String is
      F   : Text_Io.File_Type;
      Ubs : Unbounded_String;
   begin
      Text_Io.Open (F, Text_Io.In_File, Filename);
      begin
         loop
            Append (Ubs, Ada.Strings.Unbounded.Text_Io.Get_Line (F) & ASCII.LF);
         end loop;
      exception
         when Text_Io.End_Error =>
            Text_Io.Close (F);
      end;
      return To_String (Ubs);
   end File_Content;

   use Ada.Directories;
   Dir_Ent : Directory_Entry_Type;
   Search  : Search_Type;

   T : Sql.Transaction_Type;
begin

   Sql.Connect(Host     => "localhost",
               Port     => 5432,
               Db_Name  => "bnl",
               Login    => "bnl",
               Password => "ld4BC9Q51FU9CYjC21gp");

   T.Start;            

   -- find data - *.json
   -- for each file, check type and call correct parser

   Start_Search
     (Search    => Search,
      Directory => "/home/bnl/solis/data",
      Pattern   => "*.json");
   loop
      exit when not More_Entries (Search => Search);
      Get_Next_Entry (Search => Search, Directory_Entry => Dir_Ent);
      --        Text_io.put_line(Full_Name(Dir_Ent));
      Text_io.put_line (Simple_Name (Dir_Ent));

      --        Text_io.put_line(Kind(full_Name (Dir_Ent))'img);
      --        File_List.Append(Fname);

      declare
         s_name  : string := Simple_Name (Dir_Ent);
         content : String := File_Content (Full_Name (Dir_Ent));
      begin
         if s_name (1 .. 5) = "solis" then
            On_Solis_Data (content);

         elsif s_name (1 .. 4) = "smhi" then
            On_SMHI_Data (content);

         elsif s_name (1 .. 8) = "elpriser" then
            On_Elpriser_Data (content);

         else
            Text_io.put_line ("unhandled file " & Full_Name (Dir_Ent));
         end if;
      exception
         when E : others =>
            Text_io.put_line ("Error processing file " & Full_Name (Dir_Ent));
       --     Text_io.put_line ("content was " & content);
            Text_io.put_line ("Error message was " & Exception_Message (E));
            raise;
      end;
   end loop;
   End_Search (Search => Search);
   T.Commit;
   Sql.Close_Session;

end Json_Extracter;
