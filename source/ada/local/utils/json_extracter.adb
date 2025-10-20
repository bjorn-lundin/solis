with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_Io;
with Ada.Environment_Variables;
with Ada.Directories;
with ada.exceptions;        use ada.exceptions;

--with bot_types; use bot_types;
with Text_io;
with botcoll.json; use botcoll.json;

procedure Json_Extracter is

   --    package Directory_List_Package is new Ada.Containers.Doubly_Linked_Lists(String_Object);
   --    package A_Sorter is new Directory_List_Package.Generic_Sorting;
   --    File_List : Directory_List_Package.List;

   procedure Log (w : string) is
   begin
      text_io.put_line (w);
   end Log;

   -----------------------------------------------------
   procedure On_Solis_Data (filename : string) is
   begin
      null;
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

   begin
      J := Read (Strm => Data, Filename => "");
      if J.Has_Field ("timeSeries") then
         time_Series_Array := j.Get ("timeSeries");
         for I in 1 .. Length (time_Series_Array) loop
            Log ("we have result #:" & I'Img);
            data_item := Get (time_Series_Array, I);

            if data_item.Has_Field ("time") then
               declare
                  tm : string := data_item.Get ("time");
               begin
                  Log ("time " & Tm);
               end;
            end if;

            if data_item.Has_Field ("intervalParametersStartTime") then
               declare
                  ipst : string := data_item.Get ("intervalParametersStartTime");
               begin
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
                     Log ("air_temperature: " & air_temperature'image);
                  end;
               end if;

               if data_item.Has_Field ("wind_speed") then
                  declare
                     wind_speed : float := data_item.Get ("wind_speed");
                  begin
                     Log ("wind_speed: " & wind_speed'image);
                  end;
               end if;

               if data_item.Has_Field ("cloud_area_fraction") then
                  declare
                     cloud_area_fraction : Long_Long_Integer :=
                       data_item.Get ("cloud_area_fraction");
                  begin
                     Log("cloud_area_fraction: " & cloud_area_fraction'image);
                  end;
               end if;

            end if;
         end loop;
      end if;
   end on_SMHI_Data;
   -----------------------------------------------------
   procedure On_Elpriser_Data (Data : String) is
      J                 : Json_Value := Create;
      data_items        : Json_Array := Empty_Array;
      data_item         : Json_Value := Create_Object;
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
         if data_item.Has_Field ("SEK_per_kWh") then
            declare
               sek : float:= data_item.Get ("SEK_per_kWh");
            begin
               Log ("SEK_per_kWh "& sek'img);
            end;
         end if;
         
         if data_item.Has_Field ("time_start") then
            declare
               ts : string:= data_item.Get ("time_start");
            begin
               Log ("time_start" & ts);
            end;
         end if;

         if data_item.Has_Field ("time_end") then
            declare
               te : string:= data_item.Get ("time_end");
            begin
               Log ("time_end " & te);
            end;
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
            Append (Ubs, Ada.Strings.Unbounded.Text_Io.Get_Line (F));
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

begin

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
     --      On_Solis_Data (content);
null;
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
            Text_io.put_line ("content was " & content);
            Text_io.put_line ("Error message was " & Exception_Message (E));
            raise;
      end;
   end loop;
   End_Search (Search => Search);
end Json_Extracter;
