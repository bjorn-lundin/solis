with Types;    use Types;
with Calendar2; use Calendar2;
with Text_IO;
with Ini;
with Ada.Environment_Variables;
with Rpc;
with Table_Astarttimes;
with Stacktrace;
with Ada.Containers;

procedure Race_Time is
   package EV renames Ada.Environment_Variables;
   GDebug : Boolean := False;
   -------------------------------
   procedure Debug (What : String) is
   begin
      if GDebug then
         Text_Io.Put_Line (Text_Io.Standard_Error, Calendar2.String_Date_Time_ISO (Clock, " " , "") & " " & What);
      end if;
   end Debug;
   pragma Warnings(Off, Debug);
   -------------------------------
   procedure Print (What : String) is
   begin
      Text_Io.Put_Line (What);
   end Print;
   -------------------------------
   Start_Time_List : Table_Astarttimes.Astarttimes_List_Pack2.List;
   Arrow_Is_Printed : Boolean := False;
   Now : Time_Type := Time_Type_First;
   --use type Text_Io.Count;
   use type Ada.Containers.Count_Type;

begin
   Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");
   Rpc.Init(
            Username   => Ini.Get_Value("betfair","username",""),
            Password   => Ini.Get_Value("betfair","password",""),
            Product_Id => Ini.Get_Value("betfair","product_id",""),
            Vendor_Id  => Ini.Get_Value("betfair","vendor_id",""),
            App_Key    => Ini.Get_Value("betfair","appkey","")
           );

   Days : loop
      begin
         Start_Time_List.Clear;
         for I in 1 .. 100 loop
            begin
               Rpc.Login;
               exit;
            exception
               when Rpc.Login_Failed =>
                  Print(I'Img & " Login failed - wait 1 minute (of 100 tries)");
                  delay 60.0; -- wait a minute
               when Rpc.Post_Timeout =>
                  Print(I'Img & " Post_Timeout - wait 1 minute (of 100 tries)");
                  delay 60.0; -- wait a minute
            end;
         end loop;
         for I in 1 .. 100 loop
            begin
               Rpc.Get_Starttimes(List => Start_Time_List);
               exit;
            exception
               when Rpc.Post_Timeout =>
                  Print(I'Img & " Post_Timeout-2 - wait 1 minute (of 100 tries)");
                  delay 60.0; -- wait a minute
            end;
         end loop;

         begin
            Rpc.Logout;
         exception
            when others =>
               Print("caught logout issues");
         end;

         if Start_Time_List.Length = 0 then
            Print("no races left today? Retry in 5 min");
            delay 300.0;
         end if;

         Day : loop
            Arrow_Is_Printed := False;
            Now := Calendar2.Clock;
            Text_Io.New_Line(Text_Io.Count(100));
            for S of Start_Time_List loop
               if not Arrow_Is_Printed and then
                 Now <= S.Starttime then
                  Print(
                        S.Starttime.String_Time(Seconds => False) & " | " &
                          S.Venue(1..15) & " <===="
                       ) ;
                  Arrow_Is_Printed := True;
               else
                  Print(
                        S.Starttime.String_Time(Seconds => False) & " | " &
                          S.Venue(1..15)
                       ) ;
               end if;
            end loop;
            for I in 1 .. 30 loop
               Text_Io.Put('.');
               delay 1.0;
            end loop;
            -- new day, get new list after it is written to db
            exit Day when (Now.Hour = 0 and then Now.Minute = 0 and then Now.Second < 30) or else
              Start_Time_List.Length = 0 ;
         end loop Day;
      end;
   end loop Days;
exception
   when E: others =>
      Stacktrace.Tracebackinfo(E);
end Race_Time;
