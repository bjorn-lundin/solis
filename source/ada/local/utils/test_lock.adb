
with Gnatcoll.Json; use Gnatcoll.Json;

with Text_Io; use Text_io;
with Stacktrace;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Rpc;
with Ini;
with Logging; use Logging;
with Ada.Environment_Variables;

procedure Test_Lock is
  Me : constant String := "Test_Lock.";
  Data : Unbounded_String;
  package EV renames Ada.Environment_Variables;
--  use type Rpc.Result_Type;
  
  root   : JSON_Array := Empty_Array;
  root2  : JSON_Array := Empty_Array;
  
  Child1 : JSON_Value := Create_Object;
  Child2 : JSON_Value := Create_Object;
  
begin

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
--  Log(Me, "Connect Db");
--  Sql.Connect
--        (Host     => Ini.Get_Value("database", "host", ""),
--         Port     => Ini.Get_Value("database", "port", 5432),
--         Db_Name  => Ini.Get_Value("database", "name", ""),
--         Login    => Ini.Get_Value("database", "username", ""),
--         Password =>Ini.Get_Value("database", "password", ""));
--  Log(Me, "db Connected");
    -- Ask a pythonscript to login for us, returning a token
  Log(Me, "Login betfair");
  Rpc.Init(
            Username   => Ini.Get_Value("betfair","username",""),
            Password   => Ini.Get_Value("betfair","password",""),
            Product_Id => Ini.Get_Value("betfair","product_id",""),
            Vendor_Id  => Ini.Get_Value("betfair","vendor_id",""),
            App_Key    => Ini.Get_Value("betfair","appkey","")
          );
  Rpc.Login;
  Log(Me, "Login betfair done");
  
  Rpc.Get_Navigation_Data(Nav_Data => Data) ;
  Log(Me, "got data");
  
  --Text_Io.Put_Line(To_String(Data));
  
  Child1 := Read (Strm     => To_String(Data),
                Filename => "");
  
  root := Child1.Get("children");
  for i in 1 .. Length(root) loop
    Child2 := Get(root,i);
--    if Child2.Has_Field("name") then
--      Put_Line("name " & Child2.Get("name"));
--    end if;  
--    if Child2.Has_Field("type") then
--      Put_Line("type " & Child2.Get("type"));
--    end if;
    
    if Child2.Has_Field("id") then     
      declare
        Id : String := Child2.Get("id"); 
      begin
--        Put_Line("id " & Id);
        if Id = "7" then -- horserace        
          Root2 := Child2.Get("children");
          for j in 1 .. Length(Root2) loop
            Child2 := Get(Root2,j);
--            if Child2.Has_Field("name") then
--              Put_Line("name " & Child2.Get("name"));
--            end if;  
--            if Child2.Has_Field("id") then
--              Put_Line("id " & Child2.Get("id"));
--            end if;  
            if Child2.Has_Field("type") then
              declare
                typ : String := Child2.Get("type");
              begin
--                Put_Line("type " & Child2.Get("type"));
                if typ = "RACE" then
                  declare
                    Root3  : JSON_Array := Empty_Array;  
                    Child3 : JSON_Value := Create_Object;                    
                    Country_Code : String := Child2.Get("countryCode");
                  begin
                    if (Country_Code = "GB" or Country_Code = "IE" ) then
                    
                      Root3 := Child2.Get("children");
                      for k in 1 .. Length(Root3) loop
                        Child3 := Get(Root3,k);
                          if Child3.Has_Field("marketType") and then 
                            To_String(Child3.Get("marketType")) = "PLACE" and then
                            Child3.Has_Field("numberOfWinners") and then
                            Child3.Get("numberOfWinners") >= 3 then
                          
                           if Child2.Has_Field("venue") then
                             Set_Col(1);
                             Put(Country_Code);
                             Set_Col(4);
                             Put(" " & Child2.Get("venue"));
                           end if;
                          --Put_Line("    marketType " & Child3.Get("marketType"));
                          --if Child3.Has_Field("name") then
                          --  Put_Line("    name " & Child3.Get("name"));
                          --end if;  
--                          if Child3.Has_Field("id") then
--                            Put_Line("    id " & Child3.Get("id"));
--                          end if;  
--                          if Child3.Has_Field("type") then
--                            Put_Line("    type " & Child3.Get("type"));
--                          end if;
                          if Child3.Has_Field("marketStartTime") then
                            declare 
                              TS : String := Child3.Get("marketStartTime");
                              -- 2014-10-21T19:30:00.000Z
                            begin
                              Ts(11) := ' ';
                              --2014-10-21 19:30
                              Set_Col(19);
                              Put_Line("  " & Ts(1..16));
                            end;
                          end if;
                          
                          
                        end if;  
                      end loop;    
                    end if;                      
                  end;
                end if;  
              end;
            end if;            
          end loop;
        end if;
      end;      
      
    end if;  
  
  
  end loop;
  
  
  

  Log(Me, "done");
exception
  when E: others => Stacktrace.Tracebackinfo(E);
end Test_Lock;
