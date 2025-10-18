
with Ada.Characters.Handling;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Command_Line;

--with Ada.Containers.Doubly_Linked_Lists;
--with Ada.Directories;


with Gnat; use Gnat;
with Gnat.Awk;

with Types; use Types;
with Calendar2; use Calendar2;

with Text_Io; use Text_Io;
--with Tics;
with Stacktrace;
with Price_Histories;
with Markets;
--with Prices;
with Bot_Types;



procedure Check_Bet_On_2_3_4 is
  package Ev renames Ada.Environment_Variables;
--  package Ad renames Ada.Directories;
  --use Ad;

 -- package Backprice_Sorter is new Prices.Lists.Generic_Sorting("<");
  type Best_Runners_Array_Type is array (1..16) of Price_Histories.Price_History_Type;

  procedure Get_Profit(Ts : in Calendar2.Time_Type; Market : in Markets.Market_Type; Bra : in out Best_Runners_Array_Type) is
    Computer_File : Awk.Session_Type;
    Race_Type : String := "plc";
    Side : Bot_Types.Bet_Side_Type := Bot_Types.Back;
    Path          : String := Ev.Value("BOT_HISTORY") & "/data/ai/" & Race_Type & "/rewards/" & Ada.Characters.Handling.To_Lower(Side'Img);
    Filename      : String := Path & "/" & Market.Marketid & ".dat";
 --   List          : Price_Histories.Lists.List;
    Is_First_Line : Boolean := True;
  begin

    Awk.Set_Current (Computer_File);
    Awk.Open (Separators => "|", Filename   => Filename);

    Awk_Loop : while not Awk.End_Of_File loop
      Awk.Get_Line;
      if Is_First_Line then
        Is_First_Line := False;

        for I in Bra'Range loop
          Bra(I).Marketid := Market.Marketid;
          Bra(I).Selectionid := Integer_4'Value(Awk.Field(Awk.Count(i+1)));
        end loop;
      else
        declare
          Time : Calendar2.Time_Type := Calendar2.To_Time_Type(Date_Str => Market.Startts.To_String(1..10), Time_Str => Awk.Field(1));
        begin
          if Time >= Ts then
            for I in Bra'Range loop
              Bra(I).Pricets := Time;
              Bra(I).Backprice := Fixed_Type'Value(Awk.Field(Awk.Count(i+1)));
            end loop;
            exit Awk_Loop;
          end if;
        end;
      end if;
    end loop Awk_Loop;
    Awk.Close (Computer_File);
  end Get_Profit;
  ----------------------

  Ts : Calendar2.Time_Type := (2020,10,15,17,31,14,450);
  Market : Markets.Market_Type;
  Bra : Best_Runners_Array_Type;
begin


  --  Läs in Bets med stragegi 1_07/1_28 i lista
  -- kolla vem som om 1/2/3
  -- klla deras resultat




  Market.Marketid := "1.168062606";
  Market.Startts := Ts;

  Get_Profit(Ts, Market, Bra);

  for I in Bra'Range loop
    Text_Io.Put_Line(Bra(I).To_String);
  end loop;
exception
  when E : others =>
    declare
      Last_Exception_Name     : constant String  := Ada.Exceptions.Exception_Name(E);
      Last_Exception_Messsage : constant String  := Ada.Exceptions.Exception_Message(E);
      Last_Exception_Info     : constant String  := Ada.Exceptions.Exception_Information(E);
    begin
      Put_Line(Last_Exception_Name);
      Put_Line("Message : " & Last_Exception_Messsage);
      Put_Line(Last_Exception_Info);
      Put_Line("addr2line" & " --functions --basenames --exe=" &
            Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump(Last_Exception_Info));
    end ;


end Check_Bet_On_2_3_4;
