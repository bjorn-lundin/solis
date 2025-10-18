
with Types; use Types;
with Calendar2; use Calendar2;

with GNAT; use GNAT;
with GNAT.AWK;
with Text_Io; use Text_Io;
with Ada.Directories;
with Tics;
with Stacktrace;
with Ada.Containers.Doubly_Linked_Lists;

procedure Split_To_AI is
  Computer_File : AWK.Session_Type;
  Is_First_Line : Boolean := True;
  package AD renames Ada.Directories;
  use AD;

  package Fl is new Ada.Containers.Doubly_Linked_Lists(Float);
  Fl_List       : Fl.List;
  use type Ada.Containers.Count_Type;

  -----------------------------------------------------
  function To_Time(S2: String) return Calendar2.Time_Type is
    Tmp : Time_Type;
    S   : String (1 .. S2'Last - S2'First + 1) := S2;
  begin
    -- '22-01-2013 11:09:06'

    Tmp.Year  := Year_Type'Value (S (7 .. 10));
    Tmp.Month := Month_Type'Value (S (4 .. 5));
    Tmp.Day   := Day_Type'Value (S (1 .. 2));

    Tmp.Hour   := Hour_Type'Value (S (12 .. 13));
    Tmp.Minute := Minute_Type'Value (S (15 .. 16));
    if S'Length = 19 then
      Tmp.Second := Second_Type'Value (S (18 .. 19));
    else
      Tmp.Second := 0;
    end if;
    return Tmp;
  end To_Time;
  pragma Unreferenced(To_Time);
  -----------------------------------------------------

  Path          : String   :=  "/Users/bnl/Downloads/plots/price_plots/dats";
  Search_Pattern : String  := "*.dat";
  Dir_Ent       : Directory_Entry_Type;
  The_Search    : Search_Type;
  Test,Train    :Text_IO.File_Type;
  --Files_Processed : Natural := 0;
begin


  Text_IO.Create(File => Train,
                 Mode => Text_IO.Out_File,
                 Name => "/Users/bnl/Downloads/plots/ai/train_all.csv");

  Text_IO.Create(File => Test,
                 Mode => Text_IO.Out_File,
                 Name => "/Users/bnl/Downloads/plots/ai/test_all.csv");



  Start_Search(Search    => The_Search,
               Directory => Path,
               Pattern   => Search_Pattern);
  loop

    exit when not More_Entries(Search => The_Search);
    Get_Next_Entry(Search          => The_Search,
                   Directory_Entry => Dir_Ent);
   -- Files_Processed := Files_Processed +1;
    declare
      Filename        : String := Ad.Base_Name(Ad.Full_Name(Dir_Ent));
      Idx             : Natural := 0;
      Odds            : Fixed_Type := 0.0;
      Tic             : Tics.Tics_Type;
      Norm_Tic        : Float := 0.0;
      type Mode_Type is (Testset, Trainset);
      Mode            : Mode_Type ;
      Line            : Natural := 0;

      procedure Put(S : String ) is
      begin
        case Mode is
          when Testset => Put (Test,S);
          when Trainset => Put (Train,S);
        end case;
      end Put;
      procedure Put_Newline is
      begin
        case Mode is
          when Testset => Put_Line (Test,"");
          when Trainset => Put_Line (Train,"");
        end case;
      end Put_Newline;

    begin --1.123501093_3310716_loser.dat
      if Filename(16) = '4' or else Filename(16) = '6' then
        Mode := Testset;
      else
        Mode := Trainset;
      end if;

      Is_First_Line := True;


      AWK.Set_Current (Computer_File);
      AWK.Open (Separators => "|",
                Filename   => Ad.Full_Name(Dir_Ent));

      while not AWK.End_Of_File loop
        AWK.Get_Line;
        if Is_First_Line then
          Is_First_Line := False;
          Line := 0;
          Fl_List.Clear;
        else
          Line := Line +1;
          Odds := Fixed_Type'Value(Awk.Field(3));
          begin
            Tic := Tics.Get_Tic_Index(Odds);
            Norm_Tic := Float(Tic)/350.0;
            Fl_List.Append(Norm_Tic);
          exception
            when TICS.BAD_ODDS => null;
          end;
        end if;
        exit when Line = 783;
      end loop;
      AWK.Close (Computer_File);

      if  Fl_List.Length >= 783 then
        if Filename(Idx+1..Filename'Last) = "loser" then
          Put("0,");
        else
          Put("1,");
        end if;

        declare
          F2 : Float;
        begin
          for F of Fl_List loop
            F2 := F;
            Put(F'Image & ",");
          end loop;
          Put(F2'Image);
        end;
        Put_Newline;
      end if;
    end;
   -- exit when Files_Processed > 20_000;
  end loop;
  End_Search (Search => The_Search);
  Text_IO.Close(Test);
  Text_IO.Close(Train);

exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);

end Split_To_AI;
