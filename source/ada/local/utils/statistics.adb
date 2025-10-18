
with Text_Io;
with Utils;

package body Statistics is

 --  type Part_Type is record
 --    Cnt     : Natural := 0 ;
 --    Won     : Natural := 0 ;
 --    Hitrate : Fixed_Type := 0.0;
 --  end record;
 --
 --  type Stats_Type is tagged record
 --    Every               : Part_Type;
 --    Matched             : Part_Type;
 --    Needed_Hitrate      : Fixed_Type := 0.0;
 --    Odds                : Fixed_Type := 0.0;
 --    Profit              : Fixed_Type := 0.0;
 --  end record;


  Commission : constant Fixed_Type := 6.5/100.0;

  function Needed_Hitrate(O : Fixed_Type) return Fixed_Type is
    -- =1/(K6-(K6-1)*$A$2)
  begin
    return 1.0/(O - (O-1.0)* Commission) ;
  end Needed_Hitrate;
  ------------------------------------------------------------
  procedure Calculate_Avg_Odds(Self : in out Stats_Type) is
    Tmp : Fixed_Type := 0.0;
  begin
    for o of Self.Every.Odds_List loop
      Tmp := Tmp + o;
    end loop;
    Self.Every.Avg_Odds := Tmp / Fixed_Type(Self.Every.Odds_List.Length);

    Tmp := 0.0;
    for o of Self.Matched.Odds_List loop
      Tmp := Tmp + o;
    end loop;
    Self.Matched.Avg_Odds := Tmp / Fixed_Type(Self.Matched.Odds_List.Length);

    Self.Needed_Hitrate := Needed_Hitrate(Self.Matched.Avg_Odds);
  end Calculate_Avg_Odds;

  ------------------------------------------------------------
  procedure Treat(Self : in out Stats_Type; Bet : Table_Abets.Data_Type) is
  begin
    Self.Every.Cnt := Self.Every.Cnt +1;
    if Bet.Betwon then
      Self.Every.Won := Self.Every.Won +1;
    end if;

    Self.Every.Odds_List.Append(Bet.Pricematched);

    if Self.Every.Cnt > 0 then
      Self.Every.Hitrate := Fixed_Type(Self.Every.Won) / Fixed_Type(Self.Every.Cnt);
    end if;

    if Bet.Status(1..7) = "SUCCESS" then
      Self.Matched.Cnt := Self.Matched.Cnt +1;
      Self.Matched.Hitrate := Fixed_Type(Self.Matched.Won) / Fixed_Type(Self.Matched.Cnt);

      if Bet.Betwon then
        Self.Matched.Won := Self.Matched.Won +1;
        Self.Profit := Self.Profit + Bet.Profit * (1.0 - Commission);
        Self.Matched.Odds_List.Append(Bet.Pricematched);
      else
        Self.Profit := Self.Profit + Bet.Profit; -- Bet.Profit < 0
      end if;

    end if;
  end Treat;



  ------------------------------------------------------------
  procedure Print_Result(Self   : in out Stats_Type;
                         First  : in First_Odds_Range_Type;
                         Second : in Second_Odds_Range_Type;
                         Market : in Market_Type) is
                         pragma Unreferenced (Market);
    use Text_Io;
    use Utils;
    -- 123456789012345
    -- A_1_01_1_05,
    -- A_01_07,

  begin
    if Self.Matched.Won > 0 then
      -- first/second/cnt/
      Put_Line(First_Odds_Range_Type'Pos(First)'Img & "|" &
               Second_Odds_Range_Type'Pos(Second)'Img & "|" &
               F8_Image(Self.Profit) & ":-\n" &
               Trim(Self.Matched.Won'Img) & "/" & Trim(Self.Matched.Cnt'Img) & "\n" &
               "D" & F8_Image(100.0 * (Self.Matched.Hitrate-Self.Needed_Hitrate)) & "%\n" &
               "A" & F8_Image(Self.Matched.Avg_Odds)
      );
      if Second = Second_Odds_Range_Type'last then
        New_Line;
      end if;
    else -- to black out all nono-matched, fake loss of 99999
      -- first/second/cnt/
      Put_Line(First_Odds_Range_Type'Pos(First)'Img & "|" &
               Second_Odds_Range_Type'Pos(Second)'Img & "|" &
               "-99999:-\n" &
               Trim(Self.Matched.Won'Img) & "/" & Trim(Self.Matched.Cnt'Img) & "\n" &
               "D" & F8_Image(100.0 * (Self.Matched.Hitrate-Self.Needed_Hitrate)) & "%\n" &
               "A" & F8_Image(Self.Matched.Avg_Odds)
      );
      if Second = Second_Odds_Range_Type'last then
        New_Line;
      end if;
    end if;


  end Print_Result;
  ------------------------------------------------------------

  function Get_First_Odds_Range(Betname : String) return First_Odds_Range_Type is
  begin --      1         2
    -- 1234567890123456789012345678
    -- BACK_1_01_1_05_11_13_1_2_WIN
      return First_Odds_Range_Type'Value("A_" & Betname(6..14));
  exception
    when Constraint_Error =>
     Text_io.Put_Line (Betname);
     raise;
  end Get_First_Odds_Range;
  ------------------------------------------------------------

  function Get_Second_Odds_Range(Betname : String) return Second_Odds_Range_Type is
  begin --      1         2
    -- 1234567890123456789012345678
    -- BACK_1_01_1_05_11_13_1_2_WIN
      return Second_Odds_Range_Type'Value("A_" & Betname(16..20) );
  exception
    when Constraint_Error =>
     Text_io.Put_Line (Betname);
     raise;
  end Get_Second_Odds_Range;
  ------------------------------------------------------------
  function Get_Market_Type(Betname : String) return Market_Type is
  begin --      1         2
    -- 1234567890123456789012345678
    -- BACK_1_01_1_05_11_13_1_2_WIN
      return Market_Type'Value(Betname(26..28) );
  exception
    when Constraint_Error =>
     Text_io.Put_Line (Betname);
     raise;
  end Get_Market_Type;
  ------------------------------------------------------------


  function Get_Avg_Odds(Betname : String) return Fixed_Type is
    First, Second  : Fixed_Type := 0.0;
  begin --      1         2
    -- 1234567890123456789012345678
    -- BACK_1_01_1_05_11_13_1_2_WIN
    First  := Fixed_Type'Value( Betname( 6.. 6) & "." & Betname( 8.. 9));
    Second := Fixed_Type'Value( Betname(11..11) & "." & Betname(13..14));

    if Betname(26..28) = "WIN" then
      return (First + Second) / 2.0;
    elsif Betname(26..28) = "PLC" then
      if    First >= 2.0 then
        return 1.10;
      elsif First >= 1.9 then
        return 1.09;
      elsif First >= 1.8 then
        return 1.08;
      elsif First >= 1.7 then
        return 1.07;
      elsif First >= 1.6 then
        return 1.06;
      elsif First >= 1.5 then
        return 1.05;
      elsif First >= 1.4 then
        return 1.04;
      elsif First >= 1.3 then
        return 1.03;
      elsif First >= 1.2 then
        return 1.02;
      else
        return 1.02;
      end if;
    else
      raise Constraint_Error with "bad name not PLC/WIN";
    end if;
  exception
    when Constraint_Error =>
     Text_io.Put_Line (Betname);
     raise;
  end Get_Avg_Odds;

  ------------------------------------------------------------

end Statistics;
