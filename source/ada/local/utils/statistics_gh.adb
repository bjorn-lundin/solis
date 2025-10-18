
with Text_Io;
with Utils;

package body Statistics_Gh is

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


  Commission : constant Fixed_Type := 0.065;

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

    if Bet.Status(1) = 'M' then
       Self.Matched.Cnt := Self.Matched.Cnt +1;
      if Bet.Betwon then
        Self.Matched.Won := Self.Matched.Won +1;
      end if;
      if Self.Matched.Cnt > 0 then
        Self.Matched.Hitrate := Fixed_Type(Self.Matched.Won) / Fixed_Type(Self.Matched.Cnt);
      end if;
      
      Self.Profit := Self.Profit + Bet.Profit * (1.0 - Commission);
      Self.Matched.Odds_List.Append(Bet.Pricematched);
    end if;
  end Treat;
  
  
  
  ------------------------------------------------------------
  procedure Print_Result(Self   : in out Stats_Type;
                         First  : in First_Odds_Range_Type;
                         Second : in Second_Odds_Range_Type;
                         Market : in Market_Type) is
    use Text_Io;
    use Utils;
    -- 123456789012345
    -- A_1_01_1_05,
    -- A_01_07,

  begin
    Put_Line(First_Odds_Range_Type'Pos(First)'Img & "|" &
             Second_Odds_Range_Type'Pos(Second)'Img & "|" &
             Trim(Integer_4(Self.Profit)'Img) & "/" &  -- no decimals
             Trim(Self.Matched.Cnt'Img)             
         --    F8_Image(Self.Profit,0,0)  -- no decimals
    );
             --& ":-\n" & 
             --Trim(Self.Matched.Won'Img) & "/" & Trim(Self.Matched.Cnt'Img) & "\n" & 
             --"D" & F8_Image(100.0 * (Self.Matched.Hitrate-Self.Needed_Hitrate),1) & "%\n" &
             --"A" & F8_Image(Self.Matched.Avg_Odds)
    
    if Second = Second_Odds_Range_Type'last then
      New_Line;    
    end if;
    
  end Print_Result;
  ------------------------------------------------------------

 --delta tics
  function Get_First_Odds_Range(Bet : Table_Abets.Data_Type) return First_Odds_Range_Type is
  begin 
      return First_Odds_Range_Type(Bet.Betmode);
  exception
    when Constraint_Error =>
     Text_io.Put_Line (Bet.To_String);
     raise;
  end Get_First_Odds_Range;
  ------------------------------------------------------------
  --max lay odds * 10
  function Get_Second_Odds_Range(Bet : Table_Abets.Data_Type) return Second_Odds_Range_Type is
  begin 
      return Second_Odds_Range_Type(Bet.Powerdays);
  exception
    when Constraint_Error =>
     Text_io.Put_Line (Bet.To_String);
     raise;
  end Get_Second_Odds_Range;
  ------------------------------------------------------------
  function Get_Market_Type(Bet : Table_Abets.Data_Type) return Market_Type is
  begin
      return Market_Type'Value("WIN");
  exception
    when Constraint_Error =>
     Text_io.Put_Line (Bet.To_String);
     raise;
  end Get_Market_Type;
  ------------------------------------------------------------

  
  function Get_Avg_Odds(Bet : Table_Abets.Data_Type) return Fixed_Type is
   -- First, Second  : Fixed_Type := 0.0;
  begin
    return 1.01;
  exception
    when Constraint_Error =>
     Text_io.Put_Line (Bet.To_String);
     raise;
  end Get_Avg_Odds;
  
  ------------------------------------------------------------
  
end Statistics_Gh;

