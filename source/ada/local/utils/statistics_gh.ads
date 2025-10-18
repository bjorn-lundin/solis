


with Types; use Types; 
with Table_Abets;
with Ada.Containers.Doubly_Linked_Lists;
  

package Statistics_Gh is

  bad : exception;

  subtype First_Odds_Range_Type is integer_4 range -1 .. 50; --delta tics
  subtype Second_Odds_Range_Type is integer_4 range 11 ..60; --Max_layprice *10

  type Market_Type is (Win,Plc);
  type Result_Type_Type is (Count, Hitrate, Hitrate_Times_Count);
  
  package Fixed_Type_Pack is new Ada.Containers.Doubly_Linked_Lists(Fixed_Type);
  
  type Part_Type is record
    Cnt        : Natural := 0 ;
    Won        : Natural := 0 ;
    Hitrate    : Fixed_Type := 0.0;     
    Odds_List  : Fixed_Type_Pack.List;
    Avg_Odds   : Fixed_Type := 0.0; 
  end record;
  
  
  type Stats_Type is tagged record
    Every               : Part_Type;
    Matched             : Part_Type;
    Needed_Hitrate      : Fixed_Type := 0.0;
    Profit              : Fixed_Type := 0.0; 
  end record;  
  
  procedure Treat(Self : in out Stats_Type; Bet : Table_Abets.Data_Type);
  procedure Calculate_Avg_Odds(Self : in out Stats_Type) ;
  procedure Print_Result(Self   : in out Stats_Type;
                         First  : in First_Odds_Range_Type;
                         Second : in Second_Odds_Range_Type;
                         Market : in Market_Type);
 
  type Stats_Array_Type is array (First_Odds_Range_Type'range,
                                  Second_Odds_Range_Type'range,
                                  Market_Type'range) of Stats_Type;

                                  
                                  
  function Get_First_Odds_Range(Bet : Table_Abets.Data_Type) return First_Odds_Range_Type;
  function Get_Second_Odds_Range(Bet : Table_Abets.Data_Type) return Second_Odds_Range_Type;
  function Get_Market_Type(Bet : Table_Abets.Data_Type) return Market_Type;
     
  
  function Get_Avg_Odds(Bet : Table_Abets.Data_Type) return Fixed_Type ;

  

end Statistics_Gh;
