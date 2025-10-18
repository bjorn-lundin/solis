


with Types; use Types; 
with Table_Abets;
with Ada.Containers.Doubly_Linked_Lists;
  

package Statistics is

  bad : exception;

  type First_Odds_Range_Type is (
    A_1_01_1_05,
    A_1_06_1_10,
    A_1_11_1_15,
    A_1_16_1_20,
    A_1_21_1_25,
    A_1_26_1_30,
    A_1_31_1_35,
    A_1_36_1_40,
    A_1_41_1_45,
    A_1_46_1_50,
    A_1_51_1_55,
    A_1_56_1_60,
    A_1_61_1_65,
    A_1_66_1_70,
    A_1_71_1_75,
    A_1_76_1_80,
    A_1_81_1_85,
    A_1_86_1_90,
    A_1_91_1_95,
    A_1_96_2_00
  );
  type Second_Odds_Range_Type is (
--    A_01_07,
    A_01_04,
    A_05_07,
    A_08_10,
    A_11_13,
    A_14_17,
    A_18_20,
    A_21_23,
    A_24_26,
    A_27_30,
    A_31_33,
    A_34_37,
    A_38_40
  );

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

                                  
                                  
  function Get_First_Odds_Range(Betname : String) return First_Odds_Range_Type;
  function Get_Second_Odds_Range(Betname : String) return Second_Odds_Range_Type;
  function Get_Market_Type(Betname : String) return Market_Type;
     
  
   function Get_Avg_Odds(Betname : String) return Fixed_Type ;

  

end Statistics;






