with Types; use Types;
with Bot_Types; use Bot_Types;
package Tics is
  Bad_odds : exception;
  type Tics_Type is new integer range 1 .. 350;

------------------------------------------
  function Get_Tic_Index(Price : Fixed_Type) return Tics_Type ;
------------------------------------------
  function Get_Tic_Price(I : Tics_Type) return Fixed_Type ;
------------------------------------------
  function Get_Zero_Size(Backprice : Back_Price_Type;
                         Backsize  : Bet_Size_Type;
                         Layprice  : Lay_Price_Type) return Bet_Size_Type ;
------------------------------------------
  function Get_Green_Size(Layprice   : Lay_Price_Type;
                          Laysize    : Bet_Size_Type;
                          Backprice  : Back_Price_Type) return Bet_Size_Type ;

  function Get_Nearest_Higher_Tic_Index(Price : Fixed_Type) return Tics_Type ;
------------------------------------------
  procedure Tic_Table;
  ------------------------------------------
  procedure Test_Float_To_Fix_Conversion;
end Tics;
