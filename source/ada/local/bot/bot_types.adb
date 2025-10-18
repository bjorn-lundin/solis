


package body Bot_Types is


  ---------------------------------------------
  function "-" (Left : Back_Price_Type ; Right : Delta_Price_Type) return Back_Price_Type is
  begin
    return Left - Back_Price_Type (Right);
  end "-";
  ---------------------------------------------
  function "-" (Left : Bet_Price_Type ; Right : Delta_Price_Type) return Bet_Price_Type is
  begin
    return Left - Bet_Price_Type (Right);
  end "-";
  ---------------------------------------------

  function "+" (Left : Back_Price_Type ; Right : Delta_Price_Type) return Back_Price_Type is
  begin
    return Left + Back_Price_Type (Right);
  end "+";
  ---------------------------------------------
--  function "+" (Left : Back_Price_Type ; Right : Favorite_By_Type) return Back_Price_Type is
--  begin
--    return Left + Back_Price_Type (Right);
--  end "+";
--  ---------------------------------------------
--  function "+" (Left : Fixed_Type ; Right : Favorite_By_Type) return Back_Price_Type is
--  begin
--    return Back_Price_Type (Left) + Back_Price_Type (Right);
--  end "+";
--  ---------------------------------------------
--  function "+" (Left : Fixed_Type ; Right : Favorite_By_Type) return Fixed_Type is
--  begin
--    return Left + Fixed_Type (Right);
--  end "+";
  ---------------------------------------------
  function "+" (Left : Bet_Price_Type ; Right : Delta_Price_Type) return Bet_Price_Type is
  begin
    return Left + Bet_Price_Type (Right);
  end "+";
  ---------------------------------------------

  function "<" (Left : Min_Lay_Price_Type ; Right : Fixed_Type) return Boolean is
  begin
    return Left < Min_Lay_Price_Type (Right);
  end "<";
  ---------------------------------------------
  function "<" (Left : Profit_Type ; Right : Max_Daily_Loss_Type) return Boolean is
  begin
    return Fixed_Type (Left) < Fixed_Type (Right);
  end "<";

  ---------------------------------------------
  function "<" (Left : Integer ; Right : Min_Num_Runners_Type) return Boolean is
  begin
    return Left < Integer (Right);
  end "<";


  ---------------------------------------------
  function ">" (Left : Fixed_Type ; Right : Max_Exposure_Type) return Boolean is
  begin
    return Left > Fixed_Type (Right);
  end ">";
  ---------------------------------------------
  function ">" (Left : Integer ; Right : Max_Num_Runners_Type) return Boolean is
  begin
    return Left > Integer (Right);
  end ">";
  ---------------------------------------------


  function "<=" (Left : Back_Price_Type ; Right : Fixed_Type) return Boolean is
  begin
    return Left <= Back_Price_Type (Right);
  end "<=";

  ---------------------------------------------
--    function "<=" (Left : Fixed_Type ; Right : Back_Price_Type) return Boolean is
--    begin
--      return Left <= Fixed_Type (Right);
--    end "<=";
  ---------------------------------------------
  function "<=" (Left : Min_Lay_Price_Type ; Right : Fixed_Type) return Boolean is
  begin
    return Left <= Min_Lay_Price_Type (Right);
  end "<=";
  ---------------------------------------------
--  function "<=" (Left : Fixed_Type ; Right : Max_Lay_Price_Type) return Boolean is
--  begin
--    return Left <= Fixed_Type (Right);
--  end "<=";
  ---------------------------------------------

  function "<=" (Left : Fixed_Type ; Right : Price_Type) return Boolean is
  begin
    return Left <= Fixed_Type (Right);
  end "<=";
  ---------------------------------------------

  function "<=" (Left : Price_Type ; Right : Fixed_Type) return Boolean is
  begin
    return Fixed_Type(Left) <= Right;
  end "<=";
  ---------------------------------------------


  ---------------------------------------------


  function ">=" (Left : Profit_Type ; Right : Max_Daily_Profit_Type) return Boolean is
  begin
    return Fixed_Type (Left) >= Fixed_Type (Right);
  end ">=";
  ---------------------------------------------
  function ">=" (Left : Profit_Type ; Right : Max_Daily_Loss_Type) return Boolean is
  begin
    return Fixed_Type (Left) >= Fixed_Type (Right);
  end ">=";
  ---------------------------------------------

  function "=" (Left : Integer_4 ; Right : Num_Winners_Type) return Boolean is
  begin
    return Left = Integer_4 (Right);
  end "=";

  ---------------------------------------------

  function "*" (Left : Bet_Size_Type ; Right : Bet_Price_Type) return Bet_Size_Type is
  begin
    return Left * Bet_Size_Type (Right);
  end "*";
  ---------------------------------------------
  function "*" (Left : Bet_Size_Type ; Right : Price_Type) return Fixed_Type is
  begin
    return Fixed_Type (Left) * Fixed_Type (Right);
  end "*";
  ---------------------------------------------
  function "*" (Left : Bet_Size_Type ; Right : Fixed_Type) return Bet_Size_Type is
  begin
    return Left * Bet_Size_Type (Right);
  end "*";
  ----------------------------------------------------------
  function "*" (Left : Bet_Size_Type ; Right : Bet_Size_Portion_Type) return Bet_Size_Type is
  begin
    return Left * Bet_Size_Type (Right);
  end "*";
  ---------------------------------------------
  function "*" (Left, Right : Bet_Size_Type) return Bet_Size_Type is
  begin
    return Bet_Size_Type (Fixed_Type(Right) * Fixed_Type(Left));
  end "*";
  ----------------------------------------------
  function "*" (Left, Right : Bet_Price_Type) return Bet_Price_Type is
  begin
    return Bet_Price_Type (Fixed_Type (Right) * Fixed_Type (Left));
  end "*";
  ---------------------------------------------
  function "*" (Left : Bet_Size_Type ; Right : Lay_Price_Type) return Fixed_Type is
  begin
    return Fixed_Type (Right) * Fixed_Type (Left);
  end "*";



  -- function "*" (Left : Bet_Size_Type ; Right : Lay_Price_Type) return Fixed_Type is
  -- begin
  --   return Fixed_Type(Left) * Fixed_Type(Right);
  -- end "*";

  ---------------------------------------------

  function "/" (Left : Bet_Size_Type ; Right : Bet_Price_Type) return Bet_Size_Type is
  begin
    return Left / Bet_Size_Type (Right);
  end "/";
  ---------------------------------------------

  function "/" (Left : Fixed_Type ; Right : Back_Price_Type) return Bet_Size_Type is
  begin
    return Bet_Size_Type (Left) / Bet_Size_Type (Right);
  end "/";
  ---------------------------------------------


  function "/" (Left : Bet_Size_Type ; Right : Fixed_Type) return Bet_Size_Type is
  begin
    return Left / Bet_Size_Type (Right);
  end "/";
  --------------------------------------------------

  function "/" (Left : Bet_Size_Type ; Right : Bet_Size_Portion_Type) return Bet_Size_Type is
  begin
    return Left / Bet_Size_Type (Right);
  end "/";

  --------------------------------------------
  function "/" (Left, Right : Bet_Size_Type) return Bet_Size_Type is
  begin
    return Bet_Size_Type (Fixed_Type(Right) / Fixed_Type(Left));
  end "/";
  --------------------------------------------

  function "/" (Left, Right : Bet_Price_Type) return Bet_Price_Type is
  begin
    return Bet_Price_Type (Fixed_Type (Right) / Fixed_Type (Left));
  end "/";
  --------------------------------------------


end Bot_Types;
