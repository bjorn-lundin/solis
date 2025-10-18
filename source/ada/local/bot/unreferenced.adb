

package body unreferenced is

  procedure Try_To_Lay (Bettype  : Config.Bet_Type;
                        Br       : Best_Runners_Array_Type;
                        Marketid : Marketid_Type) is

    Service      : constant String := "Try_To_Lay";
    Max_Layprice : Fixed_Type;
    Min_Layprice : Fixed_Type;
    Image        : String := Bettype'Img;

  begin
    -- we want a favorite and a backup ...
    if Br (2).Backprice > Fixed_Type (5) then
      Log (Service & " " & Bettype'Img & " Br(2).Backprice > 5, skipping" & Br (2).Backprice'Img);
      return;
    end if;

    --1         2         3       4
    --  1234567890123456789012345678901234567890
    --  Horse_Lay_Win_15_00_019_50
    Min_Layprice := Fixed_Type'Value (Image (15 .. 16) & '.' & Image (18 .. 19));
    Max_Layprice := Fixed_Type'Value (Image (21 .. 23) & '.' & Image (25 .. 26));


    for I in Br'Range loop
      Log (Service & " " & Bettype'Img & " I=" & I'Img &
             " Br(I).Selid" & Br (I).Selectionid'Img &
             " Min_Layprice=" & F8_Image (Min_Layprice) &
             " Max_Layprice=" & F8_Image (Max_Layprice) &
             " Br(I).Layprice=" & F8_Image (Br (I).Layprice) &
             " Br(I).Backprice=" & F8_Image (Br (I).Backprice));

      if Min_Layprice <= Br (I).Layprice and then
        Br (I).Layprice <= Max_Layprice then
        -- to get legal odds
        --Tic := Tics.Get_Nearest_Higher_Tic_Index(Br(I).Layprice);
        --Lay_Price := Tics.Get_Tic_Price(Tic+4); -- some small margin to get the bet

        Send_Lay_Bet (Selectionid     => Br (I).Selectionid,
                      Main_Bet        => Bettype,
                      Marketid        => Marketid,
                      Max_Price       => Max_Lay_Price_Type (Br (I).Layprice),
                      Match_Directly  => True,
                      Fill_Or_Kill    => True);
        -- save the bets so we can put correct back bets
      end if;
    end loop;


    Bets_Allowed (Bettype).Has_Betted := True; -- disabled in send_lay_bet/send_back_bet for this type of bets

  end Try_To_Lay;
  pragma Unreferenced(Try_To_Lay);
  -----------------------------------------------------------------------------------

  procedure Try_To_Back_Place_At_Start (Bettype  : Config.Bet_Type;
                                        Br       : Best_Runners_Array_Type;
                                        Marketid : Marketid_Type) is

    Service      : constant String := "Try_To_Back_Place_At_Start";
    Backprice : Fixed_Type;
    Image        : String := Bettype'Img;
  begin

    -- back all horses on place market that has the win-markets backprice
    -- we want a favorite and a backup ...

    --1         2         3       4
    --  1234567890123456789012345678901234567890
    --  Horse_Back_Plc_04_00
    Backprice := Fixed_Type'Value (Image (16 .. 17) & '.' & Image (19 .. 20));

    for I in Br'Range loop
      Log (Service & " " & Bettype'Img & " I=" & I'Img &
             " Br(I).Selid" & Br(I).Selectionid'Img &
             " Backprice=" & F8_Image(Br(I).Backprice));

      if Backprice = Br(I).Backprice then
        Send_Back_Bet (Selectionid     => Br(I).Selectionid,
                       Main_Bet        => Bettype,
                       Marketid        => Marketid,
                       Min_Price       => 1.01, --Price_Type(Backprice),
                       Match_Directly  => True,
                       Fill_Or_Kill    => False);
      end if;
    end loop;

  end Try_To_Back_Place_At_Start;
  pragma Unreferenced(Try_To_Back_Place_At_Start);

  -----------------------------------------------------------------------------------

  -----------------------------------------------------------------------------------
  procedure Try_To_Greenup_Lay_Back (Bettype         : Config.Bet_Type;
                                     Br              : Best_Runners_Array_Type;
                                     Marketid        : Marketid_Type) is

    Service      : constant String := "Try_To_Greenup_Lay_Back";
    Max_Layprice : Fixed_Type;
    Min_Layprice : Fixed_Type;
    Image        : String := Bettype'Img;
    Lay_Bet      : Bets.Bet_Type;
    Lay_Bet_List : Bets.Lists.List;
    Backprice    : Back_Price_Type := 0.0;
    Backsize     : Fixed_Type := 0.0;
    Did_Bet      : Boolean := False;
    Tic          : Integer := 0;
    Betname      : Betname_Type := (others => ' ');

  begin
    -- we want a favorite and a backup ...
    if Br (2).Backprice > Fixed_Type (5) then
      Log (Service & " " & Bettype'Img & " Br(2).Backprice > 5, skipping" & Br (2).Backprice'Img);
      return;
    end if;

    --1         2         3       4
    --  1234567890123456789012345678901234567890
    --  Horse_Greenup_Lay_Back_Win_15_00_19_50
    Min_Layprice := Fixed_Type'Value (Image (28 .. 29) & '.' & Image (31 .. 32));
    Max_Layprice := Fixed_Type'Value (Image (34 .. 35) & '.' & Image (37 .. 38));


    for I in Br'Range loop
      Log (Service & " " & Bettype'Img & " I=" & I'Img &
             " Br(I).Selid" & Br (I).Selectionid'Img &
             " Min_Layprice=" & F8_Image (Min_Layprice) &
             " Max_Layprice=" & F8_Image (Max_Layprice) &
             " Br(I).Layprice=" & F8_Image (Br (I).Layprice) &
             " Br(I).Backprice=" & F8_Image (Br (I).Backprice));

      if Min_Layprice <= Br (I).Layprice and then
        Br (I).Layprice <= Max_Layprice then
        -- to get legal odds
        --Tic := Tics.Get_Nearest_Higher_Tic_Index(Br(I).Layprice);
        --Lay_Price := Tics.Get_Tic_Price(Tic+4); -- some small margin to get the bet

        Send_Lay_Bet (Selectionid     => Br (I).Selectionid,
                      Main_Bet        => Bettype,
                      Marketid        => Marketid,
                      Max_Price       => Max_Lay_Price_Type (Br (I).Layprice),
                      Match_Directly  => True,
                      Fill_Or_Kill    => True);
        Did_Bet := True;
        -- save the bets so we can put correct back bets
      end if;
    end loop;

    if not Did_Bet then
      return;
    end if;

    -- delay for giving last bet time to be placed properly
    Log (Service & "delay 5 secs for giving db processing time");
    delay 5.0;

    -- ok check bets matched. They are either cancelled or fully accepted - match_directly is true
    Lay_Bet.Marketid := Marketid;
    Bets.Read_Marketid (Data => Lay_Bet, List => Lay_Bet_List);
    Move (Image, Betname);
    for Bet of Lay_Bet_List loop
      if Bet.Status (1 .. 18) = "EXECUTION_COMPLETE" and then
        Bet.Betname = Betname then -- or we get hits from similar strategies and double bets :-(
        Tic := Tics.Get_Nearest_Higher_Tic_Index (Bet.Pricematched);
        Backprice := Back_Price_Type (Tics.Get_Tic_Price (Tic + 1));

        declare
          Pricematched_Minus_One : Float := Float (Bet.Pricematched) - Float (1.0);
          Backprice_Minus_One    : Float := Float (Backprice) - Float (1.0);
        begin
          Backsize := Bet.Sizematched * Fixed_Type (Pricematched_Minus_One / Backprice_Minus_One);
        end;

        Send_Back_Bet (Selectionid     => Bet.Selectionid,
                       Main_Bet        => Bettype,
                       Marketid        => Marketid,
                       Min_Price       => Backprice,
                       Match_Directly  => False,
                       Back_Size       => Backsize);
      end if;
    end loop;

    Bets_Allowed (Bettype).Has_Betted := True; -- disabled in send_lay_bet/send_back_bet for this type of bets

  end Try_To_Greenup_Lay_Back;
  pragma Unreferenced (Try_To_Greenup_Lay_Back);
  -----------------------------------------------------------------------------------


end unreferenced;
