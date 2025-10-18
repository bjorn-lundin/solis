with Ada.Strings.Fixed;
with Logging; use Logging;
with Ada.Characters.Handling;
with Ini;
with Utils;
with Text_Io;


package body Config is
  Me       : constant String := "Config.";
  Bad_Data : exception;
  -------------------------------------------------------------
  function Create(Filename : String) return Config_Type is
    Service : constant String := "Create";
    Cfg     : Config_Type;
  begin
    text_io.put_line(Me & Service &  " read ini file :'" & Filename & "'");
    Log(Me & Service, "read ini file :'" & Filename & "'");

    Ini.Load(Filename);
    Cfg.Enabled           := Ini.Get_Value("global","enabled",False);
    Cfg.Allowed_Countries := To_Unbounded_String(Ini.Get_Value("global","countries",""));
    Cfg.Max_Exposure      := Fixed_Type'Value(Ini.Get_Value("global","max_exposure","5000.0")); -- -1.0 -> -100% of size
    Cfg.Max_Total_Loss_Per_Day := Fixed_Type'Value(Ini.Get_Value("global","max_total_loss_per_day","-800.0"));

    for I in Bet_Type'Range loop
      Cfg.Bet(I).Size := Bet_Size_Type'Value(Ini.Get_Value(I'Img,"size","1.0"));
      Cfg.Bet (I).Max_Loss_Per_Day := Fixed_Type'Value (Ini.Get_Value (I'Img, "max_loss_per_day", "-1.0")); -- -1.0 -> -100% of size
      Cfg.Bet (I).Max_Earnings_Per_Day := Fixed_Type'Value (Ini.Get_Value (I'Img, "max_earnings_per_day", "1_000_000.0"));
      Cfg.Bet(I).Min_Price := To_Unbounded_String(Ini.Get_Value(I'Img,"min_price","1.01"));
      Cfg.Bet(I).Enabled := Ini.Get_Value(I'Img,"enabled",False);
      Cfg.Bet(I).Chase_Allowed := Ini.Get_Value(I'Img,"chase",False);
      Cfg.Bet(I).Hurdle_Allowed := Ini.Get_Value(I'Img,"hurdle",False);
    end loop;

    declare
      use Ada.Characters.Handling;
      use Ada.Strings.Fixed;
      Days : String := Ini.Get_Value("global","allowed_days","no");
      use Calendar2;
      Zero : Natural := 0;
    begin
      Cfg.Allowed_Days := (others => True); --default to ok
      if To_Lower(Days) /= "no" then
        for I in Week_Day_Type'Range loop
          Cfg.Allowed_Days(I) := Index(To_Lower(Days), To_Lower(I'Img)(1..2)) > Zero;
          Log(Me & Service, I'Img & " To_Lower(J'Img)(1..2) in To_Lower(Days) " & To_Lower(I'Img)(1..2) & " in " & To_Lower(Days) & " " & Cfg.Allowed_Days(I)'img );
        end loop;
      else
        Cfg.Allowed_Days := (others => True); --ok if no entry
      end if;
    end;

    -- override allowed days
    for I in Bet_Type'Range loop
      Cfg.Bet(I).Allowed_Days := (others => True); --default to ok
      declare
        use Ada.Characters.Handling;
        use Ada.Strings.Fixed;
        Days : String := Ini.Get_Value(I'Img,"allowed_days","no");
        use Calendar2;
        Zero : Natural := 0;
      begin
        if To_Lower(Days) /= "no" then
          for J in Week_Day_Type'Range loop
            Cfg.Bet(I).Allowed_Days(J) := Index(To_Lower(Days), To_Lower(J'Img)(1..2)) > Zero;
            Log(Me & Service, J'Img & " override " & I'Img & " To_Lower(J'Img)(1..2) in To_Lower(Days)" & To_Lower(J'Img)(1..2) & " in " & To_Lower(Days) & " " & Cfg.Bet(I).Allowed_Days(J)'img );
          end loop;
        end if;
      end;
    end loop;

    return Cfg;
  end Create;
  -------------------------------------------------------------
  function To_String(Cfg : Config_Type) return String is
    use Utils;
    use Calendar2;
    use Ada.Characters.Handling;
    Part1 : String :=
              "<config>" &
              "<enabled>" & Cfg.Enabled'Img & "</enabled>" &
              "<max_turns_not_started_race>" & Cfg.Max_Turns_Not_Started_Race'Img & "</max_turns_not_started_race>" &
              "<allowed_countries>" & To_String(Cfg.Allowed_Countries) & "</allowed_countries>" &
              "<max_exposure>" & F8_Image(Cfg.Max_Exposure) & "</max_exposure>" &
            "<max_total_loss_per_day>" & F8_Image(Cfg.Max_Total_Loss_Per_Day) & "</max_total_loss_per_day>" ;
    Part3 : String := "</config>";
    Days  : Unbounded_String := Null_Unbounded_String;
    Days_In_Bet  : Unbounded_String := Null_Unbounded_String;
    Bets  : Unbounded_String := Null_Unbounded_String;
  begin

    Append(Days, "<days>");
    for I in Week_Day_Type'Range loop
      Append(Days, "<" & To_Lower(I'Img) & ">" &
               To_Lower(Cfg.Allowed_Days(I)'Img) &
               "</" & To_Lower(I'Img) & ">" );
    end loop;
    Append(Days, "</days>");

    Append(Bets, "<bets>");
    for I in Bet_Type'Range loop

      Days_In_Bet := Null_Unbounded_String;
      Append(Days_In_Bet, "<days>");
      for J in Week_Day_Type'Range loop
        Append(Days_In_Bet, "<" & To_Lower(J'Img) & ">" &
                 To_Lower(String'(Cfg.Bet(I).Allowed_Days(J)'Img)) &
                 "</" & To_Lower(J'Img) & ">" );
      end loop;
      Append(Days_In_Bet, "</days>");


      Append(Bets, "<" & To_Lower(I'Img) & ">" &
               "<size>" & F8_Image(Fixed_Type(Cfg.Bet(I).Size)) & "</size>" &
               "<max_loss_per_day>" & F8_Image(Cfg.Bet(I).Max_Loss_Per_Day) & "</max_loss_per_day>" &
               "<max_earnings_per_day>" & F8_Image(Cfg.Bet(I).Max_Earnings_Per_Day) & "</max_earnings_per_day>" &
               "<min_price>" & To_String(Cfg.Bet(I).Min_Price) & "</min_price>" &
               "<enabled>" & Cfg.Bet(I).Enabled'Img & "</enabled>" &
               "<chase_allowed>" & Cfg.Bet(I).Chase_Allowed'Img & "</chase_allowed>" &
               "<hurdle_allowed>" & Cfg.Bet(I).Hurdle_Allowed'Img & "</hurdle_allowed>" &
               Days_In_Bet &
             "</" & To_Lower(I'Img) & ">" );
    end loop;
    Append(Bets, "</bets>");

    return Part1 & To_String(Days) & To_String(Bets) & Part3;

  end To_String;
  -------------------------------------------------------------
  function Country_Is_Ok (Cfg : Config_Type; Country_Code : String) return Boolean is
    Service   : constant String := "Country_Is_Ok";
    -- Allowed country ?
    --Countries is a ',' separated list of 2 char abbrevations.
    Countries : String := Utils.Upper_Case(To_String(Cfg.Allowed_Countries));
    Cntry     : String(1..2) := (others => ' ');
    Index     : Integer := 1;
    Found     : Boolean := False;
  begin
    for I in Countries'Range loop
      case Countries(I) is
        when ',' =>
          if Cntry = Country_Code then
            Found := True;
            exit;
          end if;
        when others =>
          case Index is
            when 1 =>
              Cntry(1) := Countries(I);
              Index := 2;
            when 2 =>
              Cntry(2) := Countries(I);
              Index := 1;
            when others => raise Bad_Data with "Index = " & Index'Img;
          end case;
      end case;
    end loop;
    -- check also for the last entry (EN,IE)
    if Cntry = Country_Code then
      Found := True;
    elsif Cntry = "AL" then
      Found := True;
    elsif Countries = "AL" then
      Found := True;
    end if;

    if not Found then
      Log(Me & Service, "wrong country. OK countries are :'" & Countries & "' market country is '" & Country_Code & "'");
    end if;
    return Found;
  end Country_Is_Ok;
  -------------------------------------------------------------

  procedure Print_Strategies is
  begin

    for I in Bet_Type'Range loop
      Text_Io.Put(I'Img & " ");
    end loop;

  end Print_Strategies;



end Config;
