
with Ada.Strings; use  Ada.Strings;
with Ada.Strings.Fixed; use  Ada.Strings.Fixed;
with Ada.Characters;
with Ada.Environment_Variables;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Unicode.Encodings;

--with Text_Io;

package body Utils is
  package EV renames Ada.Environment_Variables;


  function Expand_File_Path (File_Path : String) return String is
    Tmp         : String(File_Path'range) := File_Path;
    Start_Symbol,End_Symbol : Integer         := -1;
    --------------------------------
    procedure Check_Unix_Syntax is
    begin
      for I in Tmp'range loop
        if Tmp(I) = '$' then
          if Start_Symbol = -1 then Start_Symbol := I;  end if;
        end if;
        if Start_Symbol /= -1 then
          if Tmp(I) = '/' then  End_Symbol := I-1; exit; end if;
        end if;
      end loop;
    end Check_Unix_Syntax;
    --------------------------------
    function Expand_Symbol(Tmp : String) return String is
    begin
      return EV.Value(To_Upper(Tmp(Start_Symbol+1..End_Symbol)));
    end Expand_Symbol;
    --------------------------------
  begin
    --Text_Io.Put_Line("arg: '" & File_Path & "'");
    Check_Unix_Syntax;

    if Start_Symbol /= -1 and End_Symbol /= -1 then
      if    Start_Symbol /= Tmp'First and End_Symbol /= Tmp'Last then
        return Tmp(Tmp'First..Start_Symbol-1) &
               Expand_Symbol(Tmp(Start_Symbol..End_Symbol)) &
               Tmp(End_Symbol+1..Tmp'Last);
      else
        if Start_Symbol /= Tmp'First then
          return Tmp(Tmp'First..Start_Symbol-1) &
                 Expand_Symbol(Tmp(Start_Symbol..End_Symbol));
        else
          return Expand_Symbol(Tmp(Start_Symbol..End_Symbol))
                 & Tmp(End_Symbol+1..Tmp'Last);
        end if;
      end if;
    else
      if Start_Symbol /= -1  then
        End_Symbol := Tmp'Last;
        if Start_Symbol /= Tmp'First then
          return Tmp(Tmp'First..Start_Symbol-1) &
                 Expand_Symbol(Tmp(Start_Symbol..End_Symbol));
        else
          return Expand_Symbol(Tmp(Start_Symbol..End_Symbol));
        end if;
      else
        return Tmp;
      end if;
    end if;

  end Expand_File_Path;

  --------------------------------------------------------
   -------------------------------------
   function Position (S, Match : String) return Integer is
   begin
      if Match'Length > 0 then
         for I in S'First .. S'Last - Match'Length + 1 loop
            if S (I .. I + Match'Length - 1) = Match then
              return I;
            end if;
         end loop;
      end if;
      return S'First - 1;
   end Position;
   -------------------------------------
   function Skip_All_Blanks (S : String) return String is
      Result : String (S'range);
      To     : Integer := Result'First - 1;
   begin
      for I in S'range loop
         if S (I) /= ' ' then
            To := To + 1;
            Result (To) := S (I);
         end if;
      end loop;
      return Result (Result'First .. To);
   end Skip_All_Blanks;
   -------------------------------------

   function Trim (What : String) return String is
   begin
      return Trim (What, Both);
   end Trim;
   -------------------------------------

--   function F8_Image(F : Fixed_Type; Aft : Natural := 2 ; Exp : Natural := 0) return String is
   function F8_Image(F : Fixed_Type) return String is
      S : String(1..15) := (others => ' ');
      Dotpos : Integer := 0;
   begin
   Move(F'Img, S);
    for I in S'Range loop
      case S(I) is
        when '.' => Dotpos := I ; exit;
        when others => null;
      end case;
    end loop;

    declare
      Int  : Integer_8 := Integer_8'Value(S(S'first..Dotpos-1));
      rest : Integer_8 := Integer_8'Value(S(Dotpos+1..S'Last));
      New_Rest : String(1..2) := "00";
    begin
      case Rest is
        when 000 .. 004 => New_Rest := "00";
        when 005 .. 014 => New_Rest := "01";
        when 015 .. 024 => New_Rest := "02";
        when 025 .. 034 => New_Rest := "03";
        when 035 .. 044 => New_Rest := "04";
        when 045 .. 054 => New_Rest := "05";
        when 055 .. 064 => New_Rest := "06";
        when 065 .. 074 => New_Rest := "07";
        when 075 .. 084 => New_Rest := "08";
        when 085 .. 094 => New_Rest := "09";

        when 095 .. 104 => New_Rest := "10";
        when 105 .. 114 => New_Rest := "11";
        when 115 .. 124 => New_Rest := "12";
        when 125 .. 134 => New_Rest := "13";
        when 135 .. 144 => New_Rest := "14";
        when 145 .. 154 => New_Rest := "15";
        when 155 .. 164 => New_Rest := "16";
        when 165 .. 174 => New_Rest := "17";
        when 175 .. 184 => New_Rest := "18";
        when 185 .. 194 => New_Rest := "19";

        when 195 .. 204 => New_Rest := "20";
        when 205 .. 214 => New_Rest := "21";
        when 215 .. 224 => New_Rest := "22";
        when 225 .. 234 => New_Rest := "23";
        when 235 .. 244 => New_Rest := "24";
        when 245 .. 254 => New_Rest := "25";
        when 255 .. 264 => New_Rest := "26";
        when 265 .. 274 => New_Rest := "27";
        when 275 .. 284 => New_Rest := "28";
        when 285 .. 294 => New_Rest := "29";

        when 295 .. 304 => New_Rest := "30";
        when 305 .. 314 => New_Rest := "31";
        when 315 .. 324 => New_Rest := "32";
        when 325 .. 334 => New_Rest := "33";
        when 335 .. 344 => New_Rest := "34";
        when 345 .. 354 => New_Rest := "35";
        when 355 .. 364 => New_Rest := "36";
        when 365 .. 374 => New_Rest := "37";
        when 375 .. 384 => New_Rest := "38";
        when 385 .. 394 => New_Rest := "39";

        when 395 .. 404 => New_Rest := "40";
        when 405 .. 414 => New_Rest := "41";
        when 415 .. 424 => New_Rest := "42";
        when 425 .. 434 => New_Rest := "43";
        when 435 .. 444 => New_Rest := "44";
        when 445 .. 454 => New_Rest := "45";
        when 455 .. 464 => New_Rest := "46";
        when 465 .. 474 => New_Rest := "47";
        when 475 .. 484 => New_Rest := "48";
        when 485 .. 494 => New_Rest := "49";

        when 495 .. 504 => New_Rest := "50";
        when 505 .. 514 => New_Rest := "51";
        when 515 .. 524 => New_Rest := "52";
        when 525 .. 534 => New_Rest := "53";
        when 535 .. 544 => New_Rest := "54";
        when 545 .. 554 => New_Rest := "55";
        when 555 .. 564 => New_Rest := "56";
        when 565 .. 574 => New_Rest := "57";
        when 575 .. 584 => New_Rest := "58";
        when 585 .. 594 => New_Rest := "59";

        when 595 .. 604 => New_Rest := "60";
        when 605 .. 614 => New_Rest := "61";
        when 615 .. 624 => New_Rest := "62";
        when 625 .. 634 => New_Rest := "63";
        when 635 .. 644 => New_Rest := "64";
        when 645 .. 654 => New_Rest := "65";
        when 655 .. 664 => New_Rest := "66";
        when 665 .. 674 => New_Rest := "67";
        when 675 .. 684 => New_Rest := "68";
        when 685 .. 694 => New_Rest := "69";

        when 695 .. 704 => New_Rest := "70";
        when 705 .. 714 => New_Rest := "71";
        when 715 .. 724 => New_Rest := "72";
        when 725 .. 734 => New_Rest := "73";
        when 735 .. 744 => New_Rest := "74";
        when 745 .. 754 => New_Rest := "75";
        when 755 .. 764 => New_Rest := "76";
        when 765 .. 774 => New_Rest := "77";
        when 775 .. 784 => New_Rest := "78";
        when 785 .. 794 => New_Rest := "79";

        when 795 .. 804 => New_Rest := "80";
        when 805 .. 814 => New_Rest := "81";
        when 815 .. 824 => New_Rest := "82";
        when 825 .. 834 => New_Rest := "83";
        when 835 .. 844 => New_Rest := "84";
        when 845 .. 854 => New_Rest := "85";
        when 855 .. 864 => New_Rest := "86";
        when 865 .. 874 => New_Rest := "87";
        when 875 .. 884 => New_Rest := "88";
        when 885 .. 894 => New_Rest := "89";

        when 895 .. 904 => New_Rest := "90";
        when 905 .. 914 => New_Rest := "91";
        when 915 .. 924 => New_Rest := "92";
        when 925 .. 934 => New_Rest := "93";
        when 935 .. 944 => New_Rest := "94";
        when 945 .. 954 => New_Rest := "95";
        when 955 .. 964 => New_Rest := "96";
        when 965 .. 974 => New_Rest := "97";
        when 975 .. 984 => New_Rest := "98";
        when 985 .. 994 => New_Rest := "99";

        when 995 .. 999 => Int := Int +1; New_Rest := "00";
        when others     => New_Rest := "00";
      end case;

      return Trim(Int'Img) & "." & New_Rest;
    end;
  end F8_Image;
   -------------------------------------

   function Lower_Case (C : Character) return Character is
   begin
      return Ada.Characters.Handling.To_Lower (C);
   end Lower_Case;
   -------------------------------------

   function Lower_Case (S : String) return String is
      Result     : String (S'Range) := S;
      Tmp_Result : String := Ada.Characters.Handling.To_Lower (S);
   begin
      Result := Tmp_Result (Tmp_Result'First .. Tmp_Result'Last);
      return Result;
   end Lower_Case;
   -------------------------------------


   function Upper_Case (C : Character) return Character is
   begin
      return Ada.Characters.Handling.To_Upper (C);
   end Upper_Case;
   -------------------------------------


   function Upper_Case (S : String) return String is
      Result     : String (S'Range) := S;
      Tmp_Result : String := Ada.Characters.Handling.To_Upper (S);
   begin
      Result := Tmp_Result (Tmp_Result'First .. Tmp_Result'Last);
      return Result;
   end Upper_Case;
   -------------------------------------


  -- Convert a string value to ISOLATIN
  --============================================================================
  function To_Iso_Latin_15(Str : Unicode.CES.Byte_Sequence) return String is
    use Unicode.Encodings;
  begin
    return  Convert(Str  => Str,
                    From => Get_By_Name("utf-8"),
                    To   => Get_By_Name("iso-8859-15"));
  exception
    when Unicode.CES.Invalid_Encoding => return Str;
  end To_Iso_Latin_15;

  --============================================================================
  -- Convert a string value to UTF8
  --============================================================================
  function To_Utf8(Str : Unicode.CES.Byte_Sequence) return String is
    use Unicode.Encodings;
  begin
    return  Convert(Str  => Str,
                    From => Get_By_Name("iso-8859-15"),
                    To => Get_By_Name("utf-8"));
  exception
    when Unicode.CES.Invalid_Encoding => return Str;
  end To_Utf8;


end Utils;
