


set Horses_Country_List [list GB IE FR SG US ZA]
set Hounds_Country_List [list GB]

set Animal_List [list HORSES HOUNDS]
#set Animal_List [list HORSES ]

set Race_Type_List [list WIN PLC]

set Horses_Bet_Type_List [list LAY1 LAY2 LAY3 LAY4 LAY5 LAY6 LAY7 LAY8 LAY9 FAV2 FAV3 FAV4 FAV5 FAV6]
set Hounds_Bet_Type_List [list LAY1 LAY2 LAY3 LAY4 LAY5 LAY6 FAV2 FAV3 FAV4 FAV5 FAV6]

set Odds_List [list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25]


####################################

proc Max_Odds {Animal Race_Type Bet_Type } {
  switch -exact -- $Animal {
    HORSES  {
      switch -exact -- $Race_Type {
        WIN     {
          switch -exact -- $Bet_Type {
            LAY1    { return 8 }
            LAY2    { return 10 }
            LAY3    { return 12 }
            LAY4    { return 12 }
            LAY5    { return 15 }
            LAY6    { return 15 }
            LAY7    { return 20 }
            LAY8    { return 20 }
            LAY9    { return 20 }
            FAV2    { return 20 }
            FAV3    { return 20 }
            FAV4    { return 20 }
            FAV5    { return 20 }
            FAV6    { return 20 }
            default { puts stderr "bad bet type $Bet_Type"; exit 1}          
          }     
        }
        PLC     {
          switch -exact -- $Bet_Type {
            LAY1    { return 6 }
            LAY2    { return 7 }
            LAY3    { return 8 }
            LAY4    { return 10 }
            LAY5    { return 11 }
            LAY6    { return 12 }
            LAY7    { return 13 }
            LAY8    { return 14 }
            LAY9    { return 15 }
            FAV2    { return 20 }
            FAV3    { return 20 }
            FAV4    { return 20 }
            FAV5    { return 20 }
            FAV6    { return 20 }
            default { puts stderr "bad bet type $Bet_Type"; exit 1}          
          }             
        } 
        default { puts stderr "bad race type $Race_Type"; exit 1}
      }
    }
    HOUNDS  {
      switch -exact -- $Race_Type {
        WIN     {
          switch -exact -- $Bet_Type {
            LAY1    { return 5 }
            LAY2    { return 8 }
            LAY3    { return 10 }
            LAY4    { return 15 }
            LAY5    { return 18 }
            LAY6    { return 20 }
            FAV2    { return 15 }
            FAV3    { return 15 }
            FAV4    { return 15 }
            FAV5    { return 15 }
            FAV6    { return 15 }
            default { puts stderr "bad bet type $Bet_Type"; exit 1}          
          }     
        }
        PLC     {
          switch -exact -- $Bet_Type {
            LAY1    { return 4 }
            LAY2    { return 5 }
            LAY3    { return 6 }
            LAY4    { return 7 }
            LAY5    { return 8 }
            LAY6    { return 9 }
            FAV2    { return 10 }
            FAV3    { return 10 }
            FAV4    { return 10 }
            FAV5    { return 10 }
            FAV6    { return 10 }
            default { puts stderr "bad bet type $Bet_Type"; exit 1}          
          }             
        }
        default { puts stderr "bad race type $Race_Type"; exit 1}
      }
    }  
    default { puts stderr "bad animal $Animal"; exit 1}
  }
}


####################################


proc Min_Runners {Animal Race_Type Bet_Type } {
  switch -exact -- $Animal {
    HORSES  {
      switch -exact -- $Race_Type {
        WIN     {
          switch -exact -- $Bet_Type {
            LAY1    { return 5 }
            LAY2    { return 6 }
            LAY3    { return 7 }
            LAY4    { return 8 }
            LAY5    { return 8 }
            LAY6    { return 8 }
            LAY7    { return 8 }
            LAY8    { return 8 }
            LAY9    { return 9 }
            FAV2    { return 2 }
            FAV3    { return 3 }
            FAV4    { return 4 }
            FAV5    { return 5 }
            FAV6    { return 6 }
            default { puts stderr "bad bet type $Bet_Type"; exit 1}          
          }     
        }
        PLC     {
          switch -exact -- $Bet_Type {
            LAY1    { return 7 }
            LAY2    { return 8 }
            LAY3    { return 9 }
            LAY4    { return 9 }
            LAY5    { return 9 }
            LAY6    { return 9 }
            LAY7    { return 9 }
            LAY8    { return 9 }
            LAY9    { return 9 }
            FAV2    { return 2 }
            FAV3    { return 3 }
            FAV4    { return 4 }
            FAV5    { return 5 }
            FAV6    { return 6 }
            default { puts stderr "bad bet type $Bet_Type"; exit 1}          
          }             
        } 
        default { puts stderr "bad race type $Race_Type"; exit 1}
      }
    }
    HOUNDS  {
          return 6
        }
    default { puts stderr "bad animal $Animal"; exit 1}
  }
}
####################################

proc Max_Runners {Animal Race_Type } {
  switch -exact -- $Animal {
    HORSES  {
      switch -exact -- $Race_Type {
        WIN     { return 25}
        PLC     { return 25}
        default { puts stderr "bad race type $Race_Type"; exit 1}
      }
    }  
    HOUNDS  {
      switch -exact -- $Race_Type {
        WIN     {return 6}
        PLC     {return 6}
        default { puts stderr "bad race type $Race_Type"; exit 1}
      }
    }        
    default { puts stderr "bad animal $Animal"; exit 1}
  }
}

####################################

proc Num_Winners {Animal Race_Type Bet_Type} {
  switch -exact -- $Animal {
    HORSES  {
      switch -exact -- $Race_Type {
        WIN     { return 1}
        PLC     { return 3}
        default { puts stderr "bad race type $Race_Type"; exit 1}
      }
    }  
    HOUNDS  {
      switch -exact -- $Race_Type {
        WIN     {return 1}
        PLC     {return 2}
        default { puts stderr "bad race type $Race_Type"; exit 1}
      }
    }        
    default { puts stderr "bad animal $Animal"; exit 1}
  }
}


####################
#begin
#################

set Bet_Type_List {}
foreach Animal $Animal_List {
    switch -exact -- $Animal {
      HORSES  { 
          set Bet_Type_List $Horses_Bet_Type_List 
          set Country_List $Horses_Country_List 
      }  
      HOUNDS  { 
          set Bet_Type_List $Hounds_Bet_Type_List
          set Country_List $Hounds_Country_List 
      }
    }
    foreach Race_Type $Race_Type_List {
        foreach Country $Country_List {
            set Filename [string tolower $Animal\_$Race_Type\_$Country].ini
            puts $Filename
            set File_Ptr [open $Filename {WRONLY CREAT}]
            puts $File_Ptr "\[Global\]"
            puts $File_Ptr "logging=False"
            puts $File_Ptr ""
            foreach Bet_Type $Bet_Type_List {
                foreach Odds $Odds_List {
                    set max_odds [Max_Odds $Animal $Race_Type $Bet_Type]
                    if { $max_odds >= $Odds } {
                        set max_runners [Max_Runners $Animal $Race_Type]
                        set min_runners [Min_Runners $Animal $Race_Type $Bet_Type]                    
                        set num_winners [Num_Winners $Animal $Race_Type $Bet_Type]                    
                        incr i
                        puts $File_Ptr "\[$Animal\_$Race_Type\_$Bet_Type\_$Country\_$Odds\]"
                        puts $File_Ptr "enabled=True"
                        puts $File_Ptr "bet_size=30.0"
                        puts $File_Ptr "max_daily_profit=100000.0"
                        puts $File_Ptr "max_daily_loss=-200"
                        puts $File_Ptr "allow_in_play=False"
                        puts $File_Ptr "max_odds=$Odds.0"
                        puts $File_Ptr "countries=$Country"
                        puts $File_Ptr "mode=simulation"
                        puts $File_Ptr "no_of_winners=$num_winners"
                        puts $File_Ptr "min_num_runners=$min_runners"
                        puts $File_Ptr "max_num_runners=$max_runners"
                        puts $File_Ptr "allowed_days=al"
                        puts $File_Ptr ""
                    }
                }               
            }     
            close $File_Ptr
        }
    }  
}



