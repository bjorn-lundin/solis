

#l=lowest match odds
#h=highest match odds
#d=highest diff match odds
#r=max price any unquoted
#n=name

proc do_echo {low high d r n} { 
  puts "\[HUMAN_${n}_${low}_${high}_${d}_${r}_LAY_AL\]"
  puts "enabled=true"
  puts "bet_size=30.0"
  puts "max_daily_profit=100000.0"
  puts "max_daily_loss=-5000.0"
  puts "countries=AL"
  puts "max_price=$high"
  puts "min_price=$low"
  puts "mode=sim"
  puts "no_of_winners=1"
  puts "min_num_runners=3"
  puts "max_num_runners=25"
  puts "allowed_days=al"
  puts "green_up_mode=None"
  puts "delta_price=$d"
  puts "max_num_in_the_air=15"
  puts "max_exposure=1200.0"
  puts "min_num_runners_better_ranked=100"
  puts "race_favorite_max_price=$r"
  
#  if { ${n} == "HALF-TIME-SCORE" } {
#    puts "pass_on_half_time_score=true"
#  }
#  if { ${n} == "FULL-TIME-SCORE" } {
#    puts "pass_on_full_time_score=true"
#  }
  
  puts ""
}


puts "\[global\]"
puts "bot_mode=simulation"
puts ""

foreach l [list 2.0 ] {
  foreach h [list 12.0]  {
    if {[expr $l < $h ]} {
      foreach d [list 5.0 10.0 ] {
        foreach r [list 20 30 40 50 60 70 80 ] {
          foreach n [list HALF-TIME-SCORE FULL-TIME-SCORE ] {
            do_echo $l $h $d $r $n
          }
        }
      }
    }
  }
}

