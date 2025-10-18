

package require Pgtcl

set db "bnl"
#set host "localhost"
#set host "192.168.1.7"
set host "192.168.1.20"
set port 5432
set user "bnl"
set pass "bnl"
#set pass "ld4BC9Q51FU9CYjC21gp"

set Bet_Name_List [list [lindex $argv 0]]
set Powerday_List [list 107]

global Weekday_Profit_Array(null)
global Weekday_Count_Array(null)
global Weekday_Count_Pct_Array(null)

set ::conn [pg_connect $db -host $host -port $port -user $user -password $pass]

foreach Bet_Name $Bet_Name_List {
  puts "<?xml version=\'1.0\' encoding=\'iso8859-1\'?>"
  puts "<chapter xml:id=\'$Bet_Name\' xmlns=\'http://docbook.org/ns/docbook\' version=\'5.0\' >"
  puts "<title>$Bet_Name</title>"

  foreach Powerday $Powerday_List {
    puts "<section xml:id=\'$Bet_Name\_$Powerday\'>"
   # puts "<title>$Powerday</title>"
   # puts "<mediaobject>"
   # puts "  <imageobject>"
   # puts "    <imagedata fileref=\'../script/plot/$Bet_Name\-$Powerday.png\'"
   # puts "               align=\'left\'"
   # puts "               width=\'80%\'"
   # puts "               scalfit=\'1\'/>"
   # puts "  </imageobject>"
   # puts "</mediaobject>"
#        -- b.startts::date > (select current_date - interval '420 days') \

    set query " select \
        count('a'), \
        round(avg(b.profit)::numeric, 1) as avgprofit, \
        round(sum(b.profit)::numeric, 0) as sumprofit, \
        round(avg(b.pricematched)::numeric, 3) as avgprice, \
        round((sum(b.profit)/avg(b.pricematched))::numeric, 0) as sumprofit_price, \
        min(b.startts)::date as mindate, \
        max(b.startts)::date as maxdate, \
        max(b.startts)::date - min(b.startts)::date  + 1 as days, \
        round(count('a')/(max(b.startts)::date - min(b.startts)::date  + 1)::numeric,1) as betsperday, \
        round((sum(profit)/(max(b.startts)::date - min(b.startts)::date  + 1))::numeric, 0) as profitperday, \
        count('b'), \
        b.betname, \
        case \
          when b.betname like '%LAY%' then round((sum(b.profit)/(avg(b.sizematched)* (avg(b.pricematched) -1)))::numeric, 0) \
          else round((100.0 * sum(b.profit)/sum(b.sizematched))::numeric, 3) \
        end as riskratio ,  \
        avg(b.sizematched) as avg_size \
      from \
        abets b \
      where \
        b.startts::date > '2013-07-30' \
        and b.status = 'SETTLED' \
        and b.betwon is not null \
        and b.betname = '[string toupper $Bet_Name]' \
      group by \
        b.betname \
      having sum(b.profit) > -100000000.0"
    #puts stderr $query    
    set res [pg_exec $::conn $query]

    set ntups [pg_result $res -numTuples]
    set Tuples {}
    for {set i 0} {$i < $ntups} {incr i} {
        lappend Tuples [pg_result $res -getTuple $i]
    }
    if { $ntups == 0 } {
       set Tuples [ list "0 0 0 0 0 - - 0 0 0 0"]
    }

    pg_result $res -clear

    puts "<table><title>Interesting facts about $Bet_Name</title>"
    puts "<tgroup cols=\"12\">"
    puts "<thead><row><entry>Country</entry><entry>Profit/Risk (%)</entry><entry>Count</entry>"
    puts "<entry>avg(Profit)</entry><entry>sum(Profit)</entry><entry>avg(Price)</entry>"
    puts "<entry>-</entry><entry>min(Date)</entry><entry>max(Date)</entry>"
    puts "<entry>num(Days)</entry><entry>Bets/Day</entry><entry>Profit/Day</entry></row></thead><tbody>"

    foreach Tuple $Tuples {
      puts "<row><entry>no country</entry><entry>[lindex $Tuple 12]</entry><entry>[lindex $Tuple 0]</entry>"
      puts "<entry>[lindex $Tuple 1]</entry><entry>[lindex $Tuple 2]</entry><entry>[lindex $Tuple 3]</entry>"
      puts "<entry>-</entry><entry>[lindex $Tuple 5]</entry><entry>[lindex $Tuple 6]</entry>"
      puts "<entry>[lindex $Tuple 7]</entry><entry>[lindex $Tuple 8]</entry><entry>[lindex $Tuple 9]</entry></row>"
    }
    puts "</tbody></tgroup></table>"
#       -- b.startts::date > (select current_date - interval '420 days') \

    # how is the income spread across weekdays?
    set query " select \
        round(sum(b.profit)::numeric, 0) as sumprofit, \
        b.betname, \
        extract(dow from b.startts ) as weekday, \
        count(b.profit) as count \
      from \
        abets b
      where \
        b.startts::date > '2013-07-30' \
        and b.status = 'SETTLED' \
        and b.betwon is not null \
        and b.betname = '[string toupper $Bet_Name]' \
      group by \
        b.betname, \
        weekday \
      having sum(b.profit) > -100000000.0 \
      order by \
        weekday "
        
    #puts stderr $query
    set res [pg_exec $::conn $query]

    set ntups [pg_result $res -numTuples]
    set Tuples {}
    for {set i 0} {$i < $ntups} {incr i} {
        lappend Tuples [pg_result $res -getTuple $i]
    }
    if { $ntups == 0 } {
       set Tuples [ list "0 0 0 0 0"]
    }

    pg_result $res -clear

    puts "<table><title>Profit / num bets / % num bets distributed per weekday </title>"
    puts "<tgroup cols=\"7\">"
    puts "<thead><row><entry>mon</entry><entry>tue</entry><entry>wed</entry><entry>thu</entry><entry>fri</entry><entry>sat</entry>"
    puts "<entry>sun</entry></row></thead>"
    puts "<tbody>"

    set Days [list 1 2 3 4 5 6 0 ]

    foreach Weekday $Days {
      if {![info exists Weekday_Profit_Array($Weekday) ]} {
        set Weekday_Profit_Array($Weekday) 0
      }
      if {![info exists Weekday_Count_Array($Weekday) ]} {
        set Weekday_Count_Array($Weekday) 0
      }
      if {![info exists Weekday_Count_Pct_Array($Weekday) ]} {
        set Weekday_Count_Pct_Array($Weekday) 0
      }
    }

    foreach Tuple $Tuples {
      set Weekday [lindex $Tuple 2]
      set Weekday_Profit_Array($Weekday) [lindex $Tuple 0]
      set Weekday_Count_Array($Weekday) [lindex $Tuple 3]
    }

    set Tot_Num_Bets 0
    foreach Weekday $Days {
      set Tot_Num_Bets [expr $Tot_Num_Bets + $Weekday_Count_Array($Weekday)]
    }
    set Tot_Num_Bets  [expr $Tot_Num_Bets + 0.0] ; # make it a float
    
    foreach Weekday $Days {
      if {$Tot_Num_Bets > 0 } {
        set Weekday_Count_Pct_Array($Weekday) "[format "%.0f" [expr 100 * $Weekday_Count_Array($Weekday) / $Tot_Num_Bets ]]%"
      }
    }
    
    puts "<row>"
    foreach Weekday $Days {
      puts "<entry>$Weekday_Profit_Array($Weekday)/$Weekday_Count_Array($Weekday)/$Weekday_Count_Pct_Array($Weekday)</entry>"
    }
    puts "</row>"    
    puts "</tbody></tgroup></table>"
    puts "</section>"
    puts "<?hard-pagebreak?>"
  }
  puts "</chapter>"
  pg_disconnect $::conn
}



