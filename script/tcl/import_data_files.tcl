#
# 07-Aug-2009	    Björn Lundin
# script to be called every hour
# some_log_15_May_2009_22_14_34.cir is produced by a process
# when the cir file is about to reset. Instead, close
# the file, rename it with date + time 
# and open a new. This also applies to .log files
#This script compresses the files as only zip can
# and also removes the cir file. Also, if a .gz or .zip file
# is found, it is deleted if older than Days_To_Keep_Zip days

#1952 - BNL - 2013-06-03 zip/move aws log fies too
#

###############################################################
# Config start
#Want output? If set, writes to stderr. 
set Debug 1
################################################################################


#-----------------------------------------------------
proc Dbg {what} {
    if {$::Debug} {
        puts stderr "[Get_Time] -> $what"
    }
}
#-----------------------------------------------------
proc Get_Time {} {
    return [clock format [clock seconds] -format "%Y-%b-%dT%H:%M:%S"]
}

#-----------------------------------------------------

proc Import {f} {

  if { [catch { exec zcat $f | psql --no-psqlrc bnl } msg] } {
    puts "Bad : $f"
    puts "Information about it: $::errorInfo"
    puts "Information about it: $msg"
  } else {
    Dbg "imported $f"
  }

}


proc Import_Zip {f} {
  Import $f
}

proc Import_Gz {f} {
  Import $f
}



#-----------------------------------------------------
proc Is_To_Be_Imported {f} {

  set Import 0
  switch -glob $f {
    */bnl_* {
            switch -glob $f {
              *apriceshistory* {set Import 0}
              *abalance*       {set Import 0}
              *abets*          {set Import 0}
              default          {set Import 1}
            }
    }

    */dry_* {
            switch -glob $f {
              *apriceshistory* {set Import 0}
              *abalance*       {set Import 0}
              default          {set Import 0}
            }
    }
  }

  return $Import
}

#-----------------------------------------------------

proc Traverse_Directories {f} {
    set Cur_Pwd [pwd]
    if {[file isdirectory $f]} {
#        Dbg "cd to '$f' from $Cur_Pwd"
        if {[catch {cd $f} Result]} {
            Dbg $Result
            return
        }
        foreach g [glob -nocomplain *] {
	        Traverse_Directories [file join [pwd] $g]
	    }

    } elseif {[file isfile $f]} {
      #  Dbg "found $f"
        set Is_To_Be_Imported_File [Is_To_Be_Imported $f]

        if {$Is_To_Be_Imported_File } {
          set Is_Zip_File [string match *.zip $f]
          set Is_Gz_File  [string match  *.gz $f]
          if {$Is_Zip_File} {
            Import_Zip $f
          } elseif {$Is_Gz_File} {
            Import_Gz $f
          } else {
            Dbg "'$f' is neither a zip nor a gz"
          }
        }
    } else {
        Dbg "'$f' is neither a file nor a directory"
    }
    cd $Cur_Pwd
}

############### start main ########################

#Dbg "Start"
set This_Pwd [pwd]

set YEAR [lindex $::argv 0]
set MONTH [lindex $::argv 1]
set DAY [lindex $::argv 2]


if { ($DAY == "0") && ($MONTH == "0") } {
  Traverse_Directories [file join / usr2 data db_dumps $YEAR]
} elseif  { $DAY == "0" } {
  Traverse_Directories [file join / usr2 data db_dumps $YEAR $MONTH]
} else {
  Traverse_Directories [file join / usr2 data db_dumps $YEAR $MONTH $DAY]
}

#Traverse_Directories [file join / usr2 data db_dumps $YEAR $MONTH $DAY]
cd $This_Pwd
#Dbg "Stop"

############### stop main ########################

