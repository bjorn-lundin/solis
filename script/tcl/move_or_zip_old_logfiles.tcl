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
set Debug 0

#The zip utility to use, defined in Find_Zip_Utility
set Compressor {}

# How long to keep them
set Days_To_Keep_Zip 30

# Where to store zipped files. This dir is created if it does not exist
set Directory_Zipped_Files [file join $::env(BOT_HOME) zipped_logs]
# Config stop
#################################################################################3

set Keep_Zipped_File_Seconds [expr $Days_To_Keep_Zip * 24 * 60 * 60 ]

#-----------------------------------------------------
proc Dbg {what} {
    if {$::Debug} {
        puts stderr "[Get_Time] -> $what"
    }
}
#-----------------------------------------------------
proc Find_Zip_Utility {} {
    #Seems like 'zip' is present on all kinds of platforms
	set Local_Compressor "None"
	set null {}
    switch -exact -- $::tcl_platform(os) {
	    "Windows NT" {set null "nul";       set Local_Compressor "zip" }  
        AIX          {set null "/dev/null"; set Local_Compressor "zip" }  
	    Linux        {set null "/dev/null"; set Local_Compressor "zip" }  
        default      {Dbg "no zip utility defined, exiting" ; exit 1}
    }
    #test to see if it exists, otherwise tell and quit
    if {[catch "exec $Local_Compressor -h 2> $null" Result]} {
        Dbg $Result
        Dbg "zip utility '$Local_Compressor' not found, exiting"
        exit 1
    }
#	Dbg  "Local_Compressor -> '$Local_Compressor'"
	return $Local_Compressor
}
#-----------------------------------------------------
proc Get_Time {} {
    return [clock format [clock seconds] -format "%Y-%b-%d %H:%M:%S"]
}

#-----------------------------------------------------
proc Move_Zipped_Logfiles {f} {
    if {[string match -nocase *zipped_logs [string tolower [pwd]]]} {
        Dbg "we are in zipped_logs, return -> [pwd]"
        return 0
    }
    if {[string equal [string tolower [pwd]] [string tolower $::Directory_Zipped_Files]]} {
        return 0
    }
    
   string match -nocase pattern string    
    
    if {! [file exists $::Directory_Zipped_Files] } {
      if {[catch { file mkdir $::Directory_Zipped_Files} Result]} {
        # Ok create Directory_Zipped_Files failed, log and return?
        Dbg $Result
        Dbg "creating  '$::Directory_Zipped_Files' FAILED"
      } else {
        Dbg "created  '$::Directory_Zipped_Files'"
      }      
    }
    if {[catch { file rename $f $::Directory_Zipped_Files} Result]} {
      # Ok rename failed, try copy/delete
        Dbg $Result
        catch { file delete [file join $::Directory_Zipped_Files $f]}
        Dbg "deleted [file join $::Directory_Zipped_Files $f]"
        
        catch { file copy $f $::Directory_Zipped_Files}
        Dbg "copied $f to  [file join $::Directory_Zipped_Files]"
        catch { file delete $f}
        Dbg "deleted  $f"
        Dbg "tried copy/deleted $f to $::Directory_Zipped_Files"
        return 1
    } else {
        #The rename succeeded
        Dbg "moved $f to $::Directory_Zipped_Files"
        return 1
    }
}

#-----------------------------------------------------
proc Check_Time_To_Delete {f} {
    set Modified [file mtime $f]
    set Timediff [expr [clock seconds] - $Modified ]
    set Delete_File [expr $Timediff > $::Keep_Zipped_File_Seconds ]
    #puts "Check_Time_To_Delete $f"
    if {$Delete_File} {
        Dbg "delete $f due to older than $::Keep_Zipped_File_Seconds seconds"
        file delete $f
    }
}

#-----------------------------------------------------
proc Compress_File {f} {
    Dbg "Will compress and remove file '$f'"
  #  zip -qj all.pg_sql_2.zip all.pg_sql
  #  -q   quiet operation 
  #  -j   junk (don't record) directory names
    set switches "-qj1"
    exec $::Compressor $switches $f.zip $f
#    exec $::Compressor $switches < $f > $f.zip 
# above will give archives with filename = '-' 
	#gzip, and zip -m removes the file, so check existence first
	if {[file exists $f]} {
        catch {file delete $f}
	}  
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
        Dbg "found $f"
        set Is_Log_File_Cir [regexp {([0-9]){2}_([A-Za-z]){3}_([0-9]){4}_([0-9]){2}_([0-9]){2}_([0-9]){2}_([0-9]){3}.cir} \
	                         $f all    day        month         year       hour       minute     second    millsecond]
        set Is_Log_File_Log_Type_I [regexp {([0-9]){8}_([0-9]){9}.log} \
	                               $f  all  date        time]
        set Is_Log_File_Log_Type_II [regexp {([0-9]){4}_([0-9]){2}_([0-9]){2}_([0-9]){2}_([0-9]){2}_([0-9]){2}_([0-9]){3}.log} \
	                               $f  all     year      month       day       hour        minute     second   millsecond]
        #1952                                   
        set Is_Log_File_Log_Type_III [regexp {([0-9]){4}-([0-9]){2}-([0-9]){2}.log} \
	                                 $f  all    year      month        day        time]
        #aws log files
                               
        if {$Is_Log_File_Cir} {   
             #check what to do, the already zipped ones are called bla_bla_date_time.cir.gz 
			 # or bla_bla_date_time.cir.zip
            set Ext [file extension $f]
            switch -exact -- $Ext {
                .cir  - 
                .log {Compress_File $f}
                .zip  - 
                .Z    -
                .gz  {
                        Move_Zipped_Logfiles $f
                        catch {Check_Time_To_Delete $f}
                }
            }
        } elseif  {$Is_Log_File_Log_Type_I || $Is_Log_File_Log_Type_II} { 
             #check what to do, the already zipped ones are called bla_bla_date_time.cir.gz 
			 # or bla_bla_date_time.cir.zip
             #New style logs are zipped by the background process itself
            set Ext [file extension $f]
            switch -exact -- $Ext {
                .zip  - 
                .Z    -
                .gz  {
                        Move_Zipped_Logfiles $f
                        catch {Check_Time_To_Delete $f}
                }
                .log {Compress_File $f}
            }
        #1952    
        } elseif  {$Is_Log_File_Log_Type_III} { 
             Dbg " is type III found $f"
             #this is an aws log file, do not zip/move the one with current date 
            set Modified [file mtime $f]
            set Timediff [expr [clock seconds] - $Modified ]
            set Treat_File [expr $Timediff > 2 * 86400 ]
            set Ext [file extension $f]
            if {$Treat_File} { 
                switch -exact -- $Ext {
                    .log {Compress_File $f}
                }
            }
            # if already zipped, move it. if it was zipped last time, timediff is just 1 hour then
            if {$Ext == ".zip" || $Ext == ".Z" || $Ext == ".gz"} { 
                switch -exact -- $Ext {
                    .zip  - 
                    .Z    -
                    .gz  {
                            Move_Zipped_Logfiles $f
                            catch {Check_Time_To_Delete $f}
                    }
                }
            }
        }
    } else {
#        Dbg "'$f' is neither a file nor a directory"
    }
    cd $Cur_Pwd
}

############### start main ########################

#Dbg "Start"
set ::Compressor [Find_Zip_Utility]
set This_Pwd [pwd]
Traverse_Directories $::env(BOT_HOME)
Traverse_Directories $::env(BOT_TARGET)
cd $This_Pwd
#Dbg "Stop"

############### stop main ########################

