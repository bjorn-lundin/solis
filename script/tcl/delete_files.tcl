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
#################################################################################3


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
proc Delete_File {f} {
  if {[file exists $f]} {
    Dbg "delete $f"
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
      #  Dbg "found $f"
        set Is_Msm_File [string match */msm* $f]
        set Is_Jmb_File [string match */jmb* $f]
        set Is_Soc_File [string match */soc* $f]
        set Is_Ael_File [string match */ael* $f]
        set Is_Ghd_File [string match */ghd* $f]
        set Is_Schema   [string match *.dmp $f]
                               
        if {$Is_Msm_File || $Is_Jmb_File || $Is_Soc_File || $Is_Ael_File || $Is_Ghd_File || $Is_Schema } {   
           Delete_File $f
        }
    } else {
        Dbg "'$f' is neither a file nor a directory"
    }
    cd $Cur_Pwd
}

############### start main ########################

#Dbg "Start"
set This_Pwd [pwd]
Traverse_Directories [file join / data betbot data raw ]
cd $This_Pwd
#Dbg "Stop"

############### stop main ########################

