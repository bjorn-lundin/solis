#!/bin/sh
# \
exec tclsh $0 $*

# ExtractDefinition :
# Get the description of tables in database provided
# as first argument to the script, in XML format, to stdout
# Björn Lundin 2003-06-21



package require Pgtcl

proc getFields {Connection The_Table} {
        set Fields {}
        set res [pg_exec $Connection \
          "SELECT a.attname, \
             pg_catalog.format_type(a.atttypid, a.atttypmod), \
             a.attnotnull, a.atthasdef, a.attnum \
           FROM pg_catalog.pg_attribute a \
           WHERE a.attrelid = (select relfilenode from pg_catalog.pg_class \
                               where relname = '$The_Table') \
           AND a.attnum > 0 AND NOT a.attisdropped \
           ORDER BY a.attnum"]
        set ntups [pg_result $res -numTuples]
        set Fielddata {}
        for {set i 0} {$i < $ntups} {incr i} {
           lappend Fielddata [pg_result $res -getTuple $i]
        }
        puts "    <Fields>"
        foreach Field $Fielddata {
          puts "      <Fielddesc>"
          puts "        <Name>[lindex $Field 0]</Name>"
          puts "        <Type>[lindex $Field 1]</Type>"
          puts "        <NotNull>[lindex $Field 2]</NotNull>"
          puts "        <HasDef>[lindex $Field 3]</HasDef>"
          puts "        <FieldNum>[lindex $Field 4]</FieldNum>"
          puts "      </Fielddesc>"
        }
        puts "    </Fields>"
        pg_result $res -clear
}

proc getKeys {Connection The_Table} {
        set Fields {}
        set res [pg_exec $Connection \
          "SELECT c2.relname, i.indisprimary, i.indisunique, \
                 pg_catalog.pg_get_indexdef(i.indexrelid) \
           FROM pg_catalog.pg_class c, pg_catalog.pg_class c2, pg_catalog.pg_index i \
           WHERE c.oid = (select relfilenode from pg_catalog.pg_class \
                               where relname = '$The_Table') \
           AND c.oid = i.indrelid AND i.indexrelid = c2.oid \
           ORDER BY i.indisprimary DESC, i.indisunique DESC, c2.relname"]
        set ntups [pg_result $res -numTuples]
        set Keydata {}
        for {set i 0} {$i < $ntups} {incr i} {
           lappend Keydata [pg_result $res -getTuple $i]
        }
        puts "    <Keys>"
        foreach Key $Keydata {
          puts "      <Key>"
          puts "        <Name>[lindex $Key 0]</Name>"
          puts "        <IsPrimary>[lindex $Key 1]</IsPrimary>"
          puts "        <IsUnique>[lindex $Key 2]</IsUnique>"
          puts "        <Definition>[lindex $Key 3]</Definition>"
          puts "      </Key>"
      }
      puts "    </Keys>"
      pg_result $res -clear
}

proc getRelations {Connection The_Table} {
        puts "    <Relations>"
        puts "    </Relations>"
}


proc ExtractDefinition { db {host "localhost"} {port "5432"} {passwd ""} } {
    set conn [pg_connect $db -host $host -port $port -password $passwd]
    #Put all tables in list
    set res [pg_exec $conn \
    "SELECT  c.relname as Name \
     FROM pg_catalog.pg_class c \
       LEFT JOIN pg_catalog.pg_user u ON u.usesysid = c.relowner \
       LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace \
     WHERE c.relkind ='r' \
       AND n.nspname NOT IN ('pg_catalog', 'pg_toast') \
       AND pg_catalog.pg_table_is_visible(c.oid) \
     ORDER BY c.relname"]
    set ntups [pg_result $res -numTuples]
    set Tables {}
    for {set i 0} {$i < $ntups} {incr i} {
        lappend Tables [pg_result $res -getTuple $i]
    }
    pg_result $res -clear

    puts "<Tables>"
    foreach Table $Tables {
        puts "  <Table>"
        puts "    <Name>$Table</Name>"
    #For each table put keys
        getKeys $conn $Table
    #For each table put relations (implemented as dummy)
#        getRelations $conn $Table
    #For each table put fields
        getFields $conn $Table
        puts "  </Table>"
    }
    puts "</Tables>"

    pg_disconnect $conn
    return ""
}

#Main
#ExtractDefinition [lindex $argv 0]
#ExtractDefinition "bnl" "192.168.1.7" 5432 "ld4BC9Q51FU9CYjC21gp"
ExtractDefinition "bnl" "192.168.1.20" 5432 "bnl"
 