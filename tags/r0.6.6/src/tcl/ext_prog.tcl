##################################################################################
#
# Copyright (c) 2009, 2010 Nuno A. Fonseca (nunofonseca at acm.org)
#
#    This file is part of LogCHEM.
#
#    LogCHEM is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    LogCHEM is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with LogCHEM; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
##################################################################################

package provide ext_prog 1.0

package require Tcl 8.5
package require Tk 8.5

namespace eval ext_prog {
    
    variable debug       1
    variable show_status 0
    array set availability {}
    # external programs
    # (cmdname) "package"  "invoquecmd" "mandatory(yes=1/No=0)"
    array set external_progs {
	"generatemd" { JChem  "generatemd" 0}
	"gzip"       { gzip   "gzip" 1}
	"gunzip"     { gunzip "gunzip" 1}
	"babel"      { openbabel "babel" 1}
	"yap"        { "Yap Prolog"  "yap" 1}
	"perl"       { "Perl"  "perl" 1}
    }

    namespace export run_prog
    namespace export run_prog_sb
    namespace export run_prog_bg
    namespace export run_babel
    namespace export run_generatemd
    namespace export check_ext_deps
    namespace export prog_available
    namespace export wait2complete
    ##########################################################
    
    proc prog_available { name } {
	variable availability
	set s [lsearch -exact "$name" [array names availability]]
	if { $s < 0 || !$availability($s) } {
	    return 0
	}
	return 1
    }
    #
    proc debug-info {message} {
	variable debug	
	if {$debug} {
	    puts "debug: $message"
	}
    }
    #
    # Execute cmd and show a status bar during the execution
    # 
    proc run_prog_sb { cmd  {win ""} {cmd_desc "" } {show_cmd 0} {parent_win ""}} {
	debug-info "$cmd"
	if { [catch {set fd [open "|$cmd " ]} output] } {
	    debug-info "$output"
	    return 0
	}
	# pid of the first program in the pipeline
	set pid [lindex [pid $fd] 0]
	fconfigure $fd -blocking 0 -buffering line
	debug-info "PID:$pid"
	#debug-info "$output"
	if { $win != "" } {
	    if { ![winfo exists $win]} {
		toplevel $win
		wm title $win "Running $cmd_desc"		
		wm protocol $win WM_DELETE_WINDOW [list [namespace current]::stop $win $pid]
		wm withdraw $win

		pack [ label $win.l -text $cmd_desc ] -side top
		if { $show_cmd } {
		    pack [ label $win.cmd -text "$cmd" ] -side top
		}
		pack [ ttk::progressbar $win.sb -mode indeterminate] -side top
		pack [ button $win.b -text "Stop" -command [list [namespace current]::stop $win $pid]] -side top -anchor c
	    }
	    $win.sb start
#	    update
	    wm deiconify $win
	    if { $parent_win!="" } {
		center_window $parent_win $win 
	    }
	    raise $win
	    grab $win
	    #tkwait window $win
 	    #wm withdraw $win
	}
	return $fd
    }
    proc stop { win pid } {
	set r1 ""
	set r2 ""
	set r3 ""
	if { $win!="" && $win!="." } {
	    catch { $win.sb stop } r1
	    catch { destroy $win} r2
	}
	catch { exec kill -9 $pid } r3
	debug-info "stop: $r1\n$r2\n$r3"
    }
    # wait for a command to complete
    proc wait2complete { fd win } {	
	fileevent $fd readable "[namespace current]::is_cmd_completed $fd $win  [lindex [pid $fd] 0]"	
	tkwait window $win
    }
    proc is_cmd_completed { fd win pid } {
	if { [eof $fd] } {
	    fileevent $fd readable ""
	    stop $win $pid
	    return
	}	
	set s [gets $fd]
	debug-info "$s"
	return 0
    }
    # Run cmd in background and return the PID
    proc run_prog_bg { cmd } {
	set r [run_prog "$cmd" "&"]
	set pid [lindex $r 1]
	return $pid
    }
    
    #
    proc run_prog { cmd {bg ""}} {
	set err ""
	set out ""
	set status 0
	debug-info "$cmd "
	if {[catch {exec -ignorestderr bash -c "$cmd" $bg } results options]} {
	    set details [dict get $options -errorcode]
	    if {[lindex $details 0] eq "CHILDSTATUS"} {
		set status [lindex $details 2]
	    } else {
		# Some kind of unexpected failure
		debug-info "Error: $cmd\n$options\n$results"
		set status 10
	    }
	    return [list $status $results]
	}
	return [list 0 $results]
    }
    #
    proc run_babel { args  { last_lines2discard 0} }  {
	variable external_progs
	set cmd [lindex $external_progs(babel) 1]
	set err ""
	set out ""
	set status 0
	debug-info "$cmd $args"
	return [run_prog "$cmd $args 2> /dev/null | head -n -$last_lines2discard"] 
    }

    #
    proc babel { src_file target_file {args "" }} {
	variable external_progs
	set cmd [lindex $external_progs(babel) 1]
	set err ""
	catch {
	    exec rm -f $target_file
	    exec $cmd $args $src_file $target_file 
	    set _ ""
	} err	
	if { $err == "" || ![file exists $target_file] } {
	    debug-info "babel $args $src_file $target_file failed: $err"
	    return 0
	}
	return 0
    }

    proc run_generatemd { args { show_status 0} {pwin "."}} {
	variable external_progs
	set mdcmd [lindex $external_progs(generatemd) 1]
	set cmd   "$mdcmd $args"
 	debug-info "$cmd"
        if { $show_status} {
	    set res [run_prog_sb "$cmd"  .mdsb "Generating molecular descritors..." 0 $pwin]	    
	    wait2complete $res .mdsb
	    debug-info "genMD completed................................"
	    return 0
        } else {
	    set res [run_prog $cmd]
	    return [lindex $res 0]
	}
    }

    ###########################################################################
    # Check if the external programs required by logchem are available
    proc check_ext_deps {} {
	variable external_progs
	variable availability

	array set availability {}
	debug-info "\[INFO\] Checking for external programs availability"
	foreach p [array names external_progs] {
	    set pack    [lindex $external_progs($p) 0]
	    set cmd     [lindex $external_progs($p) 1]
	    set mandatory     [lindex $external_progs($p) 2]
	    debug-info "\[CHECK\] Searching for $cmd..."
	    set r ""
	    catch {
		set r  [exec which $cmd]
		set _ ""
	    } err
	    if { $r == "" } {
		if { $mandatory } {
		    debug-info "\[ERROR\] program $cmd not found"
		    exit 1
		}
		debug-info "\[Warning\] program $cmd not found"
		set availability($p) 0
	    } else {
		set availability($p) 1
	    }
	    debug-info "\[OK\] found $cmd at $r"
	}
    }
    #
    # procedure to center the window win in the parent window parent
    proc center_window { parent win } {
	update
	
	set wWidth [winfo reqwidth $win]
	set wHeight [winfo reqheight $win]
	
	set pWidth [winfo reqwidth $parent]
	set pHeight [winfo reqheight $parent]
	set pX [winfo rootx $parent]
	set pY [winfo rooty $parent]
	
	set centerX [expr {$pX +($pWidth / 2)}]
	set centerY [expr {$pY +($pHeight / 2)}]
	
	set x [expr {$centerX -($wWidth / 2)}]
	set y [expr {$centerY -($wHeight / 2)}]
	
	wm geometry $win "=+${x}+${y}"
	update
    } 
}


