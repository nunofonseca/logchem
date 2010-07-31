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

package provide lcp 1.0

package require Tcl 8.5

#package require persistent

namespace eval lcp {

    variable debug 0
    variable num_projects
    variable err
    array set projs {}
    
    # varname type  default value validation_proc
    array set projs_vtype {
	name          { t "" - }
	dir           { f "" - }
	notes         { t "" - }
	sdf_orig      { f ""  - }
	sdf_proj      { f "" - }
	sdf_props     { l {} - }
	sdf_class_prop   { t "" - }
	sdf_class_type   { t "" - }
	tanimoto_coef    { d 0  valid_double }
	md               { l {} - }
	md_file          { f {} - } 
	valid_project    { b 0  - } 
	n_sdf_ops        { d 0  - }
	nmols            { d 0  - }
	hist             { l {} - }
	ilp_dir          { d {} - }
	ilp_b_file       { f "" - }
	ilp_f_groups     { l {} - }
    }        
    
    namespace export lcp_save
    namespace export lcp_close
    namespace export lcp_init
    namespace export lcp_new
    namespace export lcp_set_value 
    namespace export lcp_get_value 
    namespace export lcp_get_default_value 
    namespace export lcp_proj_opened
    namespace export lcp_md_selected
    namespace export lcp_new_sdf_op
    namespace export lcp_list_sdf_ops
    namespace export lcp_get_error
    namespace export lcp_new_analysis
    namespace export lcp_sdf_list_props
    #-------------------------------------------------------
    proc lcp_new_analysis { type  fgroups lc_dir} {
	#upvar $fgroups_a fgroups
	set sdf_file  [lcp::lcp_get_value sdf_proj]
	set proj_name [lcp::lcp_get_value name]
	set anal_dir "[file dirname $sdf_file]/$type"
	##############################
	# create a folder if necessary
	if { ![file exists $anal_dir] } {
	    catch { exec mkdir $anal_dir }
	} else {
	    if { ![file isdirectory $anal_dir] } {
		lcp_set_error "Unable to create directory $anal_dir"
		return 0
	    }
	}
	##############################
	# copy the func_groups.pl file
	catch { exec cp "$lc_dir/lib/pl/func_groups.pl" "$anal_dir"}

	# 
	catch { exec cp "$lc_dir/lib/pl/bk_template.b"  "[lcp_get_value ilp_b_file]"}	
	catch { exec cp "$lc_dir/lib/pl/logchem.pl"     "$anal_dir/"}
	# TODO: remove this file
	catch { exec cp "$lc_dir/lib/pl/func.yap"     "$anal_dir/"}
	# enable the selected func groups...
	# include func groups	
	set s [join $fgroups "','"]
	lcp::lcp_set_value ilp_f_groups $fgroups
	set preds2enable ":-enable_props(\['$s'\])."
	set fd [open "$anal_dir/mode_decls.pl" w]
	if { $fd < 0 } {
	    lcp_set_error "Unable to create file $anal_dir/mode_decls.pl"
	    return 0
	}
	puts $fd  "$preds2enable"
	close $fd
	# Empty files
	# user_constraints - empty (create a procedure)
	foreach f "user_constraints.pl user_settings.pl" {
	    set f "$anal_dir/$f"
	    set fd [open $f w]
	    if { $fd < 0 } {
		lcp_set_error "Unable to create file $f"
		return 0
	    }
	    close $fd
	}
	# convert the sdf file to prolog (it should be done only once...but for now we always convert the data)
	if { [sdf2prolog $sdf_file "$anal_dir/sdf.pl"] } {
	    lcp_set_error "Error converting sdf file to prolog."
	    return 0
	}
	# generate the files with the examples
	if { [gen_examples "$anal_dir/sdf.pl" [file rootname [lcp_get_value ilp_b_file]]] } {
	    lcp_set_error "Error generating the files with the examples."
	    return 0
	}	
	return 1
    }
    proc lcp_set_error { error_msg } {
	variable err
	set err $error_msg
    }
    proc lcp_get_error {} {
	variable err
	return $err
    }
    proc lcp_init {} {
	variable num_projects 0
	variable projs
	array set projs {}
	set projs(valid_project) 0
    }
    
    proc lcp_get_default_value { var } {
	variable projs_vtype
	if { [array names projs_vtype "$var"]=="" } {
	    return "undef"
	}
	return [lindex $projs_vtype($var) 1]
    }
    #
    proc lcp_set_value { var value } {
	variable projs_vtype
	variable projs
	if { [array names projs_vtype -exact "$var"] == {} } {
	    puts stderr "invalid value $value for $var"
	    return -1
	}
	# TODO: validate the value
	set projs($var) $value
	#puts "lcp_set_value $var $value"
	if { $var == "sdf_file" } {
	    # update the list of properties
	    #
	    return [lcp_set_value sdf_props [lcp_sdf_list_props $value]]
	}
	return 1
    }
    proc lcp_get_value { var } {
	variable projs_vtype
	variable projs
	if { [array names projs_vtype -exact "$var"] == {} } {
	    debug-info "lcp_get_value::invalid value for $var"
	    return -1
	}
	# TODO: validate the value
	parray projs
	return  $projs($var)
    }
    proc lcp_md_selected { val } {
	set l [lcp_get_value md]
	if { [lsearch -exact $l  $val] >=0 } { return 1} 
	return 0
    }
    #
    proc lcp_close {} {
	variable projs_vtype
	variable projs
	# reset the variables
	foreach v [array names projs_vtype] {
	    set projs($v) [lcp_get_default_value $v]
	}		
    }
    # types:
    # t=text
    # f=filename
    # i=integer
    # l=list
    proc lcp_new { name directory sdf_orig } {
	variable projs_vtype
	variable projs
	foreach v [array names projs_vtype] {
	    lcp_set_value $v [lcp_get_default_value $v]
	}		
	lcp_set_value name $name
	lcp_set_value dir  "$directory/$name"	
	if { ![file exists "$directory/$name"]} {
	    file mkdir "$directory/$name"	    
	}
	# 
	cd $directory/$name
	#
	lcp_set_value ilp_dir  "ilp"
	lcp_set_value ilp_b_file  "ilp/$name.b"
	lcp_set_value sdf_orig  $sdf_orig
	lcp_set_value valid_project  1
	set filename [file tail $projs(sdf_orig)]
	lcp_set_value sdf_proj  "$filename"
	#puts "lcp_new:$name-$directory-$sdf_orig"
	if { [lcp_save] <= 0 } {
	    return 0
	}
	# copy the original sdf file
	eval exec cp "$sdf_orig" "$projs(sdf_proj)"
	lcp_new_sdf_op  "Original file"
	# Add info
	#exec cxcalc -o "$projs(sdf_proj)" lowestenergyconformer "$sdf_orig" -f sdf	
	# run_babel -isdf $sdf_orig -osdf $projs(sdf_proj) -p
	lcp_save
	return 1
    }
    #
    proc lcp_proj_opened {} {
	variable projs
	return $projs(valid_project)
    }
    #
    proc lcp_save {} {
	variable projs
	# check if dir exits
	if { ![lcp_proj_opened] } {
	    return -1
	}
	set pdir "$projs(dir)"
	if { ![file exists "$pdir"]} {
	    file mkdir "$pdir"	    
	}
	if { ![file isdirectory $pdir] } {
	    return -1
	}
	# file exists
	set fd [open "$pdir/logchem.lc_proj" w+]
	if { $fd < 0 } {
	    return -1
	}
	foreach v [array names projs] {
	    puts $fd "$v=$projs($v)"
	}
	close $fd
	return 1
    }
    proc lcp_load { file } {
	variable projs
	variable projs_vtype
	foreach v [array names projs_vtype] {
	    #puts $v
	    lcp_set_value $v [lcp_get_default_value $v]
	}		
	set fd [open "$file" r]
	if { $fd < 0 } {
	    return -1
	}
	while { ![eof $fd] } {
	    set line [string trim [gets $fd]]
	    if { [string length $line] > 0 } {
		set l [split $line "="]
		set v [lindex $l 0]
		set value [join [lreplace $l 0 0]]
		if { [lcp_set_value $v $value]<0 } {
		    debug-info "lcp_load error: $v=$value"
		    return -1
		}
	    }
	}
	close $fd
	# change to project directory
	set dir [file dirname $file]
	cd $dir
	lcp_set_value dir $dir
	return 1
    }
    
    proc valid_double { val } {
	return true
    }
    
    proc lcp_new_sdf_op { fun_descr } {
	variable projs
	
	catch { 
	    eval exec cp "$projs(sdf_proj)"	 "$projs(sdf_proj).$projs(n_sdf_ops)" 
	    eval exec rm -f "$projs(sdf_proj).$projs(n_sdf_ops).gz" 
	    eval exec gzip "$projs(sdf_proj).$projs(n_sdf_ops)"
	    #eval exec rm -f "$projs(sdf_proj).$projs(n_sdf_ops)" 
	    set _ ""
	} err
	if { $err != "" } {
	    debug-info "lcp_new_sdf_op error:$err"
	}	    
	set projs(hist) [lappend projs(hist) $projs(n_sdf_ops) [list "$fun_descr" "$projs(sdf_proj).$projs(n_sdf_ops)"]]
	incr projs(n_sdf_ops)
	# update the list of properties
	lcp_set_value sdf_props [lcp_sdf_list_props $projs(sdf_proj)]
    }

    proc lcp_list_sdf_ops { } {
	variable projs
	return $projs(hist)
    }

    #---------------Converts a SDF file into a Prolog predicate file------------#
    proc sdf2prolog { sdf_file pl_file} {
	    
	# calls sdf2plstruct (prolog script)
	set cmd "lc_sdf2plstruct $sdf_file $pl_file"
	debug-info "$cmd"
	set fd [ext_prog::run_prog_sb $cmd .sdf2prolog "Converting SDF file" 0 .]
	if { $fd <=0 } {
	    return 1
	}
	ext_prog::wait2complete $fd .sdf2prolog
	debug-info "sdf2plstruct $sdf_file $pl_file OK!!!"
	return 0
    }
    
    # sdf_all_props
    proc lcp_sdf_list_props {sdf_file} {
	set props ""
	catch {
	    set props [eval exec lc_sdf_list_props $sdf_file]
	    set _ ""
	} err
	return $props
    }    
    
    proc gen_examples { pl_file file_prefix } {
	# generate the files with the examples
	# how to define the class?
	# IMPROVE THIS!!!!
	# 
	#puts stderr "gen_examples $pl_file $file_prefix"

	set prefix $file_prefix
	set posfile "$prefix.f"
	set negfile "$prefix.n"
	
	set suffix [string toupper $prefix]
	set hndl [open $pl_file r]
	set act  [open "$posfile" w+]
	set inac [open "$negfile" w+]   
	set prev ""
	while {![eof $hndl]} {
	    set sdline [gets $hndl ]
	    # change to regexp
	    set type ""
	    set id ""
	    set infor ""
	    puts -nonewline stderr  "."
	    flush stderr
	    if { [regexp -nocase "activityoutcome_(\[^\(_\]+)(\[^\(\]*).(\[^),\]+),(\[^),\]+).\.$" $sdline  _ ds type id infor]  } {
		#debug-info "$ds:$type:$id:$infor" 
		if { $prev!=$id } {
		    if {$infor == "active"} {
			puts -nonewline $act "active($id).\n"		    
		    } elseif {$infor == "inactive"} {
			puts -nonewline $inac "active($id).\n"
		    }
		}
		set prev $id
	    }
	}
	puts stderr "Complete\n"
	flush stderr
	close $hndl
	close $act
	close $inac
	return 0
    }
    
    proc debug-info {message} {
	variable debug	
	if {$debug} {
	    puts "debug: $message"
	}
    }
}


