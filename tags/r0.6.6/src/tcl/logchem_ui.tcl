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

package provide logchem_ui 1.0

package require Tk 8.5

package require lcp
package require ext_prog

namespace eval logchem_ui {
    array set g {
	LogCHEM_DIR     ""
	appname         "iLogCHEM"
        version   0.6.6
	debug           1
	proj_loaded     0
	exitValue       0
	statusProject   ""
	statusInfo      ""
	tanimoto_coef   0.6

	task_name       ""
	task_status     ""

	aleph_nodes     2000
	aleph_precision 0.6
	aleph_noise     0.15
	aleph_minpos    15
	aleph_clauselength 8
	aleph_search    bf
       
	started         0
	initOK          0
	learning        1
	mode            0
    }

    set aleph_vars "aleph_nodes aleph_precision aleph_noise aleph_minpos aleph_clauselength aleph_search"
    # "availability=y|n|?"
    # molecular descriptors
    array set descriptors {
	CF "Chemical Fingerprint"
	PF "Pharmacophore Fingerprint"
	RF "Reaction Fingerprint"
	BCUT "BCUT descriptors"
	"HDon/HAcc" "Hydrogen bond Donor/Acceptor count"
	Logd "octanol-water distribution coefficient"
	LogP "octanol-water partition coefficient"
	TPSA "Topological Polar Surface Area"
	Mass "Mass of molecule"
	Heavy "Number of Heavy atoms"
    }
    array set func_groups {
	Methyl "(CH3)"
	Amino "(NH2)" 
	Aldehyde "(O=CH-)"
	"Carboxylic acid" "(R-COOH)"
	Hydroxyl "(OH)"
	Cyano "(C≡N)" 
	Ketone "(C=O)"
	"Benzene ring" "(C6H6)"
    }
    # mode is deprecatedrs
    # mode: 0-none, 1-logchem file only, 2-sdf only, 3-mining (implies 2), 4-mining and logchem file
    array set opts {}
    array set tan_matrix {}
    
    namespace export start_logchem_ui

    proc init_win_setup {} {
	variable opts
	variable g
	variable screenWidth
	variable screenHeight
	variable w
	variable colorchg
	
	# get this out of the way -- we want to draw the whole user interface
	# behind the scenes, then pop up in all of its well-laid-out glory
	set screenWidth  [winfo vrootwidth .]
	set screenHeight [winfo vrootheight .]
	wm withdraw .
	
	# FIXME - move to preferences
	option add "*TearOff" false 100
	option add "*BorderWidth" 1 100
	option add "*ToolTip.background" LightGoldenrod1
	option add "*ToolTip.foreground" black
	
	# determine the windowing platform, since there are different ways to
	# do this for different versions of tcl
	if {[catch {tk windowingsystem} g(windowingSystem)]} {
	    if {"$::tcl_platform(platform)" == "unix"} {
		set g(windowingSystem) "x11"
	    } elseif {"$::tcl_platform(platform)" == "macintosh"} {
		set g(windowingSystem) "classic"
	    } else {
		# this should never happen, but just to be sure...
		set g(windowingSystem) "x11"
	    }
	}
	
	# Try to find a pleasing native look for each platform.
	# Fonts.
	set sysfont [font actual system]
	#debug-info "system font: $sysfont"
	
	# See what the native menu font is
	. configure -menu .native
	menu .native
	set menufont [lindex [.native configure -font] 3]
	destroy .native
	
	# Find out what the tk default is
	label .testlbl -text "LABEL"
	set labelfont [lindex [.testlbl configure -font] 3]
	destroy .testlbl
    
	text .testtext
	set textfont [lindex [.testtext configure -font] 3]
	destroy .testtext
	
	entry .testent
	set w(selcolor) [lindex [.testent configure -selectbackground] 4]
	set entryfont [lindex [.testent configure -font] 3]
	destroy .testent
	# the above results in a nearly undistinguishable darker gray for the
	# selected color (rh8 with tk 8.3.3-74) "#c3c3c3"
	set w(selcolor) "#b03060"
	
	set font [list $textfont]
	set bold [list [concat $textfont bold]]
	set italic [list [concat $textfont italic]]
	
	option add *Label.font $labelfont userDefault
	option add *Button.font $labelfont userDefault
	option add *Menu.font $menufont userDefault
	option add *Entry.font $entryfont userDefault
	
	# This makes tk_messageBox use our font.  The default tends to be terrible
	# no matter what platform
	option add *Dialog.msg.font $labelfont userDefault
	

	if {[string first "color" [winfo visual .]] >= 0} {
	    #nf
	    set g(bold)   "-background #43ce80 -relief raised -borderwidth 1"
	    set g(normal) "-background {} -relief flat"
	    
	    array set opts [subst {
		matchantag "-background gray"
		matchtag   "-background $colorchg"
		inlinetag  "-background $colorchg -font $bold"
		textopt    "-background white -foreground black -font $font"
		currtag    "-background Khaki"
		
		patlinetag ""
		pattag     ""
	    }]
	    
	} else {
	    
	    set g(bold) "-foreground white -background black"
	    set g(normal) "-foreground {} -background {}"
	    # Assume only black and white
	    set bg "black"
	    array set opts [subst {
		matchtanag    "-background white -foreground black -font $bold"
		matchtag    "-background white -foreground black -font $bold"
		inlinetag  "-background $colorchg -font $bold"
		textopt    "-background white -foreground black -font $font"	    
		currtag    "-background white -foreground black -font $font"	    
	    }]
	}
	
	# make sure wrapping is turned off. 
	set opts(textopt) "$opts(textopt) -wrap none"
	
    }

    proc init_arrays {} {
	variable g
	variable opts  
	variable uniq 
	variable data
	variable colorins 
	variable colorchg

	set colordel Tomato
	set colorins PaleGreen
	set colorchg DodgerBlue


	#deprecated
	array set data {
	    prefix          ""
	    logchem_file    ""
	    sdf_file        ""
	    discr_file      ""
	    ilp_b_file      ""
	    cur_dir         "."
	    ilp_b_file_tmpl ""
	    workingFile     ""
	    moleculeSelect  3
	    mols            ""
	    exp_name        ""
	    classes         ""
	    nclasses        ""
	    rules           ""
	    nrules          ""
	    cur_rule        ""
	}
	set  data(ilp_b_file_tmpl) "$g(LogCHEM_DIR)/pl/logchem.b"
	# mode: 0-none, 1-logchem file only, 2-sdf only, 3-mining (implies 2), 4-mining and logchem file
		
	array set opts {
	    geometry          "80x30"
	    vmd_cmd           "vmd"
	}
	set uniq 0	
    }
    

    
    ###########################################################
    proc get_cde_params {} {
	variable w

	# Set defaults for all the necessary things
	set bg [option get . background background]
	set fg [option get . foreground foreground]
	set guifont [option get . buttonFontList buttonFontList]
	set txtfont [option get . FontSet FontSet]
	set listfont [option get . textFontList textFontList]
	set textbg $bg
	set textfg $fg
	
	# If any of these aren't set, I don't think we're in CDE after all
	if {![string length $fg]} {
	    return 0
	}
	if {![string length $bg]} {
	    return 0
	}
	if {![string length $guifont]} {
	    return 0
	}
	if {![string length $txtfont]} {
	    return 0
	}
	
	set guifont [string trimright $guifont ":"]
	set txtfont [string trimright $txtfont ":"]
	set listfont [string trimright $txtfont ":"]
	regsub {medium} $txtfont "bold" dlgfont
	
	# They don't tell us the slightly darker color they use for the
	# scrollbar backgrounds and graphics backgrounds, so we'll make
	# one up.
	set rgb_bg [winfo rgb . $bg]
	set shadow [format #%02x%02x%02x [expr {(9*[lindex $rgb_bg 0]) /2560}] \
			[expr {(9*[lindex $rgb_bg 1]) /2560}] [expr {(9*[lindex $rgb_bg 2]) \
									 /2560}]]
	
	# If we can find the user's dt.resources file, we can find out the
	# palette and background/foreground colors
	set fh ""
	set palette ""
	set cur_rsrc ~/.dt/sessions/current/dt.resources
	set hom_rsrc ~/.dt/sessions/home/dt.resources
	if {[file readable $cur_rsrc] && [file readable $hom_rsrc]} {
        if {[file mtime $cur_rsrc] > [file mtime $hom_rsrc]} {
            if {[catch {open $cur_rsrc r} fh]} {
                set fh ""
            }
        } else {
            if {[catch {open $hom_rsrc r} fh]} {
                set fh ""
            }
        }
	} elseif {[file readable $cur_rsrc]} {
	    if {[catch {open $cur_rsrc r} fh]} {
		set fh ""
	    }
	} elseif {[file readable $hom_rsrc]} {
	    if {[catch {open $hom_rsrc r} fh]} {
		set fh ""
	    }
	}
	if {[string length $fh]} {
	    set palf ""
	    while {[gets $fh ln] != -1} {
		regexp "^\\*background:\[ \t]*(.*)\$" $ln nil textbg
		regexp "^\\*foreground:\[ \t]*(.*)\$" $ln nil textbg
		regexp "^\\*0\\*ColorPalette:\[ \t]*(.*)\$" $ln nil palette
		regexp "^Window.Color.Background:\[ \t]*(.*)\$" $ln nil textbg
		regexp "^Window.Color.Foreground:\[ \t]*(.*)\$" $ln nil textfg
	    }
	    catch {close $fh}
	    #
	    # If the *0*ColorPalette setting was found above, try to find the
	    # indicated file in ~/.dt, $DTHOME, or /usr/dt.
	    #
	    if {[string length $palette]} {
		foreach dtdir {/usr/dt /etc/dt ~/.dt} {
		    # This uses the last palette that we find
		    if {[file readable [file join $dtdir palettes $palette]]} {
			set palf [file join $dtdir palettes $palette]
		    }
		}
		# debug-info "Using palette $palf"
		if {[string length $palf]} {
		    if {![catch {open $palf r} fh]} {
			gets $fh activetitle
			gets $fh inactivetitle
			gets $fh wkspc1
			gets $fh textbg
			gets $fh guibg ;#(*.background) - default for tk under cde
			gets $fh menubg
			gets $fh wkspc4
			gets $fh iconbg ;#control panel bg too
			close $fh
			
			option add *Entry.highlightColor $activetitle userDefault
			option add *selectColor $activetitle userDefault
			option add *Text.highlightColor $wkspc4 userDefault
			option add *Dialog.Background $menubg userDefault
			option add *Menu.Background $menubg userDefault
			option add *Menubutton.Background $menubg userDefault
			option add *Menu.activeBackground $menubg userDefault
			option add *Menubutton.activeBackground $menubg userDefault
			set w(selcolor) $activetitle
		    }
		}
	    }
	} else {
	    puts stderr "Neither ~/.dt/sessions/current/dt.resources nor"
	    puts stderr "        ~/.dt/sessions/home/dt.resources was readable"
	    puts stderr "   Falling back to plain X"
	    return 0
	}

	option add *Text.Background $textbg userDefault
	option add *Entry.Background $textbg userDefault
	option add *Text.Foreground $textfg userDefault
	option add *Entry.Foreground $textfg userDefault
	option add *Button.activeBackground $bg userDefault
	option add *Button.activeForeground $fg userDefault
	option add *Scrollbar.activeBackground $bg userDefault
	option add *Scrollbar.troughColor $shadow userDefault
	option add *Canvas.Background $shadow userDefault
	
	# These menu configs work if you use native menus.
	option add *Menu.borderWidth 1 userDefault
	option add *Menu.activeForeground $fg userDefault
	option add *Menubutton.activeForeground $fg userDefault
	
	# This draws a thin border around buttons
	#option add *highlightBackground $bg userDefault
	# Suppress the border
	option add *HighlightThickness 0 userDefault
	# Add it back for text and entry widgets
	option add *Text.highlightBackground $bg userDefault
	option add *Entry.highlightBackground $bg userDefault
	option add *Text.HighlightThickness 2 userDefault
	option add *Entry.HighlightThickness 1 userDefault
	
	return 1
    }



    proc  update_win_options {} {
	setup_learn_options
	#setup_sdf_options
    }
    
    #--------------------Browses SDF files (datasets)----------------------------------#
    proc commandline {} {
	debug-info "commandline"
	global data
	global argv
	global argc
	debug-info "  argv: $argv"
	global finfo
	global opts
	global g
	
	set g(initOK) 0
	set argindex 0
	set pths 0
	# Loop through argv
	while {$argindex < $argc} {
	    set arg [lindex $argv $argindex]
	    switch -regexp -- $arg {
		"^-h" -
		"^--help" {
		    do-usage cline
		    exit 0
		}
		"^-m$" {
		    incr argindex
		    set data(logchem_file) [lindex $argv $argindex]
            }
		default {
		    incr pths
		    set finfo(pth,$pths) $arg
		    set finfo(f,$pths) $arg
		}
	    }
	    incr argindex
	}
	return 0
    }

    
    ################################################################################
    # 				Project procedures			       #
    ################################################################################
    
    #-----------------Browses the .LogCHEM files---------------------#
    proc BrowseLog {title f {filetype { { {LogCHEM} {*.LogCHEM} } } } } {
	variable w
	
	set foo         $f
	set initialdir  [file dirname $foo]
	set initialfile [file tail $foo]
	set filename    [tk_getOpenFile -title $title -initialfile $initialfile \
			     -initialdir $initialdir -filetypes  $filetype]
	if {[string length $filename] > 0} {
	    return $filename
	} else {
	    after idle {raise .}
	    return {}
	}
    }
    ##################################################
    #
    # View, save and delete constraints
    #
    proc eConstraints {} {
	variable g
	variable w 
	variable t

	set t(list_constraints) {}
	set w(econs) .econs
	set tl $w(econs)
	if { ![winfo exists $tl]} {
	    toplevel $tl
	    wm group $tl
            if {[winfo viewable [winfo toplevel [winfo parent $tl]]] } {
               wm transient $tl [winfo toplevel [winfo parent $tl]]
            }
	    wm title $tl "Constraints"		
	    wm protocol $tl WM_DELETE_WINDOW [list [namespace current]::bcancel $tl]
	    wm withdraw $tl
	    set fp [frame $tl.p]
	    pack $fp -side top -fill x
	    label $fp.l1 -text "Constraints" 
	    pack $fp.l1 -side top
	    
	    listbox $fp.list -listvariable "[namespace current]::t(list_constraints)" -width 100 -selectmode single -yscrollcommand "$fp.vscr set"
	    scrollbar $fp.vscr -orient vertical -command "$fp.list yview"
	    pack $fp.list -side left
	    pack $fp.vscr -side left -expand 1 -fill y

	    button $fp.del     -text "Delete"  -command [list [namespace current]::del_constraint $fp.list list_constraints]
	    button $fp.save    -text "Save"    -command [list [namespace current]::save_constraints list_constraints ]
	    button $fp.bcancel -text "Cancel"  -command [list [namespace current]::bcancel $tl]
	    
	    pack $fp.del $fp.save $fp.bcancel -side top -fill x -anchor c
	} 
	load_constraints list_constraints
#	update
	wm deiconify $tl
	raise $tl
	grab $tl
	tkwait window $tl
	grab release $tl
	wm withdraw $tl
    }
    #
    # Save the set of constraints to a file
    #
    proc save_constraints { var } {
	variable t
	set f "[lcp::lcp_get_value ilp_dir]/user_constraints.pl"
	set fd [open $f w+]
	if { $fd <0 } {
	    do-error "Unable to open $f"
	    return 0
	}
	puts $fd "%This file is automatically generated."
	foreach c $t($var) {
	    puts $fd "$c"
	}
	close $fd
	do-info "Constraints saved."
    }
    #
    # Load the set of constraints to a file
    #
    proc load_constraints { var } {
	variable t
	set f "[lcp::lcp_get_value ilp_dir]/user_constraints.pl"
	set fd [open $f r]
	if { $fd <0 } {
	    do-error "Unable to open $f"
	    return 0
	}
	set t($var) {}
	while { ![eof $fd] } {
	    set l [gets $fd]
	    if { [regexp  "^:" $l] } {
		#puts $l
		lappend t($var) $l
	    }
	}
	close $fd
    }
    proc del_constraint { lb var } {
	variable t
	set c [$lb curselection]
	if { $c >= 0 } {
	    set t($var) [lreplace $t($var) $c $c]
	}
    }
    ################################################################################
    # 				ILP procedures			       #
    ################################################################################
    #---------Starts the mining procedure, looking for patterns in dataset----# 
    proc startMining {} {
	variable data
	set lc_file     [lcp::lcp_get_value logchem_file]
	set bkf         [file tail [lcp::lcp_get_value ilp_b_file]]
	set file_prefix [regsub ".b$" $bkf ""]
	set dir         [lcp::lcp_get_value ilp_dir]
	set rules_file  "$dir/$file_prefix.aleph"
	set data(rules_file) $rules_file

	if { ![file exists $rules_file] } {
	    do-error "$rules_file not found"
	    return
	}
	# if logchem file does not exist or is older than .aleph file
	# file mtime 
	# set mtime1 [file mtime $lc_file]
	# set mtime2 [file mtime $rules_file]
	set expname  "$dir/vmd"
	set sdf_file "$dir/sdf.pl"
	if { ![doMatch $expname $sdf_file $rules_file] } {
	    return
	}
	#   ** Info: converting rules **
	set data(logchem_file) "$expname/Mols.LogCHEM"
	
	set nrules [load_logChem $data(logchem_file)]

	if { $nrules > 0 } {
	    # draw display with listboxes for each class
	    #draw_display $w(client)
	    #optMolecules "$data(logchem_file)"
	    #create the handler to update the window when the file with the rules changes
	    #watch_file $data(rules_file) "[namespace current]::startMining"
	    visualize_results "$data(logchem_file)"
	}
	return
    }
	
    proc watch_file { filename code } {
	debug-info "start spy...1"
	set mtime [file mtime $filename]
	after idle [spy_file $filename $mtime $code]
    }
    proc spy_file { filename mtime code } {
	debug-info "spying..."
	set mtime2 [file mtime $filename]
	if { $mtime2!=$mtime } {
	    eval $code
	}
	after 1000000 [spy_file $filename $mtime $code]
    }
    proc stop_watch_file { id } {
	after cancel $id
    }
    #----------------------Exits mining procedure----------------------#   
    proc exitMining {} {	
	destroy .wmg
	return
    }
    #---------------------Browses theory files-------------------------#
    proc Browse {title f {filetype { { {Theory File} {*.yap} } } } } {
	variable w
	
	set foo         [$f get]
	set initialdir  [file dirname $foo]
	set initialfile [file tail $foo]
	set filename    [tk_getOpenFile -title $title -initialfile $initialfile \
			     -initialdir $initialdir -filetypes  $filetype]
	if {[string length $filename] > 0} {
	    $f delete 0 end
	    $f insert 0 $filename
	    $f selection range 0 end
	    $f xview end
	    focus $f
	    return $filename
	} else {
	    after idle {raise .}
	    return {}
	}
    }
    #----------------ILP Datasets---------------#
    proc learnDataset_gui {} {
	variable w 
	variable g 
	variable func_groups
	#nodes minPrec maxNoise minPos clauseLen theoryFile 
	
	set waitvar {}
	set w(dt) .dt 
	if {![winfo exists $w(dt)]} {
	    toplevel $w(dt)
	    wm group $w(dt) .
            if {[winfo viewable [winfo toplevel [winfo parent $w(dt)]]] } {
               wm transient $w(dt) [winfo toplevel [winfo parent $w(dt)]]
            }

	    wm title $w(dt) "Generate Dataset"
	    
	    if {$g(windowingSystem) == "aqua"} {
		setAquaDialogStyle $w(dt)
	    }
	    
	    wm protocol $w(dt) WM_DELETE_WINDOW [list [namespace current]::bcancel $w(dt)]
	    wm withdraw $w(dt)
	    
	    set fdt [frame $w(dt).p1 -borderwidth 2 -relief groove]
	    pack $fdt -side top -fill x
	    #
	    set type [lcp::lcp_get_value sdf_class_type]

	    set fdt [frame $w(dt).p2 -borderwidth 2 -relief groove]
	    pack $fdt -side top -fill x
	    # Functional groups
	    label $w(dt).l1 -text "Functional Groups"
	    pack $w(dt).l1 -side top
	    set fdt [frame $w(dt).p3 -borderwidth 2 -relief groove]
	    pack $fdt -side top -fill x
	    
	    set iFG 0	    
	    foreach fg [array names func_groups] {
		# get the selected values from the project data
		# or use the defaults
		checkbutton $fdt.c$iFG -text "$fg$func_groups($fg)" -variable [namespace current]::t(fg$iFG) 
		incr iFG  
	    }
	    for {set x 0} {$x<$iFG} {incr x} {
		pack $fdt.c$x -anchor nw 
	    }

	    # Buttons
	    set fb [frame $w(dt).b -borderwidth 2]  
	    pack $fb -side top	    
	    button $fb.bsave   -text "Generate" -command [namespace current]::do_genILPDataset
	    button $fb.bcancel -text "Cancel"  -command [list [namespace current]::bcancel $w(dt)]
	    pack $fb.bsave $fb.bcancel -side left
	}  

	#update
	wm deiconify $w(dt)
	raise $w(dt)
	grab $w(dt)
	tkwait variable waitvar
	wm withdraw $w(dt) 
	grab release $w(dt)
    }
        
    #------------------Exits run aleph procedure-----------------------#
    proc aboutHelp {} {
	variable g
	set name .about
	catch { destroy $name } 
	toplevel $name
	if {[winfo viewable [winfo toplevel [winfo parent $name]]] } {
	    wm transient $name [winfo toplevel [winfo parent $name]]
	}
	grab $name
	wm resizable $name 0 0
	wm title $name   "About $g(appname)"
	pack [frame  .about.f1 ] -side top -fill x 
	#    pack [frame  .about.f -bg black] -side top -fill x 
	set header    "About $g(appname) $g(version)"
	set authors   "Nuno A. Fonseca, Vitor Santos Costa, Max Pereira, and Rui Camacho"
	#set url       "http://cracs.fc.up.pt/logchem"
	set license_v "GNU GENERAL PUBLIC LICENSE Version 3"
	set license   "LogCHEM is free software; you can redistribute it and/or modify 
it under the terms of the GNU General Public License as published 
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.\n
LogCHEM is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with Wiithon; if not, write to the Free Software Foundation, Inc.,
51 Franklin St, Fifth Floor, Boston, MA 02110-1301  USA"


        pack [label .about.f1.title -text "\n\n$header\n\n" -font titleFont ] -side top -fill x -expand 1
        pack [label .about.f1.authors -text "Authors: $authors\n" -justify left -font fixedFont] -side top -fill x -expand 1
        pack [label .about.f1.lic1 -text "License: $license_v\n\n$license" -justify left -font fixedFont] -side top

	pack [button .about.f1.b -text "Close" -command "destroy .about" ] -side top -anchor s

    }
    
    ##########################################################################
    proc start_logchem_ui { logchem_dir } {
	variable g
	init_arrays
	image create photo nullImage
	
	set g(LogCHEM_DIR) $logchem_dir
	set g(debug) t

	ext_prog::check_ext_deps
	# set a couple o' variables that we might need sooner than later
	lcp::lcp_init
	# Work-around for bad font approximations
	catch {tk scaling [expr {100.0 / 72}]}
	
	init_win_setup
	main
    }

    #-------------------------Shows error/info messages-------------------------------#
    proc do-error {msg {parent .}} {
	variable g	
	tk_messageBox -message "$msg" -title "$g(appname): Error" -icon error -type ok -parent $parent
    }
    
    proc do-info { msg {parent .}} {
	variable g	
	tk_messageBox -message "$msg" -title "$g(appname): Info" -icon info -type ok -parent $parent
    }

    proc debug-info {message} {
	variable g
	
	if {$g(debug)} {
	    puts "$message"
	}
    }
    #######################################################################
    #                              GUI

    #-----------------Centers the slave window----------------------------#
    proc centerWindow {w {size {}}} {
	#update
	set parent .
	
	if {[llength $size] > 0} {
	    set wWidth [lindex $size 0]
	    set wHeight [lindex $size 1]
	} else {
	    set wWidth [winfo reqwidth $w]
	    set wHeight [winfo reqheight $w]
	}
	
	set pWidth [winfo reqwidth $parent]
	set pHeight [winfo reqheight $parent]
	set pX [winfo rootx $parent]
	set pY [winfo rooty $parent]
	
	set centerX [expr {$pX +($pWidth / 2)}]
	set centerY [expr {$pY +($pHeight / 2)}]
	
	set x [expr {$centerX -($wWidth / 2)}]
	set y [expr {$centerY -($wHeight / 2)}]
	
	if {[llength $size] > 0} {
	    wm geometry $w "=${wWidth}x${wHeight}+${x}+${y}"
	} else {
	    wm geometry $w "=+${x}+${y}"
	}
	#update
    }

    proc create-display {} {
	global tk_version
	variable g 
	variable opts 
	variable bg 
	variable w
	variable tmpopts
	
	# these are the four major areas of the GUI:
	# menubar - the menubar (duh)
	# client  - the area with the the lb and other data
	# status         - a bottom status line
	
	# this block of destroys is only for stand-alone testing of
	# the GUI code, and can be blown away (or not, if we want to
	# be able to call this routine to recreate the display...)
	catch {
	    destroy .menubar
	    destroy .client
	    destroy .status
	}
	
	# create the top level frames and store them in a global
	# array..
	set w(client)  .client
	set w(menubar) .menubar
	set w(status)  .status
    
	# other random windows...
	set w(preferences) .pref
	set w(popupMenu)   .popupMenu
	
	# now, simply build all the pieces
	build-menubar
	build-client
	build-status
	
	frame .separator1 -height 2 -borderwidth 2 -relief groove
	frame .separator2 -height 2 -borderwidth 2 -relief groove
	
	# ... and fit it all together...
	. configure -menu $w(menubar)

	pack .separator1 -side top -fill x -expand n
	
	pack $w(client) -side top -fill both -expand y
	pack .separator2 -side top -fill x -expand n
	
	pack $w(status) -side bottom -fill x -expand n

	
	# normally, keyboard traversal using tab and shift-tab isn't
	# enabled for text widgets, since the default binding for these
	# keys is to actually insert the tab character. Because all of
	# our text widgets are for display only, let's redefine the
	# default binding so the global <Tab> and <Shift-Tab> bindings
	# are used.
	bind Text <Tab> {continue}
	bind Text <Shift-Tab> {continue}
	
	wm deiconify .
	#    focus -force $w(PosText)
	update idletasks
	# Need this to make the pane-resizing behave
	grid propagate $w(client) f
    }
    #
    # 
    #
    proc build-client {} {	
	variable w
	variable g

	#debug-info "build-client"
	
	frame $w(client) -bd 2 -relief flat
	pack $w(client)
	
	image create photo img
	set img_file "$g(LogCHEM_DIR)/images/welcome.gif"
	if { [file exists  $img_file] } {
	    img configure -file  $img_file
	    label $w(client).img -image img
	    pack $w(client).img -padx 2 -pady 2
	}
	set w(BottomText)    $w(client).bottomtext
    }    
    #-------------------Build the Menu bar-----------------------------#
    proc build-menubar {} {
	# debug-info "build-menubar"
	variable g
	variable opts
	variable w
	
	menu $w(menubar)
	
	# this is just temporary shorthand ...
	set menubar $w(menubar)
	
	# First, the menu buttons...
	set projectMenu $w(menubar).project 
	set sdfMenu $w(menubar).sdf
	set learnMenu $w(menubar).learn
	set helpMenu $w(menubar).help
	set quitMenu $w(menubar).quit
	set bkMenu $w(menubar).bk

	set w(projectMenu) $projectMenu
	set w(sdfMenu) $sdfMenu
	set w(learnMenu) $learnMenu
	set w(helpMenu) $helpMenu
	set w(quitMenu) $quitMenu
	
	$w(menubar) add cascade -label "Project" -menu $projectMenu -underline 0  
	$w(menubar) add cascade -label "SDF" -menu $sdfMenu -underline 0 -state disabled
	$w(menubar) add cascade -label "Analysis"          -menu $learnMenu -underline 0 -state disabled
	$w(menubar) add cascade -label "Help" -menu $helpMenu -underline 0 
	
	# File menu...
	menu $projectMenu
	$projectMenu add command -label "New        "   -underline 0 -command [namespace current]::newProject
	$projectMenu add command -label "Open"  -underline 0 -command [namespace current]::openProject
	$projectMenu add command -label "Save"  -underline 0 -command [namespace current]::saveProject -state disabled
	$projectMenu add command -label "Close" -underline 0 -command [namespace current]::closeProject -state disabled
	
	#$projectMenu add separator
	#$projectMenu add command -label "Preferences" -underline 0 -command [namespace current]::prefProject -state disabled
	$projectMenu add separator
	$projectMenu add command -label "Exit" -underline 0 -command [namespace current]::do-exit
    
	# Edit menu...  If you change, add or remove labels, be sure and
	# update the tooltips.
	menu $sdfMenu
        # 
	$sdfMenu add command -label "Generate Molecular Descriptors" -underline 0 -command [namespace current]::sdfGenMD_gui
	$sdfMenu add command -label "Filter Molecules (Tanimoto)" -underline 0   -command  [namespace current]::sdfTanFilter_gui
# select features
	$sdfMenu add separator
	$sdfMenu add command -label "Export Properties as a CSV File" -underline 0 -command "[namespace current]::exportsdf csv"
#	$sdfMenu add command -label "Export as ARFF (Weka)" -underline 0 -command "[namespace current]::exportsdf arff"
#	$sdfMenu add command -label "Export as dat (SVMLight)" -underline 0 -command [namespace current]::exportSVMlight -state disabled

	
	menu $learnMenu
	$learnMenu add command -label "Create Dataset" -underline 0 -command [namespace current]::learnDataset_gui
	$learnMenu add separator
	$learnMenu add command -label "View Background Knowledge" -underline 0 -command [namespace current]::viewBk  -state disabled
	$learnMenu add command -label "Settings" -underline 0      -command [namespace current]::searchRestrictions 
	$learnMenu add command -label "Constraints" -underline 0 -command [namespace current]::eConstraints  -state disabled
	$learnMenu add command -label "Run Aleph" -underline 0      -command [namespace current]::runAleph  -state disabled
	$learnMenu add separator
	$learnMenu add command -label "Visualize" -underline 0      -command [namespace current]::startMining -state disabled
	
	menu $helpMenu
	#$helpMenu add command -label "Help" -underline 0 -command helpHelp
	#$helpMenu add separator
	$helpMenu add command -label "About" -underline 0 -command [namespace current]::aboutHelp
	
    }
    proc build-status {} {
	variable w
	variable g
	
	frame $w(status) -bd 0
	
	set w(statusLabel) $w(status).label
	set w(statusProject) $w(status).current
	
	# MacOS has a resize handle in the bottom right which will sit
	# on top of whatever is placed there. So, we'll add a little bit
	# of whitespace there. It's harmless, so we'll do it on all of the
	# platforms.
	label $w(status).blank -image nullImage -width 16 -bd 1 -relief flat
	
	label $w(statusProject) -textvariable [namespace current]::g(statusProject) -anchor e \
	    -width 44 -borderwidth 1 -relief flat -padx 4 -pady 2
	label $w(statusLabel) -textvariable [namespace current]::g(statusInfo) -anchor w -width 1 \
	    -borderwidth 1 -relief sunken -pady 2
	pack $w(status).blank -side right -fill y
	
	pack $w(statusProject) -side right -fill y -expand n
	pack $w(statusLabel) -side left -fill both -expand y
    }
    #--------------------Main procedure---------------------#
    proc main {} {
	variable g 
	variable errorInfo
	variable startupError
	
	wm withdraw .
	wm protocol . WM_DELETE_WINDOW [namespace current]::do-exit
	wm title . "$g(appname) $g(version)"
	
	# teste

	set g(started) 1
	if {$g(windowingSystem) == "x11"} {
	    get_cde_params
	}
	if {$g(windowingSystem) == "aqua"} {
	    get_aqua_params
	} 
	create-display

        font create fixedFont  -family Courier   -size 10
        font create titleFont  -family Helvetica -size 18 -weight bold
	
	#update
	
	wm deiconify .
	update idletasks
	if {[info exists startupError]} {
	    tk_messageBox -icon warning -type ok -title "$g(appname) - Error in \
          Startup File" -message $startupError
	}
    }
    ###########################################################################
    # Save windows
    proc pick_sdf_file { file var} {
	upvar $var v
	variable t
	set tf [BrowsePick_file "Select SDF File" $file  [list [list "SDF files" "*.sdf"]]]
	if { $tf == "" }  {
	    return
	}
	# get the list of properties and select the on that will be used as target/active value
	set props [lcp::lcp_sdf_list_props $tf]
	if { [llength $props] == 0 } {
	    debug-info "Warning: no properties found in $tf"
            do-error "Warning: no properties found in $tf"
	    set v ""
	    return
	}	
	#debug-info $props
	# create a window with the list of properties to the user select
	set l [win_sel_props $props yes]
	#debug-info "win_sel_props $l"
	set prop [lindex $l 0]
	set type [lindex $l 1]
	if { $prop=="" || $type =="" } {
	    return
	}
	set v $tf
	set t(sdf_class_prop) $prop
        debug-info "sdf_class_prop=$prop"
	set t(sdf_class_type) $type	
    }
    proc win_sel_props { lprops  {input_type no} } {
	variable g
	variable w 
	variable t
        variable sp_$input_type
        set sp_$input_type 0
	set t(list_props) $lprops
	set w(sp_$input_type) .sel_props
	set fp $w(sp_$input_type).f
	set fp2 $w(sp_$input_type).i
	if { $input_type == "no" } {
	    set title "Select Properties"
	} else {
	    set title "Select Activity Property"
	}
	if { ![winfo exists $w(sp_$input_type)]} {
	    toplevel $w(sp_$input_type)
	    wm group $w(sp_$input_type) .
	    wm transient $w(sp_$input_type) .
	    wm title $w(sp_$input_type) "$title"		
	    wm protocol $w(sp_$input_type) WM_DELETE_WINDOW [list [namespace current]::bcancel $w(sp_$input_type)]
	    #wm withdraw $w(task_status)
	    set fp [frame $w(sp_$input_type).p]
	    pack $fp -side top -fill x
	    label $fp.l1 -text "Properties:" 
	    pack $fp.l1 -side top
	    if { $input_type=="no" } {
		set lbmode multiple
	    } else {
		set lbmode single		
	    }
            frame $w(sp_$input_type).f
	    pack $fp -side top -fill x
	    listbox $fp.list -listvariable "[namespace current]::t(list_props)" -width 33 -selectmode $lbmode -yscrollcommand "$fp.vscr set"
	    scrollbar $fp.vscr -orient vertical -command "$fp.list yview"
	    pack $fp.list -side left
	    pack $fp.vscr -side left -expand 1 -fill y

	    if { $input_type=="yes" } {
                frame $w(sp_$input_type).i
		pack $fp2 -side top -fill x
		labelframe $fp2.l -text "Type: " -padx 0		
		checkbutton $fp2.l.c1 -text "numeric"   -variable t(type_n) -command "$fp2.l.c2 deselect;set [namespace current]::t(type) numeric"
		checkbutton $fp2.l.c2 -text "string"    -variable t(type_s) -command "$fp2.l.c1 deselect;set [namespace current]::t(type) string"
		pack $fp2.l  -side top -fill x
		pack $fp2.l.c1 $fp2.l.c2 -side left
	    }
	    button $w(sp_$input_type).bok -text "OK"  -command [list  set [namespace current]::sp_$input_type 1]
	    button $w(sp_$input_type).bcancel -text "Cancel"  -command [list [namespace current]::bcancel $w(sp_$input_type)]

	    if { $input_type=="yes" } {
		pack $w(sp_$input_type).bok -side top 
	    } else {
		pack $w(sp_$input_type).bok $w(sp_$input_type).bcancel -side top 
	    }
            $fp.list selection set 0
	} 
	if { $input_type=="yes" } {
          if { $t(type) == "numeric" } {
	    $fp2.l.c1 select
          } else {
	    $fp2.l.c2 select
          }
        }
#	update
	wm deiconify $w(sp_$input_type)
	raise $w(sp_$input_type)
        grab $w(sp_$input_type)

	tkwait variable [namespace current]::sp_$input_type
	wm withdraw $w(sp_$input_type)
        grab release $w(sp_$input_type)
        set class_prop [lindex $t(list_props) [$fp.list curselection]]
	return [list $class_prop $t(type)]
#	return [list $t(list_props) $t(type)]
    }
    proc pick_lcproj_file { file var} {
	upvar $var v
	set v [BrowsePick_file "Select Project File" $file  [list [list "LogCHEM project files" "*.lc_proj"]]]
    }

    # filetypes filetype { { { label} {ext} } ...}
    proc BrowsePick_file { title f filetype } {
	variable w
	
	set foo         $f
	set initialdir  [file dirname $foo]
	set initialfile [file tail $foo]
	set filename    [tk_getOpenFile -title $title -initialfile $initialfile \
			     -initialdir $initialdir -filetypes  $filetype]
	return $filename	
    }
    proc BrowseSave_file { title f filetype } {
	variable w
	
	set foo         $f
	set initialdir  [file dirname $foo]
	set initialfile [file tail $foo]
	set filename    [tk_getSaveFile -title $title -initialfile $initialfile \
			     -initialdir $initialdir -filetypes  $filetype]
	return $filename	
    }
    
    ########################################################################
    #                         Project Menu
    proc do-exit {{exitcode {}}} {
	variable g	
	if {$exitcode == ""} {
	    set exitcode $g(exitValue)
	}
	# exit with an appropriate return value
	catch {exit $exitcode} msg	
    }
    #
    proc newProject {} {	
	closeProject
	project_gui new "New Project"
    }
    #
    proc openProject {} {	
	variable g
	set proj_file ""
	pick_lcproj_file ""  proj_file
	if { $proj_file!="" } {
	    closeProject
	    if { [lcp::lcp_load $proj_file]>=0 } {
		set g(proj_loaded) 1
	    } else {		
		set g(proj_loaded) 0
		lcp::lcp_close
	    }
	}
	update_win_status
    }    
    #
    proc saveProject {} {	
	variable g
	if { $g(proj_loaded) } {
	    lcp::lcp_save
	}	
    }
    #
    proc closeProject {} {	
	variable g
	if { $g(proj_loaded) } {
	    lcp::lcp_save
	    lcp::lcp_close
	    set g(proj_loaded) 0
	}
        catch { reset_tan_win }
	update_win_status
    }
    # GUI to create a new project
    proc project_gui { type title } {
	variable w
	variable g
	variable t
	array set t { 
	    name     ""
	    sdf_file ""
	    dir      ""
	    type     string
            type_s   1
            type_n   0
	}
	set t(dir) [pwd]
	if { $type!="new" } {
	    set t(name)     [lcp::lcp_get name]
	    set t(sdf_file) [lcp::lcp_get sdf_proj]
	    set t(dir) 	    [lcp::lcp_get dir]
	}
	set waitvar {}
	set w(proj) .proj
	if {[winfo exists $w(proj)]} {
	    # already exists
	} else {  
	    toplevel $w(proj)
	    wm group $w(proj) .
	    if {[winfo exists $w(client)]} {
		wm transient $w(proj) .
	    }
	    if {$g(windowingSystem) == "aqua"} {
		setAquaDialogStyle $w(proj)
	    }

	    wm title $w(proj) "$title"
	    
	    wm protocol $w(proj) WM_DELETE_WINDOW [list [namespace current]::bcancel $w(proj)]
	    wm withdraw $w(proj)
	    
	    set fp [frame $w(proj).p -borderwidth 2 -relief groove]
	    pack $fp -side top -fill x

	    label $fp.l1 -text "Name" 
	    entry $fp.i1 -width 40 -relief sunken -textvariable [namespace current]::t(name)
	    label $fp.l2 -text "SDF File" 
	    if { $type=="new" } {
		button $fp.i2  -textvariable [namespace current]::t(sdf_file) -command "[namespace current]::pick_sdf_file \"\" [namespace current]::t(sdf_file)" 
	    } else {
		button $fp.i2  -textvariable [namespace current]::t(sdf_file) -command "[namespace current]::pick_sdf_file $t(sdf_file) [namespace current]::t(sdf_file)" 
	    }
#	    entry $fp.i2 -width 40 -relief sunken -textvariable [namespace current]::t(sdf_file)
	    label $fp.l3 -text "Directory" 
	    entry $fp.i3 -width 40 -relief sunken -textvariable [namespace current]::t(dir)
	    #$fp.i1 insert end ""
	    
	    grid $fp.l1 -row 0 -column 0 -sticky e
	    grid $fp.i1 -row 0 -column 1 -columnspan 4 -sticky nsew -pady 4

	    grid $fp.l2 -row 1 -column 0 -sticky e
	    grid $fp.i2 -row 1 -column 1 -columnspan 4 -sticky nsew -pady 4

	    grid $fp.l3 -row 2 -column 0 -sticky e
	    grid $fp.i3 -row 2 -column 1 -columnspan 4 -sticky nsew -pady 4
	    
	    set w(proj,entry1) $fp.i1
	    
	    set fb [frame $w(proj).b -borderwidth 2]  
	    pack $fb -side top

	    if { $type=="new" } {	    
		button $fb.bnew    -text "Create" -command [namespace current]::new_proj_cmd
	    } else {
		button $fb.bnew    -text "Change" -command [namespace current]::save_proj_cmd
	    }
	    button $fb.bcancel -text "Cancel" -command [list [namespace current]::bcancel $w(proj)]
	    pack $fb.bcancel -side left  
	    pack $fb.bnew -side left

	}
	if {[winfo exists $w(client)]} {
	    centerWindow $w(proj)
	} else {
#	    update
	}
	wm deiconify $w(proj)
	raise $w(proj)
	focus $w(proj,entry1)
	tkwait variable waitvar
	wm withdraw $w(proj) 

    }
    proc bcancel { win } {
	#puts stderr $win
	if {[winfo exists $win]} {
	    wm withdraw $win 	
  	    grab release $win
	}
    }
    # create a new project object (namespace)
    proc new_proj_cmd {} {
	variable t
	variable w
	variable g
	# check if name, sdf_file and dir values are ok
	set name [string trim $t(name)]
	set dir  $t(dir)
	set sdf_file $t(sdf_file)
	if { ![string length $name] } {
	    do-error "Missing or invalid project name."
	    return
	}
        #ttk::progressbar $w(proj).pb -mode indeterminate
        #pack [$w(proj).pb]
	#$w(proj).pb -start

	# check if the molecules have names/titles
	set n [llength [mols_titles $sdf_file] ]
	if { $n ==0 } {
	    debug-info "Titles not found in $sdf_file"
	    return
	}
	if { [lcp::lcp_new $name $dir $sdf_file] } {
	    lcp::lcp_set_value nmols $n
	    lcp::lcp_set_value sdf_class_prop $t(sdf_class_prop)
	    lcp::lcp_set_value sdf_class_type $t(sdf_class_type)
	    lcp::lcp_save
	    wm withdraw $w(proj) 
	    set g(proj_loaded) 1
	}
        #$w(proj).pb stop
	update_win_status
    }    
    
    proc reset_tan_win {} {
	variable w
	catch {$w(filter_but) configure -state disabled}
	$w(log) delete 1.0 end
    }
    proc update_win_status {} {
	variable g
	variable w
	debug-info "update_win_status"
#	parray g

	if { $g(proj_loaded) } {
	    set g(statusProject) "Project: [lcp::lcp_get_value name] Num.Mols: [lcp::lcp_get_value nmols]"
	    # enable menu options
	    $w(menubar) entryconfigure 1 -state normal ;#sdf
	    $w(menubar) entryconfigure 2 -state normal ;#analysis
            # FILE options
            $w(menubar).project entryconfigure 0 -state disabled
            $w(menubar).project entryconfigure 1 -state disabled
            $w(menubar).project entryconfigure 2 -state normal
            $w(menubar).project entryconfigure 3 -state normal
	    # enable project options
	    # disable project options
            # SDF options
            set state disabled
            if { [ext_prog::prog_available generatemd]  } { set state normal }
	    $w(menubar).sdf entryconfigure  0 -state $state
	    setup_learn_options
	} else {
	    set g(statusProject) ""
            $w(menubar).project entryconfigure 0 -state normal
            $w(menubar).project entryconfigure 1 -state normal
            $w(menubar).project entryconfigure 2 -state disabled
            $w(menubar).project entryconfigure 3 -state disabled

	    # disable menu options
	    $w(menubar) entryconfigure 1 -state disabled ;#sdf
	    $w(menubar) entryconfigure 2 -state disabled ;#analysis
	    # enable project options
	    # disable project options
	}

    }
    ###############################################################################
    #               Generates molecular descriptors using GenerateMD              #
    proc sdfGenMD_gui {} {
	set vars "w g"
	foreach var $vars { variable $var }
	
	set waitvar {}
	set w(mol) .mol
	
	if {[winfo exists $w(mol)]} {
	    # already exists
	} else {
	    
	    toplevel $w(mol)
	    wm group $w(mol) .
	    if {[winfo exists .client]} {
		wm transient $w(mol) .
	    }
	    wm title $w(mol) "Generate Molecular Descriptors"	    
	    if {$g(windowingSystem) == "aqua"} {
		setAquaDialogStyle $w(mol)
	    }	    
	    wm protocol $w(mol) WM_DELETE_WINDOW [list [namespace current]::bcancel $w(mol)]
	    wm withdraw $w(mol)
	    
	    set fmol [frame $w(mol).p1 -borderwidth 2 -relief groove]
	    pack $fmol -side top -fill x
	    
	    set fb [frame $w(mol).b -borderwidth 2]
	    button $fb.sel -text "Run"    -command "[namespace current]::gen_md"
	    button $fb.can -text "Cancel" -command "[namespace current]::bcancel $w(mol)"
	    
	    set i 0
	    variable descriptors
	    variable sel_md
	    foreach d [array names descriptors] {
		checkbutton $fmol.c$i -text "$descriptors($d) ($d)" -variable [namespace current]::sel_md($i)		
		if { [lcp::lcp_md_selected $d] } {
		    $fmol.c$i select
		    set sel_md($i) 1
		} else {
		    $fmol.c$i deselect
		    set sel_md($i) 0
		}
		pack $fmol.c$i -anchor nw 
		incr i  
#		set descr($i) [string range $sdline [expr $inimd + 1] [expr $endmd - 1]]  
	    }
	    pack $fb -side top
	    pack $fb.sel -side left 
	    pack $fb.can -side left
	}
	if {[winfo exists .client]} {
	    centerWindow $w(mol)
	} else {
#	    update
	}
	wm deiconify $w(mol)
	raise $w(mol)
        grab $w(mol)
	tkwait variable waitvar
        grab release $w(mol)
	wm withdraw $w(mol)	
    }
    #---------------Runs the generate molecular descriptors procedure-------------#
    proc gen_md {} {
	#variable i w descr c ctr
	variable w
	variable sel_md
	
	set i 0
	variable descriptors
	set selected {} 
	foreach d [array names descriptors] {
	    if { $sel_md($i) } {
		lappend selected $d
	    }
	    incr i  
	}
	if { [ genMD $selected $w(mol)] } {
	    lcp::lcp_set_value md $selected
	}
	wm withdraw $w(mol)
	return 
    }
    #---------------------Calls GenerateMD Software--------------------------------#
    proc genMD { selected_md pwin} {
	set mds [join $selected_md " -k "]
	set mds "-k $mds"
	set sdf_proj [lcp::lcp_get_value sdf_proj]
	regsub ".sdf"  $sdf_proj  "_md.sdf" md_file
	lcp::lcp_new_sdf_op "genMD"
	lcp::lcp_set_value md "$selected_md"
	catch { 
            ext_prog::run_generatemd  "c $sdf_proj -o $md_file -S $mds" 1 $pwin
	    #eval exec $mdcmd c $sdf_proj -o $md_file -S $mds 
	    lcp::lcp_set_value md_file $md_file
	    lcp::lcp_set_value sdf_proj $md_file
	    set err ""
	} msg
	
        debug-info "debug:genMD: $msg"
	if {  $msg == "" } {
            do-info "Succesfuly added the molecular descritors."
	    return 1
	}
	return 0
    }
    #############################################################################3
    #------Discards similar molecules in the dataset (tanimoto coefficient)--------#
    proc sdfTanFilter_gui {} {
	variable w 
	variable g 

	set waitvar {}
	set w(tan) .tan
	
	if { ![winfo exists $w(tan)] } {
	    set w(tan) [toplevel .tan]
	    wm  group $w(tan) .
	    if {[winfo exists .client]} {
		wm transient $w(tan) .
	    }
	    wm title $w(tan) "Tanimoto Coefficient"

	    if {$g(windowingSystem) == "aqua"} {
		setAquaDialogStyle $w(tab)
	    }	    
	    wm protocol $w(tan) WM_DELETE_WINDOW [list [namespace current]::bcancel $w(tan)]
	    wm withdraw $w(tan)
	    
	    set ft [frame $w(tan).p1 -borderwidth 2 -relief groove]
	    pack $ft -side top -fill x
	    
	    button $ft.quit -text Close -command "[namespace current]::bcancel $w(tan)"
	    set butOk [button $ft.ok -text Run -command "[namespace current]::do_tCoefficient"]
	    pack $ft.quit $ft.ok -side right
	    
	    label $ft.l -text "Threshold: " -padx 0
	    entry $ft.input -width 20 -relief sunken -textvariable [namespace current]::g(tanimoto_coef)
	    pack $ft.l -side left
	    pack $ft.input -side left -fill x -expand true
	    
	    set w(tan,entry1) $ft.input
	    
	    set fb [frame $w(tan).b -borderwidth 2]
	    pack $fb -side top
	    set w(filter_but) $fb.bsave
	    button $fb.bsave  -text "Save SDF" -command "[namespace current]::filter_sdf" -state disabled
	    pack $fb.bsave -side left

	    # displays the output information after tanimoto procedure finished
	    frame .tan.t -borderwidth 2 
	    set log [text .tan.t.log -borderwidth 2 -relief raised -yscrollcommand {.tan.t.scroll set}]
	    scrollbar .tan.t.scroll -command {.tan.t.log yview}
	    set w(log) .tan.t.log
	    
	    pack .tan.t -side top
	    pack .tan.t.scroll -side right -fill y
	    pack .tan.t.log -side left -fill both -expand true
	} else {
	    reset_tan_win
	}

	if {[winfo exists .client]} {
	    centerWindow $w(tan)
	} else {
#	    update
	}
	wm deiconify $w(tan)
	raise $w(tan)
	focus $w(tan,entry1)
        grab $w(tan)
	tkwait variable $w(tan)
	wm withdraw $w(tan) 
        grab release $w(tan)
    }
    #
    #
    proc filter_sdf {} {
	variable g
	variable t

	AddLine2Log "Updating SDF file..."
	set f_sdf  [lcp::lcp_get_value sdf_proj]
	set ids2remove $t(2discard)
	
	set l ""
	set f 0
	set sep ""
	foreach id $ids2remove {
	    if { $f } { set sep " " }
	    set l "$l $sep title!=$id "
	    set f 1
	}
	lcp::lcp_new_sdf_op "filter tanimoto $g(tanimoto_coef)"        
        set cmd_args "$f_sdf --filter \"$l\" [pid].sdf"
        set status [lindex [ext_prog::run_babel "$cmd_args"] 0]
        if { !$status } {
            do-error "An error ocorred while executing babel"
	    debug-info "Error: babel $cmd_args"
            
        } else {
	   exec mv [pid].sdf $f_sdf
	   AddLine2Log "SDF ($f_sdf) file updated."
        }
        # update the number of molecules
	set n [llength [mols_titles $f_sdf] ]
	lcp::lcp_set_value nmols $n
        update_win_status
    }
    #
    #
    proc do_tCoefficient {} {
	variable g
	variable t
	variable w

	AddLine2Log "Threshold: $g(tanimoto_coef)"	
	set f_sdf  [lcp::lcp_get_value sdf_proj]
	regsub ".sdf" $f_sdf ".mat" f_mat
	debug-info "File exists $f_mat?"
	set cached 0
	if { [file readable $f_mat] } {
	    AddLine2Log "Loading cached tanimoto matrix: $f_mat"
	    if { [load_tan_matrix $f_mat] } { set cached 1 
	    } else {
		AddLine2Log "Error loading cached tanimoto matrix."
	    }
	}
	if { !$cached } {
	    AddLine2Log "Computing tanimoto matrix..."
	    set status [genTanimotoMatrix $f_sdf]
	    if { !$status } { return }
	    AddLine2Log "Computing tanimoto matrix \[done\]"
	    AddLine2Log "Saving tanimoto matrix to $f_mat ..."
	    if { [save_tan_matrix $f_mat] } {
		AddLine2Log "Saving tanimoto matrix to $f_mat \[done\]"
	    }
	    AddLine2Log "Exporting tanimoto matrix to CSV format $f_mat.csv ..."
	    if { [matrix2csvfile $f_mat.csv [namespace current]::tan_matrix  [mols_titles $f_sdf ]]} {
		Log "Exporting tanimoto matrix to CSV format $f_mat.csv \[done\]"
	    }
	}
	set ids2discard    [mols2discard $f_sdf $g(tanimoto_coef)]
	set nids2discard   [llength $ids2discard]
	set t(2discard) $ids2discard
	if { $nids2discard >0 } {
	    AddLine2Log "If you really want do discard $nids2discard molecules then press the button 'save sdf'"
	    AddLine2Log ""
	    $w(filter_but) configure -state active
	} else {
	    $w(filter_but) configure -state disabled
	    AddLine2Log "No molecules found to discard."
	    AddLine2Log ""

	}
	##
	#debug-info "Converting SMI file to fingerprint..."
	#if { ![babel $f_smi $f_fpr] } {
	#    do-error "Internal error: failed to convert SDF file to fingerprint format"
	#    return	    
        #}
	
	return
    }
    # returns a list with the titles/names of the molecules
    proc mols_titles { f_sdf } {
	# list of ids
	debug-info "Getting list of molecules from $f_sdf"
	set ids [lindex [ext_prog::run_babel "$f_sdf -osmi -xt" 2] 1]
	if { [llength $ids] == 0 } {
	    do-error "Unable to obtain molecules' titles from $f_sdf file."
	    return {}
	}
	debug-info "found [llength $ids] mols in $f_sdf"
	return $ids
    }
    # load a cached tanimoto matrix
    proc load_tan_matrix { f } {
	variable tan_matrix
	catch { source $f; set _ "" } err
	if { $err != "" || ![llength [array names tan_matrix]] } {
	    array set tan_matrix {}
	    return 0
	}
	return 1
    }
    # save a tanimoto matrix to a file
    proc save_tan_matrix { f } {
	variable tan_matrix
	#parray tan_matrix
	set nm [namespace current]
	if { [set fd [open $f w+]]<0 } {
	    return 0
	}	
	puts $fd "array set ${nm}::tan_matrix {}"
	foreach idx [array names tan_matrix ] {
	    puts $fd "set ${nm}::tan_matrix($idx) $tan_matrix($idx)"
	}
	close $fd
	return 1
    }
    #
    #
    proc matrix2csvfile { filename matrix_a elements} {
	upvar $matrix_a matrix
	
	set fd [open $filename w+]
	if { $fd <0 } {
	    do-error "Unable to open file $filename"
	    return 0
	}
	foreach f $elements {
	    puts -nonewline $fd ",$f"
	}

	puts $fd ""
	foreach f1 $elements {
	    puts -nonewline $fd "$f1"
	    foreach f2 $elements {
		set val ""
		catch { set val $matrix($f1,$f2); set _ "" } err
#		puts stderr $err
		if { $err!="" } {
		    catch { set val $matrix($f2,$f1); set _ "" } err
		}
#		puts stderr $err
		puts -nonewline $fd ",$val"
	    }
	    puts $fd ""
	}
	close $fd
	
	return 1
    }
    #
    # get a value from the tanimoto matrix
    proc get_matrix_val { id1 id2 } {
	variable tan_matrix
	set val "0"
	catch { set val $tan_matrix($id1,$id2); set _ ""} err
	if { $err == "" } { return $val }
	catch { set val $tan_matrix($id2,$id1); set _ ""} err
	return $val
    }
    # given a precomputed matrix with tanimoto coef. values
    # select a set of molecules with a coef > lim
    proc mols2discard { f_sdf lim } {
	variable tan_matrix
	set ids  [mols_titles $f_sdf]
	
	set 2discard {}
	foreach id $ids {
	    if { [lsearch  $2discard $id] == -1 } {
		foreach id2 $ids {
		    set val [get_matrix_val $id $id2]
		    if { $id!=$id2 && $val>=$lim } {
			if { [lsearch  $2discard $id2]==-1 } { lappend 2discard $id2	       }
		    }
		}
	    }
	}
	debug-info "2discard $2discard"
	return $2discard
    }
    # generates a matrix with the tanimoto values between all molecules
    proc  genTanimotoMatrix { f_sdf } {
	variable tan_matrix

	# list of ids
	set ids  [mols_titles $f_sdf]
	set nids [llength $ids]
	lcp::lcp_set_value nmols $nids
	if { $nids == 0 } {
	    return 0
	}
	#initialize the matrix
	array set tan_matrix {}
	set i 1 
	foreach id1 $ids {
	    set tan_matrix($id1,$id1) 0
	}
	#
	set n 1
	update_task_status "Computing tanimoto coef." 0 $nids
	foreach id $ids {
	    set coefs [split [lindex [ext_prog::run_babel "-f $n $f_sdf -ofpt | grep \"\>\" | tail -n +2"] 1]  "\n"]
#	    set coefs [lreplace $coefs 0 0]
	    foreach c $coefs {
		set X [string trim [lindex $c 0] ">"]
		set Y [lindex $c 3]
		set V [lindex $c 5]
		#puts "$X,$Y=$V"
		set tan_matrix($X,$Y) $V
		#set tan_matrix($Y,$X) $V
	    }
	    update_task_status "Computing tanimoto coef." $n $nids
	    incr n
	}
	update_task_status "Computing tanimoto coef." $nids $nids
	return 1
    }
    #
    proc update_task_status { name cur max } {
	variable g
	variable w
	set w(task_status) .task_status

	if { ![winfo exists $w(task_status)]} {
	    toplevel $w(task_status)
	    wm group $w(task_status) .
	    if {[winfo exists $w(client)]} {
		    wm transient $w(task_status) .
	    }
	    wm title $w(task_status) "Status"		
	    wm protocol $w(task_status) WM_DELETE_WINDOW [list [namespace current]::bcancel $w(task_status)]
	    #wm withdraw $w(task_status)
	    set fp [frame $w(task_status).p -borderwidth 2 -relief groove]
	    pack $fp -side top -fill x
	    label $fp.l1 -text "$name" 
	    label $fp.i1 -width 4 -justify right -textvariable [namespace current]::g(task_status)
	    label $fp.l2 -text "% complete" 
	    grid $fp.l1 -row 0 -column 0 -sticky e
	    grid $fp.i1 -row 0 -column 1 -sticky nsew -pady 4
	    grid $fp.l2 -row 0 -column 2 -sticky nsew -pady 4
	} 
	wm deiconify $w(task_status)
	raise $w(task_status)
	update
	if { $cur == $max } {
	    set g(task_status) "Complete"
	    wm withdraw $w(task_status)
	} else {
	    set per [expr 1.0*int($cur*10000.0/$max)/100]
	    set g(task_name) $name
	    set g(task_status) $per
	}
	debug-info "$name: $g(task_status)%"
    }
    #######################################################
    # Export a sdf file to ARFF or CSV format
    proc exportsdf { format } {	
	if  { $format == "csv" } {
	    set ext "csv"
	    set type "CSV"
	} else {
	    set ext "arff"
	    set type "ARFF"
	}       
	set f_sdf  [lcp::lcp_get_value sdf_proj]
	set filename [regsub ".sdf$" $f_sdf ".$ext"]
	set fileNameSave [BrowseSave_file "Export to $type format" $filename [list [list "$type Files" "*.$ext"]]]
	debug-info "Export $f_sdf to $fileNameSave"
	if { $fileNameSave == "" } {
	    return 0
	}
	#
	set args [get_sdfconv_args]
	set cmd "lc_sdfconv.pl $format $f_sdf $args > $fileNameSave"	
        set res [ext_prog::run_prog $cmd]
        set status [lindex $res 0]
        if { $status==0 } {
          do-info  "File $fileNameSave successfully created"
          return 1
        } else {
          do-error "Failed to create $fileNameSave"
          return 0
        }
    }
    ################################################################################
    # return a list of the type sdf_property_name/arff_type
    # Note: the last pair property/type will be the class attribute in the arff file
    proc get_sdfconv_args {} {
	set target      [lcp::lcp_get_value sdf_class_prop]
	set target_type [lcp::lcp_get_value sdf_class_type]
	# Mol. descriptors created by the user
	set mds [lcp::lcp_get_value md]	
	set smds ""
	foreach md $mds {
	    if { $md != "{}" } {
		set smds "$smds $md/numeric "
	    }
	}
	# include the descriptors selected
	return "$smds Species/string StudyType/string STRUCTURE_SMILES/string $target/$target_type"
    }
    #----------------Sets search restrictions parameters---------------#
    proc searchRestrictions {} {
	variable w 
	variable g 
	variable t
	variable aleph_vars
	#nodes minPrec maxNoise minPos clauseLen theoryFile 
	#set search [list "ar"  "heuristic" "bf" "df"]	
	set search [list "bf" "df"]	
	# temporary copy

	foreach v $aleph_vars {
	    set [namespace current]::t($v) [set [namespace current]::g($v)]
	}
	set [namespace current]::sr_close 0
	set waitvar {}
	set w(sr) .sr 
	if {![winfo exists $w(sr)]} {
	    toplevel $w(sr)
	    wm group $w(sr) .
	    if {[winfo exists .client]} {
		wm transient $w(sr) .
	    }
	    wm title $w(sr) "Settings"	    
	    if {$g(windowingSystem) == "aqua"} {
		setAquaDialogStyle $w(sr)
	    }	    
	    wm protocol $w(sr) WM_DELETE_WINDOW [list [namespace current]::bcancel $w(sr)]
	    wm withdraw $w(sr)
	    set fs [frame $w(sr).f1 -borderwidth 2 -relief groove]
	    pack $fs -side top -fill x
	    
	    label $fs.l1 -text "Nodes: "
	    label $fs.l2 -text "Minimum precision: "
	    label $fs.l3 -text "Maximum noise: "	
	    label $fs.l4 -text "Minimum positives covered: "
	    label $fs.l5 -text "Maximum clause length: "
	    label $fs.l6 -text "Search Strategy: "

	    entry $fs.i1 -width 40 -relief sunken -textvariable [namespace current]::t(aleph_nodes)
	    entry $fs.i2 -width 40 -relief sunken -textvariable [namespace current]::t(aleph_precision)
	    entry $fs.i3 -width 40 -relief sunken -textvariable [namespace current]::t(aleph_noise)
	    entry $fs.i4 -width 40 -relief sunken -textvariable [namespace current]::t(aleph_minpos)
	    entry $fs.i5 -width 40 -relief sunken -textvariable [namespace current]::t(aleph_clauselength)
	    listbox $fs.i6 -height 0 -selectmode single

	    set c 0
	    foreach s $search {
		$fs.i6 insert end $s
		if { [set [namespace current]::g(aleph_search)] == $s } {
		    $fs.i6 selection set $c
		    $fs.i6 activate $c
		}
		incr c
	    }
	    for { set k 1} { $k<7 } { incr k } {
		grid $fs.l$k -row [expr $k-1] -column 0 -sticky e
		grid $fs.i$k -row [expr $k-1] -column 1 -columnspan 4 -sticky nsew -pady 4
	    }
	    set fb [frame $w(sr).b -borderwidth 2]  
	    pack $fb -side top	    
	    button $fb.bsave   -text "OK"     -command [list set [namespace current]::sr_close 1]
	    button $fb.bcancel -text "Cancel" -command [list [namespace current]::bcancel $w(sr)]
	    pack $fb.bsave   -side left
	    pack $fb.bcancel -side left
	}  
	if {[winfo exists .client]} {
	    centerWindow $w(sr)
	} else {
#	    update
	}
	wm deiconify $w(sr)
	raise $w(sr)
	tkwait variable [namespace current]::sr_close
	if { [set [namespace current]::sr_close] } {
	    set i [$w(sr).f1.i6 curselection]
	    set [namespace current]::t(aleph_search) [lindex $search $i]
	    foreach v $aleph_vars {
		set [namespace current]::g($v) [set [namespace current]::t($v)]
	    }
	    gen_ilp_user_settings_file
	}
	wm withdraw $w(sr)
    }
    #-------------------Runs Aleph (ILP Engine)-----------------------------#
    proc runAleph {} {
	variable w
	variable g
	variable aleph_vars
	variable input
	set bkf [file tail [lcp::lcp_get_value ilp_b_file]]
	set dir [lcp::lcp_get_value ilp_dir]
	set file_prefix [regsub ".b$" $bkf ""]
	
	if {[winfo exists .tn]} {
	    destroy .tn
	} 
	toplevel .tn
	wm group .tn .
	if {[winfo exists .client]} {
	    wm transient .tn .
	}
	wm title .tn "Aleph"
	
	if {$g(windowingSystem) == "aqua"} {
	    setAquaDialogStyle .tn
	}
	
        # 
	wm protocol .tn WM_DELETE_WINDOW [list [namespace current]::bcancel .tn]
	wm withdraw .tn
	
	frame .tn.t -borderwidth 2 
	set log [text .tn.t.log -borderwidth 2 -relief raised -yscrollcommand {.tn.t.scroll set}]
	set w(log) $log
	scrollbar .tn.t.scroll -command {.tn.t.log yview}

	pack .tn.t.scroll -side right -fill y
	pack .tn.t -side top
	pack .tn.t.log -side left -fill both -expand true
	
#	update
	wm deiconify .tn
	raise .tn

	#####################################
	if { ![gen_ilp_user_settings_file] } {
	    return
	}

	set command "lc_runaleph $dir $file_prefix &"
        AddLine2Log "**** CMD: $command"
        AddLine2Log "**** WD: [pwd]"
        AddLine2Log "**** This may take a long time."
        AddLine2Log "*************************************"

        if { [set fd [ext_prog::run_prog_sb $command ]] > 0 } {
#            debug-info ">>[pid]>[pid $fd]"
            set pid [lindex [pid $fd] 0]
	    fileevent $fd readable [list [namespace current]::ALog $fd "" $pid]
     	    wm protocol .tn WM_DELETE_WINDOW  [list [namespace current]::stop_aleph $fd "" $pid]

	}        
        after 1000
    }
    proc stop_aleph { fd win pid } {
         catch { 
                 close $fd 
                 ext_prog::stop $win $pid
                 do-info "Execution interrupted" .tn
         }
         [namespace current]::bcancel .tn                  

    }
    proc setup_learn_options { } {
	variable w 	
	set all_options [list 2 3 4 5 7]
	# first - a project file must be created - experiment name and a theory file
	if { ![file exists [lcp::lcp_get_value sdf_proj]] } {
	    $w(learnMenu) entryconfigure 0 -state disabled
	} else {
	    $w(learnMenu) entryconfigure 0 -state normal
	}
	if { [file exists [lcp::lcp_get_value ilp_b_file]] } {
	    # enable all options on the LEARN menu
	    foreach e $all_options { 
		#puts $e
		$w(learnMenu) entryconfigure $e -state normal 
	    }
	} else {
	    # the user must select/create a dataset before proceeding
	    foreach e $all_options {  $w(learnMenu) entryconfigure $e -state disabled }
	}    
    }
    #------------------Shows the Background Knowledge file-------------------------#
    proc viewBk {} {
	variable g 
	variable w
	
	set bkf [lcp::lcp_get_value ilp_b_file]
	if { ! [file exists $bkf] } {
	    return 0
	}

	set bk [open $bkf r]
	if { $bk < 1 } {
	    return 0
	}

	if {[winfo exists .tn]} {
	    destroy .tn
	} 
	toplevel .tn
	wm group .tn .
	if {[winfo exists .client]} {
	    wm transient .tn .
	}
	wm title .tn "Background knowledge ($bkf)"
	
	if {$g(windowingSystem) == "aqua"} {
	    setAquaDialogStyle .tn
	}
	
	wm protocol .tn WM_DELETE_WINDOW [list [namespace current]::bcancel .tn]
	wm withdraw .tn
	
	frame .tn.t -borderwidth 2 
	set log [text .tn.t.log -borderwidth 2 -relief raised -yscrollcommand {.tn.t.scroll set}]
	set w(log) $log

	scrollbar .tn.t.scroll -command {.tn.t.log yview}
	
	pack .tn.t.scroll -side right -fill y
	pack .tn.t -side top
	pack .tn.t.log -side left -fill both -expand true
	
	if {[winfo exists .client]} {
	    centerWindow .tn
	} else {
#	    update
	}
	wm deiconify .tn
	raise .tn
	
	while {[gets $bk bkLine] >= 0} {
	    AddLine2Log $bkLine
	}
	close $bk 
    }   
    ###########################################################
    #
    proc ALog { input {win ""} {pid ""} } {
	
	if { [eof $input] } {
            debug-info "Closing ifile..."
	    catch {close $input}
	    AddLine2Log "************** All done **************"
	    #AddLine2Log "************** The results can be visualized by selecting the Analisys->Visualize menu option. ************** "
            if { $win != "" } { ext_prog::stop $win $pid }
	} else {
            set l "" 
            if { [gets $input l] >0 } {
	       AddLine2Log $l
            }
	}
        after 1
    }
    proc AddLine2Log {input}  {	
	variable w	       
	$w(log) insert end "$input\n"
	$w(log) see end
    }
    #--------------
    proc do_genILPDataset {} {
	variable g
	variable w
	variable t
	variable func_groups
	# check threshold
	debug-info "do_genILPDataset"
	##############
	# new analysis
	set ctype [lcp::lcp_get_value sdf_class_type]
	if { $ctype == "numerical" } {
	    do-error "ILP analysis require that the type of the target property to be a string. For the moment the values of the target property must be active and inactive."
	    return 
	}
	# functional groups
	set fgroups {}
	set names [array names func_groups]
	foreach e [array names t "fg*"] {
	    if { $t($e) } {
		set id [regsub "fg" $e ""]
		lappend fgroups [lindex $names $id]
	    }
	}
	if { ![lcp::lcp_new_analysis ilp  $fgroups $g(LogCHEM_DIR)] } {
	    set err [lcp::lcp_get_error ]
	    do-error  "$err"
	    return
	}
	set f_ilp [lcp::lcp_get_value ilp_b_file]
	do-info "Dataset file $f_ilp succesfully generated."	
	update_win_status
	bcancel $w(dt)
    }
    #--------------------------Saves Aleph theory-----------------------------------#
    proc recTheory { theoryf} {
	variable g 
	#nodes minPrec maxNoise minPos clauseLen theoryFile log input 
	set dir [lcp::lcp_get_value ilp_dir]
	set theoryf "$dir/$theoryf"
	if {[file exists $theoryf]} {
	    exec rm $theoryf
	}
	set th [open $theoryf a]	
	set rulFile [open LOG r]
	set showTheory 0
	set fileout 1
	while {[gets $rulFile ruleLine] >= 0} {
	    if {$ruleLine == "\[Training set performance\]"} {set fileout 0} 
	    if {$showTheory} {
		if {$fileout} {
		    if {![string first \[ $ruleLine]} {
			puts $th "%$ruleLine"
		    } else {
		  puts $th $ruleLine 
		    }
		}
#           Log $ruleLine
	    }
	    if {$ruleLine == "\[theory\]"} {
		set showTheory 1
	    }
	}
	close $th
    }
    
    proc gen_ilp_user_settings_file {} {

	variable aleph_vars
	set bkf [file tail [lcp::lcp_get_value ilp_b_file]]
	set dir [lcp::lcp_get_value ilp_dir]
	set file_prefix [regsub ".b$" $bkf ""]
	#####################################
	#
	set settings_file "$dir/user_settings.pl"
	set fd [open $settings_file w]
	if { $fd < 0 } {
	    do-error "Unable to create file $settings_file"
	    return 0
	}
	foreach v $aleph_vars {
	    set var [regsub "aleph_" $v ""]
	    puts $fd ":-set($var,[lindex [set [namespace current]::g($v)] 0])."
	}
	puts $fd ":-set(rulefile,'$file_prefix.aleph')."
	puts $fd ":-set(i,[set [namespace current]::g(aleph_clauselength)])."
	puts $fd ":-set(verbosity,0)."
	close $fd
	return 1
    }
    #------------------Calls Match.yap (prolog)------------------------#
    proc doMatch { experiName struct_file rules_file } {
	debug-info "lc_matchmol $experiName $struct_file $rules_file"
        ext_prog::run_prog "lc_matchmol \"$experiName\" \"$struct_file\" \"$rules_file\""
	return 1
    }
    #----------------Loads a LogCHEM file with all molecules and classes--------#
    proc load_logChem { filename } {
	variable linenum
	variable data
	
	# classes
	# each line of the file contains the pattern, statistics and the seed
	if {[catch {set hndl [open "$filename" r]}]} {
	    do-error "Failed to open file: $filename"
	    return 0
	}
	set linenum 0
	#############################################
	# read the number of classes
	set nclasses [logchem_read_next_line $hndl]
	if { $nclasses=="" || $nclasses<2 } {
	    # ERROR
	    do-error "Error in Mols file  ($linenum): invalid number of classes"
	    return 0;
	}
	#############################################
	# classes' names
	set c 0
	set classes {}
	while { $c!=$nclasses } {
	    set cl [logchem_read_next_line $hndl]
	    if { $cl=="" } { break }
	    set classes [lappend classes $cl]
	    set data("mols-$cl") {}
	    incr c
	    if { $c == $nclasses } { break }
	} 
	if { $c != $nclasses } {
	    # ERROR
	    do-error "Error in Mols file  ($linenum): invalid number of classes"
	    return 0
	}
	set data(classes) $classes
	set data(nclasses) $nclasses
	set l {}
	################################################
	# number of rules
	set nrules [logchem_read_next_line $hndl]
	if { $nrules=="" || $nrules==0 } {
	    # ERROR
	    do-error "Error in Mols file  ($linenum): invalid number of rules"
	    return 0
	}
	set cur_rule_id 0
	array set rules {}
	while { $cur_rule_id!=$nrules } {
	    set cl [logchem_read_next_line $hndl]
	    if { $cl=="" } { break }
	    set cl [split $cl ","]
	    set id [lindex $cl 0]
            # TODO: change this code to handle multiple classes
	    set nmols [expr [lindex $cl 1]+[lindex $cl 2]]
	    set pat   [lindex $cl 3]
	    set rule  [lreplace $cl 0 3]
	    ########################################################################
	    # read the molecules associated with the rule
	    set read_nmols 0
	    foreach class $data(classes) {
		set mols_class($class) {}
		set ctr_mols_class($class) 0
	    }
	    while { ![eof $hndl] && $read_nmols!=$nmols} {	
		set line  [string trim 	[logchem_read_next_line $hndl]]
		if { $line!="" } {
		    set line  [split $line ","]
		    set mol   [lindex $line 0]
		    set class [lindex $line 1]
		    incr ctr_mols_class($class)
		    set mols_class($class) [lappend mols_class($class) $mol]
		    #set data("mols-$class") [lappend data("mols-$class") [lindex $line 0]]
		    #debug-info "data(mols-$class)=[set data(\"mols-$class\")]"
		    incr read_nmols
		}
	    }
	    debug-info "$cur_rule_id $id $nmols $pat"
	    set rules($cur_rule_id) [list nmols $nmols pat $pat rule $rule ctr_mols_class "[array get ctr_mols_class]" mols_class "[array get mols_class]" ]
	    incr cur_rule_id
	    
#	    if { $cur_rule_id == $nrules } { break }
	}     
	if { $cur_rule_id != $nrules } {
	    # ERROR
	    do-error "Error in Mols file  ($linenum): invalid number of rules"
	    return 0
	}
	set data(rules) [array get rules]
	set data(nrules) $nrules
	close $hndl
	debug-info "Read: nmols=$nmols nrules=$nrules nclasses=$nclasses"
	#parray rules
	#set data(mols) $l
	return $nrules
    }

    #-----------------------Reads a line of the LogCHEM file-----------------#
    proc logchem_read_next_line { fd } {
	variable linenum
	if { [eof $fd] } {
	    return ""
	}
	set l [gets $fd]
	incr linenum
	if { [regexp "^#" $l] } {
	    return [logchem_read_next_line $fd]
	}
	return $l
    }
    ###############################################################3
    # VMD interaction
    proc visualize_results { molfile } {	
	variable g 
	variable w
	variable data

	set waitvar {}
	set w(viz) .viz
	
	if {![winfo exists $w(viz)]} {
	    toplevel $w(viz)
	    wm group $w(viz) .
            if {[winfo viewable [winfo toplevel [winfo parent $w(viz)]]] } {
               wm transient $w(viz) [winfo toplevel [winfo parent $w(viz)]]
            }

	    if {$g(windowingSystem) == "aqua"} {
		setAquaDialogStyle $w(viz)
	    }
	    wm protocol $w(viz) WM_DELETE_WINDOW [list [namespace current]::bcancel $w(viz)]
	    wm withdraw $w(viz)
	    wm title $w(viz) "ILP Results"
	    #########################################
	    # Listbox with the patterns found
	    frame $w(viz).patlist
	    pack $w(viz).patlist -expand 1 -side top -fill y -anchor w
	    label $w(viz).patlist.l -text "Patterns Found" 
	    pack $w(viz).patlist.l -side top

	    listbox $w(viz).patlist.list -width 50 -selectmode single -yscrollcommand [list $w(viz).patlist.vscr set]
	    scrollbar $w(viz).patlist.vscr -orient vertical -command [list $w(viz).patlist.list yview]
	    # visualize pattern: TODO: change vprocess to vmd_display_pat
	    bind $w(viz).patlist.list <Double-1>  "[namespace current]::vis_pat_sel \[$w(viz).patlist.list  curselection\]"
	    pack $w(viz).patlist.list -side left
	    pack $w(viz).patlist.vscr -side left -expand 1 -fill y
 	    ######################################################
	    # molecules
	    pack [frame $w(viz).mols] -side top -expand 1 -fill y
	    pack [ label $w(viz).mols.l -text "Selected pattern" ] -side top
	    pack [ label $w(viz).mols.l2 -textvariable [namespace current]::data(cur_rule_pat)] -side top
	    foreach c $data(classes) {
		frame $w(viz).mols.lb$c
		pack  $w(viz).mols.lb$c -expand 1 -fill y -side left
	    
		set w(List_mols$c)     $w(viz).mols.lb$c.lb
		set w(List_mols_sb$c)  $w(viz).mols.lb$c.lb_sb  
		set data("nmols-$c") ""
		listbox $w(List_mols$c) -listvariable [namespace current]::data("mols-$c") -height 20 -yscroll "$w(List_mols_sb$c) set" -setgrid 1 
		scrollbar $w(List_mols_sb$c) -command "$w(List_mols$c) yview"
		bind $w(List_mols$c) <Double-1> "[namespace current]::show_mol_in_vmd \[$w(List_mols$c) curselection\] \[selection get\]"

		label $w(viz).mols.lb$c.l -text "$c"
		label $w(viz).mols.lb$c.l2 -textvariable [namespace current]::data("nmols-$c")

		pack $w(viz).mols.lb$c.l -side top
		pack $w(viz).mols.lb$c.l2 -side top
		pack $w(List_mols$c) -fill both -side left
		pack $w(List_mols_sb$c) -side right -fill y 
		$w(List_mols$c) activate 0
	    }
	    button $w(viz).close -text "Close"  -command [list [namespace current]::bcancel $w(viz)]
	    pack $w(viz).close -side top -anchor c
	    
	}	
	# 

#	update
	wm deiconify $w(viz)
	raise $w(viz)
        #grab $w(viz)        
	#####
	array set rules $data(rules)
	$w(viz).patlist.list delete 0 end
	foreach r [lsort -integer [array names rules]] {
	    array set rule $rules($r)
            # hardcoded to two preds in the prefix
            set extra [pprint_rule "$rule(rule)" "$rule(pat)"]
	    $w(viz).patlist.list insert end "$extra"
	}
	$w(viz).patlist.list activate 0
	$w(viz).patlist.list selection set 0
	vis_pat_sel 0
        
        tkwait window $w(viz)
        grab release $w(viz)
	return
    }
    proc vis_pat_sel { id } {
	debug-info "update_mols_list $id"
	variable data
	variable w
	if { $id == "" } {
	    return
	}
	array set rules $data(rules)
	set data(cur_rule) $id
	array set rule $rules($data(cur_rule))
	set data(cur_rule_pat) [pprint_rule $rule(rule) $rule(pat)]
	array set mols_class $rule(mols_class)
	foreach c $data(classes) {	    
	    # set the variables for the listboxes	    
	    set data("mols-$c") $mols_class($c)
	    set data("nmols-$c") [llength $mols_class($c)]
	}
    }

    proc pprint_rule { rule pat } {
            set extra [regsub -all "'" [regsub "ato.*" $rule ""] ""]
            if { $extra != "" } {
                # property
                set prop [regsub {\(.*} [lindex $extra 0] ""]
                set l [join $extra ","]
                set op [string trim [regsub {\(.*} [lindex [split $extra ")"] 1] ""]]
                #debug-info "Rule=$rule OP=$op prop=$op"
                if { $op != "" } {
                    set op [pprint_op $op]
                    set v [string trim [lindex $extra 3] ")"]
                    set extra "  and  $prop $op $v"
                } else {                                        
                    set extra "  and  $prop"
                }
            }
            return "$pat $extra"
    }
    proc pprint_op { op } {
        switch $op {
          eq   { return "=" } 
          gte  { return ">=" } 
          gt   { return ">" } 
          lt   { return "<" } 
          lte  { return "<=" } 
          _    { return "$op" }
        }
    }
    #---------Run VMD to display the Molecule and its match pattern--------------#
    proc show_mol_in_vmd { molid mol  { vmd_ops show_mol} } {
	variable g
	variable opts
	variable data
	set rule $data(cur_rule)
	set vmd_cmd $opts(vmd_cmd)
	# call vmd
	set tmpname "/tmp/[pid]"
	set ilp_dir [lcp::lcp_get_value ilp_dir]
	set path    "$ilp_dir/vmd"

	set bkf [file tail [lcp::lcp_get_value ilp_b_file]]
	set file_prefix [regsub ".b$" $bkf ""]
	
	# variables to export to vmd
	set data(vmd_new_pat_file)  "$path/new_pat.sdf"
	set data(vmd_new_rule_file) "$ilp_dir/new_rule.yap"
	set data(vmd_files_dir)     "$path"
	set data(vmd_ilp_dir)       "$ilp_dir"
	set data(vmd_ilp_dataset)   "$file_prefix"
	set data(vmd_classes)       "$data(classes)"
	set data(vmd_valid_ops)     "$vmd_ops"
        set data(vmd_rules_file)    "$data(rules_file)"
	set fd [open $tmpname w]
	if { $fd < 0 } {
	    do-error "Unable to open $tmpname"
	    return
	}
	######################################################
	puts $fd "mol on 0;"
	# export some variables to VMD
	puts $fd "set vmd_new_pat       $data(vmd_new_pat_file);"
	puts $fd "set vmd_new_rule_file $data(vmd_new_rule_file);"
	puts $fd "set vmd_files_dir     $data(vmd_files_dir);"
	puts $fd "set vmd_ilp_dataset   $data(vmd_ilp_dataset);"
	puts $fd "set vmd_ilp_dir       $data(vmd_ilp_dir);"
	puts $fd "set vmd_valid_ops     $data(vmd_valid_ops);"
	puts $fd "set vmd_classes       \[list $data(vmd_classes)\];"
	puts $fd "set vmd_rules_file    $data(vmd_rules_file);"
	puts $fd "source $g(LogCHEM_DIR)/bin/lc_vmd;"
	close $fd
	
	set rule [expr $rule+1]
	if { [saveSDFBonds $path $mol $rule] < 0 } {
	    return
	}
	
	set cmd "$vmd_cmd -startup $g(LogCHEM_DIR)/config/lc_startvmd.conf $path/MATCH_${rule}_$mol.sdf -m $path/MOL_${rule}_$mol.sdf -e $tmpname" 

        ext_prog::run_prog "$cmd"	
#	exec bash -c "$cmd"
    }
    
    proc saveSDFBonds {dir nmol rule} {
	set m      [open "$dir/MOL_${rule}_$nmol.sdf" r]
	set bonds_file "$dir/bonds.txt"
	exec rm -f "$bonds_file"

	set sb [open "$bonds_file" w]
	if { $sb < 0 } {
	    debug-info "Unable to create/open $bonds_file"
	    return -1
	}
	set cline 0
	set ctrat 0
	while {[gets $m mline] >= 0} {
	    incr cline	 
	    if {$cline < 4} {
		continue
	    }
	    if {$cline == 4} {
		set natoms [string trimleft [string range $mline 0 2]]
		set ctrat [expr $cline + $natoms] 
	    }
	    if {$cline > $ctrat} {
		if {$mline != "> <NSC>"} {
		    puts $sb $mline
		} else { break }             
	    }        
	}
	close $m
	close $sb
	return 1
    }       
} 

