## In order to run the t-unittestcases.test file, copy this
## file to the project level.  Then copy and execute the 
## t-unitTestCase.tcl at the project level to run the testcases.

source global.tcl

# Create index files that allow packages to be loaded automatically 
# with package require
proc updateIndex { libdir } {
   set index [file join $libdir tclIndex]
   if {![file exists $index]} {
      set update 1
   } else {
      set age [file mtime $index]
      set update 0
     ## Changes to directory mean files were deleted
      if {[file mtime $libdir] > $age} {
        ## Check each file for modification
         foreach file [glob [file join $libdir *.tcl]] {
            if {[file mtime $file] > $age} {
               set update 1
               break
            }
         }
      }
   }
  ## Create the necessary index files
   if { $update } {
      pkg_mkIndex $libdir *.tcl
      auto_mkindex $libdir *.tcl
   }
}

# Go into each package and create the necessary index files
foreach file [glob [file join [pwd] *]] {
   switch [catch {
      if {[file isdirectory $file]} {
         updateIndex $file
      }
   } result] {  
      0 {# Normal operation}
      1 {# Error occurred, but usually no tcl files found is the error} 
   }
}

# Appends all packages to the auto_path
lappend auto_path .

# Required packages
package require t-unit
