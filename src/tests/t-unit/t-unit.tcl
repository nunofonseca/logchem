## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  CHANGE TO YOUR PACKAGE NAME AND VERSION HERE.
##
##  This is the only place you need to modify this file!
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set   my_package_name     t-unit
set   my_version_number   1.0

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Don't modify file from this point forward!!!
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
namespace eval ::${my_package_name} {
   namespace export *

  ##
  ##  The version of this package
  ##
   variable version $::my_version_number

  ##
  ##  Variable for storing scope trace to file
  ##

}
set packageDir [file join [file dirname [info script]]]

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  This line appends this package's scripts directory to the auto_path
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lappend auto_path $packageDir

package provide ${my_package_name} [set ${my_package_name}::version]
