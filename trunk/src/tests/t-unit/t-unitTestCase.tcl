## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
##  File Name:  t-unitTestCase.tcl
##  @short      A template and usable test case for t-unit.  
##  @author     Original author ~ Joe Boyle
##  @date       Original date   ~ 30-May-2006
##  @version    1.0   Initial release
##
##  @comment      This module can be used as a template or can be run to 
##  @c            test the t-unit test suite.  In order to run the 
##  @c            t-unittestcases.test file, copy this file to the project 
##  @c            level and global.tcl to the project level before executing 
##  @c            this script.  
##
##  @note       Multiple test cases can be run from one file
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
##  Revision history
##  ----------------
##
##          Rev    Rev. Date   Released by:  Revision Description
##         -----  -----------  ------------  --------------------------------
## version 1.0--  30-May-2006  Joe Boyle     Initial version written
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  No GLOBAL VARIABLES
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source init.tcl
t-unit::testHarness t-unit/t-unittestcases.test