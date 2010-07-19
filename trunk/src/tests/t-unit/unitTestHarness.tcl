## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
##  File Name:  unitTestHarness.tcl
##  @short      Use a file to run unit tests on Tcl procedures
##  @author     Original author ~ Joe Boyle
##  @date       Original date   ~ 15-Feb-2006
##  @version    1.0   Initial release
##
##  @comment      This module file provides functionality to evaluate a list of
##  @c            Tcl procedures as an automated unit test suite.  The list
##  @c            is processed from a test file.  The file is expected to be
##  @c            formatted specifically for this process, which supports a
##  @c            method of text differentiation that is easily read and can
##  @c            be understood by a user.  The test file also allows for the
##  @c            use of comments, to further explain and delimit operations.
##
##  @note       See procedure definition header below for more details.
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
##  Revision history
##  ----------------
##
##          Rev    Rev. Date   Released by:  Revision Description
##         -----  -----------  ------------  --------------------------------
## version 1.0--  16-Feb-2006  Joe Boyle     Initial version written
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  NO MODULE GLOBAL VARIABLES
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

proc t-unit::testHarness { filename }   {
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Proc Name:  testHarness
##  @short      Check if the arguments passed in are indeed equal to each other
##  @argument   filename   Name of file containing commands to evaluate
##  @result     returns number of successes/failures/total tests run
##  @example    t-unit::testHarness t-unit/t-unittestcases.test
##
##  @comment      This module file provides functionality to run unit tests on
##  @c            a set of Tcl procedures in an automated manner.  The tests
##  @c            are kept in a specially formatted test file, which can be
##  @c            created using any editor capable of producing "unformatted"
##  @c            ASCII text.&p&p
##  @c
##  @c            The file is organized as follows.  Any line that starts with
##  @c            a "hash" symbol (one or more) is considered a comment and will
##  @c            not be executed as a test.  Comments MUST take the entire line;
##  @c            no "in-line" comment syntax is provided.  All other lines are
##  @c            considered to be commands.&p&p
##
##  @c            Command lines are structured as follows.  Everything up to the
##  @c            first occurrance of the vertical bar symbol ("|" or "pipe" as
##  @c            it is known in UNIX/Solaris), is considered a Tcl command which
##  @c            may include arguments to that command.  There should be two,
##  @c            and only two, occurrances of the pipe.  Everything between the
##  @c            two pipes is considered to be the expected result of the
##  @c            execution of that line's command.  Everything occurring after
##  @c            the second occurrance of the pipe is considered to be the result
##  @c            returned by the command; this can include either "OK", or some
##  @c            other phrase indicating an expected "forced error condition"
##  @c            which occurs when the command's error handling functionality is
##  @c            being tested.&p&p
##
##  @c            Note that blank lines are considered comments.&p&p
##
##  @c            An annotated example is as follows, with ">"indicating the
##  @c            file margin:&p&p
##
##  @c     >## This is a comment line                                      &p
##  @c     >## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ another comment          &p
##  @c     >                                                               &p
##  @c     >## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~          &p
##  @c     >##  This is an example of a block comment                      &p
##  @c     >## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~          &p
##  @c     >  t-unit::assertEquals 12.34 12.34       |  PASS  |  OK        &p
##  @c     >  t-unit::assertFalse  TRUE              |  FAIL  |  NOT_FALSE &p
##  @c
##
##
##  @note       The setup and teardown procedures should be defined in order
##  @n          for the test to be performed properly.  The setup and teardown
##  @n          procedures are called once per each unit test.  
##
##  @danger     None
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   If the file name is empty, output a "usage" message and quit.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   after 1000
   if { 0 == [string length $filename] }   {
      puts stderr "Syntax:\n \
                   testHarness <filename> where:\n \
                   <filename> is the name of a unit test file\n"
      return -1
   }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Declare and initialize the counters
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   set      passCount      0
   set      failCount      0
   set      commentCount   0
   lappend  failedLines

   puts "\n\n\n\n"
   puts "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
   puts "                  BEGINNING TEST EXECUTION FILE"
   puts "\n   File name:\t$filename"
   puts "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
   puts "\n\n\n\n"

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Open the file and check open error; file is opened READONLY
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if { [catch {open $filename r} fID] } {
      puts stderr "Could not open $filename for reading\n$fID"
      return 1
   }
   
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Run the setup for this test
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   setup

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Read the file, one line at a time, parse the command, the expected,
  ##     and the pass or fail from the line, then evaluate the command and
  ##     check PASS or FAIL.  after 20 added to properly order error messages
  ##     and any other output generated
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   set lineNumber 0
   while {[gets $fID line] >= 0}   {
      incr lineNumber                  ;# Tracks line number for later comparison
      if { "#" != [string range $line 0 0] } {
         if { 0 != [string length $line] }   {
            set ndx1 [string first "|" $line 0]
            set ndx2 [string first "|" $line [expr $ndx1 + 1]]
            set command   [string range $line 0 [expr $ndx1 - 1]]
            if { "puts" == [string trim [lindex $line 0]] } {
               eval $command
               after 20
               incr commentCount
            } elseif {"global" == [string trim [lindex $line 0]] } {
               eval $command
               after 20
               incr commentCount
            } elseif {"set" == [string trim [lindex $line 0]] } {
               eval $command
               after 20
               incr commentCount
            } elseif {$ndx1 == -1} {
               incr commentCount
            } else {
               set expectPF  [string trim [string range $line [expr $ndx1 + 1] [expr $ndx2 - 1]]]
               set expResult [string trim [string range $line [expr $ndx2 + 1] end]]
               set result    [eval $command]
               after 20
               if { "OK" == $result }   {                   ;# PASS that should PASS
                  set passfail "PASS"
               } else {                                     ;# Don't know yet...
                  set ndx3 [string first ":" $result 0]
                  if { -1 == $ndx3 }  {                     ;# No ":" in the string
                     if { "OK" == $result }   {             ;# PASS that should PASS
                        set passfail "PASS"
                     } else {                               ;# FAIL that should PASS
                        set passfail "FAIL"
                     }
                  } else {                                  ;# FAIL that should FAIL == PASS
                     set passfail "FAIL"
                     set result [string range $result 0 [expr $ndx3 - 1]]
                  }
               }

              ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ##   Log the results of each test to stdout.
              ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               set lineOut [format "%6d" $lineNumber]
               if { $expectPF == $passfail }   {
                  puts "  Test at line $lineOut    PASSED : exp/got -- $expResult/$result"
                  incr passCount
               } else {
                  puts stderr "  Test at line $lineOut   *FAILED*: exp/got -- $expResult/$result"
                  lappend failedLines $lineNumber
                  incr failCount
               }
            }
         } else {
            incr commentCount
         }
      } else {
         incr commentCount
      }
   }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Close the file
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if {[catch {close $fID} err]} {
       puts "Close command failed: $err"
   }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Output the final results
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   puts "\n\n\n\n"
   puts "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
   puts "                  TEST EXECUTION FILE COMPLETED"
   puts "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
   puts "\n\n\n\n"
   puts "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
   puts "   RESULTS OF TESTS CONDUCTED USING COMMANDS IN TEST FILE:"
   puts "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
   puts "\n"
   puts "      Test Case File Name:\t$filename"
   puts "\n"
   puts "      Total lines read from file:\t$lineNumber"
   puts "      Total comment lines read:\t\t$commentCount"
   puts "      Total tests PASSED:\t\t$passCount"
   puts "      Total tests FAILED:\t\t$failCount"
   puts "      Total tests executed:\t\t[expr $passCount + $failCount]"
   puts "\n"
   if { 0 != [llength $failedLines] } {
      foreach value $failedLines {
         puts "         Test failed at line $value"
      }
      puts "\n"
   }
   puts "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Run the teardown for this test
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
   teardown

}

